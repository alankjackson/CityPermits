#
#     View Houston Building Permits
#
#           Alan Jackson September 2019
#
#       Display map of area
#       Display chart of variable by date
#       Select by neighborhood or zipcode
#       Select date range
#       Select by searching description field
#

library(shiny)
library(tidyverse)
library(leaflet)
library(leafpop) # for popup on map
library(sf)
library(htmltools)
library(lubridate)
library(shinyWidgets)
library(DT)


###################################
#   get and set up the basic data
###################################

#   Directory where data is stored

DataLocation <- "https://www.ajackson.org/Permits/data/"

#   Tibble database

#DF <- readRDS(gzcon(url(paste0(DataLocation, "MasterPermits_toGini.rds"))))
DF <- readRDS(gzcon(url(paste0(DataLocation, "MasterPermits.rds"))))
df <- DF %>% filter(!is.na(lat)) 

MapCenter <- c(-95.363345, 29.756555) # center on downtown

init_zoom <- 14

NeighborCentroids <- readRDS(gzcon(url(paste0(DataLocation, 
                                              "SuperNeighborhoodCentroids.rds"))))
#   Add the -All- neighborhood, centered on downtown
All <- tribble(~SNBNAME, ~lat, ~lon,
               "-All-", MapCenter[2], MapCenter[1])
All <- st_as_sf(All, coords=c('lon', 'lat'), crs=4326)

NeighborCentroids <- rbind(NeighborCentroids, All)

Superneighborhoods <- str_sort(NeighborCentroids$SNBNAME) 

Zips <- readRDS(gzcon(url(paste0(DataLocation, 
                                 "ZipcodeCentroids.rds"))))
Zipsonly <- Zips$Zip

minDate <- min(DF$Date)
maxDate <- max(DF$Date)

colorIcons <- iconList(
  blue = makeIcon(gzcon(url(paste0(DataLocation,"../marker-icon-blue.png")))),  
  black = makeIcon(gzcon(url(paste0(DataLocation,"../marker-icon-back.png")))),  
  green = makeIcon(gzcon(url(paste0(DataLocation,"../marker-icon-green.png")))),  
  grey = makeIcon(gzcon(url(paste0(DataLocation,"../marker-icon-grey.png")))),  
  red = makeIcon(gzcon(url(paste0(DataLocation,"../marker-icon-red.png")))),  
  violet = makeIcon(gzcon(url(paste0(DataLocation,"../marker-icon-violet.png")))),  
  yellow = makeIcon(gzcon(url(paste0(DataLocation,"../marker-icon-yellow.png"))))  
)

##################################################
# Define UI for displaying and annotating photos
##################################################

shinyApp(
    ui = basicPage(
        fluidRow(
            column(width = 8,
                   leafletOutput("LocalMap"),
                   HTML("<hr>"),
                   #        Data Table
                   div(DT::dataTableOutput("table"), style = "font-size:80%"),
                   #DT::dataTableOutput('table'),
                   HTML("<hr>")
                   #   put ggplot graphic here
            ),
            
            #       Add right column with controls for display
            column(width = 3,
                   #    Select type of geographic selection
                   radioButtons("geography", label = strong("Which Geography?"),
                                choices = list("Nbhd" = "SNbhd", "Zip Code" = "Zip"), 
                                selected = "SNbhd",
                                width='90%',
                                inline=TRUE),
                   conditionalPanel(
                     #    Select Neighborhood
                     condition = "input.geography == 'SNbhd'",
                     selectInput("nbhd", "Choose a Neighborhood:",
                                 Superneighborhoods,
                                 selected="Downtown" )
                   ),
                   conditionalPanel( 
                     #    Select Zip
                     condition = "input.geography == 'Zip'",
                     selectInput("ZipCode", label="Choose a Zip Code:",
                                 Zips$Zip,
                                 selected="77002")
                   ),
                   HTML("<hr>"),
                   sliderInput('dateRange', label='Date range (drag from middle):',
                               min = minDate,
                               max = maxDate,
                               value=c(maxDate-7, maxDate),
                               dragRange=TRUE,
                               animate=TRUE,
                               step=7),
                   
                   HTML("<hr>"),
                   #    Select type of filter selection
                   radioButtons("filter", label = strong("Which Filter Type?"),
                                choices = list("Standard" = "Std", "Arbitrary" = "Arb"), 
                                selected = "Std",
                                width='90%',
                                inline=TRUE),
                   conditionalPanel(
                     #    Select Standard Filter
                     condition = "input.filter == 'Std'",
                     radioButtons("std_search", label = "Standard Filters",
                                  choices = list("Harvey"="HARVEY", 
                                                 "Solar"="SOLAR", 
                                                 "Demolition"="DEMO",  
                                                 "Repair"="REPAIR", 
                                                 "Pool"="POOL", 
                                                 "Residence"="SFR|S\\.F\\. RES", 
                                                 "All"="All"),
                                  selected = "All" )
                   ),
                   conditionalPanel( 
                     #    Select Zip
                     condition = "input.filter == 'Arb'",
                     searchInput(
                       inputId = "arb_search", label = "Enter filter text (regular expression)",
                       placeholder = "CELL TOWER",
                       btnSearch = icon("search"),
                       btnReset = icon("remove"),
                       width = "100%"
                     )
                   ),
                   
##########################   stopped here
                   #    Select or unselect all
                  # HTML("<hr>"),
                  # actionButton("selectAll", label = "Select All"),
                  # actionButton("deselectAll", label = "Deselect All") ,
                   HTML("<hr>")
            )
        ) # fluidRow
    ), # basicPage
    
    #####################################################
    # Define Server for displaying map and photos
    #####################################################
    
    server <- function(input, output, session) {
        
        #       This bit won't change
        ##################################
        #           Basemap
        ##################################
        output$LocalMap <- renderLeaflet({
            #   Basemap
          if (input$geography=="SNbhd") {
            ll <- st_coordinates(NeighborCentroids[NeighborCentroids$SNBNAME==input$nbhd,]$geometry)
            init_zoom <- 14
          } else {
            ll <- st_coordinates(Zips[Zips$Zip==input$ZipCode,]$Shape)
            init_zoom <- 14
          }
            leaflet(df) %>% 
                setView(lng = ll[1] , lat = ll[2], zoom = init_zoom ) %>%   
                addTiles()
        }) 
        ##################################
        #           Add points
        ##################################
        observe({
          # Clear old stuff off of map
          leafletProxy("LocalMap") %>% 
              clearMarkers()  
          # Filter down to what is selected
          #     Geography
          dftemp <- df %>% 
            #select(Date, Permit_Number, Address, Description) %>% 
            {if (input$geography=="SNbhd") filter(., df$SuperNeighborhood==input$nbhd) else .} %>% 
            {if (input$geography=="Zip")   filter(., df$Zip==input$ZipCode) else .}
          #     -All- neighborhood
          if(input$geography=="SNbhd" & input$nbhd=="-All-") {
            dftemp <- df
          }
          #     Date - with an elaborate dance to alway window a weeks worth
          dftemp <- dftemp %>%
            filter(between(Date, min(input$dateRange[1], input$dateRange[2]-7), 
                                 max(input$dateRange[2], input$dateRange[1]+7)))
          #     Standard search on Description
          if (input$std_search != "All" & input$filter=="Std"){
            dftemp <- dftemp %>% 
              filter(grepl(input$std_search, Description))
          }
          #     Arbitrary search on Description
          if (input$filter=="Arb"){
            dftemp <- dftemp %>% 
              filter(grepl(input$arb_search, Description, ignore.case=TRUE))
          }
          
          #     Add rownumber index
          dftemp <- tibble::rowid_to_column(dftemp, "ID")
          
          #     If markers > 1000, refuse to draw. It takes too long.
          if (nrow(dftemp)>1000){
            showNotification("More than 1000 markers. Too many - app would be very slow",
                             type="warning")
          }
          #     Draw markers if there are any to draw
          #     and data table
          if (nrow(dftemp)>0 & nrow(dftemp)<=1000){
             leafletProxy("LocalMap") %>% 
               addMarkers(dftemp$lon, dftemp$lat, label=htmlEscape(dftemp$Description))
            #   add red markers if selected in table
            ids <- input$table_rows_selected
            if (length(ids)>0){
              print(paste("--1-- ids ", ids))
              print(paste("--2-- lon ", dftemp[ids,]$lon))
             leafletProxy("LocalMap") %>% 
              addMarkers(dftemp[ids,]$lon, dftemp[ids,]$lat, label=htmlEscape(dftemp[ids,]$Description),
                          icon=colorIcons["red"])
            }
            output$table <- DT::renderDataTable(dftemp[, c("ID",
                                                           "Date", 
                                                           "Permit_Number", 
                                                           "Address", 
                                                           "Description")],
                                                server=TRUE)
        ###output$table = DT::renderDataTable(df, server = FALSE)
        ###output$y12 = renderPrint(input$x12_rows_selected)
          }
        })
#        observe({
#            if (!is.null(input$quality)){
#                leafletProxy("LocalMap") %>% 
#                    clearShapes()  
#                for (i in 1:nrow(OldDF)){
#                    if (OldDF[i,]$Quality %in% input$quality) {
#                        leafletProxy("LocalMap") %>% 
#                            addPolylines(
#                                lat=c(OldDF[i,]$GPSLatitude, OldDF[i,]$EndLat),
#                                lng=c(OldDF[i,]$GPSLongitude, OldDF[i,]$EndLon),
#                                color=OldDF[i,]$Codes,
#                                weight=wgt,
##                                popup = popupImage(OldDF[i,]$SourceFile, src="remote",
#                                                   height=150,
#                                                   width=150),
#                                opacity=1
#                            )
#                    }
#                }
#            } 
#            else {
#                leafletProxy("LocalMap") %>% 
#                    clearShapes()  
#            }
#        })
        
        ##################################
        #  Select all and Unselect all
        ##################################
        #      Select    
#        observeEvent(input$selectAll, {
#            if (input$selectAll>0){ # don't run on initialization
#                updateCheckboxGroupInput(session = session, 
#                                         inputId = "quality", 
#                                         selected = colorDF$Quality)
#            }
#        }, ignoreNULL=FALSE)
#        
#        #      Unselect    
#        observeEvent(input$deselectAll, {
#            if (input$deselectAll>0){ # don't run on initialization
#                updateCheckboxGroupInput(session = session, 
#                                         inputId = "quality", 
#                                         selected = "")
#            }
#        }, ignoreNULL=FALSE)
        
    }
)