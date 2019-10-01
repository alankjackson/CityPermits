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


###################################
#   get and set up the basic data
###################################

#   Directory where data is stored

DataLocation <- "https://www.ajackson.org/Permits/data/"

#   Tibble database

DF <- readRDS(gzcon(url(paste0(DataLocation, "MasterPermits_toGini.rds"))))

MapCenter <- c(-95.363345, 29.756555) # center on downtown

init_zoom <- 14

NeighborCentroids <- readRDS(gzcon(url(paste0(DataLocation, 
                                              "SuperNeighborhoodCentroids.rds"))))

Superneighborhoods <- NeighborCentroids$SNBNAME 


##################################################
# Define UI for displaying and annotating photos
##################################################

shinyApp(
    ui = basicPage(
        fluidRow(
            column(width = 8,
                   leafletOutput("LocalMap"),
                   HTML("<hr>")
                   #   put ggplot graphic here
            ),
            
            #       Add right column with controls for display
            column(width = 2,
                   #    Select Neighborhood
                   selectInput("nbhd", "Choose a Neighborhood:",
                               Superneighborhoods,
                               selected="Downtown"
                   ),
##########################   stopped here
                   checkboxGroupInput("quality", label = "Sidewalk qualities to display",
                                      choices = list("Good"="Good", 
                                                     "Bushes"="Bushes", 
                                                     "Gap"="Gap",  
                                                     "Offset"="Offset", 
                                                     "Shattered"="Shattered", 
                                                     "Obstructed"="Obstructed", 
                                                     "Debris/Mud"="Debris/Mud", 
                                                     "Gravel"="Gravel", 
                                                     "No Curb Cut"="No Curb Cut", 
                                                     "Missing"="Missing"),
                                      selected = "Good" ),
                   #    Select or unselect all
                   HTML("<hr>"),
                   actionButton("selectAll", label = "Select All"),
                   actionButton("deselectAll", label = "Deselect All") ,
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
            ll <- st_coordinates(NeighborCentroids[NeighborCentroids$SNBNAME==input$nbhd,]$geometry)
            leaflet(DF) %>% 
                setView(lng = ll[1] , lat = ll[2], zoom = init_zoom ) %>%   
                addTiles()
        }) 
        
        ##################################
        #           Lines
        ##################################
#        observe({
#            #   Polylines (done in a loop to keep them from connecting)
#            #   scale width of lines in concert with zoom scale
#            wgt <- max(2,floor(2*(input$LocalMap_zoom-init_zoom)+0.5) + init_weight)
#            #print(paste("weight:",wgt))
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