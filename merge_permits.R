
#     Merge weekly permit files together

#     Cron looks like this:
## * 16  *   *   2    /usr/lib/R/bin/Rscript '/home/ajackson/Dropbox/Rprojects/CityPermits/ReadPermits.R'  >> '/home/ajackson/Dropbox/Rprojects/CityPermits/ReadPermits.log' 2>&1
## * 20  *   *   2    /usr/lib/R/bin/Rscript '/home/ajackson/Dropbox/Rprojects/CityPermits/merge_permits.R'  >> '/home/ajackson/Dropbox/Rprojects/CityPermits/MergePermits.log' 2>&1
## * 21  *   *   2    /usr/bin/weex MyLinux




library(tidyverse)
library(lubridate)
library(sf)

googlecrs <- 4326


inpath <- "/home/ajackson/Dropbox/Rprojects/CityPermits"
path <- "/home/ajackson/mirrors/ajackson/Permits/data"

# Read files in, do date and numeric conversion, and then join together

DF <- list.files(path=inpath, pattern="^20[12].*rds$") %>%
  purrr::set_names(.) %>%
  map_dfr(~ readRDS(file.path(inpath, .))) %>%
  rename(Description=Comments, Date=Permit_Date) %>% 
  mutate(Date=as.Date(Date, format= "%Y/%m/%d"),
         lat=as.numeric(lat),
         lon=as.numeric(lon))

#   fix clutch city

DF$match <- str_replace(DF$match, "CLUTCH CITY", "HOUSTON")

#   filter out rows with bad coordinates
DF <- DF %>% filter(!(is.na(DF$lat) | is.na(DF$lon)))

#  Create a temporary sf data frame for doing the intersects
# set longitudes as the first column and latitudes as the second
dat <- data.frame(Longitude=DF$lon, Latitude=DF$lat, match=DF$match, stringsAsFactors = FALSE)

dat <- st_as_sf(dat, coords=c("Longitude", "Latitude"), crs=googlecrs, agr = "identity")

# prep superneighborhoods

temp <- readRDS("/home/ajackson/Dropbox/CrimeStats/SuperNeighborhoodPolys.rds") %>% 
  mutate(SNBNAME=str_to_title(SNBNAME))
#   find points in polygons
#   since superneighborhoods don't overlap, let's just grab the array index
#   instead of creating a huge matrix
a <- st_intersects(dat, temp, sparse = TRUE)

# Replace empty values with 89
a <- unlist(replace(a, !sapply(a, length),89))
# Now add super neighborhood 89 as NA
temp[89,] <- temp[88,]
temp$SNBNAME[89] <- "None"

#   Append the super neighborhood field to the data frame
DF$SuperNeighborhood <- temp$SNBNAME[a]

###   save master file

saveRDS(DF, paste0(path, "/MasterPermits.rds"))
