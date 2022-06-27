library(tidyverse)
library(lubridate)
library(moveVis)
library(move)
library(raster)
library(ggplot2)

#import data file as exported, filter for location quality, and rename key columns
Data <- read.csv("ArgosData_2022_06_24_14_53_34.csv", header=T, check.names=T)
Qual <- c("3", "2", "1")
Data <- Data %>%
  rename(PlatformID = ï..Platform.ID.No., MsgDate = Msg.Date, LocDate = Loc..date, LocQuality = Loc..quality)%>%
  filter (LocQuality==Qual)%>%
  mutate(MsgDate = mdy_hms(MsgDate), LocDate = mdy_hms(LocDate))

move_df <- as.data.frame(Data)
move_df <- methods::as(move_df, "data.frame")

move_df <- distinct(move_df, move_df$MsgDate, .keep_all = TRUE)

# use df2move to convert the data.frame into a moveStack
m <- df2move(move_df,
        proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
        x = "Longitude", y = "Latitude", time = "MsgDate", track_id = "PlatformID")

# align move_data to a uniform time scale
m <- align_move(m, res = 2, unit = "hours")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
  add_labels(x = "Latitude", y = "Longitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()

#create spatial frames with a mapbox satelite map
##switch trace_show if needed
frames <- frames_spatial(m, path_colours = c("red"), trace_show = T,
                         map_service = "mapbox", map_type = "satellite",
                         map_token = "pk.eyJ1Ijoic3RlcGhlbnJlc2VhcmNoIiwiYSI6ImNrdmF3dG8yaTNocDIyb3Exd3hhODVtZnMifQ.uqy7IDlqM6PnGECKBvf0Dw", alpha = 0.5) %>%
  add_labels(x = "Latitude", y = "Longitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()


frames[[25]] # preview one of the frames, e.g. the 25th frame

# animate frames
animate_frames(frames, out_file = "to 20220624 track sat trace.gif")
