#install.packages("plotly")
#install.packages("ggmap")
#
library(shiny)
library(readr)
library(plyr)
library(plotly)
library(sp)
library(rworldmap)
library(maps)
library(ggmap)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

hotel <- read.csv("Hotel_Reviews.csv")

nrow(hotel)
#Clean data and give id numbers
hotel <- na.omit(hotel)
hotel$id <- 1:nrow(hotel)

#TODO check for additional cleaning
hotel$Negative_Review[hotel$Negative_Review == "No Negative"] <- NA
hotel$Negative_Review[hotel$Negative_Review == " "] <- NA

hotel$Positive_Review[hotel$Positive_Review == 'No Positive'] <- NA
hotel$Positive_Review[hotel$Positive_Review == " "] <- NA
#sapply(hotel, function(hotel) ((sum(is.na(hotel)))))

#Label hotels per country
hotel$Country <- map.where(database="world", hotel$lng, hotel$lat)

#Select hotels from the Netherlands
hotel.NL <- subset(hotel, Country=="Netherlands")
hotel.NL$id <- 1:nrow(hotel.NL)

# create map
markers <- hotel.NL[-c(3,6,7,8,10,11,12,13,14,15,18)]
markers <- unique(markers)
markers$id <- seq.int(nrow(markers))

# create icon
markerHotel <- makeAwesomeIcon(icon = "map-marker", markerColor = "blue")

# show the map
showmap <- function() {
  leaflet() %>%
    addTiles() %>%
    setView(lng=4.893, lat=52.362, zoom=11) %>%
    addAwesomeMarkers(markers$lng, markers$lat, icon = markerHotel, group = "Hotels", layerId = markers$id, label = markers$Hotel_Name) %>%
    addResetMapButton() %>%
    addSearchFeatures(
      targetGroups = markers$Hotel_Name,
      options = searchFeaturesOptions(firstTipSubmit = TRUE)
    )
}
#showmap()

#merge the markers and hotel dataframes for matching id's
for (i in 1: length(hotel.NL$id)) {
  hotel.NL$id[i] <- markers$id[hotel.NL$Hotel_Address[i] == markers$Hotel_Address]
}

#Create Pie chart for for all hotels
pie.NL <- plot_ly(hotel.NL, labels = c("Negative", "Positive"), values = ~c(length(Negative_Review[!is.na(Negative_Review)]),length(Positive_Review[!is.na(Positive_Review)])) , type = 'pie')

#Create Pie chart per hotel
PiePerHotel <- function(id){
  hotel.Filter <- hotel.NL[hotel.NL$id == id,]
  plot_ly(hotel.Filter, labels = c("Negative", "Positive"), values = ~c(length(Negative_Review[!is.na(Negative_Review)]),length(Positive_Review[!is.na(Positive_Review)])) , type = 'pie') %>% layout(title = hotel.Filter$Hotel_Name)
# NEW: line above (title of plot)
  }

data <- hotel.NL[,c('Positive_Review', 'Negative_Review')]
#PiePerHotel(3)

ShowReviews <- function(id){
  hotel.Filter <- hotel.NL[hotel.NL$id == id,]
  data <- hotel.Filter[,c('Positive_Review', 'Negative_Review')]
  DT::datatable({
    data
  }, rownames = FALSE)
}
#ShowReviews(3)

