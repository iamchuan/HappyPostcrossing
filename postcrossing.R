library(jsonlite)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)

rm(list = ls())

# country <- stream_in(file("~/Workspace/scrapy/postcrossing/country.json"))
# 
# folder_dir <- "~/Workspace/scrapy/postcrossing/"
# 
# postcard <- NULL
# 
# for(filename in dir(folder_dir)) {
#   if(grepl("2016-11-", filename)) {
#     if(is.null(postcard)) {
#       postcard <- stream_in(file(paste0(folder_dir, filename)))
#     } else {
#       postcard <- rbind(postcard, 
#                         stream_in(file(paste0(folder_dir, filename))))
#     }
#   }
# }
#     
# postcard <- unique(postcard) %>% filter(to_date == "12 Nov, 2016")
# 
# str2int <- function(x) {
#   as.integer(gsub(",", "", gsub("[[:space:]]+[[:alpha:]]+", "", x)))
# } 
# 
# country$postcards <- str2int(country$postcards)
# country$members <- str2int(country$members)
# country$population <- str2int(country$population)
# country <- country %>% filter(members > 0)
# 
# postcard$distance <- str2int(postcard$distance)
# postcard$travel_time <- str2int(postcard$travel_time)
# postcard$from_lat <- as.numeric(postcard$from_lat)
# postcard$from_lng <- as.numeric(postcard$from_lng)
# postcard$to_lat <- as.numeric(postcard$to_lat)
# postcard$to_lng <- as.numeric(postcard$to_lng)


postcard <- readRDS("./postcard.rds")
country <- readRDS("./country.rds")

# barchart: postcards by country
PostcardCount <- country %>% 
  top_n(10, postcards) %>%
  mutate(postcards = postcards / 1e6)
PostcardCount$country <-factor(PostcardCount$country, 
                               levels=PostcardCount$country[order(PostcardCount$postcards)])
ggplotly(
  ggplot(PostcardCount, 
       aes(country, postcards)) +
  geom_bar(fill= rep(c("#3279BA", "#DA2349"),5), stat="identity", colour="white") +
  coord_flip() +
  xlab("Country (Top10)") + ylab("Nnumber of Postcards (sent), Million ") +
  ggtitle("Postcards Exchanged by Country (2005-2016)") + 
  theme_bw())

# barchart: members by country
MemberCount <- country %>% 
  top_n(10, members)
MemberCount$country <-factor(MemberCount$country, 
                               levels=MemberCount$country[order(MemberCount$members)])
ggplotly(
  ggplot(MemberCount, 
         aes(country, members)) +
    geom_bar(fill= rep(c("#3279BA", "#DA2349"),5), stat="identity", colour="white") +
    coord_flip() +
    xlab("Country (Top10)") + ylab("Nnumber of Postcrossers") +
    ggtitle("Postcrossers by Country (2005-2016)") + 
    theme_bw())

# barchart: density by country
country <- country %>% 
  mutate(density=1000 * members/population)

Density <- country %>% top_n(10, density)

Density$country <-factor(Density$country, 
                         levels=Density$country[order(Density$density)])

ggplotly(
  ggplot(Density, aes(country, density)) + 
    geom_bar(fill= rep(c("#3279BA", "#DA2349"),5), stat="identity", colour="white") +
    coord_flip() +
    xlab("Country (Top10)") + ylab("#Postcrossers in Every 1000 People") +
    ggtitle("Density of Postcrossers by Country (2005-2016)") + 
    theme_bw())

# barchart: pcpermem by country
country <- country %>% 
  mutate(pcpermem = postcards/members)

Permem <- country %>% 
  filter(members > 1000) %>% 
  top_n(10, pcpermem)

Permem$country <-factor(Permem$country, 
                        levels=Permem$country[order(Permem$pcpermem)])

ggplotly(
  ggplot(Permem, aes(country, pcpermem)) +
    geom_bar(fill= rep(c("#3279BA", "#DA2349"),5), stat="identity", colour="white") +
    coord_flip() +
    xlab("Country (Top10)") + ylab("#Postcards (sent) Per Postcrosser") +
    ggtitle("#Postcards Sent per Postcrosser by Country (2005-2016)") +
    theme_bw())


## leaflet 1

from_icon <- icons(
  iconUrl = "https://static1.postcrossing.com/favicon.ico",
  iconWidth = 32, iconHeight = 32)

to_icon <- icons(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/social-media-pro-icons/1080/Fancy-01-128.png",
  iconWidth = 32, iconHeight = 32)

domain = "https://www.postcrossing.com"

post_popup <- paste0("<a href='", 
                     postcard$card_url,
                     "'><img src='", 
                     postcard$img_url, 
                     "' width=200px></a><br><a href='",
                     domain, "/user/",
                     postcard$from_user,
                     "'>", postcard$from_user,"</a>",
                     " sent on ", postcard$from_date,
                     "<br><a href='",
                     domain, "/user/",
                     postcard$to_user,
                     "'>", postcard$to_user,"</a>",
                     " received on ", postcard$to_date)

leaflet_crossing <- leaflet(postcard) %>%
  addProviderTiles("Thunderforest.Transport") %>%
  addMarkers(~from_lng, ~from_lat, 
             clusterOptions = markerClusterOptions(),
             icon = from_icon, popup = post_popup,
             group = "From") %>%
  addMarkers(~to_lng, ~to_lat, 
             clusterOptions = markerClusterOptions(),
             icon = to_icon, popup = post_popup,
             group = "To") %>%
  addLayersControl(overlayGroup = c("From", "To"),
                   options = layersControlOptions(collapsed = FALSE))

leaflet_crossing


## route
library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(plyr)

#rm(list = ls())

routes_all <- postcard %>% select(from_lat, from_lng, to_lat, to_lng)

fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}

routes = gcIntermediate(routes_all[,c('from_lng', 'from_lat')], 
                        routes_all[,c('to_lng', 'to_lat')], 
                        200, breakAtDateLine = FALSE, addStartEnd = TRUE, sp=TRUE)

fortifiedroutes = fortify.SpatialLinesDataFrame(routes) 

#### merge to form great circles

#### creat the backgroud urbanworld map

urbanareasin <- readShapePoly("~/Downloads/Map/ne_10m_urban_areas/ne_10m_urban_areas.shp")
worldmap <- map_data ("world")

wrld<-c(geom_polygon(aes(long,lat,group=group), 
                     size = 0.1, colour= "#090D2A", fill="#090D2A", 
                     alpha=0.8, data=worldmap))

urb <- c(geom_polygon(aes(long, lat, group = group),
                      size = 0.3,
                      color = "#ffffff",
                      fill = "#ffffff",
                      alpha = 1,
                      data = urbanareasin))

ggplot() + wrld + urb +
  geom_line(aes(long, lat, group=id, colour="#DA2349"), 
            alpha = 0.05, size=0.5, data=fortifiedroutes) + 
  theme(panel.background = element_rect(fill='#00001C',colour='#00001C'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="none") + 
  guides(fill = guide_legend(keywidth = 20, keyheight = 20))


ggplot(postcard, aes(distance, travel_time)) + 
  geom_point(color = ifelse(postcard$from_country == postcard$to_country,
                            "#3279BA", "#DA2349"),
             alpha = ifelse(postcard$from_country == postcard$to_country,
                            .5, .1)) +
  xlab("Travel Distance") + ylab("Travel Time") +
  ggtitle("Postcards Travel Distance vs. Travel Time") +
  theme_bw()



  

library(maps)
map_world <- map("world", fill = TRUE, plot = FALSE)

map_attr <- function(col_to_add) {
  col_val <- rep(NA, length(map_world$names))
  for(m in country$country) {
    col_val[grep(m, map_world$names)] <- country[country$country == m, 
                                                 col_to_add]
  }
  col_val
}

map_attr("population")
map_world$population <- map_attr("population")
map_world$postcards <- map_attr("postcards")

leaflet(map_world) %>% addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              fillColor = heat.colors(6, alpha = NULL))

qpal <- colorQuantile("RdYlBu", map_world$postcards, n = 7)
leaflet(map_world) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~qpal(postcards)
  )

saveRDS(postcard, "~/Desktop/postcard.rds")
