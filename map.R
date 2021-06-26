library("cartography")
library("sf")
# library("tidyversey")
library(stringr)
library(ggplot2)

setwd("C:/Users/Anmol/Desktop/Covid-19-Nepal")

data <- st_read("hermes_NPL_new_wgs/hermes_NPL_new_wgs_2.shp")

getDistrictData <- function() {
  districtData <-
    read.csv("districtData.csv")
  return (districtData)
}
districtData <- getDistrictData()

for (i in districtData[2]) {
  districtData[2] = sapply(strsplit(as.character(i), " "), `[`, 2)
}

districtData <-
  subset(districtData,
         select = c(District, Value))

districtData$District <- str_to_title(districtData$District)

# Rename district values here
districtData$District[districtData$District == "Makwanpur"] <-
  "Makawanpur"
districtData$District[districtData$District == "Dhanusa"] <-
  "Dhanusha"
districtData$District[districtData$District == "Kavrepalanchok"] <-
  "Kabhrepalanchok"
districtData$District[districtData$District == "Rukum"][1] <-
  "Rukum East"
districtData$District[districtData$District == "Rukum"][1] <-
  "Rukum West"
districtData$District[districtData$District == "Nawalparasi"][1] <-
  "Nawalpur"
districtData$District[districtData$District == "Nawalparasi"][1] <-
  "Parasi"


colnames(districtData) <- c('DISTRICT', 'Value')
mapData <- merge(data, districtData, by = "DISTRICT")

plot(st_geometry(mapData))
plot(st_geometry(mapData),
     col = sf.colors(12, categorical = TRUE),
     border = 'grey')
choroLayer(
  x = mapData,
  var = "Value",
  method = "quantile",
  nclass = 5,
  legend.title.txt = "Covid Infected Value"
)
layoutLayer(
  title = "Covid-19 infecred Values",
  tabtitle = TRUE,
  frame = TRUE,
  scale = 6
)

points <-
  cbind(mapData, st_coordinates(st_centroid(mapData$geometry)))

mapData <- as.data.frame(mapData)

rangeValue <- seq(from = 1000, to = 202471, by = 5000)

ggplot(data = points, colour = Value) +
  geom_sf(aes(fill = Value), color = "grey", size = 0.2) +
  geom_text(data = points,
            aes(
              x = X,
              y = Y,
              label = (paste(DISTRICT)),
              vjust = +1,
            )) +
  scale_fill_gradient(
    high = "red",
    low = "#ffffb3",
    breaks = rangeValue,
    name = "Infected Cases Range"
  ) +
  guides(fill = guide_colorbar(barheight = 40)) +
  ggtitle("Covid-19 District-wise Infected Cases") + xlab("Longitude") + ylab("Latitude")


ggplot(data = points, colour = Value) +
  geom_sf(aes(fill = Value), color = "grey", size = 0.2) +
  geom_text(data = points,
            aes(
              x = X,
              y = Y,
              label = (paste(Value)),
              vjust = +1,
            )) +
  scale_fill_gradient(
    high = "red",
    low = "#ffffb3",
    breaks = rangeValue,
    name = "Infected Cases Range"
  ) +
  guides(fill = guide_colorbar(barheight = 40)) +
  ggtitle("Covid-19 District-wise Infected Cases") + xlab("Longitude") + ylab("Latitude")


# Province Wise Map

data <- st_read("hermes_NPL_new_wgs/hermes_NPL_new_wgs_1.shp")

districtData <- getDistrictData()

districtData <-
  subset(districtData,
         select = c(Province, Value))

districtData <-  aggregate(list(Value = as.numeric(districtData$Value)), by = (list(ProvinceName = districtData$Province)), sum)

districtData$PROVINCE <- c(3,4,6,5,1,2,7)

mapData <- merge(data, districtData, by = "PROVINCE")

points <-
  cbind(mapData, st_coordinates(st_centroid(mapData$geometry)))
mapData <- as.data.frame(mapData)

provinceRangeValue <- seq(from = 10000, to = 322764, by = 10000)

map<-ggplot(data = points, colour = Value) +
  coord_equal(expand=FALSE) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(aes(fill = Value), color = "grey", size = 0.2) +
  geom_text(data = points,
            aes(
              x = X,
              y = Y,
              label = (paste(Value)),
              vjust = +1,
            )) +
  geom_text(data = points,
            aes(
              x = X,
              y = Y,
              label = (paste(PR_NAME)),
              vjust = -1,
            ),
            size = 4,
            fontface = "bold") +
  scale_fill_gradient(
    high = "red",
    low = "#ffffb3",
    breaks = provinceRangeValue,
    name = "Infected Cases Range"
  ) +
  guides(fill = guide_colorbar(barheight = 40)) +
  ggtitle("Covid-19 Province-wise Infected Cases") + xlab("Longitude") + ylab("Latitude")

map
