#install.packages("maptools")
#install.packages("classInt")
#install.packages("RColorBrewer")
#install.packages("spatstat")
#install.packages("nlme")
#install.packages("rpart")

library(ggmap)
library(nlme)
library(rpart)
library(spatstat)
library(sp)
library(rgeos)

setwd ("V:/My Documents/r-projects/Crime") #set working directory

crime <- read.csv(file = "SalfordCrime.csv", header = TRUE, sep = ",") #Bring in CSV crime file 

#Change class of longitude and latitude so SpacialPointsDataFrame can be used
coords <- cbind(Longitude = as.numeric(as.character(crime$Longitude)), Latitude = as.numeric(as.character(crime$Latitude)))
#SPDF - coords from line above, df minus long &lat
crime.pts <- SpatialPointsDataFrame(coords, crime[, -(5:6)], proj4string = CRS("+init=epsg:4326"))
#plot the crime points
plot(crime.pts, pch = ".", col = "darkred")


#Plot Manchester Google map
map <- qmap(location = "Salford", maptype = "hybrid", zoom = 11)
#plot crime points on top of map 
map + geom_point(data = crime, aes(x = Longitude, y = Latitude), color="red", size = 0.5, alpha=0.5)

###### Heat map style map

#Create density map from crime.pts variable
sSp <- as(SpatialPoints(crime.pts), "ppp")
Dens <- density(sSp, adjust = 0.2)
class(Dens)

plot(Dens)
contour(density(sSp, adjust = 0.2), nlevels = 4)

#categorise density
Dsg <- as(Dens, "SpatialGridDataFrame")
Dim <- as.image.SpatialGridDataFrame(Dsg)
Dcl <- contourLines(Dim)
SLDF <- ContourLines2SLDF(Dcl)
proj4string(SLDF) <- proj4string(crime.pts)

#remove the entries that are 0 and plot heat map
SLDF <- SLDF[SLDF@data$level != 0,]
plot(SLDF, col = terrain.colors(5), lwd = 5)
plot(crime.pts, pch = ".", col = "red", add = T)

#add highlight for highest areas
Polyclust <- gPolygonize(SLDF[5, ]) #number from 1-8 to alter which contour is filled in
gas <- gArea(Polyclust, byid = T)/10000
Polyclust <- SpatialPolygonsDataFrame(Polyclust, data = data.frame(gas), match.ID = F)
#summarise data
cAg <- aggregate(crime.pts, by = Polyclust, FUN = length)
plot(SLDF, col = terrain.colors(8))
plot(cAg, col = "red", add = T)


download.file("https://data.cdrc.ac.uk/dataset/cdrc-2015-os-geodata-pack-salford-e08000006","E08000006.zipZIP ")
unzip("E08000006.zipZIP ")

salford <- readShapeSpatial("E08000006")


