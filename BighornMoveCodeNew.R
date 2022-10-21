rm(list=ls())
library(adehabitatLT)
#install.packages("adehabitatLT")
library(move)
#install.packages("move")
library(circular)
#install.packages("circular")
library(sp)
#install.packages("sp")
library(maptools)
#install.packages("maptools")
library(adehabitatHR)
#install.packages("adehabitatHR")
library(PBSmapping)
#install.packages("PBSmapping")
library(psych)

setwd("C:/Users/tywer/OneDrive/TyFiles/Chapter3/R/BBMMMinusWeekOne")

bighorns <-read.csv("C:/Users/tywer/OneDrive/TyFiles/Chapter3/R/BBMMMinusWeekOne/AllBighornLocationsOriginalMinusWeekOne.csv",colClasses=c("character","character","character","numeric","numeric","character","numeric","numeric","numeric","numeric","factor","numeric","numeric","numeric"))

#str(bighorns)

#FOR ALL Bighorn Sheep
bighorns$DT <-as.POSIXct(strptime(bighorns$GPSFixTime, format='%Y.%m.%d %H:%M:%OS',tz="MST"))
# check that we don't have NA
which(is.na(bighorns$DT))

#Sort data to make sure everything is grouped by bighorn and time series
bighorns <- bighorns[order(bighorns$id,bighorns$DT),]
bighorns$id <- factor(bighorns$id)
#bighorns[1:300,]
str(bighorns)
#summary(bighorns$id)

# get time difference between different timestamps 
bighorns$timediff <- c(0,diff(bighorns$DT))

 # get rid of the first observation for each bighorn ID
bighorns <- bighorns[-match(levels(bighorns$id), bighorns$id),]
dim(bighorns)


#Create a move object for all Bighorn Sheep using the \emph{Move} package
loc <- move(x=bighorns$X, y=bighorns$Y, time=bighorns$DT, proj=CRS("+proj=utm"),data=bighorns,animal=bighorns$id)

#Now create a dBBMM object
BH_dbbmm <- brownian.bridge.dyn(object=loc, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
BH_dbbmm$BH223
plot(BH_dbbmm$BH223)
contour(BH_dbbmm, levels=c(.5,.9,.99), add=T)
plot(contour)




#BH223
# for a single Bighorn Sheep
# both years
which.sheep <- "BH223"
my.subset <- data.frame(bighorns[bighorns$id == which.sheep,])
my.move.obj <- move(x= my.subset $X, y= my.subset $Y, time= my.subset $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset, animal= my.subset $id)
single.animal.BH_dbbmm <- brownian.bridge.dyn(object= my.move.obj, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm)
contour(single.animal.BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm)

# year 1
my.subset.Y1 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==1,])
my.move.obj.Y1.223 <- move(x= my.subset.Y1 $X, y= my.subset.Y1 $Y, time= my.subset.Y1 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y1, animal= my.subset.Y1 $id)
single.animal.BH_dbbmm.Y1 <- brownian.bridge.dyn(object= my.move.obj.Y1, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y1)
contour(single.animal.BH_dbbmm.Y1, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y1)

# year 2
my.subset.Y2 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==2,])
my.move.obj.Y2.223 <- move(x= my.subset.Y2 $X, y= my.subset.Y2 $Y, time= my.subset.Y2 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y2, animal= my.subset.Y2 $id)
single.animal.BH_dbbmm.Y2 <- brownian.bridge.dyn(object= my.move.obj.Y2, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y2)
contour(single.animal.BH_dbbmm.Y2, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y2)
#BH232
# for a single Bighorn Sheep
# both years
which.sheep <- "BH232"
my.subset <- data.frame(bighorns[bighorns$id == which.sheep,])
my.move.obj <- move(x= my.subset $X, y= my.subset $Y, time= my.subset $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset, animal= my.subset $id)
single.animal.BH_dbbmm <- brownian.bridge.dyn(object= my.move.obj, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm)
contour(single.animal.BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm)

# year 1
my.subset.Y1 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==1,])
my.move.obj.Y1.232 <- move(x= my.subset.Y1 $X, y= my.subset.Y1 $Y, time= my.subset.Y1 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y1, animal= my.subset.Y1 $id)
single.animal.BH_dbbmm.Y1 <- brownian.bridge.dyn(object= my.move.obj.Y1, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y1)
contour(single.animal.BH_dbbmm.Y1, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y1)

# year 2
my.subset.Y2 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==2,])
my.move.obj.Y2.232 <- move(x= my.subset.Y2 $X, y= my.subset.Y2 $Y, time= my.subset.Y2 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y2, animal= my.subset.Y2 $id)
single.animal.BH_dbbmm.Y2 <- brownian.bridge.dyn(object= my.move.obj.Y2, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y2)
contour(single.animal.BH_dbbmm.Y2, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y2)
#BH233
# for a single Bighorn Sheep
# both years
which.sheep <- "BH233"
my.subset <- data.frame(bighorns[bighorns$id == which.sheep,])
my.move.obj <- move(x= my.subset $X, y= my.subset $Y, time= my.subset $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset, animal= my.subset $id)
single.animal.BH_dbbmm <- brownian.bridge.dyn(object= my.move.obj, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm)
contour(single.animal.BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm)

# year 1
my.subset.Y1 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==1,])
my.move.obj.Y1.233 <- move(x= my.subset.Y1 $X, y= my.subset.Y1 $Y, time= my.subset.Y1 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y1, animal= my.subset.Y1 $id)
single.animal.BH_dbbmm.Y1 <- brownian.bridge.dyn(object= my.move.obj.Y1, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y1)
contour(single.animal.BH_dbbmm.Y1, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y1)

# year 2
my.subset.Y2 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==2,])
my.move.obj.Y2.233 <- move(x= my.subset.Y2 $X, y= my.subset.Y2 $Y, time= my.subset.Y2 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y2, animal= my.subset.Y2 $id)
single.animal.BH_dbbmm.Y2 <- brownian.bridge.dyn(object= my.move.obj.Y2, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y2)
contour(single.animal.BH_dbbmm.Y2, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y2)
#BH239
# for a single Bighorn Sheep
# both years
which.sheep <- "BH239"
my.subset <- data.frame(bighorns[bighorns$id == which.sheep,])
my.move.obj <- move(x= my.subset $X, y= my.subset $Y, time= my.subset $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset, animal= my.subset $id)
single.animal.BH_dbbmm <- brownian.bridge.dyn(object= my.move.obj, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm)
contour(single.animal.BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm)

# year 1
my.subset.Y1 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==1,])
my.move.obj.Y1.239 <- move(x= my.subset.Y1 $X, y= my.subset.Y1 $Y, time= my.subset.Y1 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y1, animal= my.subset.Y1 $id)
single.animal.BH_dbbmm.Y1 <- brownian.bridge.dyn(object= my.move.obj.Y1, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y1)
contour(single.animal.BH_dbbmm.Y1, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y1)

# year 2
my.subset.Y2 <- data.frame(bighorns[bighorns$id == which.sheep & bighorns$Year==2,])
my.move.obj.Y2.239 <- move(x= my.subset.Y2 $X, y= my.subset.Y2 $Y, time= my.subset.Y2 $DT, proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data= my.subset.Y2, animal= my.subset.Y2 $id)
single.animal.BH_dbbmm.Y2 <- brownian.bridge.dyn(object= my.move.obj.Y2, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
window()
plot(single.animal.BH_dbbmm.Y2)
contour(single.animal.BH_dbbmm.Y2, levels=c(.5,.9,.95,.99), add=TRUE)
show(single.animal.BH_dbbmm.Y2)


window()

par(mfrow=c(2,2))
mtext("My Title", side = 3, line = 0, outer = T)
plot(my.move.obj.Y1.223, type="o", col="black", lwd=2, pch=20, xlim=c(xmin(my.move.obj.Y2.223), xmax(my.move.obj.Y1.223)), ylim=c(ymin(my.move.obj.Y2.223), ymax(my.move.obj.Y1.223)))
points(my.move.obj.Y2.223, type="o", col=gray(.5), lwd=2, pch=20, xlab="location_east",ylab="location_north")

plot(my.move.obj.Y1.232, type="o", col="black", lwd=2, pch=20,xlim=c(xmin(my.move.obj.Y1.232), xmax(my.move.obj.Y1.232)), ylim=c(ymin(my.move.obj.Y1.232), ymax(my.move.obj.Y1.232)))
points(my.move.obj.Y2.232, type="o", col=gray(.5), lwd=2, pch=20, xlab="location_east",ylab="location_north")

plot(my.move.obj.Y1.233, type="o", col="black", lwd=2, pch=20,xlim=c(xmin(my.move.obj.Y1.233), xmax(my.move.obj.Y1.233)), ylim=c(ymin(my.move.obj.Y1.233), ymax(my.move.obj.Y1.233)))
points(my.move.obj.Y2.233, type="o", col=gray(.5), lwd=2, pch=20, xlab="location_east",ylab="location_north")

plot(my.move.obj.Y1.239, type="o", col="black", lwd=2, pch=20, xlim=c(xmin(my.move.obj.Y2.239), xmax(my.move.obj.Y2.239)), ylim=c(ymin(my.move.obj.Y2.239), ymax(my.move.obj.Y1.239)))
points(my.move.obj.Y2.239, type="o", col=gray(.5), lwd=2, pch=20, xlab="location_east",ylab="location_north")



#### 







#or select a single Bighorn Sheep
#dataBH225 <- subset(bighorns, bighorns$id == "BH225")
#dataBH225$id <- factor(dataBH225$id)
#BH225 <- move(x=dataBH225$X, y=dataBH225$Y, time=as.POSIXct(dataBH225$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH225, animal=dataBH225$id)
#BH_dbbmm <- brownian.bridge.dyn(object=BH225, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
#plot(BH_dbbmm)
#contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
#show(BH_dbbmm)

#Plot the movement of the animal
plot(BH225, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH225_cont <- getVolumeUD(BH225_dbbmm)
BH225_cont50 <- BH225_cont<=.50
BH225_cont95 <- BH225_cont<=.95
area50 <- sum(values(BH225_cont50))
area50
area95 <- sum(values(BH225_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH225_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH225contour50", driver = "ESRI Shapefile")
BH225map.50 <- readOGR(dsn=".", layer="BH225contour50")
plot(BH225map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH225contour95", driver = "ESRI Shapefile")
BH225map.95 <- readOGR(dsn=".", layer="BH225contour95")
plot(BH225map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH225contour99", driver = "ESRI Shapefile")
BH225map.99 <- readOGR(dsn=".", layer="BH225contour99")
plot(BH225map.99)

#BH233


#or select a single Bighorn Sheep
dataBH233 <- subset(bighorns, bighorns$id == "BH233")
dataBH233$id <- factor(dataBH233$id)
BH233 <- move(x=dataBH233$X, y=dataBH233$Y, time=as.POSIXct(dataBH233$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH233, animal=dataBH233$id)
BH233_dbbmm <- brownian.bridge.dyn(object=BH233, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH233_dbbmm)
contour(BH233_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH233_dbbmm)

#Plot the movement of the animal
plot(BH233, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH233_cont <- getVolumeUD(BH233_dbbmm)
BH233_cont50 <- BH233_cont<=.50
BH233_cont95 <- BH233_cont<=.95
BH233_cont99 <- BH233_cont<=.99
area50 <- sum(values(BH233_cont50))
area50
area95 <- sum(values(BH233_cont95))
area95
area99 <- sum(values(BH233_cont99))
area99
##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH233_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH233contour50", driver = "ESRI Shapefile")
BH233map.50 <- readOGR(dsn=".", layer="BH233contour50")
plot(BH233map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH233contour95", driver = "ESRI Shapefile")
BH233map.95 <- readOGR(dsn=".", layer="BH233contour95")
plot(BH233map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH233contour99", driver = "ESRI Shapefile")
BH233map.99 <- readOGR(dsn=".", layer="BH233contour99")
plot(BH233map.99)


#BH256


#or select a single Bighorn Sheep
dataBH256 <- subset(bighorns, bighorns$id == "BH256")
dataBH256$id <- factor(dataBH256$id)
BH256 <- move(x=dataBH256$X, y=dataBH256$Y, time=as.POSIXct(dataBH256$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH256, animal=dataBH256$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH256, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH256, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH256_cont <- getVolumeUD(BH256_dbbmm)
BH256_cont50 <- BH256_cont<=.50
BH256_cont95 <- BH256_cont<=.95
area50 <- sum(values(BH256_cont50))
area50
area95 <- sum(values(BH256_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH256_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH256contour50", driver = "ESRI Shapefile")
BH256map.50 <- readOGR(dsn=".", layer="BH256contour50")
plot(BH256map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH256contour95", driver = "ESRI Shapefile")
BH256map.95 <- readOGR(dsn=".", layer="BH256contour95")
plot(BH256map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH256contour99", driver = "ESRI Shapefile")
BH256map.99 <- readOGR(dsn=".", layer="BH256contour99")
plot(BH256map.99)


#BH239


#or select a single Bighorn Sheep
dataBH239 <- subset(bighorns, bighorns$id == "BH239")
dataBH239$id <- factor(dataBH239$id)
BH239 <- move(x=dataBH239$X, y=dataBH239$Y, time=as.POSIXct(dataBH239$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH239, animal=dataBH239$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH239, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH239, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH239_cont <- getVolumeUD(BH239_dbbmm)
BH239_cont50 <- BH239_cont<=.50
BH239_cont95 <- BH239_cont<=.95
area50 <- sum(values(BH239_cont50))
area50
area95 <- sum(values(BH239_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH239_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH239contour50", driver = "ESRI Shapefile")
BH239map.50 <- readOGR(dsn=".", layer="BH239contour50")
plot(BH239map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH239contour95", driver = "ESRI Shapefile")
BH239map.95 <- readOGR(dsn=".", layer="BH239contour95")
plot(BH239map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH239contour99", driver = "ESRI Shapefile")
BH239map.99 <- readOGR(dsn=".", layer="BH239contour99")
plot(BH239map.99)


#BH234


#or select a single Bighorn Sheep
dataBH234 <- subset(bighorns, bighorns$id == "BH234")
dataBH234$id <- factor(dataBH234$id)
BH234 <- move(x=dataBH234$X, y=dataBH234$Y, time=as.POSIXct(dataBH234$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH234, animal=dataBH234$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH234, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH234, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH234_cont <- getVolumeUD(BH234_dbbmm)
BH234_cont50 <- BH234_cont<=.50
BH234_cont95 <- BH234_cont<=.95
area50 <- sum(values(BH234_cont50))
area50
area95 <- sum(values(BH234_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH234_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH234contour50", driver = "ESRI Shapefile")
BH234map.50 <- readOGR(dsn=".", layer="BH234contour50")
plot(BH234map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH234contour95", driver = "ESRI Shapefile")
BH234map.95 <- readOGR(dsn=".", layer="BH234contour95")
plot(BH234map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH234contour99", driver = "ESRI Shapefile")
BH234map.99 <- readOGR(dsn=".", layer="BH234contour99")
plot(BH234map.99)


#BH230


#or select a single Bighorn Sheep
dataBH230 <- subset(bighorns, bighorns$id == "BH230")
dataBH230$id <- factor(dataBH230$id)
BH230 <- move(x=dataBH230$X, y=dataBH230$Y, time=as.POSIXct(dataBH230$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH230, animal=dataBH230$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH230, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH230, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH230_cont <- getVolumeUD(BH230_dbbmm)
BH230_cont50 <- BH230_cont<=.50
BH230_cont95 <- BH230_cont<=.95
area50 <- sum(values(BH230_cont50))
area50
area95 <- sum(values(BH230_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH230_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH230contour50", driver = "ESRI Shapefile")
BH230map.50 <- readOGR(dsn=".", layer="BH230contour50")
plot(BH230map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH230contour95", driver = "ESRI Shapefile")
BH230map.95 <- readOGR(dsn=".", layer="BH230contour95")
plot(BH230map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH230contour99", driver = "ESRI Shapefile")
BH230map.99 <- readOGR(dsn=".", layer="BH230contour99")
plot(BH230map.99)


#BH223


#or select a single Bighorn Sheep
dataBH223 <- subset(bighorns, bighorns$id == "BH223")
dataBH223$id <- factor(dataBH223$id)
BH223 <- move(x=dataBH223$X, y=dataBH223$Y, time=as.POSIXct(dataBH223$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH223, animal=dataBH223$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH223, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH223, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH223_cont <- getVolumeUD(BH223_dbbmm)
BH223_cont50 <- BH223_cont<=.50
BH223_cont95 <- BH223_cont<=.95
area50 <- sum(values(BH223_cont50))
area50
area95 <- sum(values(BH223_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH223_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH223contour50", driver = "ESRI Shapefile")
BH223map.50 <- readOGR(dsn=".", layer="BH223contour50")
plot(BH223map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH223contour95", driver = "ESRI Shapefile")
BH223map.95 <- readOGR(dsn=".", layer="BH223contour95")
plot(BH223map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH223contour99", driver = "ESRI Shapefile")
BH223map.99 <- readOGR(dsn=".", layer="BH223contour99")
plot(BH223map.99)


#BH232


#or select a single Bighorn Sheep
dataBH232 <- subset(bighorns, bighorns$id == "BH232")
dataBH232$id <- factor(dataBH232$id)
BH232 <- move(x=dataBH232$X, y=dataBH232$Y, time=as.POSIXct(dataBH232$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH232, animal=dataBH232$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH232, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH232, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH232_cont <- getVolumeUD(BH232_dbbmm)
BH232_cont50 <- BH232_cont<=.50
BH232_cont95 <- BH232_cont<=.95
area50 <- sum(values(BH232_cont50))
area50
area95 <- sum(values(BH232_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH232_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH232contour50", driver = "ESRI Shapefile")
BH232map.50 <- readOGR(dsn=".", layer="BH232contour50")
plot(BH232map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH232contour95", driver = "ESRI Shapefile")
BH232map.95 <- readOGR(dsn=".", layer="BH232contour95")
plot(BH232map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH232contour99", driver = "ESRI Shapefile")
BH232map.99 <- readOGR(dsn=".", layer="BH232contour99")
plot(BH232map.99)


#BH236


#or select a single Bighorn Sheep
dataBH236 <- subset(bighorns, bighorns$id == "BH236")
dataBH236$id <- factor(dataBH236$id)
BH236 <- move(x=dataBH236$X, y=dataBH236$Y, time=as.POSIXct(dataBH236$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH236, animal=dataBH236$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH236, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH236, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH236_cont <- getVolumeUD(BH236_dbbmm)
BH236_cont50 <- BH236_cont<=.50
BH236_cont95 <- BH236_cont<=.95
area50 <- sum(values(BH236_cont50))
area50
area95 <- sum(values(BH236_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH236_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH236contour50", driver = "ESRI Shapefile")
BH236map.50 <- readOGR(dsn=".", layer="BH236contour50")
plot(BH236map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH236contour95", driver = "ESRI Shapefile")
BH236map.95 <- readOGR(dsn=".", layer="BH236contour95")
plot(BH236map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH236contour99", driver = "ESRI Shapefile")
BH236map.99 <- readOGR(dsn=".", layer="BH236contour99")
plot(BH236map.99)


#BH258


#or select a single Bighorn Sheep
dataBH258 <- subset(bighorns, bighorns$id == "BH258")
dataBH258$id <- factor(dataBH258$id)
BH258 <- move(x=dataBH258$X, y=dataBH258$Y, time=as.POSIXct(dataBH258$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH258, animal=dataBH258$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH258, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH258, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH258_cont <- getVolumeUD(BH258_dbbmm)
BH258_cont50 <- BH258_cont<=.50
BH258_cont95 <- BH258_cont<=.95
area50 <- sum(values(BH258_cont50))
area50
area95 <- sum(values(BH258_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH258_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH258contour50", driver = "ESRI Shapefile")
BH258map.50 <- readOGR(dsn=".", layer="BH258contour50")
plot(BH258map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH258contour95", driver = "ESRI Shapefile")
BH258map.95 <- readOGR(dsn=".", layer="BH258contour95")
plot(BH258map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH258contour99", driver = "ESRI Shapefile")
BH258map.99 <- readOGR(dsn=".", layer="BH258contour99")
plot(BH258map.99)


#BH228


#or select a single Bighorn Sheep
dataBH228 <- subset(bighorns, bighorns$id == "BH228")
dataBH228$id <- factor(dataBH228$id)
BH228 <- move(x=dataBH228$X, y=dataBH228$Y, time=as.POSIXct(dataBH228$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH228, animal=dataBH228$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH228, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH228, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH228_cont <- getVolumeUD(BH228_dbbmm)
BH228_cont50 <- BH228_cont<=.50
BH228_cont95 <- BH228_cont<=.95
area50 <- sum(values(BH228_cont50))
area50
area95 <- sum(values(BH228_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH228_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH228contour50", driver = "ESRI Shapefile")
BH228map.50 <- readOGR(dsn=".", layer="BH228contour50")
plot(BH228map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH228contour95", driver = "ESRI Shapefile")
BH228map.95 <- readOGR(dsn=".", layer="BH228contour95")
plot(BH228map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH228contour99", driver = "ESRI Shapefile")
BH228map.99 <- readOGR(dsn=".", layer="BH228contour99")
plot(BH228map.99)



#BH237


#or select a single Bighorn Sheep
dataBH237 <- subset(bighorns, bighorns$id == "BH237")
dataBH237$id <- factor(dataBH237$id)
BH237 <- move(x=dataBH237$X, y=dataBH237$Y, time=as.POSIXct(dataBH237$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH237, animal=dataBH237$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH237, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH237, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH237_cont <- getVolumeUD(BH237_dbbmm)
BH237_cont50 <- BH237_cont<=.50
BH237_cont95 <- BH237_cont<=.95
area50 <- sum(values(BH237_cont50))
area50
area95 <- sum(values(BH237_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH237_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH237contour50", driver = "ESRI Shapefile")
BH237map.50 <- readOGR(dsn=".", layer="BH237contour50")
plot(BH237map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH237contour95", driver = "ESRI Shapefile")
BH237map.95 <- readOGR(dsn=".", layer="BH237contour95")
plot(BH237map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH237contour99", driver = "ESRI Shapefile")
BH237map.99 <- readOGR(dsn=".", layer="BH237contour99")
plot(BH237map.99)



#BH247


#or select a single Bighorn Sheep
dataBH247 <- subset(bighorns, bighorns$id == "BH247")
dataBH247$id <- factor(dataBH247$id)
BH247 <- move(x=dataBH247$X, y=dataBH247$Y, time=as.POSIXct(dataBH247$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH247, animal=dataBH247$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH247, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH247, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH247_cont <- getVolumeUD(BH247_dbbmm)
BH247_cont50 <- BH247_cont<=.50
BH247_cont95 <- BH247_cont<=.95
area50 <- sum(values(BH247_cont50))
area50
area95 <- sum(values(BH247_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH247_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH247contour50", driver = "ESRI Shapefile")
BH247map.50 <- readOGR(dsn=".", layer="BH247contour50")
plot(BH247map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH247contour95", driver = "ESRI Shapefile")
BH247map.95 <- readOGR(dsn=".", layer="BH247contour95")
plot(BH247map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH247contour99", driver = "ESRI Shapefile")
BH247map.99 <- readOGR(dsn=".", layer="BH247contour99")
plot(BH247map.99)



#BH245


#or select a single Bighorn Sheep
dataBH245 <- subset(bighorns, bighorns$id == "BH245")
dataBH245$id <- factor(dataBH245$id)
BH245 <- move(x=dataBH245$X, y=dataBH245$Y, time=as.POSIXct(dataBH245$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH245, animal=dataBH245$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH245, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH245, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH245_cont <- getVolumeUD(BH245_dbbmm)
BH245_cont50 <- BH245_cont<=.50
BH245_cont95 <- BH245_cont<=.95
area50 <- sum(values(BH245_cont50))
area50
area95 <- sum(values(BH245_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH245_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH245contour50", driver = "ESRI Shapefile")
BH245map.50 <- readOGR(dsn=".", layer="BH245contour50")
plot(BH245map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH245contour95", driver = "ESRI Shapefile")
BH245map.95 <- readOGR(dsn=".", layer="BH245contour95")
plot(BH245map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH245contour99", driver = "ESRI Shapefile")
BH245map.99 <- readOGR(dsn=".", layer="BH245contour99")
plot(BH245map.99)



#BH226


#or select a single Bighorn Sheep
dataBH226 <- subset(bighorns, bighorns$id == "BH226")
dataBH226$id <- factor(dataBH226$id)
BH226 <- move(x=dataBH226$X, y=dataBH226$Y, time=as.POSIXct(dataBH226$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH226, animal=dataBH226$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH226, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH226, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH226_cont <- getVolumeUD(BH226_dbbmm)
BH226_cont50 <- BH226_cont<=.50
BH226_cont95 <- BH226_cont<=.95
area50 <- sum(values(BH226_cont50))
area50
area95 <- sum(values(BH226_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH226_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH226contour50", driver = "ESRI Shapefile")
BH226map.50 <- readOGR(dsn=".", layer="BH226contour50")
plot(BH226map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH226contour95", driver = "ESRI Shapefile")
BH226map.95 <- readOGR(dsn=".", layer="BH226contour95")
plot(BH226map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH226contour99", driver = "ESRI Shapefile")
BH226map.99 <- readOGR(dsn=".", layer="BH226contour99")
plot(BH226map.99)



#BH249


#or select a single Bighorn Sheep
dataBH249 <- subset(bighorns, bighorns$id == "BH249")
dataBH249$id <- factor(dataBH249$id)
BH249 <- move(x=dataBH249$X, y=dataBH249$Y, time=as.POSIXct(dataBH249$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH249, animal=dataBH249$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH249, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH249, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH249_cont <- getVolumeUD(BH249_dbbmm)
BH249_cont50 <- BH249_cont<=.50
BH249_cont95 <- BH249_cont<=.95
area50 <- sum(values(BH249_cont50))
area50
area95 <- sum(values(BH249_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH249_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH249contour50", driver = "ESRI Shapefile")
BH249map.50 <- readOGR(dsn=".", layer="BH249contour50")
plot(BH249map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH249contour95", driver = "ESRI Shapefile")
BH249map.95 <- readOGR(dsn=".", layer="BH249contour95")
plot(BH249map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH249contour99", driver = "ESRI Shapefile")
BH249map.99 <- readOGR(dsn=".", layer="BH249contour99")
plot(BH249map.99)



#BH227


#or select a single Bighorn Sheep
dataBH227 <- subset(bighorns, bighorns$id == "BH227")
dataBH227$id <- factor(dataBH227$id)
BH227 <- move(x=dataBH227$X, y=dataBH227$Y, time=as.POSIXct(dataBH227$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH227, animal=dataBH227$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH227, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH227, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH227_cont <- getVolumeUD(BH227_dbbmm)
BH227_cont50 <- BH227_cont<=.50
BH227_cont95 <- BH227_cont<=.95
area50 <- sum(values(BH227_cont50))
area50
area95 <- sum(values(BH227_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH227_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH227contour50", driver = "ESRI Shapefile")
BH227map.50 <- readOGR(dsn=".", layer="BH227contour50")
plot(BH227map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH227contour95", driver = "ESRI Shapefile")
BH227map.95 <- readOGR(dsn=".", layer="BH227contour95")
plot(BH227map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH227contour99", driver = "ESRI Shapefile")
BH227map.99 <- readOGR(dsn=".", layer="BH227contour99")
plot(BH227map.99)



#BH248


#or select a single Bighorn Sheep
dataBH248 <- subset(bighorns, bighorns$id == "BH248")
dataBH248$id <- factor(dataBH248$id)
BH248 <- move(x=dataBH248$X, y=dataBH248$Y, time=as.POSIXct(dataBH248$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH248, animal=dataBH248$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH248, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH248, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH248_cont <- getVolumeUD(BH248_dbbmm)
BH248_cont50 <- BH248_cont<=.50
BH248_cont95 <- BH248_cont<=.95
area50 <- sum(values(BH248_cont50))
area50
area95 <- sum(values(BH248_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH248_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH248contour50", driver = "ESRI Shapefile")
BH248map.50 <- readOGR(dsn=".", layer="BH248contour50")
plot(BH248map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH248contour95", driver = "ESRI Shapefile")
BH248map.95 <- readOGR(dsn=".", layer="BH248contour95")
plot(BH248map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH248contour99", driver = "ESRI Shapefile")
BH248map.99 <- readOGR(dsn=".", layer="BH248contour99")
plot(BH248map.99)


#BH231


#or select a single Bighorn Sheep
dataBH231 <- subset(bighorns, bighorns$id == "BH231")
dataBH231$id <- factor(dataBH231$id)
BH231 <- move(x=dataBH231$X, y=dataBH231$Y, time=as.POSIXct(dataBH231$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH231, animal=dataBH231$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH231, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH231, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH231_cont <- getVolumeUD(BH231_dbbmm)
BH231_cont50 <- BH231_cont<=.50
BH231_cont95 <- BH231_cont<=.95
area50 <- sum(values(BH231_cont50))
area50
area95 <- sum(values(BH231_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH231_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH231contour50", driver = "ESRI Shapefile")
BH231map.50 <- readOGR(dsn=".", layer="BH231contour50")
plot(BH231map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH231contour95", driver = "ESRI Shapefile")
BH231map.95 <- readOGR(dsn=".", layer="BH231contour95")
plot(BH231map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH231contour99", driver = "ESRI Shapefile")
BH231map.99 <- readOGR(dsn=".", layer="BH231contour99")
plot(BH231map.99)


#BH229


#or select a single Bighorn Sheep
dataBH229 <- subset(bighorns, bighorns$id == "BH229")
dataBH229$id <- factor(dataBH229$id)
BH229 <- move(x=dataBH229$X, y=dataBH229$Y, time=as.POSIXct(dataBH229$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH229, animal=dataBH229$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH229, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH229, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH229_cont <- getVolumeUD(BH229_dbbmm)
BH229_cont50 <- BH229_cont<=.50
BH229_cont95 <- BH229_cont<=.95
area50 <- sum(values(BH229_cont50))
area50
area95 <- sum(values(BH229_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH229_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH229contour50", driver = "ESRI Shapefile")
BH229map.50 <- readOGR(dsn=".", layer="BH229contour50")
plot(BH229map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH229contour95", driver = "ESRI Shapefile")
BH229map.95 <- readOGR(dsn=".", layer="BH229contour95")
plot(BH229map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH229contour99", driver = "ESRI Shapefile")
BH229map.99 <- readOGR(dsn=".", layer="BH229contour99")
plot(BH229map.99)



#BH224


#or select a single Bighorn Sheep
dataBH224 <- subset(bighorns, bighorns$id == "BH224")
dataBH224$id <- factor(dataBH224$id)
BH224 <- move(x=dataBH224$X, y=dataBH224$Y, time=as.POSIXct(dataBH224$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH224, animal=dataBH224$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH224, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH224, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH224_cont <- getVolumeUD(BH224_dbbmm)
BH224_cont50 <- BH224_cont<=.50
BH224_cont95 <- BH224_cont<=.95
area50 <- sum(values(BH224_cont50))
area50
area95 <- sum(values(BH224_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH224_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH224contour50", driver = "ESRI Shapefile")
BH224map.50 <- readOGR(dsn=".", layer="BH224contour50")
plot(BH224map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH224contour95", driver = "ESRI Shapefile")
BH224map.95 <- readOGR(dsn=".", layer="BH224contour95")
plot(BH224map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH224contour99", driver = "ESRI Shapefile")
BH224map.99 <- readOGR(dsn=".", layer="BH224contour99")
plot(BH224map.99)



#BH235


#or select a single Bighorn Sheep
dataBH235 <- subset(bighorns, bighorns$id == "BH235")
dataBH235$id <- factor(dataBH235$id)
BH235 <- move(x=dataBH235$X, y=dataBH235$Y, time=as.POSIXct(dataBH235$GPSFixTime, format="%Y.%m.%d %H:%M:%S"), proj=CRS("+proj=utm +zone=13 +datum=NAD83"),data=dataBH235, animal=dataBH235$id)
BH_dbbmm <- brownian.bridge.dyn(object=BH235, location.error=22, window.size=19, margin=7,  dimSize=100,time.step=180)
plot(BH_dbbmm)
contour(BH_dbbmm, levels=c(.5,.9,.95,.99), add=TRUE)
show(BH_dbbmm)

#Plot the movement of the animal
plot(BH235, type="o", col=3, lwd=2, pch=20, xlab="location_east",ylab="location_north")

#Code below will get area of each isopleth in R
BH235_cont <- getVolumeUD(BH235_dbbmm)
BH235_cont50 <- BH235_cont<=.50
BH235_cont95 <- BH235_cont<=.95
area50 <- sum(values(BH235_cont50))
area50
area95 <- sum(values(BH235_cont95))
area95

##Cast the data over to an adehabitatHR estUD
dbbmm.px <- as(BH235_dbbmm, "SpatialPixelsDataFrame")
image(dbbmm.px)
dbbmm.ud <- new("estUD",dbbmm.px)
dbbmm.ud@vol = FALSE
dbbmm.ud@h$meth = "dBBMM"
##Convert the raw UD values to volume
#udvol <- getvolumeUD(dbbmm.ud, standardize=FALSE)

shp50 <- getverticeshr(dbbmm.ud, percent=50, standardize=TRUE)
class(shp50)#Now is a SpatialPolygonsDataFrame
map.ps50 <- SpatialPolygons2PolySet(shp50)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(map.ps50, projection = 'UTM', zone = '13')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="BH235contour50", driver = "ESRI Shapefile")
BH235map.50 <- readOGR(dsn=".", layer="BH235contour50")
plot(BH235map.50)

shp95 <- getverticeshr(dbbmm.ud, percent=95, standardize=TRUE)
class(shp95)#Now is a SpatialPolygonsDataFrame
plot(shp95, add=TRUE)
map.ps95 <- SpatialPolygons2PolySet(shp95)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.95 <- as.PolySet(map.ps95, projection = 'UTM', zone = '13')
diss.map.p95 <- PolySet2SpatialPolygons(diss.map.95, close_polys = TRUE)
data95 <- data.frame(PID = 1)
diss.map.p95 <- SpatialPolygonsDataFrame(diss.map.p95, data = data95)
writeOGR(diss.map.p95, dsn = ".", layer="BH235contour95", driver = "ESRI Shapefile")
BH235map.95 <- readOGR(dsn=".", layer="BH235contour95")
plot(BH235map.95)

shp99 <- getverticeshr(dbbmm.ud, percent=99, standardize=TRUE)
class(shp99)#Now is a SpatialPolygonsDataFrame
plot(shp99, add=TRUE)
map.ps99 <- SpatialPolygons2PolySet(shp99)
#diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.99 <- as.PolySet(map.ps99, projection = 'UTM', zone = '13')
diss.map.p99 <- PolySet2SpatialPolygons(diss.map.99, close_polys = TRUE)
data99 <- data.frame(PID = 1)
diss.map.p99 <- SpatialPolygonsDataFrame(diss.map.p99, data = data99)
writeOGR(diss.map.p99, dsn = ".", layer="BH235contour99", driver = "ESRI Shapefile")
BH235map.99 <- readOGR(dsn=".", layer="BH235contour99")
plot(BH235map.99)