
library(raster)
library(rgdal)
library(adehabitatHR)
library(sp)
library(rgeos)
#to find standard error and mean
library(psych)
# Set working directory if you aren't using an R project with 'setwd(...)'

# Import the csv file
###Week 1 of movement data were removed to reduce bias towards release site of translocated sheep
bighorn <- read.csv("AllBighornLocationsMinusWeekOne.csv") 
bighorn1 <- read.csv("AllBighornLocationsYear1MinusWeekOne.csv")
bighorn2 <- read.csv("AllBighornLocationsYear2.csv")

#make sure time is military with hh:mm:ss

#timestamp <- bighorn$timestamp
#concatenate date and time into a timestamp
timestamp <- bighorn$DateTime
timestamp

timestamp1 <- bighorn1$DateTime
timestamp1

timestamp2 <- bighorn2$DateTime
timestamp2
#r has a hard time with dates and time
#make date readable using POXIct the concatenate with timee using paste
timestamp <- as.POSIXct(timestamp, "%Y.%m.%d %H:%M:%S", tz="UTC") #couble check that there are dates and not NA values in timestamp
timestamp

timestamp1 <- as.POSIXct(timestamp1, "%Y.%m.%d %H:%M:%S", tz="UTC") #double check that there are dates and not NA values in timestamp
timestamp1

timestamp2 <- as.POSIXct(timestamp2, "%Y.%m.%d %H:%M:%S", tz="UTC") #double check that there are dates and not NA values in timestamp
timestamp2
# bind the timestamp vector with the dataframe as an additional column
bighorn <- cbind(bighorn, timestamp)
head(bighorn)

bighorn1 <- cbind(bighorn1, timestamp1)
head(bighorn)

bighorn2 <- cbind(bighorn2, timestamp2)
head(bighorn)
# Reduce the columns to only ones that might be useful - we don't need a lot
# of the data that came with the dataset We'll keep the 'Event ID' - row
# identifier; 'individual.local.identifier' as an animal ID number;
# 'timestamp' as the date/time; and the coordinate fields

bighorn <- bighorn[, c("event.id", "visible", "individual.local.identifier",
                   "timestamp","location.long", "location.lat", "utm.easting", "utm.northing", "utm.zone", "StudySeason")]
bighorn1 <- bighorn1[, c("event.id", "visible", "individual.local.identifier",
                   "timestamp1","location.long", "location.lat", "utm.easting", "utm.northing", "utm.zone", "StudySeason")]
bighorn2 <- bighorn2[, c("event.id", "visible", "individual.local.identifier",
                   "timestamp2","location.long", "location.lat", "utm.easting", "utm.northing", "utm.zone", "StudySeason")]



## The below line should normally work, but the values for decimal seconds
## (which are all 0) seem to cause problems, so we use the above code, with
## the strptime function included bighorn$timestamp <-
# Copy bighorn object to another (bighorn.spdf), which we will work with for
# this step.
bighorn.spdf <- bighorn
bighorn1.spdf <- bighorn1
bighorn2.spdf <- bighorn2

# Look at summary of dataset - especially the coordinates fields, to see if
# there are NA values
#summary(bighorn.spdf)
#which(is.na(bighorn.spdf$location.lat))
#which(is.na(bighorn.spdf$location.long))

## Note: if you wanted to find which rows had a specific value, instead of
## NA, you would remove the 'is.na' function, and use a logical operator at
## the end, followed by a specific value: which(bighorn.spdf$location.long ==
## [enter value here])

# Remove rows with NA for 'location.long' column
#bighorn.spdf <- bighorn.spdf[!is.na(bighorn.spdf$location.long), ]
# Remove rows with NA for 'timestamp'
#bighorn.spdf <- bighorn.spdf[!is.na(bighorn.spdf$timestamp), ]

#There are three main arguments we are specifying:
#The Coordinates (as longitude and latitude),
#the data that will go in the table (bighorn.spdf), and the projection information
bighorn.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(bighorn.spdf$utm.easting,
                                                                bighorn.spdf$utm.northing)), data=bighorn.spdf, 
                                     proj4string =CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83"))

bighorn1.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(bighorn1.spdf$utm.easting,
                                                                bighorn1.spdf$utm.northing)), data=bighorn1.spdf, 
                                     proj4string =CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83"))

bighorn2.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(bighorn2.spdf$utm.easting,
                                                                bighorn2.spdf$utm.northing)), data=bighorn2.spdf, 
                                     proj4string =CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83"))

#bighorn.spdf <- spTransform(bighorn.spdf,CRS("+proj=longlat +zone=13 +ellps=GRS80 +datum=NAD83"))


#You can plot the points as you would any other Spatial object
#it might take a minute as there are a lot of points
plot(bighorn.spdf, axes = TRUE)
plot(bighorn1.spdf, axes = TRUE)
plot(bighorn2.spdf, axes = TRUE)
# mcp will operate on a SpatialPoints Object or SpatialPointsDataFrame. For
# the former, it assumes the data are all for one individual; for the
# latter, you simply specify the SpatialPointsDataFrame and the field that
# identifies individuals, and MCPs are computed for each individual the
# 'Percent' argument indicates what percentage of points you want to
# include. the 'unin' and 'unout' arguments indicate what the spatial units
# of the input data are, and of the output area calculations, respectively.

# First: 100% MCP
bighorn.mcp <- mcp(bighorn.spdf[, "individual.local.identifier"], percent = 99,
                 unin = "m", unout = "km2")
bighorn1.mcp <- mcp(bighorn1.spdf[, "individual.local.identifier"], percent = 99,
                 unin = "m", unout = "km2")
bighorn2.mcp <- mcp(bighorn2.spdf[, "individual.local.identifier"], percent = 99,
                 unin = "m", unout = "km2")
#bighorn.mcp <- mcp(bighorn.spdf, percent = 95,
#                 unin = "m", unout = "km2")
# You can plot the home ranges - use the 'col' argument to specify colors
# for different individuals - there are 7 individuals tracked, so we can
# specify we want to use colors 1:7, based on standard R colors. You could
# customize this by specifying indivual colors (one per individual ID) add
# the argument 'axes=TRUE' to add x/y axes based on the data projection
summary(bighorn$individual.local.identifier)

plot(bighorn.mcp, col = c(1:22), axes=T)
# Custom-specified colors with axes
plot(bighorn.mcp, col = c("blue", "gray", "red", "orange", "green", "yellow",
                        "magenta"), axes = TRUE)

#The area of each home range is stored as a value in a table that gets created with the MCP output.
# You could call this table and view the results with either of the following,
#and you could also assign the resulting table to a new object
bighorn.mcp@data
as.data.frame(bighorn.mcp)
bighorn.mcp
describe(bighorn.mcp$area)

bighorn1.mcp@data
as.data.frame(bighorn1.mcp)
bighorn1.mcp
describe(bighorn1.mcp$area)

bighorn2.mcp@data
as.data.frame(bighorn2.mcp)
bighorn2.mcp
describe(bighorn2.mcp$area)
#Output
#Notice - the ID and row name are the identifier codes for each individual.

# first argument is the name of the MCP object in R, the second is a folder
# name (to be created in the working directory), third argument is a name to
# assign the layer in the shapefile, and last argument is specifying what
# type of file to output the data as.
writeOGR(bighorn.mcp, dsn = "BighornMCP99", layer = "BighornMCP99", driver = "ESRI Shapefile")
writeOGR(bighorn1.mcp, dsn = "Bighorn1MCP95", layer = "Bighorn1MCP95", driver = "ESRI Shapefile")
writeOGR(bighorn2.mcp, dsn = "Bighorn2MCP95", layer = "Bighorn2MCP95", driver = "ESRI Shapefile")


####################################################################KERNEL DENSITY ESTIMATES#######################################################

#Next, we will create a kernel density estimate of the home range for each individual bighorn, using the 'kernelUD'
#fnction in the adehabitatHR package.
bighorn.Khref <- kernelUD(bighorn.spdf[, "individual.local.identifier"], grid = 1000, extent = 2)
# You can plot the KDE for each individual separately:
image(bighorn.Khref)
bighorn1.Khref <- kernelUD(bighorn1.spdf[, "individual.local.identifier"], grid = 1000, extent = 2)
bighorn2.Khref <- kernelUD(bighorn2.spdf[, "individual.local.identifier"], grid = 1000, extent = 2)


# You can convert the output to a 'raster' object for each individual.
# Demonstrated here for individual z3864. You can then output the raster to
# a file for use in other GIS software:
BH229.rast <- (raster(as(bighorn.Khref$BH229, "SpatialPixelsDataFrame")))
# file gets saved to current working directory unless otherwise specified
writeRaster(BH229.rast, "BH229Rast.tif")
# Plot the raster on its own - note, color scheme is different for raster
# package than when you previously plotted it
plot(BH229.rast)
# You can subset the SpatialPointsDataFrame too, and plot the points for
# only that individual on top of the raster
BH229 <- bighorn.spdf[bighorn.spdf$individual.local.identifier == "BH229", ]
plot(BH229, add = T, cex = 0.1) #use 'cex' to adjust the size; default is 1


#With Kernel Density Estimates, home ranges are often characterized as a certain percentage of the entire
#density (e.g., 90% or 95%). Thus, we can extract these areas automatically using the function 'getverticeshr'.
bighorn.KDE99 <- getverticeshr(bighorn.Khref, percent = 99)
bighorn1.KDE99 <- getverticeshr(bighorn1.Khref, percent = 99)
bighorn2.KDE99 <- getverticeshr(bighorn2.Khref, percent = 99)

bighorn.KDE95 <- getverticeshr(bighorn.Khref, percent = 95)
bighorn1.KDE95 <- getverticeshr(bighorn1.Khref, percent = 95)
bighorn2.KDE95 <- getverticeshr(bighorn2.Khref, percent = 95)

bighorn.KDE50 <- getverticeshr(bighorn.Khref, percent = 50)
bighorn1.KDE50 <- getverticeshr(bighorn1.Khref, percent = 50)
bighorn2.KDE50 <- getverticeshr(bighorn2.Khref, percent = 50)

plot(bighorn.KDE99, col = c("blue", "gray", "red", "orange", "green", "yellow",
                          "magenta"), axes = TRUE)
help("getverticeshr")
bighorn.KDE99@data
as.data.frame(bighorn.KDE99)
bighorn.KDE99
describe(bighorn.KDE99$area)

bighorn1.KDE99@data
as.data.frame(bighorn1.KDE99)
bighorn1.KDE99
describe(bighorn1.KDE99$area)

bighorn2.KDE99@data
as.data.frame(bighorn2.KDE99)
bighorn2.KDE99
describe(bighorn2.KDE99$area)

bighorn.KDE95@data
as.data.frame(bighorn.KDE95)
bighorn.KDE95
describe(bighorn.KDE95$area)

bighorn1.KDE95@data
as.data.frame(bighorn1.KDE95)
bighorn1.KDE95
describe(bighorn1.KDE95$area)

bighorn2.KDE95@data
as.data.frame(bighorn2.KDE95)
bighorn2.KDE95
describe(bighorn2.KDE95$area)

bighorn.KDE50@data
as.data.frame(bighorn.KDE50)
bighorn.KDE50
describe(bighorn.KDE50$area)

bighorn1.KDE50@data
as.data.frame(bighorn1.KDE50)
bighorn1.KDE50
describe(bighorn1.KDE50$area)

bighorn2.KDE50@data
as.data.frame(bighorn2.KDE50)
bighorn2.KDE50
describe(bighorn2.KDE50$area)


writeOGR(bighorn.KDE99, dsn = "bighornKDE99", layer = "bighornKDE99", driver = "ESRI Shapefile")
writeOGR(bighorn1.KDE99, dsn = "bighorn1KDE99", layer = "bighorn1KDE99", driver = "ESRI Shapefile")
writeOGR(bighorn2.KDE99, dsn = "bighorn2KDE99", layer = "bighorn2KDE99", driver = "ESRI Shapefile")

writeOGR(bighorn.KDE95, dsn = "bighornKDE95", layer = "bighornKDE95", driver = "ESRI Shapefile")
writeOGR(bighorn1.KDE95, dsn = "bighorn1KDE95", layer = "bighorn1KDE95", driver = "ESRI Shapefile")
writeOGR(bighorn2.KDE95, dsn = "bighorn2KDE95", layer = "bighorn2KDE95", driver = "ESRI Shapefile")

