## ----------------------------------------------------------------------------------------------------------------------
## accessibility_example.r
## Author: Amelia Bertozzi-Villa, Institute for Disease Modeling and University of Oxford
## Date: April 27th 2018
## Description: Example useage of MAP's malariaAtlas package and friction surface to find travel time to the nearest 
##               big mountain in Colorado State. Adapted from Dan Weiss' script at
##               https://map.ox.ac.uk/research-project/accessibility_to_cities/
##
## Citation: D.J. Weiss, A. Nelson, H.S. Gibson, W. Temperley, S. Peedell, A. Lieber, M. Hancher, E. Poyart, S. Belchior,
##           N. Fullman, B. Mappin, U. Dalrymple, J. Rozier, T.C.D. Lucas, R.E. Howes, L.S. Tusting, S.Y. Kang, E. Cameron,
##           D. Bisanzio, K.E. Battle, S. Bhatt, and P.W. Gething. A global map of travel time to cities to assess 
##           inequalities in accessibility in 2015. (2018). Nature. doi:10.1038/nature25181.
## ----------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

## Packages
library(gdistance)
library(abind)
library(rje)
library(ggplot2)
library(malariaAtlas)

## Plot defaults
theme_set(theme_minimal(base_size=14))

## Directories
main.dir <- "./"
setwd(main.dir)

## Input Files
points.filename <- "peaks.csv"

## Output Files
T.filename <- 'transmission.matrix.rds'
T.GC.filename <- 'geocorrected.transition.matrix.rds'
output.raster.filename <- 'travel.times.tif'


## Shapefile

#  Use malariaAtlas to download a shapefile for Colorado state
USA.shp <- getShp(ISO = "USA", admin_level = "admin1")
analysis.shp <- USA.shp[USA.shp@data$name=="Colorado",]
plot(analysis.shp, main="Shape for Clipping")


## Friction:

# Extract friction surface, clipped to shapefile
friction <- getRaster(surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
                      shp = analysis.shp)
autoplot_MAPraster(friction)

# Make and geocorrect the transition matrix (i.e., the graph)
T <- transition(friction, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
T.GC <- geoCorrection(T)                    


## Point locations
point.locations <- read.csv(file = points.filename)
names(point.locations) <- c("X_COORD", "Y_COORD", "name")

# Keep only point coordinates within the shapefile bounds
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(analysis.shp)
overlap <- over(point.locations, analysis.shp)
point.locations <- point.locations[!is.na(overlap$gid),]

# Convert coordinates to a matrix for the accessibility function
points <- as.matrix(point.locations@coords) 


## Accessibility
# Run the accumulated cost algorithm to make the accessibility map
access.raster <- accCost(T.GC, points)

# Plot the resulting raster, overlaid with point locations
p <- autoplot_MAPraster(access.raster, 
                        shp_df=analysis.shp, printed=F)
full_plot <- p[[1]] + 
             geom_point(data=data.frame(point.locations@coords), 
                                 aes(x=X_COORD, y=Y_COORD)) +
             scale_fill_gradientn(colors=rev(cubeHelix(gamma=1.0, 
                                                       start=1.5, 
                                                       r=-1.0, 
                                                       hue=1.5, 
                                                       n=16)), 
                                  name="Minutes \n of Travel") + 
             ggtitle("Travel Time to Most Accessible Peak") +
             theme(axis.text=element_blank(),
                   panel.border=element_rect(fill=NA, color="white"))
plot(full_plot) 


## Save Outputs
saveRDS(T, T.filename)
saveRDS(T.GC, T.GC.filename)
writeRaster(access.raster, output.raster.filename)
png("travel_times.png", width=10, height=6, units="in", res=120)
  print(full_plot)
graphics.off()
