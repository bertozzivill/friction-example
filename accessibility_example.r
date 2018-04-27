## -----------------------------------------------------------------------------------------------------
## accessibility_example.r
## Author: Amelia Bertozzi-Villa, Institute for Disease Modeling and University of Oxford
## Date: April 27th 2018
## Description: Example useage of MAP's malariaAtlas package and friction surface to find travel time to the nearest 
##               big mountain in Colorado State. Adapted from Dan Weiss' script at
##               https://map.ox.ac.uk/research-project/accessibility_to_cities/
##
## Citation: D.J. Weiss, A. Nelson, H.S. Gibson, W. Temperley, S. Peedell, A. Lieber, M. Hancher, E. Poyart, S. Belchior, N. Fullman, B. Mappin, U. Dalrymple, J. Rozier, 
##           T.C.D. Lucas, R.E. Howes, L.S. Tusting, S.Y. Kang, E. Cameron, D. Bisanzio, K.E. Battle, S. Bhatt, and P.W. Gething. A global map of travel time to cities to assess 
##           inequalities in accessibility in 2015. (2018). Nature. doi:10.1038/nature25181.
## -----------------------------------------------------------------------------------------------------

rm(list=ls())

## Packages
library(gdistance)
library(abind)
library(rje)
library(ggplot2)
library(malariaAtlas)

# Plot defaults
theme_set(theme_minimal(base_size=14))

# Set to TRUE if a transition matrix (T.GC) has already been created and saved.   
transition.matrix.exists.flag <- F

## Root Directories
main.dir <- "./"
out.dir <- main.dir

## Input Files
points.filename <- paste0(main.dir, "peaks.csv")

## Output Files
T.filename <- paste0(out.dir, 'transmission.matrix.rds')
T.GC.filename <- paste0(out.dir, 'geocorrected.transition.matrix.rds')
output.raster.filename <- paste0(out.dir, 'travel.times.tif')

## Point locations
point.locations <- fread(file = points.filename)
setnames(point.locations, c("lon", "lat"), c("X_COORD", "Y_COORD"))

## Shapefile

#  Use malariaAtlas to download a shapefile for Colorado state
USA.shp <- getShp(ISO = "USA", admin_level = "admin1")
analysis.shp <- USA.shp[USA.shp@data$name=="Colorado",]

# Keep only point coorinates within the shapefile bounds
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(analysis.shp)
overlap <- over(point.locations, analysis.shp)
point.locations <- point.locations[!is.na(overlap$gid),]
points <- as.matrix(point.locations@coords) # save coordinates as a matrix for the accessibility function


## Friction:

# Extract friction surface, clipped to shapefile
friction <- getRaster(surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
                      shp = analysis.shp)

# Make the graph and the geocorrected version of the graph (or read in the latter).
if (transition.matrix.exists.flag) {
  # Read in the transition matrix object if it has been pre-computed
  T.GC <- readRDS(T.GC.filename)
} else {
  # Make and geocorrect the transition matrix (i.e., the graph)
  T <- transition(friction, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
  saveRDS(T, T.filename)
  T.GC <- geoCorrection(T)                    
  saveRDS(T.GC, T.GC.filename)
}

# Run the accumulated cost algorithm to make the accessibility map
access.raster <- accCost(T.GC, points)
writeRaster(access.raster, output.raster.filename)

# Plot the resulting raster, overlaid with point locations
p <- autoplot_MAPraster(access.raster, shp_df=analysis.shp, printed=F)

p[[1]] + geom_point(data=data.frame(point.locations@coords), aes(x=X_COORD, y=Y_COORD)) +
  scale_fill_gradientn(colors = rev(cubeHelix(gamma=1.0, start=1.5, r=-1.0, hue=1.5, n=16)), 
                       name="Minutes \n of Travel") + 
  ggtitle("Travel Time to Most Accessible Peak") +
  theme(axis.text = element_blank(),
        panel.border = element_rect(fill = NA, color = "white"))
  
