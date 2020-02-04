# Austin Schumacher
# 1/29/2020
# Example code for mapping the Ohio data

# clear the environment
rm(list=ls())

# if you haven't installed INLA, run this line (because INLA is not available on CRAN)
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

# load libraries
library(rgdal); library(maptools); library(sp); library(spdep); library(SpatialEpi); 
library(RColorBrewer); library(ggplot2); library(maps); library(broom);

# set working directory to where the Ohio shapefiles and data are located
#   I suggest keeping all shapefiles and data for the course in one folder
setwd("~/Dropbox/555-Materials/555-2020/555-2020-Data")

# read in the Ohio shapefile using the readOGR function from the rgdal package
ohio_map <- readOGR(dsn = ".",layer = "ohio_map")

# can also use the readShapePoly function from the maptools package, but this is deprecated and no longer updated
# it doesn't read in the CRS from the file, which is unfortunate. But the Ohio shapefile doesn't come with a CRS anyways.
ohio_map2 <- readShapePoly(fn = "ohio_map")

# let's look at the data from this shapefile
head(ohio_map@data)

# now, let's read in the lung cancer data
ohio_data <- read.table("ohio_2019_version.txt", header = TRUE)

# create SMRs
ohio_data$SMR <- ohio_data$Obs/ohio_data$Exp

# merge the Ohio lung cancer data onto the shapefile data
ohio_map$fips <- as.numeric(as.character(ohio_map$CNTYIDFP00))

ohio_map <- merge(ohio_map,ohio_data,by="fips")

# now, let's make a few example maps with the spplot() function
# for these maps, we're going to save them as PDFs onto our computer directly in the code,
#   so you don't have to make the plot and export it by going through the menu.
#   This is useful if you are typing your HW in a word doc rather than an R markdown file

# first, set the working directory to where you want to save the PDF
setwd("~/Dropbox/555-Materials/555-2020/555-2020-Misc/figures")

# start the graphical device
# this will create the pdf on our computer
pdf("ohio_map_spplot_default.pdf", # name of file you want to save
    width = 8, # width of plot
    height = 8) # height of plot

# make the plot, which will be put into the PDF that we "opened" 
# observed counts using spplot (simple)
spplot(ohio_map, c("Obs"))

# close the graphical device
# this means that we can now open the PDF on our computer
dev.off()

# now, the file "ohio_map_spplot_default.pdf" exists on our computer. You can copy and paste this into a word doc

# set number of bins of colors to use
num_bins <- 8

# choose a color palette
plotting_color_palette <- rev(brewer.pal(8, "RdBu"))

# set breakpoints for plotting
quants <- round(quantile(ohio_map@data$Obs, seq(0, 1, length.out = num_bins)),2)
ohio_map@data$obs_cut <- cut(ohio_map@data$Obs, breaks = c(quants, Inf), include.lowest = TRUE, right = FALSE)   # I modified the breaks

# start graphical device
pdf("ohio_map_spplot_pretty.pdf", width = 8, height = 8)

# make the pretty plot
spplot(ohio_map["obs_cut"],
       colorkey = list(height = 1, 
                       labels = list(at = seq(0.5, length(quants) -0.5), 
                                     labels = quants)),
       col.regions = colorRampPalette(plotting_color_palette)(length(quants)))

# close graphical device
dev.off()

# we can also use the mapvariable() function
pdf("ohio_map_mapvariable.pdf", width = 8, height = 8)
mapvariable(ohio_map@data$Obs,ohio_map)
dev.off()

# perhaps ggplot is your preferred way to make maps?

# make a data frame of polygon x- and y-coordingates using the tidy() function from the broom package
# this gives warnings that it changes factor variables into character variables, which is fine
ohio_df <- tidy(ohio_map)

# now we need to merge on the data we want to plot
# we will use sapply to extract the data for each polygon, then merge this data onto the data frame
ohio_map$polyID <- sapply(slot(ohio_map, "polygons"), function(x) slot(x, "ID"))
ohio_df <- merge(ohio_df, ohio_map, by.x = "id", by.y="polyID")

# for ggplot we don't have to use graphical devices to save the pdfs, we can use the ggsave() function
ggplot() +                                               # initialize ggplot object
    geom_polygon(                                          # make a polygon
        data = ohio_df,                                    # data frame
        aes(x = long, y = lat, group = group,                # coordinates, and group them by polygons
            fill = obs_cut)) +                # variable to use for filling
    scale_fill_brewer("Deaths", palette = "RdBu", direction = -1) + # fill with brewer colors 
    theme(line = element_blank(),                          # remove axis lines ..
          axis.text=element_blank(),                       # .. tickmarks..
          axis.title=element_blank()) +                      # .. axis labels..
    coord_equal()  

# save the pdf. ggsave() saves the last ggplot produced as
ggsave("ohio_map_ggplot.pdf", width = 8, height = 8)
