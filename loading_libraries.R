## A list of general packages used for all projects. It is not necessary that every single package mentioned here is being used in the script ### 

## if the pacakages are unavailable install them ####
unavailable <- setdiff(c("data.table",'tidyr',
                         'tidyverse','dplyr', 'janitor',
                         'StreamMetabolism',"skimr",
                         "magrittr", 'statsr','openair',
                         'zoo','lubridate',
                         'ggplot2','ggalt','gghighlight',
                         'grid','gridExtra', 'plotly',
                         'RColorBrewer', 'mapplots',
                         'mapview','leaflet', 'gstat',"pracma",
                         'rgdal','sp','sf', 'rgeos', "geojsonio",
                         'raster','webshot', 'readxl', 'writexl'), rownames(installed.packages()))
if(length(unavailable>0)){
  install.packages(unavailable)
  
} else {
  "The code below can run on your currently installed packages"
}

## load all the pacakages ###
## dataframe manipulation packages ####
library(data.table)
library(tidyr)
library(tidyverse)
library(dplyr)
library(statsr)
library(skimr)
library(magrittr)

library(StreamMetabolism) ## sunrise, sunset calculations ###
library(readxl) ## excel manipulations ###
library(writexl)
library(janitor) ## pretty column names

## air quality package ####
library(openair)

## datetime manipulation packages ####
library(zoo)
library(lubridate)

## plotting/ interactive plotting packages ####
library(ggplot2)
library(ggalt)
library(gghighlight)
library(grid)
library(gridExtra)
library(plotly)
library(RColorBrewer)

## spatial data analysis/ mapping packages ####
library(mapplots)
library( mapview)
library(leaflet)
library(rgdal)
library(sp)
library(sf)
library(raster)
library(webshot)
library(rgeos)
library(gstat)
library(pracma) ## orthogonal regression
library(geojsonio) ## read GeoJson files from the web
