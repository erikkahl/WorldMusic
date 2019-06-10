#based on a file of previous data retrieval and process of spotify the maps of the world are created 

library(readxl)
library(rworldmap)
library(RColorBrewer)


inFile <-'IT.csv'
IT <- read.csv(inFile,header=TRUE,as.is=TRUE)

#create a map-shaped window
mapDevice('x11')

#join to a coarse resolution map
sPDF <- joinCountryData2Map(IT, joinCode='NAME', nameJoinColumn='country', verbose='TRUE')

#colourPalette
color<- brewer.pal(7,"YlGnBu") 
#possible values Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

mapCountryData (sPDF, nameColumnToPlot="Acousticness", catMethod="fixedWidth", colourPalette = color )
mapCountryData (sPDF, nameColumnToPlot="Energy", catMethod="fixedWidth")

