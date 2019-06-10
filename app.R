#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(readxl)
library(rworldmap)
library(RColorBrewer)
library(shinythemes)

#based on a file of previous data retrieval and process of spotify the maps of the world are created 
inFile <-'IT.csv'
IT <- read.csv(inFile,header=TRUE,as.is=TRUE)

#create a map-shaped window
mapDevice('x11')

#join to a coarse resolution map
sPDF <- joinCountryData2Map(IT, joinCode='NAME', nameJoinColumn='country', verbose='TRUE')


# Define UI for application that draws a histogram
if (interactive()){ 
  ui <- fluidPage( 
    theme=shinytheme("slate"),
    titlePanel("Which are the most important music characteristics to be popular in a country?"),
    fluidRow(
      column(2,
        radioButtons ("characteristic", "Select the music characteristic you want to explore:",
                      c("Energy",
                        "Danceability",
                        "Acousticness",
                        "Speechiness",
                        "Loudness", 
                        "Liveness", 
                        "Instrumentalness", 
                        "Valence", 
                        "Tempo"))
      ),
      column(10,
        textOutput("info"),
        plotOutput("map")
      )
    )
  )
  
  server <- function (input,output) {
    output$info <-renderText({
      characteristic <- switch(input$characteristic,
                               Energy = "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy. ",
                               Danceability = "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable. ",
                               Acousticness = "A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.",
                               Speechiness = "Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks.",
                               Loudness = "The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db. ",
                               Liveness = "Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live. ",
                               Instrumentalness = "Predicts whether a track contains no vocals. “Ooh” and “aah” sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly “vocal”. The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0. ",
                               Valence = "	A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)",
                               Tempo ="	The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration. "
      )
    })
    
    output$map <- renderPlot({
      characteristic <- switch(input$characteristic,
                               Energy = mapCountryData (sPDF, 
                                                        nameColumnToPlot="Energy", 
                                                        catMethod="fixedWidth", 
                                                        colourPalette = brewer.pal (7,"YlOrRd") ),
                               Danceability= mapCountryData (sPDF, 
                                                             nameColumnToPlot="Danceability", 
                                                             catMethod="fixedWidth", 
                                                             colourPalette = brewer.pal (7,"YlOrBr") ),
                               Acousticness= mapCountryData (sPDF, 
                                                             nameColumnToPlot="Acousticness", 
                                                             catMethod="fixedWidth", 
                                                             colourPalette = brewer.pal (7,"YlGnBu") ),
                               Speechiness= mapCountryData (sPDF, 
                                                            nameColumnToPlot="Speechiness", 
                                                            catMethod="fixedWidth", 
                                                            colourPalette = brewer.pal (7,"YlGn") ),
                               Loudness= mapCountryData (sPDF, 
                                                         nameColumnToPlot="Loudness", 
                                                         catMethod="fixedWidth", 
                                                         colourPalette = brewer.pal (7,"Reds") ), 
                               Liveness= mapCountryData (sPDF, 
                                                         nameColumnToPlot="Liveness", 
                                                         catMethod="fixedWidth", 
                                                         colourPalette = brewer.pal (7,"RdPu") ), 
                               Instrumentalness= mapCountryData (sPDF, 
                                                                 nameColumnToPlot="Instrumentalness", 
                                                                 catMethod="fixedWidth", 
                                                                 colourPalette = brewer.pal (7,"Purples") ), 
                               Valence= mapCountryData (sPDF, 
                                                        nameColumnToPlot="Valence", 
                                                        catMethod="fixedWidth", 
                                                        colourPalette = brewer.pal (7,"PuBuGn") ), 
                               Tempo= mapCountryData (sPDF, 
                                                      nameColumnToPlot="Tempo", 
                                                      catMethod="fixedWidth", 
                                                      colourPalette = brewer.pal (7,"PuBu") )
                         



        )
      #possible values Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
      
    })
  }
  
  shinyApp(ui, server)
}





