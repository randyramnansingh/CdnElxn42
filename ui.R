
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

require(shiny) # For rendering shiny app 
require(dygraphs)
require(googleVis)

shinyUI(pageWithSidebar(
  titlePanel("Twitter Sentiment Analysis - Canada Elections 2015"),
  sidebarPanel(
    radioButtons("dataset", "Choose a Candidate:",
                 c("Stephen Harper", "Thomas Mulcair", "Justin Trudeau", "Gilles Duceppe", "Elizabeth May"))
  ),
  mainPanel("Overall Twitter Sentiment by Geography", htmlOutput("view"),
            tags$div("____________________________________________________________________________________________________________________________"),
            "Timeseries analysis of Twitter Sentiment", dygraphOutput("dygraph"))
))