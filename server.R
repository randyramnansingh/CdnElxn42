
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# Author: Randy Ramnansingh

library(shiny)
library(ibmdbR) 
library(dygraphs)
library(googleVis)

# connecting to dashdb, setting up ida data frames for tables with tweets for each respective party
con <- idaConnect('BLUDB','','')
idaInit(con)
tweets_cpc <- ida.data.frame('DASH013612."tweets_CPC"') # Conservative 
tweets_ndp <- ida.data.frame('DASH013612."tweets_NDP"') # New Democratic
tweets_gpc <- ida.data.frame('DASH013612."tweets_GPC"') # Green
tweets_lpc <- ida.data.frame('DASH013612."tweets_LPC"') # Liberal
tweets_bq <- ida.data.frame('DASH013612."tweets_BQ"')   # Bloc Québécois
#tweets_fetd <- ida.data.frame('DASH013612."tweets_FETD"') # Strength in Democracy


######################################################################################################################
# Function to calculate the Sentiment Score by Province for each Party

sentimentScorebyProvince <- function(x)
{
  print("calculating sentiment score by province")
  x <- as.data.frame(x[x$smaAuthorState != '', 5:7]) #TODO: chanage 5:7 to columns relative to your data (5 is State, 7 is Sentiment Number)
  x <- x[ x$smaAuthorState %in% c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Northwest Territories", "Nunavut", "Yukon"), ]
  a <- aggregate(x$sent_Num, by = list(smaAuthorState = x$smaAuthorState), mean, na.action = na.omit)
  print("calculation done")
  return (a)
}
#####################################################################################################################
#####################################################################################################################
# Function to generate multiple time series object

groupTweetsbyTime <- function(x)
{
  if(nrow(x) == 0)
  {
    print(paste("Grouping tweets by time :",deparse(substitute(x))))
    x <- as.data.frame(x);
    l <- length(unique(x$msgPostedTime))
    msgPostedTime <- Sys.Date() - c(1:l)
    sent_Num <- c(0,l)
    x <- data.frame(msgPostedTime, sent_Num)
  }
  else
  {
    print(paste("Grouping tweets by time :",deparse(substitute(x))))
    x <- as.data.frame(x[x$msgId != '' ,c('sent_Num', 'msgPostedTime')])
    print("Parsing Date")
    x$msgPostedTime <- as.Date(x$msgPostedTime)
  }
  print("Aggregating Data")
  a <- aggregate(x$sent_Num, by = list(Date = x$msgPostedTime), mean)
  a <- xts(a[,-1],a$Date)
  return (a)
}
if(!file.exists("ts.RData")){ #TODO: check last modified date of ts.RData, if older than x amount of time, then refresh, otherwise use same data
  sent_cpc_ts <- groupTweetsbyTime(tweets_cpc)
  sent_gpc_ts <- groupTweetsbyTime(tweets_gpc)
  sent_lpc_ts <- groupTweetsbyTime(tweets_lpc)
  sent_ndp_ts <- groupTweetsbyTime(tweets_ndp)
  sent_bq_ts <- groupTweetsbyTime(tweets_bq)
  sent_ts <- cbind(sent_cpc_ts, sent_lpc_ts, sent_gpc_ts, sent_ndp_ts, sent_bq_ts)
  save(sent_ts, file = "ts.RData")  
} else{
  load("ts.RData")
  ls()
}


#####################################################################################################################

shinyServer(function(input, output) {
  
  output$dygraph <- renderDygraph({
    dygraph(sent_ts, main = "Overall Twitter Sentiment per Party") %>%
      dySeries("..1", label = "CPC", drawPoints = TRUE, color = "blue") %>%
      dySeries("..2", label = "LPC", drawPoints = TRUE, color = "red") %>%
      dySeries("..3", label = "GPC", drawPoints = TRUE, color = "green") %>%
      dySeries("..4", label = "NDP", drawPoints = TRUE, color = "orange") %>%
      dySeries("..5", label = "BQ", drawPoints = TRUE, color = "lightskyblue") %>%
      dyRangeSelector()
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Stephen Harper" = sentimentScorebyProvince(tweets_cpc),
           "Thomas Mulcair"= sentimentScorebyProvince(tweets_ndp),
           "Justin Trudeau" = sentimentScorebyProvince(tweets_lpc),
           "Gilles Duceppe" = sentimentScorebyProvince(tweets_bq),
           "Elizabeth May" = sentimentScorebyProvince(tweets_gpc))
  })
  
  output$view <- renderGvis({
    gvisGeoChart(datasetInput(), locationvar='smaAuthorState', "x",
                 options=list(region="CA", displayMode="regions", resolution="provinces"))
  })
})
