
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ibmdbR)   # In-Database analytics functions for DB2
require(ibmdbRXt) #In-Database functions for geospatial analytics
require(leaflet) # For rendering map visualisation
require(RColorBrewer) # For rendering color to map
require(ggplot2)

shinyServer(function(input, output) {
  
  #TODO: TimeSeries causes data to be generated twice, either figure out why or force data to be stored locally so it is not generated every call
  output$TimeSeries <- renderPlot({
  
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
      return (a)
    }
    sent_cpc_ts <- groupTweetsbyTime(tweets_cpc)
    sent_gpc_ts <- groupTweetsbyTime(tweets_gpc)
    sent_lpc_ts <- groupTweetsbyTime(tweets_lpc)
    sent_ndp_ts <- groupTweetsbyTime(tweets_ndp)
    sent_bq_ts <- groupTweetsbyTime(tweets_bq)
    #sent_fetd_ts <- groupTweetsbyTime(tweets_fetd)
    print("Grouping done")
    
    #Strength in Democracy plot ommitted simply because there is no data for this party, reduces number of errors R runs into
    ggplot() + geom_line(data = sent_cpc_ts,aes(x = Date, y = x), stat = "smooth", col = "blue", size = 0.8) + geom_line(data = sent_gpc_ts,aes(x = Date, y = x), stat = "smooth", col = "green",size = 0.8) + geom_line(data = sent_lpc_ts,aes(x = Date, y = x), stat = "smooth", col = "red",size = 0.8) + geom_line(data = sent_ndp_ts,aes(x = Date, y = x), stat = "smooth", col = "orange",size = 0.8) + geom_line(data = sent_bq_ts,aes(x = Date, y = x), stat = "smooth", col = "lightskyblue",size = 0.8) + xlab("Date") + ylab("Overall Sentiment") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + scale_colour_manual("",breaks = c("Conservative","Green","Liberal","NDP","BQ"),values = c("blue","green","red","orange","lightskyblue"))
  })
  
  output$MapandChart <- renderLeaflet({
    
    map1 <- leaflet(data = can1) %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -106.857, lat = 58.667, zoom = 3)
    
    if(input$cd  == "c1")
    {
      
      
      #map <- 
      addPolygons(map1,fillColor = ~pal(pal_cpc), fillOpacity = 0.8, color = "#BDBDC3",
                  weight = 1,popup = state_popup_c1) %>% 
        addLegend("bottomright",pal = pal, values = pal_cpc, 
                  title = "Overall Sentiment",opacity = 1)
    }
    else if(input$cd  == "c2")
    {
      
      
      #map <- 
      addPolygons(map1,fillColor = ~pal(pal_ndp), fillOpacity = 0.8, color = "#BDBDC3",
                  weight = 1,popup = state_popup_c2) %>% 
        addLegend("bottomright",pal = pal, values = pal_ndp, 
                  title = "Overall Sentiment",opacity = 1)
    }
    
    else if(input$cd  == "c3")
    {
      #map <- 
      addPolygons(map1,fillColor = ~pal(pal_lpc), fillOpacity = 0.8, color = "#BDBDC3",
                  weight = 1,popup = state_popup_c3) %>% 
        addLegend("bottomright",pal = pal, values = pal_lpc, 
                  title = "Overall Sentiment",opacity = 1)
    }
    
    else if(input$cd == "c4")
    {
      
      #map <- 
      addPolygons(map1,fillColor = ~pal(pal_bq), fillOpacity = 0.8, color = "#BDBDC3",
                  weight = 1,popup = state_popup_c4) %>% 
        addLegend("bottomright",pal = pal, values = pal_bq, 
                  title = "Overall Sentiment",opacity = 1)
    }
    
    else if(input$cd  == "c5")
    {
      #map <- 
      addPolygons(map1,fillColor = ~pal(pal_gpc), fillOpacity = 0.8, color = "#BDBDC3",
                  weight = 1,popup = state_popup_c5) %>% 
        addLegend("bottomright",pal = pal, values = pal_gpc, 
                  title = "Overall Sentiment",opacity = 1)
    }
  #  else if(input$cd  == "c6")
  #  {
      
      #map <- 
  #    addPolygons(map1,fillColor = ~pal(pal_fetd), fillOpacity = 0.8, color = "#BDBDC3",
  #                weight = 1,popup = state_popup_c6) %>% 
  #      addLegend("bottomright",pal = pal, values = pal_fetd,
  #                title = "Overall Sentiment",opacity = 1)
  #  }
    
  })
})