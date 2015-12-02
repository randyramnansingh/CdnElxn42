require(ibmdbR)
require(ibmdbRXt)
require(ggplot2)
require(grid)
require(leaflet)
require(RColorBrewer)

# Connect to the dashDB for accessing the tables 
con <- idaConnect('BLUDB','','')
idaInit(con)
#idaShowTables()
#ibmdbR:::idaDeleteTempTables()

load("can1.Rdata",envir=.GlobalEnv) #changed from ~/Documents/TwitterSentiment/can1.Rdata

# Gather data for all positive,negative and neutral tweets 
# for a single political party per province
print("Attempting to get data")
tweets_cpc <- ida.data.frame('DASH013612."tweets_CPC"') # Conservative 
tweets_ndp <- ida.data.frame('DASH013612."tweets_NDP"') # New Democratic
tweets_gpc <- ida.data.frame('DASH013612."tweets_GPC"') # Green
tweets_lpc <- ida.data.frame('DASH013612."tweets_LPC"') # Liberal
tweets_bq <- ida.data.frame('DASH013612."tweets_BQ"')   # Bloc Québécois
#tweets_fetd <- ida.data.frame('DASH013612."tweets_FETD"') # Strength in Democracy

print("data retrieved")

######################################################################################################################
# Function to calculate the Sentiment Score by Province for each Party

sentimentScorebyProvince <- function(x)
{
  Provinces = c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland","Nova Scotia","Ontario","Prince Edward","Quebec","Saskatchewan","Northwest Territories","Nunavut","Yukon")
  sent_score <- 0
  for(i in 1:length(Provinces))
  {
    df <- as.data.frame(x[x$smaAuthorState == Provinces[i], "sent_Num"])
    if (nrow(df) == 0)
      sent_score[i] = 0
    else if(sum(df) == 0)
      sent_score[i] = 0
    else
      sent_score[i] =   round((sum(df)/nrow(df)),2)
  }
  return(sent_score)
}
#####################################################################################################################

# calculates sentiment score per party, saves to can1
can1@data$SentScore_cpc <- sentimentScorebyProvince(tweets_cpc)
can1@data$SentScore_gpc <- sentimentScorebyProvince(tweets_gpc)
can1@data$SentScore_lpc <- sentimentScorebyProvince(tweets_lpc)
can1@data$SentScore_ndp <- sentimentScorebyProvince(tweets_ndp)
can1@data$SentScore_bq <- sentimentScorebyProvince(tweets_bq)
#can1@data$SentScore_fetd <- sentimentScorebyProvince(tweets_fetd)

print("Sentiment Score by Province calculated for each party")
print(can1@data$SentScore_cpc)
print(can1@data$SentScore_gpc)
print(can1@data$SentScore_lpc)
print(can1@data$SentScore_ndp)
print(can1@data$SentScore_bq)
#print(can1@data$SentScore_fetd)


# Display the sentiment score by province on a map using leaflet
pal_score = rep(0,13)

myColorPalette <- function(x)
{
  for(i in 1:13)
  {
    if(x[i]>0)
      pal_score[i] = 1
    else if (x[i]<0)
      pal_score[i] = -1
    else 
      pal_score[i] = 0
  }
  return(pal_score)
}

pal_cpc <- myColorPalette(can1@data$SentScore_cpc)
pal_gpc <- myColorPalette(can1@data$SentScore_gpc)
pal_lpc <- myColorPalette(can1@data$SentScore_lpc)
pal_ndp <- myColorPalette(can1@data$SentScore_ndp)
pal_bq <- myColorPalette(can1@data$SentScore_bq)
#pal_fetd <- myColorPalette(can1@data$SentScore_fetd)

print("Color palette defined")
print(pal_cpc)
print(pal_gpc)
print(pal_lpc)
print(pal_ndp)
print(pal_bq)
#print(pal_fetd)

#Stephen Harper - CPC (c1)

state_popup_c1 <- paste("<strong>Province: </strong>", 
                        can1$NAME_1, 
                        "<br><strong><font color = \"#2EFE2E\" >Sentiment Score </font></strong>", 
                        can1@data$SentScore_cpc)


# Tom Mulcair - NDP (c2)

state_popup_c2 <- paste("<strong>Province: </strong>", 
                        can1$NAME_1, 
                        "<br><strong><font color = \"#2EFE2E\">Sentiment Score </font></strong>", 
                        can1@data$SentScore_ndp)

#Justin Trudeau - LPC (c3)

state_popup_c3 <- paste("<strong>Province: </strong>", 
                        can1$NAME_1, 
                        "<br><strong><font color = \"#2EFE2E\">Sentiment Score </font></strong>", 
                        can1@data$SentScore_lpc)

#Gilles Duceppe - BQ (c4)

state_popup_c4 <- paste("<strong>Province: </strong>", 
                        can1$NAME_1, 
                        "<br><strong><font color = \"#2EFE2E\">Sentiment Score </font></strong>", 
                        can1@data$SentScore_bq)

#Elizabeth May - GPC (c5)

state_popup_c5 <- paste("<strong>Province: </strong>", 
                        can1$NAME_1, 
                        "<br><strong><font color = \"#2EFE2E\">Sentiment Score </font></strong>", 
                        can1@data$SentScore_gpc)

#Jean Fortin - fetd  (c6)

#state_popup_c6 <- paste("<strong>Province: </strong>", 
                       # can1$NAME_1, 
                       # "<br><strong><font color = \"#2EFE2E\">Sentiment Score </font></strong>", 
                       # can1@data$SentScore_fetd)

print("Popup for each province on map defined")

#map1 <- leaflet(data = can1) %>% addProviderTiles("CartoDB.Positron")
pal <- colorFactor("RdYlGn", NULL, n = 13)

print("Map defined")

