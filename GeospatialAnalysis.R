library(ibmdbR)
library(ibmdbRXt)
library(ggplot2)
library(plotrix)
print("Geospatial Analysis called")
con <- idaConnect('BLUDB','','')
idaInit(con)
idaShowTables()
#ibmdbR:::idaDeleteTempTables()


# Read the geospatial shapefile for Canada and demographics data
idaCan <- ida.data.frame('DASH013612."GEO_CANADA2"')
CanSRS <- idaSrsId(idaCan$GEO_DATA) # Obtain the Spatial Reference System of the Canada provinces shapefile
Can_popu <- ida.data.frame('DASH013612."CAN_POPU"')

print("Geospatial data retrieved")

# Merge the demographic data with Canada geometry
Can_demo <- idaMerge(idaCan,Can_popu,by.x = "ID_1", by.y = "ID", all = FALSE)
print("Merged geospatial data with demographics data for canada")
Can_demo <- Can_demo[,c("ID_1","PROVINCE","P_2015","GEO_DATA")]
Can_demo$AREA <- idaArea(Can_demo)
#Can_demo <- as.data.frame(Can_demo)

print("Area calculated with geometry and demographics")

idacpc <- ida.data.frame('DASH013612."tweets_CPC"')
idagpc <- ida.data.frame('DASH013612."tweets_GPC"')
idalpc <- ida.data.frame('DASH013612."tweets_LPC"')
idandp <- ida.data.frame('DASH013612."tweets_NDP"')
idabq <- ida.data.frame('DASH013612."tweets_BQ"')
idafetd <- ida.data.frame('DASH013612."tweets_FETD"')

print("ida.data.frames created with extracted coordinates")

# Create a new column for storing ST_POINT geometry in the ida.data.frame for each political party

idacpc$GEO_DATA <- idaPoint(list(idacpc$x,idacpc$y),CanSRS)
idagpc$GEO_DATA <- idaPoint(list(idagpc$x,idagpc$y),CanSRS)
idalpc$GEO_DATA <- idaPoint(list(idalpc$x,idalpc$y),CanSRS)
idandp$GEO_DATA <- idaPoint(list(idandp$x,idandp$y),CanSRS)
idabq$GEO_DATA <- idaPoint(list(idabq$x,idabq$y),CanSRS)
idafetd$GEO_DATA <- idaPoint(list(idafetd$x,idafetd$y),CanSRS)

print("ST_POINT created")

# Count the tweets from Alberta for each political party 

avgSentbyAttrib <- function(x,n)
{
  Prov = c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador","Northwest Territories","Nova Scotia","Nunavut","Ontario","Prince Edward Island","Quebec","Saskatchewan","Yukon")
  ID =c(1:13)
  for(i in 1:length(Prov))
  {
    idaprov <- Can_demo[Can_demo$PROVINCE == Prov[i],]
    
    tweets_within_Prov <- idaWithin(x,idaprov)
    l <- length(tweets_within_Prov)
    tweets_within_Prov <- tweets_within_Prov[tweets_within_Prov[,l] == 1, c("msgId","sent_Num")]
    tweets_df <- as.data.frame(tweets_within_Prov)
    score[i] <- round((sum(tweets_df$sent_Num)/nrow(tweets_df)),2)
  }
  df <- data.frame(ID,Prov,score)
  idf <- as.ida.data.frame(df)
  
  # Merge the scores of each province with population for 2011 to 2015
  
  idf <- idaMerge(idf,Can_demo,by.x = "ID", by.y = "ID_1", all = FALSE)
  if(n == 1)
  {
    df <- as.data.frame(idf[,c("ID","PROVINCE","score","P_2015")])
    wtdAvg <- aggregate(df$score, by = list(Province = df$PROVINCE, Population = df$P_2015), mean)
  }
  else if (n == 2)
  {
    df <- as.data.frame(idf[,c("ID","PROVINCE","score","AREA")])
    wtdAvg <- aggregate(df$score, by = list(Province = df$PROVINCE, Area = df$AREA), mean)
  }
  
  return (wtdAvg)
}

print("function for sentiment score by attributes defined")


# Find the variance of sentiment with the population density per province 
cpc_score_by_Popu <- avgSentbyAttrib(idacpc,1)
gpc_score_by_Popu <- avgSentbyAttrib(idagpc,1)
lpc_score_by_Popu <- avgSentbyAttrib(idalpc,1)
ndp_score_by_Popu <- avgSentbyAttrib(idandp,1)
bq_score_by_Popu <- avgSentbyAttrib(idabq,1)
fetd_score_by_Popu <- avgSentbyAttrib(idafetd,1)
print("Score by Area calculated")


# Find the variation of sentiment with the size of the province
cpc_score_by_Area <- avgSentbyAttrib(idacpc,2)
gpc_score_by_Area <- avgSentbyAttrib(idagpc,2)
lpc_score_by_Area <- avgSentbyAttrib(idalpc,2)
ndp_score_by_Area <- avgSentbyAttrib(idandp,2)
bq_score_by_Area <- avgSentbyAttrib(idabq,2)
fetd_score_by_Area <- avgSentbyAttrib(idafetd,2)
print("Score by Area calculated")


# Plot the variation in overall sentiment with Population density
g = ggplot()+geom_point(data = cpc_score_by_Popu,aes(x = round(as.numeric(Population)),y = x, colour = "Conservative"))+geom_line(data = cpc_score_by_Popu,aes(x = as.numeric(Population),y = x), col = "red",stat = "smooth") 
g1 = g + geom_point(data = gpc_score_by_Popu,aes(x = round(as.numeric(Population)),y = x, colour = "Green"))+geom_line(data = gpc_score_by_Popu,aes(x = as.numeric(Population),y = x), col = "blue",stat = "smooth") 
g2 = g1 + geom_point(data = lpc_score_by_Popu,aes(x = round(as.numeric(Population)),y = x, colour = "Liberal"))+geom_line(data = lpc_score_by_Popu,aes(x = as.numeric(Population),y = x), col = "green",stat = "smooth")
g3 = g2 + geom_point(data = ndp_score_by_Popu,aes(x = round(as.numeric(Population)),y = x, colour = "NDP"))+geom_line(data = ndp_score_by_Popu,aes(x = as.numeric(Population),y = x), col = "yellow",stat = "smooth")
g4 = g3 + geom_point(data = bq_score_by_Popu,aes(x = round(as.numeric(Population)),y = x, colour = "BQ"))+geom_line(data = bq_score_by_Popu,aes(x = as.numeric(Population),y = x), col = "black",stat = "smooth") 
g5 = g4 + geom_point(data = fetd_score_by_Popu,aes(x = round(as.numeric(Population)),y = x, colour = "Strength in Democracy"))+geom_line(data = fetd_score_by_Popu,aes(x = as.numeric(Population),y = x), col = "orange",stat = "smooth") 
g6 = g5 + xlab("Population in Thousands") + ylab("Average Sentiment Score") + scale_colour_manual("",breaks = c("Conservative","Green","Liberal","NDP","BQ","Strength in Democracy"),values = c("red","blue","green","yellow","black","orange"))
g7 = g6 + ggtitle("Variation of Twitter Sentiment with Population Density of Provinces")

g7

# Plot the variation in overall sentiment with Area of the Province
a = ggplot()+geom_point(data = cpc_score_by_Area,aes(x = round(Area),y = x, colour = "Conservative"))+geom_line(data = cpc_score_by_Area,aes(x = round(Area),y = x), col = "red",stat = "smooth") 
a1 = a + geom_point(data = gpc_score_by_Area,aes(x = round(Area),y = x, colour = "Green"))+geom_line(data = gpc_score_by_Area,aes(x = round(Area),y = x), col = "blue",stat = "smooth") 
a2 = a1 + geom_point(data = lpc_score_by_Area,aes(x = round(Area),y = x, colour = "Liberal"))+geom_line(data = lpc_score_by_Area,aes(x = round(Area),y = x), col = "green",stat = "smooth")
a3 = a2 + geom_point(data = ndp_score_by_Area,aes(x = round(Area),y = x, colour = "NDP"))+geom_line(data = ndp_score_by_Area,aes(x = round(Area),y = x), col = "yellow",stat = "smooth")
a4 = a3 + geom_point(data = bq_score_by_Area,aes(x = round(Area),y = x, colour = "BQ"))+geom_line(data = bq_score_by_Area,aes(x = round(Area),y = x), col = "black",stat = "smooth") 
a5 = a4 + geom_point(data = fetd_score_by_Area,aes(x = round(Area),y = x, colour = "Strength in Democracy"))+geom_line(data = fetd_score_by_Area,aes(x = round(Area),y = x), col = "orange",stat = "smooth") 
a6 = a5 + xlab("Area in miles") + ylab("Average Sentiment Score") + scale_colour_manual("",breaks = c("Conservative","Green","Liberal","NDP","BQ","Strength in Democracy"),values = c("red","blue","green","yellow","black","orange"))
a7 = a6 + ggtitle("Variation of Twitter Sentiment with Geographic Area of Provinces")


a7

# Pie chart for variation of twitter sentiment with Geographic area of the Provinces for Conservative Party

colnames(cpc_score_by_Area) <- c("Province","Area","OverallSentiment")

slices <- cpc_score_by_Area$Area
lbls <- paste(cpc_score_by_Area$OverallSentiment,cpc_score_by_Area$Province)

colorPie <- function(x)
{
  l = length(x$OverallSentiment)
  col = rep("x",13)
  for(i in 1:l)
  {
    if(x$OverallSentiment[i] <0)
      col[i] = "red"
    else if(x$OverallSentiment[i] = 0)   
      col[i] = "yellow"
    else
      col[i] = "green" 
  }
  return(col)  
}

pie3D(slices,labels = lbls, col=colorPie(cpc_score_by_Area), main="Distribution of Twitter Sentiment by Area for Conservative Party",explode = 0.2)
# ibmdbR:::idaDeleteTempTables()
idaClose(con)