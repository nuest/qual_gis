Titel <- "number of pubs./ number of pubs. per million"

# before starting ----

# !!!!!change DB directory !!!!

# !!!!!requires internet connection !!!!

print(Titel)
#************
#Packages---------
#************
#Database & Data handling
library(RODBC) # Database
library(dplyr)
library(plotly)
#TextMine
library(tidytext) #
library(tidyr) #
library(purrr) #
library(readr)
library(widyr) # Correlation must be installed via devtools ("dgrtwo/widyr")
library(stringr) #
#Visual & Plotting
library(ggplot2) # Visual
library(ggraph) # Visual
library(igraph) # Visual
library(ggthemes) #
#Spatial operations and mapping
library(leaflet) #Spatial & Map
library(sp) # Spatial & Map
library(rgdal)

#**********************************************************
#Loading Database-------------------------------
#**********************************************************
#loading Access db


# !!!!!change DB directory !!!!

# !!!!!requires internet connection !!!!


db <- "E:/GoogleDrive/Studium/qual_gis/WOS_Literatur/Endnote_to_Citavi/Datenbank/Database_WOS.accdb"
#db <- "C:/Users/Eric/Documents/Gdrive/Studium/qual_gis/WOS_Literatur/Endnote_to_Citavi/Datenbank/Database_WOS.accdb"
#db <- "E:/GoogleDrive/Studium/qual_gis/WOS_Literatur/Endnote_to_Citavi/Datenbank/Database_WOS.accdb"
con <- odbcConnectAccess2007(db)

#queries + new selection of Abstracts
qryRel <- "SELECT tblWOS.idCItavi, tblQUAL_GIS.Qual_Context, tblAbstract.abstract, *
FROM tblQUAL_GIS, tblAbstract INNER JOIN tblWOS ON tblAbstract.WOS = tblWOS.WOS
WHERE (((tblQUAL_GIS.fidCitavi)=[idCitavi]) AND ((tblQUAL_GIS.Qual_Context)=True))
ORDER BY tblWOS.idCItavi;"
relevant <- sqlQuery(con, qryRel)

#**********************************************************
#DATA Formatting-------------------------------
#**********************************************************
c1 <- data.frame(w = as.character(relevant$WOS),
                 c = str_split_fixed(relevant$fidCountries, ";", 4),
                 y = relevant$year)

c1[5] <- NULL  # deleting // just 1st Author
c1[4] <- NULL
c1[3] <- NULL

colnames(c1)[2] <- "c"

c1 <- transform(c1, c = as.character(c))

#*****************
#rename countryIDs-----
#*****************
c1[,"c"][c1[,"c"] == "1"]<- "United States of America"
c1[,"c"][c1[,"c"] == "2"]<- "Canada"
c1[,"c"][c1[,"c"] == "3"]<- "France"
c1[,"c"][c1[,"c"] == "4"]<- "Germany"
c1[,"c"][c1[,"c"] == "5"]<- "Belgium"
c1[,"c"][c1[,"c"] == "6"]<- "Netherlands"
c1[,"c"][c1[,"c"] == "7"]<- "Switzerland"
c1[,"c"][c1[,"c"] == "8"]<- "United Kingdom"
c1[,"c"][c1[,"c"] == "9"]<- "Italy"
c1[,"c"][c1[,"c"] == "10"]<-"Spain"
c1[,"c"][c1[,"c"] == "11"]<-"Sweden"
c1[,"c"][c1[,"c"] == "12"]<-"Norway"
c1[,"c"][c1[,"c"] == "13"]<-"China"
c1[,"c"][c1[,"c"] == "14"]<-"Russia"
c1[,"c"][c1[,"c"] == "15"]<-"Australia"
c1[,"c"][c1[,"c"] == "16"]<-"Indonesia"
c1[,"c"][c1[,"c"] == "17"]<-"Ghana"
c1[,"c"][c1[,"c"] == "18"]<-"Yemen"
c1[,"c"][c1[,"c"] == "19"]<-"Denmark"
c1[,"c"][c1[,"c"] == "20"]<-"Austria"
c1[,"c"][c1[,"c"] == "21"]<-"Turkey"
c1[,"c"][c1[,"c"] == "22"]<-"Barbados"
c1[,"c"][c1[,"c"] == "23"]<-"Finland"
c1[,"c"][c1[,"c"] == "24"]<-"Brazil"
c1[,"c"][c1[,"c"] == "25"]<-"India"
c1[,"c"][c1[,"c"] == "26"]<-"South Africa"
c1[,"c"][c1[,"c"] == "27"]<-"Portugal"
c1[,"c"][c1[,"c"] == "28"]<-"Bulgaria"
c1[,"c"][c1[,"c"] == "29"]<-"Peru"
c1[,"c"][c1[,"c"] == "30"]<-"Iran"
c1[,"c"][c1[,"c"] == "31"]<-"Tanzania"
c1[,"c"][c1[,"c"] == "32"]<-"Poland"
c1[,"c"][c1[,"c"] == "34"]<-"New Zealand"
c1[,"c"][c1[,"c"] == "35"]<-"Mexico"
c1[,"c"][c1[,"c"] == "36"]<-"Thailand"
c1[,"c"][c1[,"c"] == "37"]<-"Costa Rica"
c1[,"c"][c1[,"c"] == "38"]<-"Philippines"
c1[,"c"][c1[,"c"] == "39"]<-"Singapore"
c1[,"c"][c1[,"c"] == "40"]<-"Bangladesh"
c1[,"c"][c1[,"c"] == "41"]<-"Slovenia"
c1[,"c"][c1[,"c"] == "42"]<-"South Korea"
c1[,"c"][c1[,"c"] == "43"]<-"Israel"
c1[,"c"][c1[,"c"] == "44"]<-"Malaysia"
c1[,"c"][c1[,"c"] == "45"]<-"Greece"
c1[,"c"][c1[,"c"] == "46"]<-"Ireland"
c1[,"c"][c1[,"c"] == "47"]<-"Japan"
c1[,"c"][c1[,"c"] == "48"]<-"Pakistan"
c1[,"c"][c1[,"c"] == "49"]<-"Chile"
c1[,"c"][c1[,"c"] == "50"]<-"Argentina"
c1[,"c"][c1[,"c"] == "51"]<-"Uganda"
c1[,"c"][c1[,"c"] == "52"]<-"Czech Republic"
c1[,"c"][c1[,"c"] == "53"]<-"Iceland"
c1[,"c"][c1[,"c"] == "54"]<-"Bolivia"
c1[,"c"][c1[,"c"] == "55"]<-"Venezuela"
c1[,"c"][c1[,"c"] == "56"]<-"China"
c1[,"c"][c1[,"c"] == "33"]<- "China"
c1[,"c"][c1[,"c"] == "NA"]<- NA
c1[,"c"][c1[,"c"] == ""]<- NA
c1 <- c1[complete.cases(c1$c),] # remove NAs caused by spliting the country IDs


###**********
##Data arrangment--------
###*********

total_c1 <- c1 %>%
  group_by(c) %>%
  count(c)

library(wpp2015)  # world population dataset 2015

data(pop)
colnames(pop)[2] <- "c"

total_c1 <- pop %>%
  select(c,`2015`)%>%
  left_join(total_c1, pop, by = "c") # ignore waring, caused by NAs

total_c1 <- total_c1[complete.cases(total_c1$n),] # remove NAs caused by spliting the country IDs

total_c1 <- cbind(total_c1,"ppm"= 0)
colnames(total_c1)[2] <- "p"

total_c1 <- transform(total_c1, n = as.numeric(n))
total_c1 <- transform(total_c1, p = as.numeric(p))


###**********
###analysis------
###**********

perMillion <- function(n,p){
  temp =(n/(p*1000)) # dataset uses dots....
  result = temp * 1000000
  return(result)}

total_c1$ppm <- mapply(perMillion,total_c1$n,total_c1$p)




###*******
###Plots-----
###*******


plot_country <- total_c1 %>%
  filter(n > 4) %>%
  ggplot(aes(reorder(c,n),n, fill= n)) + geom_bar(stat = "identity", show.legend = FALSE)+
  coord_flip() + scale_y_continuous(breaks = seq(0,200,20)) +
  scale_fill_gradient(high="red", low="grey") +
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  geom_text(aes(label = n), size = 3.5, alpha= 0.6, hjust = 1.6) +
  xlab("") + ylab("")+
  theme(axis.text.y = element_text(size = 11, angle = 0))
plot_country


plot_pM <- total_c1 %>%
  filter(ppm > 0.5 & n > 2) %>%
  ggplot(aes(reorder(c,ppm),ppm, fill= ppm)) + geom_bar(stat = "identity", show.legend = FALSE)+
  coord_flip() + scale_y_continuous(breaks = seq(0,3, 0.5)) +
  scale_fill_gradient(high="red", low="grey") +
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  geom_text(aes(label =format(round(ppm, 2), nsmall = 2)), size = 3.5, alpha= 0.6, hjust = 1.4) +
  xlab("") + ylab("")+
  theme(axis.text.y = element_text(size = 11, angle = 0))
plot_pM


wrap <- total_c1 %>%
  filter(ppm > 0.5 & n > 2) %>%
  ggplot(aes())

ggplot(aes(reorder(name, total_titel),total_titel, fill = total_titel)) +
  geom_bar(stat = "identity", show.legend = FALSE) + coord_flip()+
  scale_y_continuous(breaks = seq(0,200,20)) +
  scale_fill_gradient(high="red", low="grey") +
  ggtitle("Veröffentlichungen pro Land") + ylab("Anzahl Veröffentlichungen") + xlab("")+
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  geom_text(aes(label = total_titel), size = 3.5, alpha= 0.6, hjust = 1.6) +
  theme(axis.text.y = element_text(size = 11, angle = 0))


plot_country

plt_tf <-tf_idf %>%
  select(country,word,tf_idf, total_titel, n)%>%
  filter(country == "United States"| country == "Canada"
         | country == "Finland" |country == "United Kingdom" |
           country == "Australia") %>%
  filter(country == "Finland" | country == "United Kingdom" | country == "Canada") %>%
  ggplot(aes(n) ) + geom_histogram(bins = "80", fill = "indianred") +
  facet_wrap(~country)+ ylab("")+
  scale_fill_gradient(high="red", low="grey") + xlab("Worthäufigkeiten") +
  theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12, angle = 0), axis.text.x = element_text(size=12),
        axis.title = element_text(size = 14))





odbcCloseAll()
