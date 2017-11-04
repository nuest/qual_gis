
library(RODBC)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)
db <- "C:/Users/Eric/Documents/Gdrive/Studium/qual_gis/WOS_Literatur/Endnote_to_Citavi/Datenbank/Database_WOS.accdb"
# Rechner db <- "E:/GoogleDrive/Studium/qual_gis/WOS_Literatur/Endnote_to_Citavi/Datenbank/Database_WOS.accdb"
con <- odbcConnectAccess2007(db)
#loading tables
all <- sqlTables(con, tableType = "TABLE")$TABLE_NAME
all2 <-sqlTables(con, tableType = "TABLE")
#queries
qryRel <- "SELECT * FROM tblWOS,tblQUAL_GIS WHERE fidCitavi = idCitavi AND Qual_Context = 1 ORDER BY idCItavi "
relevant <- sqlQuery(con, qryRel)
# formatting data from query - Publication count
year<-data.frame(year = relevant$year)
year<-year$year [order(year)]
nryear <- data.frame(year = year, nr =c(1:nrow(relevant)))


# final data for p1
yfreq <- nryear %>%
  count(year)

yfreq <- cbind(yfreq, "nr" = cumsum(yfreq$n))

#plot p1 - all relevant publications

p1 <- yfreq %>%
  filter(year < 2017) %>%
  ggplot(aes(year, nr))+ geom_bar(stat = "identity", aes(fill = n), width = 0.8) + scale_fill_gradient(high="cyan", low="red")+
  ylab("number of Publications") + xlab("")+
  scale_x_continuous(breaks =seq(1994,2016,2)) + scale_y_continuous(breaks = seq(0,400, 50))+
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  guides(fill=guide_legend(title= "Frequency per year")) + theme(legend.position = "bottom")
p1

odbcCloseAll()
