#Titel: spatial textmining just 1st authors

# before starting ----

# !!!!!change DB directory !!!!

# !!!!!requires internet connection !!!!

print("spatial textmining - 1st author")
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


library(ggmap) # Map
library(maptools) # Map
library(maps) # Map
#**********************************************************
#Loading Database-------------------------------
#**********************************************************
#loading Access db


# !!!!!change DB directory !!!!

# !!!!!requires internet connection !!!!



db <- "C:/Users/Eric/Desktop/Abgabe/Datenbank/Database_WOS.accdb"

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
c1<-data.frame( w =as.character(relevant$WOS), c =str_split_fixed(relevant$fidCountries, ";", 4))
locAbs <- data.frame(y = relevant$year,
                     t = relevant$titel,
                     a = as.character(relevant$abstract),
                     w = as.character(relevant$WOS))
locAbs <- left_join(locAbs, c1, by = "w")

colnames(locAbs)[5] <- "c"

locAbs[8] <- NULL  # deleting // just 1st Author
locAbs[7] <- NULL
locAbs[6] <- NULL

locAbs <- transform(locAbs, c = as.character(c))

#*****************
#rename countryIDs-----
#*****************
locAbs[,"c"][locAbs[,"c"] == "1"]<- "United States"
locAbs[,"c"][locAbs[,"c"] == "2"]<- "Canada"
locAbs[,"c"][locAbs[,"c"] == "3"]<- "France"
locAbs[,"c"][locAbs[,"c"] == "4"]<- "Germany"
locAbs[,"c"][locAbs[,"c"] == "5"]<- "Belgium"
locAbs[,"c"][locAbs[,"c"] == "6"]<- "Netherlands"
locAbs[,"c"][locAbs[,"c"] == "7"]<- "Switzerland"
locAbs[,"c"][locAbs[,"c"] == "8"]<- "United Kingdom"
locAbs[,"c"][locAbs[,"c"] == "9"]<- "Italy"
locAbs[,"c"][locAbs[,"c"] == "10"]<-"Spain"
locAbs[,"c"][locAbs[,"c"] == "11"]<-"Sweden"
locAbs[,"c"][locAbs[,"c"] == "12"]<-"Norway"
locAbs[,"c"][locAbs[,"c"] == "13"]<-"China"
locAbs[,"c"][locAbs[,"c"] == "14"]<-"Russia"
locAbs[,"c"][locAbs[,"c"] == "15"]<-"Australia"
locAbs[,"c"][locAbs[,"c"] == "16"]<-"Indonesia"
locAbs[,"c"][locAbs[,"c"] == "17"]<-"Ghana"
locAbs[,"c"][locAbs[,"c"] == "18"]<-"Yemen"
locAbs[,"c"][locAbs[,"c"] == "19"]<-"Denmark"
locAbs[,"c"][locAbs[,"c"] == "20"]<-"Austria"
locAbs[,"c"][locAbs[,"c"] == "21"]<-"Turkey"
locAbs[,"c"][locAbs[,"c"] == "22"]<-"Barbados"
locAbs[,"c"][locAbs[,"c"] == "23"]<-"Finland"
locAbs[,"c"][locAbs[,"c"] == "24"]<-"Brazil"
locAbs[,"c"][locAbs[,"c"] == "25"]<-"India"
locAbs[,"c"][locAbs[,"c"] == "26"]<-"South Africa"
locAbs[,"c"][locAbs[,"c"] == "27"]<-"Portugal"
locAbs[,"c"][locAbs[,"c"] == "28"]<-"Bulgaria"
locAbs[,"c"][locAbs[,"c"] == "29"]<-"Peru"
locAbs[,"c"][locAbs[,"c"] == "30"]<-"Iran"
locAbs[,"c"][locAbs[,"c"] == "31"]<-"Tanzania"
locAbs[,"c"][locAbs[,"c"] == "32"]<-"Poland"
locAbs[,"c"][locAbs[,"c"] == "34"]<-"New Zealand"
locAbs[,"c"][locAbs[,"c"] == "35"]<-"Mexico"
locAbs[,"c"][locAbs[,"c"] == "36"]<-"Thailand"
locAbs[,"c"][locAbs[,"c"] == "37"]<-"Costa Rica"
locAbs[,"c"][locAbs[,"c"] == "38"]<-"Philippines"
locAbs[,"c"][locAbs[,"c"] == "39"]<-"Singapore"
locAbs[,"c"][locAbs[,"c"] == "40"]<-"Bangladesh"
locAbs[,"c"][locAbs[,"c"] == "41"]<-"Slovenia"
locAbs[,"c"][locAbs[,"c"] == "42"]<-"South Korea"
locAbs[,"c"][locAbs[,"c"] == "43"]<-"Israel"
locAbs[,"c"][locAbs[,"c"] == "44"]<-"Malaysia"
locAbs[,"c"][locAbs[,"c"] == "45"]<-"Greece"
locAbs[,"c"][locAbs[,"c"] == "46"]<-"Ireland"
locAbs[,"c"][locAbs[,"c"] == "47"]<-"Japan"
locAbs[,"c"][locAbs[,"c"] == "48"]<-"Pakistan"
locAbs[,"c"][locAbs[,"c"] == "49"]<-"Chile"
locAbs[,"c"][locAbs[,"c"] == "50"]<-"Argentina"
locAbs[,"c"][locAbs[,"c"] == "51"]<-"Uganda"
locAbs[,"c"][locAbs[,"c"] == "52"]<-"Czech Republic"
locAbs[,"c"][locAbs[,"c"] == "53"]<-"Iceland"
locAbs[,"c"][locAbs[,"c"] == "54"]<-"Bolivia"
locAbs[,"c"][locAbs[,"c"] == "55"]<-"Venezuela"
locAbs[,"c"][locAbs[,"c"] == "56"]<-"China"
locAbs[,"c"][locAbs[,"c"] == "33"]<- "China"
locAbs[,"c"][locAbs[,"c"] == "NA"]<- NA
locAbs[,"c"][locAbs[,"c"] == ""]<- NA
locAbs <- locAbs[complete.cases(locAbs$c),] # remove NAs caused by spliting the country IDs


#*******************************************************************************
#Perprocess Text----------------------------------------------------------------
#*******************************************************************************
#transform to char, run two times
locAbs <- transform(locAbs, abs = as.character(a))
locAbs <- transform(locAbs, country = as.factor(c))

clean_text_org <- locAbs %>%
  group_by(c, t) %>%
  ungroup()

y = 0
j = 1995
for (j in c(2010,2012,2015,2018)){
  clean_text<-  clean_text_org%>% 
    filter(y < j)
  
  
  
  y = y +1
  print(y)
  print(j)
  
  #removing stopwords and custom words-----
  print("start removing")
  clean_text <- transform(clean_text, a = as.character(a))
  #clean_text <- clean_text[complete.cases(clean_text$a),] # remove NAs caused by spliting the country IDs
  
  
  LocAbs_words <- clean_text %>%
    unnest_tokens(word,a)%>%    # schreibe alle Worte in neue Zeile behalte dabei LandID
    filter(str_detect(word, "[a-z']$"),
           !str_detect(word,"^research"),
           !str_detect(word,"^based"),
           !str_detect(word,"^information"),
           !str_detect(word,"^study"),
           !str_detect(word,"^data"),
           !str_detect(word, "^de"),
           !str_detect(word, "^ah"),
           !str_detect(word, "^e.g"),
           !str_detect(word, "^elsevier"),
           !str_detect(word, "^la"),
           !str_detect(word, "^los"),
           !str_detect(word, "^el"),
           !str_detect(word, "^es"),
           !str_detect(word, "^en"),
           !str_detect(word, "^attachment"),
           !str_detect(word, "^key"),
           !str_detect(word, "^select"),
           !str_detect(word, "^sample"),
           !str_detect(word, "^view"),
           !str_detect(word, "^term"),
           !str_detect(word, "^recently"),
           !str_detect(word, "^obtain"),
           !str_detect(word, "^makes"),
           !str_detect(word, "^avoided"),
           !str_detect(word, "^article"),
           !str_detect(word, "^i.e"),
           !str_detect(word, "^introduction"),
           !word %in% stop_words$word) %>%
    count(c, word, sort = TRUE)     # zähle Worte, abs muss dabei chr sein transform nicht vergessen
  

  #total words and titel ----
  
  
  total_words <- LocAbs_words %>%
    group_by(c) %>%
    summarize(total = sum(n))
  
  total_titel <- clean_text %>%    # zähle Veröffentlichung pro land
    group_by(t) %>%
    count(c,t, sort = TRUE) %>%
    group_by(c) %>%
    summarize(total_titel = sum(n)) 
  
  total_titel <- left_join(total_titel, total_words) #join for popup Nr. of Papers and Words
  
  LocAbs_words <- left_join(LocAbs_words, total_words)
  LocAbs_words <- left_join(LocAbs_words, total_titel)
  
  
  #************
  #geocode the countries ----
  #********
  colnames(LocAbs_words)[1] <- "country"
  
  geo1<- as.data.frame(table(LocAbs_words$country))           # get country names
  geo <- geocode(as.character(geo1$Var1), output = c("more")) # geocode them
  LocAbs_words <- left_join(LocAbs_words, geo)
  
  colnames(geo1)[1] <- "name" #change for later joining
  
  print("geoccode done")
  ##words by country 
  words_by_country <- LocAbs_words %>%
    count(country, word,n, sort = TRUE) %>%
    arrange(desc(country, n)) 
  
  
  #**********************************************************
  #tf-idf-------------------------------
  #**********************************************************
  # berechnet tf-idf für jedes Worte und gruppiert dabei nach LänderID
  
  
  tf_idf <- LocAbs_words %>%
    
    bind_tf_idf(word, country, n) %>%
    arrange(desc(tf_idf))
  tf_idf
  print("tf_idf done")
  # Join with words by country to identify most important words by tf idf
  colnames(tf_idf)[9] <- "name"
  tf_idf_base <- left_join(words_by_country, tf_idf)
  
  tf_idf_word <- data.frame(name = tf_idf_base$country,            #join the tf_idf
                            word = as.character(tf_idf_base$word),
                            tf_idf = tf_idf_base$tf_idf)
  
  tf_idf_word <- transform(tf_idf_word, word = as.character(word)) #if not it wont work because words in the list just factors
  
  tf_idf_word <- tf_idf_word %>%  # tf_id_word is needed for verticies popups
    filter(tf_idf > 0.003) %>%    # Popups shows just words which have a tf_idf over 0.003
    arrange(desc(name,tf_idf))
  
  tf_idf_word[3] <- NULL
  
  tf_idf_word <- tf_idf_word %>%
    nest(word)
  
  print("tf_idf join done")
  #**********
  # pairweis pearson Cor-----
  #**********
  
  
  ###new with filter tf_idf
  #working cor
  LocAbs_cors<- tf_idf %>%
    filter(total_titel > 1)%>%
    filter(tf_idf > 0.0009) %>%# filter tf_idf to remove unimportant words
    pairwise_cor(country, word,n, sort = TRUE)
  print("cor done")
  
  ## new word count after tf_idf filter
  ttl_word2<- tf_idf %>%
    filter(tf_idf > 0.0009)%>%
    count(country, word, sort = TRUE) %>%
    group_by(country) %>%
    summarize(total2= sum(nn))
  colnames(ttl_word2)[1]<- "name"
  
  tf_idf_word <- left_join(tf_idf_word, ttl_word2)
  #************
  #Data for Plot-----
  #************
  
  colnames(LocAbs_cors)[1] <- "country"
  colnames(LocAbs_cors)[2] <- "country2"
  
  LocAbs_cors <- left_join(LocAbs_cors, geo) ####Spatial Info of Corellations
  
  
  
  df <- data.frame(from = LocAbs_cors$country, 
                   to =LocAbs_cors$country2,
                   correlation = LocAbs_cors$correlation,
                   cor = LocAbs_cors$correlation,
                   weight = LocAbs_cors$correlation,
                   lon = LocAbs_cors$lon,
                   lat = LocAbs_cors$lat)
  df <- df[complete.cases(df),]
  df <- df[!duplicated(df[,3]),]
  
  
  df$weight[df$correlation <0.4] <-0.2
  df$weight[df$correlation >0.5] <-0.5
  df$weight[df$correlation > 0.5] <-2
  df$weight[df$correlation > 0.6] <-5
  df$weight[df$correlation > 0.7] <-10
  df$weight[df$correlation > 0.8] <-12.5
  df$weight[df$correlation > 0.9] <-17
  
  
  df$cor[df$correlation > 0.4] <-0.4
  df$cor[df$correlation > 0.5] <-0.5
  df$cor[df$correlation > 0.6] <-0.6
  df$cor[df$correlation > 0.7] <-0.7
  df$cor[df$correlation > 0.8] <-0.8
  df$cor[df$correlation > 0.9] <-0.9
  
  df <- df[!(df$cor < 0.4),]
  
  print("df done")
  meta <- data.frame(name = LocAbs_cors$country,
                     lon = LocAbs_cors$lon,
                     lat = LocAbs_cors$lat)
  
  meta <- meta[complete.cases(meta),]
  meta <- meta[!duplicated(meta),]  
  
  lost <- data_frame(name = c("Ireland", "Czech Republic", "Denmark", "Finland","Austria", "Bangladesh"),
                     lon = c(-7.651600,15.268294,9.150484,25.768525,15.617265, 89.802567 ),
                     lat = c(53.422047,49.863959, 55.656755, 62.698190, 47.907434,23.136735))
  meta <- rbind(meta, lost)
  
  colnames(total_titel)[1] <- "name"
  meta <- left_join(meta, total_titel)  # geo1 Häufigkeit der Länder in Freq
  
  meta <- left_join(meta, tf_idf_word) # data for vert titelfreq
  meta <- meta[!duplicated(meta$name),]
  
  print("meta done")
  #********
  #igraph----
  #******
  g <- graph.data.frame(df, directed = FALSE, vertices = meta)
  lo <- layout.norm(as.matrix(meta[,2:3]))
  
  
  #*********
  #Leaflet Map------
  #*********
  ####transforming to spatial objects
  if(y == 1){
    gg10 <- get.data.frame(g, "both")
    vert10 <- gg10$vertices
    coordinates(vert10) <- ~lon+lat
    #*******
    # data 2010------
    #*******
    edges10 <- gg10$edges
    
    edges10 <- lapply(1:nrow(edges10), function(i) {
      as(rbind(vert10[vert10$name == edges10[i, "from"], ], 
               vert10[vert10$name == edges10[i, "to"], ]), 
         "SpatialLines")
    })
    for (i in seq_along(edges10)) {
      edges10[[i]] <- spChFIDs(edges10[[i]], as.character(i))
    }
    edges10 <- do.call(rbind, edges10)
    #content 2010----
    #****
    content10_1 <- paste( 
      "<b>",vert10$name,": ", vert10$total_titel,"Paper(s)", "</b>",
      "</br>",
      "<b>Important words: </b>",
      format(head(vert10$data)),"</br><b>", vert10$total2,
      "total words</b>") # format for a pretty print, head to limit the print
    
    content10_2 <- paste("Correlation:",format(round(gg10$edges$correlation, 2), nsmall = 2),
                         "between:", gg10$edges$from, "-", gg10$edges$to)
    #***
  }#2010
  if(y == 2){
    gg12 <- get.data.frame(g, "both")
    vert12 <- gg12$vertices
    coordinates(vert12) <- ~lon+lat
    #****
    #data 2012-----
    #****
    edges12 <- gg12$edges
    
    edges12 <- lapply(1:nrow(edges12), function(i) {
      as(rbind(vert12[vert12$name == edges12[i, "from"], ], 
               vert12[vert12$name == edges12[i, "to"], ]), 
         "SpatialLines")
    })
    for (i in seq_along(edges12)) {
      edges12[[i]] <- spChFIDs(edges12[[i]], as.character(i))
    }
    edges12 <- do.call(rbind, edges12)
    
    #****
    
    #content 2012----
    #****
    content12_1 <- paste( 
      "<b>",vert12$name,": ", vert12$total_titel,"Paper(s)", "</b>",
      "</br>",
      "<b>Important words: </b>",
      format(head(vert12$data)),"</br><b>", vert12$total2,
      "total words</b>") # format for a pretty print, head to limit the print
    
    content12_2 <- paste("Correlation:",format(round(gg12$edges$correlation, 2), nsmall = 2),
                         "between:", gg12$edges$from, "-", gg12$edges$to)
    
    
    
    #*******
  }#2012
  if(y == 3){
    gg15 <- get.data.frame(g, "both")
    vert15 <- gg15$vertices
    coordinates(vert15) <- ~lon+lat
    #data 2015-----
    #****
    edges15 <- gg15$edges
    
    edges15 <- lapply(1:nrow(edges15), function(i) {
      as(rbind(vert15[vert15$name == edges15[i, "from"], ], 
               vert15[vert15$name == edges15[i, "to"], ]), 
         "SpatialLines")
    })
    for (i in seq_along(edges15)) {
      edges15[[i]] <- spChFIDs(edges15[[i]], as.character(i))
    }
    edges15 <- do.call(rbind, edges15)
    
    #****
    #content 2015----
    #****
    content15_1 <- paste( 
      "<b>",vert15$name,": ", vert15$total_titel,"Paper(s)", "</b>",
      "</br>",
      "<b>Important words: </b>",
      format(head(vert15$data)),"</br><b>", vert15$total2,
      "total words</b>") # format for a pretty print, head to limit the print
    
    content15_2 <- paste("Correlation:",format(round(gg15$edges$correlation, 2), nsmall = 2),
                         "between:", gg15$edges$from, "-", gg15$edges$to)
    
    
    #***
  }#2015
  if(y == 4){ 
    gg17 <- get.data.frame(g, "both")
    vert17 <- gg17$vertices
    coordinates(vert17) <- ~lon+lat
    #data 2017-----
    #****
    edges17 <- gg17$edges
    
    edges17 <- lapply(1:nrow(edges17), function(i) {
      as(rbind(vert17[vert17$name == edges17[i, "from"], ], 
               vert17[vert17$name == edges17[i, "to"], ]), 
         "SpatialLines")
    })
    for (i in seq_along(edges17)) {
      edges17[[i]] <- spChFIDs(edges17[[i]], as.character(i))
    }
    edges17 <- do.call(rbind, edges17)
    #***
    #content 2017----
    #****
    content17_1 <- paste( 
      "<b>",vert17$name,": ", vert17$total_titel,"Paper(s)", "</b>",
      "</br>",
      "<b>Important words: </b>",
      format(head(vert17$data)),"</br><b>", vert17$total2,
      "total words</b>") # format for a pretty print, head to limit the print
    
    content17_2 <- paste("Correlation:",format(round(gg17$edges$correlation, 2), nsmall = 2),
                         "between:", gg17$edges$from, "-", gg17$edges$to)
  }#2017
} #end of for-loop

#Leaflet Map------
#********
leaflet(vert10) %>% addTiles() %>% 
  addPolygons(data = edges10,       # polgons to add hover labels
              weight = df$weight,
              opacity = df$cor,
              label = content10_2,
              smoothFactor = 5,
              color = "cyan", group = "2010",
              highlightOptions = highlightOptions(color = "red", weight = 12,
                                                  bringToFront = TRUE)) %>% 
  addCircleMarkers(data = vert10,
                   radius =vert10$total_titel/3 ,
                   col = "orangered", group = "2010",
                   popup =content10_1) %>%
  addPolygons(data = edges12,       # polgons to add hover labels
              weight = df$weight,
              opacity = df$cor,
              label = content12_2,
              smoothFactor = 5,
              color = "cyan",group= "2012", 
              highlightOptions = highlightOptions(color = "red", weight = 12,
                                                  bringToFront = TRUE)) %>% 
  addCircleMarkers(data = vert12,
                   radius =vert12$total_titel/3 ,
                   col = "orangered", group = "2012",
                   popup =content12_1) %>%
  
  addPolygons(data = edges15,       # polgons to add hover labels
              weight = df$weight,
              opacity = df$cor,
              label = content15_2,
              smoothFactor = 5,
              color = "cyan",group= "2015", 
              highlightOptions = highlightOptions(color = "red", weight = 12,
                                                  bringToFront = TRUE)) %>% 
  addCircleMarkers(data = vert15,
                   radius =vert15$total_titel/3 ,
                   col = "orangered", group = "2015",
                   popup =content15_1) %>%
  
  addPolygons(data = edges17,       # polgons to add hover labels
              weight = df$weight,
              opacity = df$cor,
              label = content17_2,
              smoothFactor = 5,
              color = "cyan",group= "2017",
              highlightOptions = highlightOptions(color = "red", weight = 12,
                                                  bringToFront = TRUE)) %>% 
  addCircleMarkers(data = vert17,
                   radius =vert17$total_titel/3 ,
                   col = "orangered", group = "2017",
                   popup =content17_1) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Dark")%>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, group= "Light") %>%
  addLayersControl(overlayGroups = c("Light"),
                   baseGroups = c("2010", "2012","2015","2017"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(0,30,zoom = 2)





## to shapefile------
lines <- SpatialLinesDataFrame(edges17, data = gg17$edges)
writeOGR(lines, "lines1s","lines1s", driver = "ESRI Shapefile")

gg_points <- gg17$vertices %>%
  select(lon, lat, total_titel, name)

points <- SpatialPointsDataFrame(vert17, data = gg_points)
writeOGR(points, "points17_1s","points17_1s", driver = "ESRI Shapefile")