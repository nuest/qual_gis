#Titel: qual Analysen-gis app combinations

#Internet connection required for connection to the remote DB

#******
#packages---------
#******
pacman::p_load(ggplot2, ggthemes, tidyverse,
               stringr, plotly, ggraph, igraph,
               RPostgreSQL)
#******
#Database------
#******
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "mzsrnrwj",        #change con to elephantsql database
                 host = "horton.elephantsql.com", port = 5432,
                 user = "mzsrnrwj", password = "Nv8xD1m4lY2bYKsH4Zxw9y4dE86jFcx5")



relevant <- dbGetQuery(con, "select * from wos")
relevant2 <- dbGetQuery(con, "select * from main_qual_gis")
colnames(relevant)[1] <- "fidCitavi"
relevant <- left_join(relevant, relevant2)
relevant <- relevant %>%
  filter(Qual_Context == TRUE) %>%
  as.tibble()

#qual_gis WHERE fidCitavi = idCitavi AND Qual_Context = 1 ORDER BY idCItavi
#data
app1 <- data.frame(w =as.character(relevant$WOS), a =str_split_fixed(relevant$fidGIS_app, ";", 3))

qa <- data.frame(w = relevant$WOS,
                 year = relevant$year,
                 analysis = relevant$fidQualAnalyse)

qualApp <- left_join(qa, app1)

#apps in colum app2 & 3
app2 <- data.frame(w = qualApp$w, year = qualApp$year, analysis = qualApp$analysis, a = qualApp$a.2)
app3 <- data.frame(w = qualApp$w, year = qualApp$year, analysis = qualApp$analysis, a = qualApp$a.3)

qualApp[6] <- NULL
qualApp[5] <- NULL
colnames(qualApp)[4] <- "a"

qualApp <- rbind(qualApp, app2)
qualApp <- rbind(qualApp, app3)

qualApp <- transform(qualApp, a = as.character(a))


qualApp[qualApp == 13] <- "PGIS"
qualApp[qualApp == 14] <- "mobile GIS"
qualApp[qualApp == 15] <- "WebGIS"
qualApp[qualApp == "" ] <- NA
qualApp <- qualApp[complete.cases(qualApp$a),]
#*******
#rename ID to category, exclude freq lower 5 - qual. Analysis ----
#*******
qualApp$analysis[qualApp$analysis == 1] <- NA
qualApp$analysis[qualApp$analysis == 2] <- "Narrative Analysis"
qualApp$analysis[qualApp$analysis == 3] <- "Interpretive Analysis"
qualApp$analysis[qualApp$analysis == 4] <- "Grounded Theory"
qualApp$analysis[qualApp$analysis == 5] <- "Text/Content Analysis"
qualApp$analysis[qualApp$analysis == 6] <- "transcription Analysis"
qualApp$analysis[qualApp$analysis == 7] <- "categorisation Analysis"
qualApp$analysis[qualApp$analysis == 8] <- "thematic Analysis"
qualApp$analysis[qualApp$analysis == 11] <- "discourse Analysis"
qualApp$analysis[qualApp$analysis == 12] <- "relationship Analysis"
qualApp$analysis[qualApp$analysis == 18] <- "visual Analysis"
qualApp$analysis[qualApp$analysis == 20] <- "hot-spot Analysis"
qualApp$analysis[qualApp$analysis == 29] <- "Grounded Visualistation"
qualApp <- qualApp[complete.cases(qualApp),]

#******
#total Analyse & App-------
#******
total_app <- qualApp %>%
  group_by(a) %>%
  count(a, sort = TRUE)

total_Analyse <- qualApp %>%
  group_by(analysis) %>%
  count(analysis, sort = TRUE) %>%
  filter(n > 5)

total_Analyse_app <- qualApp %>%
  group_by(analysis, a) %>%
  count(analysis, sort= TRUE) %>%
  filter(n > 5)

total_Analyse_app <- left_join(total_Analyse_app, total_app, by = "a")
total_Analyse_app <- left_join(total_Analyse_app, total_Analyse, by = "analysis")

#*****
#graph data-----
#*****

df <-data.frame(from = total_Analyse_app$analysis,
                 to =as.character(total_Analyse_app$a),
                 weight = total_Analyse_app$n.x,
                appweight = total_Analyse_app$n.y,
                analysisweight = total_Analyse_app$n)
df <- transform(df, to =as.character(df$to))
df <- transform(df, from =as.character(df$from))



meta1 <- data.frame( name =c(df$from,df$to))
meta1 <-as.data.frame(meta1[!duplicated(meta1$name),])
meta1 <- cbind(meta1, "x" = c(4,4,4,4,4,4,4,4,1,7), "y"=c(10,20,30,40,50,60,70,80, 45,45))
meta1 <- cbind(meta1, "freq" = df$analysisweight)
meta1$freq[meta1$`meta1[!duplicated(meta1$name), ]`== "PGIS"] <- 155
meta1$freq[meta1$`meta1[!duplicated(meta1$name), ]`== "WebGIS"] <- 56
meta1 <- cbind(meta1, "col" =0)
meta1$col[meta1$y == 45] <- "steelblue"
meta1$col[meta1$x == 4] <- "indianred"

colnames(df)[3] <- "Kombinationen"

#****
#graph----
#****
library(igraph)
g <- graph.data.frame(df, directed = FALSE, vertices = meta1)
lo <- layout.norm(as.matrix(meta1[,2:3]))

ggraph(g)+
  geom_edge_link(aes(width = Kombinationen, alpha = Kombinationen), show.legend = TRUE)+
  geom_node_point(size = meta1$freq/2, col = meta1$col ) +
  #geom_node_label(aes(label = meta1$freq), size = 8) +
  geom_node_text(aes(label = meta1$`meta1[!duplicated(meta1$name), ]`), col = "black", vjust = 0, size = 4)+
  theme(legend.position = "bottom right")+
  theme_void()

