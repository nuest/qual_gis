
#### GIS-Transfer- GIS - Software - Qual.-Data
#### script for a three sited net.plot

# Internet connection required for connection to the remote DB


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




####----
#data
transfer <- data.frame(w =as.character(relevant$WOS),
                       t =relevant$fidQualGIS_transfer)

soft <- data.frame(w = relevant$WOS,
                 year = relevant$year,
                 GIS = relevant$fidGIS)

qdata <- data.frame(w = relevant$WOS,
                    qdata = str_split_fixed(relevant$fidQualData, ";", 13))

trans_soft <- left_join(soft, transfer)
###************************
###qdata colums------
###************************

# extracting data columns and binding them into one df

iterations = 13 #

i = 1
j = 2
for (i in 1:iterations) {

  assign(paste0("data_",i), #assigning new dfs
         data.frame(w = qdata$w,q = qdata[j]))


  j = j +1
 print("this iteration was:")
 print(i)
 print("next column will be:")
 print(j)
}


library(data.table)
x.n <- c(paste0("data_", 1:13))  #listing dfs
x.list <- (lapply(x.n, get))    # getting dfs values
dt_data_all <- rbindlist(x.list) # binding them in one df

colnames(dt_data_all)[2] <- "qdata"

dt_data_all$qdata[dt_data_all$qdata == "" ] <- NA
dt_data_all <- dt_data_all[complete.cases(dt_data_all$qdata),]




###*************
###renaming----
###*************

# renaming trans_soft

trans_soft$GIS[trans_soft$GIS == 1] <- "No GIS"
trans_soft$GIS[trans_soft$GIS == 4] <- "ArcGIS"

trans_soft$GIS[trans_soft$GIS == 2] <- "free GIS"
trans_soft$GIS[trans_soft$GIS == 3] <- "free GIS"
trans_soft$GIS[trans_soft$GIS == 6] <- "free GIS"
trans_soft$GIS[trans_soft$GIS < 9] <- "free GIS"



trans_soft$t[trans_soft$t == 1 ] <-  NA
trans_soft$t[trans_soft$t == 3 ] <- "Geocoding"
trans_soft$t[trans_soft$t == 4 ] <- "Hyperlinks"
trans_soft$t[trans_soft$t == 5 ] <- "GIS extension"
trans_soft$t[trans_soft$t == 7 ] <- "Modelling spatial reasoning"

trans_soft <- trans_soft[complete.cases(trans_soft$t),]
# renaming dt_data_all

dt_data_all <- transform( dt_data_all, qdata = as.character(qdata))

dt_data_all$qdata[dt_data_all$qdata == 22 ] <- "Interview"
dt_data_all$qdata[dt_data_all$qdata == 23 ] <- "Recording"
dt_data_all$qdata[dt_data_all$qdata == 24 ] <- "Photography"
dt_data_all$qdata[dt_data_all$qdata == 25 ] <- "Survey"
dt_data_all$qdata[dt_data_all$qdata == 26 ] <- "Observation"
dt_data_all$qdata[dt_data_all$qdata == 27 ] <- "Focus Group"
dt_data_all$qdata[dt_data_all$qdata == 28 ] <- "Story"
dt_data_all$qdata[dt_data_all$qdata == 29 ] <- "Narration"
dt_data_all$qdata[dt_data_all$qdata == 30 ] <- "Questionnaire"
dt_data_all$qdata[dt_data_all$qdata == 31 ] <- "Diary"
dt_data_all$qdata[dt_data_all$qdata == 32 ] <- "Field notes"
dt_data_all$qdata[dt_data_all$qdata == 33 ] <- "mapping \n Workshop"
dt_data_all$qdata[dt_data_all$qdata == 34 ] <- "Sketch"
dt_data_all$qdata[dt_data_all$qdata == 36 ] <- "Q-Survey"
dt_data_all$qdata[dt_data_all$qdata == 37 ] <- NA

dt_data_all <- dt_data_all[complete.cases(dt_data_all$qdata),]

#******

#total Analyse & App-------
#******

kombi <- left_join(trans_soft, dt_data_all)


# single totals
total_soft <- trans_soft %>%
  group_by(GIS) %>%
  count(GIS, sort = TRUE)
colnames(total_soft)[1] <- "name"
total_soft <- as.data.frame(total_soft)


total_trans <- trans_soft %>%
  group_by(t) %>%
  count(t, sort = TRUE)
colnames(total_trans)[1] <- "name"
total_trans <- as.data.frame(total_trans)

total_qdata <- dt_data_all %>%
  group_by(qdata) %>%
  count(qdata, sort = TRUE) %>%
  filter(n > 35)
colnames(total_qdata)[1] <- "name"
total_qdata<- as.data.frame(total_qdata)

#kombi totals

total_soft_trans <- kombi %>%
  group_by(GIS, t ) %>%
  count(GIS, sort= TRUE)
total_soft_trans <- as.data.frame(total_soft_trans)
colnames(total_soft_trans)[1] <- "from"
colnames(total_soft_trans)[2] <- "to"


total_trans_qdata <- kombi %>%
  group_by(t, qdata) %>%
  count(qdata, sort = TRUE) %>%
  filter(qdata != "Sketch",
         qdata != "Story",
         qdata != "Recording",
         qdata != "Diary",
         qdata != "Q-Survey")

total_trans_qdata <- as.data.frame(total_trans_qdata)
colnames(total_trans_qdata)[1] <- "from"
colnames(total_trans_qdata)[2] <- "to"


#*****
#graph data-----
#*****

df_tri <- rbind(total_soft_trans, total_trans_qdata)
df_tri <- df_tri[complete.cases(df_tri$to),]

meta <- bind_rows(total_qdata, total_trans, total_soft)
levels(meta$name) <- gsub(" ", "\n", levels(meta$name))


meta <- cbind(meta, "x" = 0)
meta[1:9,]$x[meta[1:9,]$x == 0] <- 1
meta[10:13,]$x[meta[10:13,]$x == 0] <- 2
meta[14:16,]$x[meta[14:16,]$x == 0] <- 3

meta <- cbind(meta, "y" = 0)
meta[1:9,]$y[meta[1:9,]$y == 0] <- rev(seq(10,150, by = 15))
meta[10:13,]$y[meta[10:13,]$y == 0] <- rev(seq(30,100, by = 23))
meta[14:16,]$y[meta[14:16,]$y == 0] <- rev(seq(30,130, by = 30)) # ignore error

meta <- cbind(meta, "col" = 0)
meta[1:9,]$col[meta[1:9,]$col == 0] <- "grey"
meta[10:13,]$col[meta[10:13,]$col == 0] <- "steelblue"
meta[14:16,]$col[meta[14:16,]$col == 0] <- "indianred"



colnames(df_tri)[3] <- "Combinations"

#****
#graph----
#****
library(igraph)
g <- graph.data.frame(df_tri, directed = FALSE, vertices = meta)
lo <- layout.norm(as.matrix(meta[,2:3]))


ggraph(g)+
  geom_edge_link(aes(width = Combinations, alpha = Combinations), show.legend = TRUE)+
  geom_node_point(size = meta$n/5.5, col =meta$col) +
  geom_node_text(aes(label = meta$name), col = "black", vjust = 0,hjust = 0,size = 4.5, parse = FALSE)+
  geom_node_label(aes(label = meta$n), size = 4, parse = TRUE, vjust = 1.2) +
  theme(legend.position = "bottom")+
  theme_void()
