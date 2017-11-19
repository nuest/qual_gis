# Citation Plot

# Internet connection required for connection to the remote DB


#******
#packages---------
#******
pacman::p_load(ggplot2, ggthemes, tidyverse,
               stringr, plotly, ggraph, igraph,
               RPostgreSQL)
#******
#Database & Data construction------
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
  filter(Qual_Context == TRUE)

####WOS data to times of citation

dir <- "C:/Users/Eric/Documents/Gdrive/Studium/qual_gis/WOS_Literatur/"
pathlit <- paste0(dir, "wos_lit.txt")


wos_lit <- readLines(pathlit)


df_tc <- tibble(WOS = grep("^UT.*", wos_lit, value = T))
df_tc$WOS <- substr(df_tc$WOS, 4, nchar(df_tc$WOS))
df_tc$tc <- grep("^TC.*", wos_lit, value = T)
df_tc$tc <- substr(df_tc$tc, 3, nchar(df_tc$tc))

as.tibble(df_tc)

#df_tc <- transform(df_tc, tc = as.numeric(tc))

df_all <- left_join(relevant, df_tc)
##joing db-data and wos data
df
