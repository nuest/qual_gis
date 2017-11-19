
pacman::p_load(tidyverse, rpostgres,zip)

setwd("C:/Users/Eric/Documents/Gdrive/Studium/qual_gis/WOS_Literatur/Endnote_to_Citavi/Citavi Attachments/")
path <- "C:/Users/Eric/Documents/Gdrive/Studium/qual_gis/WOS_Literatur/Endnote_to_Citavi/Citavi Attachments/"
##Database new------
drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "qual_gis",        #change con to elephantsql database
                 host = "localhost", port = 5432,
                 user = "postgres", password = "user")


qry <- {"select * from qual_gis_main inner join wos on fidcitavi = idcitavi where note like '%review%'"
}
relevant <- dbGetQuery(con, qry)

file_list <- list()

pb <- txtProgressBar(min = 0, max = total, style = 3)
for (i in 1:NROW(relevant)){
  pattern <-  paste0("^",substr(relevant$author[i],1,5),".*"," ",relevant$year[i]," ","-", ".*.pdf")
  temp <- list.files(".", pattern = pattern, recursive = TRUE )
  temp <- as.character(paste0(path, temp))
  if (temp != path){
    zip_append("reviews.zip",temp)
  }
  setTxtProgressBar(pb, i)
}
close(pb)

