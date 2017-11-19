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


# joining db data with wos data
df_all <- left_join(relevant, df_tc)

#data perp. for plot

df_plot <- df_all %>%
  select(year,tc)

df_plot <- df_plot[order(df_plot$year,df_plot$tc),]

years <- unique(df_plot$year)

year_list <- list()
j <- 1

for (i in years){
  print(i)
  temp <- subset(df_plot, df_plot$year == i)
  sum_tc <- sum(as.numeric(temp$tc))
  year_list[[j]] <- assign(paste0("year",i),tibble(y = i, sum = sum_tc))
  j = j+1
}

final <- rbind_list(year_list)
final <- cbind(final, cumsum = cumsum(final$sum))


#plotting

p1 <- final %>%
  filter(y < 2017) %>%
  ggplot(aes(y, cumsum))+ geom_bar(stat = "identity", aes(fill = sum), width = 0.8) + scale_fill_gradient(high="cyan", low="red")+
  ylab("number of Citations") + xlab("")+
  scale_x_continuous(breaks =seq(1994,2016,2)) + scale_y_continuous(breaks = seq(0,6000, 1000))+
  theme(plot.title = element_text(lineheight=.3, face="bold"))+
  guides(fill=guide_legend(title= "Citations per year")) + theme(legend.position = "bottom")
p1




