# Filename: table.R (2018-05-03)
#
# TO DO: papers per country and per capita and compared to total GIS output
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
library("stringr")
library("forcats")
library("magrittr")

# attach data
wos = readRDS("images/00_wos.rds")
qual = readRDS("images/00_qual.rds")
gis_all = readRDS("images/00_gis_all.rds")
# attach keys
(load("images/00_keys.rda"))
# attach population data
data(pop, package = "wpp2015")

# attach GIS studies per country
gpc = data.table::fread("data/gis_total/gis_per_country.txt")
# just keep the first two columns
gpc = gpc[, 1:2]
# rename
names(gpc) = c("country", "n")
gpc = as.data.frame(gpc)

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

d = left_join(wos, qual, by = c("id_citavi" = "fid_citavi"))
d = select(d, id_citavi, fidCountries)
# several countries have been identified per manuscript, one for each author,
# here, we just make use of the first author's country
# first find out the maximum number of countries of a paper
my_max = stringr::str_count(d$fidCountries, ";") %>%
  max(na.rm = TRUE)
# split accordingly, and just keep the first country id
cou_key = str_split_fixed(d$fidCountries, ";", my_max + 1)[, 1] %>%
  as.integer
d$fidCountries = cou_key
d = left_join(select(d, fidCountries),
              select(cous_key, idCountries, Country),
              by = c("fidCountries" = "idCountries"))
# aggregate, i.e. sum up the number of qualitative GIS papers per country
d = group_by(d, Country) %>%
  dplyr::summarize(n = n()) %>%
  arrange(desc(n))

# 2.1 Harmonize country names==============================
#**********************************************************

# harmonize country names of d and pop
setdiff(d$Country, pop$name)
grep(pop$name, pattern = "Korea", value = TRUE)
pop$name = gsub("Republic of Korea", "South Korea", pop$name)
grep(pop$name, pattern = "Tanzania", value = TRUE)
grep(pop$name, pattern = "Iran", value = TRUE)
grep(pop$name, pattern = "China", value = TRUE)
grep(pop$name, pattern = "Venezuela", value = TRUE)
grep(pop$name, pattern = "United", value = TRUE)
pop$name = gsub(" of America", "", pop$name)
pop$name = gsub(" \\(.*", "", pop$name)
pop$name = gsub(".*Republic of ", "", pop$name)
d$Country %<>%
  fct_recode(Finland = "Finnland",
             Belgium = "Belguim",
             China = "Republic of China",
             "United States" = "USA")
setdiff(d$Country, pop$name)  # 0, perfect

# harmonize country names of d and gcp
# more or less copied from ?chartr
simple_cap = function(x) {
  s = strsplit(x, " ")
  sapply(s, function(x) {
    paste(toupper(substring(x, 1, 1)), substring(x, 2),
          sep = "", collapse = " ")
  })
}

# make sure that countries start with a capital letter
gpc$country %<>%
  tolower %>%
  simple_cap
# find out about the differences
setdiff(d$Country, gpc$country)
# harmonize the differences
grep("usa", gpc$country, value = TRUE)
gpc$country = gsub("Usa", "United States", gpc$country)
gpc$country = gsub(".*China", "China", gpc$country)
grep("England|Scotland|Wales|Ireland", gpc$country, value = TRUE)
gpc$country %<>%
  fct_collapse("United Kingdom" =
                 c("England", "Scotland", "Wales", "North Ireland"))
# aggregate since UK now consists of four rows
gpc = group_by(gpc, country) %>%
  dplyr::summarize(n = sum(n)) %>%
  dplyr::rename(n_gis = n)
setdiff(d$Country, gpc$country)  # 0, perfect

#**********************************************************
# 3 CONSTRUCT OUTPUT TABLE---------------------------------
#**********************************************************

# join d (qualitative GIS), population and gcp (all GIS manuscripts per country)
d = inner_join(d, dplyr::select(pop, name, pop = "2015"),
               by = c("Country" = "name"))
d = inner_join(d, gpc, by = c("Country" = "country"))

mutate(d,
       por = n / n_gis * 100,
       per_capita = n / (pop / 1000)) %>%
  filter(n > 3) %>%
  arrange(desc(por))
