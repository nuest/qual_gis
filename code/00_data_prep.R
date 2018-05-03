# Filename: 00_data_prep.R (2018-04-20)
#
# TO DO: Data preparations which should result in cleaned datasets which will
#        form the basis for all subsequent analyses
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA EXPLORATION
# 3. SAVE PREPARED DATASETS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
library("RPostgreSQL")

# attach data
# loads the PostgreSQL driver
drv = dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con = dbConnect(drv, dbname = "mzsrnrwj",
                # change con to elephantsql database
                host = "horton.elephantsql.com", port = 5432,
                user = "mzsrnrwj",
                password = "Nv8xD1m4lY2bYKsH4Zxw9y4dE86jFcx5")
# what tables are available
dbListTables(con)
# table containing abstracts
abs_df = dbGetQuery(con, "SELECT * FROM abstract")
# table containing the manually collected information
qual = dbGetQuery(con, "select * from main_qual_gis")
# paper-specific information (author, journal, title, doi, etc.)
wos = dbGetQuery(con, "select * from wos")
# get the key tables
# list of countries (key)
cous_key = dbGetQuery(con, "select * from countries")
# qualitative data collection method (key)
qdata_key = dbGetQuery(con, "select * from qual_data")
# used GIS (key)
gis_key = dbGetQuery(con, "select * from gis_software")
# applied GIS method (key)
agis_key = dbGetQuery(con, "select * from qual_gis_transfer")
# close the connection
dbDisconnect(conn = con)

# read in times cited
cits = readLines(paste0("https://raw.githubusercontent.com/EricKrg/qual_gis/",
                        "master/data/qual_gis/wos_lit.txt"))

#**********************************************************
# 2 DATA EXPLORATION AND PREPARATION-----------------------
#**********************************************************

# 2.1 qual table===========================================
#**********************************************************
# First, remove all irrelevant publications
qual = filter(qual, Qual_Context == TRUE)

# duplicates and NAs in qual
dups = qual[duplicated(qual$fid_citavi) |
              duplicated(qual$fid_citavi, fromLast = TRUE), ]
# 0 duplicates, excellent
dups[order(dups$fid_citavi), ]
qual[is.na(qual$fid_citavi), ]  # 0, perfect
qual[duplicated(qual$doi), ]  # 0, perfect
qual[duplicated(qual$WOS), ]  # 0, perfect
# so until there is a solution, remove inconsistencies
# qual = qual[!is.na(qual$fid_citavi) & !duplicated(qual$fid_citavi), ]

# 2.2 wos table============================================
#**********************************************************
# duplicates and NAs in wos
wos[duplicated(wos$id_citavi), ]  # 0, perfect
wos[is.na(wos$id_citavi), ]  # 0 NAs
wos[duplicated(wos$doi), ]$doi  # just NAs, so ok in the case of doi
wos[duplicated(wos$WOS), ]  # 0, perfect
# just keep relevant wos records
# are all Citavi Ids available in both tables
setdiff(qual$fid_citavi, wos$id_citavi)  # 0
# setdiff(wos$idCitavi, qual$fidCitavi)
# just keep relevatn qual GIS records
wos = filter(wos, id_citavi %in% qual$fid_citavi)

# 2.3 times cited table====================================
#**********************************************************
tc = data.frame(WOS = unlist(stringr::str_extract_all(cits, "WOS:.*")))
# extract lines with times cited
tcits = grep("^TC", cits, value = TRUE)
year = grep("^PY", cits, value = TRUE)
tc$tc = as.numeric(unlist(stringr::str_extract_all(tcits, "\\d.*")))
tc$year = as.numeric(unlist(stringr::str_extract_all(year, "\\d.*")))
dim(tc)  # 490, pubs only had 475
sum(duplicated(tc$WOS))
# ok, only keep WOS which can only be found in pubs
setdiff(tc$WOS, wos$WOS)
setdiff(wos$WOS, tc$WOS)  # ok, all WOS of tc can be found in wos, perfect
# just keep relevant qual_gis records
tc = tc[tc$WOS %in% wos$WOS, ]  # 380

# 2.4 abstract table=======================================
#**********************************************************
sum(abs_df == "NA")
# replace by true NAs
abs_df[abs_df == "NA"] = NA
# there is one NA in abs_df$WOS
doi = abs_df[is.na(abs_df$WOS), "doi"]
# any other duplicates
abs_df[!is.na(abs_df$doi) &
         (duplicated(abs_df$doi) | duplicated(abs_df$doi, fromLast = TRUE)), ]
# remove the second
abs_df = abs_df[!(abs_df$doi == doi & is.na(abs_df$WOS)), ]
abs_df[duplicated(abs_df$WOS), ]  # 0, perfect
# just keep relevant abstracts
setdiff(wos$WOS, abs_df$WOS)  # 0, perfect
setdiff(abs_df$WOS, wos$WOS)

abs_df = abs_df[abs_df$WOS %in% wos$WOS, ]
dim(abs_df)  # 380

#**********************************************************
# 3 TOTAL GIS RECORDS--------------------------------------
#**********************************************************

# search terms were:
# GIS OR "geographic* information system"
# year range: 1990-2017
dir_data = "data/gis_total"
files = grep("savedrecs", dir(dir_data), value = TRUE)
# read the first file
d = data.table::fread(file.path(dir_data, files[1]))
# unfortunately, end of lines end with \t\t, which indicates a second column for
# which there is no column name
# get column names
tmp = names(warnings()[1])
cn = gsub(".*data: ", "", tmp) %>%
  strsplit(split = "\t") %>%
  unlist
files = files[-1]
# load all other files and rbind them
for (i in files) {
  tmp = data.table::fread(file.path(dir_data, i))
  d = rbind(d, tmp)
}
# delete last column
d = as.data.frame(d)
d = d[, -ncol(d)]
# add column names
names(d) = cn
# just keep year and times cited
d = select(d, PY, TC)
d = mutate(d, n = 1)
# aggregate
d = group_by(d, PY) %>%
  summarize_all(funs(sum)) %>%
  arrange(PY)
# remove records from the year 2017
gis_all = filter(d, PY != 2017)
# plot(TC / sum(TC) ~ PY, gis_all, type = "l")
# plot(n / sum(n) ~ PY, gis_all, type = "l")


#**********************************************************
# 4 SAVE OUTPUT--------------------------------------------
#**********************************************************

saveRDS(abs_df, "images/00_abs_df.rds")
saveRDS(qual, "images/00_qual.rds")
saveRDS(tc, "images/00_tc.rds")
saveRDS(wos, "images/00_wos.rds")
saveRDS(gis_all, "images/00_gis_all.rds")
save(cous_key, qdata_key, gis_key, agis_key,
     file = "images/00_keys.rda")
