# Filename: 00_data_prep.R (2017-11-22)
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

# define directories
dir_main = "."
dir_fig = file.path(dir_main, "figures")
dir_data = file.path(dir_main, "data")
dir_ima = file.path(dir_main, "images")

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
abs_df = dbGetQuery(con, "SELECT * FROM abstract")
qual = dbGetQuery(con, "select * from main_qual_gis")
wos = dbGetQuery(con, "select * from wos")
dbDisconnect(conn = con)

# read in times cited
cits = readLines(paste0("https://raw.githubusercontent.com/EricKrg/qual_gis/",
                        "master/data/wos_lit.txt"))

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
# 4 duplicates -> no good -> ask Erik
dups[order(dups$fid_citavi), ]
qual[is.na(qual$fid_citavi), ]  # there is one NA -> ask Eric
qual[duplicated(qual$doi), ]  # 0, perfect
qual[duplicated(qual$WOS), ]  # 0, perfect
# so until there is a solution, remove inconsistencies
qual = qual[!is.na(qual$fid_citavi) & !duplicated(qual$fid_citavi), ]

# 2.2 wos table============================================
#**********************************************************
# duplicates and NAs in wos
wos[duplicated(wos$id_citavi), ]  # 0, perfect
wos[is.na(wos$id_citavi), ]  # 0 NAs
wos[duplicated(wos$doi), ]$doi  # just NAs, so ok in the case of doi
wos[duplicated(wos$WOS), ]  # 0, perfect
# just keep relevant wos records
# are all Citavi Ids available in both tables
setdiff(qual$fid_citavi, wos$id_citavi)  # 478
# setdiff(wos$idCitavi, qual$fidCitavi)
wos = filter(wos, id_citavi %in% qual$fid_citavi)

# 2.3 times cited table====================================
#**********************************************************
tc = data.frame(WOS = unlist(stringr::str_extract_all(cits, "WOS:.*")))
# extract lines with times cited
tcits = grep("^TC", cits, value = TRUE)
year = grep("^PY", cits, value = TRUE)
tc$tc = as.numeric(unlist(stringr::str_extract_all(tcits, "\\d.*")))
tc$year = as.numeric(unlist(stringr::str_extract_all(year, "\\d.*")))
dim(tc)  # 490, pubs only had 475 --------> why?, ask Erik
sum(duplicated(tc$WOS))
# ok, only keep WOS which can only be found in pubs
setdiff(tc$WOS, wos$WOS)
setdiff(wos$WOS, tc$WOS)  # ok, all WOS of tc can be found in wos, perfect
tc = tc[tc$WOS %in% wos$WOS, ]

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
dim(abs_df)  # 379

#**********************************************************
# 3 SAVE OUTPUT--------------------------------------------
#**********************************************************

save(abs_df, qual, tc, wos, file = file.path(dir_ima, "01_input.Rdata"))
# load(file.path(dir_ima, "01_input.Rdata"))
