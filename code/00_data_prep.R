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
con = dbConnect(drv, dbname = "mreolgsw",    
                # change con to elephantsql database
                host = "horton.elephantsql.com", port = 5432,
                user = "mreolgsw", 
                password = "VOvgCnJaQuFBr5dZknPbyDDO1vcUpfnW")
wos = dbGetQuery(con, "select * from wos")
qual = dbGetQuery(con, "select * from main_qual_gis")
dbDisconnect(conn = con)

# read in times cited
cits = readLines(paste0("https://raw.githubusercontent.com/EricKrg/qual_gis/",
                        "master/data/wos_lit.txt"))

#**********************************************************
# 2 DATA EXPLORATION AND PREPARATION-----------------------
#**********************************************************

# 2.1 qual table===========================================
#==========================================================
# First, remove all irrelevant publications
qual = filter(qual, Qual_Context == TRUE)

# duplicates and NAs in qual
dups = qual[duplicated(qual$fidCitavi) | 
              duplicated(qual$fidCitavi, fromLast = TRUE), ]
# 4 duplicates -> no good -> ask Erik
dups[order(dups$fidCitavi), ]
qual[is.na(qual$fidCitavi), ]  # there is one NA -> ask Erik
qual[duplicated(qual$doi), ]  # 0, perfect
qual[duplicated(qual$WOS), ]  # 0, perfect
# so until there is a solution, remove inconsistencies
qual = qual[!is.na(qual$fidCitavi) & !duplicated(qual$fidCitavi), ]

# 2.2 wos table============================================
#**********************************************************
# duplicates and NAs in wos
wos = rename(wos, idCitavi = idCItavi)
wos[duplicated(wos$idCitavi), ]  # 0, perfect
wos[is.na(wos$idCitavi), ]  # 0 NAs
wos[duplicated(wos$doi), ]$doi  # just NAs, so ok in the case of doi
wos[duplicated(wos$WOS), ]  # 0, perfect
# just keep relevant wos records
# are all Citavi Ids available in both tables
setdiff(qual$fidCitavi, wos$idCitavi)  # NA in qual$fidCitavi
# setdiff(wos$idCitavi, qual$fidCitavi)  
wos = filter(wos, idCitavi %in% qual$fidCitavi)

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

#**********************************************************
# 3 SAVE OUTPUT--------------------------------------------
#**********************************************************

save(qual, wos, cits, file = file.path(dir_ima, "01_input.Rdata"))
# load(file.path(dir_ima, "01_input.Rdata"))