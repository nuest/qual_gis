# Filename: 01_cits_freq.R (2017-11-21)
#
# TO DO: Figure of publication frequency and times cited
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. 
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("lattice")
library("grid")
library("gridExtra")
library("data.table")
library("dplyr")
library("RPostgreSQL")

# define directories
dir_main = "D:/uni/science/projects/computing/qual_gis/review"
dir_fig = file.path(dir_main, "figs")
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

# save(cits, wos, qual, file = file.path(dir_ima, "01_input.Rdata"))
load(file.path(dir_ima, "01_input.Rdata"))

#**********************************************************
# 2 DATA EXPLORATION---------------------------------------
#**********************************************************

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

# duplicates and NAs in wos
wos = rename(wos, idCitavi = idCItavi)
wos[duplicated(wos$idCitavi), ]  # 0, perfect
wos[is.na(wos$idCitavi), ]  # 0 NAs
wos[duplicated(wos$doi), ]$doi  # just NAs, so ok in the case of doi
wos[duplicated(wos$WOS), ]  # 0, perfect

# are all Citavi Ids available in both tables
setdiff(qual$fidCitavi, wos$idCitavi)  # NA in qual$fidCitavi
setdiff(wos$idCitavi, qual$fidCitavi)  
# 12 idCitavis in wos which cannot be found in qual

#**********************************************************
# 3 VISUALIZATION------------------------------------------
#**********************************************************

# construct publication frequency table per year
# left_join to keep all wos records
pubs = left_join(wos, select(qual, fidCitavi, Qual_Context),
                 by = c("idCitavi" = "fidCitavi"))
dim(pubs)  # 475
# check
pubs[duplicated(pubs$idCitavi), ]
pubs[duplicated(pubs$WOS), ]
pubs[duplicated(pubs$doi), "doi"]
# Qual_Context == TRUE ??? -----------> ask Erik if needed
# qual = qual %>%
#   filter(Qual_Context == TRUE)

# construct number of publication-per-year table
pubs_agg = group_by(pubs, year) %>%
  summarize(pubs = sum(length(year))) %>%
  mutate(pub_norm = pubs / sum(pubs))

# construct citation-per-year table
# extract lines with wos id
tc = data.frame(WOS = unlist(stringr::str_extract_all(cits, "WOS:.*")))
# extract lines with times cited
tcits = grep("^TC", cits, value = TRUE) 
year = grep("^PY", cits, value = TRUE) 
tc$tc = as.numeric(unlist(stringr::str_extract_all(tcits, "\\d.*")))
tc$year = as.numeric(unlist(stringr::str_extract_all(year, "\\d.*")))
dim(tc)  # 490, pubs only had 475 --------> why?, ask Erik
sum(duplicated(tc$WOS))
# ok, only keep WOS which can only be found in pubs
setdiff(tc$WOS, pubs$WOS)
setdiff(pubs$WOS, tc$WOS)
tc = tc[tc$WOS %in% pubs$WOS, ]
# aggregate
tc_agg = group_by(tc, year) %>%
  summarize(tc = sum(tc)) %>%
  mutate(tc_norm = tc / sum(tc))

pub_cits = inner_join(pubs_agg, tc_agg, by = "year") %>%
  filter(year < 2017) %>%
  select(year, pubs, tc)
pub_cits = melt(pub_cits, id.var = "year")

xyplot(value ~ year | variable, data = pub_cits, type = "l",
       col = c("black", grey(0.5)), ylab = "Number", xlab = c(), 
       aspect = "xy", layout = c(2, 1),
       between = list(x = 0.5),
       scales = list(
         y = list(relation = "free"),
         tck = c(1, 0), 
         alternating = c(1, 1)),
       strip = strip.custom(
         bg = "white",
         factor.levels = c("Publications per year", "Citations per year"))
       # Create your own legend
       #  key = list(space = "right", text = list(c("all", "grad"), cex = 0.8),
       #             lines = list(col = c("black", grey(0.5)), lwd = 2,  cex = 0.6))
)

# grid.text("a)", x = unit(0.125, "npc"), y = unit(0.765, "npc"), 
#           gp = gpar(font = "bold", fontsize = 10))
# grid.text("b)", x = unit(0.49, "npc"), y = unit(0.765, "npc"),
#           gp = gpar(font = "bold", fontsize = 10))

