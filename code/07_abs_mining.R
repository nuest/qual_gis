# Filename: 01-abs_mining.R (2017-08-30)
#
# TO DO: create word-paper matrix for ordinations
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. EXPLORATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RODBC")
library("data.table")
library("dplyr")
library("stringr")
library("tm")
library("wordcloud")
library("ggplot2")
library("vegan")
library("labdsv")
library("tidyverse")

# define directories
dir_main = "D:/uni/science/projects/computing/qual_gis/review"
dir_data = file.path(dir_main, "data")
dir_ima = file.path(dir_main, "images")
dir_figs = file.path(dir_main, "figures")

drv <- dbDriver("PostgreSQL")
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "mreolgsw",        
                 # change con to elephantsql database
                 host = "horton.elephantsql.com", port = 5432,
                 user = "mreolgsw", 
                 password = "VOvgCnJaQuFBr5dZknPbyDDO1vcUpfnW")
RPostgreSQL::dbListTables(conn = con)
# retrieve table with all the abstracts
abs_df = dbGetQuery(con, "SELECT * FROM abstract")
qual = dbGetQuery(con, "SELECT * FROM main_qual_gis")
wos = dbGetQuery(con, "SELECT * FROM wos")  
# idCitavi -> tblQualGIS
# WOS connecting tblWOS and tblAbstract
dbDisconnect(conn = con)

#**********************************************************
# 2 EXPLORATION--------------------------------------------
#**********************************************************

# there is one NA in abs_df$WOS
doi = abs_df[is.na(abs_df$WOS), "doi"]
# however, the same pub has a WOS in the wos table (ask Eric!!)
wos[wos$doi == doi & !is.na(wos$doi), ]
# retrieve wos
wos_id = wos[wos$doi == doi & !is.na(wos$doi), "WOS"]
abs_df[abs_df$WOS %in% wos_id, ]
abs_df[abs_df$doi %in% doi, ]  # ok, duplicated
# remove the second
# abs_df = abs_df[!(abs_df$doi == doi & is.na(abs_df$WOS)), ]
# any other duplicates
abs_df[duplicated(abs_df$doi) | duplicated(abs_df$WOS), "doi"]
# no just NA in dois

# now add citavi ID
abs_df = inner_join(abs_df, dplyr::select(wos, WOS, idCitavi = idCItavi),
                    by = "WOS")
abs_df[duplicated(abs_df$idCitavi), ]

# remove NAs
abs = abs_df$abstract
names(abs) = abs_df$idCitavi
abs[which(sapply(abs, nchar) < 7)]
abs[abs == "NA"] = NA
abs = abs[!is.na(abs)]

# are there any abstracts without a word
sum(unlist(lapply(abs, nchar)) == 0)  # no, perfect
# match (C) 2016
grep( "\\(C\\) \\d{4}", abs)
# check if we are not extracting something wrong
stringr::str_extract_all(abs, " \\(C\\) \\d{4}.*")
# ok, dismiss it
abs = gsub(" \\(C\\) \\d{4}.*", "",  abs)

# check DOIs
abs[grep(" DOI:", abs)]
# delete
abs = gsub(" DOI:.*", "", abs)

grep("•", abs, value = TRUE)
# replace
abs = gsub("•", " ", abs)

# find land use
grep("land use", abs)
abs = gsub("land use", "landuse", abs)
ids = names(abs)
# delete punctuation and alike
abs = str_replace_all(abs, "[:punct:]|[:digit:]|[:cntrl:]", "") %>%
  stripWhitespace()

stop_words = c(stopwords("English"), 
               c("due", "also", "however", "using", "within", "therefore", 
                 "like"))
test = lapply(abs, function(x) {
  x_2 = str_split(x, pattern = " ") %>%
    unlist %>%
    tolower
  x_2 = x_2[!x_2 %in% stop_words]
  stemDocument(x_2, language = "eng")
})

tmp <- do.call(c, test)
# can we delete these characters?
sort(table(tmp[sapply(tmp, nchar) < 3]))
# yes, I guess, we can
test = lapply(test, function(x) {
  x[nchar(x) > 3]
})


# GOT RID OF SPANISH ABSTRACTS BY MANUALLY DELETING THEM IN THE DB
# ind = sapply(test, function(x) {
#   sum(grepl("^de$", x)) > 0 && sum(grepl("^y$", x)) > 0
# })
# sum(ind)
# test[ind]
# test[c(69, 70)]
# # ok, get rid off Spanish abstracts
# abs[69]

test_2 = lapply(test, function(x) {
  table(x) %>%
    reshape::melt(., ) %>%
    reshape::cast(., ~x) %>%
    # delete the first column (value)
    dplyr::select(-1)
})

# pub-word matrix (plot-species matrix)
mat = data.table::rbindlist(test_2, fill = TRUE)
# replace NAs by 0
mat[is.na(mat)] = 0
mat = as.data.frame(mat)
rownames(mat) = ids
# order by row number and colnames
mat = mat[as.character(sort(as.numeric(rownames(mat)))), ]
mat = mat[, sort(names(mat))]
# save your result
# save(mat, file = file.path(dir_ima, "01-mat.Rdata"))
# (load(file.path(dir_ima, "01-mat.Rdata")))

#**********************************************************
# 3 ORDINATIONS--------------------------------------------
#**********************************************************

# presence-absence matrix
# mat[mat > 0] = mat[mat > 0]^0

# inapproriateness of PCA
# ord = rda(decostand(mat, "pa"))
# cumulative proportion: first three axes explain less than 5%
# head(cumsum(eigenvals(ord) / sum(eigenvals(ord))))

# downweighting of rare species
ord = vegan::decorana(decostand(mat, "pa"), iweigh = 1)

# last line corresponds to axis lengths
ord              
# retrieving axis lengths manually
# length first axis (2.23)
# sum(abs(range(scores(ord, "sites")[, 1])))                     
# length second axis (2.2)
# sum(abs(range(scores(ord, "sites")[, 2])))                     
# proportion of variance
ord$evals / sum(ord$evals)                                      
# cumulative proportion
cumsum(ord$evals / sum(ord$evals))                             
# proportion of variance as used in other software (PC-Ord), see ?decorana
# ord$evals.decorana / sum(ord$evals.decorana)                    
#cumulative proportion as used in other software (PC-Ord)
# cumsum(ord$evals.decorana / sum(ord$evals.decorana))            


# ward's clustering also outputs meaningful results with Bray-Curtis distance
# again using a frequency matrix (not p/a)
# dis = vegdist(decostand(mat, "pa"), "bray")
# clus = cluster::agnes(dis, method = "ward")
# plot(1:20, sort(clus$height, decreasing = TRUE)[1:20])

# # Ward's clustering (euclidean distance)
# clus = cluster::agnes(decostand(mat, "pa"), method = "ward")
# # hclust(, method = "ward.D2") is doing exactly the same, see ?hclust
# plot(1:20, sort(clus$height, decreasing = TRUE)[1:20])
# classes = stats::cutree(clus, k = 4)
# table(classes)
# # computing indicator values
# ind = labdsv::indval(mat, classes, numitr = 1000)

# set.seed(14)
# kmeans clustering
classes = kmeans(vegdist(mat, "bray"), 4)
# classes = pam(vegdist(mat, "bray"), 4)
# computing the indicator values
ind = labdsv::indval(mat, classes$cluster, numitr = 1000)

out = data.frame(
  # the indicator values for each class for each word
  ind$indval,
  # significance value for a word
  "pval" = ind$pval, 
  "words" = names(ind$pval),
  # assign the most likeliest class, i.e. the class with the
  # highest indicator value
  class = ind$maxcls,
  # scores
  "scores_1" = vegan::scores(ord, display = "species")[, 1],
  "scores_2" = vegan::scores(ord, display = "species")[, 2],
  "scores_3" = vegan::scores(ord, display = "species")[, 3])

# just keep the most important indicator value (this is also the one which
# decided to which class the word belongs)
out$ind_val = apply(out[, c("X1", "X2", "X3", "X4")], 1, max)
# just keep pvals < 0.005
out = filter(out, pval <= 0.05) %>%
  dplyr::select(-one_of(c("X1", "X2", "X3", "X4")))

# group by class
out_2 = group_by(out, class) %>%
  # order the indicator value descendingly and the p-value ascendingly
  arrange(desc(ind_val), pval) %>%
  # use the n most significant words of each group
  slice(1:15) %>%
  as.data.frame

# plot(x = out_2$DCA1, y = out_2$DCA2, type = "n")
# text(x = out_2$DCA1, y = out_2$DCA2, labels = out_2$words, col = out_2$class)
pal = RColorBrewer::brewer.pal("Set3", n =  6)[3:6]
p_1 = ggplot(out_2) + 
  ggrepel::geom_label_repel(aes(out_2$scores_1, 
                                out_2$scores_2, 
                                fill = factor(out_2$class), 
                                label = out_2$words,
                                colour = pal),
                            segment.color = NA,
                            fontface = "bold", color = "black", size = 3,
                            box.padding = unit(0.15, "lines")) +
  labs(x = "scores 1st axis", y = "scores 2nd axis") + 
  guides(fill = ggplot2::guide_legend(title = NULL)) + 
  theme_classic(base_size = 12) + 
  scale_fill_manual(values = pal)
ggsave(file.path(dir_figs, "dca.png"), p_1, dpi = 300, width = 15, height = 15, 
       units = "cm")
# to be able to reproduce the exact same plot
# save(out, out_2, ind, classes, file = file.path(dir_ima, "01-classes.Rdata"))


#**********************************************************
# WORD-COMBINATION ORDINATION------------------------------
#**********************************************************

# "ecosystem services", "land use", "children GIS", "geographic information",
# etc. yeah, would be interesting but maybe is too much for this paper... also 
# interesting to discuss other clustering (kmean, pam, etc) and ordination 
# techniques (NMDS, Isomap)
x <- tmp$Abstract[1]
text_1 <- str_split(x, pattern = " ") %>%
  unlist %>%
  gsub("\\.|;|:|,", "", .)
table(text_1) %>% 
  sort
text_2 <- paste(text_1, text_1[-1])
# the last one is due to recycling, so remove it
text_2 <- text_2[-length(text_2)]
table(text_2) %>% 
  sort


#**********************************************************
# WORDCLOUD------------------------------------------------
#**********************************************************

test <- do.call(c, test)
tab <- table(test) %>%
  sort %>% 
  rev
# just consider the most important words
test <- test[test %in% names(tab[tab > 50])]
# sample
test <- test[sample(1:length(test), 15000)]
# plot a wordcloud
wordcloud(test, scale = c(5, 0.5), max.words = 100, random.order = FALSE,
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))

#**********************************************************
# OPEN-SOURCE GIS USAGE------------------------------------
#**********************************************************

con = odbcConnectAccess2007(file.path(dir_data, "Database_WOS.accdb"))
qual = sqlFetch(con, "tblQUAL_GIS", as.is = TRUE)
cous = sqlFetch(con, "tblCountries", as.is = TRUE)
gis = sqlFetch(con, "tblGIS_Software", as.is = TRUE)
close(con)

# we could use the research field for posthoc plotting onto the ordination
sort(table(qual$Research_field))
sort(table(gsub(",.*", "", qual$Research_field)))
qual$Research_field %>%
  tolower %>%
  gsub("-", " ", .) %>%
  gsub("(,|;).*", "", .) %>%
  gsub(" .*", "", .) %>%
  stemDocument(., language = "eng") %>%
  gsub("ubran", "urban", .) %>%
  gsub("crictic|circitc", "critic", .) %>%
  gsub("decison", "decis", .) %>%
  gsub("foresteri|forestri", "forest", .) %>%
  table %>%
  sort
# public health and publie transportation
grep("public", tolower(names(table(gsub(",.*", "", qual$Research_field)))), 
     value = TRUE)

os_soft = filter(gis, `Open-source` == 1) %>%
  distinct(GIS_Software) %>%
  pull

names(gis) = gsub("-", "_", names(gis))
qual$fidCountries = as.numeric(gsub(";.*", "", qual$fidCountries))

tab = inner_join(qual, dplyr::select(gis, idGIS, Open_source),
           by = c("fidGIS" = "idGIS")) %>%
  filter(Open_source == 1) %>%
  inner_join(., cous, by = c("fidCountries" = "idCountries")) %>%
  dplyr::select(Country) %>%
  table %>%
  sort(decreasing = TRUE) %>%
  as.data.frame
names(tab) = c("Country", "Frequency")
library("R2wd")
wdGet()
R2wd::wdTable(format(tab), 
              caption = "Countries using open-source GIS.")


#**********************************************************
# TESTIN ISOMAP, NMDS, PAM, KMEANS-------------------------
#**********************************************************

# ISOMAP
source(file.path("D:/uni/science/projects/ecology/asia/Mongolia/", 
                 "TINN-R/functions/bestisomap.r"))
#Isomap
pa = decostand(mat, "pa")
# tmp = as.data.frame(pa)
bestiso = bestisomap(vegdist(pa, "bray"), k = 2) 
# explained variance first two axes = 22.91 % (k = 406)
bestiso = bestisomap(vegdist(mat, "bray"), k = 2) 
# explained variance first two axes = 22.1 % (k = 342)

# NMDS

# default values (not the best solution for heterogeneous datasets)
mds = metaMDS(pa)
# no convergence
mds = metaMDS(pa, noshare = FALSE, autotransform = FALSE, try = 50,
              weakties = TRUE)
# no convergence, stress 0.33 (really bad...)
# using mat, stress 0.33...


# PAM clustering===========================================
#**********************************************************
library("cluster")
# Bray-Dissimilarity
x = vegdist(pa, "bray")
asw = numeric(5)
## Note that "k=1" won?t work!
for (k in 2:5) {
  asw[k] = pam(pa, k)$silinfo$avg.width
}
k.best = which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
plot(1:5, asw, type = "h", main = "pam() clustering assessment",
     xlab = "k (# clusters)", ylab = "average silhouette width")
axis(1, k.best, paste("best", k.best, sep = "\n"), col = "red", 
     col.axis = "red")

# KMEANS===================================================
#**********************************************************
kmeans.fun <- function (data, maxclusts = 15) {
  t <- kmeans(data, 1, nstart = 20)$totss
  w <- lapply(as.list(2:maxclusts), function(nc)
    kmeans(data,nc)$tot.withinss)
  
  plot(1:maxclusts, c(t,w), type = "b",
       xlab = "Number of Clusters",
       ylab = "Within groups sum of squares",
       main = paste(deparse(substitute(data))))
}

# kmeans.fun(pa)
kmeans.fun(vegdist(pa, "bray"))  # ok, looks like 4-6 clusters
kmeans.fun(mat)  # > 10
kmeans.fun(pa)  # > 10
kmeans.fun(vegdist(mat, "bray"))  # 4-6 classes
classes = kmeans(vegdist(mat, "bray"), 4)
ind = labdsv::indval(mat, classes$cluster, numitr = 1000)
out = data.frame(
  # the indicator values for each class for each word
  round(ind$indval, d = 2), 
  # significance value for a word
  "pval" = ind$pval, 
  "words" = names(ind$pval),
  # assign the most likeliest class, i.e. the class with the
  # highest indicator value
  class = ind$maxcls,
  # DCA scores
  vegan::scores(ord, display = c("species")))

#**********************************************************
# TIDY WORDS-----------------------------------------------
#**********************************************************

library("tidyr")
# compute tf idf and dismiss all unimportant words (tf_idf < 0.003)
bind_tf_idf(word, country, n)
