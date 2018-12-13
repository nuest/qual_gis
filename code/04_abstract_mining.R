# Filename: 04_abstract_mining.R (2017-08-30)
#
# TO DO: create word-paper matrix for ordinations and clustering, i.e. apply
#        a text mining approach to the words of the abstract
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. EXPLORATION
# 3. ORDINATION & CLUSTERING
# 4. CLUSTER CENTROIDS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("data.table")
library("dplyr")
library("stringr")
library("tm")
library("wordcloud")
library("ggplot2")
library("vegan")
library("labdsv")
library("tidyverse")
library("magrittr")

# attach data
# abstracts
abs_df = readRDS("images/00_abs_df.rds")
# wos id
wos = readRDS("images/00_wos.rds")
# times cited
tc = readRDS("images/00_tc.rds")
# transformation, used gis, qualitative data 
trans_soft_qd = readRDS("images/03_trans_soft_qd.rds")

#**********************************************************
# 2 EXPLORATION--------------------------------------------
#**********************************************************

# add citavi ID
abs_df = inner_join(abs_df, dplyr::select(wos, WOS, id_citavi),
                    by = "WOS")
abs_df[duplicated(abs_df$id_citavi), ]  # 0, perfect

# remove NAs
abs = abs_df$abstract
# give abs the Citavi ID
names(abs) = abs_df$id_citavi
abs[which(sapply(abs, nchar) < 7)]
abs[abs == "NA"] = NA
sum(is.na(abs))  # 13 publications without abstract
# have a look at them
ids = as.numeric(names(abs[is.na(abs)]))
abs_df[abs_df$idCitavi %in% ids, ]
# delete them
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

# grep("•", abs, value = TRUE)
# # replace
# abs = gsub("•", " ", abs)

# find land use
grep("land use", abs)
abs = gsub("land use", "landuse", abs)
ids = names(abs)
# delete punctuation and alike
abs = str_replace_all(abs, "[:punct:]|[:digit:]|[:cntrl:]", "") %>%
  stripWhitespace()

# stop_words = c(stopwords("English"),
#                c("due", "also", "however", "using", "within", "therefore",
#                  "like"))
stop_words = c(stopwords("SMART"),
               c("due", "also", "however", "using", "within", "therefore",
                 "like"))
# abs have lost their names, give them back!!
names(abs) = ids
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
# check names
# names(test_2)  # ok, still Citavi ID

# pub-word matrix (plot-species matrix)
mat = data.table::rbindlist(test_2, fill = TRUE)
# replace NAs by 0
mat[is.na(mat)] = 0
mat = as.data.frame(mat)
rownames(mat) = ids  # is the same names(abs), names(test), names(test_2)
# identical(ids, names(abs))  # is the same as names(abs)
# identical(names(abs), names(test_2))
# identical(names(abs), names(tes))
# order by colnames
mat = mat[, sort(names(mat))]
# save your result and remember that rownames(mat) correspond to abs_df$idCitavi
# saveRDS(mat, file = "images/04_mat.rds")

#**********************************************************
# 3 ORDINATION & CLUSTERING--------------------------------
#**********************************************************

# load the input data
mat = readRDS("images/04_mat.rds")
# presence-absence matrix
# mat[mat > 0] = mat[mat > 0]^0

# inapproriateness of PCA
# ord = rda(decostand(mat, "pa"))
# cumulative proportion: first three axes explain less than 5%
# head(cumsum(eigenvals(ord) / sum(eigenvals(ord))))

# downweighting of rare species
# ord = decorana(decostand(mat, "pa"), iweigh = 1)
ord = decorana(mat, iweigh = 1)

# yields a better result but does not give a score for each word
# ord = decorana(vegdist(decostand(mat, "pa"), "bray"), iweigh = 1)
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

# kmeans clustering (output cluster classes are arbitrarily assigned since
# kmeans starts with k randomly chosen centroids; hence when rerunning the
# clustering class 1 might become class 3)
# To make results reproducible, set a seed
set.seed(27042018)
classes = kmeans(vegdist(mat, "bray"), 4)
table(classes$cluster)
# classes = pam(vegdist(mat, "bray"), 4)
# computing the indicator values
# a seed is also needed here, since results may vary slightly from run to run
set.seed(270420182)
ind = labdsv::indval(mat, classes$cluster, numitr = 1000)
# save ordination/classification output
# saveRDS(ord, "images/04_ord.rds")
# saveRDS(classes, "images/04_classes.rds")
# saveRDS(ind, "images/04_ind.rds")

# load classification output
ind = readRDS("images/04_ind.rds")
ord = readRDS("images/04_ord.rds")
classes = readRDS("images/04_classes.rds")
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
# each time you run labdsv::indval cluster order changes, so in one run class 1
# is urban/infrastructure and in the next run class 1 is ppgis, hence, you have
# to create also anew 08_class_mapping (you would have to do it in any case, but
# just to remember)!!!
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
  scale_fill_manual(values = pal,
                    labels = c("EL cluster", "MT cluster",
                               "PC cluster", "UI cluster"))
ggsave("figures/04_dca.png", p_1, dpi = 300, width = 18, height = 15,
       units = "cm")
save(p_1, "images/04_p_1.rda")

#**********************************************************
# 4 CLUSTER CENTROIDS--------------------------------------
#**********************************************************
#
# abs_df = readRDS("images/00_abs_df.rds")
tc = readRDS("images/00_tc.rds")
wos = readRDS("images/00_wos.rds")
# # if abs_df and wos were loaded anew, you have to add  id_citavi again
# abs_df = inner_join(abs_df, dplyr::select(wos, WOS, id_citavi),
#                     by = "WOS")

# add times cited
abs_df = inner_join(abs_df, dplyr::select(tc, tc, WOS), by = "WOS")

# find cluster centroids (centers) in ordination space
x = data.frame(scores(ord, display = "sites")[, 1:2], class = classes$cluster)
plot(x[, 1:2], col = classes$cluster, pch = 16)
cen = group_by(x, class) %>%
  summarize_all(funs(mean))
points(cen[, c("DCA1", "DCA2")], cex = 3, pch = 16,  col = "yellow")
# find the points closest to each center
dists = sp::spDists(as.matrix(dplyr::select(x, -class)),
                    as.matrix(dplyr::select(cen, -class)))
dists = as.data.frame(dists)

# using a spatial approach (centroids of convex hulls)
# x = st_as_sf(x, coords = c("DCA1", "DCA2"))
# x_1 = filter(x, class == 1)
# x_1 = st_union(x_1) %>% st_convex_hull
# plot(st_centroid(x_1), add = TRUE, col = "blue", pch = 16)
# plot(st_geometry(x))
# plot(st_centroid(x_1), add = TRUE, col = "blue", pch = 16)
# points(within(cen, rm(class)), col = "red", cex = 1, pch = 16)

# use the original rownames (inherited from abs_df, i.e. idCitavi)
rownames(dists) = rownames(x)
# find the 5 closest points to each centroid
out = lapply(seq_len(ncol(dists)), function(i) {
  # make sure to only select points belonging to the corresponding cluster
  tmp = dists[x$class == i, ]
  # now select the twenty closest points
  rownames(tmp[order(tmp[, i]), ])[1:20]
})
# check
x[out[[1]], ]  # ok
x[out[[2]], ]  # ok
i = 2
filter(x, class == i) %>% dplyr::select(-class) %>% plot
points(cen[i, c("DCA1", "DCA2")], cex = 3, pch = 16,  col = "yellow")
points(dplyr::select(x[out[[i]], ], -class), pch = 16, col = "lightblue")

res = data.frame(x[unlist(out), "class"], as.numeric(unlist(out)),
                 stringsAsFactors = FALSE)
names(res) = c("class", "id_citavi")
res = inner_join(dplyr::select(abs_df, -abstract), res, by = "id_citavi")

res = group_by(res, class) %>%
  # top_n(15, tc) %>%
  arrange(class, desc(tc))

# to find the right cluster names, use the ordination plot
p_1

res$class = as.factor(res$class)
levels(res$class) =
  c("Ecology and landscape", "Media and technology",
    "Participation and community", "Urban and infrastructure")

# add data collection methods, GIS methods, used GIS

# check if there are studies which use more than 1 GIS
tmp = group_by(trans_soft_qd, w) %>% 
  summarize(n = n_distinct(GIS)) %>% 
  pull(n) != 1
sum(tmp)  # 0
# check if there are studies which use more than transformation method
tmp = group_by(trans_soft_qd, w) %>% 
  summarize(n = n_distinct(t)) %>% 
  pull(n) != 1
sum(tmp)  # 0
# ok, in this case let's put all qualitative data collection methods in one col
# but before doing so, let us rename certain factor levels
levels(trans_soft_qd$qdata) %<>%
  fct_recode("Mapping Workshop" = "Mapping\nWorkshop")
levels(trans_soft_qd$t) %<>% 
  fct_recode("Transformations" = "Transfor-\nmations")
trans_soft_qd = group_by(trans_soft_qd, w) %>%
  # put all qualitative data collection methods into one column
  mutate(qd = paste(unique(qdata), collapse = ";")) %>%
  # just keep the first row of each group
  slice(1) %>%
  select(-qdata)
dim(trans_soft_qd)  # 380 rows, perfect

# TO DO: there are studies in which the same qualitative data collection method
# was recorded more than once, pls change in 03_alluvial.R
# !!!!!!!!!!!!!!!!!!!!!!!!!!!------------------------------
  
res = left_join(select(res, WOS, year, titel, tc, class),
                select(trans_soft_qd, -year), by = c("WOS" = "w"))
setnames(res, c("WOS", "titel", "GIS", "t", "qd"), 
         c("wos_id", "title", "used_GIS", "data_transform", "collect_meth"))

# save your output
# write.csv2(res, file = "C:/Users/pi37pat/Desktop/centroids.csv",
#            row.names = FALSE)

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
# public health and public transportation
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
# TESTING ISOMAP, NMDS, PAM, KMEANS------------------------
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

kmeans.fun(decostand(mat, "pa"), "bray")
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


