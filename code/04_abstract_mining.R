# Filename: 04_abstract_mining.R (2019-01-16)
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
# 2. ABSTRACT MINING
# 3. ORDINATION & CLUSTERING
# 4. VISUALIZATION - KMEANS/DCA BIPLOT
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("data.table")
library("tm")
# see
# browseURL("https://github.com/vegandevs/vegan/issues/303")
# use >= vegan5.4-4
# devtools::install_github("vegandevs/vegan")
library("vegan")  
library("tidyverse")

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
# 2 ABSTRACT MINING----------------------------------------
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

# find land use
grep("land use", abs)
abs = gsub("land use", "landuse", abs)
ids = names(abs)
# delete punctuation and alike
# just checking what will be remove and what not
str_replace_all(c(":hal^l2o!>/", "ha.<lo$2", "^serv12}us"), 
                "([:punct:]|[:digit:]|[:cntrl:])", "")
abs = str_replace_all(abs, "[:punct:]|[:digit:]|[:cntrl:]", "") %>%
  stripWhitespace()
which(sapply(str_extract_all(abs, "\\$|\\^|\\}"), length) > 0)
which(sapply(str_extract_all(abs, "<|>"), length) > 0)
abs = str_replace_all(abs, "[<>\\$\\^\\{\\}]", "")

# what about web addresses
sapply(str_extract_all(abs, "www|http"), length) > 0
# ok, delete web addresses
# find www or http and then go til the next empty space (non-greedy) or if there
# is no space just go to the end (can happen when the web address is the last
# word of an abstract)
# str_extract_all(abs, "(www|http)(.*? |.*)")
abs = str_replace_all(abs, "(www|http)(.*? |.*)", "")

# remove stop words
stop_words = c(stopwords("SMART"), c("due"))
# abs have lost their names, give them back!!
names(abs) = ids
test = lapply(abs, function(x) {
  x_2 = str_split(x, pattern = " ") %>%
    unlist %>%
    tolower
  x_2 = x_2[!x_2 %in% stop_words]
  stemDocument(x_2, language = "eng")
})

tmp = do.call(c, test)
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
grep("enviro", names(mat), value = TRUE)
grep("environmentalchang", abs)  # 363
# in the original abstract, it says 'environmental-change',
# 'human-environmental', not much we can do about that 

# save your result and remember that rownames(mat) correspond to abs_df$id_citavi 
# saveRDS(mat, file = "images/04_mat.rds")

#**********************************************************
# 3 ORDINATION & CLUSTERING--------------------------------
#**********************************************************

# 3.1 DCA==================================================
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

# save ordination output
# saveRDS(ord, "images/04_ord.rds")

# 3.2 Clustering===========================================
#**********************************************************

# 3.2.1 Optimal number of classes using wss################
#**********************************************************

# Determine optimal number of classes using within sum-of-squares
library("factoextra")
library("NbClust")

# Elbow method - kmeans
fviz_nbclust(scores(ord, display = "species", choices = 1:2), kmeans,
             method = "wss", k.max = 15) +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# similar result when using pam
fviz_nbclust(scores(ord, display = "species", choices = 1:2), pam, 
             method = "wss", k.max = 15)

# similar results when using Ward's clustering 
# hcut uses as default hc_method = "ward.D2"
fviz_nbclust(scores(ord, display = "species", choices = 1:2), hcut,  
             method = "wss", k.max = 15)  

# 3.2.2 kmeans clustering##################################
#**********************************************************
# kmeans clustering (output cluster classes are arbitrarily assigned since
# kmeans starts with k randomly chosen centroids; hence when rerunning the
# clustering class 1 might become class 3)

# To make cluster results reproducible, set a seed
set.seed(16012019)
classes = kmeans(scores(ord, display = "species", choices = 1:2), 4)
table(classes$cluster)

# save classification output
# saveRDS(classes, "images/04_classes.rds")

#**********************************************************
# 4 Visualization - DCA-cluster biplot---------------------
#**********************************************************

# 4.1 Visualization========================================
#**********************************************************
# attach ordination/classification output
mat = readRDS("images/04_mat.rds")
ord = readRDS("images/04_ord.rds")
classes = readRDS("images/04_classes.rds")

# indicator analysis no longer meaninful since the DCA axes are assigned a class
# and not the words. Instead, we simply use the most frequent, and thus most
# important, words as label in the plot. This is much simpler, easier to
# understand, and produces almost the same results.

out = data.frame(
  "words" = names(classes$cluster),  # should be the same as names(mat)
  # count the number of words assuming that the most frequent words are also the
  # most important
  n = colSums(mat),
  # cluster class
  class = classes$cluster,
  # scores
  "scores_1" = vegan::scores(ord, display = "species")[, 1],
  "scores_2" = vegan::scores(ord, display = "species")[, 2])

out_2 = group_by(out, class) %>%
  arrange(desc(n)) %>%
  slice(1:15) %>%
  as.data.frame

# define color palette
pal = RColorBrewer::brewer.pal("Set3", n =  6)[3:6]
# DCA biplot with most frequent words as labels, labels are colored in
# accordance with their class
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
  # check labels in case you classify anew since categories are assigned
  # randomly meaning that "EL cluster" could be 2 next time instead of 1
  scale_fill_manual(values = pal,
                    labels = c("TR cluster", "CU cluster",
                               "EL cluster", "PC cluster"))
# PC = Participatory Community cluster
# EL = Ecology and Landscape cluster
# TR = Theoretical Review cluster
# CU = Children Urban cluster 
p_1
# ggsave("figures/04_dca.png", p_1, dpi = 300, width = 18, height = 15,
#        units = "cm")
# save(p_1, "images/04_p_1.rda")

# 4.2 Named cluster table (majority vote)==================
#**********************************************************
# rownames correspond to words
clus = data.frame(cluster = classes$cluster)
# ok, we will have a majority vote, i.e., we will assign a paper the class that
# was most often assigned to its abstract words
clus = tibble::rownames_to_column(clus, var = "word") %>%
  mutate(cluster = as.factor(cluster))

# construct the majority vote
library("parallel")
out = mclapply(seq_len(nrow(mat)), mc.cores = 6, FUN = function(i) {
out = lapply(seq_len(nrow(mat)), FUN = function(i) {
  print(i)
  obs = mat[i, names(mat)[mat[i, ] > 0]]
  # reshape so that each word and its count value represent two columns
  obs = melt(obs)
  # extract the cluster class for the words appearing in the abstract
  filter(clus, word %in% obs$variable) %>%
    # add the cluster class column
    cbind(select(obs, value), .) %>%
    # aggregate the word counts by cluster
    group_by(cluster) %>%
    summarize(n = sum(value)) %>%
    arrange(desc(n))
  })

# check the cases where two classes have the same number of words
ind = sapply(out, function(x) {
  identical(x$n[1], x$n[2])
})
sum(ind)  # in 7 cases the first and the second place is shared
out[ind]  # well, then we will favor the lower-number classes

# extract the winning class
class_pap = unlist(lapply(out, function(x) x[1, ] %>% pull(cluster)))
table(class_pap)
clus = data.frame(id_citavi = as.integer(rownames(mat)),
                  cluster = class_pap)
levels(clus$cluster) = c("TR", "CU", "EL", "PC")

# check
head(clus)
table(clus$cluster)
classes$cluster[names(classes$cluster) %in%
                  c("green", "valu", "environment", "landscap")]
classes$cluster[names(classes$cluster) %in%
                  c("public", "stakehold", "ppgis", "particip")]
# save cluster majority vote
saveRDS(clus, "images/04_classes_df.rds")
