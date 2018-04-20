library("dplyr")
library("data.table")

dir_data = "data/gis_total"

# search terms:
# GIS OR "geographic* information system"
# year range: 1990-2017
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
d = filter(d, PY != 2017)
plot(TC / sum(TC) ~ PY, d, type = "l")
plot(n / sum(n) ~ PY, d, type = "l")
