#' @title Save barplots to a specific location
#' @details `save_barplot` creates a barplot for each cluster group and saves
#'   the resulting figures to a specific location.
#' @param d Molten [data.frame].
#' @param value name of the column which should be the input for the barplot
#' @param bar_name Name of the column which contains the names of the bars.
#' @param dir_name Path to the directory where to save the barplots including
#'   the beginning of the basename. The basename will be completed by rownumber
#'   i and ".png".
save_barplot = function(d, value, bar_name, dir_name) {
  for (i in unique(d$cluster)) {
    tmp = dplyr::filter(d, cluster == i)
    png(filename = paste0(dir_name, i, ".png"), 
        res = 300, width = 8, height = 4, units = "cm")
    par(font = 6, font.lab = 6, font.axis = 6, font.sub = 6, mar = rep(0, 4))
    bar.stats = barplot(tmp[, value] %>% pull, width = 0.5,
                        axisnames = FALSE, horiz = TRUE, axes = FALSE,
                        border = "gray75",
                        #ylab = "triggering factors",
                        xlim = c(-6.5, 85),
                        ylim = c(-1, 3),
                        col = "lightgrey")
    axis(1, at = c(seq(0, ceiling(max(d[, value]) / 10) * 10, 10)), 
         pos = -(0.025 * 1), cex.axis = 1,
         mgp = c(3, 0.3, 0)) # mgp: now the x-labels are closer to the axis
    text(x = 20, y = bar.stats, pos = 4, offset = -3, 
         labels = unique(d[, bar_name]) %>% pull, cex = 1, srt = 0, 
         col = "black", font = 1)
    # text (x = 40, y = -0.5, labels = "%", col = "black", cex = 1)
    dev.off()
  }
}
