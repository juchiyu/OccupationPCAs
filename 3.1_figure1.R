#==== Summary ====
# This file reads the results of PCAs and clustering analyses and create Figure1
# 

# ---- Clean up ----
rm(list = ls())
graphics.off()

# ---- Packages ----
library(devtools)
library(readxl)
library(ExPosition)
library(InPosition)
library(ggplot2)
library(PTCA4CATA)
library(dplyr)
library(data4PCCAR)
library(factoextra)
library(dendextend)
library(ggplot2)
library(ggdendro)
library(tidyverse)
library(plyr)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggpubr)

# ---- Read.results ----
load("Results/from2_Dat4Plot.rda")

# ---- Scree ----
PlotMyScree <- function(eigres, color.sig = "mediumorchid4", color.ns = "grey60",
                        cex.sig = 1.1, cex.ns = 0.5, text.cex = 10, lwd = 1, title = NULL, xlab = "Components"){
  eigres %>% as.data.frame %>% ggplot(aes(x = 1:length(eig), y = tau)) +
    geom_line(color = "grey40", size = lwd) +
    geom_point(aes(color = as.character(eigres$pval < .05), size = as.character(eigres$pval < .05))) +
    scale_color_manual(values = c("TRUE" = color.sig, "FALSE" = color.ns)) +
    scale_size_manual(values = c("TRUE" = cex.sig, "FALSE" = cex.ns)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "darkgreen", size = lwd) +
    scale_y_continuous(name = bquote(atop(bold(.(title)),paste('\n\n\nPercentage of variance explained'))),
                       sec.axis = sec_axis(~.*(eigres$eig[1]/eigres$tau[1]), name = "Eigenvalues\n")) +
    xlab(xlab) +
    theme(text = element_text(size = text.cex),
          legend.position = "none",
          axis.text.y.left = element_text(angle = 90),
          axis.text.y.right = element_text(angle = 270),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent"))
}
scree_all <- PlotMyScree(eigres_all, cex.sig = 8, cex.ns = 5, text.cex = 30, lwd = 2, title = "All Job Zones", xlab = "") %>%
  arrangeGrob(top = textGrob(expression(bold("A")), x = unit(0, "npc"), y = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
scree_45 <- PlotMyScree(eigres_45, cex.sig = 8, cex.ns = 5, text.cex = 30, lwd = 2, title = "Job Zones 4 & 5", xlab = "") %>%
  arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
scree_123 <- PlotMyScree(eigres_123, cex.sig = 8, cex.ns = 5, text.cex = 30, lwd = 2, title = "Job Zones 1, 2, & 3") %>%
  arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

# ---- row.cluster ----
plot.dend <- function(hc, k, clust.info,
                      expand.x = c(0,0.5), expand.y = c(0.05,0.05), y.position = "right",
                      x.label = "Squared Ward Distance", y.label = "", colour.label = "", size.label = "",
                      top.dend.label = "~underline(bold('Occupation clusters'))", top.n.label = "~underline(~italic('N'))",
                      show.n.size = 3.5, show.nlabel.size = 3.5, show.n.pos = 1.1, offset.clustName = -10, offset.clustN = -350,
                      show.x.lim = c(0,250), show.x.break = 100, real.x.max = -450,
                      coord.xlim = NULL,
                      adjust.title.clustName = -50, adjust.title.clustN = 25, adjust.title.y = 50){
  x.offset.clustName = offset.clustName
  x.offset.clustN = offset.clustN
  cutoff <- mean(rev(hc$height)[(k-1):k])
  dend_data <- cut(as.dendrogram(hc), h = cutoff)$upper %>% hang.dendrogram(hang = -1)
  
  dend_data_cut <- dendro_data(dend_data)
  
  segment_data <- with(
    segment(dend_data_cut), 
    data.frame(x = y, y = x, xend = yend, yend = xend))
  
  cluster_positions <- segment_data[segment_data$xend == 0, "y"]
  
  n_of_clust <-  cutree(hc, k) %>% table
  clust_n_data <- data.frame(list(x = rep(0,k),
                                  y = cluster_positions,
                                  label = clust.info$names,
                                  label.n = as.character(n_of_clust)))
  
  if (is.null(coord.xlim)){coord.xlim = c(max(segment_data$x),real.x.max)}
  
  plt_dendr <- ggplot(segment_data) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    scale_x_reverse(expand = expand.x) + 
    scale_y_continuous(expand = expand.y) +
    labs(x = x.label, y = y.label, colour = colour.label, size = size.label) +
    # add cluster names
    geom_text(data = clust_n_data, inherit.aes = FALSE,
              aes(x = x.offset.clustName, y = y, label = label), hjust = 0,
              color = clust.info$col$gc.vec[clust_n_data$label], fontface = "bold", size = show.n.size) +
    annotate("text", x = x.offset.clustName + adjust.title.clustName, y = max(clust_n_data$y) + adjust.title.y, 
             label = top.dend.label, parse = TRUE, size = show.nlabel.size) +
    # add N of clusters
    geom_text(data = clust_n_data, inherit.aes = FALSE,
              aes(x = x.offset.clustN, y = y, label = label.n), hjust = 1,
              color = clust.info$col$gc.vec[clust_n_data$label], fontface = "bold", size = show.n.size) +
    annotate("text", x = x.offset.clustN + adjust.title.clustN, y = max(clust_n_data$y) + adjust.title.y, 
             label = top.n.label, parse = TRUE, size = show.nlabel.size) +
    # set themes
    coord_cartesian(xlim = coord.xlim) +
    scale_x_continuous(breaks = seq(show.x.lim[1], show.x.lim[2], by = show.x.break)) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          rect = element_rect(fill = "transparent"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_text(size = size.label),
          axis.title.x = element_text(size = size.label),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(), 
          panel.border = element_blank())
  
  dend.res <- list(dend = plt_dendr, seg_dat = segment_data)
  return(dend.res)
}

# get dendrogram data
get.dend.r_all <- plot.dend(fit.r_all, 18, occu.clust$all, size.label = 25, show.n.size = 8, adjust.title.y = 60, x.label = "",
                            show.nlabel.size = 10)
get.dend.r_jz45 <- plot.dend(fit.r_45, 10, occu.clust$jz45, size.label = 25, show.n.size = 8, adjust.title.y = 40, x.label = "", 
                             coord.xlim = c(max(get.dend.r_all$seg_dat$x), -450), top.dend.label = "", top.n.label = "") #25
get.dend.r_jz123 <- plot.dend(fit.r_123, 10, occu.clust$jz123, size.label = 30, show.n.size = 8, adjust.title.y = 40, 
                              coord.xlim = c(max(get.dend.r_all$seg_dat$x), -450), top.dend.label = "", top.n.label = "") #25

# plot the trees
dend.r_all <-  get.dend.r_all$dend + 
  annotate("segment", x = 0, y = -20, xend = 0, yend = max(get.dend.r_all$seg_dat$y)+10, size = 2) +
  annotate("segment", x = 100, y = -20, xend = 100, yend = max(get.dend.r_all$seg_dat$y)+10, linetype = 2, colour = "grey", size = 2) +
  annotate("segment", x = 200, y = -20, xend = 200, yend = max(get.dend.r_all$seg_dat$y)+10, linetype = 2, colour = "grey", size = 2)
dend.r_jz45 <- get.dend.r_jz45$dend +
  annotate("segment", x = 0, y = -20, xend = 0, yend = max(get.dend.r_jz45$seg_dat$y)+10, size = 2) +
  annotate("segment", x = 100, y = -20, xend = 100, yend = max(get.dend.r_jz45$seg_dat$y)+10, linetype = 2, colour = "grey", size = 2) +
  annotate("segment", x = 200, y = -20, xend = 200, yend = max(get.dend.r_jz45$seg_dat$y)+10, linetype = 2, colour = "grey", size = 2)
dend.r_jz123 <- get.dend.r_jz123$dend +
  annotate("segment", x = 0, y = -20, xend = 0, yend = max(get.dend.r_jz123$seg_dat$y)+10, size = 2) +
  annotate("segment", x = 100, y = -20, xend = 100, yend = max(get.dend.r_jz123$seg_dat$y)+10, linetype = 2, colour = "grey", size = 2) +
  annotate("segment", x = 200, y = -20, xend = 200, yend = max(get.dend.r_jz123$seg_dat$y)+10, linetype = 2, colour = "grey", size = 2)

dend.r_all
# ---- column.cluster ----
get.clust.segment <- function(hc, k, clust.info, y.pos, show.name.order){
  cutoff <- mean(rev(hc$height)[(k-1):k])
  segment_data <- cut(as.dendrogram(hc), h = cutoff)$upper %>% hang.dendrogram(hang = -1) %>% dendro_data %>% segment
  cluster_positions <- segment_data[segment_data$yend == 0, "x"]
  n_of_clust <-  cutree(hc, k) %>% table
  clust_n_data <- data.frame(list(x = cluster_positions,
                                  y = rep(y.pos,k),
                                  label = clust.info$names[show.name.order],
                                  label.n = as.character(n_of_clust[show.name.order])))
}

# all job zones
order.c_all <- c(5, 8, 7, 4, 3, 9, 1, 6, 2)
order.color.c_all <- c(5, 1, 2, 3, 4, 8, 9, 7, 6)
order.gc_all <- trt.clust$all$col$gc.vec[order.color.c_all]
order.oc_all <- trt.clust$all$col$oc[fit.c_all$order]
seg.c_all <- get.clust.segment(fit.c_all, 9, trt.clust$all, y.pos = -160, order.c_all)
get.dend.c_all <- fviz_dend(fit.c_all, k = 9,show_labels = FALSE,
                            labels_track_height = 170,
                            rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                            rect_border = order.gc_all,
                            label_cols = order.oc_all,
                            cex = 1, #xlab = 'Job Traits',
                            main = 'Occupational trait clusters',
                            ylab = "Squared Ward Distance",
                            k_colors = "black") +
  geom_text(data = seg.c_all, inherit.aes = FALSE, angle = 90,
            aes(x = x + c(0,0,2,0,10,0,0,0,0), y = y + c(70,73,65,70,70,70,70,70,70), label = label), hjust = "center",
            color = order.gc_all, fontface = "bold", size = 10) +
  theme(text = element_text(size = 30), plot.title = element_text(hjust = 0.5))
dend.c_all <- get.dend.c_all +
  scale_y_continuous(breaks = seq(0, 250, by = 100)) + 
  annotate("segment", x = 0, y =0, xend = dim(trt.clust$all$list)[1]+1, yend = 0, size = 2) +
  annotate("segment", x = 0, y = 100, xend = dim(trt.clust$all$list)[1]+1, yend = 100, linetype = 2, colour = "grey", size = 2) +
  annotate("segment", x = 0, y = 200, xend = dim(trt.clust$all$list)[1]+1, yend = 200, linetype = 2, colour = "grey", size = 2)
dend.c_all
# job zones 45
order.c_jz45 <- c(10, 6, 3, 4, 9, 5, 7, 8, 2, 1)
order.color.c_jz45 <- c(1,2,3,4,10, 7, 5, 9, 8, 6)
order.gc_jz45 <- trt.clust$jz45$col$gc.vec[order.color.c_jz45]
order.oc_jz45 <- trt.clust$jz45$col$oc[fit.c_45$order]
seg.c_jz45 <- get.clust.segment(fit.c_45, 10, trt.clust$jz45, y.pos = -140, order.c_jz45)
get.dend.c_jz45 <- fviz_dend(fit.c_45, k = 10,
                            labels_track_height = 150, show_labels = FALSE,
                            rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                            rect_border = order.gc_jz45,
                            label_cols = order.oc_jz45,
                            cex = 1, #xlab = 'Job Traits', 
                            main = '',
                            ylab = "Squared Ward Distance",
                            k_colors = "black") +
  geom_text(data = seg.c_jz45, inherit.aes = FALSE, angle = 90,
            aes(x = x + c(0,2,3,0,0,0,2,0,2,2), y = y + 70, label = label), hjust = "center",
            color = order.gc_jz45, fontface = "bold", size = 10) +
  theme(text = element_text(size = 30))
dend.c_jz45 <- get.dend.c_jz45 +
  scale_y_continuous(breaks = seq(0, 80, by = 50)) + 
  annotate("segment", x = 0, y =0, xend = dim(trt.clust$jz45$list)[1]+1, yend = 0, size = 2) +
  annotate("segment", x = 0, y = 50, xend = dim(trt.clust$jz45$list)[1]+1, yend = 50, linetype = 2, colour = "grey", size = 2) 
dend.c_jz45
# job zones 123
order.c_jz123 <- c(8, 7, 2, 4, 9, 3, 1, 5, 6)
order.color.c_jz123 <- c(2, 1, 4, 3, 9, 8, 6, 7, 5)
order.gc_jz123 <- trt.clust$jz123$col$gc.vec[order.color.c_jz123]
order.oc_jz123 <- trt.clust$jz123$col$oc[fit.c_123$order]
seg.c_jz123 <- get.clust.segment(fit.c_123, 9, trt.clust$jz123, y.pos = -160, order.c_jz123)
get.dend.c_jz123 <- fviz_dend(fit.c_123, k = 9,
                             labels_track_height = 170, show_labels = FALSE,
                             rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                             rect_border = order.gc_jz123,
                             label_cols = order.oc_jz123,
                             cex = 1, #xlab = 'Job Traits',
                             main = '',
                             ylab = "Squared Ward Distance",
                             k_colors = "black") +
  geom_text(data = seg.c_jz123, inherit.aes = FALSE, angle = 90,
            aes(x = x + c(0,-1,5,0,0,1,3,0.5,0), y = y + 70, label = label), hjust = "center",
            color = order.gc_jz123, fontface = "bold", size = 10) +
  theme(text = element_text(size = 30))
dend.c_jz123 <- get.dend.c_jz123 +
  scale_y_continuous(breaks = seq(0, 140, by = 50)) + 
  annotate("segment", x = 0, y =0, xend = dim(trt.clust$jz123$list)[1]+1, yend = 0, size = 2) +
  annotate("segment", x = 0, y = 50, xend = dim(trt.clust$jz123$list)[1]+1, yend = 50, linetype = 2, colour = "grey", size = 2) +
  annotate("segment", x = 0, y = 100, xend = dim(trt.clust$jz123$list)[1]+1, yend = 100, linetype = 2, colour = "grey", size = 2)
dend.c_jz123

# ---- merge.plots ----
png(filename =  "Figure1-fix-CWgradient_202109.png", width = 90, height = 80, units = "cm", bg = "white",res = 300)
ggarrange(scree_all, dend.r_all, dend.c_all,
          scree_45, dend.r_jz45, dend.c_jz45,
          scree_123, dend.r_jz123, dend.c_jz123,
          nrow = 3, ncol = 3,
          widths = c(1,1,2.5),
          heights = c(1.2,1,1))+
  theme(plot.margin = margin(0.5,0,0.5,1, "cm"))
dev.off()
