#==== Summary ====
# This file reads the results of PCAs and clustering analyses and create supplementary figures that show detailed dendrograms of occupations
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
library(ggrepel)
library(ggpubr)

# ---- Read.results ----
load("Results/from1_DatPcaClus.rda")
load("Results/from2_Dat4Plot.rda")

# ---- function to plot the clustering results ----
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

# ---- Supp 1: all job zones ----
order.c_all <- c(5, 8, 7, 4, 3, 9, 1, 6, 2)
order.color.c_all <- c(5, 1, 2, 3, 4, 8, 9, 7, 6)
order.gc_all <- trt.clust$all$col$gc.vec[order.color.c_all]
order.oc_all <- trt.clust$all$col$oc[fit.c_all$order]
seg.c_all <- get.clust.segment(fit.c_all, 9, trt.clust$all, y.pos = -120, order.c_all)
get.dend.c_all <- fviz_dend(fit.c_all, k = 9,show_labels = TRUE,
                            labels_track_height = 150,
                            rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                            rect_border = order.gc_all,
                            label_cols = order.oc_all,
                            cex = 0.8, #xlab = 'Job Traits',
                            main = 'Occupational trait clusters',
                            ylab = "Squared Ward Distance",
                            k_colors = "black") +
  geom_text(data = seg.c_all, inherit.aes = FALSE, angle = 0,
            aes(x = x + c(0.5,0,2,0,10,0.5,1,0.5,1), y = y + c(0,-20,-10,0,0,0,0,0,0), label = label), hjust = "center",
            color = order.gc_all, fontface = "bold", size = 5) +
  theme(text = element_text(size = 30))
dend.c_all <- get.dend.c_all +
  scale_y_continuous(breaks = seq(0, 250, by = 100)) + 
  annotate("segment", x = 0, y =0, xend = dim(trt.clust$all$list)[1]+1, yend = 0, linetype = 1) +
  annotate("segment", x = 0, y = 100, xend = dim(trt.clust$all$list)[1]+1, yend = 100, linetype = 2, colour = "grey", size = 1.5) +
  annotate("segment", x = 0, y = 200, xend = dim(trt.clust$all$list)[1]+1, yend = 200, linetype = 2, colour = "grey", size = 1.5)
dend.c_all

png(filename =  "Supp1.png", width = 60, height = 30, units = "cm", bg = "white",res = 300)
dend.c_all
dev.off()

# ---- Supp 2: job zones 45 ---- (with 6 components)
get.dend.c_jz45_6fac <- fviz_dend(fit.c_45_6fac, k = 10,
                             labels_track_height = 60, show_labels = TRUE,
                             rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                             rect_border = "grey60",
                             # label_cols = order.oc_jz45,
                             cex = 0.8, #xlab = 'Job Traits', 
                             main = 'Occupational trait clusters',
                             ylab = "Squared Ward Distance",
                             k_colors = "black") +
  theme(text = element_text(size = 30))
dend.c_jz45_6fac <- get.dend.c_jz45_6fac +
  scale_y_continuous(breaks = seq(0, 80, by = 50)) + 
  annotate("segment", x = 0, y =0, xend = dim(trt.clust$jz45$list)[1]+1, yend = 0, linetype = 1) +
  annotate("segment", x = 0, y = 50, xend = dim(trt.clust$jz45$list)[1]+1, yend = 50, linetype = 2, colour = "grey", size = 1.5) 
dend.c_jz45_6fac

png(filename =  "Supp2.png", width = 60, height = 30, units = "cm", bg = "white",res = 300)
dend.c_jz45_6fac
dev.off()

# ---- Supp 3: job zones 45 ----
order.c_jz45 <- c(10, 6, 3, 4, 9, 5, 7, 8, 2, 1)
order.color.c_jz45 <- c(1,2,3,4,10, 7, 5, 9, 8, 6)
order.gc_jz45 <- trt.clust$jz45$col$gc.vec[order.color.c_jz45]
order.oc_jz45 <- trt.clust$jz45$col$oc[fit.c_45$order]
seg.c_jz45 <- get.clust.segment(fit.c_45, 10, trt.clust$jz45, y.pos = -60, order.c_jz45)
get.dend.c_jz45 <- fviz_dend(fit.c_45, k = 10,
                             labels_track_height = 70, show_labels = TRUE,
                             rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                             rect_border = order.gc_jz45,
                             label_cols = order.oc_jz45,
                             cex = 0.8, #xlab = 'Job Traits', 
                             main = 'Occupational trait clusters',
                             ylab = "Squared Ward Distance",
                             k_colors = "black") +
  geom_text(data = seg.c_jz45, inherit.aes = FALSE, angle = 0,
            aes(x = x + c(0,2,3,0,0,0,2,0,2,2), y = y + c(0,-5,0,0,0,0,0,0,0,0), label = label), hjust = "center",
            color = order.gc_jz45, fontface = "bold", size = 5) +
  theme(text = element_text(size = 30))
dend.c_jz45 <- get.dend.c_jz45 +
  scale_y_continuous(breaks = seq(0, 80, by = 50)) + 
  annotate("segment", x = 0, y =0, xend = dim(trt.clust$jz45$list)[1]+1, yend = 0, linetype = 1) +
  annotate("segment", x = 0, y = 50, xend = dim(trt.clust$jz45$list)[1]+1, yend = 50, linetype = 2, colour = "grey", size = 1.5) 
dend.c_jz45

png(filename =  "Supp3.png", width = 60, height = 30, units = "cm", bg = "white",res = 300)
dend.c_jz45
dev.off()

# ---- Supp 4: job zones 123 ----
order.c_jz123 <- c(8, 7, 2, 4, 9, 3, 1, 5, 6)
order.color.c_jz123 <- c(2, 1, 4, 3, 9, 8, 6, 7, 5)
order.gc_jz123 <- trt.clust$jz123$col$gc.vec[order.color.c_jz123]
order.oc_jz123 <- trt.clust$jz123$col$oc[fit.c_123$order]
seg.c_jz123 <- get.clust.segment(fit.c_123, 9, trt.clust$jz123, y.pos = -67, order.c_jz123)
get.dend.c_jz123 <- fviz_dend(fit.c_123, k = 9,
                              labels_track_height = 70, show_labels = TRUE,
                              rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                              rect_border = order.gc_jz123,
                              label_cols = order.oc_jz123,
                              cex = 0.8, #xlab = 'Job Traits',
                              main = 'Occupational trait clusters',
                              ylab = "Squared Ward Distance",
                              k_colors = "black") +
  geom_text(data = seg.c_jz123, inherit.aes = FALSE, angle = 0,
            aes(x = x + c(1,-1,0,0,0.5,0.5,2.5,1,0), y = y, label = label), hjust = "center",
            color = order.gc_jz123, fontface = "bold", size = 5) +
  theme(text = element_text(size = 30))
dend.c_jz123 <- get.dend.c_jz123 +
  scale_y_continuous(breaks = seq(0, 140, by = 50)) + 
  annotate("segment", x = 0, y =0, xend = dim(trt.clust$jz123$list)[1]+1, yend = 0, linetype = 1) +
  annotate("segment", x = 0, y = 50, xend = dim(trt.clust$jz123$list)[1]+1, yend = 50, linetype = 2, colour = "grey", size = 1.5) +
  annotate("segment", x = 0, y = 100, xend = dim(trt.clust$jz123$list)[1]+1, yend = 100, linetype = 2, colour = "grey", size = 1.5)
dend.c_jz123

png(filename =  "Supp4.png", width = 60, height = 30, units = "cm", bg = "white",res = 300)
dend.c_jz123
dev.off()
