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

# ---- Read.results ----
load("Results/from1_withRotation.rda")

assigncol <- function(target.list, col4clust){
  # target.list: is the list where the col list (that includes gc.vec, gc, and oc) is stored
  # col4clust: is a vector of colors for each cluster. The names of these clusters are the names of these elements.
  col <- list()
  col$gc.vec <- col4clust
  col$gc <- col$gc.vec %>% as.matrix
  col$oc <- dplyr::recode(target.list$list, !!!col$gc.vec) %>% as.matrix
  return(col)
}
getColorFromTheWheel <- function(f.scores, design, order.dim = 1, color.range = c("#006e00", "#e6b207")){
  mean.f <- getMeans(f.scores, design)[,1:2]
  ordered.f <- mean.f[order(mean.f[,1]),]
  colfunc <- colorRampPalette(color.range)
  colvec <- colfunc(nrow(mean.f))
  names(colvec) <- rownames(ordered.f)
  return(colvec)
}


# Dendogram
## traits
### summary: All job zones PCA and Job Zones 1-3 PCA are mostly the same as the non-rotated one.
###          Job Zones 4-5 PCA are different
fviz_dend(fit.c_all, k = 9,
          labels_track_height = 60, show_labels = TRUE,
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "grey60",
          # label_cols = order.oc_jz45,
          cex = 0.8, #xlab = 'Job Traits', 
          main = 'Trait clusters',
          ylab = "Squared Ward Distance",
          k_colors = "black") +
  theme(text = element_text(size = 30))

fviz_dend(fit.c_45, k = 10,
          labels_track_height = 60, show_labels = TRUE,
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "grey60",
          # label_cols = order.oc_jz45,
          cex = 0.8, #xlab = 'Job Traits', 
          main = 'Trait clusters',
          ylab = "Squared Ward Distance",
          k_colors = "black") +
  theme(text = element_text(size = 30))

fviz_dend(fit.c_123, k = 9,
          labels_track_height = 60, show_labels = TRUE,
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "grey60",
          # label_cols = order.oc_jz45,
          cex = 0.8, #xlab = 'Job Traits', 
          main = 'Trait clusters',
          ylab = "Squared Ward Distance",
          k_colors = "black") +
  theme(text = element_text(size = 30))

### Figures
fitres = fit.c_45_6fac
pcarot = pca_45rot_fj
pcares = pca_45
k = 10

clusfj_all <- cutree(fitres, k = k)
trt.clust <- list()
trt.clust$all$names <- c('1' = "Physical",
                         '2' = "Oper&Cntrl",
                         '3' = "Cognitive",
                         '4' = "Sensory",
                         '5' = "Math",
                         '6' = "Communication",
                         '7' = "Business",
                         '8' = "NatSci",
                         '9' = "SciEng",
                         '10'= "SocSci&Humanities")
trt.clust$all$list <- as.matrix(dplyr::recode(clusfj_all, !!! trt.clust$all$names))
rownames(trt.clust$all$list) <- rownames(clusfj_all)

col.range = c("#00429d", "#cd3278")
trt.col4clust_all <- getColorFromTheWheel(pcares$Fixed.Data$ExPosition.Data$fj, trt.clust$all$list, color.range = col.range)
trt.clust$all$col <- assigncol(trt.clust$all, trt.col4clust_all)
rownames(trt.clust$all$col$oc) <- rownames(trt.clust$all$list)

showfj_all <- createFactorMap(pcarot, col.points = trt.clust$all$col$oc)
fj_mean <- getMeans(pcarot, trt.clust$all$list)
showfj_name <- createFactorMap(fj_mean, col.labels = trt.clust$all$col$gc[rownames(fj_mean),])
showTIfj_all <- MakeToleranceIntervals(pcarot, trt.clust$all$list, col = trt.clust$all$col$gc[rownames(fj_mean),], p.level = 1)
showfj_all$zeMap_background + showfj_all$zeMap_dots + showTIfj_all + showfj_name$zeMap_text

## occupation figures

