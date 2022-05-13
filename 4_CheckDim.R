#==== Summary ====
# This file reads the results of PCAs and clustering analyses and create Figures 2-4
# 

# ---- Clean up ----
rm(list = ls())
graphics.off()

# ---- Packages ----
library(ggplot2)
library(ggdendro)
library(tidyverse)
library(plyr)
library(ggrepel)
library(ggpubr)
library(gridExtra)
library(grid)
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
library(stringr)

func.dir <- ("./functions/")
sapply(paste0(func.dir,list.files(func.dir, pattern = "\\.[Rr]$")), source)


# ---- Read.results ----
load("Results/from2_Dat4Plot.rda")

# ---- get.job.zones.color ----
job.zone.col <- c(`1` = "darkgoldenrod2",
                  `2` = "orange2",
                  `3` = "darkorange2",
                  `4` = "darkorange3",
                  `5` = "darkorange4")
jb.col_45 <- jb.col_123 <- jb.col_all <- list()
jb.col_45$gc <- jb.col_123$gc <- jb.col_all$gc <- job.zone.col %>% as.matrix

jb.col_all$oc <- recode(onet.jz_all, !!!job.zone.col)
jb.col_45$oc <- recode(onet.jz_45, !!!job.zone.col)
jb.col_123$oc <- recode(onet.jz_123, !!!job.zone.col)

# ---- fix all trait names (remove .) ----
rownames(pcares_all$fj) <- gsub("\\.", " ", rownames(pcares_all$fj))
rownames(pcares_45$fj) <- gsub("\\.", " ", rownames(pcares_45$fj))
rownames(pcares_123$fj) <- gsub("\\.", " ", rownames(pcares_123$fj))

# ---- Tables ----
TopBottomTable <- function(f, col.list, leDim = 1, show.n = 3, str4wrap = 30, column.name = "Occupations", row.label = c("Right", "Left")){
  # leDim = 1
  # show.n = 10
  
  order.f <- order(f[,leDim])
  order.fcol <- col.list$oc[order.f,]
  
  fmax <- f[order.f,leDim] %>% tail(show.n) %>% rev %>% round(2)
  fmax.name <- names(fmax) %>% str_wrap(str4wrap)
  fmax.col <- order.fcol %>% tail(show.n) %>% rev
  
  fmin <- f[order.f,leDim] %>% head(show.n) %>% round(2)
  fmin.name <- names(fmin) %>% str_wrap(str4wrap)
  fmin.col <- order.fcol %>% head(show.n)
  
  tab <- list()
  
  tab$top <- cbind(Scores = fmax,
                   fmax.name,
                   color = fmax.col) %>% as.data.frame
  
  tab$bottom <- cbind(Scores = c(fmin),
                      c(fmin.name),
                      color = c(fmin.col)) %>% as.data.frame
  
  rownames(tab$top) <- c(paste0(row.label[1]," ",1), c(2:show.n))
  rownames(tab$bottom) <- c(paste0(row.label[2], " ",1), paste0(" ",c(2:show.n)))
  
  colnames(tab$top)[2] <- column.name
  colnames(tab$bottom)[2] <- column.name
  
  return(tab)
}

## All job zones - Occupation

fi.tab_all.cp1 <- TopBottomTable(pcares_all$fi, occu.clust$all$col, show.n = 25)
fi.tab_all.cp2 <- TopBottomTable(pcares_all$fi, occu.clust$all$col, leDim = 2, row.label = vertical.label)
fi.tab_all.cp3 <- TopBottomTable(pcares_all$fi, occu.clust$all$col, leDim = 3, row.label = vertical.label)

## All job zones - traits

fj.tab_all.cp1 <- TopBottomTable(pcares_all$fj, trt.clust$all$col, column.name = "Job traits")
fj.tab_all.cp2 <- TopBottomTable(pcares_all$fj, trt.clust$all$col, column.name = "Job traits", leDim = 2, row.label = vertical.label)
fj.tab_all.cp3 <- TopBottomTable(pcares_all$fj, trt.clust$all$col, column.name = "Job traits", leDim = 3, row.label = vertical.label)

## Job Zones 45 - Occupation

fi.tab_jz45.cp1 <- TopBottomTable(pcares_45$fi, occu.clust$jz45$col)
fi.tab_jz45.cp2 <- TopBottomTable(pcares_45$fi, occu.clust$jz45$col, leDim = 2, row.label = vertical.label)
fi.tab_jz45.cp3 <- TopBottomTable(pcares_45$fi, occu.clust$jz45$col, leDim = 3, row.label = vertical.label)

## Job zones 45 - traits

fj.tab_jz45.cp1 <- TopBottomTable(pcares_45$fj, trt.clust$jz45$col, column.name = "Job traits")
fj.tab_jz45.cp2 <- TopBottomTable(pcares_45$fj, trt.clust$jz45$col, column.name = "Job traits", leDim = 2, row.label = vertical.label)
fj.tab_jz45.cp3 <- TopBottomTable(pcares_45$fj, trt.clust$jz45$col, column.name = "Job traits", leDim = 3, row.label = vertical.label)

## Job zones 123 - Occupation

fi.tab_jz123.cp1 <- TopBottomTable(pcares_123$fi, occu.clust$jz123$col)
fi.tab_jz123.cp2 <- TopBottomTable(pcares_123$fi, occu.clust$jz123$col, leDim = 2, row.label = vertical.label)
fi.tab_jz123.cp3 <- TopBottomTable(pcares_123$fi, occu.clust$jz123$col, leDim = 3, row.label = vertical.label)

## Job zones 123 - traits

fj.tab_jz123.cp1 <- TopBottomTable(pcares_123$fj, trt.clust$jz123$col, column.name = "Job traits")
fj.tab_jz123.cp2 <- TopBottomTable(pcares_123$fj, trt.clust$jz123$col, column.name = "Job traits", leDim = 2, row.label = vertical.label)
fj.tab_jz123.cp3 <- TopBottomTable(pcares_123$fj, trt.clust$jz123$col, column.name = "Job traits", leDim = 3, row.label = vertical.label)


#---- job.zone.plots ----
get.FacPlot <- function(eigres, pcares, design, col.list, xaxis = 1, yaxis = 2, TiLab = FALSE,
                        alpha.indiv.point = 0.1, alpha.mean.point = 0.9,
                        cex.indiv.point = 5, cex.mean.point = 10, cex.mean.text = 10, label.size = 20, title = "", title.size = 10,
                        print.list = FALSE, list.title = "occupations", str4wrap = 30, list.size = 10, list.ybase = 0, list.scale.y.ony = c(0.1,0,-0.1), list.scale.y.onx = c(0.2,0.25,0.30), list.xmin.scale = 0.15, list.scale.ymin = 0.1){
  jbplt <- PlotFactor(lambda = eigres$eig, tau = eigres$tau,
                      xaxis = xaxis, yaxis = yaxis,
                      f = pcares, design = design, title = title,
                      col.list = col.list, alpha.indiv.point = alpha.indiv.point, alpha.mean.point = alpha.mean.point,
                      cex.indiv.point = cex.indiv.point, cex.mean.point = cex.mean.point, cex.mean.text = cex.mean.text)
  if(isFALSE(TiLab)){
    plot.res <- jbplt$f.map$zeMap_background + jbplt$f.map$zeMap_dots +
      jbplt$mean.map$zeMap_dots + jbplt$mean.map$zeMap_text + jbplt$label +
      theme(text = element_text(size = label.size))
  }else{
    plot.res <- jbplt$f.map$zeMap_background + jbplt$f.map$zeMap_dots +
      jbplt$TI +
      jbplt$mean.map$zeMap_text + jbplt$label +
      theme(text = element_text(size = label.size))    
  }
  
  if(print.list == TRUE){
    x.range <- jbplt$f.map$constraints$maxx - jbplt$f.map$constraints$minx
    y.range <- jbplt$f.map$constraints$maxy - jbplt$f.map$constraints$miny
    x.tab <- TopBottomTable(pcares, col.list, column.name = "text", leDim = xaxis, str4wrap = str4wrap)
    y.tab <- TopBottomTable(pcares, col.list, column.name = "text", leDim = yaxis, str4wrap = 100) #100
    # title.tab <- matrix(rep(c("NA", sprintf("~underline('Most contributing %s')", list.title), "black"),4), nrow = 4, byrow = T, 
    #                      dimnames = list(c(), c("Score", "text", "color")))
    text2print <- rbind(x.tab$top, x.tab$bottom, y.tab$top, y.tab$bottom) %>% data.frame
    text2print$x <- c(rep(jbplt$f.map$constraints$maxx + 0.1*x.range, 3), # max on dim1
                      rep(jbplt$f.map$constraints$minx - list.xmin.scale*x.range, 3), # min on dim1
                      rep(0, 6) # max and min on dim2
    )
    text2print$y <- c(rep(c(list.scale.y.ony*y.range), 2) + list.ybase, # max and min on dim1
                      c(rep(jbplt$f.map$constraints$maxy , 3) + rev(list.scale.y.onx)*y.range), # max on dim2
                      c(rep(jbplt$f.map$constraints$miny-list.scale.ymin*y.range, 3) - list.scale.y.onx*y.range) # min on dim2
    )
    text2print$hjust <- c(rep("left",3),
                          rep("right",3),
                          rep("middle",3),
                          rep("middle",3))
    # title2print <- title.tab %>% data.frame
    # title2print$x <- c(jbplt$f.map$constraints$maxx + 0.1*x.range, # max on dim1
    #                    jbplt$f.map$constraints$minx - list.xmin.scale*x.range, # min on dim1
    #                    rep(0, 2)) # max and min on dim2
    # title2print$y <- c(rep((2*list.scale.y.ony[1]-list.scale.y.ony[2])*y.range + list.ybase,2),
    #                    jbplt$f.map$constraints$maxy + (2*list.scale.y.onx[3]-list.scale.y.onx[2])*y.range,
    #                    jbplt$f.map$constraints$miny-list.scale.ymin*y.range - (2*list.scale.y.onx[1] - list.scale.y.onx[2])*y.range
    #                    )
    # title2print$hjust <- c("left", "right", "middle", "middle")
    plot.res <- plot.res + 
      geom_text(data = text2print, x = text2print$x, y = text2print$y,
                size = list.size, label = text2print$text, 
                color = text2print$color, hjust = text2print$hjust) + 
      # geom_text(data = title2print, x = title2print$x, y = title2print$y,
      #           size = list.size, label = title2print$text, 
      #           color = title2print$color, hjust = title2print$hjust, parse = TRUE) +
      theme(plot.title = element_text(size = title.size), axis.title = element_text(size = title.size), plot.margin = unit(c(2.5,5,2.5,5), "cm")) + coord_fixed(ratio = 1, clip = "off")
  }
  return(plot.res)
}

# ---- fi.plots ----
fi.point.size = 5; fi.text.size = 5; title.size = 30
xaxis = 4; yaxis = 5

## General PCA
get.FacPlot(eigres_all, pcares_all$fi, design = occu.clust$all$list, occu.clust$all$col, xaxis = xaxis, yaxis = yaxis, cex.mean.point = fi.point.size, cex.indiv.point = fi.point.size-2, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = FALSE, 
                         str4wrap = 30, list.xmin.scale = 0.2, list.scale.y.ony = c(0.2,0,-0.2), list.scale.y.onx = c(0.1,0.16,0.23)+0.05, list.scale.ymin = 0.02, list.size = 12)
## Cognitive PCA
get.FacPlot(eigres_45, pcares_45$fi, design = occu.clust$jz45$list, occu.clust$jz45$col, xaxis = xaxis, yaxis = yaxis, cex.mean.point = fi.point.size, cex.indiv.point = fi.point.size-2, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = FALSE, 
                          str4wrap = 30, list.ybase = 3, list.xmin.scale = 0.2, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.scale.ymin = 0.03, list.scale.y.ony = c(0.25,0,-0.25), list.size = 12)
## Labour PCA
get.FacPlot(eigres_123, pcares_123$fi, design = occu.clust$jz123$list, occu.clust$jz123$col, xaxis = xaxis, yaxis = yaxis,cex.mean.point = fi.point.size, cex.indiv.point = fi.point.size-2, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = FALSE, 
                           str4wrap = 30, list.xmin.scale = 0.2, list.scale.ymin = 0.03, list.scale.y.onx = c(0.1,0.18,0.26)+0.05, list.scale.y.ony = c(0.15,0,-0.15), list.size = 12)
# ---- fj.plots ----
fj.point.size = 5; fj.text.size = 5

## General PCA
get.FacPlot(eigres_all, pcares_all$fj, design = trt.clust$all$list, trt.clust$all$col, xaxis = xaxis, yaxis = yaxis, cex.mean.point = fj.point.size, cex.indiv.point = fi.point.size-2, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = FALSE, list.title = "traits", 
                         str4wrap = 30, list.ybase = 24, list.scale.y.ony = c(0.15,0,-0.15), list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.scale.ymin = 0.05, list.size = 12)
## Cognitive PCA
get.FacPlot(eigres_45, pcares_45$fj, design = trt.clust$jz45$list, trt.clust$jz45$col, xaxis = xaxis, yaxis = yaxis, cex.mean.point = fj.point.size, cex.indiv.point = fi.point.size-2, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = FALSE, list.title = "traits", 
                          list.ybase = 10, list.xmin.scale = 0.2, list.scale.y.ony = c(0.15,0,-0.15), list.scale.ymin = 0.04, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.size = 12)

## Labour PCA
get.FacPlot(eigres_123, pcares_123$fj, design = trt.clust$jz123$list, trt.clust$jz123$col, xaxis = xaxis, yaxis = yaxis,cex.mean.point = fj.point.size, cex.indiv.point = fi.point.size-2, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = FALSE, list.title = "traits", 
                           str4wrap = 25, list.ybase = 5, list.scale.ymin = 0.03, list.scale.y.ony = c(0.20,0,-0.20), list.scale.y.onx = c(0.1,0.17,0.24)+0.05, list.size = 12)
