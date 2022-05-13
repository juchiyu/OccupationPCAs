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

# vertical.label <- c("Top", "Bottom")

# fi.tab_all.cp1 <- TopBottomTable(pcares_all$fi, occu.clust$all$col)
# fi.tab_all.cp2 <- TopBottomTable(pcares_all$fi, occu.clust$all$col, leDim = 2, row.label = vertical.label)
# fi.tab_all.cp3 <- TopBottomTable(pcares_all$fi, occu.clust$all$col, leDim = 3, row.label = vertical.label)
# 
# fj.tab_all.cp1 <- TopBottomTable(pcares_all$fj, trt.clust$all$col, column.name = "Job traits")
# fj.tab_all.cp2 <- TopBottomTable(pcares_all$fj, trt.clust$all$col, column.name = "Job traits", leDim = 2, row.label = vertical.label)
# fj.tab_all.cp3 <- TopBottomTable(pcares_all$fj, trt.clust$all$col, column.name = "Job traits", leDim = 3, row.label = vertical.label)
# 
# fi.tab_jz45.cp1 <- TopBottomTable(pcares_45$fi, occu.clust$jz45$col)
# fi.tab_jz45.cp2 <- TopBottomTable(pcares_45$fi, occu.clust$jz45$col, leDim = 2, row.label = vertical.label)
# fi.tab_jz45.cp3 <- TopBottomTable(pcares_45$fi, occu.clust$jz45$col, leDim = 3, row.label = vertical.label)
# 
# fj.tab_jz45.cp1 <- TopBottomTable(pcares_45$fj, trt.clust$jz45$col, column.name = "Job traits")
# fj.tab_jz45.cp2 <- TopBottomTable(pcares_45$fj, trt.clust$jz45$col, column.name = "Job traits", leDim = 2, row.label = vertical.label)
# fj.tab_jz45.cp3 <- TopBottomTable(pcares_45$fj, trt.clust$jz45$col, column.name = "Job traits", leDim = 3, row.label = vertical.label)
# 
# fi.tab_jz123.cp1 <- TopBottomTable(pcares_123$fi, occu.clust$jz123$col)
# fi.tab_jz123.cp2 <- TopBottomTable(pcares_123$fi, occu.clust$jz123$col, leDim = 2, row.label = vertical.label)
# fi.tab_jz123.cp3 <- TopBottomTable(pcares_123$fi, occu.clust$jz123$col, leDim = 3, row.label = vertical.label)
# 
# fj.tab_jz123.cp1 <- TopBottomTable(pcares_123$fj, trt.clust$jz123$col, column.name = "Job traits")
# fj.tab_jz123.cp2 <- TopBottomTable(pcares_123$fj, trt.clust$jz123$col, column.name = "Job traits", leDim = 2, row.label = vertical.label)
# fj.tab_jz123.cp3 <- TopBottomTable(pcares_123$fj, trt.clust$jz123$col, column.name = "Job traits", leDim = 3, row.label = vertical.label)


# ---- job.zone.plots ----
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

jbplt_all <- get.FacPlot(eigres_all, pcares_all$fi, design = onet.jz_all, jb.col_all, xaxis = 1, yaxis = 2, title = "Occupations by Job Zones") %>%
  arrangeGrob(top = textGrob(expression(bold("A")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
jbplt_all_23 <- get.FacPlot(eigres_all, pcares_all$fi, design = onet.jz_all, jb.col_all, xaxis = 1, yaxis = 3, title = "Occupations by Job Zones") %>%
  arrangeGrob(top = textGrob(expression(bold("A")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
jbplt_all_34 <- get.FacPlot(eigres_all, pcares_all$fi, design = onet.jz_all, jb.col_all, xaxis = 3, yaxis = 4, title = "Occupations by Job Zones") %>%
  arrangeGrob(top = textGrob(expression(bold("A")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

jbplt_jz45 <- get.FacPlot(eigres_45, pcares_45$fi, design = onet.jz_45, jb.col_45, xaxis = 1, yaxis = 2, title = "Occupations by Job Zones") %>%
  arrangeGrob(top = textGrob(expression(bold("A")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
# jbplt_jz45_23 <- get.FacPlot(eigres_45, pcares_45$fi, design = onet.jz_45, jb.col_45, xaxis = 3, yaxis = 2, title = "Occupations by job zones")

jbplt_jz123 <- get.FacPlot(eigres_123, pcares_123$fi, design = onet.jz_123, jb.col_123, xaxis = 1, yaxis = 2, title = "Occupations by Job Zones") %>%
  arrangeGrob(top = textGrob(expression(bold("A")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
# jbplt_jz123_23 <- get.FacPlot(eigres_123, pcares_123$fi, design = onet.jz_123, jb.col_123, xaxis = 3, yaxis = 2, title = "Occupations by job zones")

# ---- fi.plots ----
fi.point.size = 6; fi.text.size = 8; title.size = 30

fiplt_all <- get.FacPlot(eigres_all, pcares_all$fi, design = occu.clust$all$list, occu.clust$all$col, xaxis = 1, yaxis = 2,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = TRUE, 
                         str4wrap = 30, list.xmin.scale = 0.2, list.scale.y.ony = c(0.2,0,-0.2), list.scale.y.onx = c(0.1,0.16,0.23)+0.05, list.scale.ymin = 0.02, list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
fiplt_all_23 <- get.FacPlot(eigres_all, pcares_all$fi, design = occu.clust$all$list, occu.clust$all$col, xaxis = 1, yaxis = 3,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = TRUE,
                            str4wrap = 22, list.scale.y.ony = c(0.30,0,-0.30), list.scale.y.onx = c(0.1,0.16,0.22)+0.08, list.scale.ymin = 0.03, list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

fiplt_all_34 <- get.FacPlot(eigres_all, pcares_all$fi, design = occu.clust$all$list, occu.clust$all$col, xaxis = 3, yaxis = 4,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = TRUE, 
                            str4wrap = 22, list.scale.y.ony = c(0.30,0,-0.30), list.scale.y.onx = c(0.1,0.16,0.22)+0.08, list.scale.ymin = 0.03, list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

fiplt_jz45 <- get.FacPlot(eigres_45, pcares_45$fi, design = occu.clust$jz45$list, occu.clust$jz45$col, xaxis = 1, yaxis = 2,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = TRUE, 
                          str4wrap = 30, list.ybase = 3, list.xmin.scale = 0.2, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.scale.ymin = 0.03, list.scale.y.ony = c(0.25,0,-0.25), list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
# fiplt_jz45_23 <- get.FacPlot(eigres_45, pcares_45$fi, design = occu.clust$jz45$list, occu.clust$jz45$col, xaxis = 1, yaxis = 3,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title = "Occupation clusters")
# 
# fiplt_jz45_34 <- get.FacPlot(eigres_45, pcares_45$fi, design = occu.clust$jz45$list, occu.clust$jz45$col, xaxis = 3, yaxis = 4,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = TRUE, 
#                              str4wrap = 30, list.ybase = 3, list.xmin.scale = 0.2, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.scale.ymin = 0.03, list.scale.y.ony = c(0.25,0,-0.25), list.size = 12) %>%
#   arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

fiplt_jz123 <- get.FacPlot(eigres_123, pcares_123$fi, design = occu.clust$jz123$list, occu.clust$jz123$col, xaxis = 1, yaxis = 2,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = TRUE, 
                           str4wrap = 30, list.xmin.scale = 0.2, list.scale.ymin = 0.03, list.scale.y.onx = c(0.1,0.18,0.26)+0.05, list.scale.y.ony = c(0.15,0,-0.15), list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
# fiplt_jz123_23 <- get.FacPlot(eigres_123, pcares_123$fi, design = occu.clust$jz123$list, occu.clust$jz123$col, xaxis = 1, yaxis = 3,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title = "Occupation clusters")


# fiplt_jz123_34 <- get.FacPlot(eigres_123, pcares_123$fi, design = occu.clust$jz123$list, occu.clust$jz123$col, xaxis = 3, yaxis = 4,cex.mean.point = fi.point.size, cex.mean.text = fi.text.size, title.size = title.size, title = "Occupation clusters", print.list = TRUE, 
#                               str4wrap = 30, list.xmin.scale = 0.2, list.scale.ymin = 0.03, list.scale.y.onx = c(0.1,0.18,0.26)+0.05, list.scale.y.ony = c(0.15,0,-0.15), list.size = 12) %>%
#   arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

# ---- fj.plots ----
fj.point.size = 8; fj.text.size = 10

fjplt_all <- get.FacPlot(eigres_all, pcares_all$fj, design = trt.clust$all$list, trt.clust$all$col, xaxis = 1, yaxis = 2,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = TRUE, list.title = "traits", 
                         str4wrap = 30, list.ybase = 24, list.scale.y.ony = c(0.15,0,-0.15), list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.scale.ymin = 0.05, list.size = 12)  %>%
  arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
fjplt_all_23 <- get.FacPlot(eigres_all, pcares_all$fj, design = trt.clust$all$list, trt.clust$all$col, xaxis = 1, yaxis = 3,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = TRUE, list.title = "traits",
                            str4wrap = 30, list.ybase = 10, list.xmin.scale = 0.2, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.scale.ymin = 0.03, list.scale.y.ony = c(0.15,0,-0.15), list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

fjplt_all_34 <- get.FacPlot(eigres_all, pcares_all$fj, design = trt.clust$all$list, trt.clust$all$col, xaxis = 3, yaxis = 4,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = TRUE, list.title = "traits", 
                            str4wrap = 30, list.ybase = 10, list.xmin.scale = 0.2, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.scale.ymin = 0.03, list.scale.y.ony = c(0.15,0,-0.15), list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

fjplt_jz45 <- get.FacPlot(eigres_45, pcares_45$fj, design = trt.clust$jz45$list, trt.clust$jz45$col, xaxis = 1, yaxis = 2,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = TRUE, list.title = "traits", 
                          list.ybase = 10, list.xmin.scale = 0.2, list.scale.y.ony = c(0.15,0,-0.15), list.scale.ymin = 0.04, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
# fjplt_jz45_23 <- get.FacPlot(eigres_45, pcares_45$fj, design = trt.clust$jz45$list, trt.clust$jz45$col, xaxis = 1, yaxis = 3,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title = "Job trait clusters")

# fjplt_jz45_34 <- get.FacPlot(eigres_45, pcares_45$fj, design = trt.clust$jz45$list, trt.clust$jz45$col, xaxis = 3, yaxis = 4,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = TRUE, list.title = "traits", 
#                              list.ybase = 10, list.xmin.scale = 0.2, list.scale.y.ony = c(0.15,0,-0.15), list.scale.ymin = 0.04, list.scale.y.onx = c(0.1,0.16,0.22)+0.05, list.size = 12) %>%
#   arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

fjplt_jz123 <- get.FacPlot(eigres_123, pcares_123$fj, design = trt.clust$jz123$list, trt.clust$jz123$col, xaxis = 1, yaxis = 2,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = TRUE, list.title = "traits",
                           str4wrap = 25, list.ybase = 5, list.scale.ymin = 0.03, list.scale.y.ony = c(0.20,0,-0.20), list.scale.y.onx = c(0.1,0.17,0.24)+0.05, list.size = 12) %>%
  arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))
# fjplt_jz123_23 <- get.FacPlot(eigres_123, pcares_123$fj, design = trt.clust$jz123$list, trt.clust$jz123$col, xaxis = 1, yaxis = 3,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title = "Job trait clusters")

# fjplt_jz123_34 <- get.FacPlot(eigres_123, pcares_123$fj, design = trt.clust$jz123$list, trt.clust$jz123$col, xaxis = 3, yaxis = 4,cex.mean.point = fj.point.size, cex.mean.text = fj.text.size, TiLab = TRUE, title.size = title.size, title = "Trait clusters", print.list = TRUE, list.title = "traits", 
#                               str4wrap = 25, list.ybase = 5, list.scale.ymin = 0.03, list.scale.y.ony = c(0.20,0,-0.20), list.scale.y.onx = c(0.1,0.17,0.24)+0.05, list.size = 12) %>%
#   arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

# ---- merge.plots ----
## Figure 2
### horizontal
# png(filename =  "Figure2-CW_202109-test.png", width = 120, height =70, units = "cm", bg = "white",res = 300)
# grid.arrange(grobs = list(jbplt_all, jbplt_all_23, 
#                           fiplt_all, fjplt_all, fiplt_all_23,fjplt_all_23),
#              widths = c(0.5, 0.3, 1.5, 0.3, 0.3, 1.5, 0.3),
#              heights = c(0.3,rep(1, 3), 0.4, rep(1,3), 0.5),
#              layout_matrix = rbind(c(NA, NA, NA, NA, NA, NA, NA),
#                c(1, NA, 3, NA, NA, 4, NA),
#                c(1, NA, 3, NA, NA, 4, NA),
#                c(1, NA, 3, NA, NA, 4, NA),
#                c(NA, NA, NA, NA, NA, NA, NA),
#                c(2, NA, 5, NA, NA, 6, NA),
#                c(2, NA, 5, NA, NA, 6, NA),
#                c(2, NA, 5, NA, NA, 6, NA),
#                c(NA, NA, NA, NA, NA, NA, NA)))+
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
# dev.off()

### vertical
png(filename =  "Figure2a-CW_202203.png", width = 70, height =100, units = "cm", bg = "white",res = 300)
grid.arrange(grobs = list(jbplt_all, fiplt_all, fjplt_all),
             widths = c(0.2, 1.5,0.2),
             heights = c(0.8, 0.3, 1.5, 0.3, 1.5, 0.3),
             layout_matrix = rbind(c(NA, 1, NA),
                                   c(NA, NA, NA),
                                   c(NA, 2, NA),
                                   c(NA, NA, NA),
                                   c(NA, 3, NA),
                                   c(NA, NA, NA)))+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
dev.off()

# ## vertical
png(filename =  "Figure2b-CW_202203.png", width = 70, height =100, units = "cm", bg = "white",res = 300)
grid.arrange(grobs = list(jbplt_all_23, fiplt_all_23, fjplt_all_23),
             widths = c(0.2, 1.5,0.2),
             heights = c(0.8, 0.3, 1.5, 0.3, 1.5, 0.3),
             layout_matrix = rbind(c(NA, 1, NA),
                                   c(NA, NA, NA),
                                   c(NA, 2, NA),
                                   c(NA, NA, NA),
                                   c(NA, 3, NA),
                                   c(NA, NA, NA)))+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
dev.off()

## Components 3 & 4
png(filename =  "Figure2c-CW_202203.png", width = 70, height =100, units = "cm", bg = "white",res = 300)
grid.arrange(grobs = list(jbplt_all_34, fiplt_all_34, fjplt_all_34),
             widths = c(0.2, 1.5,0.2),
             heights = c(0.8, 0.3, 1.5, 0.3, 1.5, 0.3),
             layout_matrix = rbind(c(NA, 1, NA),
                                   c(NA, NA, NA),
                                   c(NA, 2, NA),
                                   c(NA, NA, NA),
                                   c(NA, 3, NA),
                                   c(NA, NA, NA)))+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
dev.off()


## Figure 3
### horizontal
# png(filename =  "Figure3-CW_0824.png", width = 120, height = 40, units = "cm", bg = "white",res = 300)
# grid.arrange(grobs = list(jbplt_jz45, fiplt_jz45, fjplt_jz45),
#              widths = c(0.5, 0.3, 1.5, 0.3, 0.3, 1.5, 0.3),
#              # heights = c(0.4,rep(0.33, 3)),
#              layout_matrix = rbind(c(1, NA, 2, NA, NA, 3, NA),
#                                    c(1, NA, 2, NA, NA, 3, NA),
#                                    c(1, NA, 2, NA, NA, 3, NA),
#                                    c(NA, NA, NA, NA, NA, NA, NA)))+
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
# dev.off()

### vertical
png(filename =  "Figure3-CW_202203.png", width = 70, height =100, units = "cm", bg = "white",res = 300)
grid.arrange(grobs = list(jbplt_jz45, fiplt_jz45, fjplt_jz45),
             widths = c(0.2, 1.5,0.2),
             heights = c(0.8, 0.3, 1.5, 0.3, 1.5, 0.3),
             layout_matrix = rbind(c(NA, 1, NA),
                                   c(NA, NA, NA),
                                   c(NA, 2, NA),
                                   c(NA, NA, NA),
                                   c(NA, 3, NA),
                                   c(NA, NA, NA)))+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
dev.off()

## Figure 4
### horizontal
# png(filename =  "Figure4-CW_0824.png", width = 120, height = 45, units = "cm", bg = "white",res = 300)
# grid.arrange(grobs = list(jbplt_jz123, fiplt_jz123, fjplt_jz123),
#              widths = c(0.5, 0.3, 1.5, 0.3, 0.3, 1.5, 0.3),
#              # heights = c(rep(0.33, 3),0.4),
#              layout_matrix = rbind(c(1, NA, 2, NA, NA, 3, NA),
#                                    c(1, NA, 2, NA, NA, 3, NA),
#                                    c(1, NA, 2, NA, NA, 3, NA),
#                                    c(NA, NA, NA, NA, NA, NA, NA)))+
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
# dev.off()

### vertical
png(filename =  "Figure4-CW_202203.png", width = 70, height =100, units = "cm", bg = "white",res = 300)
grid.arrange(grobs = list(jbplt_jz123, fiplt_jz123, fjplt_jz123),
             widths = c(0.3, 1.5,0.3),
             heights = c(0.8, 0.3, 1.5, 0.3, 1.5, 0.3),
             layout_matrix = rbind(c(NA, 1, NA),
                                   c(NA, NA, NA),
                                   c(NA, 2, NA),
                                   c(NA, NA, NA),
                                   c(NA, 3, NA),
                                   c(NA, NA, NA)))+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
dev.off()

# # ---- merge.plots (with tables) ----
# ## Figure 2
# png(filename =  "Figure2-simplet.png", width = 120, height = 65, units = "cm", bg = "white",res = 300)
# grid.arrange(grobs = list(jbplt_all, jbplt_all_23, 
#                           fiplt_all, fjplt_all, fiplt_all_23,fjplt_all_23,
#                           fi.tabplt_all.cp1, fi.tabplt_all.cp2, fi.tabplt_all.cp3,
#                           fj.tabplt_all.cp1, fj.tabplt_all.cp2, fj.tabplt_all.cp3),
#              widths = c(0.6, 1, 1, 1, 0.8),
#              heights = c(0.6,rep(0.33, 5)),
#              layout_matrix = rbind(c(NA, 3, 7, 4, 10),
#                                    c(1, 3, 7, 4, 10),
#                                    c(1, 3, 8, 4, 11),
#                                    c(2, 5, 8, 6, 11),
#                                    c(2, 5, 9, 6, 12),
#                                    c(NA, 5, 9, 6, 12)))+
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
# dev.off()
# 
# ## Figure 3
# png(filename =  "Figure3-simple.png", width = 120, height = 45, units = "cm", bg = "white",res = 300)
# grid.arrange(grobs = list(jbplt_jz45, fiplt_jz45, fjplt_jz45, 
#                           fi.tabplt_jz45.cp1, fi.tabplt_jz45.cp2, 
#                           fj.tabplt_jz45.cp1, fj.tabplt_jz45.cp2),
#              widths = c(0.6, 1, 1, 1, 0.8),
#              heights = c(0.4,rep(0.33, 3)),
#              layout_matrix = rbind(c(NA, 2, 4, 3, 6),
#                                    c(1, 2, 4, 3, 6),
#                                    c(1, 2, 5, 3, 7),
#                                    c(NA, 2, 5, 3, 7)))+
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
# dev.off()
# 
# ## Figure 4
# png(filename =  "Figure4-simple.png", width = 120, height = 50, units = "cm", bg = "white",res = 300)
# grid.arrange(grobs = list(jbplt_jz123, fiplt_jz123, fjplt_jz123, 
#                           fi.tabplt_jz123.cp1, fi.tabplt_jz123.cp2, 
#                           fj.tabplt_jz123.cp1, fj.tabplt_jz123.cp2),
#              widths = c(0.6, 1, 1, 1, 0.8),
#              heights = c(rep(0.33, 3),0.4),
#              layout_matrix = rbind(c(NA, 2, 4, 3, 6),
#                                    c(1, 2, 4, 3, 6),
#                                    c(1, 2, 5, 3, 7),
#                                    c(NA, 2, 5, 3, 7)))+
#   theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
# dev.off()

# # ---- plot.tables ----
# 
# plot.tab <- function(tab, show.n = 7, size = 20, main.title = "Table"){
#   tab2plot <- rbind(tab$top[1:show.n,], tab$bottom[rev(1:show.n),])
#   color2plot <- tab2plot$color
#   
#   plot.all <- ggtexttable(tab2plot[,2:1], theme = ttheme(
#     rownames.style = rownames_style(face = "bold", size = size,
#                                     vjust = 1, y = 0.9),
#     colnames.style = colnames_style(fill = "white", size = size),
#     tbody.style = tbody_style(fill = "white", color = color2plot, size = size, 
#                               hjust = rep(c(0,1),each = show.n*2), x=rep(c(0.1, 0.9), each = show.n*2)))) %>%
#     tab_add_title(text = main.title, face = "bold", size = size) %>%
#     tab_add_hline(at.row = 1+c(1, 2), row.side = "top", linewidth = 2) %>%
#     tab_add_hline(at.row = show.n*2 + 2, row.side = "bottom", linewidth = 2) %>%
#     tab_add_hline(at.row = show.n + 2, row.side = "bottom", linecolor = "grey70", linewidth = 2) 
#   # plot.top <- ggtexttable(tab$top[,2:1], theme = ttheme(
#   #   rownames.style = rownames_style(face = "bold", size = size),
#   #   colnames.style = colnames_style(fill = "white", size = size),
#   #   tbody.style = tbody_style(fill = "white", color = top.color, size = size, hjust = rep(c(0,1),each = show.n), x=rep(c(0.1, 0.9), each = show.n)))) %>%
#   #   tab_add_hline(at.row = c(1, 2), row.side = "top") %>%
#   #   tab_add_hline(at.row = show.n + 1, row.side = "bottom")
#   # plot.bottom <- ggtexttable(tab$bottom[,2:1], theme = ttheme(
#   #   rownames.style = rownames_style(face = "bold", size = size),
#   #   colnames.style = colnames_style(fill = "white", size = size),
#   #   tbody.style = tbody_style(fill = "white", color = bottom.color, size = size, hjust = rep(c(0,1),each = show.n), x=rep(c(0.1, 0.9), each = show.n)))) %>%
#   #   tab_add_hline(at.row = c(1, 2), row.side = "top") %>%
#   #   tab_add_hline(at.row = show.n + 1, row.side = "bottom")
#   # 
#   #   plot <- ggarrange(plot.top, plot.bottom, 
#   #                         nrow = 1)+
#   #   theme(plot.margin = margin(0,0,0,0, "cm"))
#     
#     return(plot.all+
#              theme(plot.margin = margin(0.5,0,0,0, "cm"))
#              )
# }
# 
# ## all job zones
# fi.tabplt_all.cp1 <- plot.tab(fi.tab_all.cp1, main.title = "Component 1")
# fi.tabplt_all.cp2 <- plot.tab(fi.tab_all.cp2, main.title = "Component 2")
# fi.tabplt_all.cp3 <- plot.tab(fi.tab_all.cp3, main.title = "Component 3")
# 
# fj.tabplt_all.cp1 <- plot.tab(fj.tab_all.cp1, main.title = "Component 1")
# fj.tabplt_all.cp2 <- plot.tab(fj.tab_all.cp2, main.title = "Component 2")
# fj.tabplt_all.cp3 <- plot.tab(fj.tab_all.cp3, main.title = "Component 3")
# 
# ## job zones 4 and 5
# fi.tabplt_jz45.cp1 <- plot.tab(fi.tab_jz45.cp1, main.title = "Component 1")
# fi.tabplt_jz45.cp2 <- plot.tab(fi.tab_jz45.cp2, main.title = "Component 2")
# fi.tabplt_jz45.cp3 <- plot.tab(fi.tab_jz45.cp3, main.title = "Component 3")
# 
# fj.tabplt_jz45.cp1 <- plot.tab(fj.tab_jz45.cp1, main.title = "Component 1")
# fj.tabplt_jz45.cp2 <- plot.tab(fj.tab_jz45.cp2, main.title = "Component 2")
# fj.tabplt_jz45.cp3 <- plot.tab(fj.tab_jz45.cp3, main.title = "Component 3")
# 
# ## job zones 1, 2, and 3
# fi.tabplt_jz123.cp1 <- plot.tab(fi.tab_jz123.cp1, main.title = "Component 1")
# fi.tabplt_jz123.cp2 <- plot.tab(fi.tab_jz123.cp2, main.title = "Component 2")
# fi.tabplt_jz123.cp3 <- plot.tab(fi.tab_jz123.cp3, main.title = "Component 3")
# 
# fj.tabplt_jz123.cp1 <- plot.tab(fj.tab_jz123.cp1, main.title = "Component 1")
# fj.tabplt_jz123.cp2 <- plot.tab(fj.tab_jz123.cp2, main.title = "Component 2")
# fj.tabplt_jz123.cp3 <- plot.tab(fj.tab_jz123.cp3, main.title = "Component 3")
# 
# # ---- ci.max.tab ----
# order.fi_all <- order(pcares_all$fi[,1])
# order.ficol_all <- occu.clust$all$col$oc[order.fi_all,]
# 
# fimax_all <- pcares_all$fi[order.fi_all,1] %>% tail(10) %>% round(2)
# fimax.name_all <- names(fimax_all) %>% str_wrap(30)
# fimax.col_all <- order.ficol_all %>% tail(10)
# 
# tab <- cbind(fimax_all, fimax.name_all, fimax.col_all)
# rownames(tab) <- paste0("top", c(1:10))
# 
# ggtexttable(tab, rows = NULL, theme = ttheme("blank")) %>% 
#   tab_add_hline(at.row = c(1,11),
#                 row.side = c("bottom"),linewidth = 2)
# 
# 
# tabplt <- gridExtra::tableGrob(tab)
# tabplt$layout$
# grid.table(tab, theme = ttheme("light"))
# 
# pcares_all$fi[order(pcares_all$fi[,1]),1] %>% head(10)
# 
# # ---- ci.barpots ----
# ci.font.size = 5; cj.font.size = 3
# 
# get.BarPlot <- function(pcares, f = "fi", col.list, design = NULL, cp2plot = 1, horizontal = TRUE, no.label = FALSE, ylim = NULL,
#                         label.size = 20, font.size = 10, threshold.col = "white", threshold.size = 1, signifOnly = TRUE, ...){
#   if (f == "fi"){
#     cci <- getMeans(pcares$ci, design) %>% as.matrix
#     sign.cci <- getMeans(pcares$fi, design) %>% as.matrix
#     signed.ctri <- cci*sign(sign.cci)
#     color2use <- col.list$gc[rownames(signed.ctri),]
#     threshold = 0
#   }else if(f == "fj"){
#     signed.ctri <- pcares$cj * sign(pcares$fj)  
#     color2use <- col.list$oc
#     threshold <- 0
#     }
#   
#   plot <- PrettyBarPlot2(
#     bootratio = (100*signed.ctri[,cp2plot]), 
#     threshold = threshold, 
#     font.size = font.size,
#     horizontal = horizontal,
#     ylim = ylim,
#     line.col = threshold.col,line.size = threshold.size,
#     color4bar = gplots::col2hex(color2use),
#     color4ns = "gray75", 
#     plotnames = FALSE,
#     signifOnly = signifOnly,
#     # main = 'Contributions barplot career cluster Dim 1.', 
#     ylab = "Signed Contributions", ...) + 
#     theme(text = element_text(size = label.size))
#   if(no.label){
#     plot <- plot + theme(axis.text = element_blank())
#   }
#   return(plot)
# }
# 
# fi.bar_all.1 <- get.BarPlot(pcares_all, f = "fi", occu.clust$all$col, design = occu.clust$all$list, horizontal = FALSE, threshold.size = 0, font.size = ci.font.size, main = "Component 1")
# fi.bar_all.2 <- get.BarPlot(pcares_all, f = "fi", occu.clust$all$col, design = occu.clust$all$list, cp2plot = 2, horizontal = TRUE, threshold.size = 0, font.size = ci.font.size, main = "Component 2")
# fi.bar_all.3 <- get.BarPlot(pcares_all, f = "fi", occu.clust$all$col, design = occu.clust$all$list, cp2plot = 3, horizontal = FALSE, threshold.size = 0, font.size = ci.font.size, main = "Component 3")
# 
# fi.bar_jz45.1 <- get.BarPlot(pcares_45, f = "fi", occu.clust$jz45$col, design = occu.clust$jz45$list, horizontal = FALSE, threshold.size = 0, font.size = ci.font.size, main = "Component 1")
# fi.bar_jz45.2 <- get.BarPlot(pcares_45, f = "fi", occu.clust$jz45$col, design = occu.clust$jz45$list, cp2plot = 2, horizontal = TRUE, threshold.size = 0, font.size = ci.font.size, main = "Component 2")
# fi.bar_jz45.3 <- get.BarPlot(pcares_45, f = "fi", occu.clust$jz45$col, design = occu.clust$jz45$list, cp2plot = 3, horizontal = FALSE, threshold.size = 0, font.size = ci.font.size, main = "Component 3")
# 
# fi.bar_jz123.1 <- get.BarPlot(pcares_123, f = "fi", occu.clust$jz123$col, design = occu.clust$jz123$list, horizontal = FALSE, threshold.size = 0, font.size = ci.font.size, main = "Component 1")
# fi.bar_jz123.2 <- get.BarPlot(pcares_123, f = "fi", occu.clust$jz123$col, design = occu.clust$jz123$list, cp2plot = 2, horizontal = TRUE, threshold.size = 0, font.size = ci.font.size, main = "Component 2")
# fi.bar_jz123.3 <- get.BarPlot(pcares_123, f = "fi", occu.clust$jz123$col, design = occu.clust$jz123$list, cp2plot = 3, horizontal = FALSE, threshold.size = 0, font.size = ci.font.size, main = "Component 3")
# 
# # ---- cj.barplots ----
# 
# fj.bar_all.1 <- get.BarPlot(pcares_all, f = "fj", trt.clust$all$col, horizontal = FALSE, font.size = cj.font.size, ylim = c(-2.5,5), main = "Component 1")
# fj.bar_all.2 <- get.BarPlot(pcares_all, f = "fj", trt.clust$all$col, cp2plot = 2, horizontal = TRUE, font.size = cj.font.size, ylim = c(-7,8), main = "Component 2")
# fj.bar_all.3 <- get.BarPlot(pcares_all, f = "fj", trt.clust$all$col, cp2plot = 3, horizontal = FALSE, font.size = cj.font.size, ylim = c(-7,13), main = "Component 3")
# 
# fj.bar_jz45.1 <- get.BarPlot(pcares_45, f = "fj", trt.clust$jz45$col, horizontal = FALSE, font.size = cj.font.size, ylim = c(-5,10), main = "Component 1")
# fj.bar_jz45.2 <- get.BarPlot(pcares_45, f = "fj", trt.clust$jz45$col, cp2plot = 2, horizontal = TRUE, font.size = cj.font.size, ylim = c(-7,15), main = "Component 2")
# fj.bar_jz45.3 <- get.BarPlot(pcares_45, f = "fj", trt.clust$jz45$col, cp2plot = 3, horizontal = FALSE, font.size = cj.font.size, main = "Component 3")
# 
# fj.bar_jz123.1 <- get.BarPlot(pcares_123, f = "fj", trt.clust$jz123$col, horizontal = FALSE, font.size = cj.font.size, ylim = c(-5,10), main = "Component 1")
# fj.bar_jz123.2 <- get.BarPlot(pcares_123, f = "fj", trt.clust$jz123$col, cp2plot = 2, horizontal = TRUE, font.size = cj.font.size, ylim = c(-5,6), main = "Component 2")
# fj.bar_jz123.3 <- get.BarPlot(pcares_123, f = "fj", trt.clust$jz123$col, cp2plot = 3, horizontal = FALSE, font.size = cj.font.size, ylim = c(-7,7.5), main = "Component 3")

