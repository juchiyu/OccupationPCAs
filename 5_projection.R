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
library(data4PCCAR)
library(factoextra)
library(dendextend)
library(stringr)
library(effectsize)
library(corrplot)
library(psych)
library(car)
library(dplyr)



func.dir <- ("./functions/")
sapply(paste0(func.dir,list.files(func.dir, pattern = "\\.[Rr]$")), source)


# ---- Read.results ----
load("Results/from2_Dat4Plot.rda")



# ---- Read data for projection ----
rockland <- read.csv("Data/rockland.cognitive.ses.onet.csv") # N = 996

rockland.filter <- rockland %>% 
  filter(
    head_injury == 0,
    stroke == 0,
    bipolar == 0,
    autism_asperger_pervasivedevdisorder == 0,
    adhd == 0,
    alzheimers == 0,
    epilepsy == 0,
    native_language == 1,
    FSIQ > 70 , # N = 808
    Job.Zone > 3,
    !is.na(wasi_vci_comp),
    !is.na(wasi_pri_comp)) # 507

rockland.filter$wasi_diff_comp <- rockland.filter$wasi_vci_comp - rockland.filter$wasi_pri_comp
rockland.filter$wasi_diff_grp <- ifelse(abs(rockland.filter$wasi_diff_comp) < 10, "Same", 
                                        ifelse(rockland.filter$wasi_diff_comp > 0, "VCI+", "PRI+"))


## Use wasi as design
wasi.grp.idx <- c("Same" = "grey60",
                  "VCI+" = "#F28E2B",
                  "PRI+" = "#B07AA1")
wasi.grp.col <- list()
wasi.grp.col$oc <- dplyr::recode(rockland.filter$wasi_diff_grp, !!!wasi.grp.idx) %>% as.matrix
wasi.grp.col$gc <- wasi.grp.idx %>% as.matrix

## Comparing Groups
#create new dataframe with only VCI+ and PRI+ people
wasi_2grp <- filter(rockland.filter, wasi_diff_grp != "Same")

#test if age if diff
t.test(age ~ wasi_diff_grp, wasi_2grp)

#test if gender is diff
chisq.test(wasi_2grp$sex_m1f2, wasi_2grp$wasi_diff_grp)

## Projection
load("Data/OccuPCA4proj_completeONET.rda")
Proj2Cognitive <- getProj(rockland.filter, onet.column = "onet", PCA.type = "cognitive", number.of.dimensions = 2)

## ============================= ##

## (project subject onto the occupation row space)
# data2plot <- rockland.filter[,c("dim1_45", "dim2_45")]
data2plot <- Proj2Cognitive[,c("Dimension.1", "Dimension.2")]
colnames(data2plot) <- c("Component 1", "Component 2")
proj.wasi.occu <- createFactorMap(data2plot,
                                  axis1 = 2, axis2 = 1,
                                  col.points = wasi.grp.col$oc,
                                  alpha.points = 0.1,
                                  cex = 5,
                                  col.background = NULL,
                                  alpha.axes = 0.5,
                                  col.axes = "#42376B",
                                  width.axes = 1,
                                  title = "Project the Rockland data onto\nthe Cognitive PCA space")


wasi.occu.boot <- Boot4Mean(data2plot, rockland.filter$wasi_diff_grp, niter = 1000)
wasi.occu.boot.gnd <- Boot4Mean(data2plot, paste0(rockland.filter$wasi_diff_grp, rockland.filter$sex_m1f2), niter = 1000)

proj.wasi.mean <- createFactorMap(wasi.occu.boot$GroupMeans[c(1,3),],
                                  axis1 = 2, axis2 = 1,
                                   col.points = wasi.grp.col$gc[rownames(wasi.occu.boot$GroupMeans[c(1,3),],),],
                                   col.labels = wasi.grp.col$gc[rownames(wasi.occu.boot$GroupMeans[c(1,3),],),],
                                   alpha.points = 1,
                                   pch = 17,
                                   cex = 8,
                                  text.cex = 10)

# proj.wasi.mean.gnd <- createFactorMap(wasi.occu.boot.gnd$GroupMeans[c(1:2,5:6),],
                                      # col.points = rep(wasi.grp.col$gc[rownames(wasi.occu.boot$GroupMeans)[c(1,3)],], each = 2),
                                      # col.labels = rep(wasi.grp.col$gc[rownames(wasi.occu.boot$GroupMeans)[c(1,3)],], each = 2),
                                      # alpha.points = 1,
                                      # pch = 1,
                                      # cex = 2,
                                      # text.cex = 3,
                                      # col.background = NULL,
                                      # col.axes = "#42376B",
                                      # width.axes = 1)

wasi.mean.ci <- MakeCIEllipses(wasi.occu.boot$BootCube[c(1,3),,], 
                               axis1 = 1, axis2 = 2,
                               names.of.factors = paste0("Component ", c(1:2)),
                               col = wasi.grp.col$gc[rownames(wasi.occu.boot$BootCube[c(1,3),,]),],
                               alpha.ellipse = 0.1,
                               line.size = 0.7)

# wasi.mean.gnd.ci <- MakeCIEllipses(wasi.occu.boot.gnd$BootCube[c(1:2,5:6),,],
#                                names.of.factors = paste0("Component ", c(1:2)),
#                                col = rep(wasi.grp.col$gc[rownames(wasi.occu.boot$BootCube[c(1,3),,]),], each = 2),
#                                alpha.ellipse = 0.1,
#                                line.size = 0.7)

Rockland.cog <- proj.wasi.occu$zeMap_background + proj.wasi.occu$zeMap_dots + wasi.mean.ci + 
  proj.wasi.mean$zeMap_dots + proj.wasi.mean$zeMap_text +
  xlab("Component 2\nHealth vs. Computational Science") +
  ylab("Componnet 1\nSTEM vs. Social Science and Humanities") + 
  theme(plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        plot.margin = unit(c(1,1,1,1), "mm"),
        text = element_text(size = 30))


Rockland.cog %<>% 
  arrangeGrob(top = textGrob(expression(bold("B")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

proj.wasi.mean.gnd$zeMap_background + proj.wasi.occu$zeMap_dots + wasi.mean.ci + #wasi.mean.gnd.ci +
  proj.wasi.mean$zeMap_dots + proj.wasi.mean$zeMap_text #+ proj.wasi.mean.gnd$zeMap_dots + proj.wasi.mean.gnd$zeMap_text



  
## Univariate Analyses
rockland.filter.new <- rockland.filter
rockland.filter.new[,c("dim1_45", "dim2_45")] <- data2plot

# write_csv(rockland.filter.new, "/Users/moriah/Dropbox/rockland.csv")
#LM predicting dim1 (STEM)

model_VCI_PRI_STEM = lm(dim1_45 ~ sex_m1f2 + wasi_vci_comp + wasi_pri_comp, data = rockland.filter.new)
model_VCI_PRI_STEM  %>% summary()
model_VCI_PRI_STEM  %>% effectsize()
model_VCI_PRI_STEM  %>% eta_squared()
model_VCI_PRI_STEM  %>% vif()

#bivariate correlations
cor(rockland.filter.new$dim1_45, rockland.filter.new$wasi_vci_comp, method = "pearson")


cor.test(rockland.filter.new$dim1_45, rockland.filter.new$wasi_pri_comp, method = "pearson")

#NEED TO COMPARE CORRELATIONS
rpri_vci <- cor(rockland.filter.new$wasi_vci_comp, rockland.filter.new$wasi_pri_comp, method = "pearson")

rdim1_vci <- cor(rockland.filter.new$dim1_45, rockland.filter.new$wasi_vci_comp, method = "pearson")
rdim1_pri <- cor(rockland.filter.new$dim1_45, rockland.filter.new$wasi_pri_comp, method = "pearson")
rpri_vci <- cor(rockland.filter.new$wasi_vci_comp, rockland.filter.new$wasi_pri_comp, method = "pearson")
(test <- r.test(470, r12 = rdim1_vci, r13 = rdim1_pri, r23 = rpri_vci))
# test %>% summary()

#Correlation of difference score
cor.test(rockland.filter.new$dim1_45, rockland.filter.new$wasi_diff_comp)

model_diff_STEM <- lm(dim1_45 ~ sex_m1f2 + wasi_diff_comp, data = rockland.filter.new)
model_diff_STEM %>% summary()
model_diff_STEM  %>% effectsize()
model_diff_STEM  %>% eta_squared()
model_diff_STEM  %>% vif()



# Scatterplot of VCI and PRI --> Dim1
scatter_dim1_wasi <-
  rockland.filter.new %>%
  select(dim1_45, wasi_vci_comp, wasi_pri_comp) %>%
  reshape2::melt(id = "dim1_45", measured = c("wasi_vci_comp", "wasi_pri_comp")) %>% 
  ggplot(aes(x = value, y = dim1_45, color = variable)) +
  geom_point(alpha = .1, position = "jitter", size = 5) +
  geom_smooth(method = "lm", size = 3) +
  stat_cor(method = "pearson", label.x = 65, label.y = c(0, -7), size = 10, show.legend = FALSE) +
  theme_bw() +
  labs(x = "WASI Composite Score", y = "Component 1\nSTEM vs. Social Science and Humanities") +
  scale_colour_manual(name = "WASI Domain",
                      values = c("#F28E2B","#B07AA1"),
                      labels = c("VCI", "PRI")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 30),
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        plot.margin = unit(c(1,1,1,1), "mm"),
        text = element_text(size = 30))
scatter_dim1_wasi

scatter_dim1_wasi %<>%
  arrangeGrob(top = textGrob(expression(bold("C")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

# Scatterplot of VCI>PRI --> Dim1
scatter_dim1_diff_wasi <-
  rockland.filter.new %>%
  select(dim1_45, wasi_diff_comp) %>%
  ggplot(aes(x = wasi_diff_comp, y = dim1_45)) +
  geom_point(alpha = .1, position = "jitter", size = 5) +
  geom_smooth(method = "lm", size = 3, color = "royalblue3") +
  stat_cor(method = "pearson", size = 10, show.legend = FALSE, label.y = 5) +
  theme_bw() +
  labs(x = "WASI Difference Score\n(VCI-PRI)", y = "Component 1\nSTEM vs. Social Science and Humanities") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 30),
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        plot.margin = unit(c(1,5,1,1), "mm"),
        text = element_text(size = 30))
scatter_dim1_diff_wasi

scatter_dim1_diff_wasi %<>%
  arrangeGrob(top = textGrob(expression(bold("D")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))




## Histogram
rockland.filter.new <- within(rockland.filter.new,
                              rowclust_45 <- factor(rowclust_45, 
                                                    levels= c("SocSci", "BusGovt", "AltThrpy",
                                                              "MedGen", "SalesLog", "CompSci",
                                                              "MedSpec", "SciMaths", "Enviro", "Engi")))

rockland.table <- rockland.filter.new %>% select(wasi_diff_grp, rowclust_45) %>% table
rockland.freq <- (rockland.table/rowSums(rockland.table))


rock.hist <- rockland.freq %>% as.data.frame %>% filter(wasi_diff_grp != "Same") %>%
  ggplot(aes(x = rowclust_45, y = Freq, fill = wasi_diff_grp)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = wasi.grp.col$gc[c(3,2)], name = "Cognitive groups") +
  theme_bw() +
  xlab("Occupation clusters") +
  ylab("Proportion of individuals in each group\nby row clusters") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 30),
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.text.x = element_text(size = 30, angle = 90, hjust = 1, vjust = 0.5, color = occu.clust$jz45$col$gc[levels(rockland.filter.new$rowclust_45),]),
        plot.margin = unit(c(10,1,1,1), "mm"),
        text = element_text(size = 30))
rock.hist %<>%
  arrangeGrob(top = textGrob(expression(bold("A")), x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"), gp=gpar(col="black", fontsize=40)))

#Correlations


#set-up df
df.corr <- rockland.filter.new %>% 
  select("age", "highest_grade", "dim1_45", "dim2_45", "Job.Zone", 
         "wasi_vocab_tscore", "wasi_similarities_tscore", "wasi_matrix_tscore", "wasi_blockdesign_tscore",
         "wiat_numop_standard", "wiat_wordreading_standard", "wiat_spelling_standard")
df.corr<- df.corr %>% 
  dplyr::rename("edu" = "highest_grade",
        "jobzone" = "Job.Zone",
        "vocab" = "wasi_vocab_tscore",
       "similarities" = "wasi_similarities_tscore",
       "matrix" = "wasi_matrix_tscore",
       "block" = "wasi_blockdesign_tscore",
       "math" = "wiat_numop_standard",
       "reading" = "wiat_wordreading_standard",
       "spelling" = "wiat_spelling_standard",
        "Dim1:STEM" = "dim1_45",
        "Dim2:Numeric_Health" = "dim2_45")

#df.reg.5 <- df.reg %>% 
#  filter(jobzone == 5)

#create dfs for correlation matrices jz45
df.corr <- df.corr %>% 
  select("jobzone", "edu", "Dim1:STEM", "Dim2:Numeric_Health", 
         "similarities", "vocab", "matrix", "block") %>% 
  as.matrix()

corr = cor(df.corr)
corrplot(corr, method = "shade", addCoef.col = "black", 
         type = "lower", title = "Rockland Correlations") 


## Correlation circle
# rockland.filter.45 <- rockland.filter.new %>% filter(Job.Zone > 3)
# 
# (cor.loadings.all <- cor(rockland.filter.new[,c("wasi_vci_comp", "wasi_pri_comp")], 
#                     rockland.filter.new[,c("dim1_45","dim2_45")]))
# (cor.loadings.45 <- cor(rockland.filter.45[,c("wasi_vci_comp", "wasi_pri_comp")], 
#                          rockland.filter.45[,c("dim1_45","dim2_45")]))
# 
# col4J <- c(wasi.grp.col$gc[2,], wasi.grp.col$gc[3,])
# ## (project variables onto the components)
# jolie.ggplot.J.Q <- createFactorMap(
#   cor.loadings.all,
#   col.points = col4J, 
#   col.labels = col4J,                   
#   constraints = list(minx = -1, miny = -1,
#                      maxx =  1, maxy =  1) )
# arrows.Q <- addArrows(cor.loadings.all, color = col4J)  
# b3.jolieggMap.J.Q <- jolie.ggplot.J.Q$zeMap_background + 
#   jolie.ggplot.J.Q$zeMap_text + 
#   arrows.Q +
#   addCircleOfCor()
# b3.jolieggMap.J.Q
# 
# jolie.ggplot.J.Q45 <- createFactorMap(
#   cor.loadings.45,
#   col.points = col4J, 
#   col.labels = col4J,                   
#   constraints = list(minx = -1, miny = -1,
#                      maxx =  1, maxy =  1) )
# arrows.Q45 <- addArrows(cor.loadings.45, color = col4J)  
# b3.jolieggMap.J.Q45 <- jolie.ggplot.J.Q45$zeMap_background + 
#   jolie.ggplot.J.Q45$zeMap_text + 
#   arrows.Q45 +
#   addCircleOfCor()
# b3.jolieggMap.J.Q45

## Output figures ====
png(filename =  "Figure6_202206-withHist3.png", width = 60, height = 60, units = "cm", bg = "white",res = 300)
grid.arrange(grobs = list(rock.hist, Rockland.cog, scatter_dim1_wasi, scatter_dim1_diff_wasi),
             widths = c(1, 0.1, 1),
             heights = c(1,1),
             layout_matrix = rbind(c(1,NA,2),
                                   c(3, NA, 4))) +
  theme(plot.margin = margin(2,2,2,2, "mm"))
dev.off()
