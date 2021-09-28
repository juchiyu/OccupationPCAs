#==== Summary ====
# This file reads the results of PCAs and clustering analyses.
# The PCAs includes one for all job zones, one for job zones 4-5, and one for job zones 1-3.
# + All job zones PCA (996 occupations):
#     - 7 significant components
#     - 18 row clusters
#     - 9 column clusters
# + Job zones 4 & 5 (391 occupations):
#     - 6 significant components
#     - 10 row clusters
#     - 10 column clusters
# + Job zones 1, 2, & 3 (575 occupations):
#     - 7 significant components
#     - 10 row clusters
#     - 9 column colusters

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
load("Results/from1_DatPcaClus.rda")

# ---- get.eigres ----
geteigres <- function(pca.inres){
  outlist <- list(eig = pca.inres$Fixed.Data$ExPosition.Data$eigs,
                  tau = pca.inres$Fixed.Data$ExPosition.Data$t,
                  pval = pca.inres$Inference.Data$components$p.vals)
  return(outlist)
}

eigres_all <- geteigres(pca_all)
eigres_45 <- geteigres(pca_45)
eigres_123 <- geteigres(pca_123)

# ---- get.res ----
pcares_all <- pca_all$Fixed.Data$ExPosition.Data
pcares_45 <- pca_45$Fixed.Data$ExPosition.Data
pcares_123 <- pca_123$Fixed.Data$ExPosition.Data

# ---- get.occu.clust ----
grp.r_all <- as.matrix(cutree(fit.r_all, k =18)) # 18 row clusters
grp.r_45 <- as.matrix(cutree(fit.r_45, k = 10)) # 10 row clusters
grp.r_123 <- as.matrix(cutree(fit.r_123, k = 10)) # 10 row clusters

# ---- get.trt.clust ----
grp.c_all <- as.matrix(cutree(fit.c_all, k = 9)) # 9 column clusters
grp.c_45 <- as.matrix(cutree(fit.c_45, k = 10)) # 10 column clusters
grp.c_123 <- as.matrix(cutree(fit.c_123, k = 9)) # 9 column clusters

# ---- occu.clusname ----
occu.clust <- list()

## colored by clustering results
occu.clust$all$names <- c('1' = "FinMedLglSpec",
                          '2' = "Service",
                          '3' = "FinMedLglManag",
                          '4' = "MedPrac",
                          '5' = "GovtComManag",
                          '6' = "TechOp",
                          '7' = "ArtsCom",
                          '8' = "Construction",
                          '9' = "SciEngi",
                          '10'= "Labor",
                          '11'= "EnviroSaftey",
                          '12'= "ObjectTech",
                          '13'= "SocSci",
                          '14'= "EnviroSustain",
                          '15'= "ITSpec",
                          '16'= "MathSpatial",
                          '17'= "BioTech",
                          '18'= "FinMedLglClerks")
occu.clust$jz45$names <- c('1' = "BusGovt",
                           '2' = "AltThrpy",
                           '3' = "SocSci",
                           '4' = "MedGen",
                           '5' = "SalesLog",
                           '6' = "Engi",
                           '7' = "Enviro",
                           '8' = "MedSpec",
                           '9' = "CompSci",
                           '10'= "SciMaths")
occu.clust$jz123$names <- c('1' = "Organizational",
                            '2' = "HealthTech",
                            '3' = "Repair&Maint",
                            '4' = "HeavyMach",
                            '5' = "Art",
                            '6' = "PublicSafe",
                            '7' = "Technician",
                            '8' = "Service",
                            '9' = "Clerk",
                            '10' = "Production")

occu.clust$all$list <- as.matrix(recode(grp.r_all, !!! occu.clust$all$names))
occu.clust$jz45$list <- as.matrix(recode(grp.r_45, !!! occu.clust$jz45$names))
occu.clust$jz123$list <- as.matrix(recode(grp.r_123, !!! occu.clust$jz123$names))
rownames(occu.clust$all$list) <- rownames(grp.r_all)
rownames(occu.clust$jz45$list) <- rownames(grp.r_45)
rownames(occu.clust$jz123$list) <- rownames(grp.r_123)

# ---- trt.clustname ----
trt.clust <- list()
trt.clust$all$names <- c('1' = "Physical",
                         '2' = "Sensory",
                         '3' = "Cognitive",
                         '4' = "Business",
                         '5' = "NatSci",
                         '6' = "EngTec",
                         '7' = "Communication",
                         '8' = "SocSci&Humanities",
                         '9' = "Oper&Cntrl")
trt.clust$jz45$names <- c('1' = "Physical",
                          '2' = "Oper&Cntrl",
                          '3' = "Cognitive",
                          '4' = "Sensory",
                          '5' = "Math",
                          '6' = "Communication",
                          '7' = "Business",
                          '8' = "NatSci",
                          '9' = "SciEng",
                          '10'= "SocSci&Humanities")
trt.clust$jz123$names <- c('1' = "Physical",
                           '2' = "BussMathCogn",
                           '3' = "Oper&Cntrl",
                           '4' = "Humanities",
                           '5' = "Strength",
                           '6' = "Sensory",
                           '7' = "Communication",
                           '8' = "Soc&HealthSci",
                           '9' = "EngTec")

trt.clust$all$list <- as.matrix(recode(grp.c_all, !!! trt.clust$all$names))
trt.clust$jz45$list <- as.matrix(recode(grp.c_45, !!! trt.clust$jz45$names))
trt.clust$jz123$list <- as.matrix(recode(grp.c_123, !!! trt.clust$jz123$names))
rownames(trt.clust$all$list) <- rownames(grp.c_all)
rownames(trt.clust$jz45$list) <- rownames(grp.c_45)
rownames(trt.clust$jz123$list) <- rownames(grp.c_123)

# ---- occu.col.clust ----
assigncol <- function(target.list, col4clust){
  # target.list: is the list where the col list (that includes gc.vec, gc, and oc) is stored
  # col4clust: is a vector of colors for each cluster. The names of these clusters are the names of these elements.
  col <- list()
  col$gc.vec <- col4clust
  col$gc <- col$gc.vec %>% as.matrix
  col$oc <- recode(target.list$list, !!!col$gc.vec) %>% as.matrix
  return(col)
}

## colored by clustering results ----
# occu.col4clust_all <- c(FinMedLglSpec = "chocolate2" , 
#                         Service       = "orange2",
#                         FinMedLglManag = "darkorange1",
#                         MedPrac       = "sienna3", 
#                         GovtComManag  = "mediumorchid3", 
#                         TechOp        = "orchid",
#                         ArtsCom       = "purple3",
#                         Construction  = "darkmagenta",
#                         SciEngi       = "darkorchid4",
#                         Labor         = "royalblue4",
#                         EnviroSaftey  = "royalblue3",
#                         ObjectTech    = "blue4",
#                         SocSci        = "dodgerblue2",
#                         EnviroSustain = "chartreuse4",
#                         ITSpec        = "olivedrab3",
#                         MathSpatial   = "mediumseagreen",
#                         BioTech       = "forestgreen",
#                         FinMedLglClerks = "darkgreen")
# occu.col4clust_jz45 <- c(BusGovt  = "mediumorchid3",
#                          AltThrpy = "purple3",
#                          SocSci   = "chocolate2",
#                          MedGen   = "sienna3",
#                          SalesLog = "orange2",
#                          Engi     = "darkgreen",
#                          Enviro   = "chartreuse4",
#                          MedSpec  = "forestgreen",
#                          CompSci  = "olivedrab3",
#                          SciMaths = "mediumseagreen")
# occu.col4clust_jz123 <- c(Organizational = "burlywood4",
#                           HealthTech     = "bisque3",
#                           `Repair&Maint` = "royalblue3",
#                           HeavyMach      = "royalblue4",
#                           Art            = "purple3",
#                           PublicSafe     = "mediumorchid",
#                           Technician     = "sienna3",
#                           Service        = "chocolate2",
#                           Clerk          = "orange2",
#                           Production     = "darkorange1")

## colored manually by categories ----
# occu.col4clust_all <- c(FinMedLglSpec = "chocolate2" ,
#                         Service       = "orange2",
#                         FinMedLglManag = "darkorchid4",
#                         MedPrac       = "sienna3",
#                         GovtComManag  = "mediumorchid3",
#                         TechOp        = "royalblue3",
#                         ArtsCom       = "purple3",
#                         Construction  = "dodgerblue2",
#                         SciEngi       = "darkgreen",
#                         Labor         = "darkblue",
#                         EnviroSaftey  = "orchid",
#                         ObjectTech    = "blue3",
#                         SocSci        = "darkmagenta",
#                         EnviroSustain = "chartreuse4",
#                         ITSpec        = "olivedrab3",
#                         MathSpatial   = "mediumseagreen",
#                         BioTech       = "forestgreen",
#                         FinMedLglClerks = "sandybrown")
# occu.col4clust_jz45 <- c(BusGovt  = "sienna3",
#                          AltThrpy = "mediumorchid3",
#                          SocSci   = "chocolate2",
#                          MedGen   = "purple3",
#                          SalesLog = "orange2",
#                          Engi     = "darkgreen",
#                          Enviro   = "chartreuse4",
#                          MedSpec  = "forestgreen",
#                          CompSci  = "olivedrab3",
#                          SciMaths = "mediumseagreen")
# occu.col4clust_jz123 <- c(Organizational = "purple3",
#                           HealthTech     = "forestgreen",
#                           `Repair&Maint` = "royalblue3",
#                           HeavyMach      = "blue3",#"royalblue4",
#                           Art            = "mediumorchid",
#                           PublicSafe     = "orchid",
#                           Technician     = "blue4",
#                           Service        = "chocolate2",
#                           Clerk          = "orange2",
#                           Production     = "dodgerblue2")


## simple colors occupations ----
# occu.col4clust_all <- c(FinMedLglSpec = "orange2" ,
#                         Service       = "orange2",
#                         FinMedLglManag = "orange2",
#                         MedPrac       = "orange2",
#                         GovtComManag  = "#952599", # violet
#                         TechOp        = "#952599",
#                         ArtsCom       = "#952599",
#                         Construction  = "#952599",
#                         SciEngi       = "#952599",
#                         Labor         = "chartreuse4", # green chartreuse4
#                         EnviroSaftey  = "chartreuse4",
#                         ObjectTech    = "chartreuse4",
#                         SocSci        = "chartreuse4",
#                         EnviroSustain = "chartreuse4",
#                         ITSpec        = "chartreuse4",
#                         MathSpatial   = "chartreuse4",
#                         BioTech       = "chartreuse4",
#                         FinMedLglClerks = "chartreuse4")
# occu.col4clust_jz45 <- c(BusGovt  = "#952599",
#                          AltThrpy = "#952599",
#                          SocSci   = "orange2",
#                          MedGen   = "orange2",
#                          SalesLog = "orange2",
#                          Engi     = "chartreuse4",
#                          Enviro   = "chartreuse4",
#                          MedSpec  = "chartreuse4",
#                          CompSci  = "chartreuse4",
#                          SciMaths = "chartreuse4")
# occu.col4clust_jz123 <- c(Organizational = "chartreuse4",
#                           HealthTech     = "chartreuse4",
#                           `Repair&Maint` = "chartreuse4",
#                           HeavyMach      = "chartreuse4",
#                           Art            = "#952599",
#                           PublicSafe     = "#952599",
#                           Technician     = "orange2",
#                           Service        = "orange2",
#                           Clerk          = "orange2",
#                           Production     = "orange2")

## Gradient colors from the color wheel ----
occu.colfunc <- colorRampPalette(c("forestgreen", "gold"))

getColorFromTheWheel <- function(f.scores, design, order.dim = 1, color.range = c("#006e00", "#efc900")){
  mean.f <- getMeans(f.scores, design)[,1:2]
  ordered.f <- mean.f[order(mean.f[,1]),]
  colfunc <- colorRampPalette(color.range)
  colvec <- colfunc(nrow(mean.f))
  names(colvec) <- rownames(ordered.f)
  return(colvec)
}

occu.col4clust_all <- getColorFromTheWheel(pcares_all$fi, occu.clust$all$list)
occu.col4clust_jz45 <- getColorFromTheWheel(pcares_45$fi, occu.clust$jz45$list)
occu.col4clust_jz123 <- getColorFromTheWheel(pcares_123$fi, occu.clust$jz123$list)

occu.clust$all$col <- assigncol(occu.clust$all, occu.col4clust_all)
occu.clust$jz45$col <- assigncol(occu.clust$jz45, occu.col4clust_jz45)
occu.clust$jz123$col <- assigncol(occu.clust$jz123, occu.col4clust_jz123)
rownames(occu.clust$all$col$oc) <- rownames(occu.clust$all$list)
rownames(occu.clust$jz45$col$oc) <- rownames(occu.clust$jz45$list)
rownames(occu.clust$jz123$col$oc) <- rownames(occu.clust$jz123$list)

# ---- trt.col.clust ----
## colored by names ----
# trt.col4clust_all <- c(Physical      = "steelblue2",
#                        Sensory       = "deepskyblue3",
#                        Cognitive     = "indianred3",
#                        Business      = "maroon",
#                        NatSci        = "gold",
#                        EngTec        = "yellow2",
#                        CommHumanities = "violetred3",
#                        `Social&PsychSci`     = "hotpink",
#                        `Oper&Cntrl`  = "cadetblue3")
# trt.col4clust_jz45 <- c(Physical      = "steelblue2",
#                         `Oper&Cntrl`  = "cadetblue3",
#                         Cognitive     = "indianred3",
#                         Sensory       = "deepskyblue3",
#                         Math          = "yellow2",
#                         Communication = "violetred3",
#                         Business      = "maroon",
#                         NatSci        = "gold",
#                         SciEng        = "gold2",
#                         `Social&PsychSci`     = "hotpink"
#                         )
# trt.col4clust_jz123 <- c(Physical      = "steelblue2",
#                          BussMathCogn  = "indianred3",
#                          `Oper&Cntrl`  = "cadetblue3",
#                          Humanities    = "firebrick",
#                          Strength      = "skyblue3",
#                          Sensory       = "deepskyblue3",
#                          Communication = "violetred3",
#                          `Social&HealthSci`     = "hotpink",
#                          EngTec        = "yellow2"
#                          )

## simple colors for trait clusters ----
# trt.col4clust_all <- c(Physical      = "royalblue2",
#                        Sensory       = "royalblue2",
#                        Cognitive     = "violetred3",
#                        Business      = "violetred3",
#                        NatSci        = "violetred3",
#                        EngTec        = "royalblue2",
#                        Communication = "violetred3",
#                        `SocSci&Humanities`     = "violetred3",
#                        `Oper&Cntrl`  = "royalblue2")
# trt.col4clust_jz45 <- c(Physical      = "royalblue2",
#                         `Oper&Cntrl`  = "royalblue2",
#                         Cognitive     = "violetred3",
#                         Sensory       = "violetred3",
#                         Math          = "royalblue2",
#                         Communication = "violetred3",
#                         Business      = "royalblue2",
#                         NatSci        = "royalblue2",
#                         SciEng        = "royalblue2",
#                         `SocSci&Humanities`     = "violetred3"
# )
# trt.col4clust_jz123 <- c(Physical      = "royalblue2",
#                          BussMathCogn  = "violetred3",
#                          `Oper&Cntrl`  = "royalblue2",
#                          Humanities    = "violetred3",
#                          Strength      = "royalblue2",
#                          Sensory       = "royalblue2",
#                          Communication = "violetred3",
#                          `Soc&HealthSci`     = "violetred3",
#                          EngTec        = "royalblue2"
# )

## Gradient colors from the color wheel ----
col.range = c("#00429d", "#cd3278") # blue - red
# col.range = c("#00429d", "#b6c6ff") # blue gradient

trt.col4clust_all <- getColorFromTheWheel(pcares_all$fj, trt.clust$all$list, color.range = col.range)
trt.col4clust_jz45 <- getColorFromTheWheel(pcares_45$fj, trt.clust$jz45$list, color.range = col.range)
trt.col4clust_jz123 <- getColorFromTheWheel(pcares_123$fj, trt.clust$jz123$list, color.range = col.range)

trt.clust$all$col <- assigncol(trt.clust$all, trt.col4clust_all)
trt.clust$jz45$col <- assigncol(trt.clust$jz45, trt.col4clust_jz45)
trt.clust$jz123$col <- assigncol(trt.clust$jz123, trt.col4clust_jz123)
rownames(trt.clust$all$col$oc) <- rownames(trt.clust$all$list)
rownames(trt.clust$jz45$col$oc) <- rownames(trt.clust$jz45$list)
rownames(trt.clust$jz123$col$oc) <- rownames(trt.clust$jz123$list)


save(eigres_123, eigres_45, eigres_all,
     pcares_all, pcares_123, pcares_45,
     occu.clust, trt.clust,
     fit.c_all, fit.c_123, fit.c_45,
     fit.r_all, fit.r_123, fit.r_45,
     onet_all, onet_123, onet_45,
     onet.jz_all, onet.jz_123, onet.jz_45,
     file = "Results/from2_Dat4Plot.rda")
