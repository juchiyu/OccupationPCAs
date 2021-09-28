#==== Summary ====
# This file reads the data and runs all PCAs and clustering analyses.
# The PCAs includes one for all job zones, one for job zones 4-5, and one for job zones 1-3.
# From each PCA, we selected the components with significant eigenvalues and performed 
# clustering analyses on their row and column factor scores. 
# The results are saved to ./Results/from1_DatPcaClus.rda

# Clean up ----
rm(list = ls())
graphics.off()

# Packages ----
library(devtools)
library(readxl)
library(ExPosition)
library(InPosition)
library(ggplot2)
library(PTCA4CATA)
library(dplyr)
library(data4PCCAR)

# Read data ----
onet_data <- read.csv("Data\\AKS-ONET-JZ-CAT-FEB2020.csv")

# Prepping ----
## remove demographics and categorical data
onet_all <- onet_data[, c(3:122)]
rownames(onet_all) <- onet_data[,2]

# get data for different job zones ----
onet_45 <- subset(onet_all, onet_data$Job.Zone >= 4)
onet_123 <- subset(onet_all, onet_data$Job.Zone < 4)

# get job zones ----
onet.jz_all <- onet_data$Job.Zone
onet.jz_45 <- subset(onet_data$Job.Zone, onet_data$Job.Zone >= 4)
onet.jz_123 <- subset(onet_data$Job.Zone, onet_data$Job.Zone < 4)

names(onet.jz_all) <- rownames(onet_data)
names(onet.jz_45) <- rownames(onet_data[onet_data$Job.Zone >= 4,])
names(onet.jz_123) <- rownames(onet_data[onet_data$Job.Zone < 4,])

# run PCAs ----
## all job zones
pca_all <- epPCA.inference.battery(onet_all, scale = FALSE, center = TRUE, graphs = FALSE)
## job zones 45
pca_45 <- epPCA.inference.battery(onet_45, scale = FALSE, center = TRUE, graphs = FALSE)
## job zones 123
pca_123 <- epPCA.inference.battery(onet_123, scale = FALSE, center = TRUE, graphs = FALSE)

# clustering analysis ----
## all job zones
fit.r_all <- dist(pca_all$Fixed.Data$ExPosition.Data$fi[,1:7], method = "euclidean") %>% hclust(method = "ward.D2")
fit.c_all <- dist(pca_all$Fixed.Data$ExPosition.Data$fj[,1:7], method = "euclidean") %>% hclust(method = "ward.D2")
## job zones 45
fit.r_45 <- dist(pca_45$Fixed.Data$ExPosition.Data$fi[,1:6], method = "euclidean") %>% hclust(method = "ward.D2")
fit.c_45 <- dist(pca_45$Fixed.Data$ExPosition.Data$fj[,1:4], method = "euclidean") %>% hclust(method = "ward.D2")
fit.c_45_6fac <- dist(pca_45$Fixed.Data$ExPosition.Data$fj[,1:6], method = "euclidean") %>% hclust(method = "ward.D2")
## job zones 123
fit.r_123 <- dist(pca_123$Fixed.Data$ExPosition.Data$fi[,1:7], method = "euclidean") %>% hclust(method = "ward.D2")
fit.c_123 <- dist(pca_123$Fixed.Data$ExPosition.Data$fj[,1:7], method = "euclidean") %>% hclust(method = "ward.D2")

# save results ----
rm(list=lsf.str()) # remove functions
save.image(file = "Results/from1_DatPcaClus.rda")
