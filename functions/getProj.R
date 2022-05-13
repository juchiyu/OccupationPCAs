## PCA.type     "general" (default), "cognitive", or "labour"
## data2proj    a data frame with a column with onet occupations
## onet.column  column name of the onet occupations

getProj <- function(data2proj, onet.column = "onet", PCA.type = "general", number.of.dimensions = 6){
  library(magrittr)
  library(tidyverse)
  library(pluralize)
  
  data2proj <- as.matrix(data2proj)
  data2proj[,onet.column] <- tolower(pluralize(data2proj[,onet.column]))
  
  if (PCA.type == "cognitive"){
    dataWproj <- OnetPCA.all$pca45[data2proj[,onet.column],c(1:number.of.dimensions)]
    infoWproj <- OnetPCA.all$occ.info[data2proj[,onet.column], c("rowclust_45", "Job.Zone", "Job.Family")]
  }else if (PCA.type == "labour"){
    dataWproj <- OnetPCA.all$pca123[data2proj[,onet.column],c(1:number.of.dimensions)]
    infoWproj <- OnetPCA.all$occ.info[data2proj[,onet.column], c("rowclust_123", "Job.Zone", "Job.Family")]
  }else{ # "general"
    dataWproj <- OnetPCA.all$pcaAll[data2proj[,onet.column],c(1:number.of.dimensions)]
    infoWproj <- OnetPCA.all$occ.info[data2proj[,onet.column], c("rowclust_all", "Job.Zone", "Job.Family")]
  }
  
  dataout <- data.frame(dataWproj, infoWproj)
  rownames(dataout) <- rownames(data2proj)
  
  return(dataout)
  
}
