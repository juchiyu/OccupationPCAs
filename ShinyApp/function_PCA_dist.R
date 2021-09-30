#Function by Soudeh
#Slight modification by Luke to fit with Shiny app

# Function to run PCA and calculate dist for prechosen job zones ----
PCAonJobZones <- function(zone, onet_data){
  
  # PCAonJobZones <- function(zone)
  # zone is a character object containing the desired zone (or zones separated by space).
  # PCAonJobZones function loads ONET data, selects jobs with desired zone(s) mentioned in zone input,
  # runs PCA, finds number of significant components, and calculates dist for rows and columns.
  # The function output has 4 objects: row.dist, column.dist, data, PCA (which is PCA result).
  
  
  # # Load Data ----
  # onet_data <- data <- read.csv("AKS-ONET-JZ-CAT-FEB2020.csv")
  # 
  # Select jobs with pre-chosen zones ----
  
  onet.zone.data <- subset(onet_data, onet_data$Job.Zone %in% zone)
  
  
  # Making data PCA ready 
  onet_data_pca <- onet.zone.data[, c(3:122)] # numerical values only
  rownames(onet_data_pca) <- onet.zone.data[,2]
  
  # Run PCA ----
  res.pcaInf <- InPosition::epPCA.inference.battery(onet_data_pca, scale = FALSE, center = TRUE, graphs = FALSE)
  
  # Number of significant components ----
  sig.comp <- sum(res.pcaInf$Inference.Data$components$p.vals<0.05)
  
  # Row Clustering ----
  D.row <- dist(res.pcaInf$Fixed.Data$ExPosition.Data$fi[,1:sig.comp], method = "euclidean")
  
  # Column Clustering ----
  D.col <- dist(res.pcaInf$Fixed.Data$ExPosition.Data$fj[,1:sig.comp], method = "euclidean")
  results <- list(D.row,D.col,onet.zone.data,res.pcaInf)
  names(results) <- c('row.dist','col.dist','data','PCA')
  return(results)
}