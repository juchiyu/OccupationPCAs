# Create plotly graph of fi points

fi_plotly <- function(fi_to_plot, #matrix of n rows, 2 columns 
                      occu_clust_list = NULL, #vector with n entries of cluster each belongs to
                      occu_clust_col = NULL, #list with gc and gc.vec
                      plot_means = TRUE,
                      clustering = TRUE
){
  if(clustering){
    opac <- 0.5
    if(!plot_means){
      opac <- 1
    }
    fi_all <- data.frame(fi_to_plot, cluster = occu_clust_list)
    gc.vec <- as.vector(occu_clust_col$gc)
    names(gc.vec) <- dimnames(occu_clust_col$gc)[[1]]
    
    fi_all_means_mat <- PTCA4CATA::getMeans(fi_to_plot, occu_clust_list)
    fi_all_means <- data.frame(fi_all_means_mat, clust_name = rownames(fi_all_means_mat))
    
    p <- plot_ly(data = fi_all, x = ~X1, y = ~X2, type = "scatter",
            mode = "markers", text = ~rownames(fi_all),
            hoverinfo = 'text', color = ~cluster, colors = gc.vec,
            opacity = opac) %>% 
      layout(
        plot_bgcolor = "#ebebeb",
        paper_bgcolor = "#ebebeb",
        legend = list(orientation = "h", y = -0.2),
        xaxis = list(title = list(text = "Component 1")),
        yaxis = list(title = list(text = "Component 2"))
                      
      )
    
    if(plot_means){
      p <- p %>% add_trace(data = fi_all_means, x = ~V1, y = ~V2, type = "scatter",
                mode = "markers", text = ~rownames(fi_all_means),
                hoverinfo = 'none', color = ~clust_name, colors = occu_clust_col$gc.vec,
                opacity = 1,
                marker = list(size = 15,
                              symbol = I('triangle-up'),
                              line = list(color = "black",
                                          width = 1)
                ),
                showlegend = FALSE
                #name = "means"
      ) %>% 
        add_text(x = fi_all_means$V1 + 0.5, y = fi_all_means$V2,
                 text = rownames(fi_all_means),
                 color = I("black"),
                 opacity = 1,
                 textposition = "top right",
                 name = "Cluster Names",
                 hoverinfo = "none")
    }
  }
  else{
    opac <- 1
    if(is.null(occu_clust_col)){
      occu_clust_col <- "darkorchid4"
    }
    else if(length(occu_clust_col) > 1){
      stop("occu_clust_col is in the wrong format.")
    }
    fi_all <- data.frame(fi_to_plot)
    p <- plot_ly(data = fi_all, x = ~X1, y = ~X2, type = "scatter",
                 mode = "markers", text = ~rownames(fi_all),
                 hoverinfo = 'text', color = I(occu_clust_col),
                 opacity = opac) %>% 
      layout(
        plot_bgcolor = "#ebebeb",
        paper_bgcolor = "#ebebeb",
        legend = list(orientation = "h", y = -0.2),
        xaxis = list(title = list(text = "Component 1")),
        yaxis = list(title = list(text = "Component 2"))
        
      )
  }
  p   
}