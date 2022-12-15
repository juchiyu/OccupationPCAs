# ONET Occupation Clusters Shiny App
# Authors: Luke Moraglia and Ju-Chi Yu


library(shiny)
library(shinyWidgets)
library(shinythemes)
library(ggplot2)
library(ggwordcloud)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(plotly)
library(ExPosition)
library(stringr)
library(shinyFeedback)
library(PTCA4CATA)

onet_data <- read.csv("AKS-ONET-JZ-CAT-FEB2020.csv")
source("function_PCA_dist.R")
# If this is updated in the main repo, it will need to be copied in to update here
load("data/from2_Dat4Plot.rda")
source("fi_plotly.R")

ui <- navbarPage(
    title = "Visualization of Latent Components Assessed in O*NET Occupations (VOLCANO)",
    theme = shinytheme("superhero"),
    
    tabPanel("The Paper",
             
             fluidRow(
                 column(2,
                        selectInput("pca", "Select job zones:",
                                    choices = list("All Job Zones PCA (General)" = "all",
                                                   "Job Zones 1-3 PCA" = "123",
                                                   "Job Zones 4-5 PCA" = "45"),
                                    width = '100%'
                                    ),
                        numericInput("comp1", "Horizontal Axis Component", 1,
                                     min = 1, max = 120, step = 1),
                        numericInput("comp2", "Vertical Axis Component", 2,
                                     min = 1, max = 120, step = 1),
                        checkboxInput("fi_means", "Plot means?", value = TRUE),
                        # Removing color choice as of 2021-10-28 until I can update the color vectors
                        # to reflect changes in group names
                        
                        # selectInput("color_scheme", "Select color scheme:",
                        #             choices = list("Green-Yellow" = '4',
                        #                            "By Categories" = '2',
                        #                            "Orange, Violet, Green" = '3',
                        #                            "By Clustering" = '1'),
                        #             width = '100%'
                        #             ),
                        downloadButton("downloadData", "Download", style = "width:100%"),
                        p("Download the results including factor scores and clusters", 
                          style = "font-size:85%"), #smaller?
                        hr(style = "height:5px"),
                        actionButton("showhelp", "Help", 
                                     icon = icon("question-circle")
                                    ),
                        actionButton("license", "Credits")
                        ),
                 column(1),
                 column(8,
                        h2("Occupations Factor Scores"),
                        plotlyOutput("fi_plot", height = "600px"),
                        br(),
                        h2("Job Traits Factor Scores"),
                        plotlyOutput("fj_plot", height = "600px")
                        ),
                 column(1)
             )
             
    ),
    tabPanel("The Sandbox",
             shinyFeedback::useShinyFeedback(),
             fluidRow(
                 column(4,
                        h4("Options"),
                        p("Which Job Zones?"),
                        checkboxGroupInput("whichJobZones",
                                           label = NULL,
                                           choices = list(1,
                                                          2,
                                                          3,
                                                          4,
                                                          5),
                                           inline = TRUE
                                           ),
                        checkboxInput("sand_fi_means", "Plot means?", value = TRUE),
                        p("Optionally, set a seed for reproducible K-means"),
                        numericInput("seed", 
                                     label = NULL,
                                     value = NULL,
                                     width = "25%"),
                        actionButton("runPCA", "Run PCA and clustering!", width = '100%'),
                        hr()
                        
                 ),
                 column(4,
                        h4("Occupation Cluster Options"),
                        sliderInput(inputId = "occu_sand_num_clus",
                                    label = "Number of clusters (K):",
                                    min = 6, max = 30, step = 1, value = 10),
                        p("How do you want to generate the clusters?"),
                        radioGroupButtons(inputId = "occu_clus_method",
                                          label = NULL,
                                          choices = c("Hierarchical", "K-means"),
                                          selected = "Hierarchical",
                                          status = "success"),
                        actionButton("sand_genmaps", 
                                     "Generate occupation cluster plots", width = '100%'),
                        hr(),
                        tableOutput("sand_occu_clus_table")
                 ), 
                column(4,
                       h4("Job Trait Cluster Options"),
                       sliderInput(inputId = "trt_sand_num_clus",
                                   label = "Number of clusters (K):",
                                   min = 6, max = 30, step = 1, value = 10),
                       p("How do you want to generate the clusters?"),
                       radioGroupButtons(inputId = "trt_clus_method",
                                         label = NULL,
                                         choices = c("Hierarchical", "K-means"),
                                         selected = "Hierarchical",
                                         status = "success"),
                       actionButton("sand_genmaps_trt", 
                                    "Generate trait cluster plots", width = '100%'),
                       hr(),
                       tableOutput("sand_trt_clus_table")
                  
                      )        
                        
                 
             ),
             fluidRow(
               column(3,
                      numericInput("sand_comp1", "Horizontal Axis Component", 1,
                                   min = 1, max = 120, step = 1)
               ),
               column(3,
                      numericInput("sand_comp2", "Vertical Axis Component", 2,
                                   min = 1, max = 120, step = 1)
               ),
               column(4,
                      downloadButton("sand_downloadData", "Download", style = "width:75%"),
                      p("Download the results including factor scores and clusters",
                        style = "font-size:85%")
                      ),
               column(2,
                      actionButton("sand_showhelp", "Help", 
                                   icon = icon("question-circle"))
               )
               
             ),
             hr(),
             fluidRow(
                 column(2),
                 column(8,
                        h2("Occupations Factor Scores"),
                        plotlyOutput("sand_fi_plot", height = 600)
                 ),
                 column(2)
             ),
             hr(),
             fluidRow(
               column(2),
               column(8,
                      h2("Job Traits Factor Scores"),
                      plotlyOutput("sand_fj_plot", height = 600)
               ),
               column(2)
             ),
             hr(),
             fluidRow(
                 column(2),
                 column(8,
                        h2("Occupations MDS Cluster Plots"),
                        uiOutput("sandplots")
                 ),
                 column(2)
             ),
             hr(),
             fluidRow(
               column(2),
               column(8,
                      h2("Job Traits MDS Cluster Plots"),
                      uiOutput("sandplots_trt")
               ),
               column(2)
             )
             
             
    )
)
    

server <- function(input, output) {
    pca_res <- reactive({
        switch(input$pca,
               "all" = pcares_all,
               "123" = pcares_123,
               "45" = pcares_45)
    })
    
    occu_clust <- reactive({
        switch(input$pca,
               "all" = occu.clust$all,
               "123" = occu.clust$jz123,
               "45" = occu.clust$jz45)
    })
    
    trt_clust <- reactive({
        switch(input$pca,
               "all" = trt.clust$all,
               "123" = trt.clust$jz123,
               "45" = trt.clust$jz45)
    })
    
    # Updates the component selectors to not exceed max number of components
    observeEvent(input$pca,{
         updateNumericInput(inputId = "comp1", value = 1, max = length(pca_res()$eigs))
         updateNumericInput(inputId = "comp2", value = 2, max = length(pca_res()$eigs))
                })
    
    output$fi_plot <- renderPlotly({
        fi_plotly(pca_res()$fi, 
                  occu_clust()$list, 
                  occu_clust()$col, 
                  input$fi_means,
                  axis1 = input$comp1,
                  axis2 = input$comp2)
    })
    
    output$fj_plot <- renderPlotly({
      fi_plotly(pca_res()$fj, 
                trt_clust()$list, 
                trt_clust()$col, 
                input$fi_means,
                axis1 = input$comp1,
                axis2 = input$comp2)
    })
    
    
    D.row <- reactive({
       switch(input$pca,
            "all" = D.row.all,
            "123" = D.row.123,
            "45" = D.row.45)
        
    })
    
    
    output$downloadData <- downloadHandler(
        filename = "VOLCANO.zip",
        content = function(file){
          #go to a temp dir to avoid permission issues
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          files <- NULL;
          
          # Occupations first
          file_name <- "Occupations.csv"
          factor_scores <- pca_res()$fi[rownames(occu_clust()$list),]
          colnames(factor_scores) <- paste("Score_Comp_", 1:ncol(factor_scores))
          df <- data.frame(Occupation = rownames(occu_clust()$list), 
                           `Occupation_Cluster` = occu_clust()$list,
                           factor_scores
                           )
          write.csv(df, file_name, row.names = FALSE, na = "")
          
          # Job Traits second
          file_name <- "Job_traits.csv"
          factor_scores <- pca_res()$fj[rownames(trt_clust()$list),]
          colnames(factor_scores) <- paste("Score_Comp_", 1:ncol(factor_scores))
          df <- data.frame(Job_Trait = rownames(trt_clust()$list), 
                           Trait_Cluster = trt_clust()$list,
                           factor_scores
          )
          write.csv(df, file_name, row.names = FALSE, na = "")
          
          #create the zip file
          zip(file,c("Occupations.csv", "Job_traits.csv"))
        }
    )
    
    
    
    
    observeEvent(input$showhelp,{
        showModal(modalDialog(
          title = "Help",
          p("This app runs Principal Components Analysis (PCA) on data from ONET. 
          The data are 966 occupations measured on 120 occupational trait variables. 
          ONET groups occupations into five \"Job Zones\" which reflect 
          \"levels of education, experience, and training necessary to perform the occupation.\"
          "),
          p("The current page (The Paper) contains interactive results from our
            upcoming paper about using PCA and hierarchical clustering on these data.
            See the next page (The Sandbox) to run your own PCA and clustering."),
          p("On the right are the PCA factor scores for the occupations,
            followed by the factor scores for the job traits.
            They are colored by their clusters from the hierarchical clustering.
            Cluster barycenters (group means) and names are displayed. Hover your mouse over
            the points to see the name of the occupation.
            "),
          p("On the left, you can select which job zones to include, and
            whether or not to plot cluster means.
            You can change which components are plotted using the Horizontal and 
            Vertical Axis Component selectors."),
          p("Clicking the Download button will save a .zip with .csv files containing 
            the occupations and job traits, their clusters, and their factor scores.")
                  )
          )
    })
    
    observeEvent(input$license,{
      showModal(modalDialog(
        title = "Credits",
        h4("Project authors"),
        p("Ju-Chi Yu, H. Moriah Sokolowski, Kirthana S. Rao, Luke E. Moraglia, Soudeh A. Khoubrouy, Hervé Abdi, and Brian Levine"),
        h4("Shiny App developers"),
        p("Luke E. Moraglia and Ju-Chi Yu"),
        h4("Source Code"),
        p("Source code for the analyses, Shiny app, and information about this project can be found in our ",
          a("GitHub repository", href = "https://github.com/juchiyu/OccupationPCAs"),
          "."),
        h4("License"),
        HTML('<p style="text-align: center">
        <a href="https://www.onetonline.org/">
        <img src="https://www.onetcenter.org/image/link/onet-in-it.svg" style="width: 130px; height: 60px; border: none" alt="O*NET in-it">
        </a></p>
          <p>This page includes information from 
        <a href="https://www.onetonline.org/">O*NET OnLine</a>
        by the U.S. Department of Labor, Employment and Training Administration (USDOL/ETA). 
        Used under the <a href="https://creativecommons.org/licenses/by/4.0/">CC BY 4.0</a> license. 
        O*NET&reg; is a trademark of USDOL/ETA. The authors have modified all or some of this information. 
        USDOL/ETA has not approved, endorsed, or tested these modifications.</p>')
      )
      )
    })
    
    
    # The Sandbox
    
    
    #will contain $row.dist, $col.dist, $data, $PCA
    sand_res <- eventReactive(input$runPCA, {
        JobsSelected <- !is.null(input$whichJobZones)
        if(!JobsSelected){
            showNotification("No Job Zone(s) Selected!", type = "error")
        }
        req(JobsSelected, cancelOutput = TRUE)
        notif <- showNotification("Running PCA and clustering...", duration = NULL,
                                  closeButton = FALSE)
        on.exit(removeNotification(notif), add = TRUE)
        func_res <- PCAonJobZones(input$whichJobZones, onet_data)
        
        return(func_res)
    })
    
    observeEvent(sand_res(),{
      updateNumericInput(inputId = "sand_comp1", value = 1, max = length(sand_res()$PCA$Fixed.Data$ExPosition.Data$eigs))
      updateNumericInput(inputId = "sand_comp2", value = 2, max = length(sand_res()$PCA$Fixed.Data$ExPosition.Data$eigs))
    })
    
    sand_occu_clus <- reactive({
        req(sand_res())
        if(input$occu_clus_method == "Hierarchical"){
          fit.r <- hclust(sand_res()$row.dist, method = "ward.D2")
          clus.grpR <- matrix(NA, nrow = length(fit.r$labels), ncol = 1, 
                              dimnames = list(c(fit.r$labels[fit.r$order]), paste0()))
          get.clus <- as.matrix(cutree(fit.r, k = input$occu_sand_num_clus))
          clus.grpR <- get.clus[rownames(clus.grpR),]
          clus.count <- table(clus.grpR)
        }
        else if(input$occu_clus_method == "K-means"){
          if(is.numeric(input$seed)){
            set.seed(input$seed)
          }
          reskmeans <- kmeans(sand_res()$row.dist, input$occu_sand_num_clus)
          clus.grpR <- reskmeans$cluster
          clus.count <- reskmeans$size
        }
        return(list(clus.grpR = clus.grpR, clus.count = clus.count))
    })
    
    sand_trt_clus <- reactive({
      req(sand_res())
      if(input$trt_clus_method == "Hierarchical"){
        fit.r <- hclust(sand_res()$col.dist, method = "ward.D2")
        clus.grpR <- matrix(NA, nrow = length(fit.r$labels), ncol = 1, 
                            dimnames = list(c(fit.r$labels[fit.r$order]), paste0()))
        get.clus <- as.matrix(cutree(fit.r, k = input$trt_sand_num_clus))
        clus.grpR <- get.clus[rownames(clus.grpR),]
        clus.count <- table(clus.grpR)
      }
      else if(input$trt_clus_method == "K-means"){
        if(is.numeric(input$seed)){
          set.seed(input$seed)
        }
        reskmeans <- kmeans(sand_res()$col.dist, input$trt_sand_num_clus)
        clus.grpR <- reskmeans$cluster
        clus.count <- reskmeans$size
      }
      return(list(clus.grpR = clus.grpR, clus.count = clus.count))
    })
    
    output$sand_occu_clus_table <- renderTable({
        table <- sand_occu_clus()$clus.count %>% matrix(nrow = 1) %>% data.frame
        colnames(table) <- paste0("C", 1:NCOL(table))
        return(table)
    })
    
    output$sand_trt_clus_table <- renderTable({
      table <- sand_trt_clus()$clus.count %>% matrix(nrow = 1) %>% data.frame
      colnames(table) <- paste0("C", 1:NCOL(table))
      return(table)
    })
    
    sand_occu_clus_colors <- reactive({
        prettyGraphsColorSelection(input$occu_sand_num_clus, starting.color = 8)
    })
    
    sand_trt_clus_colors <- reactive({
      prettyGraphsColorSelection(input$trt_sand_num_clus, starting.color = 46)
    })
    
    output$sand_fi_plot <- renderPlotly({
        fi <- sand_res()$PCA$Fixed.Data$ExPosition.Data$fi
        gc <- as.matrix(sand_occu_clus_colors())
        rownames(gc) <- 1:length(gc)
        gc.vec <- as.vector(gc)
        names(gc.vec) <- rownames(gc)
        fi_plotly(fi,
                  occu_clust_list = factor(sand_occu_clus()$clus.grpR[rownames(fi)]),
                  occu_clust_col = list(gc = gc, gc.vec = gc.vec),
                  plot_means = input$sand_fi_means,
                  axis1 = input$sand_comp1,
                  axis2 = input$sand_comp2
                  )
        
    })
    
    output$sand_fj_plot <- renderPlotly({
      fj <- sand_res()$PCA$Fixed.Data$ExPosition.Data$fj
      gc <- as.matrix(sand_trt_clus_colors())
      rownames(gc) <- 1:length(gc)
      gc.vec <- as.vector(gc)
      names(gc.vec) <- rownames(gc)
      fi_plotly(fj,
                occu_clust_list = factor(sand_trt_clus()$clus.grpR[rownames(fj)]),
                occu_clust_col = list(gc = gc, gc.vec = gc.vec),
                plot_means = input$sand_fi_means,
                axis1 = input$sand_comp1,
                axis2 = input$sand_comp2
      )
      
    })
    
    
    sand_plots_reactive <- eventReactive(input$sand_genmaps, {
        notif <- showNotification("Generating occupation cluster maps...", duration = 3, 
                                  closeButton = TRUE)
        
        lapply(1:input$occu_sand_num_clus, function(x){
            dat.clus <- subset(sand_occu_clus()$clus.grpR, sand_occu_clus()$clus.grpR == x)
            dis.mat.sub <- as.matrix(sand_res()$row.dist)[names(dat.clus),names(dat.clus)]
            resMDS <- epMDS(DATA = dis.mat.sub, 
                            DATA_is_dist = TRUE,
                            method = "euclidean",
                            graphs = FALSE)
            fi <- resMDS$ExPosition.Data$fi
            dist_to_center <- sqrt(rowSums(fi^2))
            close_names <- str_trunc(names(sort(dist_to_center)[1:3]), 50, "right")
            my_title <- paste(close_names, collapse = "; ")
            output[[paste0("sandplot", x)]] <- renderPlotly({
                plot_ly(x = fi[,1], y = fi[,2], type = "scatter",
                        mode = "markers", text = ~rownames(fi),
                        hoverinfo = 'text', color = I(sand_occu_clus_colors()[x]),
                        stroke = I("black"), size = ~ 1/(dist_to_center^4), #to 4th power to increase differences in marker size
                        sizes = c(10, 200), fill = '' #avoids spurious error about line.width
                ) %>% 
                    layout(title = list(text = my_title,
                                        font = list(size = 12)),
                            xaxis = list(title = list(text = "MDS Component 1"), 
                                         scaleanchor = "y", scaleratio = 1), #fixed ratio of the axes
                            yaxis = list(title = list(text = "MDS Component 2")),              
                            plot_bgcolor = "#ebebeb",
                            paper_bgcolor = "#ebebeb"
                    )
            })
            
        })
        plot_output_list <- lapply(1:input$occu_sand_num_clus, function(x){
            plot.name <- paste0("sandplot", x)
            plotlyOutput(plot.name, height = 600)
        })
        
        do.call(tagList, plot_output_list)
        
    })
    
    output$sandplots <- renderUI({
        sand_plots_reactive()
        
    })
    
    
    sand_plots_reactive_trt <- eventReactive(input$sand_genmaps_trt, {
      notif <- showNotification("Generating trait cluster maps...", duration = 3, 
                                closeButton = TRUE)
      
      lapply(1:input$trt_sand_num_clus, function(x){
        dat.clus <- subset(sand_trt_clus()$clus.grpR, sand_trt_clus()$clus.grpR == x)
        dis.mat.sub <- as.matrix(sand_res()$col.dist)[names(dat.clus),names(dat.clus)]
        resMDS <- epMDS(DATA = dis.mat.sub, 
                        DATA_is_dist = TRUE,
                        method = "euclidean",
                        graphs = FALSE)
        fi <- resMDS$ExPosition.Data$fi
        dist_to_center <- sqrt(rowSums(fi^2))
        close_names <- str_trunc(names(sort(dist_to_center)[1:3]), 50, "right")
        my_title <- paste(close_names, collapse = "; ")
        output[[paste0("sandplot_trt", x)]] <- renderPlotly({
          plot_ly(x = fi[,1], y = fi[,2], type = "scatter",
                  mode = "markers", text = ~rownames(fi),
                  hoverinfo = 'text', color = I(sand_trt_clus_colors()[x]),
                  stroke = I("black"), size = ~ 1/(dist_to_center^4), #to 4th power to increase differences in marker size
                  sizes = c(10, 200), fill = '' #avoids spurious error about line.width
          ) %>% 
            layout(title = list(text = my_title,
                                font = list(size = 12)
            ),
            plot_bgcolor = "#ebebeb",
            paper_bgcolor = "#ebebeb"
            )
        })
        
      })
      plot_output_list <- lapply(1:input$trt_sand_num_clus, function(x){
        plot.name <- paste0("sandplot_trt", x)
        plotlyOutput(plot.name, height = 600)
      })
      
      do.call(tagList, plot_output_list)
      
    })
    
    output$sandplots_trt <- renderUI({
      sand_plots_reactive_trt()
      
    })
    
    output$sand_downloadData <- downloadHandler(
      filename = "VOLCANO_sandbox.zip",
      content = function(file) {
        #go to a temp dir to avoid permission issues
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;
        
        # Occupations first
        file_name <- "Occupations.csv"
        factor_scores <- sand_res()$PCA$Fixed.Data$ExPosition.Data$fi[names(sand_occu_clus()$clus.grpR),]
        colnames(factor_scores) <- paste("Score_Comp_", 1:ncol(factor_scores))
        df <- data.frame(Occupation = names(sand_occu_clus()$clus.grpR), 
                         Occupation_Cluster = sand_occu_clus()$clus.grpR,
                         factor_scores)
        write.csv(df, file_name, row.names = FALSE, na = "")
        
        # Job Traits second
        file_name <- "Job_traits.csv"
        factor_scores <- sand_res()$PCA$Fixed.Data$ExPosition.Data$fj[names(sand_trt_clus()$clus.grpR),]
        colnames(factor_scores) <- paste("Score_Comp_", 1:ncol(factor_scores))
        df <- data.frame(Job_Trait = names(sand_trt_clus()$clus.grpR),
                         Trait_Cluster = sand_trt_clus()$clus.grpR,
                         factor_scores)
        write.csv(df, file_name, row.names = FALSE, na = "")
        
        #create the zip file
        zip(file,c("Occupations.csv", "Job_traits.csv"))
      }
    )
    
    observeEvent(input$sand_showhelp,{
      showModal(modalDialog(
        title = "Help",
        p("This app runs Principal Components Analysis (PCA) on data from ONET. 
          The data are 966 occupations measured on 120 occupational trait variables. 
          ONET groups occupations into five \"Job Zones\" which reflect 
          \"levels of education, experience, and training necessary to perform the occupation.\"
          "),
        p("The current page (The Sandbox) lets you run your own PCA and clustering.
          Select which job zones to include, and which type of clustering to perform for occupations
          and traits.
          Optionally, you can set an integer seed so that your K-Means clusters are reproducible.
          Then hit \"Run PCA and clustering!\" It may take a
          moment to run the analysis.
          The clustering uses the components that are significant based on a permutation test of the eigenvalues.
          Once clustering has finished, you can drag the \"Number of clusters (K)\"
          sliders to select how many clusters you would like to keep for the occupations and traits.
          The sizes of the clusters are displayed
          in tables, and PCA factor scores plots colored by cluster are displayed below.
          You can change which components are plotted using the Horizontal and Vertical Axis Component selectors.",
          span("Note: if you change your job zones selection, you must re-click the \"Run PCA and clustering!\" button.",
               style = "font-style:italic")),
        p("To look at each occupation or job trait cluster by itself, click on the \"Generate occupation/trait cluster plots\" buttons. 
          Below the PCA factor scores plots,
          an MDS factor scores plot will be displayed for each cluster. These MDS plots show how occupations or traits within a
          cluster are related to each other. The items closer to the center of the plot are more prototypical for
          the cluster, and are displayed as larger dots. The title of each plot are the three most prototypical
          items.")
        
      )
      )
    }
    )
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
