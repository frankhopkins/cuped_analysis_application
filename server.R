options(shiny.maxRequestSize=3000000000*1024^2) 

library("readxl")
library("writexl")
library("shiny")
library("ggridges")
library("pwr")
library("scales")
library("shinyWidgets")
library("shinycustomloader")
library("dplyr")
library("shinyBS")
library("ggplot2")
library("ggpubr")
library("ggthemes")
library("ggExtra")


function(input, output, session) {
  

### Make uploader reactive (Uploader) ###


upload_pre_exp_cov_data <- eventReactive(c(input$upload_pre_exp_cov_data),
                                        {
                                          data_download_1 <- read_excel(input$upload_pre_exp_cov_data$datapath)
                                          
                                          
                                        })

### Make uploader reactive (Uploader) ###


upload_exp_cup_data <- eventReactive(c(input$upload_exp_cup_data),
                                         {
                                           data_download_2 <- read_excel(input$upload_exp_cup_data$datapath)
                                           
                                           
                                         })


### Pre-experiment covariate template ###
  
  output$pre_exp_cov_data_template <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(), "_cov_test_data.xlsx")
    },
    content = function(cov_file){
      file.copy("data/cov_test_data.xlsx", cov_file)
    }
  )
  
### Experiment data template ###
  
  output$exp_data_template <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(), "_exp_cup_test_data.xlsx")
    },
    content = function(cov_file){
      file.copy("data/exp_cup_test_data.xlsx", cov_file)
    }
  )
  

### CUPED Correlation ###
  
  output$stats_cor_summary <- renderPlot({
    
    input$cor_plot
    
    upload_pre_exp_cov_data <- isolate(upload_pre_exp_cov_data())
    
    upload_exp_cup_data <- isolate(upload_exp_cup_data())
    
    covariate <- as.data.frame(upload_pre_exp_cov_data)
    
    metric <- as.data.frame(upload_exp_cup_data)
    
    cor_frame <- inner_join(covariate, metric, by = 'User_Id')
    
    plot <- cor_frame %>%  ggscatter(x = "Metric", y = "Covariate_Metric", color = "Variant.x",
              add = "reg.line",                                 # Add regression line
              conf.int = TRUE,                                  # Add confidence interval
              add.params = list(color = "blue",
                                fill = "lightgray"))+ 
      stat_cor(method = "pearson",  size = 4) + 
      theme_bw()+ 
      scale_fill_brewer(palette="Accent")
    
    ggMarginal(plot, type = "boxplot", groupFill = TRUE)
    
  })
    
### CUPED Analysis ###
    
    output$stats_cuped_summary <- renderPlot({
      
      input$box_plot_cuped
      
      remove_outliers <- function(df){
        
        y <- df[6][df[6] > 0] #remove any zero values
        dfNoOutliers<- df %>% filter(df[6]< 3*sd(y) + mean(y)) #remove any outlisers
        valsremaining <- length(dfNoOutliers)/length(df)
        valsremaining
        
        if (valsremaining < 0.95){
          stop ("This function will remove more than 5% percent of your data. You need to remove outliers manually.")}
        
        else if (valsremaining < 0.99){
          warning("This calculation has removed between 1% and 5% of your data.") 
          print(paste0(valsremaining*100, "% has been removed"))
        }
        else{
          print("Less than 1% of data has been removed")
        }
        return(dfNoOutliers)
        
      }
      
      upload_pre_exp_cov_data <- isolate(upload_pre_exp_cov_data())
      
      upload_exp_cup_data <- isolate(upload_exp_cup_data())
      
      covariate <- as.data.frame(upload_pre_exp_cov_data)
      
      metric <- as.data.frame(upload_exp_cup_data)
      
      cor_frame <- inner_join(covariate, metric, by = 'User_Id')
      
      covariance <- cov(cor_frame$Metric, cor_frame$Covariate_Metric)
        
      variance <- var(cor_frame$Covariate_Metric)
  
      theta <- covariance/variance
      
      cor_frame$CUPED_Adjusted_Metric <- cor_frame$Metric - (cor_frame$Covariate_Metric - mean(cor_frame$Covariate_Metric)) * theta
      
      cor_frame_filtered <- remove_outliers(cor_frame)
      
      cor_frame_filtered %>% ggplot(aes(x= Variant.x, y= CUPED_Adjusted_Metric, fill = Variant.x), annotations=p.adj) + 
        geom_boxplot()+
        stat_compare_means(method = "anova") + 
        theme_bw() + 
        scale_fill_brewer(palette="Accent")
        
      
    })
    
### Raw Data Table (Means / Variance Reduction) ###
    
    output$data_means_cuped <- renderTable({
      
      input$raw_data_CUPED
      
      remove_outliers <- function(df){
        
        y <- df[6][df[6] > 0] #remove any zero values
        dfNoOutliers<- df %>% filter(df[6]< 3*sd(y) + mean(y)) #remove any outlisers
        valsremaining <- length(dfNoOutliers)/length(df)
        valsremaining
        
        if (valsremaining < 0.95){
          stop ("This function will remove more than 5% percent of your data. You need to remove outliers manually.")}
        
        else if (valsremaining < 0.99){
          warning("This calculation has removed between 1% and 5% of your data.") 
          print(paste0(valsremaining*100, "% has been removed"))
        }
        else{
          print("Less than 1% of data has been removed")
        }
        return(dfNoOutliers)
        
      }
      
      upload_pre_exp_cov_data <- isolate(upload_pre_exp_cov_data())
      
      upload_exp_cup_data <- isolate(upload_exp_cup_data())
      
      covariate <- as.data.frame(upload_pre_exp_cov_data)
      
      metric <- as.data.frame(upload_exp_cup_data)
      
      cor_frame <- inner_join(covariate, metric, by = 'User_Id')
      
      covariance <- cov(cor_frame$Metric, cor_frame$Covariate_Metric)
      
      variance <- var(cor_frame$Covariate_Metric)
      
      theta <- covariance/variance
      
      cor_frame$CUPED_Adjusted_Metric <- cor_frame$Metric - (cor_frame$Covariate_Metric - mean(cor_frame$Covariate_Metric)) * theta
      
      cor_frame_filtered <- remove_outliers(cor_frame)
      
      means_cuped <- aggregate(CUPED_Adjusted_Metric ~  Variant.x, cor_frame_filtered, mean)
      
      sd_cuped <- aggregate(CUPED_Adjusted_Metric ~  Variant.x, cor_frame_filtered, sd)
      
      means_exp <- aggregate(Metric ~  Variant, metric, mean)
      
      sd_exp <- aggregate(Metric ~  Variant, metric, sd)
      
      names(means_exp)[2] <- "Mean_Metric"
      
      names(sd_exp)[2] <- "SD_Metric"
      
      names(means_cuped)[2] <- "Mean_CUPED_Adjusted"
      
      names(sd_cuped)[2] <- "SD_CUPED_Adjusted"
      
      binded <- cbind(means_exp, sd_exp, means_cuped, sd_cuped)
      
      filtered_select <- binded %>% select("Variant","Mean_Metric", "SD_Metric",
                   "Mean_CUPED_Adjusted", "SD_CUPED_Adjusted")
      
      filtered_select$SD_Exp_Metric <- filtered_select$SD_Metric / filtered_select$Mean_Metric
      
      filtered_select$SD_Cuped <- filtered_select$SD_CUPED_Adjusted / filtered_select$Mean_CUPED_Adjusted
  
      filtered_select$Percentage_Variance_Reduced <- (filtered_select$SD_Cuped - filtered_select$SD_Exp_Metric)/filtered_select$SD_Exp_Metric
      
      filtered_select <- filtered_select %>% select("Variant","Mean_Metric", "SD_Metric",
                                           "Mean_CUPED_Adjusted", "SD_CUPED_Adjusted", "Percentage_Variance_Reduced")
      
      filtered_select
})
    
### Raw Data Table (Comps) ###
    
    output$data_comps_cuped <- renderTable({
      
      input$raw_data_output_cuped
      
      remove_outliers <- function(df){
        
        y <- df[6][df[6] > 0] #remove any zero values
        dfNoOutliers<- df %>% filter(df[6]< 3*sd(y) + mean(y)) #remove any outlisers
        valsremaining <- length(dfNoOutliers)/length(df)
        valsremaining
        
        if (valsremaining < 0.95){
          stop ("This function will remove more than 5% percent of your data. You need to remove outliers manually.")}
        
        else if (valsremaining < 0.99){
          warning("This calculation has removed between 1% and 5% of your data.") 
          print(paste0(valsremaining*100, "% has been removed"))
        }
        else{
          print("Less than 1% of data has been removed")
        }
        return(dfNoOutliers)
        
      }
      
      upload_pre_exp_cov_data <- isolate(upload_pre_exp_cov_data())
      
      upload_exp_cup_data <- isolate(upload_exp_cup_data())
      
      covariate <- as.data.frame(upload_pre_exp_cov_data)
      
      metric <- as.data.frame(upload_exp_cup_data)
      
      cor_frame <- inner_join(covariate, metric, by = 'User_Id')
      
      covariance <- cov(cor_frame$Metric, cor_frame$Covariate_Metric)
      
      variance <- var(cor_frame$Covariate_Metric)
      
      theta <- covariance/variance
      
      cor_frame$CUPED_Adjusted_Metric <- cor_frame$Metric - (cor_frame$Covariate_Metric - mean(cor_frame$Covariate_Metric)) * theta

      cor_frame_filtered <- remove_outliers(cor_frame)
      
      means <- aggregate(CUPED_Adjusted_Metric ~  Variant.x, cor_frame_filtered, mean)
      
      data_comps <- compare_means(CUPED_Adjusted_Metric ~ Variant.x, data = cor_frame_filtered, method = "t.test")
      
      filtered_comps <- dplyr::select(data_comps, method , group1, group2, p, p.signif)
      
      filtered_comps
      
    })

}
