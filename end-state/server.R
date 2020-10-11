library("tidyverse")
library("readxl")
library("writexl")

function(input, output, session) {
  
  uploaded_time_tracker <- eventReactive(c(input$uploaded_time_tracker),
                                         {
                                           time_tracker <- read_excel(input$uploaded_time_tracker$datapath)
                                           
                                           time_tracker
                                         })
  
  output$download_time_tracker_template <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(), "_template-time-tracker.xlsx")
    },
    content = function(file){
      file.copy("data/template-time-tracker.xlsx", file)
    }
  )
  
  output$time_tracker_summary <- renderPlot({
    
    input$anaylse_data
    
    uploaded_time_tracker <- isolate(uploaded_time_tracker())
    
    uploaded_time_tracker %>%
      group_by(`project name`) %>%
      summarise(total_hours = sum(`hours worked`)) %>%
      ggplot(aes(x = fct_reorder(`project name`, total_hours),
                 y = total_hours)) +
      geom_col() +
      labs(x = "",
           y = "Total Hours Worked") +
      theme_minimal() +
      coord_flip()
    
  })
  
}