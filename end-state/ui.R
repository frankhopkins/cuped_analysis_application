navbarPage(
  "Allow users to upload data | Time Tracker",
  tabPanel(
    "Upload data",
    fluidPage(
      wellPanel(p("Please download and complete a copy of the time trackers .xlsx file."),
                p("When finished, upload your file and click continue. Your file will be checked for errors")),
      fluidRow(
        column(
          downloadButton("download_time_tracker_template",
                         "Download template"),
          width = 2
        ),
        column(fileInput("uploaded_time_tracker",
                         label = "Upload your data"),
               width = 6)
      ),
      actionButton("anaylse_data",
                   "Analyse Data",
                   width = "100%"),
      plotOutput("time_tracker_summary")
    )
  ),
  collapsible = TRUE
)