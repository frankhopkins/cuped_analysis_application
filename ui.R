library("tidyverse")
library("readxl")
library("writexl")
library("shiny")
library("ggridges")
library("pwr")
library("scales")
library("shinyWidgets")
library("shinycustomloader")
library("shinycssloaders")
library("shinyjs")
library("dplyr")
library("shinyBS")
library("ggplot2")

### CUPED Analysis Page ###
fluidPage(
  img(src='a_b_image.jpeg'),
  titlePanel(div(column(width = 6, h2("CUPED Parametric Analysis | Application  (V1)")))),
  setBackgroundColor(
  color = "lightblue"),
  wellPanel(
    fluidPage(),
    wellPanel(p("Methods that utilise controlled under pre-experiment data (CUPED) calculations aim to use pre-experiment information
                in order to minimise within metric variance, which in turn increases the sensitivity of your experiment.
                Incrseaing the statistical power/sensitivity of your experiments will mean that experiments can be concluded
                to statistical significance with greater conclusivity. CUPED is implemented on the notion that variance that 
                pre-experiment data can explain within a metric is not correlated to any effects elicited by the change in a 
                given experiment and can therefore be omitted from analysis."),br(),
    fluidRow( ),
    p("First, you need to find a pre-experiment metric that is highly correlated to your experimental metric. A high correlation 
    between the pre-experiment covariate and our experimental metric means that the variance that existed before the experiment can 
    be controlled for so we can create an adjusted metric. Below you can download a template that 
    you can fill in with your pre-experiment covariate (please ensure this is at cookie_unit level). It is also advised that you 
    use the same metric for your pre-experiment covariate and experimental metric to achieve a high correlation between variables:"),br(),
    column(
      downloadButton("pre_exp_cov_data_template",
                     "Pre-experiment Covariate Template"), width = 12),br(),br(),br(),
    fluidRow(),
    p("Now upload your filled out template back into the application:"),
    column(
    fileInput("upload_pre_exp_cov_data",
                 "Upload Pre-experiment Covariate Data"), width = 12),br(),br(),br(),
    fluidRow(),
    p("Now you can download a template for your experimental data. Fill this out with your data:"),br(),
    column(
      downloadButton("exp_data_template",
                     "Experiment Data Template"), width = 12),br(),br(),br(),
    fluidRow(),
    p("Now you can upload this back into the application. This data will be merged with the pre-experiment covariate information that
      you have uploaded; this will be used to compute your CUPED-Adjusted Metric, which will be used for subsequent analysis:"),
    column(
      fileInput("upload_exp_cup_data",
                "Upload Experiment Data"), width = 12),br(),br(),br(),br(),
    p("Below you can plot the correlation between your experimental metric and your pre-experiment covariate. An R^2 and p-value is
    is also provided so you can determine the strenth of the relationship:"),br(),
    bsButton("cor_plot",
             "Calculate/Plot Correlation",
             width = "100%",
             size = "large",
             type = "toggle",
             style = "default"),
    withSpinner(plotOutput("stats_cor_summary"), type = 6, color = "#4C0099"),
    bsTooltip(id = "cor_plot", title = "Merges data-frames and calculates correlation betweem pre-experiment covariate and experiment metric",
              placement = "centre", trigger = "hover"),br(),
    p("Below you can now plot the results of your analysis of variance (ANOVA). This button also merges your uploaded data above and calculates
    the constant (theta) that is used to control the within metric variance that existed prior to the experiment.The below ANOVA will be performed 
      on your CUPED-adjusted metric. Performing this action also cleans your data (removing outliers +/- 3 standard deviatiations
      from the CUPED-adjusted metric's overall mean):"),br(),
    bsButton("box_plot_cuped",
             "Box Plot (CUPED-Adjusted Metric)",
             width = "100%",
             size = "large",
             type = "toggle",
             style = "default"),
    withSpinner(plotOutput("stats_cuped_summary"), type = 6, color = "#4C0099"),
    bsTooltip(id = "box_plot_cuped", title = "Merges data-frames, computes CUPED-adjusted metric and calculates significance",
              placement = "centre", trigger = "hover"),br(),
    p("Below you can create a table of the mean and standard deviation of your experimental metric and pre-experiment covariate, respectively.
      The furthest right row of this table exhibits the amount of relative variance that has been omitted for your CUPED-adjusted metric:"),br(),
    bsButton("raw_data_CUPED", "Show Group Averages", width ="100%"),br(),br(),
    tableOutput("data_means_cuped"),
    p("You can also view the pairwise comparisons (repeated t-tests) between each individual variant used in your experiment.
      This helpes you identify not only if a given variant has beaten your control condition but also statistical differences between
      all other perumutations:"),br(),
    bsButton("raw_data_output_cuped", "Statistical Output", width = "100%"),br(),
    tableOutput("data_comps_cuped"),br(),
    p("* ns = < 95% sig, * = 95% sig, ** = 99% sig, *** = 99.99% sig")
    )
)
)




