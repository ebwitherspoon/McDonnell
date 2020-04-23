# Install Packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "readxl", "irr", "shiny") 

# Import ELA and Math Data
df_ELA <- read_excel("~/Box Sync/McDonnell Teacher Learning Shared/Phase1_Coding_2020/Mechanisms Coding/Mechanisms_ELA_MASTER.xlsx")
df_MATH <- read_excel("~/Box Sync/McDonnell Teacher Learning Shared/Phase1_Coding_2020/Mechanisms Coding/Mechanisms_MATH_MASTER.xlsx")

# Stack and Clean ELA and Math 
df_full <- rbind(df_ELA, df_MATH) %>%
  drop_na(Content) %>%
  mutate(HiLo = `Hi/Lo`) %>%
  mutate(PrePost = fct_rev(`Pre/Post`)) %>%
  mutate(Simulation1 = replace_na(Simulation1, 0)) %>%
  mutate(Simulation2 = replace_na(Simulation2, 0)) %>%
  mutate(Subcode1 = (case_when(
    str_detect(Subcode1,"(?i)Eval*") ~ "Evaluation",
    str_detect(Subcode1, "(?i)Ambig*")  ~ "Ambiguity",
    str_detect(Subcode1, "(?i)Alte*")  ~ "Alternative",
    str_detect(Subcode1, "(?i)Relau*")  ~ "Relaunch",
    TRUE ~ Subcode1))) %>%
  mutate(Subcode2 = case_when(
    str_detect(Subcode2,"(?i)Eval*") ~ "Evaluation",
    str_detect(Subcode2, "(?i)Ambig*")  ~ "Ambiguity",
    str_detect(Subcode2, "(?i)Alte*")  ~ "Alternative",
    str_detect(Subcode2, "(?i)Relau*")  ~ "Relaunch",
    TRUE ~ Subcode2)) 

#### Descriptives and Graphs ####
# SHINY
# Define UI ####
ui <- fluidPage(
  titlePanel("Simulations by Group"),
  sidebarLayout(
    sidebarPanel(
      #Select Content
      selectInput(inputId = "content", 
                  label = "Content",
                  choices = unique(df_full$Content),
                  selected = "ELA"),
      #Select Variables as Groups
      selectInput(inputId = "col", 
                  label = "Variable1",
                  colnames(df_full), 
                  selected = "HiLo"),
      sliderInput("y.range", 
                  "Y Range", 
                  min=0, max=100, step=25, value=c(0,50))
      
    ),
    mainPanel(tabPanel(
      "Plot",
      fluidRow(
        plotOutput("mainPlot")
      )
    )))
  )


# Define Server ####
server <- function(input, output) {
  #Subset data based on the given course sequence
  #though we'll let ggplot do most of the summary later, we'll precalculate some of the means and standard errors to use for error bars
  chartData <- reactive({
    df_full %>%
      filter(Content == input$content) %>%
      group_by_at(input$col) %>%
      mutate(Sim_Pct = mean(Simulation1*100))
    })
  
  output$mainPlot <- renderPlot({
    ggplot(data=chartData(), 
           aes_string(x = "input$col", y = "Sim_Pct", fill = input$col)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") +
      geom_text(aes(label=round(Sim_Pct, digits = 2)), position = position_dodge(width = 1)) +
      coord_cartesian(ylim = input$y.range) 
  })
}

# Launch Shiny App ####
shinyApp(ui = ui, server = server)
