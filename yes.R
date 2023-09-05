library(shiny) 
library(dplyr) 
library(tidyverse)
library(tidyr)

preconc_longer <- preconc_new 
data2 <- preconc_longer 

data2 <- pivot_longer(preconc_new,
               cols = ends_with("Cost"),
               names_to = "Cost_Type", 
               values_to = "Cost") 

data2 <- data2[-c(1,2,3,6,7,8),]
##Building user interface 


ui <- dashboardPage( 
  dashboardHeader(title = "Preconstruction Analytics"),
  dashboardSidebar(
    sidebarMenu( 
      menuItem("Dimentions", tabName = "Dimentions", icon = icon("wrench"),
               sliderInput(
                 inputId = "Tower Gross Floor Area (GFA)", 
                 label = "TGFA", 
                 value = c(150000, 600000),
                 min = 90000,
                 max = 1000000
               ), 
               sliderInput(
                 inputId = "Parking Gross Floor Area (GFA)", 
                 label = "PGFA", 
                 value = c(60000, 400000), 
                 min = 30000, 
                 max = 600000
                 ),
               sliderInput(
                 inputId = "Retail/Commercial GFA", 
                 label = "Commercial GFA", 
                 value = c(10000, 50000), 
                 min = 5000, 
                 max = 100000
                 )
                ), 
      menuItem("Units", tabname = "Units", icon = icon("home"), 
               sliderInput(
                 inputId = "Studio", 
                 label = "Studios", 
                 value = c(310, 1500), 
                 min = 0, 
                 max = 3000
               ),
               sliderInput(
                 inputId = "1B", 
                 label = "1B", 
                 value = c(75, 250),
                 min = 0, 
                 max = 1000
               ), 
               sliderInput(
                 inputId = "2B", 
                 label = "2B", 
                 value = c(50, 100), 
                 min = 0, 
                 max = 1000
               ), 
               sliderInput(
                 inputId = "3B",
                 label = "3B",
                 value = c(10, 25), 
                 min = 0, 
                 max = 1000
               ), 
               sliderInput(
                 inputId = "Total Units", 
                 label = "Total Units", 
                 value = c(100, 5000), 
                 min = 50,
                 max = 10000
                 )
             ), 
      menuItem("Duration", tabname = "Duration", icon = icon("calendar"), 
               sliderInput(
                 inputId = "Total Duration (mos)",
                 label = "Total Duration (mos)", 
                 value = c(24, 30), 
                 min = 6, 
                 max = 32
                 ), 
               sliderInput(
                 inputId = "Project Start Year", 
                 label = "Start Year", 
                 value = c(2012, 2015), 
                 min = 2005, 
                 max = 2023,
                 ), 
               sliderInput(
                 inputId = "Project Finish Year", 
                 label = "Finish Year", 
                 value = c(2014, 2017), 
                 min = 2006, 
                 max = 2030
                 )
               ), 
      menuItem("Cost", tabname = "Cost", icon = icon("signal"), 
               sliderInput(
                 inputId = "Cost Normalization Rate", 
                 label = "Cost Normalization Rate", 
                 value = 4, 
                 min = 2, 
                 max = 16
               ) 
              ), 
      menuItem("Inputs", tabname = "Inputs", icon = icon('filter'), 
               selectInput( 
                 inputId = "Cost_Name", 
                 label = "Select Cost Activity: ",
                 choices = c(unique(data2$Cost_Name), "All"),
                 multiple = TRUE, 
                 selected = "ALL"
               ),
               selectInput(
                 inputId = "Type", 
                 label = "Select Building Type: ", 
                 choices = c(unique(data2$Type), "Both"),
                 multiple = TRUE, 
                 selected = "ALL"
               ),
               selectInput(
                 inputId = "Facet", 
                 label = "Select facet Variable: ", 
                 choices = c(colnames(data2)), 
                 multiple = FALSE, 
                 selected = 'Cost_type'
               ), 
               selectInput( 
                 inputId = "x", 
                 label = "Select X Value : ", 
                 choices = c(colnames(data2)), 
                 multiple = FALSE, 
                 selected = "Project"
               )
              )
    )
  ), 
  dashboardBody(
    fluidRow( 
      box( 
        title = "Stacked Bar Chart",
        background = "light-blue", 
        solidHeader = TRUE, 
        status = "primary", 
        width = 12, 
        plotOutput("stacked_bar_chart")
      )
    )
  )
)

############################## 
#Loading and Cleaning Data 
data2 <- preconc_longer
data


#define server logic required to draw stacked bar chart 

server <- function(input, output, session) { 
  filtered_data <- reactive({ 
    data_filtered <- preconc_longer 
    
    #Filter by Type 
    if (input$Type != "Both") { 
      data_filtered <- data_filtered[data_filtered$Type == input$Type, ]
    }
    
    if (!"All" %in% input$Cost_Name) { 
      data_filtered <- data_filtered[data_filtered$Cost_Name %in% input$Cost_Name, ]
    } 
    
    return(data_filtered) 
  }) 
  
  output$stacked_bar_chart <- renderPlotly({ 
    p <- ggplot(filtered_data, aes_string(x = !!sym(input$x), y = Cost, fill = factor(!!sym(input$Cost_Name)))) + 
                  geom_bar(position = "stack", stat = "identity") + 
                  labs( 
                    title = 'Stacked Bar Chart', 
                    x = input$x, 
                    fill = 'Cost Name'
                  ) +
                  theme_minimal()
    
    
    if (input$Facet != 'None') { 
      p <- p +facet_grid(~!!sym(input$Facet))
    }
    
    return(ggplotly(p))
  })
}
##########################
#Server try 2 
#Filter by Type 
#server <- function(input, output, session) { 
 # filtered_data <- reactive({ 
  #  data_filtered <- data2 
    
    #Filter by Type 
   # if (input$Type != "Both") { 
    #  data_filtered <- data_filtered[data_filtered$Type == input$Type, ]
    #}
    
    #if (!"All" %in% input$Cost_Name) { 
      #data_filtered <- data_filtered[data_filtered$Cost_Name %in% input$Cost_Name, ]
   # } 
    
   # return(data_filtered)  
    #})  
  
  #selected_x_var <- reactive({ 
    
   # input$x 
    
  #}) 
  
  #filtered_data <- reactive({
  #  data2[, c(selected_x_var(), "Cost")]
    # Replace 'other_columns_to_plot_on_y' with the columns you want to plot on the y-axis
  #})


  
  
  

#server <- function(input, output, session) {
  # Assuming 'your_data' is your dataset
  #selected_x_var <- reactive({
    # Get the selected variable from the input
   # input$x
  #})
  
  # Use the selected variable to subset your data
  #filtered_data <- reactive({
   # data2[, c(selected_x_var(), "Cost")]
    # Replace 'other_columns_to_plot_on_y' with the columns you want to plot on the y-axis
  #})
  
  # Create your plot using 'filtered_data'
  #output$stacked_bar_chart <- renderPlot({
    # Use 'filtered_data()' for your plot
    # Example: plot(filtered_data())
  #})
#}

#server <- function(input, output, session) {
  

   # data_filtered <- reactive({ 
    #  filtered_data <- data2 %>%  
    
    #Filter for 
    #filter("Tower Gross Floor Area (GFA)" == input$`Tower Gross Floor Area (GFA)`) %>% 
    #filter("Parking Gross Floor Area (GFA)" == input$`Parking Gross Floor Area (GFA)`) %>% 
    #filter("Retail/Commercial GFA" == input$`Retail/Commercial GFA`) %>% 
    #filter("Studio" == input$Studio) %>% 
    #filter("1B" == input$'1B') %>% 
    #filter("2B" == input$'2B') %>% 
    #filter("3B" == input$'3B') %>% 
    #filter("Total Units" == input$`Total Units`) %>% 
    #filter("Total Duration (mos)" == input$`Total Duration (mos)`) %>% 
    #filter("Project Start Year" == input$`Project Start Year`) %>% 
    #filter("Project Finish Year" == input$`Project Finish Year`) %>% 
    #filter("Cost Normalization Rate" == input$`Cost Normalization Rate`) %>% 
    #filter("Type" == input$Type) %>% 
    #filter("Cost_Name" == input$Cost_Name) 
  
   # })
 #output$stacked_bar_chart <- renderPlotly({ggplot(data_filtered, aes(x = input$x, y = data2$, fill = factor(input$Cost_Name))) + 
    # geom_bar(position = "stack", stat = "identity") + 
     #facet_grid(~ input$Facet)
   #}) 
#}  
#running the application


shinyApp(ui, server)


p
