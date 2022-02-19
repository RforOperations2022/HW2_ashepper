
### libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

NAEdata <- read.csv('NAE_data.csv')


#########################

### UI

## header and title
header <- dashboardHeader(title = 'New American Economy Cities Index', 
                          titleWidth = 400)

## Dashboard Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    menuItem('Plots', tabName = 'dashboard', icon = icon('bar-chart')),
    menuItem('Data Table', tabName = 'data', icon = icon('table'))
  ),
  
  # INPUT == overal score ---------------------------------------------
  sliderInput("overall_score", "Select Range for Overall Cities Index Score",
              min = 0, 
              max = 5,
              value = range(NAEdata$overall_score)),
  
  # INPUT - total population ------------------------------------------------
  sliderInput("pop_tot", "Select Range for City's Total Population",
              min = min(NAEdata$pop_tot), 
              max = max(NAEdata$pop_tot),
              value = range(NAEdata$pop_tot)),
  
  # INPUT - total fb population ---------------------------------------
  sliderInput("pop_fb", "Select Range for City's Total Foreign Born Population",
              min = min(NAEdata$pop_fb), 
              max = max(NAEdata$pop_fb),
              value = range(NAEdata$pop_fb))
  
)

## BODY OF DASHBOARD -------------------------------------------------------------------
body <- dashboardBody(tabItems(
    
    # tab item 1 - dashboard
    tabItem(tabName = 'dashboard',
            
            # top row: value and info boxes
            fluidRow(
              
              # total cities included in dataset
              valueBoxOutput('total_cities'),
              
              # average policy score
              infoBoxOutput('avg_pol_score'),
              
              # average se score
              infoBoxOutput('avg_se_score')
              
            ),
            
            # bottom row: tabs with plots!
            fluidRow(
              tabBox(title = '', 
                     width = 12,
                     
                     # tab 1: scatterplot fb share by rank
                     tabPanel('Score Rank',plotlyOutput('fbshare_scatterplot')),
        
                     # tab 2: naturalization rate by policy score - scatterplot
                    tabPanel('Naturalization', plotlyOutput('nat_scatterplot'),
                     
                     # tab panel 3: homeownership scatterplot
                     tabPanel('Homeownership', plotlyOutput(outputId = 'home_scatterplot')))
                              ),
    )),
    
    tabItem(tabName = 'data',
            
            # data table - box outline 
            box(width = 12,
                title = 'Data Table',
                DT::dataTableOutput(outputId = 'fulldatatable')))
    )
)
  

## define UI
ui <- dashboardPage(skin = 'green', 
                    header, 
                    sidebar, 
                    body)

#########################

###  ------------------------------------------------------------------------------------

server <- function(input, output, session) {
  

  # main dataset --> filter dataframe with input selections
  data_subset <- reactive({
    NAEdata %>%
      filter(
         overall_score >= input$overall_score[1],
         overall_score <= input$overall_score[2],
         pop_tot >= input$pop_tot[1],
         pop_tot <= pop_tot[2],
         pop_fb >= input$pop_fb[1],
         pop_fb<= input$pop_fb[2]
         
      )
  })
  
  # reactive - fb share of pop by rank
  share_scatterplot <- reactive({
    data_subset() %>%
      arrange(overall_ranking)
      
  })
  
  # reactive - naturalization rate by policy score scatterplot
  nat_scatterplot <- reactive({
    data_subset()
      
  })
  
  # reactive - homeownership rate by policy score scatterplot
  home_scatterplot <- reactive({
    data_subset()
  })
  
  ## outputs---------------------------------------------------------------
  # count cities box
  output$total_cities <- renderValueBox({
    cities <- data_subset()
    count <- nrow(cities)
    valueBox(subtitle = 'Total Cities in Dataset', 
             value = count,
             icon = icon("person"),
             color = 'yellow')
  })
  
  # policy score info box
  output$avg_pol_score <- renderInfoBox({
    polscore <- data_subset()
    meanpolscore <- mean(polscore$policy_score)
    valueBox(subtitle = 'Average Overall Policy Score', 
             value = round(meanpolscore, digits = 2),
             icon = icon("person"),
             color = 'blue')
  })
  
  # se score info box
  output$avg_se_score <- renderInfoBox({
    sescore <- data_subset()
    meanpolsescore <- mean(sescore$se_score)
    valueBox(subtitle = 'Average Socioeconomic Score', 
             value = round(meanpolsescore, digits = 2),
             color = 'purple')
  })

  # scatterplot fb share by rank
  output$fbshare_scatterplot <- renderPlotly({
    scatter_rank <- share_scatterplot()
    ggplot(data = scatter_rank,
            aes(x = overall_ranking,
                y = pop_fb_share,
                text = paste("City:", city
                ))) +
      geom_point(aes(color = factor(gov_leadership))) + 
      labs(x = "Policy Score Rank", 
           y = "Foreign Born Share of Population", 
           title = "Does City-Level Inclusion Policy Correlate with FB Share of Population")
      }) 
  
  
  # scatterplot --> naturalization rate by policy score
  output$nat_scatterplot <- renderPlotly({
    scatter_nat <- nat_scatterplot()
    ggplot(data = scatter_nat,
           aes(x = natrate,
              y = policy_score,
              text = paste("City:", city
                ))) +
      geom_point(aes(color = factor(civic_particip))) + 
      labs(x = "Naturalization Rate of Foreign BOrn Population", 
           y = "Policy Score", 
           title = "Does City-Level Inclusion Policy Correlate with Naturalization Rates")
    
  }) 
  
  
  # scatterplot --> homeownership by policy score
  output$home_scatterplot <- renderPlotly({
    scatter_home <- home_scatterplot()
    ggplot(data = scatter_home,
           aes(x = fbownershiprate,
              y = policy_score,
              text = paste("City:", city
                ))) +
      geom_point(aes(color = factor(civic_particip))) + 
      labs(x = "Homeownership Rate of Foreign BOrn Population", 
           y = "Policy Score", 
           title = "Does City-Level Inclusion Policy Correlate FB Homeownership")
    
  }) 
  
  
  # full data table
  output$fulldatatable <- DT::renderDataTable({
    
    DT::datatable(data = data_subset(),
                  options = list(pageLength = 20),
                  rownames = TRUE)
    
  })
  
}

# run the application
shinyApp(ui = ui, server = server)
