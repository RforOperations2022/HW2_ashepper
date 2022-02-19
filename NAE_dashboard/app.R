
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
    
    menuItem('Plots', tabName = 'dashboard', icon = icon('chart-bar')),
    menuItem('Data Table', tabName = 'data', icon = icon('table'))
  ),
  
  # INPUT == overall score ---------------------------------------------
  sliderInput("overall_score", "Select Range for Overall Cities Index Score",
              min = 0, 
              max = 5,
              value = c(0,5)),
  
  # INPUT - total population ------------------------------------------------
  sliderInput("pop_tot", "Select Range for City's Total Population",
              min = 200000, 
              max = 10000000,
              value = c(200000, 10000000)),
  
  # INPUT - total fb population ---------------------------------------
  sliderInput("pop_fb", "Select Range for City's Total Foreign Born Population",
              min = 9000, 
              max = 4000000,
              value = c(9000,4000000))
  
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
                     
                     # intro tab
                     tabPanel('Introduction', textOutput('introtext')),
                     
                     # tab 1: scatterplot fb share by rank
                     tabPanel('Population Share',plotlyOutput('fbshare_scatterplot')),
        
                     # tab 2: naturalization rate by policy score - scatterplot
                    tabPanel('Naturalization', plotlyOutput('nat_scatterplot')),
                     
                     # tab panel 3: homeownership scatterplot
                     tabPanel('Homeownership', plotlyOutput(outputId = 'home_scatterplot'))
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
      filter((overall_score >= input$overall_score[1]) &
          (overall_score <= input$overall_score[2]) &
           (pop_tot >= input$pop_tot[1]) & 
           (pop_tot <= pop_tot[2]) & 
           (pop_fb >= input$pop_fb[1]) &
           (pop_fb<= input$pop_fb[2])
      )
  })
  
  # reactive - fb share of pop by rank
  share_scatterplot <- reactive({
    data_subset()
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
             color = 'yellow')
  })
  
  # policy score info box
  output$avg_pol_score <- renderInfoBox({
    polscore <- data_subset()
    meanpolscore <- mean(polscore$policy_score)
    valueBox(subtitle = 'Average Overall Policy Score', 
             value = round(meanpolscore, digits = 2),
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
  
  # intro text 
  output$introtext <- renderText({'Cities across the United States are implementing policies and programs aimed at
    attracting and retaining foreign born residents. Some of these include language access initiatives, entrepreneurship support
    programs targeted towards immigrants, and establishing offices or commissions for immigrant affairs. New American Economy (newamericaneconomy.org), 
    a bipartisan immigration research and advocacy organization, annually collects data on immigration-related city policies
    for the 100 largest cities in the United States. They use this data to create the ~NAE Cities Index~, which ranks
    American cities by how "inclusive" their polcies are for foreign-born residents. This dashboard uses New American Economy data to explore
    visual correlations between a NAE scorecard scores and characacteristics about the socioeconomic outcomes of its residents. 
    The graphs here do not imply causation, but are a way to explore which cities are implementing policies aimed at attracting and retaining immigrants.'})

  # scatterplot fb share by policy score
  output$fbshare_scatterplot <- renderPlotly({
    scatter_rank <- share_scatterplot()
    ggplot(data = scatter_rank,
            aes(x = pop_fb_share,
                y = policy_score,
                text = paste("City:", city
                ))) +
      geom_point(aes(color = factor(gov_leadership))) + 
      labs(x = "Foreign Born Share of Population", 
           y = "Policy Score", 
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
      geom_point(aes(color = factor(Community))) + 
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
      geom_point(aes(color = factor(econ_empower))) + 
      labs(x = "Homeownership Rate of Foreign BOrn Population", 
           y = "Policy Score", 
           title = "Does City-Level Inclusion Policy Correlate FB Homeownership")
    
  }) 
  
  
  # full data table
  output$fulldatatable <- DT::renderDataTable({
    
    DT::datatable(data = data_subset(),
                  options = list(pageLength = 10),
                  rownames = TRUE)
    
  })
  
}

# run the application
shinyApp(ui = ui, server = server)
