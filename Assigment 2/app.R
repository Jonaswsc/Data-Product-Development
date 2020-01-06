#install.packages("shinydashboard")
#install.packages("goofleVis")
library(shinydashboard) # Dashboard package
library(shiny)
library(ggplot2)
library(googleVis)
library(DT)
library(wordcloud2)
library(countrycode) #converts Country names/code
file <- read.csv('suicidedata.csv')


# User interface ----
header <- dashboardHeader(title = "Suicide Prediction and Prevention", titleWidth = 350)

sidebar <- dashboardSidebar(width = 350,
                          sidebarMenu(id ="tabs",
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Predictive Widgets", icon = icon("th"), tabName = "widgets",
                                     badgeLabel = "new", badgeColor = "green"),
                            menuItem("Dataset", tabName = "data",
                                     badgeLabel = "Update", badgeColor = "green")
                          )
                         )
body <- dashboardBody(
        # Also add some custom CSS to make the title background area the same
        # color as the rest of the header.
          tabItems(
            tabItem(tabName = "dashboard", 
                    class = "active",
                    h2("Suicide Overview"),
                    
                    fluidRow(
                      box(
                        title = "Criteria Panel", width = 3, height = 400, solidHeader = TRUE,
                        "Periods", 
                        sliderInput("year", "Years:", 1985, 2016, c(2006, 2016))
                      ),
                      
                      box(
                        title = "Map", width = 9, solidHeader = TRUE, status = "primary",
                        htmlOutput("mymap")
                        
                      )
                    )
            ),
            
            tabItem(tabName = "widgets",
                    class = "active",
                    h2("Prediction Widgets"),
                    
                    fluidRow(
                      box(
                        h3("Help text"),
                        helpText("Note: Please select the age number according to the list below,",
                                 br(),
                                 "1: 5-14 years; 2: 15-24 years; 3: 25-34 years;",
                                 "4: 35-54 years; 5: 55-74 years; 6: 75+ years"),
                        br(),
                        
                      selectInput("Age",
                                  label = "Choose an age Range",
                                  choices = file$age_group),
                      
                      br(),
                      
                      selectInput("Sex",
                                  label = "Choose a Sex",
                                  choices = file$sex),
                      
                      br(),
                      
                      selectInput("Country",
                                  label = "Living in:",
                                  choices = file$country),
                      br(),
                      
                      numericInput("Earnings",
                                  label = "Earning per month:",
                                  value = 0)
                      ),
                      
                      box(
                      selectInput("Feelings",
                                  label = "How do you feel at the moment:",
                                  choices = c("Happy",
                                              "Sad",
                                              "Anger",
                                              "Depressed",
                                              "Frustrated")),
                      br(),
                      
                      textAreaInput("text", "Any thing bother your mind?", rows = 18),
                      
                      ),
                      
                      br(),
                      
                      box(
                        textOutput("tendency"),
                      
                        wordcloud2Output("cloud")
                      )
                    )),
            
            tabItem(tabName = "data",
                    class = "active",
                    h2("Dataset"),
                    
                    fluidPage(
                      downloadButton("download1"),
                      downloadLink("download2"),
                      DT::dataTableOutput("mytable")
                    ))
          ),
            
          tags$head(
                tags$style(HTML('
                .main-header .logo {
                  font-family: "Georgia", Times, "Times New Roman", serif;
                  font-weight: bold;
                  font-size: 18px;
                  }'
                ))
            ),
          
            tags$style(HTML('
            .skin-blue .main-header .logo {
            background-color: #3c8dbc;
            }
            .skin-blue .main-header .logo:hover {
            background-color: #3c8dbc;
            }
          ')))
          
        

ui <- dashboardPage(skin = "purple", header, sidebar, body)
# Load predictive model
RF <- get(load("suicideMLrandomforest.rda"))
pred <- function(Age, Sex, Country, Earnings) {
  inputdata <- c(Age, Sex, Country, Earnings)
  pred_data <- as.data.frame(t(inputdata))
  colnames(pred_data) <- c("Age Group", "Sex", "Living in", "Earnings per month")
  prob_out <- predict (RF, pred_data)
  return(prob_out)
}
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- file
    data <- subset(
      data,
      year >= input$year[1] & year <= input$year[2]
    )
    data
  })
    output$mymap <- renderGvis({
      file <- filtered_data()
      map <-gvisGeoChart(file, locationvar = 'country', colorvar='suicides.100k.pop',
                         options=list(projection="kavrayskiy-vii"))
      return(map)
       })
    
    
    output$mytable <- DT::renderDataTable({
      file
    })
    output$download <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv")
      },
      content = function(file) {
        write.csv(data(), file)
      }
    )
    
    output$tendency <- renderText({pred(input$Age, input$Sex, input$Country, input$Earnings)})
    
    output$cloud <- renderWordcloud2({
      create_wordcloud(data = input$text, num_words = 3, background = "white")
    })
      
}
    
# Run the application 
shinyApp(ui = ui, server = server)
