#install.packages("shinydashboard")
#install.packages("goofleVis")
#install.packages("tm")
#install.packages("SnowballC")
library(shinydashboard) # Dashboard package
library(shiny)
library(ggplot2)
library(googleVis)
library(DT)
library(countrycode) #converts Country names/code
library(wordcloud2)
library(tm)
library(SnowballC)

# create function for create word cloud by free text by users
create_wordcloud <- function(data, num_words = 100, background = "white") {
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  wordcloud2(data, backgroundColor = background)
}

# Load the dataset
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
            h1("Prediction Widgets"),
            
            fluidRow(
              box(
                h2("Declair: This results of suicidal risk prediction only based on the individual demographical factors,",
                br(),
                "word cloud shown below act as the assistance for showing top 20 wording used for his/her expressing her real time emotion and throught."),

                h3("Help text"),
                        helpText("Note: Please select the age number according to the list below,",
                                 br(),
                                 "1: 5-14 years; 2: 15-24 years; 3: 25-34 years;",
                                 "4: 35-54 years; 5: 55-74 years; 6: 75+ years"),
                br(),

                selectInput("Age",
                            label = "Choose an age Range",
                            choices = unique(file$age)),
                
                br(),
                
                selectInput("Sex",
                            label = "Choose a Sex",
                            choices = unique(file$sex)),
                
                br(),
                
                selectInput("Country",
                            label = "Living in:",
                            choices = unique(file$country)),

                br(),
                
                selectInput("Feelings",
                            label = "How is your day today:",
                            choices = c("Happy",
                                        "Sad",
                                        "Anger",
                                        "Depressed",
                                        "Frustrated")),
                br(),

                textAreaInput("text", "Any thing bother your mind?", rows = 18),
                #Add a "Enter" button to the app
                actionButton(inputId = "enter", label = "Enter")
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
pred <- function(Country, Sex, Age) {
  inputdata <- c(Country, Sex, Age)
  pred_data <- as.data.frame(t(inputdata))
  colnames(pred_data) <- c("country", "sex", "age_group")
  prob_out <- predict(RF, pred_data)
  return(prob_out$predictions)
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
  
  output$tendency <- renderPrint({pred(input$Age, input$Sex, input$Country)})

  output$cloud <- renderWordcloud2({
    input$enter
    isolate({
      create_wordcloud(input$text, num_words = 20, background = "white")
      })
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
