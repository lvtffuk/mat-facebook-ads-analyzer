library(shiny)
library(plotly)
library(shinycssloaders)
library(readr)
library(shinythemes)

path <- "czech2"

pages <- read_csv(paste0(path, "/total_ads_per_page.csv", sep=""))
pages <- pages  %>% arrange(desc(n_ads))
pages <- pages$page_name


funding <- read_csv(paste0(path, "/total_ads_per_funding.csv", sep=""))
funding <- funding  %>% arrange(desc(n_ads))
funding <- funding$funding_entity

config <- read_csv(paste0(path, "/config.csv", sep=""))

mindate <- as.character(config[1,]$mindate)
maxdate <- as.character(config[1,]$maxdate)

ui <- fluidPage(theme = shinytheme("journal"),
  titlePanel("Facebook Ads Library Explorer"),
      
  sidebarLayout(
  sidebarPanel(

#    selectInput("select", label = h3("Dataset"), 
#    choices = list("Slovak" = "slovak", "Czech" = "czech"), 
#    selected = "slovak"),

      selectizeInput(
        'specific_funding_entity', 'Funding entity', choices = funding, multiple = TRUE
      ),  
      
        selectizeInput(
        'specific_page', 'Facebook pages', choices = pages, multiple = TRUE
      ),    

#  checkboxGroupInput("checkGroup", label = h3("Age"), 
#    choices = list(
# "13-17" = "13-17", 
# "18-24" = "18-24", 
# "25-34" = "25-34", 
# "35-44" = "35-44", 
# "45-54" = "45-54", 
# "55-64" = "55-64", 
# "65+" = "65+" 
#  ),
#     selected = 1),

    selectInput("selectGender", label = h5("Gender"), 
     choices = list(
         "Not important" = "nonactive", 
         "Male" = "male", 
         "Female" = "female"), 
     selected = 1),

sliderInput("rangeGender", "Percentage of gender:",
                  min = 0, max = 1, step = 0.01,
                  value = c(0,1)),
 

  sliderInput("range", "Lower - Upper Spend:",
                  min = 0, max = 10000, step = 10,
                  value = c(0,10000)),
 
  sliderInput("minAds", "Min - ax number of ads",
                  min = 1, max = 1000, step = 1,
                  value = 1),



  dateRangeInput('dateRange',
      label = 'Date range input: yyyy-mm-dd',
      start = mindate, end = maxdate
    ),
  
  textInput("search", "Search in selected pages", ""),


  submitButton("Update View", icon("refresh")),

    h4("Facebook Ads Library Explorer"),

p("This project was inspired by article: The use of political ads on facebook in the run-up to the European Parliament elections 2019 in Austria.")
    ),


    mainPanel(
# plotOutput("tfidf") %>% withSpinner(),
tabsetPanel(

        tabPanel("Ads Overview", DT::dataTableOutput("overview")  %>% withSpinner(), DT::dataTableOutput("overviewAll") %>% withSpinner(), DT::dataTableOutput("overviewAllFunding") %>% withSpinner()),
        tabPanel("Spend Overview",  DT::dataTableOutput("overviewSpend")  %>% withSpinner(), plotOutput("totalSpend")  %>% withSpinner(), plotOutput("dailySpend")  %>% withSpinner(), plotOutput("perCategory") %>% withSpinner()),
        tabPanel("Ads audience age/gender", plotOutput("agePlotMidpoint") %>% withSpinner(), plotOutput("agePlot") %>% withSpinner(), plotOutput("heatmaps") %>% withSpinner(), plotOutput("genderDifference") %>% withSpinner()),
        tabPanel("Regions", plotOutput("regions",  height=800) %>% withSpinner()),
        tabPanel("Ads text", DT::dataTableOutput("adstext") %>% withSpinner()), 
        tabPanel("TF-IDF", plotOutput("tfidf", height=800) %>% withSpinner(), plotOutput("bitfidf", height=800) %>% withSpinner()),        
        tabPanel("Text Mining", 

         plotOutput("udpipeNouns") %>% withSpinner(), DT::dataTableOutput("udpipeNounsTable") %>% withSpinner(), 
         plotOutput("udpipeAdj") %>% withSpinner(),
         plotOutput("udpipeProp") %>% withSpinner(),

         

         plotOutput("rake") %>% withSpinner(),
         plotOutput("pmi") %>% withSpinner(),
         plotOutput("phrases") %>% withSpinner(),
         
         plotOutput("udpipeThree") %>% withSpinner(),
         plotOutput("udpipeFollow") %>% withSpinner(),
         plotOutput("udpipeCooc") %>% withSpinner())

      )

    )
  )
)

