library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(maps)
require(usmap)
require(ggplot2)
library(tidyverse)
library(plotly)
library(grid)
library(wordcloud)
library(wordcloud2)
library(tm)

setwd("~/Desktop/realtor/Webscraping_Project")

csv_files <- list.files(pattern = ".csv")

realtor <- read.csv('./realtor.csv')
realtor$price <- as.numeric(as.character(realtor$price))
realtor$zip <- as.character(as.numeric(realtor$zip))
realtor$city <- as.character(as.factor(realtor$city))
realtor$state <- as.character(as.factor(realtor$state))
#realtor$year_built <- as.numeric(as.factor(realtor$year_built))
realtor$year_built <- as.Date(realtor$year_built, format ="%Y")

population <- read.csv('./uscitypopdensity.csv')
income <- read.csv('./most_populated_cities_income.csv') 

realtor_ <- na.omit(realtor)

realtor_price_sqft <- realtor %>%
    select(., area, bath, bed, price, sqft, state, zip) %>% 
    mutate(price_sqft = price/sqft) 


# Define UI for application that draws a histogram

ui <- fluidPage(
    
    titlePanel("Guide to Living in Large Cities"),
    
    navlistPanel(
        "Table of Contents",
        tabPanel("Overview",
                 p("Exploration of Cost of living per Major USA Cities"),
                 plotOutput(outputId="price_distribution",
                            heigh="400px"),
                 plotOutput(
                         outputId = "price_area",
                         height = "400px"),
                 #p(selectizeInput(inputId = "state",
                                  #label = "state",
                                  #choices = unique(realtor$state))), 
                 p(plotOutput(outputId="price_sqft",
                              height = "400px")),
                 p(plotOutput(outputId="avg_sqft",
                              height="400px"))),

        tabPanel("Deeper Dive",
                 tabsetPanel(
                     tabPanel("Input text",
                        p(selectizeInput(inputId = "area",
                                    label = "area",
                                    choices = unique(realtor_price_sqft$area)),
                        p(plotOutput(outputId="costsqft_zip",
                                height="400px"))),
                        p(plotOutput(outputId = "realtor_num_bed_baths",
                                height="400px")),
                        p(plotOutput(outputId = "garage",
                                height="400px")),
                        p(plotOutput(outputId="year_built",
                                height="400px")),
                        p(plotOutput(outputId="year_built_sqft",
                                height="400px"))))),
        
        tabPanel("Popular Description",
                 tabsetPanel(
                     tabPanel("Input text",
                              p("Input text", style="font-size:17px"), 
                              p("Description Frequency"),
                              wordcloud2Output(
                                  outputId = "wordcloud",
                                  width ="100%",
                                  height = "400px")))),
        tabPanel("Taxes",
                 tabsetPanel(
                     tabPanel("Input text",
                              p("Input text", style="font-size:17px")))),
        
        tabPanel("Salary v Cost of Living",
                 tabsetPanel(
                     tabPanel("Input text",
                              p("Input text", style="font-size:17px"))))
        
    )
)


# Define server logic required to draw a histogram

server <- function(input, output) { 

    output$price_distribution <- renderPlot(
        realtor %>% 
            ggplot(aes(price)) +
            geom_histogram(aes(fill=area)) +
            xlab(NULL) +
            ggtitle("Distribution of Cost of Real Estate Homes in a Few Major Cities")
    )
    
    output$price_area <- renderPlot(
        realtor %>% 
        ggplot(aes(x=reorder(area, price), price)) +
        geom_boxplot(aes(fill=area)) +
        coord_flip() +
        xlab(NULL) +
        ggtitle("Distribution of Cost of Real Estate Homes in a Few Major Cities")
    )
    

    output$price_sqft <- renderPlot(
        realtor_price_sqft %>% 
            ggplot(aes(x=reorder(area, price_sqft), price_sqft)) +
            geom_boxplot(aes(fill=area)) +
            coord_flip() +
            xlab(NULL) +
            ggtitle("Distribution of Cost of Real Estate Homes per Sqft")
    )
    
    
    output$avg_sqft <- renderPlot(
        realtor %>% 
            ggplot(aes(x=reorder(area, sqft), sqft)) +
            geom_boxplot(aes(fill=area)) +
            coord_flip() +
            xlab(NULL) +
            ggtitle("Average Sqft of Homes in Various Cities")
    )
    
    output$costsqft_zip <- renderPlot(
        realtor_price_sqft %>%
            filter(area == input$area) %>%
            ggplot(aes(x=reorder(zip, price_sqft), price_sqft)) + 
            geom_boxplot(aes(color=zip)) + 
            theme(legend.position='right') +
            scale_fill_manual(values = c('tomato', 'lightseagreen'))+        
            ggtitle("Price/Sqft by Zipcode") +
            theme(axis.text.x=element_text(angle=45)) +
            xlab("Zipcode")
    )
    

    realtor_num_bed_baths <- realtor_ %>% 
            select(area, bed, bath, sqft) %>% 
            gather(bed_bath, number_BedBath, bed:bath) 

    
    output$realtor_num_bed_baths <- renderPlot(
        realtor_num_bed_baths %>% 
            ggplot(aes(x=area, y=number_BedBath)) +
            geom_boxplot(aes(fill=bed_bath)) + 
            geom_jitter(aes(color=bed_bath), alpha = 0.3) +
            ggtitle("Distribution of bed/baths by area") +
            coord_flip()
    )

    
    output$bedbaths_sqft <- renderPlot(
        realtor_num_bed_baths %>% 
            ggplot(aes(x=area, y=sqft)) +
            geom_boxplot(aes(fill=bed_bath)) + 
            geom_jitter(aes(color=bed_bath), alpha = 0.3) +
            coord_flip()
    )
    
    description <- realtor$description
    
    docs <- Corpus(VectorSource(description))

    docs <- docs %>% 
        tm_map(removeNumbers) %>% 
        tm_map(removePunctuation) %>% 
        tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
        
    dtm <- TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix), decreasing = TRUE)
    df <- data.frame(word = names(words), freq=words)



    output$wordcloud <- renderWordcloud2(
        df %>% 
            wordcloud2(size=0.5, color='random-dark')
    )
    
    realtor_detailed <- realtor %>% 
        select(., garage_space, year_built, area, price, sqft)
    
    output$garage <- renderPlot(
        realtor_detailed %>% 
            ggplot(aes(x=area, y=garage_space)) +
            geom_boxplot(aes(fill=area)) + 
            geom_jitter(aes(color=area), alpha = 0.3) +
            coord_flip()
    
    )
    


    output$year_built <- renderPlot(
        realtor %>% 
            ggplot(aes(x=year_built, y=area)) +
            geom_boxplot(aes(fill=area)) + 
            geom_jitter(aes(color=area), alpha = 0.3) +
            #scale_fill_manual(values = c('tomato', 'lightseagreen'))+
            #xlab(NULL) + ylab("Salary") + ggtitle("Distribution of Med Starting and Mid Career Salaries") +
            #scale_y_continuous()+
            scale_x_date(breaks = pretty_breaks(10))

    )
    
    output$year_built_sqft <- renderPlot(
        realtor_detailed %>% 
            ggplot(aes(x=sqft, y=year_built)) +
            geom_boxplot(aes(fill=area)) + 
            geom_jitter(aes(color=area), alpha = 0.3)
            #scale_fill_manual(values = c('tomato', 'lightseagreen'))+
            #xlab(NULL) + ylab("Salary") + ggtitle("Distribution of Med Starting and Mid Career Salaries") +
            #scale_y_continuous()+

    )
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
