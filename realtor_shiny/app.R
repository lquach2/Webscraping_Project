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
library(reshape2)

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
income$median.income <- as.numeric(income$median.income)

#realtor_ <- na.exclude(realtor)

realtor_price_sqft <- realtor %>%
    select(., area, bath, bed, price, sqft, state, zip) %>% 
    mutate(price_sqft = price/sqft, na.rm = TRUE) 

taxes <- realtor %>%
    select(., area, taxes_2015, taxes_2016, taxes_2017, taxes_2018, taxes_2019) %>%  
    gather(tax_year,taxes, taxes_2015:taxes_2019) 

avgtax <- taxes %>% 
    group_by(area) %>% 
    summarise(avgtax = mean(taxes, na.rm=TRUE))

avgtax_year <- taxes %>% 
    group_by(area, tax_year) %>% 
    summarise(avgtax = mean(taxes, na.rm=TRUE))


x <- br()
realtor_income <- merge(avg_price_home, income, by="area")


avg_price_home <- realtor %>% 
    select(price, area, state) %>% 
    mutate(avg_price_home = mean(price), area) 

price_per_income <- realtor_income %>% 
    mutate(price_per_median.income = (price/median.income))

# Define UI for application that draws a histogram

ui <- fluidPage(
    
    titlePanel("Guide to Living in Large Cities"),
    
    navlistPanel(
        "Table of Contents",
        tabPanel("Overview",
                 p("Exploration of Cost of living per Major USA Cities", style="font-size:24px"),
                 x, p("Majority of the cities explored are priced below 2.5million"),
                 plotOutput(outputId="price_distribution",
                            height="600px"),
                 x,
                 plotOutput(
                         outputId = "price_area",
                         height = "600px"),
                 x,
                 p(plotOutput(outputId="avg_sqft",
                              height = "600px")),
                 x,
                 p(plotOutput(outputId="price_sqft",
                              height ="600px"))),

        tabPanel("Deeper Dive",
                 tabsetPanel(
                     tabPanel("Input text",
                        p(selectizeInput(inputId = "area",
                                    label = "area",
                                    choices = unique(realtor_price_sqft$area)),
                        x, p(plotOutput(outputId="costsqft_zip",
                                height="600px"))),
                        x, p(plotOutput(outputId = "realtor_num_bed_baths",
                                height="600px")),
                        x, p(plotOutput(outputId = "garage",
                                height="600px")),
                        x, p(plotOutput(outputId="year_built",
                                height="600px")),
                        p(plotOutput(outputId="year_built_sqft",
                                height="600px"))))),
        
        tabPanel("Popular Description",
                 tabsetPanel(
                     tabPanel("Input text",
                              p("Input text", style="font-size:17px"), 
                              p("Description Frequency"),
                              wordcloud2Output(
                                  outputId = "wordcloud",
                                  width ="100%",
                                  height = "600px")))),
        tabPanel("Taxes",
                 tabsetPanel(
                     tabPanel("Input text",
                              p("Input text", style="font-size:17px"),
                              plotOutput(
                                  outputId = "taxes",
                                  height = "800px"),
                              x,
                              plotOutput(
                                  outputId="avgtax",
                                  height="600px"),
                              x,
                              plotOutput(
                                  outputId="avgtax_year",
                                  height="600px"),
                              ))),
        
        tabPanel("Salary v Cost of Living",
                 tabsetPanel(
                     tabPanel("Input text",
                              p("Input text", style="font-size:17px"),
                              plotOutput(
                                  outputId="median.income",
                                  height="600px"),
                              x,
                              plotOutput(
                                  outputId = "income",
                                  height = "600px"),
                              x, p(plotOutput(
                                  outputId="price_income",
                                  height="600px"),
                              ))))
    )
)


# Define server logic required to draw a histogram

server <- function(input, output) { 

#Overview
    
    output$price_distribution <- renderPlot(
        realtor_ %>% 
            ggplot(aes(price)) +
            geom_histogram(aes(fill=area)) +
            xlab("Price of Real Estate") +
            ggtitle("Distribution of Cost of Real Estate Homes in Select Cities") +
            theme(text = element_text(size=14))
        
    )
    
    output$price_area <- renderPlot(
        realtor %>% 
        ggplot(aes(x=reorder(area, price), price)) +
        geom_boxplot(aes(fill=area)) +
        coord_flip() +
        ylab("Price of Real Estate ($)") +
        xlab(NULL) +
        ggtitle("Distribution of Cost of Real Estate Homes in Select Cities")+
        theme(text = element_text(size=14))
    )
    

    output$price_sqft <- renderPlot(
        realtor_price_sqft %>% 
            ggplot(aes(x=reorder(area, price_sqft), price_sqft)) +
            geom_boxplot(aes(fill=area)) +
            coord_flip() +
            xlab(NULL) +
            ylab("Price ($) per sqft") +
            ggtitle("Distribution of Cost of Real Estate Homes per Sqft")+
            theme(text = element_text(size=14))
        
    )
    
    
    output$avg_sqft <- renderPlot(
        realtor %>% 
            ggplot(aes(x=reorder(area, sqft), sqft)) +
            geom_boxplot(aes(fill=area)) +
            coord_flip() +
            xlab(NULL) +
            ylab("Sqft")+
            ggtitle("Average Sqft of Homes in Various Cities")+
            theme(text = element_text(size=14))
        
    )
    
#Deeper Dive
    
    output$costsqft_zip <- renderPlot(
        realtor_price_sqft %>%
            filter(area == input$area) %>%
            ggplot(aes(x=reorder(zip, price_sqft), price_sqft)) + 
            geom_boxplot(aes(color=zip)) + 
            theme(legend.position='right') +
            scale_fill_manual(values = c('tomato', 'lightseagreen'))+        
            ggtitle("Price($) per Sqft by Zipcode") +
            theme(axis.text.x=element_text(angle=45)) +
            xlab("Zipcode")+
            ylab("Price ($) per Sqft") +
            theme(text = element_text(size=14))
        
    )
    

    realtor_num_bed_baths <- realtor %>% 
            select(area, bed, bath, sqft) %>% 
            gather(bed_bath, number_BedBath, bed:bath) 

    
    output$realtor_num_bed_baths <- renderPlot(
        realtor_num_bed_baths %>% 
            ggplot(aes(x=area, y=number_BedBath)) +
            geom_boxplot(aes(fill=bed_bath)) + 
            geom_jitter(aes(color=bed_bath), alpha = 0.3) +
            ggtitle("Distribution of bed/baths by area") +
            ylab("Number of Bed and Baths")+
            xlab(NULL) +
            coord_flip()+
            theme(text = element_text(size=14))
        
    )

    
    output$bedbaths_sqft <- renderPlot(
        realtor_num_bed_baths %>% 
            ggplot(aes(x=area, y=sqft)) +
            geom_boxplot(aes(fill=bed_bath)) + 
            geom_jitter(aes(color=bed_bath), alpha = 0.3) +
            coord_flip()
    )
    
#Popular Description
    
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
    
    
    output$garage <- renderPlot(
        realtor %>% 
            ggplot(aes(x=area, y=garage_space)) +
            geom_boxplot(aes(fill=area)) + 
            #geom_jitter(aes(color=area), alpha = 0.3) +
            coord_flip() +
            xlab(NULL) +
            ylab("Garage")+
            theme(text = element_text(size=14))
        
    
    )
    


    output$year_built <- renderPlot(
        realtor %>% 
            ggplot(aes(x=year_built, y=area)) +
            geom_boxplot(aes(fill=area)) + 
            geom_jitter(aes(color=area), alpha = 0.3) +
            scale_x_date(breaks = pretty_breaks(10))+
            xlab("Year Built") +
            ylab(NULL)+
            theme(text = element_text(size=14))
        

    )
    
    output$year_built_sqft <- renderPlot(
        realtor %>% 
            ggplot(aes(x=year_built, y=sqft)) +
            geom_boxplot(aes(fill=area)) + 
            scale_x_date(breaks= pretty_breaks(10))+
            theme(text = element_text(size=14))
        
            

    )
    
#Taxes    
    
    output$taxes <- renderPlot(
        taxes %>% 
            ggplot(aes(x = tax_year, y = taxes)) +
            geom_boxplot(aes(color=area)) +
            facet_wrap(facets=vars(area)) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
            xlab(NULL)+
            theme(text = element_text(size=14))
        
    )
    
    output$avgtax_year <- renderPlot(
        avgtax_year %>% 
            ggplot(aes(x=tax_year, y=avgtax, group=area)) +             
            geom_line(aes(color=area)) +
            geom_point(aes(color=area)) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
            theme(text = element_text(size=14)) +
            xlab(NULL) +
            ylab("Average Property Tax")
        
    )
    
    output$avgtax <- renderPlot(
        avgtax %>% 
            ggplot(aes(x=area, y=avgtax)) +             
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
            theme(text = element_text(size=14)) +
            xlab(NULL) +
            ylab("Average Property Tax")
        
        
    )
    
#Salary v Cost of Living
    
    output$median.income <- renderPlot(
        realtor_income %>% 
            ggplot(aes(x=area, y=median.income)) +
            geom_bar(stat="identity") +
            xlab(NULL) +
            ylab("Median Income ($)") +
            scale_y_continuous(limits=c(0,8E6)) +
            ggtitle("Median Income in Various Cities") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            theme(text = element_text(size=14))
            
    )
    
    output$income <- renderPlot(
        realtor_income %>% 
            ggplot(aes(x=reorder(median.income, price), price)) +
            geom_line(aes(color=area)) +
            ggtitle("Price of Real Estate vs. Median Income in Select Cities") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            theme(text = element_text(size=14)) +
            xlab(NULL)+
            ylab("Price of Real Estate")
    
    
    )
    
    output$price_income <- renderPlot(
        price_per_income %>% 
            ggplot(aes(x=reorder(area, price_per_median.income), price_per_median.income)) +
            geom_boxplot(aes(color=area))+
            xlab(NULL) +
            ylab("Price of Real Estate / Median Income") +
            ggtitle("Price of Real Estate per Median Income in Select Cities") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            theme(text = element_text(size=14))
        
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
