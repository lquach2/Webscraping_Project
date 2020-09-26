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

realtor <- read.csv('./realtor.csv', header=T, na.strings=c("","NA"))


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

avg.real.estate <- realtor %>% 
    group_by(area) %>% 
    summarise(avg.real.estate = mean(price))

real_estate.income <- merge(avg.real.estate, income, by ="area") 
    
x <- br()

avg_price_home <- realtor %>% 
    select(price, area, state) %>% 
    group_by(area) %>% 
    mutate(avg_price_home = mean(price), area) 

realtor_income <- merge(avg_price_home, income, by="area")


price_per_income <- realtor_income %>% 
    mutate(price_per_median.income = (price/median.income))

# Define UI for application that draws a histogram

ui <- fluidPage(
    
    titlePanel("Limited Guide to Living in Selected Large Cities"),
    
    navlistPanel(
        "Table of Contents",
        tabPanel("Overview",
                 p("Exploration of Living in Selected USA Cities", style="font-size:20px"),
                 x, 
                 plotOutput(outputId="price_distribution",
                            height="600px"),
                 p("Majority of the cities explored are priced below 2.5 million", style="font-size:14px"),
                 x,
                 plotOutput(
                         outputId = "price_area",
                         height = "600px"),
                 p("The average price of real estate is highest in los angeles, with SJ and SF coming in second. 
                   The houses in Charlotte, Philadelphia, and Phoneix have a more narrow distribution, with most houses being price similarly. 
                   In LA, SF, NYC, SJ, you can find larger distribution of priced homes.", style="font-size:14px"),
                 x,
                 p(plotOutput(outputId="avg_sqft",
                              height = "600px")),
                 p("You can find the most sqft in LA, Denver, Houston, Phoenix and Charlotte.", style="font-size:14px"),
                 x,
                 p(plotOutput(outputId="price_sqft",
                              height ="600px")),
                 p("By sqft, you pay the most per sqft in SF, followed by NYC, LA, Seattle, SJ and DC.", style="font-size:14px")),

        tabPanel("Deeper Dive",
                 tabsetPanel(
                     tabPanel("Property Details",
                        p(selectizeInput(inputId = "area",
                                    label = "area",
                                    choices = unique(realtor_price_sqft$area)),
                        x, p(plotOutput(outputId="costsqft_zip",
                                height="600px")),
                        p("Viewing the most expensive neighborhoods by zip code to live from those that were scraped. As a SF resident,
                          the most costly per sqft happens to be near the water.", style="font-size:14px")),
                        x, p(plotOutput(outputId = "realtor_num_bed_baths",
                                height="600px")),
                        p("On average, majority of real estate has less than 3 baths and 4 bedrooms. Cities like LA have close
                          to the same number of beds as baths. Cities like Charlotte and Phoenix prioritizes bedrooms over baths.
                          Places with least amount of baths are likely to be in DC, SF, and NYC.", style="font-size:14px"),
                        x, p(plotOutput(outputId = "garage",
                                height="600px")),
                        p("Many listings did not include garage space but the ones that did are places like Philadelphia, Denver and Chicago.", style = "font size:14px"),
                        x, p(plotOutput(outputId="year_built",
                                height="600px")),
                        p("Majority of the newer houses are in Charlotte, Houston, Phoenix and SJ while the older houses are in Philadelphia,
                          NYC, and DC. Chicago has the biggest range of old to new houses while Charlotte mostly have newer houses.", style="font-size:14px"),
                        x, p(plotOutput(outputId="year_built_sqft",
                                height="600px")),
                        p("The observed distribution of sqft decreases as time goes on,",style="font-size:14px")),
                     
                     tabPanel("Property Types",
                              p("Types of Properties", style="font-size:17px"),
                              p(plotOutput(outputId="property_type",
                                           height="600px")),
                              p("Majority of the listings scraped are single family homes and townhouses/condos.", style="font-size:14px"),
                              x,
                              p(plotOutput(outputId="property_price",
                                           height="600px")),
                              x,
                              p("Majority of the listings in LA are single family homes while in NYC, it’s mostly co-ops and condos.", style="font-size:14px")))),
        
        tabPanel("Popular Description",
                 tabsetPanel(
                     tabPanel("Popular Description",
                              p("Description Frequency", style="font-size:17px"), 
                              p("Most frequently used words in listing descriptions."),
                              wordcloud2Output(
                                  outputId = "wordcloud",
                                  width ="100%",
                                  height = "600px")))),
        tabPanel("Taxes",
                 tabsetPanel(
                     tabPanel("Property Taxes",
                              p("Taxes", style="font-size:17px"),
                              plotOutput(
                                  outputId = "taxes",
                                  height = "800px"),
                              x,
                              p("For the past 5 years, real estate tax has remain pretty steady in their respective city. 
                                The largest distribution in real estate tax resides LA and SF.", style="font-size:14px"),
                              plotOutput(
                                  outputId="avgtax",
                                  height="600px"),
                              p("SF pays the most property tax on average followed by LA and NYC.", stlye="font-size:14px"),
                              x,
                              plotOutput(
                                  outputId="avgtax_year",
                                  height="600px"),
                              x,
                              p("On average, taxes have steadily increased except for SF and LA. 
                                SF which jumped >30% more in taxes since 2015.", style="font-size:14px")
                              ))),
        
        tabPanel("Median Income v Cost of Living",
                 tabsetPanel(
                     tabPanel("Income",
                              p("Income vs. COL", style="font-size:17px"),
                              plotOutput(
                                  outputId="median.income",
                                  height="600px"),
                              p("Highest median income in selected cities are in SF and SJ, with the lowest in Philadelphia.", style="font-size:14px"),
                              x,
                              plotOutput(
                                  outputId = "price_income",
                                  height = "600px"),
                              x, 
                              p(plotOutput(
                                  outputId="income",
                                  height="600px")),
                              x,
                              p("The price of real estate per yearly median income is highest in California cities and NYC. LA would probably be
                                the most difficult place to buy real estate based on median income.
                                In summary, depending on what you're looking for, whether it's a family home, condo, or cost of living, the best values
                                are cities like Charlotte where home price to income ratio is low, you have a mix selection of condos, single family homes, steady
                                to low property taxes, a selection of newer homes, decent sqft, low price/sqft and a decent median income.", stlye = "font-size:14px"),
                              x,
                                p("Cities like LA are the worse value, as cost per sqft is highest in all the cities evaluated, higher property taxes, mid to high price/sqft,
                                high home price to income ratio while also having a similar median income to Charlotte. However, the only benefit to residing
                                in LA based on the observed evaluation might be the high number of bedrooms and bathrooms available.", stlye = "font-size:14px")
                              )))
    )
)


# Define server logic required to draw a histogram

server <- function(input, output) { 

#Overview
    
    output$price_distribution <- renderPlot(
        realtor %>% 
            ggplot(aes(price)) +
            geom_histogram(aes(fill=area)) +
            xlab("Price of Real Estate ($)") +
            ggtitle("Distribution of Cost of Real Estate Homes in Select Cities") +
            theme(text = element_text(size=14))
        
    )    
    
    output$price_area <- renderPlot(
        realtor %>% 
        ggplot(aes(x=reorder(area, price), price)) +
        geom_boxplot(aes(fill=area)) +
        coord_flip() +
        scale_y_continuous(limits=c(0,5E6))+
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
            ggplot(aes(x=number_BedBath, y = area)) +
            geom_boxplot(aes(fill=bed_bath)) + 
            geom_jitter(aes(color=bed_bath), alpha = 0.3) +
            ggtitle("Distribution of bed/baths by area") +
            ylab(NULL)+
            xlab("Number of Bed and Baths per Unit") +
            scale_x_continuous(limits=c(0,8)) +
            theme(text = element_text(size=14))
        
    )

    
    output$bedbaths_sqft <- renderPlot(
        realtor_num_bed_baths %>% 
            ggplot(aes(x=area, y=sqft)) +
            geom_boxplot(aes(fill=bed_bath)) + 
            geom_jitter(aes(color=bed_bath), alpha = 0.3) +
            coord_flip()
    )
#Property Type

    output$property_type <- renderPlot(
        realtor %>% 
            ggplot(aes(property_type)) +
            geom_histogram(stat="count") +
            xlab(NULL) +
            scale_y_continuous(limits=c(0,200)) +
            ggtitle("Distribution of Cost of Real Estate Homes in Select Cities") +
            theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
            theme(text = element_text(size=14))
        
    )
    
    output$property_price <- renderPlot(
        realtor %>% 
            ggplot(aes(area)) +
            geom_histogram(aes(fill=property_type), stat="count")+
            xlab(NULL) +
            scale_y_continuous(limits=c(0,50)) +
            ggtitle("Distribution of Cost of Real Estate Homes in Select Cities") +
            theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
            theme(text = element_text(size=14))
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
            ylab("Garage Space")+
            ggtitle("Garage Space Offered") +
            theme(text = element_text(size=14))
        
    
    )
    


    output$year_built <- renderPlot(
        realtor %>% 
            ggplot(aes(x=year_built, y=area)) +
            geom_boxplot(aes(fill=area)) + 
            scale_x_date(breaks = pretty_breaks(10))+
            xlab("Year Built") +
            ylab(NULL)+
            ggtitle("Real Estate in Selected Cities and Year Built")+
            theme(text = element_text(size=14))
        

    )
    
    output$year_built_sqft <- renderPlot(
        realtor %>% 
            ggplot(aes(x=year_built, y=sqft)) +
            geom_boxplot(aes(fill=area)) + 
            scale_x_date(breaks= pretty_breaks(10))+
            ggtitle("Real Estate Year Built vs. Sqft")+
            xlab("Year Built")+
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
            ggplot(aes(x=reorder(area,avgtax), avgtax)) +             
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
            theme(text = element_text(size=14)) +
            xlab(NULL) +
            ylab("Average Property Tax")
        
        
    )
    
#Salary v Cost of Living

    output$median.income <- renderPlot(
        income %>% 
            ggplot(aes(x=reorder(area, median.income), median.income)) +
            geom_bar(stat="identity") +
            xlab(NULL) +
            ylab("Median Income ($)") +
            scale_y_continuous(limits=c(0,120000)) +
            ggtitle("Median Income in Various Cities") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            theme(text = element_text(size=14))
            
    )
    
    output$income <- renderPlot(
        real_estate.income %>% 
            ggplot(aes(x=reorder(median.income, avg.real.estate), avg.real.estate)) +
            geom_bar(aes(fill=area), stat="identity") +
            ggtitle("Price of Real Estate vs. Median Income in Select Cities") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            theme(text = element_text(size=14)) +
            xlab(NULL)+
            #scale_x_continuous(limits=c(40000,120000)) +
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
            scale_y_continuous(limits=c(0,100)) +
            theme(text = element_text(size=14))
        
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
