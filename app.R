# Load packages ----
library(readr)
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(shinyWidgets)

# Load data ----
foodprice <- read.csv("food price.csv")
namelist <- unique(foodprice$Food_Name)
monthlist <- unique(foodprice$Month)
foodprice$Date <- my(paste(foodprice$Month,"-", foodprice$Year))

# calculate R square for loess regression
# https://fibosworld.wordpress.com/2012/11/04/loess-regression-with-r/


# User interface ----
ui <- fluidPage(
    
    # Application title
    titlePanel(strong("🍅📈New Zealand Food Price Dashboard 2020")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
    
            # image
            img(src = "food3.jpg", height = 400),
            br(),
            br(),
            h6("Version Update:"),
            h6(em("20jul21 - fixed slider bug; fixed bar chart bug; 
                  changed summary statistics description")),
            br(),
            
            # choose food
            selectInput("food", 
                        strong("Choose a type of food"),
                        choice = namelist,
                        selected = namelist[10]),

            # choose date
            setSliderColor("dodgerblue", 1),
            sliderInput("period",
                        strong("Choose a period"),
                        min = min(foodprice$Date),
                        max = max(foodprice$Date),
                        value = c(min(foodprice$Date), max(foodprice$Date)),
                        timeFormat="%Y-%m"),        
            br(),
            
            "⬇️",
            # text output
            htmlOutput("food"),
            htmlOutput("trendtext"),
            br(),
            htmlOutput("stat"),
            br(),
            br(),
            h4(strong("Monthly average price of your chosen period")),
            plotOutput("monthplot"),
            br(),
            h4(strong("Price table by Year & Month of your chosen food")),
            DT::dataTableOutput("mytable"),
            br()
    
        ),
        
        mainPanel(
            h4(strong("Historical food price of your chosen period")),
            plotOutput("trendplot"),
            
            h4(strong("Food price trend of your chosen period")),
            "regression smoothing method: loess",
            plotOutput("linearplot"),
            
            # choose food to compare
            
            h4(strong(span("Newly Added Feature - Food Price Comparison!", 
                           style ="color: green"))),
            "Choose another four food below:",
            
            fluidRow(
                
                column(3,
            selectInput("foodcompare1", 
                        " ",
                        choice = namelist,
                        selected = namelist[12])),  
            column(3,
            selectInput("foodcompare2", 
                        " ",
                        choice = namelist,
                        selected = namelist[7])),       
            column(3,
            selectInput("foodcompare3", 
                        " ",
                        choice = namelist,
                        selected = namelist[9])),  
            column(3,
            selectInput("foodcompare4", 
                        " ",
                        choice = namelist,
                        selected = namelist[8]))
            ),
            
            
            fluidRow(
                
                column(3,
                "choose a month:",
                selectInput("month", 
                            " ",
                            choice = monthlist,
                            selected = namelist[6])),
                
                column(4,
                "choose a year range, for example 5 = last 5 years:",
                numericInput("year", 
                             "", 
                             value = 5, max = 15)),
            ),
            
            "⬇️",
            htmlOutput("comparetext"),
            
            h4(strong("Food price comparison of the selected month")),
            plotOutput("compare"),
            
            p("Data source: ",
              a("Statistics New Zealand", 
                href = "https://www.stats.govt.nz/information-releases/food-price-index-june-2021"),
              align = "right"),
            p(strong("Dashboard created by Lillian Lu, on 19 Jul 2021"),
              br(),
              em("contact me: lillianlu.nz@gmail.com"), 
              align = "right")
        )
    )
)

# server logic ----
server <- function(input, output, session) {
    
    food <- reactive({
        foodprice %>%
            filter(Date >= input$period[1] & Date <= input$period[2] &
                       Food_Name == input$food)
    })
    
    foodcompare <- reactive({
        foodprice %>%
            filter(Food_Name == input$foodcompare1 |
                       Food_Name == input$foodcompare2 |
                       Food_Name == input$foodcompare3 |
                       Food_Name == input$foodcompare4 |
                       Food_Name == input$food) %>%
            filter(Month == input$month & Year == 2021-input$year) %>%
            group_by(Food_Name) %>%
            summarise(price_average = mean(Price))
    })

    # food selected
    output$food <- renderText({ 
        paste("You have selected <b>", input$food, "</b>")
    })
    
    # trend selected
    output$trendtext <- renderText({ 
        paste("You have selected the date range from <b>", input$period[1], 
              "</b> to <b>",input$period[2], "</b>")
    })
    
    # time series output
    output$trendplot <- renderPlot({
            ggplot(food(), aes(x=Date, y=Price)) +
            geom_line(size = 1, colour = "dodgerblue") + geom_point(size = 2, colour = "red") +
            scale_y_continuous(labels=scales::dollar_format(),
                               breaks = scales::pretty_breaks(n = 10)) +
            theme(
                panel.grid.minor = element_blank(),
                axis.title.x = element_text(size = 16),
                axis.text.x = element_text(size = 14),
                axis.title.y = element_text(size = 16),
                axis.text.y = element_text(size = 14)) 
    })
    
    # monthly average output
    output$monthplot <- renderPlot({
            food() %>%
            group_by(Month) %>%
            summarise(month_average = mean(Price)) %>%
            ggplot(aes(x=Month, y=month_average)) + theme_minimal() +
            theme(panel.grid.minor = element_blank()) +
            geom_bar(stat = "identity", fill="dodgerblue") +
            scale_y_continuous(name = "average price",
                               labels=scales::dollar_format(),
                               breaks = scales::pretty_breaks(n = 10)) +
            scale_x_discrete(limits=c("Jan", "Feb", "Mar","Apr","May","Jun",
                                      "Jul","Aug","Sep","Oct","Nov","Dec"))
    })
    
    # regression output
    output$linearplot <- renderPlot({
            ggplot(food(), aes(x=Date, y=Price)) + geom_smooth(colour = "red") +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(
                axis.title.x = element_text(size = 16),
                axis.text.x = element_text(size = 14),
                axis.title.y = element_text(size = 16),
                axis.text.y = element_text(size = 14)) 
    })
    
    # summary output
    output$stat <- renderText({ 
        paste("In the last 15 years (2006-2021), the average price of ", input$food, " was  $",
              round(mean(foodprice$Price[foodprice$Food_Name == input$food]),2),             
              ', <span style="color:green"> the lowest price was $', 
              min(foodprice$Price[foodprice$Food_Name == input$food], na.rm=TRUE),
              ', <span style="color:red"> the highest price was $', 
              max(foodprice$Price[foodprice$Food_Name == input$food], na.rm=TRUE), "</span>.")
    })
    
    #### table output ####
    options(DT.options = list(pageLength = 5))
    
    output$mytable <- DT::renderDataTable({
            foodprice %>%
            filter(Food_Name == input$food) %>%
            group_by(Year, Month) %>%
            select(Food_Name, Year, Month, Price) %>%
            arrange(desc(Year))
    })
    
    # choose five food to compare
    output$compare <- renderPlot({
        ggplot(foodcompare(), aes(x = reorder(Food_Name, -price_average), y = price_average, 
                                  fill = Food_Name)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            scale_y_continuous(name = "average price",
                               labels=scales::dollar_format(),
                               breaks = scales::pretty_breaks(n = 10)) +
            scale_fill_brewer(palette="Dark2") +
            theme(legend.position="none",
                  axis.title.x = element_text(size = 16),
                  axis.text.x = element_text(size = 14),
                  axis.title.y = element_text(size = 16),
                  axis.text.y = element_text(size = 14)) +
            xlab("Name of Food")
    })
    
    output$comparetext <- renderText({ 
        paste("You are comparing the average price of ", input$food, input$foodcompare1,
              input$foodcompare2, input$foodcompare3, " and ", input$foodcompare4,
              " in month ", input$month, 
              " between ", 2021-input$year , " and 2021.")
    })

    session$onSessionEnded(function(){
        stopApp()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
