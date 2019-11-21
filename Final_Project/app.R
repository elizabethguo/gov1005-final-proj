library(shiny)
library(readxl)
library(janitor)
library(tidyverse)

justicesdata <- read_csv("http://epstein.wustl.edu/research/justicesdata.csv",
                         col_types = cols(
                             .default = col_character(),
                             yrnom = col_double(),
                             biryear = col_double(),
                             agenom = col_double(),
                             congress = col_double(),
                             nomsen = col_double()
                         ))

# For justices who actually made it onto the bench. I tried "distinct()" and got the same number of rows. So no justices are repeated.

justices <- justicesdata %>% 
    filter(success == 1) 

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("Demographics of the U.S. Supreme Court Over Time",
               tabPanel("Religious Makeup",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(inputId = "religion_slider",
                                            label = "Year",
                                            min = 1789,
                                            max = 2018,
                                            step = 10,
                                            value = 1789),
                            ),
                            mainPanel(
                                plotOutput("religionPlot")
                            )
                        )
                        
               ),
               
               tabPanel("Family Economic Status Makeup",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(inputId = "ec_slider",
                                            label = "Year",
                                            min = 1789,
                                            max = 2018,
                                            step = 10,
                                            value = 1789),
                            ),
                            mainPanel(
                                plotOutput("ecPlot")
                            )
                        )
                        
               ),
               
               tabPanel("Racial Makeup",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(inputId = "race_slider",
                                            label = "Year",
                                            min = 1789,
                                            max = 2018,
                                            step = 10,
                                            value = 1789),
                            ),
                            mainPanel(
                                plotOutput("racePlot")
                            )
                        )
                        
               ),
               
               tabPanel("Hometown Background Makeup",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(inputId = "home_slider",
                                            label = "Year",
                                            min = 1789,
                                            max = 2018,
                                            step = 10,
                                            value = 1789),
                            ),
                            mainPanel(
                                plotOutput("homePlot")
                            )
                        )
                        
               ),
               
               tabPanel("About",
                        textOutput("aboutText")
               )
               
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$aboutText <- renderText({
        
        paste("The Project:", "Currently, sitting on the bench of the U.S. Supreme Court are 5 Catholic justices, 3 Jewish justices, and 1 Episcopalian justice. But this religious makeup would have been unthinkable just 100 years ago in a nation with deeply entrenched roots in Protestant Christianity. What changed, and when? This project examines the changing demographic makeup - including age, educational background, birth city and more - of the nine Supreme Court justices from 1789 to the present. Furthermore, this project seeks to draw connections bewteen shifts in Court demographics over time with shifting political tides and events in U.S. history.", "The Data:", "Data used in this project comes from Washington University of St. Louis' Professor Lee Epstein's U.S. Supreme Court Database. This public database was last updated on July 30, 2019 and includes data on all U.S. Supreme Court nominees (including those unconfirmed). I will focus on the justices who were confirmed and served on the Court. Data also comes from Washington University Law's Supreme Court Database. Information on justices and legal provisions was found from this database.", "About Elizabeth Guo:", "I am a sophomore at Harvard concentrating in Physics & Mathematics with a secondary in Government. I am very interested in the intersection of law and the quantitative. Contact me at elizabethguo@college.harvard.edu.")
        
    })
    
    
    datareact1 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$religion_slider) %>% 
            count(nomrelig)
    })
    
    output$religionPlot <- renderPlot({
        
        datareact1() %>%  
            ggplot(aes(x = nomrelig, y = n)) +
            geom_col() +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Religion Nominated in Selected Decade"),
                 x = "Religion",
                 y = "Count")
        
        
    })
    
    datareact2 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$ec_slider) %>% 
            count(famses)
    })
    
    output$ecPlot <- renderPlot({
        
        datareact2() %>%  
            ggplot(aes(x = famses, y = n)) +
            geom_col() +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Family Economic Status Nominated in Selected Decade"),
                 x = "Economic Status",
                 y = "Count")
        
        
    })
    
    datareact3 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$race_slider) %>% 
            count(race)
    })
    
    output$racePlot <- renderPlot({
        
        datareact3() %>%  
            ggplot(aes(x = race, y = n)) +
            geom_col() +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Race Nominated in Selected Decade"),
                 x = "Race",
                 y = "Count")
        
        
    })
    
    datareact4 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$home_slider) %>% 
            count(childsur)
    })
    
    output$homePlot <- renderPlot({
        
        datareact4() %>%  
            ggplot(aes(x = childsur, y = n)) +
            geom_col() +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Hometown Background Nominated in Selected Decade"),
                 x = "Hometown Background",
                 y = "Count")
        
        
    })
    
}

shinyApp(ui = ui, server = server)
