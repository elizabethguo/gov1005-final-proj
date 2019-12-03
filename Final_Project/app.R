# All necessary packages.

library(shiny)
library(fs)
library(gt)
library(tidymodels)
library(rsample)
library(readxl)
library(janitor)
library(RColorBrewer)
library(shinythemes)
library(tidyverse)

# Reading in demographic data set from Epstein's website. Specify col_types here.

justicesdata <- read_csv("http://epstein.wustl.edu/research/justicesdata.csv",
                         col_types = cols(
                             .default = col_character(),
                             yrnom = col_double(),
                             biryear = col_double(),
                             agenom = col_double(),
                             congress = col_double(),
                             nomsen = col_double()
                         )) 

# Filter this dataset for justices that actually made it onto the bench, not
# just those who were nominated.

justicesdata <- justicesdata %>% 
    filter(success == '1. yes, nominee took his/her seat on the court')




# Clean this dataset for the purposes of ggplot labels later on. We categorize
# all Protestant denominations together, and clean up the labels.

justicesdata$nomrelig2 <- justicesdata$nomrelig
justicesdata$nomrelig2 <- ifelse(justicesdata$nomrelig2 %in% c('1. baptist','3. congregationalist','4. disciples of christ','5. dutch reform','8. lutheran','9. methodist','10. presbyterian','11. protestant','12. quaker'), 'Protestant', justicesdata$nomrelig2)
justicesdata$nomrelig2 <- ifelse(justicesdata$nomrelig2 %in% c('2. church of england'), 'Church of England', justicesdata$nomrelig2)
justicesdata$nomrelig2 <- ifelse(justicesdata$nomrelig2 %in% c('6. episcopalian'), 'Episcopalian', justicesdata$nomrelig2)
justicesdata$nomrelig2 <- ifelse(justicesdata$nomrelig2 %in% c('7. jewish'), 'Jewish', justicesdata$nomrelig2)
justicesdata$nomrelig2 <- ifelse(justicesdata$nomrelig2 %in% c('13. roman catholic'), 'Roman Catholic', justicesdata$nomrelig2)
justicesdata$nomrelig2 <- ifelse(justicesdata$nomrelig2 %in% c('14. unitarian'), 'Unitarian', justicesdata$nomrelig2)
justicesdata$nomrelig2 <- ifelse(justicesdata$nomrelig2 %in% c('999. unclear or unknown'), 'Unknown', justicesdata$nomrelig2)

justicesdata$famses2 <- justicesdata$famses
justicesdata$famses2 <- ifelse(justicesdata$famses2 %in% c('1. lower'), 'Lower', justicesdata$famses2)
justicesdata$famses2 <- ifelse(justicesdata$famses2 %in% c('2. lower-middle', '3. middle'), 'Middle', justicesdata$famses2)
justicesdata$famses2 <- ifelse(justicesdata$famses2 %in% c('4. upper-middle', '5. upper'), 'Upper', justicesdata$famses2)

justicesdata$race2 <- justicesdata$race
justicesdata$race2 <- ifelse(justicesdata$race2 %in% c('0. white'), 'White', justicesdata$race2)
justicesdata$race2 <- ifelse(justicesdata$race2 %in% c('1. black'), 'Black', justicesdata$race2)
justicesdata$race2 <- ifelse(justicesdata$race2 %in% c('2. Hispanic origin'), 'Hispanic', justicesdata$race2)





# Read in the demographic dataset (again).

justicedem <- read_xlsx("justicesdata.xlsx")



# The following lines of code were used to read in the vote dataset, subset it
# to save memory by only selecting the 3 variables we need, and write it as a
# new csv that we will use. I then deleted the old full dataset, which was very
# large.

# justicevote <- read_xlsx("SCDB_2019_01_justiceCentered_Citation3.xlsx")
# justicevote <- justicevote %>% 
# select("justice", "term", "direction")
# justicevote <- write_csv(justicevote, "justicevote.csv")



# Reading in the new subsetted vote dataset. Specify col_types.

justicevote <- read_csv("justicevote.csv",
                        col_types = cols(
                            justice = col_double(),
                            term = col_double(),
                            direction = col_double()
                        ))



# Join the demographic dataset and subsetted vote dataset by unique common
# justice identification number. I filtered out the datapoints in which
# direction was unknown, because these would be useless in our model, and there
# are plenty of other (80,000+) observations we could use as data. I made new
# variables using mutate. I changed direction, which was originally coded as
# either 1 or 2 for Conservative or Liberal, respectively, into a factor as
# "votedirection" for either "Conservative" or "Liberal." I did this to avoid
# potential confusion with the non-binary number 2 and to avoid confusion with
# coding the parties as 0 or 1. (I also recoded the original variable "parnom"
# for Republican party as 1 and the Democratic party as 2 in the new variable
# "party", since they were other numbers in the original data.)


fullvotes <- justicevote %>% 
    inner_join(justicedem, by = c("justice" = "spaethid")) %>% 
    filter(!is.na(direction)) %>% 
    filter(yrnom >= 1948) %>% 
    mutate(votedirection = as.factor(ifelse(direction == 1, "Conservative", "Liberal"))) %>% 
    mutate(party = as.numeric(ifelse(parnom == 6, 0, 1)))




# Logistic regressions and machine learning predictions for 4 models to try to
# explain justice vote directions with various combinations of explanatory
# factors. I then put the evaluation data into gt tables. 

gt_model1 <- logistic_reg() %>%
    set_engine("glm") %>%
    fit(data = fullvotes, votedirection ~ nomrelig + famses + race + childsur) %>%
    predict(new_data = fullvotes) %>%
    bind_cols(fullvotes) %>%
    metrics(truth = votedirection, estimate = .pred_class) %>%
    gt() %>% 
    tab_header("Model 1 Evaluation on 1948-2018")

gt_model2 <- logistic_reg() %>%
    set_engine("glm") %>%
    fit(data = fullvotes, votedirection ~ parnom) %>%
    predict(new_data = fullvotes) %>%
    bind_cols(fullvotes) %>%
    metrics(truth = votedirection, estimate = .pred_class) %>%
    gt() %>% 
    tab_header("Model 2 Evaluation on 1948-2018")

gt_model3 <- logistic_reg() %>%
    set_engine("glm") %>%
    fit(data = fullvotes, votedirection ~ prespart) %>%
    predict(new_data = fullvotes) %>%
    bind_cols(fullvotes) %>%
    metrics(truth = votedirection, estimate = .pred_class) %>%
    gt() %>% 
    tab_header("Model 3 Evaluation on 1948-2018")

gt_model4 <- logistic_reg() %>%
    set_engine("glm") %>%
    fit(data = fullvotes, votedirection ~ nomrelig + famses + race + childsur + parnom + prespart) %>%
    predict(new_data = fullvotes) %>%
    bind_cols(fullvotes) %>%
    metrics(truth = votedirection, estimate = .pred_class) %>%
    gt() %>% 
    tab_header("Model 4 Evaluation on 1948-2018")




# Define UI for application. I chose the "flatly" shinytheme.

ui <- fluidPage(theme = shinytheme("flatly"),
                
                
                
                # Title of my project, which I put at the head of the navigation bar.
                
                navbarPage("Demographics and Votes of the U.S. Supreme Court",
                           
                           
                           
                           
                           # The first tab is About, which just consists of one page
                           # explaining the setup of my project.
                           
                           tabPanel("About",
                                    mainPanel(
                                        
                                        h2("Motivation"),
                                        p("Currently, sitting on the bench of the U.S. Supreme Court are 5 Catholic justices, 3 Jewish justices, and 1 Episcopalian justice. But this religious makeup would have been unthinkable just 100 years ago in a nation with deeply entrenched roots in Protestant Christianity. What has this change looked like? Moreover, can we use certain demographic information - including religion, economic background, affiliated party, and more - about a justice to predict how he or she may vote? This project examines various characteristics of Supreme Court justices to see if we can create a model to predict a liberal or conservative vote."),
                                        
                                        
                                        h2("The Data"),
                                        p("Data used in this project comes from Washington University of St. Louis' Professor Lee Epstein's U.S. Supreme Court Database. This public database was last updated on July 30, 2019 and includes data on all U.S. Supreme Court nominees (including those unconfirmed). I will focus on the justices who were confirmed and served on the Court."),
                                        p("Data also comes from Washington University Law's Supreme Court Database. Information on justices and their votes on each case was found from this database."),
                                        
                                        
                                        p(strong("Citations"), style = "font-size:80%"),
                                        p("Epstein, Lee, Thomas G. Walker, Nancy Staudt, Scott Hendrickson, and Jason Roberts. (2019). The U.S. Supreme Court Justices Database. February 11.", a("See data.", href="http://epstein.wustl.edu/research/justicesdata.html"), style = "font-size:80%"),
                                        p("Harold J. Spaeth, Lee Epstein, Andrew D. Martin, Jeffrey A. Segal, Theodore J. Ruger, and Sara C. Benesh. 2019 Supreme Court Database, Version 2019 Release 01.", a("See data.", href="http://Supremecourtdatabase.org"), style = "font-size:80%"),
                                        
                                        
                                        
                                        h2("About Elizabeth Guo"),
                                        p("I am a sophomore at Harvard concentrating in Physics & Mathematics with a secondary in Government. I am very interested in the intersection of law and the quantitative."),
                                        p("Contact me at elizabethguo@college.harvard.edu."),
                                        
                                        
                                        
                                        h2("Source Code"),
                                        p("The source code for this Shiny App can be found at my GitHub", a("here.", href="https://github.com/elizabethguo/gov1005-final-proj")),
                                        br(),
                                        br()
                                        
                                    )),
                           
                           
                           
                           
                           
                           # The second tab consists of six subtabs (create with
                           # tabsetPanel) to show changes in numbers of various demographic
                           # factors of the Court over time. I used a sidebarPanel format
                           # with a slider so viewers can choose which year to look at the
                           # cumulative numbers. The ggplots go into the mainPanel. In the
                           # sidebar, I also included some historical information as well as
                           # coding notes if I personally made decisions in cleaning and
                           # categorizing the original data.
                           
                           
                           tabPanel("Court Demographics Over Time",
                                    
                                    tabsetPanel(
                                        
                                        tabPanel("Religion",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         sliderInput(inputId = "religion_slider",
                                                                     label = "Year",
                                                                     min = 1789,
                                                                     max = 2018,
                                                                     step = 10,
                                                                     value = 1789),
                                                         br(),
                                                         br(),
                                                         h5("Dragging the slider toward 2018, we can see that the religions with which justices associate have diversified over the years. Specifically, in the past 100 years, the numbers of Roman-Catholic and Jewish justices have experienced relative growth."),
                                                         
                                                         h6(em("Coding Notes")),
                                                         h6(em("Here, I have coded Baptist, Congregationalist, Disciples of Christ, Dutch Reform, Lutheran, Methodist, Presybyterian, Protestant, and Quaker justices all as Protestant, since these are different branches of Protestantism.")),
                                                         br()
                                                     ),
                                                     mainPanel(
                                                         br(),
                                                         br(),
                                                         plotOutput("religionPlot"),
                                                         
                                                         
                                                     )
                                                 )
                                        ),
                                        
                                        
                                        tabPanel("Family Economic Status",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         sliderInput(inputId = "ec_slider",
                                                                     label = "Year",
                                                                     min = 1789,
                                                                     max = 2018,
                                                                     step = 10,
                                                                     value = 1789),
                                                         br(),
                                                         br(),
                                                         h5("Justices have historically come from an Upper socioeconomic class background. In the past 50 years, however, there has been a relative increase in the number of justices from a Middle socioeconomic class background."),
                                                         
                                                         h6(em("Coding Notes")),
                                                         h6(em("Here, I have coded Lower-Middle and Middle together as the Middle category, and Upper-Middle and Upper together as the Upper category. Having 3 larger categories as opposed to 5 makes the socioeconomic breakdown easier to see.")),
                                                         br()
                                                     ),
                                                     mainPanel(
                                                         br(),
                                                         br(),
                                                         plotOutput("ecPlot"),
                                                         br(),
                                                         
                                                     )
                                                 )
                                        ),
                                        
                                        tabPanel("Race",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         sliderInput(inputId = "race_slider",
                                                                     label = "Year",
                                                                     min = 1789,
                                                                     max = 2018,
                                                                     step = 10,
                                                                     value = 1789),
                                                         br(),
                                                         br(),
                                                         h5("The first non-white justice was Thurgood Marshall, who was the first African-American justice and was appointed in 1967. Besides Justice Marshall, there have been 2 other non-white Supreme Court justices: Justice Clarence Thomas, appointed in 1991, and Justice Sonia Sotomayor, appointed in 2009."),
                                                         br()
                                                     ),
                                                     mainPanel(
                                                         br(),
                                                         br(),
                                                         plotOutput("racePlot"),
                                                         br()
                                                     )
                                                 )
                                        ),
                                        
                                        tabPanel("Home Background",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         sliderInput(inputId = "home_slider",
                                                                     label = "Year",
                                                                     min = 1789,
                                                                     max = 2018,
                                                                     step = 10,
                                                                     value = 1789),
                                                         br(),
                                                         br(),
                                                         h5("In 1900, justices had historically mostly come from rural home backgrounds. By 2018, the numbers of justices from small towns had risen, but perhaps most noteworthy is that there have been more justices from urban backgrounds than any other background."),
                                                         br()
                                                     ),
                                                     mainPanel(
                                                         br(),
                                                         br(),
                                                         plotOutput("homePlot"),
                                                         br()
                                                     )
                                                 )
                                        ),
                                        
                                        tabPanel("Party Affiliation",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         sliderInput(inputId = "party_slider",
                                                                     label = "Year",
                                                                     min = 1789,
                                                                     max = 2018,
                                                                     step = 10,
                                                                     value = 1789),
                                                         br(),
                                                         br(),
                                                         h5("The Democratic Party was formerly known as the Democratic-Republican Party (founded in 1792 and dissolved in 1825). The modern two-party system comprising the Democratic and Republican parties arose in the 1850s: start the slider there to only observe changes in the modern two-party breakdown."),
                                                         br()
                                                     ),
                                                     mainPanel(
                                                         br(),
                                                         br(),
                                                         plotOutput("partyPlot"),
                                                         br()
                                                         
                                                     )
                                                 )
                                        ),
                                        
                                        tabPanel("Nominating Pres. Party",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         sliderInput(inputId = "pres_slider",
                                                                     label = "Year",
                                                                     min = 1789,
                                                                     max = 2018,
                                                                     step = 10,
                                                                     value = 1789),
                                                         br(),
                                                         br(),
                                                         h5("The President of the United States nominates Supreme Court Justices, who then must be confirmed by the Senate. This plot depicts the party affiliation of the presidents who nominated justices. The Democratic Party was formerly known as the Democratic-Republican Party (founded in 1792 and dissolved in 1825). The modern two-party system comprising the Democratic and Republican parties arose in the 1850s: start the slider there to only observe changes in the modern two-party breakdown."),
                                                         h5("To note, President Franklin D. Roosevelt, Democrat, appointed 9 Supreme Court Justices during his presidency from 1933 to 1945. President Richard Nixon, Republican, appointed 4 Supreme Court Justices during his presidency from 1969 to 1974. By dragging the slider over these years, one can see a noticeable growth in the Democratic and Republican columns, respectively."),
                                                         br()
                                                     ),
                                                     mainPanel(
                                                         br(),
                                                         br(),
                                                         plotOutput("presPlot"),
                                                         br()
                                                     )
                                                 )
                                        )
                                    )),
                           
                           
                           
                           
                           
                           # The third tab contains 5 subtabs. Again, I used tabset Panel.
                           # The first subtab contains notes on my models, while the other
                           # four contain a description of the logistic regression used, a
                           # gt table of an evaluaton of the model, as well as a brief
                           # written analysis on what the evaluation means.
                           
                           
                           tabPanel("Can We Predict Justice Votes? 1948 - 2018",
                                    
                                    tabsetPanel(
                                        
                                        tabPanel("Notes",
                                                 mainPanel(
                                                     h2(strong("Notes on the Following Models")),
                                                     h5(strong("This Dataset")),
                                                     h5("The dataset used to produce the following 4 models was a result of the joining of 2 separate datasets. The first, from Professor Lee Epstein's website, contains the demographic characteristic information for each Supreme Court justice. The second, from the Modern Supreme Court Database, contains information on the vote of each individual justice in each Supreme Court case from 1948 to 2018. By joining these two datasets through a common unique identification number for each justice, we have formed a dataset that contains demographic and vote information for each justice, for each time that he or she voted in the specified years. Though the previous page deals with a historic visualization of demographic change in the Supreme Court, we focus on the year 1948 to 2018 here in order to avoid any confusion about changing ideologies of political parties. In short, we focus on the modern era of politics and the Supreme Court for purposes of consistency and relevance."),
                                                     
                                                     h5(strong("The Goal")),
                                                     h5("Our aim here is to see if it is possible to predict a justice's vote direction in a Supreme Court case based on various or a combination of personal characteristics. To do so, we employ machine learning techniques - in each of the 4 trials, we run a logistic regression with the data we have, make a prediction for how the justice will vote in each datapoint using our generated model, and then compare it to the real dataset to ascertain our model accuracy and kappa percentage values (to be explained in the following sections)."),
                                                     
                                                     h5(strong("The Meaning of a \"Vote Direction\"")),
                                                     h5("Here, we categorize a particular vote direction as being either conservative or liberal. The Modern Supreme Court Database codes each justice vote according to this", a("definition.", href = "http://scdb.wustl.edu/documentation.php?var=decisionDirection"))
                                                 )
                                        ),
                                        
                                        
                                        
                                        
                                        
                                        # For each of the Model subpages, I included the
                                        # logistic regression formula in code font at the
                                        # top. The code is similar for each subpage.
                                        
                                        
                                        tabPanel("Model 1 - Personal Background",
                                                 mainPanel(
                                                     h5(strong("Logistic Regression:"), code("votedirection ~ nomrelig + famses + race + childsur")),
                                                     br(),
                                                     gt_output("personalgt"),
                                                     br(),
                                                     br(),
                                                     h4(em("Can a justice's vote direction can be explained by personal background characteristics: their religion, family socioeconomic status, race, and childhood surroundings background?")),
                                                     
                                                     h5("In our evaluation of Model 1, we see that our accuracy percentage is at 56.13%, meaning that our model predicts if a justice votes conservatively or liberally based on their personal background 56.13% of the time. The kappa value is 12.37%, meaning that our model is this much better than if we were to predict justice votes completely randomly. While the accuracy is greater than a half, it is barely so, and our kappa value also is not high enough to demonstrate that this is a convincing model.")
                                                 )
                                        ),
                                        
                                        tabPanel("Model 2 - Party Affiliation",
                                                 mainPanel(
                                                     h5(strong("Logistic Regression:"), code("votedirection ~ parnom")),
                                                     br(),
                                                     gt_output("partygt"),
                                                     br(),
                                                     br(),
                                                     h4(em("Can a justice's vote direction can be explained by their party affiliation at the time of nomination?")),
                                                     
                                                     h5("In our evaluation of Model 2, we see that our accuracy percentage is at 57.43%, meaning that our model predicts if a justice votes conservatively or liberally based on their party 57.43% of the time. The kappa value is 15.40%, meaning that our model is this much better than if we were to predict justice votes completely randomly. While the accuracy is greater than a half, it is barely so, and our kappa value also is not high enough to demonstrate that this is a convincing model. Nevertheless,", strong("this is the most accurate model out of the four examined."))
                                                 )
                                        ),
                                        
                                        tabPanel("Model 3 - Nominating President",
                                                 mainPanel(
                                                     h5(strong("Logistic Regression:"), code("votedirection ~ prespart")),
                                                     br(),
                                                     gt_output("presgt"),
                                                     br(),
                                                     br(),
                                                     h4(em("Can a justice's vote direction can be explained by the party of the president who nominated them?")),
                                                     
                                                     h5("In our evaluation of Model 3, we see that our accuracy percentage is at 52.73%, meaning that our model predicts if a justice votes conservatively or liberally based on their nominating president's party 52.73% of the time. The kappa value is 6.83%, meaning that our model is this much better than if we were to predict justice votes completely randomly. This is the weakest model out of the four.")
                                                 )
                                        ),
                                        
                                        tabPanel("Model 4 - All Factors",
                                                 mainPanel(
                                                     h5(strong("Logistic Regression:"), code("votedirection ~ nomrelig + famses + race + childsur + parnom + prespart")),
                                                     br(),
                                                     gt_output("allgt"),
                                                     br(),
                                                     br(),
                                                     h4(em("Can a justice's vote direction can be explained by a combination of all the previous factors?")),
                                                     
                                                     h5("In our evaluation of Model 4, we see that our accuracy percentage is at 56.13%, meaning that our model predicts if a justice votes conservatively or liberally based on their nominating president's party 56.13% of the time. The kappa value is 12.37%, meaning that our model is this much better than if we were to predict justice votes completely randomly. This is the second strongest model, beat closely by Model 2: Party. This demonstrates that adding in more variables as explanatory factors does not always improve a model's ability to predict. Here, it makes our accuracy worse.")
                                                 )
                                                 
                                        )
                                    )
                                    
                                    
                                    
                                    
                           ),
                           
                           
                           
                           
                           # The last tab contains Concluding Thoughts.
                           
                           tabPanel("Concluding Thoughts",
                                    
                                    tabsetPanel(
                                        
                                        
                                        
                                        # First, I plotted justice party versus vote scatterplots for
                                        # every single year in the modern era. Viewers can drag a
                                        # slider to select a certain year.
                                        
                                        
                                        # The sidebar on this page contains sliders as well as a brief
                                        # written analysis on the two plots.
                                        
                                        
                                        tabPanel("Plotting Model 2",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         sliderInput(inputId = "model2_slider",
                                                                     label = "Select Year",
                                                                     min = 1949,
                                                                     max = 2018,
                                                                     step = 1,
                                                                     value = 1949),
                                                         br(),
                                                         h6(em("Choose a year to view every individual Supreme Court justice vote that year. For each datapoint, we plot the justice's affiliated party with their vote direction in a certain case.")),
                                                         br(),
                                                         h5("Because we have coded Republican = 0, Democratic = 1 and Conservative Vote = 1, Liberal Vote = 2, if each justice were to vote in line with their party, we would expect to see a concentration of datapoints in the lower left and upper right regions (i.e. Republicans vote conservatively, and Democrats vote liberally)."),
                                                         h5("But this is not the case. Click through the years - the slope of our logistic regression line wavers but is most often quite flat in shape and only slightly positive. Justices thus do vote in party line more often than not, but this is not overwhelmingly the case."),
                                                         
                                                         br(),
                                                         h5("With all the data from 1949 to 2018 combined, we do see a slightly positive justice party-vote correlation - but still, many justices have voted out of party line in many cases.")
                                                     ),
                                                     
                                                     
                                                     # The mainPanel here contains some context for the
                                                     # guiding question, as well as the two plots in
                                                     # Output form.
                                                     
                                                     
                                                     mainPanel(
                                                         h2(strong("Are justices \"politicians in robes\"?")),
                                                         h4("Supreme Court justices are often criticized for being", a("\"politicians in robes\"", href="https://www.google.com/search?q=justices+politicians+in+robes&rlz=1C5CHFA_enUS762US762&oq=justices+politicians+in+robes&aqs=chrome..69i57.606j0j9&sourceid=chrome&ie=UTF-8"), ", meaning they simply vote according to their personal political agenda and their affiliated party line. Visualizing Model 2, which is a logistic regression for how a justice's party might affect their vote direction, may show us to what extent this stereotype is true."),
                                                         br(),
                                                         hr(em("Conclusion: Justices have voted out of expected party line quite often from 1949-2018.")),
                                                         br(),
                                                         br(),
                                                         plotOutput("model2plot"),
                                                         br(),
                                                         h5("All data from 1949-2018."),
                                                         plotOutput("model2totalplot")
                                                     )
                                                 )
                                                 
                                        ))
                           )
                           
                           
                ))

# Define server logic and different outputs referenced earlier.

server <- function(input, output) {
    
    
    
    # For the second tab, the following "over time" plots contain reactive data.
    # First define the reactive data for each subpage, referencing the input
    # data from the slider, and then code the output plot by calling the
    # reactive data.
    
    
    # Religion.
    
    
    datareact1 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$religion_slider) %>% 
            count(nomrelig2)
    })
    
    output$religionPlot <- renderPlot({
        
        datareact1() %>%  
            ggplot(aes(x = nomrelig2, y = n)) +
            geom_col(fill='darkblue') +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Religion Nominated by Selected Decade"),
                 x = "Religion",
                 y = "Count")
        
        
    })
    
    
    # Family economic status.
    
    datareact2 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$ec_slider) %>% 
            count(famses2)
    })
    
    output$ecPlot <- renderPlot({
        
        datareact2() %>%  
            ggplot(aes(x = famses2, y = n)) +
            geom_col(fill='darkblue') +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Family Economic Status Nominated by Selected Decade"),
                 x = "Economic Status",
                 y = "Count")
        
        
    })
    
    
    # Race.
    
    datareact3 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$race_slider) %>% 
            count(race2)
    })
    
    output$racePlot <- renderPlot({
        
        datareact3() %>%  
            ggplot(aes(x = race2, y = n)) +
            geom_col(fill='darkblue') +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Race Nominated by Selected Decade"),
                 x = "Race",
                 y = "Count")
        
        
    })
    
    
    # Childhood background.
    
    datareact4 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$home_slider) %>% 
            count(childsur)
    })
    
    output$homePlot <- renderPlot({
        
        datareact4() %>%  
            ggplot(aes(x = childsur, y = n)) +
            geom_col(fill='darkblue') +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Hometown Background Nominated by Selected Decade"),
                 x = "Hometown Background",
                 y = "Count")
        
    })
    
    
    # Party.
    
    datareact5 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$party_slider) %>% 
            count(parnom)
    })
    
    output$partyPlot <- renderPlot({
        
        datareact5() %>%  
            ggplot(aes(x = parnom, y = n)) +
            geom_col(fill='darkblue') +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each Party Nominated by Selected Decade"),
                 x = "Party",
                 y = "Count")
        
    })
    
    
    # Nominating president's party.
    
    datareact6 <- reactive({
        justicesdata %>% 
            filter(yrnom <= input$pres_slider) %>% 
            count(prespart)
    })
    
    output$presPlot <- renderPlot({
        
        datareact6() %>%  
            ggplot(aes(x = prespart, y = n)) +
            geom_col(fill='darkblue') +
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
            labs(title = ("Total Number of Justices of Each President's Party Nominated by Selected Decade"),
                 x = "President's Party",
                 y = "Count")
        
    })
    
    
    
    
    # For the third tab, these are the evaluations of my models, coded at the
    # top of the Shiny code and called here. I use render_gt for the gt tables.
    
    
    output$personalgt <- render_gt(
        gt_model1
    )
    
    output$partygt <- render_gt(
        gt_model2
    )
    
    output$presgt <- render_gt(
        gt_model3
    )
    
    output$allgt <- render_gt(
        gt_model4
    )
    
    
    # For the fourth tab, this is a reactive graph visualizing Model 2, vote
    # direction as explained by party.
    
    output$model2plot <- renderPlot({
        
        
        # Again, first code the reactive data which takes in input from the
        # slider. In this case, it is the term year selected.
        
        datareact7 <- reactive({
            fullvotes %>% 
                filter(term == input$model2_slider)
        })
        
        
        # Then run the logistic regression with the formula direction ~ party.
        
        glm_2 <- glm(formula = direction ~ party, data = datareact7())
        
        
        
        # Then put this into a ggplot. We use geom_jitter to see the volume of
        # individual datapoints and set alpha = 0.1 for transparency. We set
        # family = binomial because both party and vote direction are binomial.
        # Include a caption with coding description.
        
        glm_2 %>% 
            ggplot(aes(x = party, y = direction)) +
            geom_jitter(height = 0.3, width = 0.2, alpha = 0.1, fill='darkblue') +
            geom_smooth(method = "glm", method_args = list(family = "binomial"), se = TRUE) +
            labs(title = "Relationship between Justice's Party and Vote Direction in Selected Year",
                 subtitle = "Running a logistic regression",
                 x = "Party of Justice at Nomination",
                 y = "Direction of Vote",
                 caption = "Party (0 = Republican, 1 = Democrat)
       Direction (1 = Conservative, 2 = Liberal)")
        
    })
    
    
    # The code here is similar for the plot above, except this simply includes
    # all the data from 1949 - 2018, so no reactive data is needed.
    
    
    output$model2totalplot <- renderPlot({
        
        glm_2 <- glm(formula = direction ~ party, data = fullvotes)
        
        glm_2 %>% 
            ggplot(aes(x = party, y = direction)) +
            geom_jitter(height = 0.3, width = 0.2, alpha = 0.05, fill='darkblue') +
            geom_smooth(method = "glm", method_args = list(family = "binomial"), se = TRUE) +
            labs(title = "Relationship between Justice's Party and Vote Direction, Total 1949-2018",
                 subtitle = "Running a logistic regression",
                 x = "Party of Justice at Nomination",
                 y = "Direction of Vote",
                 caption = "Party (0 = Republican, 1 = Democrat)
       Direction (1 = Conservative, 2 = Liberal)")
        
    })
    
}

shinyApp(ui = ui, server = server)
