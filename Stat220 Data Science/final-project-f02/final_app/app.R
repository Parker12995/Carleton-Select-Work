library(tidyverse)
library(rvest)
library(stringr)
library(polite)
library(ggthemes)
library(shiny)
library(ggiraph)
library(plotly)
library(shinythemes)
library(tidymodels)
library(probably)


#################################


## Data Wrangling


marchmadness_raw <- read.csv("NCAATourneyFullSeasonStats_13-19.csv")
cbb_raw <- read.csv("cbb.csv")

cbb_clean <- cbb_raw %>% #Cleaned data for most of the graphs
  mutate(sweet_16 = ifelse(POSTSEASON %in% c("2ND", "Champions", "F4", "E8", 
                                             "S16"), "yes", "no")) %>%
  drop_na()

cbb_clean_model <- cbb_raw %>% #Cleaned data to be used for the model
  mutate(sweet_16 = ifelse(POSTSEASON %in% c("2ND", "Champions", "F4", "E8", 
                                             "S16"), "yes", "no")) %>%
  drop_na() %>%
  mutate(sweet_16 = as.factor(sweet_16)) %>%
  mutate(sweet_16 = fct_relevel(sweet_16, "yes", "no")) %>%
  select(-CONF, -POSTSEASON)
row.names(cbb_clean) <- str_c(cbb_clean$TEAM, cbb_clean$YEAR)
cbb_clean_model <- cbb_clean_model %>%
  select(-TEAM, -YEAR) #Non categorical/numerical columns had to be removed

cbb22 <- read_csv("cbb22.csv")
cbb22 <- cbb22 %>%
  arrange(desc(.pred_yes)) %>%
  mutate(row_num = seq.int(nrow(cbb22))) %>%
  mutate(pred_sweet_16 = ifelse(row_num <= 16, "Yes", "No")) %>%
  select(-c("prediction")) #Cleaned 2022 data

cbb22_top16 <- cbb22 %>%
  slice_max(order_by = .pred_yes, n = 16) #Predicted sweet 16 teams



## Creating the Prediction model


set.seed(1234)
cbb_split <- initial_split(cbb_clean_model, prop = 0.80, strata = sweet_16) #splitting dataset
cbb_train <- cbb_split %>%
  training()
cbb_test <- cbb_split %>%
  testing()
fitted_logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(sweet_16~., data = cbb_train)



## Draft for the shiny app


ui <- fluidPage(tags$head(tags$style("label{font-family: Georgia;}")), 
                theme = shinytheme("cyborg"),
                navbarPage(
                  HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" 
                  class="active" href="#">March Madness Data/Predictions</a>'), 
                  id="nav",
                  windowTitle = "March Madness Data and 2022 Predictions",
                  #Style inspired by Edward Parker
                  
                  tabPanel("Information About the Data", #Informational tab
                           mainPanel(
                             p(strong("What is March Madness all about?")),
                             p("Every year 64 division 1 college basketball teams convene in a massive tournament 
                             to decide who wins the National Championship. There is only one winner, and although 
                             there are always 'favorites' going in, upsets are inevitable, which is where the 
                             name March Madness comes from!"),
                             br(),
                             p("As with many sports, excited fans love to wager their bets on who will win, not 
                             only the entire tournament but the outcomes of each game. There is even a field
                             of study known as 'bracketology' where ambitious people put all of their brains
                             together to try and decide what the exact outcome of any given year's tournament
                             will be. Of course, history has shown us that the odds of predicting the outcome
                             of 32 games correctly is zero. Part of the problem arises from the fact that there
                             are a ridiculous amount of variables at play, and the caliber of the best teams 
                             are so similar that an accurate prediction by the end of the tournament becomes
                             almost impossible."),
                             br(),
                             p("One funny example of how ridiculous march maddness predictions really are, is that
                             of billionaire, Warren Buffett's challenge to general public in 2014. He said he
                             would pay anyone one billion dollars if they correctly predicited a 'perfect' bracket,
                             meaning all 32 games correct. Since then, he has offered employees at his company,
                             Berkshire Hathaway, a million dollars every year for life if they accuarately predict
                             the first two rounds of the tournament. Seemingly a much simpler task, right?"),
                             br(),
                             p("Well, we have decided to give it our best shot, but with a little more challenge
                             to it, with trying to create an informed logistic model which can predict whether
                             or not certain teams make it to the third round, also known as the 'Sweet 16', because
                             it is when there are 16 teams left."),
                             br(),
                             p("The dataset we are using is a comprehensive set of team-by-team metrics for all of
                             division 1 basketball, from 2013 through present day. We found access to this data
                             from a user named Andrew Sundberg on Kaggle. We were able to code in a cateogrical
                             variable which would indicate whether or not a team made it to the Sweet 16, and
                             otherwise all the data we needed was there to inform our categorical predicited
                             variable.")
                             )
                           ),
                  
                  
                  tabPanel("March Madness Trends", #Tab for the line and scatter plot
                           br(),
                           HTML("<table><tr><td>Statistic Abbreviation</td><td>Statistic</td></tr><tr><td>G</td><td>Number of games played</td></tr><tr><td>W</td><td>Number of games won</td></tr><tr><td>ADJOE</td><td>Adjusted Offensive Efficiency (An estimate of the offensive efficiency (points scored per 100 possessions) a team would have against the average Division I defense)</td></tr><tr><td>ADJDE</td><td>Adjusted Defensive Efficiency (An estimate of the defensive efficiency (points allowed per 100 possessions) a team would have against the average Division I offense)</td></tr><tr><td>BARTHAG</td><td>Power Rating (Chance of beating an average Division I team)</td></tr><tr><td>EFG_O</td><td>Effective Field Goal Percentage Shot</td></tr><tr><td>EFG_D</td><td>Effective Field Goal Percentage Allowed</td></tr><tr><td>TOR</td><td>Turnover Percentage Allowed (Turnover Rate)</td></tr><tr><td>TORD</td><td>Turnover Percentage Committed (Steal Rate)</td></tr><tr><td>ORB</td><td>Offensive Rebound Rate</td></tr><tr><td>DRB</td><td>Offensive Rebound Rate Allowed</td></tr><tr><td>FTR</td><td>Free Throw Rate (How often the given team shoots Free Throws)</td></tr><tr><td>FTRD</td><td>Free Throw Rate Allowed</td></tr><tr><td>X2P_O</td><td>Two-Point Shooting Percentage</td></tr><tr><td>X2P_D</td><td>Two-Point Shooting Percentage Allowed</td></tr><tr><td>X3P_O</td><td>Three-Point Shooting Percentage</td></tr><tr><td>X3P_D</td><td>Three-Point Shooting Percentage Allowed</td></tr><tr><td>ADJ_T</td><td>Adjusted Tempo (An estimate of the tempo (possessions per 40 minutes) a team would have against the team that wants to play at an average Division I tempo)</td></tr><tr><td>WAB</td><td>Wins Above Bubble (Wins against teams that made the NCAA Tournament)</td></tr><tr><td>SEED</td><td>Seed in the NCAA March Madness Tournament</td></tr></table>"),
                           br(),
                           sidebarLayout(
                             sidebarPanel(
                               varSelectInput(inputId = "yvariable", 
                                              label = "Y-axis variable:", 
                                              data = select(cbb_clean, ADJOE, ADJDE, BARTHAG, 
                                                            EFG_O, EFG_D, TOR, TORD, ORB, DRB, 
                                                            FTR, FTRD, X2P_O, X2P_D, X3P_O, 
                                                            X3P_D, ADJ_T, WAB)), 
                               selectizeInput(inputId = "round",
                                              label = "Which rounds should show:",
                                              choices = cbb_clean$POSTSEASON,
                                              selected = cbb_clean$POSTSEASON[1],
                                              multiple = TRUE)),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Graphs", plotOutput("plot", height = 500), 
                                          br(),
                                          br(),
                                          girafeOutput("plot2", height = 500)), #Sub tab for plots
                                 tabPanel("Datatable", dataTableOutput("table")))) #Sub tab for table
                           )),
                  
                  tabPanel("Trends by Conference", #Tab for stacked bar graph
                           br(),
                           p("This page is meant to highlight any possible trends that occured on a conference-to-conference basis, as there have been instances in the past where certain conferences and parts of the country are particularly dominant."),
                           br(),
                           p("The Pac-12 has consistently gone down, while the WCC has increased. Additionally, the ACC, Big 10, and Big 12 are consistently a substantial portion of the Sweet 16 teams."),
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput(inputId = "years", #Year slider
                                           label = "Select Years to Graph",
                                           min = min(cbb_clean$YEAR),
                                           max = max(cbb_clean$YEAR),
                                           value = c(min(cbb_clean$YEAR), max(cbb_clean$YEAR)))),
                             mainPanel(
                               girafeOutput("stackedplot", height = 500))
                           )), 
                  
                  tabPanel("Predictions", #Prediction tab to show model accuracy and 2022 predictions
                           br(),
                           p("To predict teams that would make the Sweet 16 out of the 68 teams that made the tournament, we fitted a logistic regression model to data from the 2013 through 2019 tournaments. The model takes in the numerical variables G, W, ADJOE, ADJDE, BARTHAG, EFG_O, EFG_D, TOR, TORD, ORB, DRB, FTR, FTRD, X2P_O, X2P_D, X3P_O, X3P_D, ADJ_T, WAB, and SEED (abbreviations explained in the table below). Based on this model, we can determine which 16 teams are most likely to make the Sweet 16 based on the season data from that year."),
                           br(),
                           p("However, there are a couple things this model cannot predict. It returns the 16 teams most likely to make the Sweet 16 based on season-long statistics, regardless of what the actual bracket looks like. This means that two teams that this model predicts will make the Sweet 16 might actually meet in an earlier round. This model also does not take into account the teams that each team will face on their way to the Sweet 16, as some teams may have an easier path than others."),
                           br(),
                           p("Based on season-long statistics found on the website barttorvik.com, our model predicts that the 2022 Sweet 16 will include the teams shown below. However, this does not take into account the real layout of the bracket, so this will not be the exact Sweet 16. For example, Richmond plays Iowa in the first round, so both of these teams will not make it."),
                           br(),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "year",
                                           label = "Which past year should show:",
                                           choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019),
                                           selected = 2019), 
                               checkboxInput(inputId = "fullTable", label = "Show full 2022 table?", 
                                             value = FALSE)),
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Past Predictions", plotOutput("plotPast", height = 500), 
                                          dataTableOutput("tablePast")),
                                 tabPanel("2022 Predictions", plotOutput("plot2022", height = 500),
                                          dataTableOutput("table2022"))))
                           ))
                  
                ))

server <- function(input, output){
  output$placeholder <- renderText("This is placeholder text.")
  
  output$plot <- renderPlot({
    ggplot(cbb_clean, aes(x = YEAR, y = !!input$yvariable, col = sweet_16)) +
      geom_smooth(se = TRUE) +
      labs(col = "Sweet 16", title = str_c("Line plot for Teams by ", 
                                           input$yvariable, " Over Time"))
  }) #Line plot to show trends of a variable based on teams that made the sweet
  #16 versus those that did not
  
  reactiveScatter <- reactive({ #Allows for the data to be reactive to input
    cbb_clean %>% 
      subset(POSTSEASON %in% input$round) %>%
      group_by(TEAM) %>%
      mutate(number_in_16 = length(POSTSEASON)) %>%
      select(YEAR, TEAM, input$yvariable, POSTSEASON, CONF) %>%
      mutate(tooltip = str_c(YEAR, "\n Team: ", TEAM, "\n Conference: ", 
                             CONF, "\n Result: ", POSTSEASON))
    #Tooltip will show inforamation when a point is hovered over
  })
  
  reactiveTable <- reactive({ #Reactive table for the data
    cbb_clean %>% 
      subset(POSTSEASON %in% input$round) %>%
      group_by(TEAM) %>%
      mutate(number_in_16 = length(POSTSEASON)) %>%
      select(YEAR, TEAM, input$yvariable, POSTSEASON, CONF)
  })
  
  output$plot2 <- renderGirafe({ #Interactive scatter plot
    abc<- ggplot(reactiveScatter(), aes(YEAR, reactiveScatter()[[input$yvariable]], 
                                        color = TEAM)) +
      #The y value is the value for the user inputted variable of interest
      geom_point_interactive(tooltip = reactiveScatter()$tooltip,
                             data_id = reactiveScatter()[[input$yvariable]],
                             size = 1, 
                             alpha = 0.8) + 
      labs(x = "Year", y = input$yvariable, title = str_c("Value of ", 
                                                          input$yvariable, " Per Year by Team")) + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="none")
    
    ggiraph(code = print(abc), option = list(
      opts_hover(css = "fill:red;cursor:pointer;"), 
      opts_selection(type = "single", css = "fill:red;")))
    #Makes the point red when you hover over it
  })
  
  output$table <- renderDataTable({
    reactiveTable() }) #Rendering the reactive data table
  
  
  reactivePlot <-reactive({ #Reactive for hovering on the stacked bar graph
    cbb_clean %>% 
      subset(YEAR >= input$years[1] & YEAR <= input$years[2]) %>% 
      filter(sweet_16 == "yes") %>%
      group_by(CONF, YEAR) %>%
      summarize(number_in_16 = length(POSTSEASON)) %>%
      mutate(tooltip = str_c(YEAR, "\n Conference: ", CONF))
  })
  
  
  output$stackedplot <- renderGirafe({ #Interactive stacked bar graph
    abc<- ggplot(reactivePlot(), aes(YEAR, number_in_16, fill = CONF)) +
      #The x value is the value for the user inputted years of interest
      geom_bar_interactive(position="stack", stat="identity",
                           tooltip = reactivePlot()$tooltip) + 
      labs(x = "Year", y = "Number of Teams in the Sweet 16", title = 
             "Teams in Sweet 16 by Year") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggiraph(code = print(abc), option = list(
      opts_hover(css = "fill:red;cursor:pointer;"), 
      opts_selection(type = "single", css = "fill:red;")))
    #Makes the point red when you hover over it
  })
  
  
  
  output$plotPast <- renderPlot({
    cbb_apply <- cbb_raw %>% #Filtering data
      mutate(sweet_16 = ifelse(POSTSEASON %in% c("2ND", "Champions", "F4", "E8", 
                                                 "S16"), "yes", "no")) %>%
      drop_na() %>%
      mutate(sweet_16 = as.factor(sweet_16)) %>%
      mutate(sweet_16 = fct_relevel(sweet_16, "yes", "no")) %>%
      filter(YEAR == input$year) %>% #take in the input year
      select(-CONF, -POSTSEASON, -YEAR)
    apply_pred_prob <- predict(fitted_logistic_model, new_data = cbb_apply, 
                               type = "prob") #Applying prediction model to our data
    cbb_apply <- cbb_apply %>%
      bind_cols(apply_pred_prob) #Binding the predictions to our data
    cbb_apply <- cbb_apply %>%
      slice_max(order_by = .pred_yes, n = 16)
    #Only showing the predicted sweet 16 teams
    
    ggplot(cbb_apply, aes(TEAM, .pred_yes, fill = sweet_16)) +
      geom_bar(stat="identity") +
      labs(x = "Team", y = "Sweet 16 Prediction", title =
             str_c("Teams Predicted to be in Sweet 16 in ", input$year)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))
    #This plot shows the predicted teams for the given year, and if they actually
    #made the sweet 16 or not
  })
  
  output$tablePast <- renderDataTable({
    cbb_apply <- cbb_raw %>% #Again filtering data
      mutate(sweet_16 = ifelse(POSTSEASON %in% c("2ND", "Champions", "F4", "E8", 
                                                 "S16"), "yes", "no")) %>%
      drop_na() %>%
      mutate(sweet_16 = as.factor(sweet_16)) %>%
      mutate(sweet_16 = fct_relevel(sweet_16, "yes", "no")) %>%
      filter(YEAR == input$year) %>% #take in the input year
      select(-CONF, -POSTSEASON, -YEAR)
    apply_pred_prob <- predict(fitted_logistic_model, new_data = cbb_apply, 
                               type = "prob") #Applying prediction model to our data
    cbb_apply <- cbb_apply %>%
      bind_cols(apply_pred_prob) #Binding the predictions to our data
    cbb_apply <- cbb_apply %>%
      slice_max(order_by = .pred_yes, n = 16)
    cbb_apply #Outputting the predicted sweet 16 teams
  })
  
  
  output$plot2022 <- renderPlot({
    ggplot(cbb22_top16, aes(TEAM, .pred_yes)) +
      geom_bar(stat="identity") +
      labs(x = "Team", y = "Sweet 16 Prediction", title =
             "Teams Predicted to be in Sweet 16 in 2022") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))
  }) #This plot shows the predicted sweet 16 teams for 2022
  
  output$table2022 <- renderDataTable({
    if (input$fullTable) {
      cbb22 #Plots the full table for 2022
    }
    else {
      cbb22_top16 #Plots the predicted sweet 16 teams for 2022
    }
  })
}

app1 <- shinyApp(ui = ui, server = server)
app1

