#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyWidgets)
library(readr)
library(janitor)
library(purrr)
library(lubridate)
library(plotly)
library(sp)
library(ggthemes)
library(coronavirus)
library(maps)
library(shinythemes)
library(dplyr)
library(maptools)
library(rvest)
library(polite)
library(stringr)
library(viridis)
library(ggiraph)
library(RColorBrewer)
library(leaflet)
library(tidyr)
library(rnaturalearth)
library(sf)

# Data Cleaning
covid_clean <- coronavirus %>% #Cleaning the coronavirus dataset
  group_by(country) %>%
  mutate(province = replace_na(province, "mainland")) %>%
  filter(province == "mainland") %>%
  mutate(cases = abs(cases)) %>%  #Turning negative recovery values positive
  arrange(date) %>%              #so we can subtract
  pivot_wider(names_from = "type",
              values_from = "cases") %>% # Make case type a variable
  mutate(
    cumulativeCase = cumsum(confirmed), #Making cumulative variables
    cumulativeDeath = cumsum(death)) %>% 
  select(
    -c(
      province, #Removing unnecessary variables
      uid,
      iso2,
      code3,
      combined_key,
      continent_code
    )
  )

vac<-covid19_vaccine %>% #Cleaning the vaccine dataset
  arrange(date) %>% 
  select(
    -c( #Removing uneccessary variables
      "uid", "iso2", "iso3", "code3", "continent_name", 
      "continent_code", "combined_key", "fips", "report_date_string"
    )) %>% 
  filter(!(country_region == "World")) %>% 
  rename(country = "country_region")

joined_covid<-left_join(vac,covid_clean, "country" = "country", "date" = "date")
#Joining the coronavirus and vaccine datasets

final<-joined_covid %>% #Final dataset for the leaflet map
  filter(country %in% str_to_title(c("afghanistan", 
                                     "cambodia", 
                                     "thailand", 
                                     "laos", 
                                     "vietnam", 
                                     "bangladesh", 
                                     "nepal",
                                     "myanmar", 
                                     "malaysia", 
                                     "philippines", 
                                     "indonesia", 
                                     "pakistan"))) %>% 
  group_by(country) %>% 
  select(lat, long, cumulativeCase,cumulativeDeath, people_fully_vaccinated) %>% 
  summarize("Total Cases" = max(cumulativeCase, na.rm = TRUE),
            "Total Death Cases" = max(cumulativeDeath, na.rm = TRUE),
            "Total Vaccinated Cases" = max(people_fully_vaccinated, na.rm = TRUE), 
            lat = max(lat), 
            long = max(long)) #Variables for cumulative variables and lat and
#long information for the leaflet map

line_final<-joined_covid %>% #Final dataset for the scatterplot and table
  filter(country %in% str_to_title(c("afghanistan", 
                                     "cambodia", 
                                     "thailand", 
                                     "laos", 
                                     "vietnam", 
                                     "bangladesh", 
                                     "nepal",
                                     "myanmar", 
                                     "malaysia", 
                                     "philippines", 
                                     "indonesia", 
                                     "pakistan"))) %>% 
  group_by(country, date) %>% 
  rename("Vaccination Population" = people_fully_vaccinated,
         "Total Cases" = cumulativeCase,
         "Death Cases" = cumulativeDeath) %>% 
  select(
    c(`Vaccination Population`, `Total Cases`, `Death Cases`)
  ) #Our 3 variables of interest



# Define UI for application that draws a histogram
ui <- fluidPage(tags$head(tags$style("label{font-family: Georgia;}")), 
                theme = shinytheme("cyborg"),
  navbarPage(
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 tracker</a>'), 
             id="nav",
             windowTitle = "COVID-19 Data Visualization",
#Style inspired by Edward Parker
    
    tabPanel("Cumulative Cases World Map", #Tab for the map
              sidebarLayout(
               sidebarPanel(
                 textOutput(outputId = "text"),
                 textOutput(outputId = "text2"),
                 radioButtons(inputId = "type",
                              label = "Case Type:",
                              choices = names(final)[2:4],
                              selected = names(final)[2])),
                mainPanel(leafletOutput("mapplot")))),
    
    
    tabPanel("COVID-19 Trends", #Tab for the plot and table
             sidebarLayout(
               sidebarPanel(
                 textOutput(outputId = "text3"),
                 selectInput(inputId = "cumulative", #Variables
                             label = "Variable to Plot:",
                             choices = colnames(line_final[3:5])
                             ),
                 selectizeInput(inputId = "countries", #Countries
                         label = "Country Name",
                         choices = line_final$country,
                         selected = line_final$country[1],
                         multiple = TRUE),
                  sliderInput(inputId = "day", #Date slider
                         label = "Select Dates to Graph",
                         min = min(line_final$date),
                         max = max(line_final$date),
                         value = c(min(line_final$date), max(line_final$date)))
               ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Lineplot", girafeOutput("lineplot")), #Sub tab for plot
                 tabPanel("Datatable", dataTableOutput("table")))) #For table
    )
 
)))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Text for the map tab
  output$text <- renderText("The map on the right shows COVID-19 cases in 
                             selected Asain countries. Hover over the country 
                             of interest for more information.")
  
  output$text2 <- renderText("The data used for this project was obtained from 
                            Rami Krispin, who pulled the data from the Johns 
                            Hopkins University Center for Systems Science 
                            and Engineering.")
  
  
 output$mapplot<-renderLeaflet({ #Rendering the map
  map1<-map("world", regions = c( "afghanistan","cambodia", "thailand", "laos", 
                                  "vietnam", "bangladesh", "nepal",
                                  "malaysia", "philippines", "indonesia", 
                                  "pakistan"), plot = FALSE, fill = TRUE)
  #Filtering from the map function the countries we want to plot
  reg<-"(?=:).+"
  map1$names<-str_remove(map1$names, reg)
  Regionmap <- map2SpatialPolygons(map1, IDs = map1$names)
  #This removes cases such as islands from map1 that would prevent Regionmap
  #and final from having the same number of rows
  
  map <- SpatialPolygonsDataFrame(Regionmap, final, match.ID = FALSE)
  
  bins <- seq(0, max(map[[input$type]]), length.out = 7) #Number of bins 
                                                          #for the map key
  pal <- colorBin("viridis", domain = map[[input$type]], bins = bins)
  

  labels <- sprintf("<strong> %s </strong> <br/> Total Case: %s <br/> Total Death: %s <br/> Total Vaccinated: %s", 
                    str_to_upper(map$country), final$`Total Cases`, 
                    final$`Total Death Cases`, final$`Total Vaccinated Cases`) %>%
    lapply(htmltools::HTML)
  #Label for hovering over a country and displaying the country's information
  
  l <- leaflet(map) %>% addTiles() 
  
  #Plotting the leaflet map by interactively adding the input type
  n <- l %>% addPolygons(color = "grey", weight = 1,
                         fillColor = ~pal(map[[input$type]]), fillOpacity = 0.7,
                         highlightOptions = highlightOptions(weight = 5),
                         label = labels) %>%
    addLegend(pal = pal, values = ~map[[input$type]], opacity = 0.5, 
              title = input$type, 
              position = "bottomright") #The map's key/legend
  
  n
 })
  

                 
  #Line Plots of Cumulative Cases

 #Text for the plot/table tab
 output$text3 <- renderText("The line plot and table on the right shows trends 
                           in cumulative cases over time.")
 
 #Reactive line that only contains input data, and the tooltip data for 
 #hovering over a point
 line_2<-reactive({
   line_final %>% 
     subset(date >= input$day[1] & date <= input$day[2]) %>% 
     subset(country %in% input$countries) %>% 
     mutate(tooltip = str_c(date, "\n Country: ", country, 
                            "\n Case of Interest: ", input$cumulative))
 })
 
 #Reactive data to only include input data for the table
 table_2 <- reactive({
   line_final %>% 
     subset(date >= input$day[1] & date <= input$day[2]) %>% 
     subset(country %in% input$countries)
 })

 #Rendering the scatterplot
  output$lineplot<-renderGirafe({
   abc<- ggplot(line_2(), aes(date, line_2()[[input$cumulative]], color = country)) +
     #The y value is the value for the user inputted case of interest
    geom_point_interactive(tooltip = line_2()$tooltip,
                           data_id = line_2()[[input$cumulative]],
                           size = 1, 
                           alpha = 0.8) + #Shows more information when you hover
                                          #over a point
     labs(y = "Cumulative cases", x = "Date", title = str_c("Cumulative ", 
                                            input$cumulative, " Over Time")) + 
     theme(plot.title = element_text(hjust = 0.5)) + 
     theme_linedraw() #Makes the plot more clean looking
  ggiraph(code = print(abc), option = list(
    opts_hover(css = "fill:red;cursor:pointer;"), 
    opts_selection(type = "single", css = "fill:red;")))
  #Makes the point red when you hover over it
 }) 
  
  output$table <- renderDataTable({
    table_2() }) #Renders the reactive datatable
}

# Run the application 
#rsconnect::setAccountInfo(name='parker12995',
#			  token='011B482A43DC4471814591DBBA863BCC',
#			  secret='DFSh8LTKT1o02DeIOAxX0fZLkbzYbNTn8XWiIBxc')
shinyApp(ui = ui, server = server)
#rsconnect::deployApp("app.R")
