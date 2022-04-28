#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library("DT")
library("sf")
library('dplyr')
library('markdown')
library('tidyr')
library('shinythemes')
library('plotly')
library("ggplot2")
#devtools::install_github("daattali/colourpicker")
#install.packages("colourpicker")
library(colourpicker)
library(devtools)

# reads in and joins the data with the state codes
library(readr)
df <- read_csv("ucr_crime_1975_2015.csv")



ui <- dashboardPage( skin = "green",
                     
                     #Application title
                     dashboardHeader(title = "CRIME ANALYSIS",titleWidth = 400),
                     
                     # dashboard sidebar functions will be inserted here
                     dashboardSidebar(
                         
                         sidebarMenu(
                             menuItem("Dashboard",tabName = "Dashboard",icon = icon("dashboard")),
                             menuItem("Map",tabName = "map",icon = icon("map")),
                             menuItem("Chart",tabName = "Chart",icon = icon("chart"))
                         ),
                         sliderInput("year",
                                     label = "Years",
                                     step = 1,
                                     value = c(1975,2015),
                                     min =  1975,
                                     max = 2015
                                     ),
                         dateRangeInput('Date Range',
                                        label = 'Filter crimes by date'
                         ),
                         numericInput("Month", "Months", 1, min = 1, max = 12),
                         
                         selectInput("department","Select Department:",
                                     choices=sort(unique(df$department_name)),
                                     multiple = F,
                                     selected = "Atlanta"),
                         
                         radioButtons("crimes",
                                      label = "Select Crime:",
                                      choices = c('violent_per_100k','homs_per_100k','rape_per_100k','rob_per_100k','agg_ass_per_100k','no crime'),
                                      selected = "homs_per_100k")
                     ),
                     # functions that must go in the body of the dashboard.
                     dashboardBody(
                         tabItems(
                             tabItem(tabName = "Dashboard",
                                     h2("Dashboard of Crimes with respect to states,cites of US and years"),
                                     
                                     plotOutput("Plot1"),height=450,
                                     tableOutput("data of Plot1")
                                     
                             ),
                             tabItem(tabName = "map",
                                     h2("Dashboard Map of Yearly trend of Crime in each state of US"),
                                     plotlyOutput("plotusa"),height =450
                                     
                             ),
                             tabItem(tabName = "Chart",
                                     h2("Bar Plot of Yearly trend of Robberies in each state of US"),
                                     plotlyOutput("plot"),height =450
                             )
                             
                         )
                     )
                     
)


server <- function(input, output) {
    # Create scatterplot object the plotOutput function is expecting
    
    df_r <- reactive({
        df_a <- df %>% filter(year >= input$year[1],
                              year <= input$year[2])
        df_a})
    
    output$Plot1 <- renderPlot({
        ggplot(df_r())
        
        if(input$crimes == "homs_per_100k"){ggplot(df_r(), aes(x = year,y = homs_per_100k))+
                geom_step(data = df_r() %>% filter(department_name %in% input$department))+
                ggtitle("Homicides v/s Yearly graph") + ylab("Homicides per 100k") + xlab("Year")}
        
        else if(input$crimes == "rape_per_100k"){ggplot(df_r(), aes(x = year,y = rape_per_100k)) + 
                geom_jitter(data = df_r() %>% filter(department_name %in% input$department)) + 
                ggtitle("Rape v/s Yearly graph") + ylab("rape_per_100k") + xlab("Year")}
        
        else if(input$crimes == "violent_per_100k"){ggplot(df_r(), aes(x = year,y = violent_per_100k)) + 
                geom_line(data = df_r() %>% filter(department_name %in% input$department)) + 
                ggtitle("Violent v/s Yearly graph") + ylab("violent_per_100k") + xlab("Year")}
        
        else if(input$crimes == "rob_per_100k"){ggplot(df_r(), aes(x = year,y = rob_per_100k)) + 
                geom_area(data = df_r() %>% filter(department_name %in% input$department)) + 
                ggtitle("Robberies v/s Yearly graph") + ylab("rob_per_100k") + xlab("Year")}
        
        else {ggplot(df_r(), aes(x = year,y = agg_ass_per_100k)) + 
                geom_point(data = df_r() %>% filter(department_name %in% input$department)) + 
                ggtitle("Assaults Vs Yearly graph") + ylab("agg_ass_per_100k") + xlab("Year")}
    })
    
    output$plotusa <- renderPlotly({
        
        df1 <- df%>% separate(department_name,c("city","state_1"),",",remove=F)
        df2 <- read_csv("states.csv")
        df3 <- merge(x=df1,y=df2,by="city",all.x=T)
        df4 = df3 %>% filter(year == input$year[2]) %>% group_by(state) %>% 
            summarise(population = sum(total_pop,na.rm=T),rape = sum(rape_sum,na.rm=T),
                      assaults = sum(agg_ass_sum,na.rm=T),robberies = sum(rob_sum,na.rm=T),
                      homicides = sum(homs_sum,na.rm=T),violent_crimes = sum(violent_crime,na.rm=T)
            )
        
        df4$click <- paste(df4$state,df4$population,
                                  df4$rape,df4$assaults, df4$robberies,
                                  df4$homicides,df4$violent_crimes)
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('black')
        )

          plot_geo()%>% add_trace(
                z = df4$robberies, 
                locations = df4$state,
                text = df4$click,
                type = 'choropleth',
                colorscale="gray",
                locationmode = 'USA-states') %>% colorbar(title = "Crime Rates")%>%
                layout(geo = g,title = paste0("Map of crimes in US in year: ",input$year[2]))
            
        })
    
    output$plot <- renderPlotly({
      
      df5 <- df%>% separate(department_name,c("city","state_1"),",",remove=F)
      df6 <- read_csv("states.csv")
      df7 <- merge(x=df5,y=df6,by="city",all.x=T)
      df8 = df7 %>% filter(year == input$year[2]) %>% group_by(state) %>% 
        summarise(population = sum(total_pop,na.rm=T),rape = sum(rape_sum,na.rm=T),
                  assaults = sum(agg_ass_sum,na.rm=T),robberies = sum(rob_sum,na.rm=T),
                  homicides = sum(homs_sum,na.rm=T),violent_crimes = sum(violent_crime,na.rm=T)
        )
      df8$click <- paste(df8$state,df8$population,
                         df8$rape,df8$assaults, df8$robberies,
                         df8$homicides,df8$violent_crimes)
  
       g <- ggplot(df8, aes(x=state, y=robberies,fill=robberies))+
         geom_bar(stat="identity",position = position_dodge(width = .25))+labs(title=paste0("Robberies in States of US yearly: ",input$year[2])) +
         xlab("state") + ylab("robberies")+coord_flip()
       options(scipen = 999)
       ggplotly(g)
      
    })
}

shinyApp(ui = ui, server = server)

