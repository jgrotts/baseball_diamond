#ui.r for shiny app
library(shiny)

# Define UI for dataset viewer application
# countries <- sort(as.character(unique(retail$Country)))

shinyUI(fluidPage(
  
  # Application title
  title = "Baseball Diamond Graphic",
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  fluidRow(
    # sidebarPanel(
      column(2,style="margin-top:50px",
      selectInput("Team", "Choose a team:", 
                  choices = c("Angels",
                              "Astros",
                              "Athletics",
                              "Blue Jays",
                              "Braves",
                              "Brewers",
                              "Cardinals",
                              "Cubs",
                              "Diamondbacks",
                              "Dodgers",
                              "Giants",
                              "Indians",
                              "Mariners",
                              "Marlins",
                              "Mets",
                              "Nationals",
                              "Orioles",
                              "Padres",
                              "Phillies",
                              "Pirates",
                              "Rangers",
                              "Rays",
                              "Red Sox",
                              "Reds",
                              "Rockies",
                              "Royals",
                              "Tigers",
                              "Twins",
                              "White Sox",
                              "Yankees"
                              
                  )),
                  selectInput("Stat", "Choose a statistics:", 
                              choices = c("Off", "Def", "WAR","wRC.","AVG","SLG","OBP")
                  )
    ),
    column(8,align="center",
           plotOutput("dist_plot"),
           tableOutput("stat_table"),
           tableOutput("description")
           
           )),
  
  # fluidRow(
  #   column(8, offset = 1,,)
  # ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    
    # tableOutput("table1"),
    # plotOutput("dist_plot")
  )
))