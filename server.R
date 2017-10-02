#shiny server file
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))

##############################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Formatting Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############################################


#Load data
baseball <- read.csv("fangraphs_2017.csv")


pa_hold <- baseball$PA > 120





table_fun <- function(team_hold, value_hold, table_out = FALSE){

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Diamond dataset
  position_hold <- c("C", "1B", "2B", "SS", 
                     "3B", "RF", "CF", "LF")
  
  # value_hold <- "Off"
  # team_hold <- "Dodgers",
  # pos <- position_hold[1]
  diam <- NULL
  for(pos in position_hold){  
    
    # value <- mean(baseball[pa_hold & baseball$Team == team_hold &
    #                          baseball$Position == pos, value_hold] * 
    #                 baseball$PA[pa_hold & baseball$Team == team_hold &
    #                               baseball$Position == pos] / 
    #                 sum(baseball$PA[pa_hold & baseball$Team == team_hold &
    #                                   baseball$Position == pos]))
    
    Off <- sum(baseball[pa_hold & baseball$Team == team_hold &
                             baseball$Position == pos, "Off"] * 
                    baseball$PA[pa_hold & baseball$Team == team_hold &
                                  baseball$Position == pos]) / 
                    sum(baseball$PA[pa_hold & baseball$Team == team_hold &
                                      baseball$Position == pos])
    
    Def <- sum(baseball[pa_hold & baseball$Team == team_hold &
                           baseball$Position == pos, "Def"] * 
                  baseball$PA[pa_hold & baseball$Team == team_hold &
                                baseball$Position == pos]) / 
                  sum(baseball$PA[pa_hold & baseball$Team == team_hold &
                                    baseball$Position == pos])
    
    WAR <- sum(baseball[pa_hold & baseball$Team == team_hold &
                           baseball$Position == pos, "WAR"] * 
                  baseball$PA[pa_hold & baseball$Team == team_hold &
                                baseball$Position == pos]) / 
                  sum(baseball$PA[pa_hold & baseball$Team == team_hold &
                                    baseball$Position == pos])
    
    wRC. <- sum(baseball[pa_hold & baseball$Team == team_hold &
                           baseball$Position == pos, "wRC."] * 
                  baseball$PA[pa_hold & baseball$Team == team_hold &
                                baseball$Position == pos]) / 
                  sum(baseball$PA[pa_hold & baseball$Team == team_hold &
                                    baseball$Position == pos])
    
    AVG <- sum(baseball[pa_hold & baseball$Team == team_hold &
                            baseball$Position == pos, "AVG"] * 
                   baseball$PA[pa_hold & baseball$Team == team_hold &
                                 baseball$Position == pos]) / 
                   sum(baseball$PA[pa_hold & baseball$Team == team_hold &
                                     baseball$Position == pos])
    
    SLG <- sum(baseball[pa_hold & baseball$Team == team_hold &
                           baseball$Position == pos, "SLG"] * 
                  baseball$PA[pa_hold & baseball$Team == team_hold &
                                baseball$Position == pos]) / 
                  sum(baseball$PA[pa_hold & baseball$Team == team_hold &
                                    baseball$Position == pos])
    
    OBP <- sum(baseball[pa_hold & baseball$Team == team_hold &
                           baseball$Position == pos, "OBP"] * 
                  baseball$PA[pa_hold & baseball$Team == team_hold &
                                baseball$Position == pos]) / 
                  sum(baseball$PA[pa_hold & baseball$Team == team_hold &
                                    baseball$Position == pos])
    
    #Find player with most PA at position
    player <- as.character(baseball$Name[baseball$Team == team_hold & 
                    baseball$Position == pos &
                    baseball$PA == max(baseball$PA[baseball$Team == team_hold & 
                                         baseball$Position == pos])])
    
    
    diam <- rbind(diam, 
                  data.frame(
                    Position = pos,
                    Player = player,
                    Off,
                    Def,
                    WAR,
                    wRC.,
                    AVG,
                    SLG,
                    OBP
                  ))
    
    
  }
  
  
  ids <- factor(c("C", "1B", "2B", "SS", 
                  "3B", "RF", "CF", "LF"))
  
  diam$col_intensity <- pnorm(q = diam[,value_hold],
                              mean = mean(baseball[pa_hold,value_hold]),
                              sd = sd(baseball[pa_hold,value_hold]))
  
  values <- data.frame(
    id = ids,
    value = round(100*diam$col_intensity)
    
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Polygon dataset
  
  positions <- data.frame(
    id = c(rep(as.character(ids[1]), 5), 
           rep(as.character(ids[2:5]), each = 4), 
           rep(as.character(ids[6]), each = 5),
           rep(as.character(ids[7]), each = 4), 
           rep(as.character(ids[8]), each = 5)),
    x = c( 7/8,  1.05,  0.0, -1.05, -7/8,    #c
           1.05,  0.5,  1.05,  2.0,           #1b
           0.0,  0.5,  1.05,  0.0,           #2b
           0.0, -0.57, -1.05,  0.0,          #SS
           -1.05, -0.5, -1.05, -2.0,       #3b
           3/8,  2.0,  2.15, 1.5, 0.75,  #RF
           3/8,  0.75, -0.75, -3/8,   #CF
           -3/8, -2.0, -2.15, -1.5, -0.75),  #LF
    y = c(0.2, 0.5, 1, 0.5, 0.2,    #c
          0.5, 5/8, 16/8, 16/8, #1B
          1, 5/8, 16/8, 16/8,   #2b
          1, 5.75/8, 16/8, 16/8,   #SS
          0.5, 5/8, 19/8, 16/8, #3b
          16/8, 16/8, 18/8, 25/8, 25/8, #RF
          16/8, 25/8, 25/8, 16/8, #CF
          16/8, 16/8, 18/8, 25/8, 25/8 #LF
    )
  )
  
  # Currently we need to manually merge the two together
  datapoly <- merge(values, positions, by = c("id"))
  
  diam[,3:6] <- round(diam[,3:6],2)
  diam[,7:9] <- round(diam[,7:9],3)
  
  for(i in 3:9){
    diam[,i] <- as.character(diam[,i])
  }
    
  if(table_out){
    return(diam[,1:9])
  } else{
    return(datapoly) 
  }
}


description_table <- rbind(
  
  c("Position", "Position on baseball diamond"),
  c("Player", "Player with the most plate appearences at the position"),
  c("Off",    "Offensive Runs Above Average: Number of runs above or below average a player has been worth offensively, combining Batting Runs and Base Running Runs Above Average"),
  c("Def",    "Defensive Runs Above Average:  Number of runs above or below average a player has been worth on defense, combining Fielding Runs (Total Zone before 2002, UZR after) and the positional adjustment"),
  c("WAR",    "Wins Above Replacement: A comprehensive statistic that estimates the number of wins a player has been worth to his team compared to a freely available player such as a minor league free agent."),
  c("wRC.",   "Weighted Runs Created Plus:  The most comprehensive rate statistic used to measure hitting performance because it takes into account the varying weights of each offensive action (like wOBA) and then adjusts them for the park and league context in which they took place."),
  c("AVG",    "Batting Average: Rate of hits per at bat, H/AB"),
  c("SLG",    "Slugging: Average number of total bases per at bat, calculated as Total Bases/AB."),
  c("OBP",    "On base percentage: Rate at which the batter reaches base, calculated as (H+BB+HBP)/(AB+BB+HBP+SF).")

)

description_table <- data.frame(description_table, stringsAsFactors = FALSE)
dimnames(description_table)[[2]] <- c("Statistic", "Description")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define server logic required to summarize country
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyServer(function(input, output) {
  
  
  #Generate tables
  # output$table1 <-  renderTable({table_fun(input$Team,input$Stat)})
  # 
  
  
  
  # Generate a distribution of order cost by country
  
  output$dist_plot <- renderPlot({
    datapoly <- table_fun(input$Team, input$Stat)
    
    ggplot(datapoly, aes(x = x, y = y)) +
      geom_polygon(aes(fill = value, group = id)) +
      scale_fill_gradientn("Values",limits = c(0, 100),
                           colors = c("red","grey","green")) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title = element_text(size = 15))
    
  })
  
  output$stat_table <- renderTable({
    table_fun(input$Team, input$Stat, table_out = TRUE)
  })
  
  output$description <- renderTable({
    description_table
  })
  
})


