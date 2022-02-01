
library(shiny)
library(tidyverse)
library(edeaR)
library(sportyR)
library(readr)

games <- read.csv("./data/games2.csv")
scouting <- read.csv("./data/PFFScoutingData2.csv")
players <- read.csv("./data/players2.csv")
plays <- read.csv("./data/plays2.csv")
tracking2018 <- read_csv("./data/small_tracking2018.csv")

punt_plays <- plays %>% filter(specialTeamsPlayType=="Punt")

punt_plays_players <- left_join(punt_plays, players, by=c("kickerId"="nflId")) %>%
    rename(punter_height=height, punter_weight=weight, punter_college=collegeName, punter_name = displayName)
punt_plays_players$returnerId <- as.numeric(punt_plays_players$returnerId)

punt_tracking <- left_join(punt_plays_players, tracking2018, by=c("gameId", "playId"))

playernames <- players %>% select(nflId, displayName)

punt_tracking_by_returner <- left_join(punt_tracking, playernames, by=c("returnerId"="nflId")) %>%
    rename(returner_name = displayName.y, tracking_player = displayName.x) %>%
    filter(returner_name==tracking_player)

punt_tracking_by_returner2 <- punt_tracking_by_returner %>%
    group_by(gameId, playId) %>%
    filter(any(event=="punt_received")) %>%
    mutate(counter = ifelse(event=="punt_received", 1, 
                            ifelse(event %in% c("tackle","fumble_defense_recovered","touchback",
                                                "fumble","lateral","safety","fair_catch",
                                                "fumble_offense_recovered","punt_muffed","touchdown",
                                                "handoff","out_of_bounds"), -1, 0)), 
           counter_sum = cumsum(counter)) %>%
    filter(counter_sum == 1)

punt_tracking_standardized_by_returner <- punt_tracking_by_returner2 %>%
    group_by(gameId, playId) %>%
    mutate(x2 = abs(x-first(x)))

data = punt_tracking_standardized_by_returner

returner_names <- data %>% 
    group_by(returner_name) %>%
    filter(row_number()==1) %>%
    pull(returner_name) %>%
    sort()

# only going in one direction: punt to the right, return to the left
punt_tracking_by_returner_right2left <- punt_tracking_by_returner2 %>%
    filter(playDirection == "right")

# only going in one direction: punt to the left, return to the right
punt_tracking_by_returner_left2right <- punt_tracking_by_returner2 %>%
    filter(playDirection == "left")

#standardized right to left
punt_tracking_standardized_by_returner_right2left <- punt_tracking_by_returner_right2left %>%
    group_by(gameId, playId) %>%
    mutate(x2 = x-first(x))

#standard left to right
punt_tracking_standardized_by_returner_left2right <- punt_tracking_by_returner_left2right %>%
    group_by(gameId, playId) %>%
    mutate(x2 = x-first(x))

data1 = punt_tracking_standardized_by_returner_right2left
data2 = punt_tracking_standardized_by_returner_left2right

# Define server logic required to create the interactive punt return app
server <- function(input, output, session) {
    
    updateSelectizeInput(session, "returner", choices = returner_names, server = TRUE)
    data1_filtered <- reactive({
        data1 %>% filter(returner_name %in% input$returner) %>%
            group_by(gameId, playId, returner_name) %>% 
            mutate(col = ifelse(last(x2)>0, "red", "green")) %>%
            filter(-last(x2) > input$return_yards[1], -last(x2) < input$return_yards[2])
            
    })
    data2_filtered <- reactive({
        data2 %>% filter(returner_name %in% input$returner) %>%
            group_by(gameId, playId, returner_name) %>% 
            mutate(col = ifelse(last(x2)<0, "red", "green")) %>%
            filter(last(x2) > input$return_yards[1], last(x2) < input$return_yards[2]) 
            
    })
    
    #Plot
    output$scatterPlot <- renderPlot({
        geom_football(league = "nfl", grass_color="darkgreen") +
            geom_path(data=data1_filtered(), aes(-x2+20,y,group = playId, color=col)) +
            geom_path(data=data2_filtered(), aes(x2+20,y,group = playId, color=col)) +
            scale_color_identity()
    })
}


# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Standardized Punt Returns for Selected Player: 2018 Season"),
    
    sidebarLayout(
        sidebarPanel(
            #selectInput("returner",
            # "Select a returner:",
            #  choices = data$returner_name)
            selectizeInput("returner", "Returner", multiple = F, choices = NULL),
            sliderInput("return_yards",
                        "Range of Net Return Yards:",
                        min = -100,
                        max = 100,
                        value = c(-30,30))
        ),
        
        mainPanel(
            plotOutput("scatterPlot")
        )
    )
)

# Run the application 
shinyApp(ui = ui, server = server)

