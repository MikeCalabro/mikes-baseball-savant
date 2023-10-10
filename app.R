library(shiny)
library(tidyverse)
library(baseballr)
library(png)
library(grid)

# Defines Necessary Functions and Objects
player_data <- read_rds("data/player_data.rds")

player_names <- player_data %>%
  select(name) %>%
  deframe()

pitch_name_data <- read_rds("data/pitch_name_data.rds")

pitch_names <- pitch_name_data %>%
  deframe()

all_pitch_names <- c("All Pitches", pitch_names)

zones <- as.character(1:14)
all_zones <- c("All Zones", zones)

get_player_id <- function(name) {
  player <- name
  id <- player_data %>%
    filter(name == player) %>%
    select(mlbam_id) %>%
    deframe()
  return(id)
} 

# Define UI for application that shows MLB Batter Launch Plot
ui <- fluidPage(

    # Application title
    titlePanel("MLB Launch Data"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
            selectInput("player",
                        "Batter Name",
                        choices = player_names,
                        selected = "Shohei Ohtani"
            )
        ),
        column(3,
            selectInput("pitch_type",
                        "Pitch Type",
                        choices = all_pitch_names
            )
        ),
        column(3,
            selectInput("zone",
                        "Select Zone",
                        choices = all_zones
          )
        ),
        # Show a plot of the generated distribution
        column(4,
           plotOutput("zonePlot")
        ),
        column(4,
           plotOutput("fieldPlot")
        ),
        column(4,
           plotOutput("launchPlot")
        ),
        column(12,
           tableOutput("dataTable")  
        )
    )
)

# Define server logic required to draw a the plots
server <- function(input, output) {

    batter_data <- reactive({
      player_id <- get_player_id(input$player)
      pitch <- input$pitch_type
      zone <- input$zone
      data <- 
        baseballr::statcast_search_batters(
          start_date = '2023-03-25',
          end_date = '2023-10-01',
          batterid = player_id
          ) %>%
        filter(pitch_name != "")
        
      if(pitch != "All Pitches") {
        data <- data %>%
          filter(pitch_name == pitch)
      } else {
        data
      }
      
      if(zone != "All Zones") {
        zone_choice <- as.integer(zone)
        data <- data %>%
          filter(zone == zone_choice)
      } else {
        data
      }
      
    })
  
    # Doesn't adjust picture for righty/lefty
    # ...Yet!!
    output$zonePlot <- renderPlot({
      stance_img <- readPNG("images/batter_stance_r.png", native = TRUE)
      plot_stance <- rasterGrob(stance_img, interpolate = TRUE)
      
      y_min <- batter_data() %>%
        summarise(bottom = min(sz_bot)) %>%
        deframe()
      
      y_max <- batter_data() %>%
        summarise(top = max(sz_top)) %>%
        deframe()
      
      batter_data() %>%
        ggplot() +
        annotation_custom(plot_stance,
                          xmin = -3,
                          ymin = -2,
                          xmax = -1,
                          ymax = 8) +
        geom_rect(aes(xmin = -0.83,
                      xmax = 0.83,
                      ymin = y_min + 0.05,
                      ymax = y_max - 0.05),
                  fill = "lightgray",
                  color = "black") +
        geom_point(aes(x = plate_x,
                       y = plate_z,
                       fill = pitch_name),
                   shape = 21,
                   size = 3.5) +
        scale_x_continuous(limits = c(-3, 2)) +
        scale_y_continuous(limits = c(-1, 6)) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_blank())
    }, width=400, height=400)
    
    # Makes the field plot
    # Very shady with the coordinates, but works decently it seems!
    output$fieldPlot <- renderPlot({
      field_img <- readPNG("images/field_image.png", native = TRUE)
      plot_field <- rasterGrob(field_img, interpolate = TRUE)
      
      batter_data() %>%
        mutate(spray_angle = 0.28*round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>%
        filter(bb_type != "") %>%
        ggplot() +
        annotation_custom(plot_field) +
        geom_point(aes(x = hit_distance_sc*sin(spray_angle*(3.14/180)),
                       y = hit_distance_sc*cos(spray_angle*(1/0.28)*(3.14/180)),
                       fill = events),
                   shape = 21,
                   size = 3) +
        theme_minimal() +
        scale_x_continuous(limits = c(-60,60)) +
        scale_y_continuous(limits = c(-90,480)) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              legend.position = "none")
    }, width=400, height=400)
    
    # Makes the launch plot
    output$launchPlot <- renderPlot({
      batter_img <- readPNG("images/batter_image.png", native = TRUE)
      plot_batter <- rasterGrob(batter_img, interpolate = TRUE)
      
      batter_data() %>%
        filter(description == "hit_into_play") %>%
        select(events, launch_angle, launch_speed) %>%
        unique() %>%
        ggplot() +
        annotation_custom(plot_batter,
                          xmin = -30,
                          ymin = -15,
                          xmax = 0,
                          ymax = 15) +
        geom_point(aes(x = launch_speed * cos(launch_angle*(3.14/180)),
                       y = launch_speed * sin(launch_angle*(3.14/180)),
                       fill = events),
                   shape = 21,
                   size = 2.5) +
        geom_segment(x=0,
                     y=0,
                     xend=120,
                     yend=0,
                     color = "gray") +
        geom_segment(x=0,
                     y=0,
                     xend = 120 * cos(45*(3.14/180)),
                     yend = 120 * sin(45*(3.14/180)),
                     color = "gray") +
        theme_minimal() +
        scale_y_continuous(limits = c(-120,120)) +
        scale_x_continuous(limits = c(-30, 120)) +
        theme(axis.title = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_blank(),
              legend.position = "left")
    }, width = 400, height = 400)
    
    output$dataTable <- renderTable({
      batter_data() %>%
        summarise(count = n())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
