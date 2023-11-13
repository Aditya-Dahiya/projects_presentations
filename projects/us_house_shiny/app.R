library(tidyverse)    # all things tidy
library(shiny)        # shiny app
library(sf)           # for maps
library(ggparliament) # for parliament seats plots
library(USAboundaries)# for maps


map_data <- read_csv("https://raw.githubusercontent.com/Aditya-Dahiya/projects_presentations/main/docs/map_data.csv")
totalparl <- read_csv("https://raw.githubusercontent.com/Aditya-Dahiya/projects_presentations/main/docs/totalparl.csv")
propparl <- read_csv("https://raw.githubusercontent.com/Aditya-Dahiya/projects_presentations/main/docs/propparl.csv")


# USA Map: Electoral Districts Boundaries
us_map <- USAboundaries::us_congressional(resolution = "low") |> 
  
  # Extract District Number
  mutate(district = parse_number(namelsad)) |> 
  
  select(state_abbr, district, geometry) |> 
  as_tibble() |> 
  
  # Correct encoding of districts to match our data
  mutate(district = replace_na(district, replace = 0))

###############################################################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "year", 
                      label = "Select the Year of Elections", 
                      min = 1976, 
                      max = 2022, 
                      value = 2020,
                      step = 2,
                      sep = "",
                      round = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("g1"),
           plotOutput("g2"),
           plotOutput("g3")
        )
    )
    
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ################# Plotting Map #################
  
  output$g1 <- renderPlot({
    map_data |> 
      
      # Join with the map geometry column
      left_join(us_map) |> 
      
      # Leave out non-continental USA
      filter(!state_abbr %in% c("PR", "AK", "HI")) |>
      filter(year == input$year) |>
      
      ggplot(aes(fill = party, 
                 geometry = geometry)) + 
      geom_sf(col = "white") + 
      geom_text(aes(label = year, x = -75, y = 48), size = 12) +
      coord_sf() + 
      
      scale_fill_manual(values = c("#3333FF", "#E81B23", "#B4B4B4")) +
      
      theme_void() +
      theme(legend.position = "none")
  })
  
  ##### Plotting Parliament
  output$g2 <- renderPlot({
    
    usparl <- totalparl |> 
      filter(year == input$year)
    
    usparl |> 
      parliament_data(type = "semicircle", 
                      parl_rows = 10,
                      party_seats = usparl$seats) |> 
      ggplot(aes(x = x, y = y, colour = party_short)) +
      geom_parliament_seats() +
      draw_majoritythreshold(n = 218, label = TRUE, type = "semicircle") +
      draw_partylabels(type = "semicircle",
                       party_names = party_long,
                       party_seats = seats,
                       party_colours = colour) +
      geom_highlight_government(government == 1, size = 4) +
      
      labs(title = paste0("USA House of Representatives: ", input$year)) +
      theme_ggparliament(legend = FALSE) +
      scale_colour_manual(values = usparl$colour,
                          limits = usparl$party_short)
  })
  
  ##### Plotting Hypothetical Parliament
  
  output$g3 <- renderPlot({
    
    uspropparl <- propparl |> 
      filter(year == input$year)
    
    uspropparl |> 
      parliament_data(type = "semicircle", 
                      parl_rows = 10,
                      party_seats = uspropparl$seats) |> 
      ggplot(aes(x = x, y = y, colour = party_short)) +
      geom_parliament_seats() +
      draw_majoritythreshold(n = 218, label = TRUE, type = "semicircle") +
      draw_partylabels(type = "semicircle",
                       party_names = party_long,
                       party_seats = seats,
                       party_colours = colour) +
      geom_highlight_government(government == 1, size = 4) +
      
      labs(title = paste0("Hypothetical USA House with seats proportional to popular vote: ", input$year)) +
      theme_ggparliament(legend = FALSE) +
      scale_colour_manual(values = usparl$colour,
                          limits = usparl$party_short) 
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#####################################################################################################################################



