#======================= #TidyTuesday : Shiny Packages ========================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(scales)         # Scale Labels in ggplot2

# Network data manipulation and visualization
library(tidygraph)
library(ggraph)

# Other optional libraries
library(glue)           # To paste together text for ggtext
#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")
# tuesdata <- tidytuesdayR::tt_load(2024, week = 16)
# 
# shiny_revdeps <- tuesdata$shiny_revdeps
# package_details <- tuesdata$package_details
# rm(tuesdata)
shiny_revdeps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-16/shiny_revdeps.csv')


#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Gotu", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Pompiere", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Define colours
text_col <- "#F2EBBBFF"               # Colour for the text
text_hil <- "#DDAA33FF"               # Colour for highlighted text
bg_col <- "grey10"                    # Background Colour
text_hil2 <- "red"                    # Highlight colour for town names 

# Define Text Size
ts =  90             # Text Size
tsi = ts / 1.5       # Text Size Inset

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- ""
subtitle_text <- glue::glue("")
plot_subtitle <- subtitle_text
plot_caption <- paste0("**Data & Inspiration:**  |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)


#==============================================================================#
# Exploratory Data Analysis & Data Wrangling -----------------------------------
#==============================================================================#

# library(summarytools)
# dfSummary(shiny_revdeps) |> view()

# Number of parent packages to visualize
nos_parent_pckgs <- 10
# Number of child packages to visualize
nos_child_pckgs <- 50

top_pkgs <- shiny_revdeps |> 
  count(parent, sort = T) |> 
  slice_max(order_by = n, n = nos_parent_pckgs) |> 
  pull(parent)

filter_pckgs <- shiny_revdeps |> 
  filter(parent %in% top_pkgs) |> 
  count(child, sort = T) |> 
  slice_max(order_by = n, n = nos_child_pckgs, with_ties = FALSE) |> 
  pull(child)

graph_df <- shiny_revdeps |> 
  filter(parent %in% top_pkgs) |> 
  filter(child %in% filter_pckgs) |> 
  rename(
    from = child,
    to = parent
  ) |> 
  relocate(dependency_type, .after = everything())

count_pckgs <- graph_df |> 
  select(from, to) |> 
  pivot_longer(
    cols = everything(),
    names_to = NULL, 
    values_to = "name"
  ) |> 
  count(name, sort = TRUE) |> 
  rename(importance = n)

plot_df <- as_tbl_graph(graph_df, directed = TRUE) |> 
  activate(nodes) |> 
  left_join(count_pckgs) |> 
  mutate(display_name = if_else(
    name %in% top_pkgs,
    name,
    NA
  ))

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g <- plot_df |> 
  ggraph(layout = "graphopt") +
  geom_edge_link(
    aes(colour = dependency_type),
    alpha = 0.1
  ) +
  geom_node_point(
    aes(size = importance),
    colour = "grey25",
    alpha = 0.5
  ) +
  geom_node_text(
    aes(size = importance, label = display_name),
    colour = "grey10",
    check_overlap = TRUE
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  )


#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_shiny_packages.png"),
  plot = g,
  width = 40, 
  height = 55, 
  units = "cm",
  bg = "white"
)
