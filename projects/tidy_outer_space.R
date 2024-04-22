#=================== #TidyTuesday : Outer Space Projects ======================#

# Welcome to the data page showcasing the annual number of objects launched into 
# space. This visualization is part of the publication "Space Exploration and 
# Satellites" by Edouard Mathieu and Max Roser (2022). The data has been adapted 
# from the United Nations Office for Outer Space Affairs and processed by Our 
# World in Data. For more information, you can access the original dataset from 
# the United Nations Office for Outer Space Affairs' Online Index of Objects 
# Launched into Outer Space, retrieved on April 21, 2024, at 
# https://ourworldindata.org/grapher/yearly-number-of-objects-launched-into-outer-space.
# 
# Credits:
# https://ourworldindata.org/grapher/yearly-number-of-objects-launched-into-outer-space
# United Nations Office for Outer Space Affairs
# Edouard Mathieu and Max Roser (2022)

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(janitor)        # Cleaning names

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(scales)         # Scale Labels in ggplot2
library(treemapify)     # Tree-Map in the ggplot2
library(colorspace)     # To lighten and darken colours
library(gganimate)      # For animation
library(ggflags)        # For Country Flags

# Other optional libraries
library(glue)           # To paste together text for ggtext

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
# tuesdata <- tidytuesdayR::tt_load('2024-04-23')

outer_space_objects <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv'
  ) |> 
  janitor::clean_names()

# Names and Codes for Countries
country_codes <- read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/61b2107766d6fd51e2bd02d9f78f6be081340efc/countries_codes_and_coordinates.csv") |> 
  clean_names() |> 
  select(entity = country,
         code2 = alpha_2_code,
         code3 = alpha_3_code) |> 
  mutate(code2 = str_to_lower(code2))


#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Raleway", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Roboto Condensed", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Colour palette
mypal <- paletteer::paletteer_d("MetBrewer::Signac", direction = -1)[1:8]
mypal <- mypal |> darken(0.2)

# Define colours
text_col <- "grey25"               # Colour for the text
text_hil <- "grey40"               # Colour for highlighted text
bg_col <- "grey90"                    # Background Colour

# Define Text Size
ts =  90             # Text Size

# Add text to plot
plot_title <- "Objects launched in Outer Space upto {frame_along}"
plot_caption <- paste0("Data: United Nations Office for Outer Space Affairs.  |  Graphics: Aditya-Dahiya on GitHub")

#==============================================================================#
# Exploratory Data Analysis & Data Wrangling -----------------------------------
#==============================================================================#
top_countries <- outer_space_objects |> 
  group_by(entity) |> 
  summarise(
    total_objects = sum(num_objects)
  ) |> 
  arrange(desc(total_objects)) |> 
  slice_max(order_by = total_objects, n = 10) |> 
  pull(entity)

plotdf <- outer_space_objects |> 
  filter(entity %in% top_countries) |> 
  filter(!(entity %in% c("World", "European Space Agency"))) |> 
  group_by(entity) |> 
  mutate(cum_objects = cumsum(num_objects)) |> 
  left_join(country_codes)

worlddf <- outer_space_objects |> 
  filter(entity == "World") |> 
  group_by(entity) |> 
  mutate(cum_objects = cumsum(num_objects))

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g <- ggplot(
  data = plotdf,
  mapping = aes(
    x = year,
    y = cum_objects,
    colour = entity,
    label = entity
    )
  ) +
  geom_line(
    linewidth = 1,
    alpha = 0.75
  ) +
  geom_flag(
    aes(country = code2),
    size = 12
    ) +
  # geom_text(
  #   aes(
  #     y = 0.95 * cum_objects,
  #     label = entity
  #   ),
  #   nudge_x = -2,
  #   colour = text_col,
  #   size = 3,
  #   hjust = "inward",
  #   na.rm = TRUE,
  #   check_overlap = FALSE
  # ) +
  # geom_text(
  #   data = worlddf,
  #   aes(
  #     x = 1957, 
  #     y = Inf,
  #     label = paste0("Globally total objects\n Launched: ", 
  #                    as.character(cum_objects))
  #   ),
  #   colour = text_col,
  #   hjust = "inward",
  #   vjust = "inward",
  #   family = "caption_font",
  #   lineheight = 0.75,
  #   size = 7
  #   ) +
  scale_y_continuous(
    expand = expansion(0)
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = seq(1940, 2020, 20)
  ) +
  scale_colour_manual(values = mypal) +
  # Labels for the plot
  labs(
    title = plot_title,
    caption = plot_caption,
    x = "Year", 
    y = NULL
  ) +
  theme_minimal(
    base_family = "body_font",
    base_size = 15
  ) +
  theme(
    panel.grid.major = element_line(
      linewidth = 0.25, 
      linetype = 3,
      colour = "grey50"
    ),
    panel.grid.minor = element_line(
      linewidth = 0.25, 
      linetype = 3,
      colour = "grey50"
    ),
    axis.line = element_line(
      linewidth = 0.5,
      linetype = 1,
      colour = text_hil,
      lineend = "round",
      arrow = arrow(length = unit(2, "mm"))
    ),
    plot.background = element_rect(
      fill = bg_col,
      colour = NA,
      linewidth = NA
    ),
    legend.position = "none",
    plot.caption = element_text(
      colour = text_hil,
      hjust = 0.5,
      family = "caption_font",
      margin = margin(5, 0, 10, 0, "pt")
    ),
    plot.title = element_text(
      colour = text_hil,
      family = "body_font",
      hjust = 0,
      size = 18,
      face = "bold",
      margin = margin(10, 0, 10, 0, "pt")
    ),
    axis.text.y = element_text(
      colour = text_col,
      margin = margin(0, 0, 0, 15, "pt")
    ),
    axis.text.x = element_text(
      colour = text_col
    ),
    axis.title.x = element_text(
      colour = text_col,
      margin = margin(20, 0, 2, 0, "pt")
    )
  ) +
    # For animation: Creating facets by year, and then removing them
  # facet_wrap(~ year) +
  # facet_null() + 
  # Create a time-based animation 
  transition_reveal(as.integer(year)) +
  view_follow(fixed_x = FALSE, fixed_y = FALSE)


#==============================================================================#
# Animation Saving -------------------------------------------------------------
#==============================================================================#

anim_save(
  filename = here::here("docs", "tidy_outer_space.gif"),
  animation = g,
  start_pause = 0,
  end_pause = 40,
  fps = 15,
  duration = 20,
  height = 500,
  width = 500,
  units = "px"
)
