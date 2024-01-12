#=================An Attempt at Shadowed Designer Bar Chart====================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(summarytools)   # Exploratory Data Analysis
library(colorfindr)     # To get colour palettes for the Viz
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(patchwork)      # For compiling plots
library(figpatch)       # Images in patchwork
library(magick)         # Work with Images and Logos
library(ggimage)        # Background Image
library(scales)         # ggplot2 labels and scaling

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2024-01-09')

canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams
rm(tuesdata)

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

# Due to a tough work-week, trying to reproduce the figure from J Law's R Blog
# hosted at https://jlaw.netlify.app/2023/12/04/are-birth-dates-still-destiny-for-canadian-nhl-players/

canada_births_1991_2022

nhl_player_births

nhl_rosters

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

# For overall population, proportion of births in each month
all_can <- canada_births_1991_2022 |> 
  group_by(month) |> 
  summarise(
    all_births = sum(births, na.rm = TRUE)
  ) |> 
  mutate(all_prop = all_births / sum(all_births))

# For NHL players, proportion of births in each month
nhl_can <- nhl_player_births |> 
  mutate(month = month(birth_date)) |> 
  group_by(month) |> 
  count(name = "nhl_births") |> 
  ungroup() |> 
  mutate(nhl_prop = nhl_births / sum(nhl_births))

# Month names
month.name
# Final dataframe for plotting

df <- all_can |> 
  left_join(nhl_can) |> 
  
  # Long names for the months
  mutate(month_name = fct(month.name,
                          levels = month.name),
         month_name = fct_rev(month_name)) |> 
  
  # Default expected proportion of births in each month
  mutate(
    default_prop = case_when(
      month %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31/365,
      month %in% c(4, 6, 9, 11) ~ 30/365,
      month == 2 ~ 28.25/365
    )
  )

#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Graduate", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Oswald", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

nhl1 <- image_read("https://pbs.twimg.com/media/F9sTTAYakAAkRv6.png") |> 
  image_resize("x100")
nhl <- "https://pbs.twimg.com/media/F9sTTAYakAAkRv6.png"

canada1 <- image_read("https://cdn-icons-png.flaticon.com/512/5372/5372678.png") |> 
  image_resize("x100")

canada <- "https://cdn-icons-png.flaticon.com/512/5372/5372678.png"
# Creating a Colour Palette for the Visualization
# Colour Palette
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
low_col <- "#fa0000"                  # Heat map: low colour
hi_col <- "#0800fa"                   # Heat map: high colour
bg_col <- "#e8e8e8"                   # Background Colour
text_col <- "#00004d"                 # Colour for the text
text_hil <- "#700000"                 # Colour for highlighted text

# Define Text Size
ts = unit(20, units = "cm")                             # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
linkedin <- "&#xf08c"
linkedin_username <- "dr-aditya-dahiya-ias"
social_caption <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span> <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span> <span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin};</span> <span style='color: {text_col}'>{linkedin_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "More NHL Players have births in earlier months !"

subtitle_text <- "Due to the January 1 cut-off date in NHL age-groups, players born in earlier months of the year could have a physical advantage over younger players (born in later months). Hence, we see a higher percentage of NHL players born in earlier months of the year, than the general Canadian population."
plot_subtitle <- paste(strwrap(subtitle_text, 150), collapse = "\n")

plot_caption <- paste0("**Data & Inspiration:** JLaw's R Blog | ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#


g <- ggplot(
  data = df, 
  mapping = aes(
    y = month_name,
    yend = month_name
  )
) +
  geom_point(
    aes(default_prop),
    color = "lightgrey",
    size = 4
  ) +
  geom_line(
    mapping = aes(
      x = default_prop,
      group = 1
    ), 
    orientation = "y",
    color = "lightgrey",
    linewidth = 2
  ) +
  geom_segment(
    mapping = aes(
      x = all_prop,
      xend = nhl_prop,
      col = (all_prop > nhl_prop)
    )
  ) +
  geom_text(
    aes(label = percent(all_prop, accuracy = 0.1),
        x = if_else(
          all_prop > nhl_prop,
          all_prop + 0.002,
          all_prop - 0.002
        )
    ),
    col = text_col,
    family = "body_font",
    size = ts/2
  ) +
  geom_text(
    aes(label = percent(nhl_prop, accuracy = 0.1),
        x = if_else(
          all_prop > nhl_prop,
          nhl_prop - 0.002,
          nhl_prop + 0.002
        )
    ),
    col = text_col,
    family = "body_font",
    size = ts/2
  ) +
  annotate(
    geom = "curve",
    x = 0.081,
    y = 10.5,
    xend = 0.075,
    yend = 8,
    color = "lightgrey",
    curvature = -0.2,
    arrow = arrow(length = unit(3, "mm"))
  ) +
  annotate(
    geom = "text",
    x = 0.075,
    y = 8,
    label = "Light grey line shows expected %\n(assuming randomness in births)",
    lineheight = 0.4,
    hjust = 1,
    color = "darkgrey",
    family = "body_font",
    size = ts/2
  ) +
  scale_color_manual(values = c(hi_col, low_col)) +
  scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
  guides(color = "none") +
  labs(x = "Percentage births in the month",
       y = NULL) +
  geom_image(
    mapping = aes(
      x = nhl_prop,
      image = nhl)
  ) +
  geom_image(
    mapping = aes(
      x = all_prop,
      image = canada
    )
  ) +
  annotation_custom(
    grid::rasterGrob(canada1),
    xmin = 0.09,
    xmax = 0.093,
    ymin = 2.6,
    ymax = 3.4
  ) +
  annotate(
    geom = "text",
    x = 0.094,
    y = 3,
    label = "Average Canadians (1991-2022)",
    color = "darkgrey",
    family = "body_font",
    hjust = 0,
    size = ts / 2
  ) +
  annotation_custom(
    grid::rasterGrob(nhl1),
    xmin = 0.09,
    xmax = 0.093,
    ymin = 1.6,
    ymax = 2.4
  ) +
  annotate(
    geom = "text",
    x = 0.094,
    y = 2,
    label = "NHL Players",
    color = "darkgrey",
    family = "body_font",
    hjust = 0,
    size = ts / 2
  ) +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption) +
  theme_minimal() +
  theme(
  panel.grid = element_blank(),
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = 1.5 * ts),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 3*ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_hil,
                                  margin = margin(4,0,2,0)),
  plot.subtitle    = element_text(hjust = 0,
                                  size = 1.6 * ts,
                                  family = "body_font",
                                  colour = text_col,
                                  margin = margin(5,0,2,0),
                                  lineheight = 0.35),
  plot.background =  element_rect(fill = bg_col,
                                  color = bg_col,
                                  linewidth = 0),
  axis.text = element_text(size = 1.5 * ts,
                           family = "body_font",
                           colour = text_col,
                           face = "bold",
                           margin = margin(0,0,0,0)),
  axis.title.x = element_text(size = 2 * ts,
                           family = "body_font",
                           colour = text_col,
                           margin = margin(0,0,0,0),
                           hjust = 1),
  axis.line.x = element_line(color = "grey",
                             arrow = arrow(length = unit(3, "mm"))),
  plot.title.position = "plot"
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "tidy_nhl_Jan_2024.png"),
  plot = g,
  width = 20, 
  height = 20, 
  units = "cm",
  bg = bg_col
)
