#=================An Attempt at Shadowed Designer Bar Chart====================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(patchwork)      # For compiling plots
library(magick)         # Work with Images and Logos
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

nhl_rosters |>
  ggplot(aes(
    x = year(birth_date),
    y = height_in_inches)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  geom_smooth()

range_heights <- nhl_rosters |> 
  mutate(birth_year = year(birth_date)) |> 
  group_by(birth_year) |> 
  summarize(
    mean_height = mean(height_in_centimeters, na.rm = TRUE),
    sd_height = sd(height_in_centimeters, na.rm = TRUE)
  ) |> 
  mutate(
    top_height = mean_height + (2 * sd_height),
    bottom_height = mean_height - (2 * sd_height),
    
  ) |> 
  drop_na()

df1 <- nhl_rosters |> 
  group_by(player_id) |> 
  filter(row_number() == 1) |> 
  mutate(birth_year = year(birth_date)) |> 
  group_by(birth_year) |> 
  mutate(mean_height = mean(height_in_centimeters, na.rm = TRUE)) |> 
  ungroup() |> 
  filter(birth_year != 2005)

top_heights <- df1 |> 
  group_by(birth_year) |> 
  mutate(max_height = max(height_in_centimeters, na.rm = TRUE)) |> 
  slice_max(height_in_centimeters, n = 1) |> 
  ungroup() |> 
  slice_max(height_in_centimeters, n = 30) |> 
  pull(player_id)

bottom_heights <- df1 |> 
  group_by(birth_year) |> 
  mutate(min_height = min(height_in_centimeters, na.rm = TRUE)) |> 
  slice_min(height_in_centimeters, n = 1) |> 
  ungroup() |> 
  slice_min(height_in_centimeters, n = 10) |> 
  pull(player_id)

# To see the outlier player IDs
df1 |> 
  ggplot(aes(
    x = birth_year,
    y = height_in_centimeters,
    label = player_id
  )) +
  geom_point(alpha = 0.2) +
  geom_text(check_overlap = TRUE)

display_photos <- c(
  8446458, 
  8465009,
  8464875,
  8474574,
  8449991,
  8473722,
  8450153,
  8471804,
  8450140,
  8476227,
  8449896
)

df2 <- df1 |> 
  filter(player_id %in% display_photos)

# Caption Coords

capc <- df1 |> 
  select(birth_year, mean_height) |> 
  filter(birth_year == 1944) |> 
  slice_head(n = 1) |> 
  as_vector()

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Graduate", 
                family = "title_font",
                bold.wt = 900)               # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Oswald", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Creating a Colour Palette for the Visualization
# Colour Palette
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
dot_col <- mypal[1]                   # Heat map: low colour
bar_col <- mypal[4]                   # Heat map: high colour
bg_col <- "#f5f5f5"                   # Background Colour
text_col <- mypal[5]                  # Colour for the text
text_hil <- mypal[4]                  # Colour for highlighted text

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
plot_title <- "NHL Players are getting taller over time !"

subtitle_text <- "The average height of NHL players has increased over the years. The red dots show the mean of heights for players born that year, while the error bar shows the range within which 95% of heights fall (i.e., mean +/- 2 SDs). The photos are of players too tall or short for their era."
plot_subtitle <- paste(strwrap(subtitle_text, 100), collapse = "\n")

plot_caption <- paste0("**Data:** JLaw's R Blog | ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g <- df1 |>   
  ggplot(aes(
    x = birth_year,
    y = height_in_centimeters)) +
  geom_errorbar(
    data = range_heights,
    mapping = aes(
      x = birth_year,
      ymin = bottom_height,
      ymax = top_height,
      y = mean_height),
    color = "grey"
  ) +
  geom_point(
    aes(y = mean_height),
    color = dot_col
  ) +
  geom_point(
    data = df2,
    mapping = aes(y = height_in_centimeters),
    color = bar_col
  ) +
  geom_image(
    data = df2,
    mapping = aes(
      image = headshot,
      x = birth_year,
      y = height_in_centimeters
    )
  ) +
  geom_text(
    data = df2,
    mapping = aes(
      x = birth_year,
      y = height_in_centimeters - 2,
      label = paste0(first_name, " ", last_name, "\n", 
                     height_in_centimeters, " cm")
    ),
    color = text_col,
    family = "caption_font",
    size = ts / 3,
    lineheight = 0.25
  ) +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = "Birth Year",
       y = "Height (in cm)") +
  scale_x_continuous(breaks = seq(1880, 2000, 20)) +
  theme_minimal() +
  theme(
  panel.grid = element_line(color = "lightgrey",
                            linetype = 3,
                            linewidth = 0.3),
  panel.grid.minor.y = element_line(color = "lightgrey",
                                    linetype = 3,
                                    linewidth = 0.3),
  panel.grid.minor.x = element_blank(),
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = 1.5 * ts),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 3.5 * ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_hil,
                                  margin = margin(4,0,2,0)),
  plot.subtitle    = element_text(hjust = 0.5,
                                  size = 2.1 * ts,
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
  axis.title = element_text(size = 2 * ts,
                           family = "body_font",
                           colour = text_col,
                           margin = margin(0,0,0,0),
                           hjust = 0.5),
  axis.line = element_line(color = "grey",
                           arrow = arrow(length = unit(3, "mm"))),
  plot.title.position = "plot"
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "tidy_nhl1_Jan_2024.png"),
  plot = g,
  width = 20, 
  height = 20, 
  units = "cm",
  bg = bg_col
)
