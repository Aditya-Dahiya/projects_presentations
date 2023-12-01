# #TidyTuesday Week 48
# Doctor Who Episodes

# Loading required libraries----------------------------------------------------
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

# Loading the data--------------------------------------------------------------

# Option 1: tidytuesdayR package 

tuesdata <- tidytuesdayR::tt_load(2023, week = 48)

episodes <- tuesdata$drwho_episodes |> 
  select(-c(era, serial_title, production_code))
writers <- tuesdata$drwho_writers

# Exploratory Data Analysis-----------------------------------------------------

# visdat::vis_dat(episodes)
# visdat::vis_dat(directors)
# visdat::vis_dat(writers)
# dfSummary(episodes) |> view()

# exploring how viewership relates to ratings - are they directly proportional or not
# lm(episodes$uk_viewers ~ episodes$rating) |> summary()
# cor(episodes$uk_viewers, episodes$rating)
# ggplot(episodes,
#       aes(x = rating, 
#           y = uk_viewers)) +
#  geom_jitter(aes(size = uk_viewers),
#              alpha = 0.5) +
#  geom_smooth(method = "lm", se = FALSE) +
#  ggrepel::geom_text_repel(aes(label = episode_title))
# Mostly positive correlation, but some outliers  

# Visualization parameters------------------------------------------------------

# Load fonts
font_add_google("Acme", "title_font")              # Font for titles
font_add_google("Pragati Narrow", "caption_font")  # Font for the caption
font_add_google("Nova Square", "body_font")        # Font for plot text
showtext_auto()

# Creating a Colour Palette for the Visualization
# Image to extract
# img <- "https://m.media-amazon.com/images/M/MV5BMDFiMGU1MmQtMTg0Ny00ZmQ4LTkyMWMtYjVlZGRmMWY5ZDliXkEyXkFqcGdeQXVyMTA0MTI2ODE4._V1_.jpg"

# Number of Colours to have in the palette
# col_numbers = 12
# set.seed(3)
# Colour Palette
# mypal <- get_colors(img) |> 
#   make_palette(n = col_numbers)
# mypal

# Define colours
plotcol <-c("#34ebd5", "#6e50bf", "#8d8e8f")      # Scatter Plot palette
bg_col <- "#010c3b"                     # Background Colour
text_col <- "#CBCBCB"                   # Colour for the text
text_hil <- "#91e4ff"                   # Colour for highlighted text

# Define Text Size
ts = 24                              # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
mastodon <- "&#xf4f6"
mastodon_username <- "@adityadahiya@mastodon.social"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: #CBCBCB'>{github_username}  </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: #CBCBCB'>{xtwitter_username}</span>"
)

# Add text to plot--------------------------------------------------------------
plot_title <- "Doctor Who Episodes: Writers' impact on Ratings and Viewership"

plot_subtitle <- "The ratings (and viewership) of Doctor Who first rose, then peaked during Season 4 and thereafter declined gradually. Of the many factors that could explain this rise and fall,\nlet's focus on Writers. Two writers dominate authorship. Ratings rose and peaked during Russell Davies' time; plateaued and started declining during Steven Moffat's \ntenure, and thereafter declined considerably. This could, perhaps, be one of the reasons of falling popularity and ratings."

plot_caption <- paste("Source: Wikipedia", "| Data Compilation: {datardis} package by Jonathan Kitt |", "#TidyTuesday  |  Graphics: Aditya Dahiya ", social_caption)

# Dr Who Logo work and Background Image

drwho_logo <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/4/43/Doctor_Who_Logo_2023.svg/512px-Doctor_Who_Logo_2023.svg.png") |> 
  image_trim() |>
  image_negate() |> 
  image_scale("x300") |> 
  image_crop("+0+30")

image_read(here::here("docs", "raw-file-drwho.jpg")) |> 
  image_composite(drwho_logo,
                  offset = "+1650+800") |> 
  image_write(path = here::here("docs", "dr-who-bg.png"),
              format = "png")


# Actual Plots------------------------------------------------------------------

season_data <- episodes |> 
  left_join(writers) |> 
  mutate(writer = fct_lump_n(writer, n = 2)) |> 
  arrange(first_aired, season_number, episode_number) |> 
  mutate(id = row_number()) |> 
  fill(season_number) |>
  group_by(season_number) |> 
  summarize(begin = min(id),
            end = max(id)) |> 
  mutate(mid = (begin + end)/2)

g <- episodes |> 
  left_join(writers) |> 
  mutate(writer = fct_lump_n(writer, n = 2)) |> 
  arrange(first_aired, season_number, episode_number) |> 
  mutate(id = row_number()) |>
  ggplot(aes(x = id,
             y = rating)) +
  
  geom_jitter(aes(col = writer,
                  size = uk_viewers),
              alpha = 0.6) +
  ggrepel::geom_text_repel(aes(label = episode_title),
            size = ts/7,
            max.overlaps = 4,
            family = "body_font",
            colour = text_col) +
  geom_errorbar(data = season_data,
                mapping = aes(x = NULL, 
                              xmin = begin, 
                              xmax = end, 
                              y = 72),
                colour = text_col,
                linetype = 2) +
  geom_text(data = season_data,
            mapping = aes(y = 72, 
                          x = mid,
                          label = paste0("Season ", season_number)),
            col = text_col, 
            family = "body_font",
            vjust = -1,
            size = ts/5) +
  labs(x = NULL,
       y = "Episode's Ratings",
       title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       col = "Writer",
       size = "U.K. viewers (in millions)") +
  scale_color_manual(values = plotcol) +
  scale_y_continuous(breaks = seq(74, 92, 2),
                     limits = c(70, 92)) +
  scale_size_continuous(range = c(1, 10)) +
  theme_minimal() +
  guides(size = guide_legend(override.aes = list(colour = text_col)),
         colour = guide_legend(override.aes = list(size = 5))) + 
  theme(
    plot.caption =  element_textbox(family = "caption_font",
                                    hjust = 0.5,
                                    colour = text_col,
                                    size = ts/1.5),
    plot.title   =     element_text(hjust = 0,
                                    size = 2 * ts,
                                    family = "title_font",
                                    face = "bold",
                                    colour = text_hil),
    plot.subtitle    = element_text(hjust = 0,
                                    size = ts/1.5,
                                    family = "body_font",
                                    colour = text_col),
    plot.background =  element_rect(fill = bg_col,
                                    color = bg_col,
                                    linewidth = 0),
    panel.grid = element_blank(),
    axis.text.y = element_text(colour = text_col,
                               size = ts/2),
    axis.line.y = element_line(colour = text_col, linetype = 2),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_text(family = "body_font",
                              colour = text_col,
                              size = ts/1.2,
                              hjust = 1),
    legend.text = element_text(colour = text_col,
                               family = "body_font"),
    legend.title = element_text(colour = text_col,
                                family = "body_font",
                                face = "bold"),
    legend.position = c(1, 0.8),
    plot.margin = margin(10, 150, 10, 5)
    
)

g <- ggbackground(g, background = here::here("docs", "dr-who-bg.png"))
g
# Saving the final image--------------------------------------------------------
ggsave(
  filename = here::here("docs", "drwho_tidy.png"),
  plot = g,
  device = "png", dpi = "retina", 
  width = 1920, height = 1080, units = "px",
  bg = bg_col)
)


