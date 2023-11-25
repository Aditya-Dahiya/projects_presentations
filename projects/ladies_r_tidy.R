# Loading required libraries----------------------------------------------------
library(tidyverse)      # tidy tools data wrangling
library(ggtext)         # text into ggplot2
library(sf)             # maps and plotting
library(here)           # files location and loading
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(summarytools)   # Exploratory Data Analysis
library(figpatch)       # Images in patchwork
library(patchwork)      # For compiling plots

# Loading the data
# Option 2: Read directly from GitHub

rladies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')

# Exploratory Data Analysis
rladies |> 
  visdat::vis_dat()

view(summarytools::dfSummary(rladies))

# Getting the data into a tidy form for plotting
df <- rladies |> 
  select(date, location) |> 
  mutate(year = year(date),
         month = month(date, label = TRUE, abbr = TRUE)) |> 
  count(year, month) |> 
  mutate(year = as.character(year))


# Visualization parameters------------------------------------------------------

# Load fonts
font_add_google("Fjalla One", "title_font")
font_add_google("Pragati Narrow", "caption_font")
font_add_google("Fjalla One", "body_font")
showtext_auto()
body_font <- "body_font"           # Font for plot legends, body etc.
title_font <- "title_font"         # Font for titles, subtitles
caption_font <- "caption_font"     # Font for the caption

# Define colours
low_col <- "#f593d7"               # Heat map: low colour
hi_col <- "#c7028c"                # Heat map: high colour
bg_col <- "#f7c8e9"                # Background Colour
text_col <- "#750153"              # Colour for the text
text_hil <- "#e30039"              # Colour for highlighted text

# Define Text Size
ts = 24                            # Text Size

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
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: #000000'>{github_username}  </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: #000000'>{xtwitter_username}</span>"
)

# Add text to plot--------------------------------------------------------------
plot_title <- "R-Ladies: Frequency of Chapter Events over the years!"

plot_subtitle <- "Chapter Events of R-Ladies Global, founded by Gabriela de Queiroz to foster gender diversity in the R programming community."

plot_caption <- paste0("Data: Federica Gazzelloni. R-Ladies Chapters. #TidyTuesday. Graphics by: ", social_caption)

# Actual Plots------------------------------------------------------------------

p_year <- df |>
  count(year, wt = n) |> 
  ggplot(aes(x = n, 
             y = year, 
             label = n)) +
  geom_col(col = NA, 
           fill = hi_col) +
  geom_text(hjust = - 0.5) +
  scale_x_continuous(limits = c(0, 1000)) +
  theme_nothing()
p_year

p_month <- df |> 
  count(month, wt = n) |> 
  ggplot(aes(x = month, 
             y = n,
             label = n)) +
  geom_col(fill = hi_col,
           col = NA) +
  geom_text(vjust = -1) +
  scale_y_continuous(limits = c(0, 500)) +
  theme_nothing()
p_month

p1 <- df |> 
  ggplot(aes(x = month, 
             y = year,
             fill = n,
             label = n)) +
  geom_tile(col = bg_col) +
  geom_text() +
  scale_fill_gradient(low = low_col, high = hi_col) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(family = body_font,
                                 colour = text_col))
p1
# Create a plot for Logo of R-Ladies

p2 <- figpatch::fig("https://rladies.org/images/logo.png",
                    b_col = low_col,
                    b_size = 3)  
  
# Failed attempt: Using a new package "aplot" to align plots together-----------
  
# library(aplot)

# g <- p1 + labs(title = plot_title,
#               subtitle = plot_subtitle,
#               caption = plot_caption) |> 
#  insert_top(p_month, height = 0.3) |> 
#  insert_right(p_year, width = 0.5)

# class(g)

# Doesn't work for me for plot annotations etc.

# Compiling Plot: Patchwork-----------------------------------------------------

library(patchwork)

g <- p_month + p2 + p1 + p_year +
  plot_layout(widths = c(2, 1),
              heights = c(1, 2)) +
  plot_annotation(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) &
  theme(
    plot.caption = element_textbox(family = caption_font,
                                          hjust = 1,
                                          colour = text_col),
    plot.title = element_text(hjust = 0.5,
                              size = ts,
                              family = title_font,
                              face = "bold",
                              colour = text_col),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = ts/1.5,
                                 family = body_font,
                                 colour = text_col),
    plot.background = element_rect(fill = bg_col,
                                   color = bg_col,
                                   linewidth = 0
                                   )
    )

# Saving the final image--------------------------------------------------------
ggsave(
  filename = here::here("docs", "ladies_r_tidy.png"),
  plot = g,
  device = "png", dpi = "retina", 
  width = 10, height = 7, units = "cm",
  bg = bg_col)
)


