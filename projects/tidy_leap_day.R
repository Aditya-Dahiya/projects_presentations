# TidyTuesday: Leap Day

# ==============================================================================#
# Library Load-in---------------------------------------------------------------
# ==============================================================================#
library(tidyverse) # Data Wrangling and Plotting
library(here) # Files location and loading
library(showtext) # Using Fonts More Easily in R Graphs
library(ggimage) # Using Images in ggplot2
library(fontawesome) # Social Media icons
library(ggtext) # Markdown Text in ggplot2
library(scales) # Labeling the axes
library(see) # Half-Violin / Half-DotPlot
library(patchwork) # For compiling plots
library(colorspace) # Lighten and Darken Colours

# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#

# Option 1: tidytuesdayR package
tuesdata <- tidytuesdayR::tt_load('2024-02-27')

events <- tuesdata$events
births <- tuesdata$births
deaths <- tuesdata$deaths

rm(tuesdata)

# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#

library(summarytools)
dfSummary(births) |> view()
dfSummary(events) |> view()
dfSummary(deaths) |> view()

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#

df <- bind_rows(
  events |> 
    mutate(type = "Events"),
  
  births |> 
    rename(
      year = year_birth,
      event = person
    ) |> 
    mutate(type = "Births") |> 
    select(year, event, type),
  
  deaths |> 
    rename(
      year = year_death,
      event = person
    ) |> 
    mutate(type = "Deaths") |> 
    select(year, event, type)
) |> 
  mutate(
    type = fct(type, levels = c("Births", "Deaths", "Events"))
  )

# Looking at day of the week
df_wd <- df |> 
  mutate(
    leap_date = make_date(
      year = year,
      month = 2,
      day = 29
    ),
    day = wday(leap_date, label = TRUE, abbr = F)
  )

# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Rampart One",
  family = "title_font"
) # Font for titles
font_add_google("Yanone Kaffeesatz",
  family = "caption_font"
) # Font for the caption
font_add_google("Oregano",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Chossing one Palette for the Visualization
mypal <- paletteer::paletteer_d("nbapalettes::hornets2")[c(1, 2, 4)]

# Define colours
bg_col <- "white"  # Background Colour
text_col <- mypal[2] # Colour for the text
text_hil <- mypal[1] # Colour for highlighted text

# Define Text Size
ts <- unit(20, units = "cm") # Text Size

# Caption stuff
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_hil}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_hil}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "Leap Day"

plot_caption <- paste0("**Data:** Wikipedia.com", " | ", " #TidyTuesday | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

subtitle_text <- "An Analysis of Lead Day (February 29) events in the 20th and 21st Centuries listed on www.wikipedia.com. As expected, there is a bias towards recent events, as shown below through a Half-Violin-Half-Dotplot Visualization. Each dot represents a birth, death or an event."
plot_subtitle <- str_wrap(subtitle_text, width = 90)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- df |> 
  filter(year >= 1900) |>  
  ggplot(
  mapping = aes(
    y = year,
    x = type,
    fill = type
    )
  ) +
  geom_violindot(
    size_dots = 8,
    trim = FALSE,
    scale = "area",
    show.legend = FALSE,
    binwidth = 0.5,
    width = 1
  ) +
  scale_y_continuous(
    limits = c(1880, 2020),
    breaks = seq(1900, 2020, 20),
    expand = expansion(c(0, 0.02))
  ) +
  scale_x_discrete(
    expand = expansion(0),
    position = "top"
  ) +
  scale_fill_manual(values = mypal) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      linewidth = 1,
      linetype = 2
    ),
    axis.ticks.y = element_line(
      linewidth = 1,
      linetype = 1,
      colour = text_col,
      lineend = "round",
      size = 3
    ),
    axis.text.x = element_text(
      size = 10 * ts,
      colour = text_col,
      hjust = 0.5, 
      family = "body_font",
      margin = margin(1,0,0,0, unit = "cm")
    ),
    axis.text.y = element_text(
      size = 5 * ts,
      colour = text_col,
      hjust = 0.5, 
      family = "caption_font",
      margin = margin(0, 0.2, 0, 1, unit = "cm")
    ),
    plot.title = element_text(
      size = 24 * ts,
      colour = text_hil,
      margin = margin(2, 0, 1, 0, unit = "cm"),
      family = "title_font",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 5.7 * ts,
      colour = text_col,
      margin = margin(1,0,1,0, unit = "cm"),
      family = "body_font",
      lineheight = 0.3,
      hjust = 0.5
    ), 
    plot.caption = element_textbox(
      size = 4 * ts,
      colour = text_hil,
      margin = margin(1,0,1,0, unit = "cm"),
      family = "caption_font",
      hjust = 0.5
    ),
    panel.grid = element_blank(),
    plot.title.position = "plot"
  )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#


ggsave(
  filename = here::here("docs", "tidy_leap_day.png"),
  plot = g,
  width = 40,
  height = 50,
  units = "cm",
  bg = bg_col
)

