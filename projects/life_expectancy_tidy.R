#================== #TidyTuesday Week 49: Life Expectancy =====================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(summarytools)   # Exploratory Data Analysis
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(magick)         # Work with Images and Logos
library(ggimage)        # Background Image
library(janitor)        # cleaning variable names
library(gganimate)      # Animation

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#
# Option 1: tidytuesdayR package 

tuesdata <- tidytuesdayR::tt_load(2023, week = 49)

life <- tuesdata$life_expectancy |> clean_names()
life_ages <- tuesdata$life_expectancy_different_ages |> clean_names()
life_fm <- tuesdata$life_expectancy_female_male |> clean_names()
rm(tuesdata)

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

# Data is already so clean and awesome

summary(life)
dfSummary(life_ages) |> view()
# Ideas
# 1. An animated racing graph of life expectancy
# 2. Comparison of le_0 vs. le_65 for income wise countries

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#


vars <- c("High-income countries", 
          "Least developed countries",
          "Less developed regions, excluding least developed countries",
          "Lower-middle-income countries",
          "Low-income countries",
          "Middle-income countries",
          "More developed regions",
          "Upper-middle-income countries"
)

focus <- c("Low-income countries",
           "Lower-middle-income countries",
           "Middle-income countries",
           "Upper-middle-income countries",
           "High-income countries")

# Comparison of life expectancy at birth and 80 years for income levels 
# over the last 50 years

facet_label <- c("Life expectancy at birth\n(Converging)",
                 "Life expectancy at 80 years\n(Diverging)")
names(facet_label) <- c("life_expectancy0", "life_expectancy80")

#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Oswald", 
                family = "title_font",
                bold.wt = 900)               # Font for titles
font_add_google("PT Sans Narrow", 
                family = "caption_font")     # Font for the caption
font_add_google("Archivo Narrow", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Colour Palette
mypal <- c("#004852", "#016c7a", "#0290a3", "#02abc2", "#02c6e0")
mypal

# Define colours
bg_col <- "#d9f4fa"                     # Background Colour
text_col <- mypal[1]                  # Colour for the text
text_hil <- mypal[2]                  # Colour for highlighted text

# Define Text Size
ts = 18                               # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "adityadahiyaias"
linkedin <- "&#xf08c"
linkedin_username <- "dr-aditya-dahiya-ias"
social_caption <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span> <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span> <span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin};</span> <span style='color: {text_col}'>{linkedin_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "Longevity Divide: Life Expectancy in Rich vs. Poor Nations"

plot_subtitle <- "\nWhile overall life expectancy gaps narrow between poorer and richer countries, an in-depth analysis of life expectancy reveals \nthat most gains poorer countries have made is in saving young lives. While the Left-Side graph shows convergence, the Right-Side Graph \nshows increasing divergence: Richer nations are extending their 'longevity lead' beyond 80 years of age. Some Possible reasons:\n-   Economic Progress & Health-care improvements in poorer nations primarily save young lives.\n-   Elderly medical care is improving faster in richer; but still neglected in poorer countries."

plot_caption <- paste0("**Data:**  Our World in Data - Life Expectancy Report.  ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

life_ages |> 
  filter(entity %in% focus) |>
  filter(year > 1970) |> 
  mutate(entity = fct(entity, levels = focus)) |> 
  select(entity, year, 
         life_expectancy0, 
         life_expectancy80) |> 
  pivot_longer(cols = -c(entity, year),
               names_to = "indicator",
               values_to = "value") |> 
  ggplot(aes(x = year,
             y = value,
             color = entity)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~indicator, 
             scales = "free_y",
             labeller = as_labeller(facet_label)) +
  labs(x = NULL, 
       y = "Life Expectancy (in years)",
       color = NULL,
       title = plot_title,
       caption = plot_caption,
       subtitle = plot_subtitle) +
  scale_color_manual(values = mypal) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.line.y = element_line(linetype = "dotted"),
    axis.title.y   =   element_text(hjust = 1,
                                    family = "body_font",
                                    color = text_col,
                                    size = ts/2),
    axis.text =        element_text(family = "body_font",
                                    face = "bold",
                                    color = text_col,
                                    size = ts/2),
    plot.caption =  element_textbox(family = "caption_font",
                                    hjust = 1,
                                    colour = text_col,
                                    size = ts/2),
    plot.title   =     element_text(hjust = 0,
                                    size = 1.5*ts,
                                    family = "caption_font",
                                    face = "bold",
                                    colour = text_col,
                                    margin = margin(2,0,2,0)),
    plot.subtitle    = element_text(hjust = 0,
                                    size = ts/1.5,
                                    family = "body_font",
                                    colour = text_col),
    plot.background =  element_rect(fill = bg_col,
                                    color = bg_col,
                                    linewidth = 0),
    strip.text    =    element_text(hjust = 0.5,
                                    family = "body_font",
                                    face = "bold",
                                    size = ts/1.5,
                                    colour = text_col),
    legend.text   =    element_text(family = "caption_font",
                                    colour = text_col,
                                    size = ts/2),
    plot.title.position = "plot"
  )

#==============================================================================#
# Animated Data Visualization---------------------------------------------------
#==============================================================================#

df <- life_ages |> 
  filter(entity %in% focus) |>
  filter(year > 1970) |> 
  mutate(entity = fct(entity, levels = focus)) |> 
  select(entity, year, 
         life_expectancy0, 
         life_expectancy80)

g <- df |> 
  nest(.by = entity) |> 
  mutate(
    # Perform loess calculation on each entity group
    m = purrr::map(data, loess,
                   formula = life_expectancy0 ~ year, span = .7),
    # Retrieve the fitted values from each model
    l0 = purrr::map(m, `[[`, "fitted")
  ) |> 
  dplyr::mutate(
    # Perform loess calculation on each entity group
    n = purrr::map(data, loess,
                   formula = life_expectancy80 ~ year, span = .7),
    # Retrieve the fitted values from each model
    l80 = purrr::map(n, `[[`, "fitted")
  ) |> 
  select(-c(m, n)) |> 
  unnest(cols = everything()) |> 
  select(-c(life_expectancy0, life_expectancy80)) |> 
  rename(life_expectancy0 = l0,
         life_expectancy80 = l80) |> 
  pivot_longer(cols = -c(entity, year),
               names_to = "indicator",
               values_to = "value") |> 
  ggplot(aes(x = year,
             y = value,
             color = entity)) +
  geom_line(lwd = 1.5) +
  facet_wrap(~indicator, 
             scales = "free_y",
             labeller = as_labeller(facet_label)) +
  theme(legend.position = "none") +
  transition_reveal(year) +
  labs(x = NULL, 
       y = "Life Expectancy (in years)",
       color = NULL,
       title = plot_title,
       caption = plot_caption,
       subtitle = plot_subtitle) +
  scale_color_manual(values = mypal) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.line.y = element_line(linetype = 2),
    axis.title.y   =   element_text(hjust = 1,
                                    family = "body_font",
                                    color = text_col,
                                    size = ts/2),
    axis.text =        element_text(family = "body_font",
                                    face = "bold",
                                    color = text_col,
                                    size = ts/2),
    plot.caption =  element_textbox(family = "caption_font",
                                    hjust = 0.5,
                                    colour = text_col,
                                    size = ts/1.5),
    plot.title   =     element_text(hjust = 0,
                                    size = 1.5*ts,
                                    family = "title_font",
                                    face = "bold",
                                    colour = text_col,
                                    lineheight = 2,
                                    margin = margin(40,0,20,0)),
    plot.subtitle    = element_text(hjust = 0,
                                    size = ts/1.5,
                                    family = "caption_font",
                                    colour = text_col,
                                    lineheight = 1.5,
                                    margin = margin(10,0,10,0)),
    plot.background =  element_rect(fill = bg_col,
                                    color = bg_col,
                                    linewidth = 0),
    strip.text    =    element_text(hjust = 0.5,
                                    family = "body_font",
                                    face = "bold",
                                    size = ts/1.2,
                                    colour = text_col,
                                    lineheight = 1.5),
    legend.text   =    element_text(family = "caption_font",
                                    colour = text_col,
                                    size = ts/1.8),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )


animate(plot = g,
        width = 800,
        height = 800,
        nframes = 100,
        fps = 10,
        duration = 10,
        start_pause = 0,
        end_pause = 20)


#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

anim_save(here("docs", "life_expectancy_tidy.gif"))