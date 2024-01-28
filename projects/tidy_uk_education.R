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

tuesdata <- tidytuesdayR::tt_load('2024-01-23')

english_education <- tuesdata$english_education

object.size(english_education)

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

library(summarytools)
dfSummary(english_education) |> view()

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

city_levels <- c("Small Towns", "Medium Towns", "Large Towns", "Cities", "London")

inc_levels = c("Poorer Towns", "Mid-Income Towns", "Richer Towns", "Cities")

df <- english_education |> 
  # Add populations of Inner and Outer London
  mutate(
    population_2011 = case_when(
      town11nm == "Outer london BUAs" ~ 4942040,
      town11nm == "Inner London BUAs" ~ 3231901,
      .default = population_2011
    )
  ) |> 
  mutate(
    size_flag = case_when(
      size_flag == "City" ~ "Cities",
      size_flag %in% c("Inner London BUA", "Outer london BUA") ~ "London",
      .default = size_flag
    )
  ) |> 
  filter(
    size_flag %in% city_levels
  ) |> 
  mutate(
    size_flag = fct(size_flag, levels = city_levels),
    size_flag = fct_rev(size_flag)
  ) |> 
  mutate(
    income_flag = case_when(
      income_flag %in% c("Cities", NA) ~ "Cities",
      income_flag == "Higher deprivation towns" ~ "Poorer Towns",
      income_flag == "Mid deprivation towns" ~ "Mid-Income Towns",
      income_flag == "Lower deprivation towns" ~ "Richer Towns"
    ),
    income_flag = fct(income_flag,
                      levels = inc_levels)
  )

# Weighted mean educational attainment scores
df1 <- df |> 
  group_by(size_flag) |> 
  summarise(
    mean_ed_score = mean(education_score, na.rm = T),
    med_ed_score = median(education_score, na.rm = T),
    w_mean = weighted.mean(x = education_score,
                           w = population_2011,
                           na.rm = T)
  )

# Overall weighted mean Educational Attainment Score for UK
uk_mean <- df |> 
  summarize(
    uk_mean = weighted.mean(education_score, w = population_2011)
  ) |> 
  as_vector() |> 
  unname()

# Adding names of cities for labels
label_cities <- c("Thurnscoe", "Great Yarmouth", 
                  "Basildon", "Nottingham", 
                  "Brighton and Hove", "Woking",
                  "Sutton Coldfield", "Harpenden",
                  "Northwood", "Olney",
                  "Outer London", "Inner London")
df_match <- tibble(town_name = label_cities)

df2 <- df |> 
  mutate(
    town_name = str_remove_all(town11nm, " BUA"),
    town_name = str_remove_all(town_name, "SD"),
    town_name = case_when(
      town_name == "Inner Londons" ~ "Inner London",
      town_name == "Outer londons" ~ "Outer London",
      .default = town_name
    )
  ) |>
  filter(town_name %in% label_cities)
  
#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Amarante", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Pompiere", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
bg_col <- "white"                   # Background Colour
text_col <- "#130059"                 # Colour for the text
text_hil <- "#41008c"                 # Colour for highlighted text

# Define Text Size
ts = unit(20, units = "cm")                             # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span> <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "Education levels in the U.K."

subtitle_text <- "Kids in Smaller towns have higher educational attainment levels than those in larger towns, on average. London stands as a sole exception to the large vs. small town trend. Further, as expected, kids in richer towns far outperform those from poorer towns."
plot_subtitle <- paste(strwrap(subtitle_text, 90), collapse = "\n")

plot_caption <- paste0("**Data & Inspiration:** Center for Public Integrity | ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#
# For Beeswarm
library(ggbeeswarm)

# For vertical markers
# devtools::install_github("wilkelab/ungeviz")
library(ungeviz)

g <- ggplot(data = df) +
  ggbeeswarm::geom_beeswarm(
    aes(
      x = education_score,
      y = size_flag,
      color = income_flag,
      size = population_2011
    ),
    priority = "random",
    alpha = 0.6
  ) +
  geom_vpline(
    data = df1,
    mapping = aes(
      x = w_mean,
      y = size_flag 
    ),
    height = 0.4,
    size = 4
  ) +
  geom_vline(
    xintercept = uk_mean,
    linetype = 2
  ) +
  geom_text(
    data = df2,
    mapping = aes(
      x = education_score,
      y = size_flag,
      label = town_name,
      hjust = case_when(
        town_name == "Outer London" ~ 0,
        town_name == "Inner London" ~ 1,
        .default = 0.5
      )
    ),
    vjust = 1.7,
    size = 0.5 * ts,
    color = text_col,
    family = "caption_font"
  ) +
  annotate(
    geom = "curve",
    x = as.numeric(df1[2,4]),
    xend = -6,
    y = 2,
    yend = 1.6,
    arrow = arrow(length = unit(3, "mm"))
  ) +
  annotate(
    geom = "text",
    x = -6,
    y = 1.55,
    label = "Bar represents weighted mean for the class of towns",
    hjust = 0.5,
    family = "body_font",
    size = 0.6 * ts,
    color = text_col
  ) +
  annotate(
    geom = "curve",
    x = uk_mean,
    xend = -2,
    y = 1.5,
    yend = 1.2,
    arrow = arrow(length = unit(3, "mm")),
    curvature = -0.5
  ) +
  annotate(
    geom = "text",
    x = -2,
    y = 1.25,
    label = "Average educational level (U.K.)",
    hjust = 1,
    family = "body_font",
    size = 0.8 * ts,
    color = text_col
  ) +
  annotate(
    geom = "segment",
    x = uk_mean + 0.1,
    xend = 9,
    y = 0.6,
    yend = 0.6,
    arrow = arrow(length = unit(3, "mm")),
    color = text_col
  ) +
  annotate(
    geom = "text",
    x = 9,
    y = 0.4,
    label = "Higher Educational Attainment",
    hjust = 1,
    family = "body_font",
    size = 0.8 * ts,
    color = text_col
  ) +
  annotate(
    geom = "segment",
    x = uk_mean - 0.1,
    xend = -9,
    y = 0.6,
    yend = 0.6,
    arrow = arrow(length = unit(3, "mm")),
    color = text_col
  ) +
  annotate(
    geom = "text",
    x = -9,
    y = 0.4,
    label = "Lower Educational Attainment",
    hjust = 0,
    family = "body_font",
    size = 0.8 * ts,
    color = text_col
  ) +
  annotate(
    geom = "text",
    x = 6,
    y = 1.78,
    label = "Town Population (2011)",
    hjust = 0,
    family = "body_font",
    size = 0.8 * ts,
    color = text_col
  ) +
  scale_x_continuous(
    breaks = c(-8, -4, 0, 4, 8),
    labels = NULL,
    expand = expansion(0.05)
  ) +
  scale_y_discrete(
    expand = expansion(0.2, 0)
  ) +
  scale_color_manual(
    values = c("#f70c00", "#ffb700", "#39b300", "#00b2e8")
  ) +
  scale_size(
    range = c(1, 10),
    labels = scales::label_number_si(),
    breaks = c(100000, 1e6, 4e6)
  ) +
  guides(
    size = guide_legend(
      override.aes = list(
        color = "lightgrey"
      )
    ),
    color = guide_legend(
      override.aes = list(
        size = 4
      )
    )
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    size = NULL,
    color = NULL
  ) +
  theme(
  legend.position = c(0.8, 0.3),
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = 1.5 * ts),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 6*ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_hil,
                                  margin = margin(1,0,0.5,0,
                                                  unit = "cm")),
  plot.subtitle    = element_text(hjust = 0.5,
                                  size = 2.5 * ts,
                                  family = "body_font",
                                  colour = text_col,
                                  margin = margin(0,0,0,0,
                                                  unit = "cm"),
                                  lineheight = 0.35),
  plot.background =  element_rect(fill = bg_col,
                                  color = "lightgrey",
                                  linewidth = 1),
  plot.title.position = "plot",
  legend.text = element_text(size = 2 * ts,
                             family = "body_font",
                             colour = text_col,
                             margin = margin(0,0,0,0),
                             hjust = 0),
  axis.text.y = element_text(family = "body_font",
                             size = 2.5 * ts,
                             face = "bold",
                             colour = text_col,
                             hjust = 1),
  panel.grid = element_blank(),
  panel.grid.major.x = element_line(color = "lightgrey",
                                    linewidth = 0.2,
                                    linetype = 2)
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "tidy_uk_education.png"),
  plot = g,
  width = 20, 
  height = 25, 
  units = "cm",
  bg = bg_col
)
