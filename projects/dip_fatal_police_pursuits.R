# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# Data URL: https://github.com/sfchronicle/police_pursuits
# Source: Fatality Analysis Reporting System (FARS)
# https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars

# Deadly police chases. Journalists at the San Francisco Chronicle have assembled 
# a comprehensive record of over 3,300 deaths resulting from police car pursuits
# between 2017 and 2022 on a national scale. This compilation draws from various 
# sources including the federal government's Fatality Analysis Reporting System 
# (DIP 2016.08.31), studies conducted by research institutions, media coverage, 
# legal actions, and public information requests. Each entry in the dataset 
# provides details such as the individual's name, age, gender, race, and their 
# involvement in the pursuit (whether as a driver, passenger, bystander, or 
# officer). Additionally, it includes information about the date, location, 
# stated reason for the pursuit, and the primary law enforcement agency 
# responsible for the incident.

# Credits:  Reporters at the San Francisco Chronicle
#           The San Francisco Chronicle
#           https://www.sfchronicle.com/projects/2024/police-chases/

# =============================================================================#
# Findings ---------------------------------------------------------------------
# =============================================================================#

# Most people killed are White, Black and driver/passengers. 
# Most victims are in the age group 20-30 years
# Accidents

# =============================================================================#
# Library Load-in---------------------------------------------------------------
# =============================================================================#

# Data Wrangling Tools
library(tidyverse)
library(openxlsx)
library(janitor)

# Final plot (ggplot2) tools
library(scales)
library(fontawesome)
library(ggtext)
library(showtext)
library(colorspace)
library(usmap)
library(usmapdata)
library(patchwork)

# =============================================================================#
# Data Load-in, EDA & Data Wrangling--------------------------------------------
# =============================================================================#

police <- read_csv("https://raw.githubusercontent.com/sfchronicle/police_pursuits/master/data/sfc_pursuit_fatalities.csv")

# summarytools::dfSummary(police) |> summarytools::view()

plotdf <- police |> 
  select(-c(unique_id, data_source, race_source)) |> 
  mutate(
    date = mdy(date),
    race = case_when(
      str_detect(race, "black") ~ "Black", # Disclaimer: a little bias introduced by data cleaning as data-rows with any one entry as black will be treated as "Black"
      str_detect(race, "white") ~ "White", # Dislcaimer: Similar data bias
      str_detect(race, "latino") ~ "Latino",
      .default = "Others"
    ),
    race = fct(race, levels = c("Black", "White", "Latino", "Others")),
    person_role = case_when(
      person_role %in% c("driver", "passenger") ~ "Driver/Passenger",
      person_role == "bystander" ~ "Bystander",
      person_role == "officer" ~ "Police Officer",
      .default = "Others"
    ),
    person_role = fct(
      person_role,
      levels = c("Driver/Passenger", "Bystander", "Police Officer", "Others")
    )
  ) |> 
  rename(lon = "long") |>
  filter(lat > 0 & lat < 90) |> 
  filter(lon > -150 & lon < 0) |> 
  usmap_transform() |> 
  as_tibble() |> 
  extract(
    col = geometry, 
    into = c('x', 'y'), 
    regex = "\\((.*), (.*)\\)", 
    convert = TRUE) 

plotdf2 <- bind_rows(
  plotdf |> 
    count(race) |>
    rename(var = race) |> 
    mutate(type = "Race of Person(s) killed"),
  plotdf |> 
    count(person_role) |> 
    rename(var = person_role) |> 
    mutate(type = "Role of Person(s) killed")
)

# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Faster One",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Iceberg",
  family = "body_font"
) 

showtext_auto()

# Define colours
bg_col <- "white" # Background Colour
text_col <- "#012169FF" # Colour for the text
text_hil <- "#C8102EFF" # Colour for highlighted text

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
plot_title <- "Deadly police chases"
plot_caption <- paste0("**Data:** The San Francisco Chronicle", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- "Geographical distribution, race, age and role of persons killed in Police Chases in the United States (2017 - 2022)"
plot_subtitle <- str_wrap(subtitle_text, width = 70)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g1 <- plot_usmap(
  "states",
  col = "darkgrey",
  fill = "white",
  linewidth = 0.4
  ) +
  geom_point(
    data = plotdf,
    mapping = aes(
      x = x,
      y = y,
      colour = person_role,
      size = number_killed),
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c("#FFC72CFF", "#1D4289FF", "#862633FF", "lightgrey")
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 8)
    )
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    colour = "Role of person(s) killed",
    size = "Number of persons killed"
  ) +
  ggthemes::theme_map(
    base_family = "body_font"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      family = "title_font",
      size = 12.5 * ts,
      colour = text_hil, 
      margin = margin(15, 0, 0, 0, unit = "mm")
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "body_font",
      size = 6 * ts,
      colour = text_col,
      lineheight = 0.3,
      margin = margin(0, 0, 0, 0, unit = "mm")
    ),
    plot.caption = element_textbox(
      hjust = 0.5,
      colour = text_hil,
      size = 3 * ts,
      family = "caption_font",
      margin = margin(11, 0, 0, 0, unit = "cm")
    ),
    legend.position.inside = c(0.4, 0.),
    legend.direction = "horizontal",
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(
      hjust = 1,
      family = "caption_font",
      size = 3.5 * ts,
      colour = text_col,
      lineheight = 0.35,
      margin = margin(0, 5, 0, 0, unit = "mm")
    ),
    legend.text = element_text(
      hjust = 1,
      family = "caption_font",
      size = 3.5 * ts,
      colour = text_col,
      lineheight = 0.35,
      margin = margin(0, 5, 0, 0, unit = "mm")
    )
  )

g2 <- plotdf2 |> 
  ggplot(aes(y = reorder(var, n), x = n, fill = type)) +
  geom_col(
    colour = "black",
    fill = "lightgrey"
  ) +
  facet_wrap(~ type, scales = "free") +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_discrete(expand = expansion(0)) +
  theme_minimal(
    base_family = "body_font"
  ) +
  labs(x = NULL, y = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      colour = "lightgrey"
    ),
    legend.position = "none",
    strip.text = element_text(
      hjust = 0,
      family = "body_font",
      size = 3.5 * ts,
      colour = text_col,
      lineheight = 0.35,
      margin = margin(0, 0, 5, 0, unit = "mm")
    ),
    axis.text.x = element_text(
      hjust = 0.5,
      vjust = 0.5,
      family = "body_font",
      size = 3.5 * ts,
      colour = text_col,
      lineheight = 0.35,
      margin = margin(0, 0, 0, 0, unit = "mm")
    ),
    axis.text.y = element_text(
      hjust = 1,
      vjust = 0.5,
      family = "caption_font",
      size = 4 * ts,
      colour = text_col,
      lineheight = 0.35,
      margin = margin(0, 0, 0, 0, unit = "mm")
    ),
    plot.title.position = "plot"
  )

g3 <- ggplot(plotdf, aes(age)) +
  geom_histogram(
    fill = "lightgrey",
    colour = "black",
    binwidth = 5
  ) +
  scale_y_continuous(
    expand = expansion(0),
    breaks = c(100, 300, 500)
    ) +
  scale_x_continuous(
    expand = expansion(0)
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = NULL, 
    y = NULL,
    subtitle = "Age Distribution of Victims"
    ) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      colour = "lightgrey"
    ),
    legend.position = "none",
    axis.text = element_text(
      hjust = 0.5,
      family = "body_font",
      size = 3.5 * ts,
      colour = text_col,
      lineheight = 0.35,
      margin = margin(0, 5, 0, 0, unit = "mm")
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "body_font",
      size = 3.5 * ts,
      colour = text_col,
      lineheight = 0.35,
      margin = margin(2, 0, 0, 0, unit = "mm")
    )
  )

g <- g1 +
  patchwork::inset_element(
    g2,
    left = 0,
    bottom = 0.02,
    right = 0.67,
    top = 0.25,
    align_to = "full",
    clip = FALSE,
    on_top = TRUE
  ) +
  patchwork::inset_element(
    g3,
    left = 0.67,
    bottom = 0.02,
    right = 1,
    top = 0.25,
    align_to = "full",
    clip = FALSE,
    on_top = TRUE
  ) 

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_fatal_police_pursuits.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = bg_col
)
