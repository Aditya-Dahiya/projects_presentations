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


# =============================================================================#
# Analysis 2: Picking up from previous section ---------------------------------
# =============================================================================#

# Load additional libraries
library(osmdata)      # wrapper for Overpass API from Open Street Maps
library(sf)           # library for manipulating Simple features objects
library(egg)
library(grid)

# Get California sf geomtetry map
cal_map <- usmapdata::us_map(
  include = "CA",
  regions = c("counties")
)

# Get California Highway Data: Open Street Maps
cal <- opq("California")

# Get California Motorways
cal_roads <- cal |>
  add_osm_feature(
    key = "highway",
    value = "motorway") |>
  osmdata_sf()

main_roads <- cal_roads$osm_lines |>
  st_transform(crs = st_crs(cal_map)) |> 
  st_intersection(cal_map)

# Get California Trunk Roads
cal_trunk_roads <- cal |>
  add_osm_feature(
    key = "highway",
    value = "trunk") |>
  osmdata_sf()

trunk_roads <- cal_trunk_roads$osm_lines |>
  st_transform(crs = st_crs(cal_map)) |> 
  st_intersection(cal_map)

# Getting Fatal Police Chase Crashes data for California
cal_crashes <- police |>
  filter(state == "CA") |> 
  filter(centroid_geo == 0) |> 
  usmap_transform(
    input_names = c("long", "lat")
  ) |> 
  as_tibble() |> 
  extract(
    col = geometry, 
    into = c('x', 'y'), 
    regex = "\\((.*), (.*)\\)", 
    convert = TRUE) |> 
  mutate(
    race = case_when(
      str_detect(race, "unknown") ~ "Unknown",
      str_detect(race, "black") ~ "Black", # Disclaimer: a little bias introduced by data cleaning as data-rows with any one entry as black will be treated as "Black"
      str_detect(race, "white") ~ "White", # Dislcaimer: Similar data bias
      str_detect(race, "latino") ~ "Latino",
      .default = "Others"
    ),
    race = fct(race, levels = c("Black", "White", "Latino", "Others", "Unknown")),
    person_role = case_when(
      person_role %in% c("driver") ~ "Driver",
      person_role == "bystander" ~ "Bystander",
      person_role == "passenger" ~ "Passenger",
      .default = "Unknown"
    ),
    person_role = fct(
      person_role,
      levels = c("Driver", "Bystander", "Passenger", "Unknown")
    )
  )
# Ready some data for inset plot

sideplotdf <- bind_rows(
  cal_crashes |> 
    count(person_role) |> 
    mutate(type = "Role of Victim", .before = everything()) |> 
    rename(variable = person_role),
  
  cal_crashes |> 
    mutate(initial_reason = case_when(
      initial_reason == "suspected nonviolent" ~ "Suspected\nNon-Violent",
      initial_reason == "suspected violent" ~ "Suspected\nViolent",
      initial_reason == "traffic stop" ~ "Traffic Stop",
      is.na(initial_reason) ~ "Not Known",
      .default = "Others"
    )) |> 
    count(initial_reason) |> 
    mutate(type = "Reason of Police Chase", .before = everything()) |> 
    rename(variable = initial_reason),
  
  cal_crashes |> 
    count(race) |> 
    mutate(type = "Race of Victim", .before = everything()) |> 
    rename(variable = race)
) |> 
  group_by(type) |> 
  mutate(n = n / sum(n)) |> 
  ungroup() |> 
  mutate(per_pop = c(rep(NA, 9), 0.05, 0.35, 0.40, 0.2, 0.0))

# Changing some colours for caption
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")
plot_caption <- paste0("**Data:** The San Francisco Chronicle", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

p1 <- ggplot() +
  geom_sf(
    data = cal_map,
    colour = "white",
    fill = "#e8e8e8"
  ) +
  geom_sf(
    data = trunk_roads,
    linewidth = 0.3,
    alpha = 0.5
  ) +
  geom_sf(
    data = main_roads,
    linewidth = 0.6,
    alpha = 0.6
  ) +
  geom_point(
    data = cal_crashes,
    aes(
      x = x,
      y = y,
      size = number_killed,
      fill = race
    ),
    alpha = 0.3,
    colour = "black",
    pch = 21
  ) +
  scale_x_continuous(
    expand = expansion(c(0.05, 0.5))
  ) +
  scale_fill_manual(
    values = c("black", "orange", "darkgreen", "#949494", "#bfbfbf")
  ) +
  scale_size(
    range = c(5, 8)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 8
      )
    ),
    size = guide_legend(
      override.aes = list(
        colour = "black",
        alpha = 1
      )
    )
  ) +
  labs(
    title = "Fatal Police Chases",
    caption = plot_caption,
    fill = "Race of Victim",
    size = "Fatalities"
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position.inside = c(0, 0.08),
    legend.direction = "vertical",
    legend.box = "vertical",
    plot.title = element_text(
      colour = text_hil,
      size = 12 * ts,
      family = "title_font",
      hjust = 0.5,
      margin = margin(1, 0, 0, 0, unit = "cm")
    ),
    plot.subtitle = element_text(
      colour = text_col,
      size = 4 * ts,
      family = "body_font",
      hjust = 0.5,
      lineheight = 0.35
    ),
    plot.caption = element_textbox(
      colour = text_col,
      hjust = 0.5,
      family = "caption_font",
      size = 3 * ts
    ),
    legend.title = element_text(
      colour = text_col,
      hjust = 0,
      size = 4 * ts,
      family = "body_font",
      margin = margin(2,0,2,2, unit = "mm")
    ),
    legend.text = element_text(
      colour = text_col,
      hjust = 0,
      size = 3 * ts,
      family = "body_font",
      margin = margin(2,0,2,2, unit = "mm")
    ),
    legend.background = element_rect(
      fill = "transparent"
    )
  )

p2 <- sideplotdf |> 
  ggplot(aes(x = n, y = reorder(variable, n))) +
  geom_col(
    colour = "#525252",
    fill = "grey",
    alpha = 0.4
  ) +
  ungeviz::geom_vpline(
    aes(
      x = per_pop
    ),
    height = 0.8,
    size = 2,
    col = "black"
  ) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  scale_x_reverse(
    label = label_percent(),
    breaks = c(0, 0.2, 0.4),
    expand = expansion(0)
  ) +
  scale_y_discrete(
    position = "right"
  ) +
  labs(
    x = NULL, 
    y = NULL,
    subtitle = str_wrap("Most deaths in police car chases in California occurred on Highways in and around major cities. The race, role and reasons for deaths are:",  40),
    title = NULL
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(
      colour = text_col,
      size = 5.5 * ts,
      family = "body_font",
      hjust = 1,
      lineheight = 0.3,
      margin = margin(10, 0, 0, 0, unit = "mm")
    ),
    strip.text = element_text(
      colour = text_col,
      hjust = 0.5,
      size = 5 * ts,
      family = "body_font",
      margin = margin(3,0,0,0, unit = "mm")
    ),
    axis.text.y = element_text(
      colour = text_col,
      hjust = 0,
      size = 3 * ts,
      family = "caption_font",
      margin = margin(3,0,0,0, unit = "mm"),
      lineheight = 0.19
    ),
    axis.text.x = element_text(
      colour = text_col,
      hjust = 0.5,
      size = 4 * ts,
      family = "body_font",
      margin = margin(3,0,0,0, unit = "mm"),
      lineheight = 0.18
    ),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      colour = text_col,
      linewidth = 0.3
    ),
    plot.title.position = "plot"
  )

# A Third Annotation plot
p3 <- tibble(
  label = "This Black bar shows the % Population\nof each race in California (2022)"
) |> 
  ggplot(aes(x = 1, y = 1, label = label)) +
  geom_text(
    colour = text_col,
    hjust = 0.5,
    family = "caption_font",
    size = ts,
    lineheight = 0.25
  ) +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = "transparent"
  ))


p <- p1 +
  inset_element(
    p2, 
    left = 0.55,
    top = 1,
    bottom = 0.25,
    right = 1,
    align_to = "panel"
  ) +
  inset_element(
    p3, 
    left = 0.45,
    top = 0.75,
    bottom = 0.72,
    right = 0.705,
    align_to = "panel"
  )

ggsave(
  filename = here::here("docs", "dip_fpp_cal.png"),
  plot = p,
  width = 38,
  height = 47,
  units = "cm",
  bg = "white"
)


# =============================================================================#
# Analysis 3: Picking up from previous section ---------------------------------
# =============================================================================#

tb1 <- police |> 
  select(-c(unique_id, year, news_urls)) |> 
  mutate(
    date = mdy(date),
    month = month(date, label = TRUE),
    year = year(date)
  ) |> 
  mutate(
    race = case_when(
      str_detect(race, "unknown") ~ "Unknown",
      str_detect(race, "black") ~ "Black", # Disclaimer: a little bias introduced by data cleaning as data-rows with any one entry as black will be treated as "Black"
      str_detect(race, "white") ~ "White", # Dislcaimer: Similar data bias
      str_detect(race, "latino") ~ "Latino",
      .default = "Others"
    ),
    race = fct(race, levels = c("Black", "White", "Latino", "Others", "Unknown")),
    person_role = case_when(
      person_role %in% c("driver") ~ "Driver",
      person_role == "bystander" ~ "Bystander",
      person_role == "passenger" ~ "Passenger",
      person_role == "officer" ~ "Officer",
      .default = "Unknown"
    ),
    person_role = fct(
      person_role,
      levels = c("Driver", "Bystander", "Passenger", "Officer", "Unknown")
    )
  )

# Doing some exploratory data analysis to get trends
tb1 |> 
  group_by(month) |> 
  count(race, wt = number_killed) |> 
  ggplot(aes(x = month, y = n, fill = race)) +
  geom_col()

tb1 |> 
  group_by(race_source) |> 
  count(race) |> 
  ggplot(aes(x = race_source, y = n, fill = race)) +
  geom_col()

tb1 |> 
  group_by(state) |> 
  count(race) |> 
  mutate(total = sum(n)) |> 
  filter(total >= 100) |> 
  ggplot(aes(x = state, y = n, fill = race)) +
  geom_col(position = "fill")

tb1 |> 
  group_by(state) |> 
  count(person_role, wt = number_killed) |> 
  mutate(total = sum(n)) |> 
  filter(total > 50) |> 
  filter(person_role != "Unknown") |> 
  ggplot(aes(x = state, y = n, fill = person_role)) +
  geom_col(position = "fill")

tb1 |> 
  group_by(year) |> 
  count(gender, wt = number_killed) |> 
  mutate(total = sum(n)) |> 
  filter(total > 50) |> 
  ggplot(aes(x = year, y = n, fill = gender)) +
  geom_col(position = "fill")


# Load additional libraries
library(osmdata)      # wrapper for Overpass API from Open Street Maps
library(sf)           # library for manipulating Simple features objects

# Get California sf geomtetry map
tx_map <- usmapdata::us_map(
  include = "TX",
  regions = c("counties")
)

# Get California Highway Data: Open Street Maps
cal <- opq("Texas")

# Get California Motorways
tx_roads <- cal |>
  add_osm_feature(
    key = "highway",
    value = "motorway") |>
  osmdata_sf()

main_roads <- tx_roads$osm_lines |>
  st_transform(crs = st_crs(tx_map)) |> 
  st_intersection(tx_map)

# Get California Trunk Roads
tx_trunk_roads <- cal |>
  add_osm_feature(
    key = "highway",
    value = "trunk") |>
  osmdata_sf()

trunk_roads <- tx_trunk_roads$osm_lines |>
  st_transform(crs = st_crs(tx_map)) |> 
  st_intersection(tx_map)

# Getting Fatal Police Chase Crashes data for California
tx_crashes <- police |>
  filter(state == "TX") |> 
  filter(centroid_geo == 0) |> 
  usmap_transform(
    input_names = c("long", "lat")
  ) |> 
  as_tibble() |> 
  extract(
    col = geometry, 
    into = c('x', 'y'), 
    regex = "\\((.*), (.*)\\)", 
    convert = TRUE) |> 
  mutate(
    race = case_when(
      str_detect(race, "unknown") ~ "Unknown",
      str_detect(race, "black") ~ "Black", # Disclaimer: a little bias introduced by data cleaning as data-rows with any one entry as black will be treated as "Black"
      str_detect(race, "white") ~ "White", # Dislcaimer: Similar data bias
      str_detect(race, "latino") ~ "Latino",
      .default = "Others"
    ),
    race = fct(race, levels = c("Black", "White", "Latino", "Others", "Unknown")),
    person_role = case_when(
      person_role %in% c("driver") ~ "Driver",
      person_role == "bystander" ~ "Bystander",
      person_role == "passenger" ~ "Passenger",
      .default = "Unknown"
    ),
    person_role = fct(
      person_role,
      levels = c("Driver", "Bystander", "Passenger", "Unknown")
    )
  )
# Ready some data for inset plot

sideplotdf <- bind_rows(
  tx_crashes |> 
    count(person_role) |> 
    mutate(type = "Role of Victim", .before = everything()) |> 
    rename(variable = person_role),
  
  tx_crashes |> 
    mutate(initial_reason = case_when(
      initial_reason == "suspected nonviolent" ~ "Suspected\nNon-Violent",
      initial_reason == "suspected violent" ~ "Suspected\nViolent",
      initial_reason == "traffic stop" ~ "Traffic Stop",
      is.na(initial_reason) ~ "Not Known",
      .default = "Not Known"
    )) |> 
    count(initial_reason) |> 
    mutate(type = "Reason of Police Chase", .before = everything()) |> 
    rename(variable = initial_reason),
  
  tx_crashes |> 
    count(race) |> 
    mutate(type = "Race of Victim", .before = everything()) |> 
    rename(variable = race)
) |> 
  group_by(type) |> 
  mutate(n = n / sum(n)) |> 
  ungroup() |> 
  mutate(per_pop = c(rep(NA, 9), 0.12, 0.40, 0.40, 0.08, 0.0))

# Changing some colours for caption
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")
plot_caption <- paste0("**Data:** The San Francisco Chronicle", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

# Changing some colours
bg_col = "black"
text_col = "#ffe6e3"
text_hil <- "#ff6a59"

p1 <- ggplot() +
  geom_sf(
    data = tx_map,
    colour = "#636363",
    fill = "#404040"
  ) +
  geom_sf(
    data = trunk_roads,
    linewidth = 0.3,
    alpha = 0.5,
    colour = "white"
  ) +
  geom_sf(
    data = main_roads,
    linewidth = 0.6,
    alpha = 0.6,
    colour = "white"
  ) +
  geom_point(
    data = tx_crashes,
    aes(
      x = x,
      y = y,
      size = number_killed,
      fill = race
    ),
    alpha = 0.3,
    colour = "white",
    pch = 21
  ) +
  scale_x_continuous(
    expand = expansion(c(0.05, 0.55))
  ) +
  scale_y_continuous(
    expand = expansion(c(0, 0.05))
  ) +
  scale_fill_manual(
    values = c("#F5433FFF","#F8DE02FF", "#579CE9FF", "#CAC7BBFF", "#CAC7BBFF")
  ) +
  scale_size(
    range = c(5, 8)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 8,
        alpha = 1
      )
    ),
    size = guide_legend(
      override.aes = list(
        colour = "white",
        alpha = 1
      )
    )
  ) +
  labs(
    title = "Police Chase Deaths",
    caption = plot_caption,
    fill = "Race of Victim",
    size = "Fatalities"
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position.inside = c(0, 0),
    legend.direction = "vertical",
    legend.box = "horizontal",
    plot.title = element_text(
      colour = text_hil,
      size = 15 * ts,
      family = "title_font",
      hjust = 0.5,
      margin = margin(1, 0, 0, 0, unit = "cm")
    ),
    plot.subtitle = element_text(
      colour = text_col,
      size = 4 * ts,
      family = "body_font",
      hjust = 0.5,
      lineheight = 0.35
    ),
    plot.caption = element_textbox(
      colour = text_col,
      hjust = 0.5,
      family = "caption_font",
      size = 3 * ts
    ),
    legend.title = element_text(
      colour = text_col,
      hjust = 0,
      size = 4 * ts,
      family = "body_font",
      margin = margin(0,0,0,2, unit = "mm")
    ),
    legend.text = element_text(
      colour = text_col,
      hjust = 0,
      size = 3 * ts,
      family = "body_font",
      margin = margin(0,0,0,2, unit = "mm")
    ),
    legend.background = element_rect(
      fill = "transparent"
    ),
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    )
  )

p2 <- sideplotdf |> 
  ggplot(aes(
    x = n, 
    y = reorder(variable, n))) +
  geom_col(
    colour = "white",
    fill = "grey",
    alpha = 0.5
  ) +
  ungeviz::geom_vpline(
    aes(
      x = per_pop
    ),
    height = 0.8,
    size = 2,
    col = "white"
  ) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  scale_x_reverse(
    label = label_percent(),
    breaks = c(0, 0.25, 0.5),
    expand = expansion(c(0.5, 0))
  ) +
  scale_y_discrete(
    position = "right"
  ) +
  labs(
    x = NULL, 
    y = NULL,
    subtitle = str_wrap("Most deaths in police car chases in Texas occurred on Highways and near major cities. Fatalities involving blacks were mostly concentrated in urban areas.",  55),
    title = NULL
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(
      colour = text_col,
      size = 4 * ts,
      family = "body_font",
      hjust = 1,
      lineheight = 0.3,
      margin = margin(10, 25, 0, 0, unit = "mm")
    ),
    strip.text = element_text(
      colour = text_col,
      hjust = 1,
      size = 4.5 * ts,
      family = "body_font",
      margin = margin(3,0,0,0, unit = "mm")
    ),
    axis.text.y = element_text(
      colour = text_col,
      hjust = 0,
      size = 3 * ts,
      family = "caption_font",
      margin = margin(3,0,0,0, unit = "mm"),
      lineheight = 0.19
    ),
    axis.text.x = element_text(
      colour = text_col,
      hjust = 0.5,
      size = 4 * ts,
      family = "body_font",
      margin = margin(3,0,0,0, unit = "mm"),
      lineheight = 0.18
    ),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      colour = text_col,
      linewidth = 0.3
    ),
    plot.title.position = "plot"
  )

# A Third Annotation plot
p3 <- tibble(
  label = "This White bar shows the % Population\nof each race in Texas (2022)"
) |> 
  ggplot(aes(x = 1, y = 1, label = label)) +
  geom_text(
    colour = "white",
    hjust = 0.5,
    family = "caption_font",
    size = ts,
    lineheight = 0.25
  ) +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = "transparent"
  ))

# A fourth plot to Label Texas
p4 <- tibble(
  label = "Texas"
) |> 
  ggplot(aes(x = 1, y = 1, label = label)) +
  geom_text(
    colour = text_hil,
    hjust = 0.5,
    family = "title_font",
    size = 3.5 * ts,
    lineheight = 0.25
  ) +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = "transparent"
  ))

p <- p1 +
  inset_element(
    p2, 
    left = 0.55,
    top = 1,
    bottom = 0.0,
    right = 1,
    align_to = "panel"
  ) +
  inset_element(
    p3, 
    left = 0.66,
    right = 0.89,
    top = 0.63,
    bottom = 0.59,
    align_to = "panel"
  ) +
  inset_element(
    p4, 
    left = 0,
    right = 0.2,
    top = 1,
    bottom = 0.8,
    align_to = "panel"
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = "transparent",
        colour = "black"
      )
    )
  )

ggsave(
  filename = here::here("docs", "dip_fpp_tx.png"),
  plot = p,
  width = 45,
  height = 35,
  units = "cm",
  bg = "black"
)

