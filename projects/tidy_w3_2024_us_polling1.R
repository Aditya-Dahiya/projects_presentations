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
library(openxlsx)         # To Load xlsx data 

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 

tuesdata <- tidytuesdayR::tt_load(2024, week = 3)

polling_places <- tuesdata$polling_places
rm(tuesdata)

# Number of polling stations per county
polldata <- polling_places |> 
  filter(election_date == as_date("2020-11-03")) |> 
  count(state, 
        county_name,
        name = "nos_poll_sites")

# Population of each county (2020 data)
popdata <- read.xlsx("https://www2.census.gov/programs-surveys/popest/tables/2020-2022/counties/totals/co-est2022-pop.xlsx") |> 
  as_tibble()
  
popdata <- popdata |> 
  select(1:2)
  
names(popdata) <- c("county", "population")  
popdata <- popdata[-(1:4),]

popdata <- popdata |> 
  separate_wider_delim(
    cols = county,
    delim = ", ",
    names = c("county", "state"),
    too_few = "align_start",
    too_many = "merge"
  ) |> 
  mutate(
    county = str_remove_all(county, "\\."),
    county = str_remove_all(county, " County"),
    county = str_to_upper(county)
  ) |> 
  mutate(
    population = as.numeric(population)
  )

statenames <- tibble(
   state_abb = state.abb,
   state = state.name
)

popdata <- popdata |> 
  left_join(statenames) |> 
  filter(!is.na(state)) |> 
  filter(!is.na(population))

# the polling stations data has no county level data on Alaska,
# So, we need to merge counties of alaska into one
popdata <- bind_rows(
  popdata |> 
    filter(state_abb == "AK") |> 
    summarize(
      county = NA,
      state = "Alaska",
      population = sum(population),
      state_abb = "AK"
    ),
  popdata |> 
    filter(state_abb != "AK")
)

# Prep column names for final merger
polldata <- polldata |> 
  rename(
    county = county_name,
    state_abb = state
  )

# Merge the two datasets
plotdf <- inner_join(
  popdata,
  polldata
) 

# Fuzzy match also produced only 1226 matches
df1 <- fuzzyjoin::stringdist_inner_join(
  popdata |> drop_na(), 
  polldata |> drop_na(), 
  by = join_by(county == county,
               state_abb == state_abb)) |> 
  filter(state_abb.x == state_abb.y)

df2 <- df1 |> 
  rename(
    county = county.x,
    state_abb = state_abb.x,
    pollsites = nos_poll_sites
  ) |> 
  select(-c(county.y, state_abb.y)) |> 
  mutate(
    poll_per_pop = 1000 * pollsites / population
  )

library(usmap)
library(usmapdata)

df3 <- usmapdata::us_map(regions = "counties") |> 
  as_tibble() |> 
  select(
    county,
    state_abb = abbr,
    fips
  ) |> 
  mutate(
    county = str_remove_all(county, " County"),
    county = str_to_upper(county)
  ) |> 
  distinct(county, state_abb, fips) |> 
  left_join(df2) |> 
  drop_na() |> 
  select(fips, poll_per_pop) |> 
  rename(values = poll_per_pop)

df3

########################## Try state-wise data

sdf <- polling_places |> 
  filter(election_date ==  as_date("2020-11-03")) |> 
  count(state)

df5 <- usmap::statepop |> 
  left_join(sdf,
            by = join_by(abbr == state)) |> 
  mutate(values = 1000000 * n / pop_2015)
#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Sirin Stencil", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Strait", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
school <- "#5773CCFF" 
church <- "#FFB900FF" 
bg_col <- "#F0F2F8FF"                   # Background Colour
text_col <- "#010440"                 # Colour for the text
text_hil <- "#010440"                 # Colour for highlighted text

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
plot_title <- "New Polling Sites in US Elections !"

subtitle_text <- "In the last 5 elections, the United States has introduced over 94,000 new polling sites, most of them in the 2020 elections. Here, we look at these new sites located in Churches or Schools. Most of these new polling sites are concentrated in a few States. Further, more Church-based new polling sites occur in the South-East, while more School-based new polling sites lie in the North-East."
plot_subtitle <- paste(strwrap(subtitle_text, 130), collapse = "\n")

plot_caption <- paste0("**Data & Inspiration:** Center for Public Integrity | ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#
cols <- paletteer::paletteer_d("calecopal::desert")[2:5]

plot_usmap(
  data = df5,
  regions = "state",
  col = "white"
  ) +
  scale_fill_binned(
    trans = "log2",
    values = cols,
    na.value = "#f2f2f2",
    breaks = c(0, 100, 500, 1000),
    type = "viridis"
  ) +
  theme(legend.position = "bottom",
        legend.key.width = unit(3, "cm"))


polling_places |> 
  unique() |> 
  length()
  
  scale_color_manual(values = c(church, school)) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    color = NULL,
    shape = NULL
  ) +
  theme(
  legend.position = c(0.7, 0.07),
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = 1.5 * ts),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 6*ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_hil,
                                  margin = margin(4,0,2,0)),
  plot.subtitle    = element_text(hjust = 0.5,
                                  size = 1.6 * ts,
                                  family = "body_font",
                                  colour = text_col,
                                  margin = margin(5,0,2,0),
                                  lineheight = 0.35),
  plot.background =  element_rect(fill = bg_col,
                                  color = bg_col,
                                  linewidth = 0),
  plot.title.position = "plot",
  legend.text = element_text(size = 2 * ts,
                             family = "body_font",
                             colour = text_col,
                             margin = margin(0,0,0,0),
                             hjust = 0),
  legend.key = element_rect(fill = bg_col,
                            colour = bg_col),
  legend.background = element_rect(fill = bg_col)
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "tidy_uspolling.png"),
  plot = g,
  width = 20, 
  height = 20, 
  units = "cm",
  bg = bg_col
)
