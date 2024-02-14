#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(ggstream)       # To use geom_stream()
library(colorspace)     # Lighten and darken Colours
library(summarytools)   # To view Summary


#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#
# uselections <- read_csv(here("data", "ledb_candidatelevel.csv"))

url <- "https://osf.io/download/tbwzd/"
uselections <- read_csv(url)

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

uselections

dfSummary(uselections) |> 
  view()
#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

racedf <- uselections |> 
  mutate(party = case_when(
    prob_democrat > 0.5 ~ "Democrat",
    prob_republican > 0.5 ~ "Republican",
    .default = "Others"
  )) |> 
  mutate(
    race_est = case_when(
      is.na(race_est) ~ "other",
      .default = race_est
    )
  ) |> 
  group_by(year, party, race_est) |> 
  count() |> 
  mutate(race_est = str_to_title(race_est)) |> 
  ungroup() |> 
  group_by(year) |> 
  mutate(prop = n / sum(n)) |> 
  mutate(party = fct(
    party, 
    levels = c("Republican", "Democrat", "Others")
  ),
  race_est = fct(
    race_est,
    levels = c("Caucasian", "Black", "Hispanic", 
               "Asian", "Other")
  )
  )


#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Fira Sans Extra Condensed", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Anaheim", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph

# Define colours
bg_col <- "white"                   # Background Colour
text_col <- "#655A5AFF"             # Colour for the text
text_hil <- "#655A5AFF"             # Colour for highlighted text

# Colours for stream graph
fill_cols <- paletteer::paletteer_d("fishualize::Anchoviella_lepidentostole",
                                    direction = -1) |> 
  colorspace::lighten(0.2)

col_cols <- paletteer::paletteer_d("fishualize::Anchoviella_lepidentostole",
                                   direction = -1) |> 
  colorspace::darken(0.4)


# Define Text Size
ts = unit(20, units = "cm")                             # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "Ethnicity of candidates in USA's local elections."
subtitle_text <- "Percentage of candidates from different ethnicities fielded by the two dominant political parties in USA over last 20 years in local elections for posts of Prosecutor, Sheriff, County Executive, Mayor, School Board, County Legislature and City Council. As we can see, the share of non-Caucasian races has increased, but at different rates in different parties."
plot_subtitle <- paste(strwrap(subtitle_text, 100), collapse = "\n")

plot_caption <- paste0(
  "**Data**: Justin de Benedictis-Kessner et al. **|** ", 
  "**Graphics**:",
  social_caption_1, 
  " **|** ",
  "**Code:**", 
  social_caption_2
)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

# UNESCO Sites Bar Graph
g <- racedf |> 
  ggplot(mapping = aes(
    x = year,
    y = prop,
    fill = race_est,
    label = race_est,
    color = race_est
  )) +
  geom_stream(
    type = "proportional",
    alpha = 0.3) +
  geom_stream_label(
    type = "proportional",
    hjust = "inward",
    size = 0.7 * ts,
    family = "body_font"
  ) +
  facet_wrap(~ party, nrow = 1) +
  scale_x_continuous(
    expand = expansion(0),
    breaks = seq(1990, 2020, 5)
  ) +
  scale_y_continuous(
    expand = expansion(0),
    labels = scales::label_percent()
  ) +
  scale_color_manual(values = col_cols) +
  scale_fill_manual(values = fill_cols) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    x = NULL,
    y = "Percentage of candidates fielded by the Party"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(
      linetype = 2
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption =  element_textbox(family = "caption_font",
                                    hjust = 0.5,
                                    colour = text_col,
                                    size = 1.5 * ts,
                                    margin = margin(1,0,0,0,
                                                    unit = "cm")),
    plot.title   =     element_text(hjust = 0.5,
                                    size = 6 * ts,
                                    family = "title_font",
                                    face = "bold",
                                    colour = text_hil,
                                    margin = margin(2,0,0.5,0,
                                                    unit = "cm")),
    plot.subtitle    = element_text(hjust = 0.5,
                                    size = 3 * ts,
                                    family = "body_font",
                                    colour = text_col,
                                    margin = margin(5,0,2,0),
                                    lineheight = 0.35),
    plot.background =  element_rect(fill = bg_col,
                                    color = bg_col,
                                    linewidth = 0),
    axis.text.y = element_text(size = 2.5 * ts,
                             family = "title_font",
                             colour = text_col,
                             face = "bold",
                             margin = margin(0,0,0,0)),
    axis.text.x = element_text(size = 1.5 * ts,
                               family = "body_font",
                               colour = text_col,
                               face = "bold",
                               margin = margin(0,0,0,0)),
    axis.title = element_text(size = 3 * ts,
                              family = "body_font",
                              colour = text_col,
                              face = "bold",
                              margin = margin(0,0,0,0)),
    strip.text = element_text(hjust = 0.5,
                              family = "title_font",
                              face = "bold",
                              colour = text_col,
                              size = 5 * ts,
                              margin = margin(1,0,0,0,
                                              unit = "cm")),
    plot.title.position = "plot"
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "dip_uselections.png"),
  plot = g,
  width = 30, 
  height = 30, 
  units = "cm",
  bg = bg_col
)
