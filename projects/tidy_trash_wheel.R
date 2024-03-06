# TidyTuesday: Trash Wheel Collection Data

# The dataset for this week focuses on Trash Wheel Collection Data sourced from 
# the Mr. Trash Wheel initiative in Baltimore's Healthy Harbor project. Mr. 
# Trash Wheel, a semi-autonomous trash interceptor situated at river mouths or 
# outfalls, utilizes solar and hydro power to collect large quantities of debris, 
# amounting to over 2,362 tons to date across four Trash Wheels. The data 
# collection methodology involves crew members manually tallying item types on 
# conveyor paddles during dumpster filling, with averages computed and 
# extrapolated to estimate total trash quantities. In instances where crew 
# presence is lacking, random bushel size samples are taken, and periodic 
# "dumpster dives" involve volunteers in comprehensive trash counting exercises. 
# The dataset aims to discern trends such as predominant trash types, potential 
# variations in collection among Trash Wheels, and fluctuations in trash 
# accumulation across different times of the year.

# ==============================================================================#
# Library Load-in---------------------------------------------------------------
# ==============================================================================#
library(tidyverse) # Data Wrangling and Plotting
library(here) # Files location and loading
library(showtext) # Using Fonts More Easily in R Graphs
library(fontawesome) # Social Media icons
library(ggtext) # Markdown Text in ggplot2
library(scales) # Labeling the axes
library(colorspace) # Lighten and Darken Colours
library(patchwork) # For compiling plots
library(paletteer) # colour palettes

# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#

# Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load(2024, week = 10)

trashwheel <- tuesdata$trashwheel
rm(tuesdata)

# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#

# library(summarytools)
# dfSummary(trashwheel) |> view()

trashwheel

visdat::vis_dat(trashwheel)

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#

df <- trashwheel |> 
  mutate(trashdate = mdy(Date)) |> 
  select(-c(ID, Name, Dumpster, Month, Year, Date)) |> 
  group_by(trashdate) |>
  summarise_all(
    .funs = sum
  )


plotdf <- df |> 
  select(-c(Weight, Volume)) |> 
  pivot_longer(
    cols = -trashdate,
    names_to = "var",
    values_to = "value"
  ) |> 
  mutate(
    var = gsub("([A-Z]){1}", " \\1", var),
    var = str_trim(var, "left"),
    year = as.character(year(trashdate)),
    month = month(trashdate, label = TRUE)
  ) |> 
  drop_na()

plotdf1 <- plotdf |> 
  group_by(var) |> 
  mutate(std_value = (value - median(value, na.rm = T)) / sd(value, na.rm = T)) |> 
  drop_na()

# Code Not used
facet_names <- plotdf1 |> 
  distinct(var) |> 
  pull(var)
  
plot_tile <- function(i){
  plotdf |> 
    filter(var == facet_names[i]) |> 
    ggplot(aes(month, year, fill = value)) +
    geom_tile() +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = facet_names[i]
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(
        size = 12,
        vjust = 0.5,
        hjust = 0.5
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 20
      )
    )
}

  
# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Cabin Sketch",
  family = "title_font"
) # Font for titles
font_add_google("Oswald",
  family = "caption_font"
) # Font for the caption
font_add_google("Abel",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Define colours
bg_col <- "#F3E0C2FF" |> lighten(0.1) # Background Colour
text_col <- "#BB5137FF" |> darken(0.5) # Colour for the text
text_hil <- "#BB5137FF" # Colour for highlighted text

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
plot_title <- "Trash Collection Stats !"

plot_caption <- paste0("**Data:** Mr. Trash Wheel Baltimore Healthy Harbor initiative", " | ", " #TidyTuesday | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

subtitle_text <- "The Baltimore Healthy Harbor initiative uses an autonomous trash-collector: Mr. Trash Wheel. The heatmap shows its trash collection trends, revealing some patterns. Plastic bottles and polystyrene increase in numbers during spring and summer, while the highest overall trash quantities were recorded between 2017 and 2018. Notably, the analysis highlights a recent uptick in the collection of sports balls."
plot_subtitle <- str_wrap(subtitle_text, width = 100)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- plotdf1 |> 
  filter(std_value < 4) |> 
  filter(!(var %in% c("Homes Powered", "Cigarette Butts"))) |> 
  filter(year >= 2015) |> 
  ggplot(aes(
    y = month,
    x = year,
    fill = std_value
  )) +
  geom_tile(
    colour = "white"
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Classic Area-Brown",
    direction = 1,
    na.value = bg_col,
    labels = c("Low", "High"),
    breaks = c(-0.8, 3.8)
  ) +
  facet_wrap(~ var) +
  scale_x_discrete(
    breaks  = seq(2015, 2023, 2)
  ) +
  scale_y_discrete(
    limits = rev
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    x = NULL,
    y = NULL
  ) +
  guides(
    fill = guide_colorbar(
      title = "Number of trash items collected"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom", 
    axis.text.x = element_text(
      size = 3 * ts,
      colour = text_col,
      hjust = 0.5, 
      family = "body_font",
      margin = margin(0,0,0,0, unit = "cm")
    ),
    axis.text.y = element_text(
      size = 3.5 * ts,
      colour = text_col,
      hjust = 0.5, 
      family = "body_font",
      margin = margin(0,0,0,0, unit = "cm")
    ),
    plot.title = element_text(
      size = 16 * ts,
      colour = text_hil,
      margin = margin(2, 0, 0.5, 0, unit = "cm"),
      family = "title_font",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 4.5 * ts,
      colour = text_col,
      margin = margin(0.5,0,1,0, unit = "cm"),
      family = "body_font",
      lineheight = 0.3,
      hjust = 0.5
    ), 
    plot.caption = element_textbox(
      size = 2 * ts,
      colour = text_hil,
      margin = margin(0.5,0,0.5,0, unit = "cm"),
      family = "caption_font",
      hjust = 0.5
    ),
    strip.text = element_text(
      size = 7 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_col
    ),
    plot.title.position = "plot",
    legend.title = element_text(
      size = 3.5 * ts,
      colour = text_col,
      hjust = 1, 
      family = "body_font",
      margin = margin(0,0.5,0,0, unit = "cm"),
      vjust = 1
    ),
    legend.text = element_text(
      size = 3.5 * ts,
      colour = text_col,
      hjust = 1, 
      family = "body_font",
      margin = margin(0.2,0,0,0, unit = "cm")
    ),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(3, "cm"),
    legend.ticks = element_blank()
  )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#


ggsave(
  filename = here::here("docs", "tidy_trash_wheel.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = bg_col
  )

