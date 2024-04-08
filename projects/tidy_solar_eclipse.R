#=============== #TidyTuesday : 2023 & 2024 US Solar Eclipses =================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading

# Mapping libraries
library(usmap)          # Nice US Map with inset Alaska and Hawaii
library(sf)             # Maps and geomcomputation

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(scales)         # Scale Labels in ggplot2
library(patchwork)      # For compiling plots

# Other optional libraries
library(glue)           # To paste together text for ggtext
library(ggimage)        # Using Images in ggplot2
library(magick)         # Work with Images and Logos

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

ea23 <- tuesdata$eclipse_annular_2023
et24 <- tuesdata$eclipse_total_2024
# ep23 <- tuesdata$eclipse_partial_2023
# ep24 <- tuesdata$eclipse_partial_2024

rm(tuesdata)

# Random explorations
ep24 |> ggplot(aes(dur_pecl)) + 
  geom_histogram(bins = 200) +
  scale_x_continuous(limits = c(1990, 2200))

#==============================================================================#
# Exploratory Data Analysis & Data Wrangling -----------------------------------
#==============================================================================#

# library(summarytools)
# dfSummary(et24) |> view()

pet24 <- et24 |> 
  mutate(dur_tecl = as.numeric(eclipse_4 - eclipse_3)) |> 
  usmap_transform()

pea23 <- ea23 |> 
  mutate(dur_aecl = as.numeric(eclipse_4 - eclipse_3)) |> 
  usmap_transform()

both_eclipse <- et24 |> 
  filter(state == "TX") |> 
  inner_join(ea23, by = join_by(
    name == name,
    lat == lat,
    lon == lon
  )) |> 
  mutate(
    dur2023 = as.numeric(eclipse_4.y - eclipse_3.y),
    dur2024 = as.numeric(eclipse_4.x - eclipse_3.x)
  ) |> 
  usmap_transform()


#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Gotu", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Pompiere", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Define colours
text_col <- "#F2EBBBFF"               # Colour for the text
text_hil <- "#DDAA33FF"               # Colour for highlighted text
bg_col <- "grey10"                    # Background Colour
text_hil2 <- "red"                    # Highlight colour for town names 

# Define Text Size
ts =  90             # Text Size
tsi = ts / 2         # Text Size Inset

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
plot_title <- "Eclipse Durations: 2023 vs. 2024"
subtitle_text <- glue::glue("Comparison of the 2023 Annular Solar Eclipse, and 2024 Total Solar<br>Eclipse in USA. Of the 54 Texas towns that witnessed both, the two<br>lucky towns: <b style='color:{text_hil2}'> Leaky, TX </b> and <b style='color:{text_hil2}'> Utopia, TX </b> saw both eclipses for the<br>maximum duration – over 4 minutes each.")
plot_subtitle <- subtitle_text
plot_caption <- paste0("**Data & Inspiration:** NASA's Scientific Visualization Studio |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)
inset_subtitle <- "Comparison of 2023 Annular Eclipse vs. 2024 Total Eclipse in Texas Towns witnessing both of them."
inset_subtitle <- str_wrap(inset_subtitle, width = (str_length(inset_subtitle)/1.9))
inset_title <- "Texas Towns witnessing both eclipses"

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g1 <- plot_usmap(
  fill = "transparent",
  colour = "grey75"
) +
  geom_sf(
    data = pet24,
    mapping = aes(
      colour = dur_tecl
    ),
    alpha = 0.5,
    size = 1.1
  ) +
  paletteer::scale_colour_paletteer_c(
    "ggthemes::Orange",
    labels = label_timespan(unit = c("secs")),
    name = "Duration of Total eclipse (2024)",
    direction = -1,
    breaks = seq(0, 300, 60),
    limits = c(0, 300)
  ) +
  ggnewscale::new_scale_colour() +
  geom_sf(
    data = pea23,
    mapping = aes(
      colour = dur_aecl
    ),
    alpha = 0.5,
    size = 1.5
  ) +
  paletteer::scale_colour_paletteer_c(
    "ggthemes::Blue-Green Sequential",
    labels = label_timespan(unit = c("secs")),
    name = "Duration of Annular eclipse (2023)",
    direction = -1,
    breaks = seq(0, 300, 60),
    limits = c(0, 300)
  ) +
  geom_sf(
    data = both_eclipse,
    shape = 1, 
    size = 5,
    colour = "grey90",
    alpha = 0.5
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  ggthemes::theme_map(
    base_family = "body_font",
    base_size = ts
  ) +
  theme(
    plot.title = element_text(
      family = "title_font",
      colour = text_hil,
      face = "bold",
      size = 2 * ts,
      hjust = 0.5
    ),
    plot.subtitle = element_textbox(
      size = 1.5 * ts,
      hjust = 0.5,
      lineheight = 0.35,
      colour = text_col
    ),
    plot.caption = element_textbox(
      size = ts,
      hjust = 0.5,
      colour = text_col,
      margin = margin(1,0,1,0, "cm"),
      family = "caption_font"
    ),
    legend.box = "vertical",
    legend.position = "bottom",
    legend.box.just = "left",
    legend.box.margin = margin(
      t = 0, r = 0, b = 0, l = 0, "cm"
    ),
    legend.box.spacing = unit(1, "cm"),
    legend.title.position = "top",
    legend.title = element_text(
      hjust = 0,
      colour = text_col, 
      margin = margin(0,0,0,0, "cm")
    ),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    legend.text = element_text(
      colour = text_col,
      margin = margin(0,0,0,0, "cm")
    ),
    legend.key.width = unit(5, "cm"),
    legend.key.height = unit(1, "cm")
  )

g2 <- both_eclipse |> 
  ggplot(
    aes(
      x = dur2023, 
      y = dur2024, 
      label = name)
  ) +
  geom_point(
    pch = 1,
    colour = "grey40",
    size = 4
  ) +
  geom_text(
    check_overlap = TRUE,
    family = "body_font",
    colour = "white",
    size = tsi / 2
  ) +
  geom_abline(
    colour = "grey40",
    linetype = 1,
    linewidth = 2,
    alpha = 0.2
  ) +
  scale_x_continuous(
    breaks = seq(0, 300, 60),
    labels = label_timespan(),
    limits = c(0, 300)
  ) +
  scale_y_continuous(
    breaks = seq(0, 300, 60),
    labels = label_timespan(),
    limits = c(0, 300)
  ) +
  coord_fixed() +
  labs(
    x = "Duration of Annular Eclipse (2023)",
    y = "Duration of Total Eclipse (2024)",
    title = inset_title,
    subtitle = inset_subtitle
  ) +
  theme_dark(
    base_family = "body_font",
    base_size = tsi
  ) +
  theme(
    plot.background = element_rect(
      colour = NA,
      fill = "transparent"
    ),
    panel.background = element_rect(
      fill = "grey15"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = 3,
      colour = "grey45"
    ),
    axis.title = element_text(
      colour = text_col
    ),
    axis.text = element_text(
      colour = text_col
    ),
    plot.title = element_text(
      colour = text_hil,
      family = "title_font",
      size = 2 * tsi
    ),
    plot.subtitle = element_text(
      colour = text_hil,
      family = "title_font",
      size = 1.5 * tsi,
      lineheight = 0.35
    )
  )

g <- g1 +
  inset_element(
    p = g2,
    l = 0.5, r = 1,
    b = 0.05, t = 0.5,
    align_to = "full",
    on_top = TRUE
    ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = bg_col,
        colour = NA
      )
    )
  )


#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_eclipse.png"),
  plot = g,
  width = 40, 
  height = 50, 
  units = "cm",
  bg = bg_col
)
  