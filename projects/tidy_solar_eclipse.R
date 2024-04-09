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
# ep24 |> ggplot(aes(dur_pecl)) + 
#   geom_histogram(bins = 200) +
#   scale_x_continuous(limits = c(1990, 2200))

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
  arrange(desc(dur2024), desc(dur2023)) |> 
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
tsi = ts / 1.5       # Text Size Inset

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
inset_title <- "Duration of both eclipses in Texas towns."

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g1 <- plot_usmap(
  fill = "transparent",
  colour = "grey50"
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
      hjust = 0.5,
      margin = margin(1, 0, 0.2, 0, "cm")
    ),
    plot.subtitle = element_textbox(
      size = 1.5 * ts,
      hjust = 0.5,
      lineheight = 0.35,
      colour = text_col,
      margin = margin(0, 0, 0, 0, "cm")
    ),
    plot.caption = element_textbox(
      size = ts / 1.5,
      hjust = 0.5,
      colour = text_col,
      margin = margin(18, 0, 0.5, 0, "cm"),
      family = "caption_font"
    ),
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.position = c(0, -0.1),
    legend.title.position = "top",
    legend.title = element_text(
      hjust = 0,
      colour = text_col, 
      margin = margin(0, 0, 0.2, 0, "cm"),
      size = 1.2 * ts,
      face = "bold"
    ),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    legend.text = element_text(
      colour = text_col,
      margin = margin(0, 0, 0, 0, "cm")
    ),
    legend.key.width = unit(2.5, "cm")
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
    limits = c(0, 300),
    expand = expansion(0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 300, 60),
    labels = label_timespan(),
    limits = c(0, 300),
    expand = expansion(0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Duration of Annular Eclipse (2023)",
    y = "Duration of Total Eclipse (2024)",
    title = inset_title
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
      fill = bg_col
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = 3,
      linewidth = 0.5,
      colour = "grey45"
    ),
    axis.title = element_text(
      colour = text_col,
      size = 1.5 * tsi
    ),
    axis.text = element_text(
      colour = text_col,
      size = tsi,
      margin = margin(0,0,0,0, "cm")
    ),
    plot.title = element_text(
      size = 1.5 * tsi,
      hjust = 0.5,
      margin = margin(0,0,0,0, "cm"),
      family = "title_font",
      colour = text_hil
    ),
    axis.ticks = element_blank()
    )

g3 <- ggplot() +
  annotate(
    geom = "text",
    label = "Towns witnessing 2024\nTotal Solar Eclipse",
    lineheight = 0.3,
    colour = "orange",
    x = 0,
    y = 0,
    hjust = 0,
    vjust = 1,
    size = ts / 2,
    family = "body_font"
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

g4 <- ggplot() +
  annotate(
    geom = "text",
    label = "Towns which saw 2023\nAnnular Solar Eclipse",
    lineheight = 0.3,
    colour = "#fbffc7",
    x = 0,
    y = 0,
    hjust = 0,
    vjust = 1,
    size = ts / 2,
    family = "body_font"
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

g5 <- ggplot() +
  annotate(
    geom = "text",
    label = "Each dot represents a town\nand colour shows eclipse duration.",
    lineheight = 0.3,
    colour = text_col,
    x = 0,
    y = 0,
    hjust = 0.5,
    vjust = 1,
    size = ts / 3,
    family = "body_font"
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

g <- g1 + 
  inset_element(
    p = g2,
    left = 0.1, right = 0.9,
    bottom = 0.05, top = 0.36,
    align_to = "full",
    on_top = TRUE
    ) +
  inset_element(
    p = g3,
    left = 0.56, right = 0.85,
    bottom = 0.6, top = 0.65,
    align_to = "full",
    on_top = TRUE
  ) +
  inset_element(
    p = g4,
    left = 0.03, right = 0.18,
    bottom = 0.55, top = 0.6,
    align_to = "full",
    on_top = TRUE
  ) +
  inset_element(
    p = g5,
    left = 0.4, right = 0.6,
    bottom = 0.7, top = 0.75,
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
  height = 55, 
  units = "cm",
  bg = bg_col
)
  