# =================An Attempt at Shadowed Designer Bar Chart====================#

# ==============================================================================#
# Library Load-in---------------------------------------------------------------
# ==============================================================================#
library(tidyverse) # Data Wrangling and Plotting
library(here) # Files location and loading
library(showtext) # Using Fonts More Easily in R Graphs
library(ggimage) # Using Images in ggplot2
library(fontawesome) # Social Media icons
library(ggtext) # Markdown Text in ggplot2
library(patchwork) # For compiling plots
library(figpatch) # Images in patchwork
library(magick) # Work with Images and Logos
library(rnaturalearth) # Maps of countries
library(colorspace) # Lighten and darken Colours
library(ggfx) # To add outer glow


# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#

# Option 1: tidytuesdayR package
## install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2024, week = 6)
heritage <- tuesdata$heritage
rm(tuesdata)
# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#

heritage

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#

# Long format data
df <- heritage |>
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "n_sites"
  ) |>
  mutate(country = str_to_lower(country))

# List of coutnries to plot
plot_cons <- df |>
  pull(country) |>
  unique() |>
  str_to_lower() |>
  sort()

# Getting map polygons of the countries
map_df <- ne_countries(
  scale = "large",
  returnclass = "sf",
  country = plot_cons
)

df_cord <- tibble(
  country = fct(plot_cons, levels = plot_cons),
  c_code = c("DK", "NO", "SE"),
  x = c(8, 9, 19),
  y = c(57, 62, 67),
  col = c("red", "darkblue", "yellow")
)
# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Cinzel",
  family = "title_font"
) # Font for titles
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) # Font for the caption
font_add_google("Bebas Neue",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
bg_col <- "black"        # Background Colour
fil_col <- "#292929"   # Colour to fill in Countries
border_col <- "darkgrey" # Bountry border colours
text_col <- "white"    # Colour for the text
text_hil <- "white"    # Colour for highlighted text

# Define Text Size
ts <- unit(2, units = "cm") # Text Size

# Caption stuff
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "UNESCO Sites: 2004 vs. 2022"
subtitle_text <- "Comparison of UNESCO Sites in Nordic Countries in 2004 and 2022.\nWhile Sweden has the most sites (15), Denmark showed the highest increase (150%)."
plot_subtitle <- subtitle_text
plot_caption <- paste0("**Data:** UNESCO World Heritage Sites | ", "**Graphics:**", social_caption_1, " |  **Code:** ", social_caption_2)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

# UNESCO Sites Bar Graph
barplot_con <- function(country_to_plot){
  df |>
    filter(country %in% country_to_plot) |>
    ggplot(aes(x = n_sites, 
               y = year,
               fill = as_factor(year))) +
    geom_col(
      width = 0.9, 
      alpha = 0.75
    ) +
    geom_text(
      aes(
        x = 0,
        label = year
      ),
      hjust = 1.1,
      size = 9 * ts,
      family = "body_font",
      color = text_col
    ) +
    geom_text(
      aes(
        x = n_sites,
        label = n_sites
      ),
      hjust = -0.1,
      size = 9 * ts,
      family = "body_font",
      color = text_col
    ) +
    scale_x_continuous(expand = expansion(c(0.4, 0.2))) +
    scale_fill_manual(values = c("#737373", "lightgrey")) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(
        fill = "transparent",
        color = "transparent"
      )
    )
}
  
g1 <- barplot_con("denmark")
g2 <- barplot_con("norway")
g3 <- barplot_con("sweden")

g4 <- map_df |>
  ggplot() +
  with_outer_glow(
    geom_sf(
      fill = fil_col,
      color = border_col
    ),
    colour = border_col
  ) +
  geom_flag(
    data = df_cord,
    mapping = aes(
      x = x,
      y = y,
      image = c_code
    )
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  coord_sf(
    xlim = c(4.5, 31),
    ylim = c(54.5, 70.5)
  ) +
  scale_x_continuous(breaks = 5:30) +
  scale_y_continuous(breaks = 55:70) +
  ggthemes::theme_map() +
  theme(
    plot.caption = element_textbox(
      family = "caption_font",
      hjust = 0.5,
      colour = text_col,
      size = 15 * ts
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 55 * ts,
      family = "title_font",
      face = "bold",
      colour = text_hil,
      margin = margin(4, 0, 2, 0)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 27 * ts,
      family = "body_font",
      colour = text_col,
      margin = margin(5, 0, 2, 0),
      lineheight = 0.35
    ),
    plot.title.position = "plot",
    plot.background = element_rect(
      color = bg_col,
      fill = bg_col)
  )

g5 <- population |> 
  mutate(country = str_to_lower(country)) |> 
  filter(country %in% plot_cons) |> 
  filter(year == 2013) |> 
  left_join(
    df |> filter(year == 2022),
    by = join_by(country == country)
  ) |> 
  mutate(area = c(42952, 385207, 450295)) |> 
  mutate(
    sites_pm = 1e6 * n_sites / population,
    sites_area = 1e5 * n_sites / area
  ) |> 
  select(country, sites_pm, sites_area) |> 
  mutate(c_code = c("DK", "NO", "SE")) |> 
  pivot_longer(
    cols = -c(country, c_code),
    names_to = "indicator",
    values_to = "value"
  ) |> 
  mutate(country = str_to_title(country)) |> 
  mutate(indicator = case_when(
    indicator == "sites_area" ~ "Sites per 100K sq. km.",
    indicator == "sites_pm" ~ "Sites per million population"
  )) |> 
  ggplot(aes(x = value,
             y = country,
             image = c_code)) +
  geom_col() +
  geom_flag(
    aes(x = 0),
    size = 0.2
  ) +
  facet_wrap(
    ~ indicator,
    scales = "free_x",
    ncol = 1) +
  labs(
    x = NULL, y = NULL,
    title = "Denmark has most sites, per unit area and per million population"
  ) +
  theme_void() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      family = "body_font",
      size = 20 * ts,
      color = text_col
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    strip.text = element_text(
      family = "body_font",
      size = 30 * ts,
      color = text_col
    )
  )


g <- g4 +
  inset_element(
    g1,
    left = 0.06, 
    bottom = 0.065, 
    right = 0.31, 
    top = 0.165
  ) +
  inset_element(
    g2,
    left = 0.11, 
    bottom = 0.35, 
    right = 0.31, 
    top = 0.45
  ) +
  inset_element(
    g3,
    left = 0.42, 
    bottom = 0.635, 
    right = 0.72, 
    top = 0.735
  ) +
  inset_element(
    g5,
    left = 0.6, 
    bottom = 0.1, 
    right = 1, 
    top = 0.5
  )


  
# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#


ggsave(
  filename = here::here("docs", "tidy_nordic_unesco.png"),
  plot = g,
  width = 20,
  height = 29.35,
  units = "cm",
  bg = bg_col
)

