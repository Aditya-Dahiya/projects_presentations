# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# Harvard Dataverse>
# Colonial Dates Dataset (COLDAT)
# 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/T9SDEW&version=3.0
# The Colonial Dates Dataset (COLDAT) aggregates information on the reach and 
# duration of European colonial empires from renowned secondary sources. 
# By aggregating secondary sources, rather than collecting from primary 
# sources, the new dataset reflects the accumulated knowledge in the discipline 
# and relieves researchers from making hard to justify choices between 
# different historical datasets.
# 
# Becker, Bastian, 2019, "Colonial Dates Dataset (COLDAT)", 
# https://doi.org/10.7910/DVN/T9SDEW, Harvard Dataverse, V3, 
# UNF:6:C6K8phVSG+Sfne/jKD+XhA== [fileUNF]
# 
# Becker, Bastian (2019), Introducing COLDAT: 
# The Colonial Dates Dataset, SOCIUM/SFB1342 Working Paper Series, 02/2019.
# 
# The Colonial Dates Dataset, compiled by political scientist Bastian Becker, 
# “aggregates information on the reach and duration of European colonial
#  empires from renowned secondary sources.” Producing the dataset from its 
#  four main sources “is largely automated, relying on predefined coding 
#  rules.” The dataset indicates the first and last years each contemporary 
#  country was colonized, disaggregated by eight colonizing countries 
#  (Belgium, Britain, France, Germany, Italy, Netherlands, Portugal, 
#  and Spain). [h/t Erik Gahner Larsen]


# =============================================================================#
# Findings ---------------------------------------------------------------------
# =============================================================================#

# Total 127 countries who were colonized.
# Total 8 colonizer countries.



# =============================================================================#
# Library Load-in---------------------------------------------------------------
# =============================================================================#

# Data Wrangling Tools
library(tidyverse)
library(janitor)
library(here)
library(dataverse)

# Final plot (ggplot2) tools
library(scales)
library(fontawesome)
library(ggtext)
library(showtext)
library(colorspace)
library(ggthemes)
library(patchwork)

# =============================================================================#
# Data Load-in, EDA & Data Wrangling--------------------------------------------
# =============================================================================#

colonial <- get_dataframe_by_id(
  fileid = 7416946,
  .f = readr::read_tsv,
  server = "dataverse.harvard.edu"
) |> 
  as_tibble()

colonial |> 
  names()

df1 <- colonial |> 
  pivot_longer(
    cols = starts_with("colstart.") & ends_with("mean"),
    names_to = NULL,
    values_to = "year_start_mean"
  ) |> 
  pivot_longer(
    cols = starts_with("colstart.") & ends_with("max"),
    names_to = NULL,
    values_to = "year_start_max"
  ) |> 
  pivot_longer(
    cols = starts_with("colend.") & ends_with("mean"),
    names_to = NULL,
    values_to = "year_end_mean"
  ) |> 
  pivot_longer(
    cols = starts_with("colend.") & ends_with("max"),
    names_to = NULL,
    values_to = "year_end_max"
  ) |> 
  pivot_longer(
    cols = c(
      "col.belgium",
      "col.britain",
      "col.france",
      "col.germany",
      "col.italy",
      "col.netherlands",
      "col.portugal",
      "col.spain"
    ),
    names_to = "colonizer",
    values_to = "filter_colonizer"
  ) |> 
  filter(filter_colonizer != 0) |> 
  select(-filter_colonizer) |> 
  mutate(
    colonizer = str_remove(colonizer, "col.")
  )

df2 <- df1 |> 
  group_by(country, colonizer) |> 
  summarise(
    year_start_mean = min(year_start_mean, na.rm = TRUE),
    year_start_max = min(year_start_max, na.rm = TRUE),
    year_end_mean = max(year_end_mean, na.rm = TRUE),
    year_end_max = max(year_end_max, na.rm = TRUE)
  ) |> 
  mutate(
    colonizer = str_to_title(colonizer),
    duration = year_end_max - year_start_max
  ) |> 
  ungroup()

colonizer_levels <- df2 |> 
  count(colonizer, sort = T) |> 
  arrange(n) |> 
  pull(colonizer)

colonizer_levels2 <- c(
  "Britain", "Belgium", "France", "Germany",
  "Spain", "Netherlands", "Portugal", "Italy"
)
df3 <- df2 |> 
  mutate(colonizer = fct(colonizer, levels = colonizer_levels2)) |> 
  group_by(colonizer) |> 
  arrange(colonizer, duration) |> 
  ungroup() |> 
  mutate(id = row_number())

df4labs <- df3 |> 
  group_by(colonizer) |> 
  summarize(
    y_place = (max(id) + min(id))/2
  ) |> 
  mutate(
    x_place = c(1500, 1600, 1500, 1500, 1900, 1500, 1450, 1500)
    )

# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Stint Ultra Condensed",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Advent Pro",
  family = "body_font"
) 

# Font for plot text 2
font_add_google("UnifrakturMaguntia",
  family = "country_font"
)

showtext_auto()

bg_col <- "white" # Background Colour
text_col <- "#364052" # Colour for the text
text_hil <- "#4a5a78" # Colour for highlighted text

# Define Text Size
ts <- 80 # Text Size

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
plot_title <- "Colonial Empires\nTimeline"
plot_caption <- paste0("**Data:** Colonial Dates Dataset (COLDAT)", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- "A graph showing the span of colonial dominions, by the eight great European powers, across nations globally. Observe the abrupt cessation of many colonial realms after the Second World War, in the latter portion of the twentieth century."
plot_subtitle <- "A graph showing the span of colonial\ndominions, by the eight great European\npowers, across nations globally. Observe\nthe abrupt cessation of many colonial\nrealms after the Second\nWorld War, in the\nlatter portion of the\ntwentieth century."

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- ggplot(
  data = df3,
  mapping = aes(
    y = id,
    fill = colonizer,
    colour = colonizer
  )
  ) +
  annotate(
    geom = "label",
    x = 1300,
    y = 0,
    hjust = 0,
    vjust = 1,
    family = "title_font",
    colour = text_hil,
    lineheight = 0.25,
    size = 1.5 * ts,
    label = plot_title,
    label.size = NA,
    fill = bg_col,
    label.padding = unit(0, "lines"),
    label.r = unit(0, "lines")
  ) +
  annotate(
    geom = "label",
    x = 1300,
    y = 25,
    hjust = 0,
    vjust = 1,
    family = "title_font",
    colour = text_col,
    lineheight = 0.3,
    size = 0.5 * ts,
    label = plot_subtitle,
    label.size = NA,
    fill = bg_col,
    label.padding = unit(0, "lines"),
    label.r = unit(0, "lines")
  ) +
  geom_segment(
    aes(
      x = year_start_max,
      xend = year_end_max
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(x = year_start_max), 
    pch = 20,
    size = 2
    ) +
  geom_point(
    aes(x = year_end_max), 
    pch = 20,
    size = 2
    ) +
  geom_text(
    aes(
      x = year_start_max - 10,
      label = country
    ),
    hjust = 1,
    size = ts/8,
    family = "body_font"
  ) +
  geom_text(
    aes(
      x = year_end_max + 10,
      label = country
    ),
    hjust = 0,
    size = ts/8,
    family = "body_font"
  ) +
  geom_text(
    data = df4labs,
    aes(x = 2050, 
        y = y_place, 
        label = colonizer),
    size = 0.75 * ts, 
    hjust = 0,
    vjust = 0.5,
    family = "country_font"
  ) +
  scale_y_reverse(
    expand = expansion(c(0.02, 0))
  ) +
  scale_x_continuous(
    breaks = seq(1400, 2000, 100),
    expand = expansion(c(0.07, 0.38))
  ) +
  paletteer::scale_colour_paletteer_d("MetBrewer::Thomas") +
  labs(
    x = "Year",
    y = NULL,
    caption = plot_caption
  ) +
  theme_minimal(
    base_family = "body_font",
    base_size = ts
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 1,
      linewidth = 2,
      colour = "grey95"
    ),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(
      colour = text_col,
      size = 1 * ts,
      margin = margin(0.2, 0, 0.5, 0, "cm"),
      vjust = 1
    ),
    axis.title = element_text(
      colour = text_col,
      size = 1.2 * ts,
      margin = margin(0.2, 0, 0.5, 0, "cm"),
      hjust = 0.5
    ),
    axis.line.x = element_line(
      colour = "grey95",
      linewidth = 2
    ),
    axis.ticks = element_blank(),
    plot.caption = element_textbox(
      hjust = 0.5,
      family = "caption_font",
      colour = text_col,
      size = ts / 1.5,
      margin = margin(0.5, 0, 1, 0, "cm")
    )
  )


# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_colonial.png"),
  plot = g,
  width = 40,
  height = 59,
  units = "cm",
  bg = "white"
)
