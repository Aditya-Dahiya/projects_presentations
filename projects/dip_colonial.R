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

df3 <- df2 |> 
  mutate(colonizer = fct(colonizer, levels = colonizer_levels)) |> 
  group_by(colonizer) |> 
  arrange(colonizer, duration) |> 
  mutate(id = row_number())

year_df <- df2 |> 
  group_by(colonizer) |> 
  summarise(
    year_start_mean = min(year_start_mean),
    year_start_max = min(year_start_max),
    year_end_mean = max(year_end_mean),
    year_end_max = max(year_end_max)
  )

total_year_df <- df2 |> 
  summarise(
    year_start_mean = min(year_start_mean),
    year_start_max = min(year_start_max),
    year_end_mean = max(year_end_mean),
    year_end_max = max(year_end_max)
  )

df3
  

# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Road Rage",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Bubbler One",
  family = "body_font"
) 

showtext_auto()

# Define colours
mypal <- c()

bg_col <- mypal[3] # Background Colour
text_col <- mypal[1] # Colour for the text
text_hil <- mypal[5] |> darken(0.3) # Colour for highlighted text

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
plot_title <- "Rolling Stone 500 Albums"
plot_caption <- paste0("**Data:** ", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- ""
plot_subtitle <- subtitle_text

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

ggplot(
  data = df3,
  mapping = aes(
    y = id,
    fill = colonizer,
    colour = colonizer
  )
) +
  geom_segment(
    aes(
      x = year_start_mean,
      xend = year_end_mean
    )
  )
  facet_wrap(
    ~ colonizer,
    ncol = 1,
    scales = "free_y",
    shrink = FALSE
  )


# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_gaza_agri.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = mypal[3]
)
