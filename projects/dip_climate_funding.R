# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# Credit: Climate Policy Initiative
# Global Landscape of Climate Finance 2023
# Barbara Buchner, Baysa Naran, Rajashree Padmanabhi, Sean Stout, Costanza 
# Strinati, Dharshan Wignarajah, Gaoyi Miao, Jake Connolly and Nikita Marini
# November 2, 2023
# https://www.climatepolicyinitiative.org/publication/global-landscape-of-climate-finance-2023/

# Climate Policy Initiative collected data to produce its "Global Landscape of 
# Climate Finance 2023" report, examining various forms of primary financing 
# aimed at reducing greenhouse gas emissions and enhancing climate resilience in 
# real economy sectors. Released in November, the report disclosed approximately 
# $1.3 trillion in such financing worldwide during 2021–2022. Accompanying the 
# report is a spreadsheet detailing estimates by year, region, sector, focus on 
# mitigation versus adaptation, financial instrument type, funder sector (public 
# versus private), and funder type (development bank, corporation, institutional 
# investors, etc.). Previous reports offer downloadable data covering 2019–2020 
# and 2017–2018, with earlier coverage on national climate funds 
# (DIP 2022.02.09) and climate finance projects (DIP 2023.06.14).

# Global-Landscape-of-Climate-Finance-2023
# GLCF 2023 – Data Download
url1 <- "https://www.climatepolicyinitiative.org/wp-content/uploads/2023/11/GLCF-2023-Data-Download.xlsx"

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
library(treemapify)
library(colorspace)
library(gganimate)

# =============================================================================#
# Data Load-in & Data Wrangling-------------------------------------------------
# =============================================================================#

# Breakdown of global climate finance by public and private actors (USD billion)
finance <- read.xlsx(
  url1,
  sheet = 3,
  colNames = TRUE,
  rowNames = FALSE,
  rows = c(4, 6:11, 13:21),
  cols = 1:7
  ) |> 
  remove_empty() |> 
  mutate(
    funding_type = c(
      rep("Private", 6),
      rep("Public", 9)
    )
  ) |> 
  rename(actors = Actors) |> 
  as_tibble() |> 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "value"
  )

# Breakdown of global climate finance by Use and Sector (USD billion)
usage <- read.xlsx(
  url1,
  sheet = 3,
  colNames = TRUE,
  rowNames = FALSE,
  rows = c(56, 58:66, 68:77, 79:88, 90:91),
  cols = 1:5
  ) |> 
  remove_empty() |> 
  mutate(
    usage_type = c(
      rep("Adaptation", 9),
      rep("Mitigation", 10),
      rep("Dual Benefits", 10),
      rep("Unknown", 2)
    )
  ) |> 
  rename(sector = `Use/Sector`) |> 
  as_tibble() |> 
  pivot_longer(
    cols = 2:5,
    names_to = "year",
    values_to = "value"
  )

# Breakdown of global climate finance by region of destination (USD billion)

regions <- read.xlsx(
  url1,
  sheet = 3,
  colNames = TRUE,
  rowNames = FALSE,
  rows = 167:177,
  cols = 1:7
) |> 
  remove_empty() |> 
  rename(region = `Region`) |> 
  as_tibble() |> 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "value"
  ) 

# Create a common tibble to join all data to use a faceted treemap

df <- bind_rows(
  regions |> 
    rename(fill_var = region) |> 
    mutate(
      group_var = " ",
      table_type = "Usage of funds"
    ),
  finance |> 
    rename(
      fill_var = actors,
      group_var = funding_type
    ) |> 
    mutate(table_type = "Source of Funds")
  ) |> 
  mutate(year = as.numeric(year))


# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
font_add_google("Fredericka the Great",
  family = "title_font"
) # Font for titles
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) # Font for the caption
font_add_google("Fira Sans Extra Condensed",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Define colours
bg_col <- "white"   # Background Colour
text_col <- "#04225CFF" # Colour for the text
text_hil <- "#309D96FF" # Colour for higlighted text

# Define Text Size
ts <- unit(30, units = "cm") # Text Size

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
plot_title <- ""
plot_caption <- paste0("**Data:** ", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- ""
plot_subtitle <- str_wrap(subtitle_text, width = 115)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

ggplot(
  df |> filter(year == 2022), 
  aes(
    label = fill_var,
    area = value,
    subgroup = group_var,
    fill = fill_var
  )) +
  geom_treemap(
    layout = "fixed"
  ) +
  geom_treemap_text(
    layout = "fixed", 
    place = "centre", 
    grow = TRUE, 
    colour = "white"
  ) +
  geom_treemap_subgroup_text(
    layout = "fixed", 
    place = "centre"
  ) +
  geom_treemap_subgroup_border(
    layout = "fixed"
  ) +
  facet_wrap(~ table_type)


  transition_time(year) +
  ease_aes('linear') +
  labs(title = "Year: {frame_time}")

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

anim_save(
  filename = here::here("docs", "dip_climate_funding.gif"),
  animation = g
)

ggview::ggview(
  device = "png",
  plot = g,
  width = 23,
  height = 30,
  units = "cm",
  bg = bg_col
)

ggsave(
  filename = here::here("docs", "dip_climate_funding.png"),
  device = "png",
  plot = g,
  width = 23,
  height = 30,
  units = "cm",
  bg = bg_col
)
