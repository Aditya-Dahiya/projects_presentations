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

# =============================================================================#
# Findings ---------------------------------------------------------------------
# =============================================================================#

# Where the money comes from:
# The share of public and private sources in the funding for climate change 
# mitigation activities has stayed almost the same. Both have increased 
# together, in same proportion. Within the various sources of funding, 
# largest increases in funding have come from “Public Funds” and “State-Owned 
# Enterprises”. Amongst private sources of funds, Commercial Institutions and 
# Individual Households provide bulk of the funding.

# Where the money goes:
# The East Asia and Pacific region continue to get an ever-increasing share of 
# total funds dedicated to Climate Change. However, importantly, the Western 
# Europe region has increased its allotment by the maximum, a staggering 152%.

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

# Get levels of categories fixed for nicer colours later on
fill_var_levels <- df |> 
  group_by(table_type, group_var, fill_var) |> 
  summarise(total_val = sum(value)) |> 
  arrange(desc(total_val)) |> 
  pull(fill_var)

plotdf <- df |> 
  mutate(fill_var = fct(fill_var, levels = fill_var_levels)) |> 
  ungroup()

# Some EDA Plots to write the messages in actual plot
plotdf |> 
  filter(table_type == "Usage of funds") |> 
  ggplot(aes(x = year, y = value, col = fill_var)) +
  geom_line(linewidth = 2) +
  scale_color_brewer(palette = "Dark2")

plotdf |> 
  filter(table_type == "Usage of funds") |> 
  group_by(fill_var) |> 
  summarise(increase = (100 * (max(value) - min(value)))/min(value))

plotdf |> 
  filter(table_type != "Usage of funds") |> 
  group_by(group_var, year) |> 
  summarise(value = sum(value, na.rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, col = group_var)) +
  geom_line(linewidth = 2)

plotdf |> 
  filter(table_type != "Usage of funds") |> 
  group_by(group_var, fill_var, year) |>
  summarise(value = sum(value, na.rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, 
             col = fill_var,
             linetype = group_var)) +
  geom_line(linewidth = 2) +
  scale_color_brewer(palette = "Dark2")

plotdf |> 
  filter(table_type != "Usage of funds") |> 
  group_by(group_var, fill_var) |> 
  summarise(increase = (100 * (max(value) - min(value)))/min(value)) |> 
  group_by(group_var) |> 
  arrange(desc(increase))

# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Fjalla One",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Anton",
  family = "body_font"
) 

showtext_auto()

# Define colours
bg_col <- "white"   # Background Colour
text_col <- "#009270FF" |> darken(0.5) # Colour for the text
text_hil <- "#009270FF" # Colour for higlighted text

# Add text to plot--------------------------------------------------------------
plot_title <- "Flow of Global Climate Funds"
plot_caption <- paste0("Data: Climate Policy Initiative | Code & Graphics: GitHub @aditya-dahiya")
subtitle_text <- "Exploring the sources of “Climate Funds” – funds for combating climate change, its mitigation, reversal; and, the regions where these go to. From 2017 to 2022, share of public and private sources in the funding has stayed constant. Within the various sources of funding, largest increases in funding have come from “Public Funds” and “State-Owned Enterprises”. Further, the East Asia and Pacific region continue to get an ever-increasing share of total funds dedicated to Climate Change. However, importantly, the Western Europe region has increased its allotment by the maximum, a staggering 152%, from 2017 to 2022."
plot_subtitle <- str_wrap(subtitle_text, width = 100)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- ggplot(plotdf, 
  aes(
    label = fill_var,
    area = value,
    subgroup = group_var,
    fill = fill_var
  )) +
  geom_treemap(
    layout = "fixed"
  ) +
  geom_treemap_subgroup_border(
    layout = "fixed",
    colour = "white",
    alpha = 0.5
  ) +
  geom_treemap_subgroup_text(
    layout = "fixed", 
    place = "centre",
    colour = "white",
    family = "body_font"
  ) +
  geom_treemap_text(
    layout = "fixed", 
    place = "center", 
    grow = FALSE, 
    colour = text_col,
    family = "caption_font",
    reflow = TRUE
  ) +
  facet_wrap(~ table_type) +
  scale_fill_manual(
    values = paletteer::paletteer_d("khroma::stratigraphy")[(115):(115 + 25)]
  ) +
  labs(
    title = paste0(plot_title, ": {frame_time}"),
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text.x = element_text(
      colour = text_col,
      family = "body_font",
      size = 18,
      margin = margin(10, 0, 10, 0)
    ),
    plot.title = element_text(
      hjust = 0.5,
      family = "title_font",
      size = 32,
      colour = text_hil, 
      margin = margin(25, 0, 10, 0)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "caption_font",
      size = 14,
      colour = text_col,
      lineheight = 1.1,
      margin = margin(10, 0, 10, 0)
    ),
    plot.caption = element_text(
      hjust = 0.5,
      colour = text_hil,
      size = 9,
      family = "caption_font",
      margin = margin(10, 0, 10, 0)
    )
  ) +
  transition_time(as.integer(year)) +
  ease_aes('linear')

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

anim_save(
  filename = here::here("docs", "dip_climate_funding.gif"),
  animation = g,
  fps = 10,
  duration = 20,
  start_pause = 5,
  end_pause = 10,
  height = 800,
  width = 600,
  units = "px"
)

ggview::ggview(
  device = "png",
  plot = g,
  width = 23,
  height = 30,
  units = "cm",
  bg = bg_col
)

