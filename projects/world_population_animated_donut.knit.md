---
title: "Some ingights on the World Population (1960-2023)"
author: "Aditya Dahiya"
date: "2024-05-20"
subtitle: "Charts and Visualizations on the Population of different regions using World Bank Data"
categories:
  - "Interactive"
  - "Data Visualization"
  - "Animation"
image: "world_population_animated_donut.gif"
format:
  html:
    code-fold: true
editor_options: 
  chunk_output_type: console
execute: 
  error: false
  message: false
  warning: false
  eval: false
filters:
  - social-share
share:
  permalink: "https://aditya-dahiya.github.io/session_presentations/projects/world_population_animated_donut.html"
  twitter: true
  linkedin: true
  email: true
  mastodon: true
---


# Share of different regions in the total world population, from 1960 to 2023

Global population distribution, visualized through an animated donut chart. Using midyear estimates from the [World Bank](https://databank.worldbank.org/) [DataBank](https://data.worldbank.org/indicator/SP.POP.TOTL) and inspired by the compelling [visual style](https://x.com/VisualCap/status/1788403705873674494?s=03) of J[ames Eagle](https://x.com/JamesEagle17)'s [analysis on internet browser popularity](https://www.visualcapitalist.com/cp/the-rise-and-fall-of-popular-web-browsers-since-1994/), this chart illustrates the percentage share of the total world population across seven distinct regions: East Asia & Pacific, Middle East & North Africa, Europe & Central Asia, South Asia, Latin America & Caribbean, Sub-Saharan Africa, and North America. This engaging animation provides a dynamic perspective on demographic trends, reflecting data sourced from the United Nations Population Division, national statistical offices, and other reputable agencies. It shows how the world's population is distributed and how these regions contribute to the global demographic landscape.

![Animated Donut (Pie) Chart of the population-share of 7 different regions in the World's Total Population. Notice the rising share of South Asia and Africa, and the falling share of the western world.](world_population_animated_donut.gif)

### **How I made this graphic?**

Loading required libraries and data import


::: {.cell}

```{.r .cell-code}
# Data Import and Wrangling Tools
library(tidyverse)            # All things tidy
library(janitor)              # Cleaning names etc.
library(wbstats)              # Fetching World Bank Data

# Final plot tools
library(scales)               # Nice Scales for ggplot2
library(fontawesome)          # Icons display in ggplot2
library(ggtext)               # Markdown text support for ggplot2
library(showtext)             # Display fonts in ggplot2
library(gganimate)            # For animation


rawdf <- wb_data(
  indicator = "SP.POP.TOTL",
  country = "regions_only",
  start_date = 1960,
  end_date = 2023,
  return_wide = FALSE,
  gapfill = TRUE,
  mrv = 65
) |> 
  select(
    region = country,
    year = date,
    population = value
  ) 

region_levels <- c(
  "South Asia",
  "Sub-Saharan Africa",
  "Latin America & Caribbean",
  "Middle East & North Africa",
  "East Asia & Pacific",
  "Europe & Central Asia",
  "North America"
)
```
:::


Visualization Parameters


::: {.cell}

```{.r .cell-code}
# Font for titles
font_add_google("Dosis",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Saira Semi Condensed",
  family = "body_font"
) 

showtext_auto()

# Colour Palette
mypal <- paletteer::paletteer_d("feathers::bee_eater")
mypal <- paletteer::paletteer_d("futurevisions::grand_tour")
mypal_fill <- paletteer::paletteer_d("ghibli::PonyoLight")
mypal_medium <- paletteer::paletteer_d("ghibli::PonyoMedium")
mypal_col <- paletteer::paletteer_d("ghibli::PonyoDark")

# Background Colour
bg_col <- "white"

# Colour for the text
text_col <- "grey20"

# Colour for highlighted text
text_hil <- "grey30" 
```
:::


Annotation Text for the Plot


::: {.cell}

```{.r .cell-code}
plot_title <- "Global Population Distribution"

plot_subtitle <- "(1960 to 2023)"

plot_caption <- "Data: World Bank.  |   Graphics: @adityadahiyaias on X"
```
:::


Data Wrangling


::: {.cell}

```{.r .cell-code}
df <- rawdf |> 
  group_by(year) |> 
  mutate(percentage = population / sum(population)) |> 
  mutate(region = fct(region, levels = region_levels)) |> 
  ungroup() |> 
  mutate(print_pop = number(population, 
                            accuracy = 0.01,
                            scale_cut = cut_short_scale())) |> 
  group_by(year) |> 
  mutate(totalpop = sum(population)) |> 
  ungroup()
```
:::


The static plot & animating it


::: {.cell}

```{.r .cell-code}
g <- df |> 
  ggplot(
    aes(
      x = 1,
      y = percentage,
      fill = region,
      group = region
    )
  ) + 
  geom_col(
    colour = "white",
    position = position_stack()
  ) +
  
  # Text in the plot / animation
  geom_text(
    mapping = aes(
      label = paste0(
        round(100*percentage, 0),
        "%"
        ),
      colour = region),
    position = position_stack(vjust = 0.5),
    family = "body_font",
    fontface = "bold",
    size = 6
  ) +
  geom_text(
    mapping = aes(
      label = paste0(
        str_wrap(region, 15),
        "\n",
        print_pop
        ),
      x = 1.6,
      colour = region
    ),
    position = position_stack(vjust = 0.5),
    hjust = "outward",
    lineheight = 1,
    family = "body_font",
    size = 3
  ) +
  
  # Central Year Annotation
  geom_text(
    aes(
      label = as.factor(year),
      x = -0.4,
      y = 0
    ),
    size = 10,
    family = "title_font",
    colour = text_hil
  ) +
  
  geom_text(
    aes(
      label = paste0("Total: ", round(totalpop / 1e9, 1), " Billion"),
      x = -0.8,
      y = 0
    ),
    size = 5,
    family = "title_font",
    colour = text_hil
  ) +
  
  # Scales
  scale_y_continuous(expand = expansion(0)) +
  scale_x_continuous(
    limits = c(-0.8, 2)
  ) +
  scale_fill_manual(values = mypal_fill) +
  scale_colour_manual(values = mypal_col) +
  labs(
    title = plot_title,
    caption = plot_caption,
    subtitle = plot_subtitle
  ) +
  coord_polar(theta = "y") +
  theme_minimal(
    base_family = "body_font"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      hjust = 0.5, 
      family = "title_font",
      colour = text_hil,
      margin = margin(0,0,0,0),
      size = 30,
      face = "bold"
    ),
    plot.subtitle = element_text(
      margin = margin(0,0,0,0),
      colour = text_hil,
      family = "title_font",
      hjust = 0.5,
      size = 15
    ),
    plot.caption = element_text(
      hjust = 0.5,
      family = "caption_font",
      colour = text_hil,
      margin = margin(0,0,0,0)
    )
  )


g_anim <- g +
  transition_states(year) +
  ease_aes("linear")
```
:::


Savings the animation


::: {.cell}

```{.r .cell-code}
anim_save(
  filename = here::here(
    "projects", 
    "world_population_animated_donut.gif"
    ),
  animation = g_anim,
  fps = 5,
  duration = 20,
  start_pause = 3,
  end_pause = 10,
  height = 500,
  width = 480
)
```
:::


## Yearly population changes

![This graphic depicts the yearly percentage changes in population for various countries from 1961 to 2023, highlighting significant increases due to immigration and sharp declines resulting from war and natural disasters.](/data_vizs/a4_world_population_change.png){width="900"}

The data used for these line charts is sourced from the World Bank DataBank, which compiles midyear population estimates based on the de facto definition of population, encompassing all residents regardless of legal status or citizenship. This data, spanning from 1961 to 2023, includes inputs from various reputable sources such as the United Nations Population Division, national statistical offices, Eurostat, and the U.S. Census Bureau. The graphic illustrates the yearly percentage changes in population for different countries, highlighting significant demographic shifts. The analysis reveals notable population increases in countries like the UAE (1969-1977, 2007-2008) and Qatar (2006-2008), likely driven by immigration. Conversely, the charts emphasize sharp population declines due to war or natural disasters in countries such as Lebanon (1978-79), Afghanistan (1981-82), Kuwait (1990-91), Rwanda (1994-95), Kosovo (1998-1999), Syria (2013-2014), Libya (2012), and Ukraine (2022). These trends provide a stark visualization of how political instability, conflict, and crises can dramatically impact population dynamics.

### How I made this graphic?

Setting Parameters


::: {.cell}

```{.r .cell-code}
# Font for titles
font_add_google("Dosis",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Saira Semi Condensed",
  family = "body_font"
) 

text_col <- "grey20"

text_hil <- "grey30"

mypal <- paletteer::paletteer_d("ggthemes::Hue_Circle")

showtext_auto()

bg_col <- "white"

# Caption stuff for the plot
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

ts = 80

plot_title <- "Global Population Trends: Surges and Declines (1961-2023)"

plot_subtitle <- glue::glue("The line charts highlight important demographic changes, with rapid population increases in the <b style='color:{mypal[16]}'>UAE</b> (1969-1977,<br>2007-2008) and <b style='color:{mypal[11]}'>Qatar</b> (2006-2008), likely due to immigration. In contrast, marked population declines are observed in<br>countries like <b style='color:{mypal[8]}'>Lebanon </b> (1978-79), <b style='color:{mypal[1]}'>Afghanistan </b> (1981-82), <b style='color:{mypal[7]}'>Kuwait  </b>(1990-91), <b style='color:{mypal[12]}'>Rwanda </b>(1994-95), <b style='color:{mypal[6]}'>Kosovo </b>(1998-1999),<br><b style='color:{mypal[14]}'>Syria</b> (2013-2014), <b style='color:{mypal[10]}'>Libya </b>(2012), and <b style='color:{mypal[15]}'>Ukraine </b>(2022), primarily resulting from wars. These patterns highlight the profound<br>impact of socio-political and environmental factors on population dynamics.")

str_view(plot_subtitle)

plot_caption <- paste0(
  "**Data:** World Bank Databank. |  ",
  "**Code:** ", 
  social_caption_1, 
  " |  **Graphics:** ", 
  social_caption_2
  )

inset_text <- "About the data: The data for these line charts is derived from the World Bank DataBank, specifically the dataset on total population (ID: SP.POP.TOTL). This dataset is based on the de facto definition of population, counting all residents regardless of legal status or citizenship, and provides midyear estimates. The primary sources include the United Nations Population Division's World Population Prospects (2022 Revision), census reports, statistical publications from national statistical offices, Eurostat's demographic statistics, the United Nations Statistical Division's Population and Vital Statistics Report, the U.S. Census Bureau's International Database, and the Secretariat of the Pacific Community's Statistics and Demography Programme. These comprehensive and reputable sources ensure the reliability and accuracy of the population data presented in the charts."

ggplot() +
  labs(subtitle = plot_subtitle) +
  theme(plot.subtitle = element_markdown())
```
:::


Data Wrangling


::: {.cell}

```{.r .cell-code}
rawdf2 <- wb_data(
  indicator = "SP.POP.TOTL",
  start_date = 1960,
  end_date = 2023,
  return_wide = FALSE,
  gapfill = TRUE,
  mrv = 65
)

high_pop_countries <- rawdf2 |> 
  filter(date == 2022) |> 
  filter(value > 1e5) |> 
  pull(country)


df <- rawdf2 |> 
  select(
    country, year = date,
    value, iso2c
  ) |> 
  mutate(iso2c = str_to_lower(iso2c)) |>
  group_by(country) |> 
  mutate(increase = 100*(value - lead(value))/value) |> 
  mutate(disp_val = (increase > 13 | increase < -5)) |> 
  filter(country %in% high_pop_countries) |> 
  mutate(country = fct(country))

df |> 
  filter(disp_val) |> 
  distinct(country)
```
:::

::: {.cell}

```{.r .cell-code}
g_base <- df |> 
  ggplot(aes(x = year, 
             y = increase,
             group = country)) +
  geom_line(colour = "grey10", alpha = 0.2) +
  geom_point(
    data = df |> filter(disp_val),
    mapping = aes(colour = country),
    size = 3, 
    alpha = 0.5
  ) +
  ggrepel::geom_text_repel(
    data = df |> filter(disp_val),
    mapping = aes(
      label = paste0(country, "\n(", round(increase, 1), "%,", year, ")"),
      colour = country
    ),
    lineheight = 0.3,
    family = "body_font",
    size = ts / 4
  ) +
  geom_hline(yintercept = 0, colour = text_hil, linewidth = 1) +
  annotate(
    geom = "label",
    x = 1962,
    y = -8,
    hjust = 0, 
    vjust = 1,
    label = str_wrap(inset_text, 60),
    colour = text_hil,
    fill = bg_col,
    lineheight = 0.35,
    size = ts / 7,
    family = "caption_font",
    label.size = NA
  ) +
  annotate(
    geom = "label",
    x = 2010, 
    y = -25,
    label = "Name of the Country\n(% Population Change, Year)",
    family = "body_font",
    fontface = "bold",
    colour = mypal[1],
    lineheight = 0.3,
    size = ts / 3,
    fill = bg_col,
    hjust = 0.5,
    vjust = 0.5,
    label.padding = unit(0.15, "lines")
  ) +
  scale_x_continuous(
    expand = expansion(0), 
    breaks = seq(1960, 2020, 10),
    limits = c(1961, 2023)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%")
  ) +
  scale_colour_manual(values = mypal) +
  labs(
    x = NULL, y = "Yearly population change (%)",
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(
    base_family = "body_font",
    base_size = ts
  ) +
  theme(
    legend.position = "none",
    axis.line.y = element_line(
      arrow = arrow(ends = "both", length = unit(10, "mm")),
      linewidth = 1,
      colour = text_hil
    ),
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.5
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(
      hjust = 0,
      family = "title_font",
      size = 2.5 * ts,
      colour = text_hil,
      margin = margin(10, 0, 3,0, "mm")
    ),
    plot.subtitle = element_markdown(
      lineheight = 0.35,
      hjust = 0,
      colour = text_col,
      margin = margin(0,0,5,0, "mm")
    ),
    plot.caption = element_textbox(
      hjust = 0.5,
      family = "caption_font",
      colour = text_hil
    ),
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    axis.title.y = element_text(margin = margin(0,0,0,5, "mm"), colour = text_col),
    axis.text.y = element_text(
      margin = margin(0,0,0,0, "mm"), 
      colour = text_col,
      size = 1.2 * ts),
    axis.text.x = element_text(
      margin = margin(0,0,0,0, "mm"), 
      colour = text_col,
      size = 1.1 * ts),
    axis.ticks.y = element_blank()
  ) 

# QR Code for the plot
url_graphics <- paste0(
  "https://aditya-dahiya.github.io/projects_presentations/projects/",
  # The file name of the current .qmd file
  "world_population_animated_donut",         
  ".html"
)
# remotes::install_github('coolbutuseless/ggqr')
# library(ggqr)
plot_qr <- ggplot(
  data = NULL, 
  aes(x = 0, y = 0, label = url_graphics)
  ) + 
  ggqr::geom_qr(
    colour = text_hil, 
    fill = bg_col,
    size = 1.9
    ) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(
    fill = NA, 
    colour = NA
    )
  )

library(patchwork)
g <- g_base +
  # Add QR Code to the plot
  inset_element(
    p = plot_qr, 
    left = 0.85, 
    right = 1,
    bottom = 0.75,
    top = 0.90, 
    align_to = "full",
    clip = FALSE
  ) +
  
  # Basix Plot Annotations
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = bg_col, 
        colour = NA, 
        linewidth = 0
      )
    )
  )

ggsave(
  plot = g,
  filename = here::here("data_vizs", "a4_world_population_change.png"),
  height = 210 * 2,
  width = 297 * 2,
  units = "mm"
)
```
:::


# Decade by Decade: The World’s Population Story

![This bee-swarm scatterplot illustrates decadal population changes by country from 1960s to 2020s, showcasing the percentage growth or decline for each decade. It reveals rapid population expansions in Gulf countries during economic booms and notable declines in war-torn and Eastern European nations, with color-coded dots representing different continents. The plot underscores the increasing growth rates in Sub-Saharan Africa and the stagnation or decline in Europe and Latin America.](/data_vizs/a4_wb_decade_population.png)

The graphic is a beeswarm scatterplot illustrating the decadal population changes for each country from 1960 to 2020, based on data sourced from the official World Bank databank. This data, encompassing total population figures derived from midyear estimates, adheres to the de facto population definition, counting all residents irrespective of legal status or citizenship. The data originates from multiple reputable sources, including the United Nations Population Division, various national statistical offices, Eurostat, the U.S. Census Bureau, and the Secretariat of the Pacific Community. In the scatterplot, each column corresponds to a decade, with the y-axis representing the percentage change in population. Each dot signifies a country, color-coded by continent according to World Bank regions, effectively illustrating regional demographic trends over time. The plot also highlights and labels the most significant outliers, showcasing the three countries with the highest population increases and the two countries with the most substantial decreases for each decade, providing a clear visual representation of extreme demographic shifts across different periods.

### How I made this graphic?

Load libraries


::: {.cell}

```{.r .cell-code}
# Data Import and Wrangling Tools
library(tidyverse)            # All things tidy
library(janitor)              # Cleaning names etc.
library(wbstats)              # Fetching World Bank Data

# Final plot tools
library(scales)               # Nice Scales for ggplot2
library(fontawesome)          # Icons display in ggplot2
library(ggtext)               # Markdown text support for ggplot2
library(showtext)             # Display fonts in ggplot2
library(gganimate)            # For animation
```
:::


Data Wrangling


::: {.cell}

```{.r .cell-code}
rawdf2 <- wb_data(
  indicator = "SP.POP.TOTL",
  start_date = 1960,
  end_date = 2023,
  return_wide = FALSE,
  gapfill = TRUE,
  mrv = 65
)

region_levels <- c(
  "East Asia & Pacific",
  "Europe & Central Asia",
  "Sub-Saharan Africa",
  "Latin America & Caribbean",
  "South Asia",
  "Middle East & North Africa",
  "North America"
)
```
:::


Linking Countries to Regions / Continents


::: {.cell}

```{.r .cell-code}
library(wbstats)

link_regions <- wbstats::wb_countries() |> 
  select(iso2c, region) |> 
  mutate(iso2c = str_to_lower(iso2c))
```
:::


Visualization Parameters


::: {.cell}

```{.r .cell-code}
# Font for titles
font_add_google("Dosis",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Saira Semi Condensed",
  family = "body_font"
) 

showtext_auto()

# Background Colour
bg_col <- "white"
text_col <- "grey15"
text_hil <- "grey30"

mypal <- paletteer::paletteer_d("nbapalettes::nuggets_city2")

# Base Text Size
bts <- 20
```
:::


Exploratory Data Analysis


::: {.cell}

```{.r .cell-code}
df3 <- rawdf2 |> 
  select(country, iso2c, year = date, value) |> 
  mutate(iso2c = str_to_lower(iso2c)) |> 
  drop_na()

# Not plotting very small territories with population below 750,000
not_plot <- df3 |> 
  filter(year == 2022) |> 
  filter(value < 7.5e5) |> 
  pull(iso2c)

# A treemap to show how many coutnries lie within population of some
# of the most populous countries

# countries_to_visualize <- df3 |> 
#   filter(year == 2022) |> 
#   slice_max(order_by = value, n = 10) |> 
#   pull(iso2c)
# 
# too_small_countries <- df3 |> 
#   filter(year == 2022) |> 
#   filter(value < 1e5) |> 
#   pull(country)
# 
# df3 |> 
#   mutate(
#     country = if_else(
#       iso2c %in% countries_to_visualize,
#       country,
#       "Others"
#     )
#   ) |> 
#   group_by(year, country) |> 
#   summarize(value = sum(value, na.rm = TRUE)) |> 
#   ungroup() |> 
#   ggplot(aes(x = year, y = value, fill = country, group = country)) +
#   ggstream::geom_stream(type = "proportional", colour = "white")
# 
# 
# df3 |> 
#   visdat::vis_miss()
# 
# big_country <- "India"
# view_year <- 2023
# 
# total_pop_to_fill <- df3 |> 
#   filter(year == view_year & country == big_country) |> 
#   pull(value)
# 
# plotdf3 <- df3 |> 
#   filter(year == view_year) |> 
#   arrange(value) |> 
#   mutate(cumsum_value = cumsum(value)) |> 
#   filter(cumsum_value <= total_pop_to_fill)
#   
# plotdf3 |> 
#   ggplot(aes(area = value, fill = country)) +
#   treemapify::geom_treemap(colour = "white") +
#   labs(
#     title = paste0("The scale of ", big_country, "'s population"),
#     subtitle = paste0("Population of ", 
#                       big_country, 
#                       " in ", view_year, ": ",
#                       number(total_pop_to_fill, big.mark = ","),
#                       "\n",
#                       nrow(plotdf3), 
#                       " countries combined have lower population than ", 
#                       big_country)
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(hjust = 0.5)
#   )



# A new thought stream - decadal population rise
decade_levels <- c(
  "rise_overall",
  "rise60s",
  "rise70s",
  "rise80s",
  "rise90s",
  "rise00s",
  "rise10s"
)

# Decade wise population rise in each country
decade_df <- df3 |> 
  filter(!(iso2c %in% not_plot)) |> 
  mutate(year = paste0("y_", year)) |> 
  pivot_wider(
    id_cols = c(country, iso2c),
    names_from = year,
    values_from = value
  ) |> 
  mutate(
    rise_overall = (y_2023 - y_1961)/y_1961,
    rise60s = (y_1970 - y_1961)/y_1961,
    rise70s = (y_1980 - y_1971)/y_1971,
    rise80s = (y_1990 - y_1981)/y_1981,
    rise90s = (y_2000 - y_1991)/y_1991,
    rise00s = (y_2010 - y_2000)/y_2000,
    rise10s = (y_2020 - y_2010)/y_2010
  ) |> 
  select(country, iso2c, starts_with("rise")) |> 
  pivot_longer(
    cols = -c(country, iso2c),
    names_to = "decade",
    values_to = "value"
  ) |> 
  left_join(link_regions) |> 
  mutate(
    decade = fct(decade, levels = decade_levels),
    region = fct(region, levels = region_levels)
  )

# Selecting countries to highlight and annotate in the plot
# Top 3 high rise countries each decade
selcon1 <- decade_df |> 
  group_by(decade) |> 
  slice_max(order_by = value, n = 3) |> 
  ungroup()

# Bottom 2 countries in each decade
selcon2 <- decade_df |> 
  group_by(decade) |> 
  slice_min(order_by = value, n = 2) |> 
  ungroup()


# Overall 10 most populous countries
selcon3 <- rawdf2 |> 
  filter(date == 2022) |> 
  slice_max(order_by = value, n = 10) |> 
  mutate(iso2c = str_to_lower(iso2c)) |> 
  pull(iso2c)

selcons <- bind_rows(
  selcon1,
  selcon2
) |> 
  mutate(to_show_text = "display")

plotdf <- decade_df |> 
  left_join(selcons) |> 
  mutate(
    to_show_text = if_else(
      is.na(to_show_text),
      NA,
      paste0(country, " (", 
             number(round(100*value), suffix = "%"),
             ")")
    )
  )

# Decadal rise overall for the world
overall_decade_df <- df3 |> 
  group_by(year) |> 
  summarise(total_pop = sum(value, na.rm = T)) |> 
  mutate(
    country = "World",
    year = paste0("y_", year),
    
  ) |> 
  pivot_wider(
    id_cols = country,
    values_from = total_pop,
    names_from = year
  ) |> 
  mutate(
    rise60s = (y_1970 - y_1961)/y_1961,
    rise70s = (y_1980 - y_1971)/y_1971,
    rise80s = (y_1990 - y_1981)/y_1981,
    rise90s = (y_2000 - y_1991)/y_1991,
    rise00s = (y_2010 - y_2000)/y_2000,
    rise10s = (y_2020 - y_2010)/y_2010,
    .keep = "none"
  ) |> 
  pivot_longer(
    cols = everything(),
    names_to = "decade",
    values_to = "value"
  ) |> 
  mutate(decade = fct(decade, levels = decade_levels))
```
:::


Plot text annotations


::: {.cell}

```{.r .cell-code}
plot_title <- "Rising and Falling: Nations' Population in past 6 Decades"

plot_subtitle <- str_wrap("The beeswarm scatterplot of decadal population growth reveals that Gulf countries experienced the most rapid population expansions during the oil booms of the 1960s, 1970s, and economic boom of the 2000s. Population declines predominantly occurred in war-torn regions like Afghanistan (1980s) and Cambodia (1970s), with sharp declines also noted in Eastern Europe in the 2000s and 2010s, particularly in Bosnia Herzegovina, Latvia, and Lithuania. Additionally, Sub-Saharan African countries are showing increasing decadal population growth rates, while Europe and Latin American countries are experiencing stagnation or decline, often falling below the 0% growth line.", 170)

text1_annotate <- str_wrap("The population surges in the UAE, Qatar, and Kuwait during the 1960s, 1970s, and 2000s were driven primarily by the discovery and exploitation of vast oil reserves, leading to rapid economic growth and significant infrastructure development. This economic boom attracted a large influx of foreign workers and expatriates to support the burgeoning industries.", 40)

text2_annotate <- str_wrap("About the Data: The data for the plot is sourced from the official World Bank databank, encompassing total population figures from 1960 to 2023. These midyear estimates are based on the de facto definition of population, counting all residents regardless of legal status or citizenship. The data is compiled from multiple reputable sources, including the United Nations Population Division, national statistical offices, Eurostat, the U.S. Census Bureau, and the Secretariat of the Pacific Community, ensuring comprehensive and reliable demographic information.", 50)

# Caption stuff for the plot
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")


plot_caption <- paste0(
  "**Data:** World Bank Databank. |  ",
  "**Code:** ", 
  social_caption_1, 
  " |  **Graphics:** ", 
  social_caption_2
  )
```
:::


The actual base plot


::: {.cell}

```{.r .cell-code}
g_base <- plotdf |> 
  filter(decade != "rise_overall") |> 
  ggplot(
    mapping = aes(
      x = decade,
      y = value,
      label = country,
      colour = region
    )
  ) +
  
  # Beeswarm of the plot
  geom_point(
    pch = 19,
    alpha = 1,
    size = 5, 
    position = ggbeeswarm::position_beeswarm(
      method = "hex",
      cex = 2,
      corral.width = 0.9,
      corral = "wrap"
    )
  ) +
  # Displaying text country names
  ggrepel::geom_text_repel(
    mapping = aes(label = to_show_text),
    position = ggbeeswarm::position_beeswarm(
      method = "hex",
      cex = 2,
      corral.width = 0.9,
      corral = "wrap"
    ),
    lineheight = 0.35,
    colour = text_hil,
    hjust = 0,
    vjust = 0.5,
    family = "body_font",
    size = bts
  ) +
  
  # Horizontal Line Annotations
  geom_hline(
    yintercept = 1,
    colour = text_hil,
    linewidth = 0.5,
    alpha = 0.5
  ) +
  annotate(
    geom = "text",
    label = "100% rise -- population doubled in the decade",
    x = 7,
    y = 1.02,
    family = "caption_font",
    colour = text_hil,
    hjust = 1,
    vjust = 0,
    size = bts
  ) +
  geom_hline(
    yintercept = 2,
    colour = text_hil,
    linewidth = 0.5,
    alpha = 0.5
  ) +
  annotate(
    geom = "text",
    label = "200% rise -- population tripled in the decade",
    x = 7,
    y = 2.02,
    family = "caption_font",
    colour = text_hil,
    hjust = 1,
    vjust = 0,
    size = bts
  ) +
  geom_hline(
    yintercept = 0,
    colour = text_hil,
    linewidth = 0.5,
    alpha = 0.5
  ) +
  annotate(
    geom = "text",
    label = "Static Population",
    x = 7,
    y = 0.02,
    family = "caption_font",
    colour = text_hil,
    hjust = 1,
    vjust = 0,
    size = bts
  ) +
  annotate(
    geom = "text",
    x = 2.5,
    y = 1.9,
    label = text2_annotate,
    colour = text_hil,
    family = "caption_font",
    size = bts/1.5,
    lineheight = 0.3,
    hjust = 0,
    vjust = 1
  ) +
  
  # Labels
  labs(
    y = "Change in population (%)",
    x = NULL,
    colour = NULL,
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +

  # Scales and Coordinates
  scale_color_manual(values = mypal) +
  scale_y_continuous(
    labels = label_percent()
  ) +
  scale_x_discrete(
    labels = c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s"),
    expand = expansion(c(-0.05, 0.05))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(
    base_family = "body_font",
    base_size = 4 * bts
  ) +
  theme(
    legend.position = c(0.99, 0.92),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.justification = c(1, 1),
    legend.text = element_text(
      margin = margin(0,0,0,2, "mm"),
      colour = text_hil,
      family = "body_font",
      size = 4 * bts
    ),
    legend.key.height = unit(10, "mm"),
    plot.title = element_text(
      size = 10.5 * bts,
      colour = text_hil,
      family = "title_font",
      margin = margin(10,0,5,0, "mm")
    ),
    plot.subtitle = element_text(
      lineheight = 0.3,
      colour = text_col,
      size = 3 * bts,
      margin = margin(0,0,0,0, "mm")
    ),
    plot.caption = element_textbox(
      colour = text_hil,
      family = "caption_font",
      margin = margin(5,0,0,0, "mm"),
      hjust = 0.5
    ),
    panel.grid.major.y = element_line(
      linewidth = 0.5
    ),
    legend.background = element_rect(
      fill = bg_col,
      colour = "transparent"
    ),
    plot.title.position = "plot",
    axis.text.x = element_text(
      size = 5 * bts,
      colour = text_col,
      margin = margin(0,0,0,0, "mm")
    ),
    axis.text.y = element_text(
      margin = margin(0,0,0,0, "mm")
    ),
    axis.title = element_text(
      colour = text_hil,
      margin = margin(0,0,0,0, "mm")
    )
  )
```
:::


Add insets and save plot


::: {.cell}

```{.r .cell-code}
# QR Code for the plot
url_graphics <- paste0(
  "https://aditya-dahiya.github.io/projects_presentations/projects/",
  # The file name of the current .qmd file
  "world_population_animated_donut.html#decade-by-decade-the-worlds-population-story"
)
# remotes::install_github('coolbutuseless/ggqr')
# library(ggqr)
plot_qr <- ggplot(
  data = NULL, 
  aes(x = 0, y = 0, label = url_graphics)
  ) + 
  ggqr::geom_qr(
    colour = text_hil, 
    fill = bg_col,
    size = 2
    ) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(
    fill = NA, 
    colour = NA
    )
  )

library(patchwork)
g <- g_base +
  inset_element(
    p = plot_qr,
    left = 0.85, right = 0.99,
    top = 0.95, bottom = 0.8,
    align_to = "full"
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = "transparent",
        colour = "transparent"
      ),
      panel.background = element_rect(
        fill = "transparent",
        colour = "transparent"
      )
    )
  )

ggsave(
  filename = here::here("data_vizs", "a4_wb_decade_population.png"),
  plot = g,
  height = 210 * 2,
  width = 297 * 2,
  units = "mm",
  bg = bg_col
)
```
:::


# Interactive Graphic on Decadal Population Growth in Countries


::: {.cell}

```{.r .cell-code}
# Data Import and Wrangling Tools
library(tidyverse)            # All things tidy
library(janitor)              # Cleaning names etc.
library(wbstats)              # Fetching World Bank Data

# Final plot tools
library(scales)               # Nice Scales for ggplot2
library(fontawesome)          # Icons display in ggplot2
library(ggtext)               # Markdown text support for ggplot2
library(showtext)             # Display fonts in ggplot2
library(gganimate)            # For animation

# Loading the data
rawdf2 <- wb_data(
  indicator = "SP.POP.TOTL",
  start_date = 1960,
  end_date = 2023,
  return_wide = FALSE,
  gapfill = TRUE,
  mrv = 65
)

# Correct Order of regions for plotting in colours
region_levels <- c(
  "East Asia & Pacific",
  "Europe & Central Asia",
  "Sub-Saharan Africa",
  "Latin America & Caribbean",
  "South Asia",
  "Middle East & North Africa",
  "North America"
)

# Regions for the countries
link_regions <- wbstats::wb_countries() |> 
  select(iso2c, region) |> 
  mutate(iso2c = str_to_lower(iso2c))

# Font for titles
font_add_google("Dosis",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Saira Semi Condensed",
  family = "body_font"
) 

showtext_auto()

# Background Colour
bg_col <- "white"
text_col <- "grey15"
text_hil <- "grey30"

mypal <- paletteer::paletteer_d("nbapalettes::nuggets_city2")

# Base Text Size
bts <- 10

# Dataframe for this analysis
df3 <- rawdf2 |> 
  select(country, iso2c, year = date, value) |> 
  mutate(iso2c = str_to_lower(iso2c)) |> 
  drop_na()

# Not plotting very small territories with population below 750,000
not_plot <- df3 |> 
  filter(year == 2022) |> 
  filter(value < 7.5e5) |> 
  pull(iso2c)

# Decadal population rise
decade_levels <- c(
  "rise_overall",
  "rise60s",
  "rise70s",
  "rise80s",
  "rise90s",
  "rise00s",
  "rise10s"
)

# Decade wise population rise in each country
decade_df <- df3 |> 
  filter(!(iso2c %in% not_plot)) |> 
  mutate(year = paste0("y_", year)) |> 
  pivot_wider(
    id_cols = c(country, iso2c),
    names_from = year,
    values_from = value
  ) |> 
  mutate(
    rise_overall = (y_2023 - y_1961)/y_1961,
    rise60s = (y_1970 - y_1961)/y_1961,
    rise70s = (y_1980 - y_1971)/y_1971,
    rise80s = (y_1990 - y_1981)/y_1981,
    rise90s = (y_2000 - y_1991)/y_1991,
    rise00s = (y_2010 - y_2000)/y_2000,
    rise10s = (y_2020 - y_2010)/y_2010
  ) |> 
  select(country, iso2c, starts_with("rise")) |> 
  pivot_longer(
    cols = -c(country, iso2c),
    names_to = "decade",
    values_to = "value"
  ) |> 
  left_join(link_regions) |> 
  mutate(
    decade = fct(decade, levels = decade_levels),
    region = fct(region, levels = region_levels)
  )

# Selecting countries to highlight and annotate in the plot
# Top 3 high rise countries each decade
selcon1 <- decade_df |> 
  group_by(decade) |> 
  slice_max(order_by = value, n = 3) |> 
  ungroup()

# Bottom 2 countries in each decade
selcon2 <- decade_df |> 
  group_by(decade) |> 
  slice_min(order_by = value, n = 2) |> 
  ungroup()


# Overall 10 most populous countries
selcon3 <- rawdf2 |> 
  filter(date == 2022) |> 
  slice_max(order_by = value, n = 10) |> 
  mutate(iso2c = str_to_lower(iso2c)) |> 
  pull(iso2c)

selcons <- bind_rows(
  selcon1,
  selcon2
) |> 
  mutate(to_show_text = "display")

plotdf <- decade_df |> 
  left_join(selcons) |> 
  mutate(
    to_show_text = if_else(
      is.na(to_show_text),
      NA,
      paste0(country, " (", 
             number(round(100*value), suffix = "%"),
             ")")
    )
  ) |> 
  mutate(id_var = row_number())

# Decadal rise overall for the world
overall_decade_df <- df3 |> 
  group_by(year) |> 
  summarise(total_pop = sum(value, na.rm = T)) |> 
  mutate(
    country = "World",
    year = paste0("y_", year),
    
  ) |> 
  pivot_wider(
    id_cols = country,
    values_from = total_pop,
    names_from = year
  ) |> 
  mutate(
    rise60s = (y_1970 - y_1961)/y_1961,
    rise70s = (y_1980 - y_1971)/y_1971,
    rise80s = (y_1990 - y_1981)/y_1981,
    rise90s = (y_2000 - y_1991)/y_1991,
    rise00s = (y_2010 - y_2000)/y_2000,
    rise10s = (y_2020 - y_2010)/y_2010,
    .keep = "none"
  ) |> 
  pivot_longer(
    cols = everything(),
    names_to = "decade",
    values_to = "value"
  ) |> 
  mutate(decade = fct(decade, levels = decade_levels))

plot_title <- "Rising and Falling: Nations' Population in past 6 Decades"

# Caption stuff for the plot
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")


plot_caption <- paste0(
  "**Data:** World Bank Databank. |  ",
  "**Code:** ", 
  social_caption_1, 
  " |  **Graphics:** ", 
  social_caption_2
  )

# Making an alternative for ggbeeswarm for interactivity
plotdf0 <- plotdf |> 
  mutate(y_value = round_to_fraction(value, 40))
  
plotdf1 <- plotdf |> 
  mutate(y_value = round_to_fraction(value, 40)) |> 
  group_by(decade, y_value) |> 
  slice_head(n = 10) |> 
  count() |> 
  rename(gp_nos = n)

# A position multiplication Factor (to manually create a beeswarm)
position_vector <- seq(-0.4, +0.4, length.out = 10)

plotdf2 <- plotdf0 |> 
  left_join(plotdf1) |> 
  group_by(decade, y_value) |> 
  arrange(decade, y_value) |> 
  mutate(group_num = row_number()) |> 
  mutate(y_jizz = case_when(
    group_num ==  1 ~ -0.04444444,
    group_num ==  2 ~ +0.04444444,
    group_num ==  3 ~ -0.13333333,
    group_num ==  4 ~ +0.13333333,
    group_num ==  5 ~ -0.22222222,
    group_num ==  6 ~ +0.22222222,
    group_num ==  7 ~ -0.31111111,
    group_num ==  8 ~ +0.31111111,
    group_num ==  9 ~ -0.40000000,
    group_num == 10 ~ +0.40000000
  )) |> 
  mutate(y_var = as.numeric(decade) + y_jizz)

bts = 9

library(ggiraph)

g_base <- plotdf2 |> 
  filter(decade != "rise_overall") |> 
  ggplot(
    mapping = aes(
      x = y_var,
      y = y_value,
      label = country,
      colour = region,
      data_id = iso2c
    )
  ) +
  
  # Beeswarm of the plot
  ggiraph::geom_point_interactive(
    mapping = aes(
      tooltip = paste0(
        "Country: ",
        country,
        "\nRegion:",
        region,
        "\nDecadal Population Change: ",
        round(value*100, 1), " %"
      )
    ),
    pch = 19,
    alpha = 1,
    size = 1.5, 
    hover_nearest = FALSE
  ) +
  
  # Horizontal Line Annotations
  geom_hline(
    yintercept = 1,
    colour = text_hil,
    linewidth = 0.5,
    alpha = 0.5
  ) +
  annotate(
    geom = "text",
    label = "100% rise -- population doubled in the decade",
    x = 8.1,
    y = 1.02,
    family = "caption_font",
    colour = text_hil,
    hjust = 1,
    vjust = 0,
    size = bts/2
  ) +
  geom_hline(
    yintercept = 2,
    colour = text_hil,
    linewidth = 0.5,
    alpha = 0.5
  ) +
  annotate(
    geom = "text",
    label = "200% rise -- population tripled in the decade",
    x = 8.1,
    y = 2.02,
    family = "caption_font",
    colour = text_hil,
    hjust = 1,
    vjust = 0,
    size = bts/2
  ) +
  geom_hline(
    yintercept = 0,
    colour = text_hil,
    linewidth = 0.5,
    alpha = 0.5
  ) +
  annotate(
    geom = "text",
    label = "Static Population",
    x = 8.1,
    y = 0.02,
    family = "caption_font",
    colour = text_hil,
    hjust = 1,
    vjust = 0,
    size = bts/2
  ) +
  
  # Labels
  labs(
    y = "Change in population (%)",
    x = NULL,
    colour = NULL,
    title = plot_title,
    caption = plot_caption,
    subtitle = "Hover over a dot to see Country details. Hover here to read about the Data Source."
  ) +

  # Scales and Coordinates
  scale_color_manual_interactive(
    values = mypal,
    data_id = function(x) x, 
    tooltip = function(x) x
    ) +
  scale_y_continuous(
    labels = label_percent()
  ) +
  scale_x_continuous(
    breaks = 1:7,
    labels = c("", "1960s", "1970s", "1980s", 
               "1990s", "2000s", "2010s"),
    limits = c(1.25, 8.15),
    expand = expansion(0)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(
    base_family = "body_font",
    base_size = bts
  ) +
  theme(
    legend.position = c(0.99, 0.92),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.justification = c(1, 1),
    legend.text = element_text(
      colour = text_hil,
      family = "body_font",
      size = 1.5 * bts
    ),
    legend.key.height = unit(2, "mm"),
    plot.title = element_text(
      size = 3 * bts,
      colour = text_hil,
      family = "title_font"
    ),
    plot.subtitle = element_text_interactive(
      size = 1.5 * bts,
      colour = text_hil,
      family = "title_font",
      data_id = "plot.subtitle",
      tooltip = "About the Data: The data for the plot is sourced from the official World Bank databank, encompassing total population figures from 1960 to 2023. These midyear estimates are based on the de facto definition of population, counting all residents regardless of legal status or citizenship. The data is compiled from multiple reputable sources, including the United Nations Population Division, national statistical offices, Eurostat, the U.S. Census Bureau, and the Secretariat of the Pacific Community, ensuring comprehensive and reliable demographic information."
    ),
    plot.caption = element_textbox(
      colour = text_hil,
      family = "caption_font",
      hjust = 0.5,
      size = 1.5 * bts
    ),
    panel.grid.major.y = element_line(
      linewidth = 0.5
    ),
    legend.background = element_rect(
      fill = bg_col,
      colour = "transparent"
    ),
    plot.title.position = "plot",
    axis.text.x = element_text(
      size = 2 * bts,
      colour = text_hil,
      margin = margin(0,0,0,0, "mm")
    ),
    axis.text.y = element_text(
      margin = margin(0,0,0,0, "mm"),
      size = 1.5 * bts
    ),
    axis.title = element_text(
      colour = text_hil,
      margin = margin(0,0,0,0, "mm"),
      size = 1.5 * bts
    )
  )

girafe(
  ggobj = g_base,
  options = list(
    opts_tooltip(
      opacity = 1,
      css = "background-color:#ffffff;color:#333333;padding:2px;border-radius:3px;font-family:Arial"
    ),
    opts_hover(
      css = "stroke:black;stroke-width:3px;"),
    opts_hover_inv(css = "opacity:0.2;"),
    opts_zoom(max = 10)
  )
)
```

::: {#fig-interact-plot3 .cell-output-display}

```{=html}
<div class="girafe html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-3817a02dac15a5614bf7" style="width:100%;height:650px;"></div>
```


The beeswarm scatterplot of decadal population growth reveals that Gulf countries experienced the most rapid population expansions during the oil booms of the 1960s, 1970s, and economic boom of the 2000s. Population declines predominantly occurred in war-torn regions like Afghanistan (1980s) and Cambodia (1970s), with sharp declines also noted in Eastern Europe in the 2000s and 2010s, particularly in Bosnia Herzegovina, Latvia, and Lithuania. Additionally, Sub-Saharan African countries are showing increasing decadal population growth rates, while Europe and Latin American countries are experiencing stagnation or decline, often falling below the 0% growth line.
:::
:::


**About the Data:** The data for the plot is sourced from the official World Bank data-bank, encompassing total population figures from 1960 to 2023. These midyear estimates are based on the de-facto definition of population, counting all residents regardless of legal status or citizenship. The data is compiled from multiple reputable sources, including the United Nations Population Division, national statistical offices, Eurostat, the U.S. Census Bureau, and the Secretariat of the Pacific Community, ensuring comprehensive and reliable demographic information.
