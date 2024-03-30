# =============================================================================#
# About the Dataset-------------------------------------------------------------
# =============================================================================#

# Source URL: https://hdr.undp.org/data-center/documentation-and-downloads
# Credits: UNDP Human Development Reports
# Human development, quantified. One of the most widely recognized measures, 
# the United Nations' Human Development Index amalgamates data on life 
# expectancy, per capita income, and educational attainment into a singular 
# value for each country-year. The UN offers downloadable files and an API 
# encompassing all yearly HDI rankings and sub-indicators spanning from 1990 to
# 2022. These resources also encompass information from correlated indices like 
# the Inequality-adjusted Human Development Index, Gender Development Index, and 
# Gender Inequality Index.

# =============================================================================#
# Findings ---------------------------------------------------------------------
# =============================================================================#

# The best performing countries, in terms of HDI improvement between 1990 and 
# 2022 are: China, Myanmar, Bangladesh, Turkiye, and, Morocco
# The wrost performing countries, in terms of HDI reduction / least increase
# between 1990 and 2022 are: Syria, Ukraine, Namibia, Libya, and, San Marino

# =============================================================================#
# Library Load-in---------------------------------------------------------------
# =============================================================================#

# Data Wrangling Tools
library(tidyverse)
library(janitor)
library(here)
library(sf)

# Final plot (ggplot2) tools
library(scales)
library(fontawesome)
library(ggtext)
library(showtext)
library(colorspace)


# =============================================================================#
# Data Load-in, EDA & Data Wrangling--------------------------------------------
# =============================================================================#

# Read in the UNDP data
url <- "https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv"

hdi <- read_csv(url)

# Number of countries to highlight in each
nos <- 5

# A wide format and housekeeping to filter out relevant variables, and also
# change names of some countries to easily recognizable names
dfwide <- hdi |> 
  select(iso3, 
         country, 
         region, 
         (contains("hdi_") & !(contains("_m_")) & !(contains("_f_")) & !(contains("ihdi")) & !(contains("phdi")) & !(contains("rank"))
          )
         ) |> 
  filter(!(country %in% c("East Asia and the Pacific"))) |> 
  mutate(country = if_else(country == "T\xfcrkiye", "Turkiye", country),
         country = if_else(country == "Syrian Arab Republic", "Syria", country))

# Long (tidy) format of the data (needed for plotting facets in ggplot2)
df1 <- dfwide |> 
  pivot_longer(
    cols = contains("hdi"),
    names_to = "year",
    values_to = "value"
  ) |> 
  mutate(year = parse_number(year))

# Improvement amongst countries
df_imp <- dfwide |> 
  mutate(improvement = hdi_2022 - hdi_1990) |> 
  select(country, improvement)

# Worst off countries
least_imp <- df_imp |> 
  slice_min(order_by = improvement, n = nos) |> 
  pull(country)

# Best improvement countries
most_imp <- df_imp |> 
  slice_max(order_by = improvement, n = nos) |> 
  pull(country)

least_imp
most_imp
# A tibble for actual percentage change in HDI - to show in graph
changes <- bind_rows(
  df_imp |> 
    slice_min(order_by = improvement, n = nos),
  df_imp |> 
    slice_max(order_by = improvement, n = nos)
) |> 
  mutate(
    improvement = round(100 * improvement, 1)
  )

# Tibble to actually use in plotting
plotdf <- df1 |> 
  mutate(
    most_improved = if_else(country %in% most_imp, country, NA),
    least_improved= if_else(country %in% least_imp, country, NA)
  ) |> 
  pivot_longer(
    cols = c(most_improved, least_improved),
    names_to = "facet_var",
    values_to = "colour_var"
  ) |> 
  mutate(colour_var = fct(colour_var, levels = c(least_imp, most_imp))) |> 
  left_join(changes)
# =============================================================================#
# Options & Visualization Parameters--------------------------------------------
# =============================================================================#

# Load fonts
# Font for titles
font_add_google("Racing Sans One",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Jockey One",
  family = "body_font"
) 

showtext_auto()

# Define colours
reds <- paletteer::paletteer_d("RColorBrewer::Reds", direction = -1)[1:nos]
greens <- paletteer::paletteer_d("RColorBrewer::Greens", direction = -1)[1:nos]
mypal <- c(reds, greens)


bg_col <- "#ffffff"   # Background Colour
text_col <- "#404040" # Colour for the text
text_hil <- '#757575' # Colour for highlighted text

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
plot_title <- "Human Development Index\n(Changes: 1990 - 2022)"
plot_caption <- paste0("**Data:** UNDP Human Development Reports", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- "Since the 1990s, HDI has been used for gauging development. While most nations have seen an enhancement in living standards, the pace of improvement varies. This graph illustrates both the top and bottom performers (% change from 1990 to 2022 in brackets)."
plot_subtitle <- str_wrap(subtitle_text, 90)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

strip_text <- c(
  "<b style='color:#CB181DFF'>Least Improvement</b>", 
  "<b style='color:#006D2CFF'>Highest improvement</b>"
  )
names(strip_text) <- c("least_improved", "most_improved")

g <- plotdf |> 
  ggplot(
    aes(
      x = year,
      y = value,
      group = country,
      color = colour_var,
      alpha = is.na(colour_var),
      linewidth = is.na(colour_var)
    )
  ) +
  geom_line() +
  ggrepel::geom_text_repel(
    data = (plotdf |> filter(year == 2022)),
    mapping = aes(
      label = if_else(
        !is.na(colour_var),
        paste0(colour_var, "\n", improvement, " %"),
        NA
      )
    ),
    force        = 10,
    nudge_x      = 2.5,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2,
    size = 30,
    lineheight = 0.25,
    family = "caption_font",
    fontface = "bold",
    box.padding = 0
  ) +
  facet_wrap(
    ~ facet_var,
    labeller = labeller(facet_var = strip_text)
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.35)),
    breaks = seq(1990, 2022, 8)
  ) +
  scale_y_continuous(
    expand = expansion(0)
  ) +
  scale_alpha_discrete(range = c(1, 0.5)) +
  scale_linewidth_discrete(range = c(2, 0.5)) +
  scale_colour_manual(
    values = mypal,
    na.value = "lightgrey"
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    x = NULL,
    y = "H.D.I  (Human Development Index)"
  ) +
  theme_minimal(
     base_family = "body_font"
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.5,
      colour = "transparent"
    ),
    panel.grid.major.x = element_line(
      linewidth = 0.5,
      linetype = 2,
      colour = text_hil
    ),
    axis.line.x = element_line(
      colour = text_hil,
      linewidth = 0.5
    ),
    axis.line.y = element_line(
      colour = text_hil,
      linewidth = 0.5,
      arrow = arrow(length = unit(0.4, "cm"))
    ),
    plot.title.position = "plot",
    plot.title = element_text(
      colour = text_hil,
      hjust = 0.5,
      family = "title_font",
      size = 12 * ts,
      margin = margin(2,0,0.25,0, "cm"),
      lineheight = 0.25
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      lineheight = 0.3,
      colour = text_hil,
      size = 5 * ts,
      margin = margin(0,0,1,0, "cm")
    ),
    axis.text = element_text(
      colour = text_col,
      hjust = 0.5,
      margin = margin(0,0,0,0),
      size = 4 * ts
    ),
    axis.title = element_text(
      colour = text_col,
      hjust = 0.5,
      margin = margin(0,0,0,0),
      size = 4 * ts
    ),
    plot.caption = element_textbox(
      family = "caption_font",
      colour = text_hil,
      size = 3 * ts,
      hjust = 0.5,
      margin = margin(0.5,0,0.8,0, "cm")
    ),
    strip.text = element_markdown(
      family = "body_font",
      size = 7 * ts,
      margin = margin(0,0,0.5,0, "cm"),
      hjust = 0
    )
  )


# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "dip_hdi.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = bg_col
)

# =============================================================================#
# Additional impact of Wars ------------------------------------------------------
# =============================================================================#

dfwide

ledata <- hdi |> 
  select(
    iso3,
    country,
    contains("ineq_le"),
    (contains("le_") & !(contains("le_m")))
  ) |> 
  select(!contains("le_f")) |> 
  pivot_longer(
    cols = contains("le_"),
    names_to = "variable",
    values_to = "value"
  ) |> 
  separate_wider_delim(
    cols = variable,
    delim = "_",
    too_few = "align_end",
    names = c("var1", "var2", "year")
  ) |> 
  mutate(
    year = parse_number(year),
    variable = if_else(
      is.na(var1),
      "life_expectancy",
      "inequality_le"
    )
  ) |> 
  select(-var1, -var2) |> 
  filter(!(country %in% c("East Asia and the Pacific"))) |> 
  mutate(country = if_else(country == "T\xfcrkiye", "Turkiye", country),
         country = if_else(country == "Syrian Arab Republic", "Syria", country))



select_countries <- c(
  "Ukraine",
  "Iraq",
  "Syria",
  "Libya"
)

# A coefficient to multiply by
coeff <- 5
colpal <- c("blue", "darkgreen")

ledata |> 
  filter(country %in% select_countries) |> 
  filter(year >= 2010) |> 
  pivot_wider(
    id_cols = c(iso3, country, year),
    names_from = variable,
    values_from = value
  ) |> 
  ggplot(aes(x = year)) +
  geom_line(aes(y = life_expectancy), colour = colpal[1]) +
  geom_point(aes(y = life_expectancy), colour = colpal[1]) +
  geom_line(aes(y = inequality_le * coeff), colour = colpal[2]) +
  geom_point(aes(y = inequality_le * coeff), colour = colpal[2]) +
  facet_wrap(
    ~ country, 
    scales = "free",
    ncol = 2
    ) +
  scale_x_continuous(
    breaks = seq(2010, 2022, 4)
  ) +
  scale_y_continuous(
    name = "Life Expectancy (in years)",
    sec.axis = sec_axis(
      name = "Inequality in Life Expectancy",
      transform = ~. * coeff
    )
  )
