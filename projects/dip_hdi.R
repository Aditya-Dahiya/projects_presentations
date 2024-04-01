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

# Another attempt at World Map with changes and improvements by shades
mapdf <- dfwide |> 
  select(-region) |> 
  pivot_longer(
    cols = -c(iso3, country),
    names_to = "year",
    values_to = "hdi"
  ) |> 
  mutate(year = parse_number(year)) |> 
  tidyr::fill(hdi, .direction = "updown") |> 
  pivot_wider(
    id_cols = c(iso3, country),
    names_from = year,
    names_prefix = "year_",
    values_from = hdi
  ) |> 
  mutate(
    improvement = year_2022 - year_1990,
    p_change = improvement / year_1990
    ) |> 
  select(iso3, country, improvement, p_change) |> 
  rename(iso_a3 = iso3)

library(rnaturalearth)

mapdata <- ne_countries(returnclass = "sf") |> 
  filter(!(sovereignt %in% c("Antarctica"))) |> 
  mutate(iso_a3 = case_when(
    iso_a3 == "FRA" ~ "NOR",
    iso_a3 == "-99" ~ "FRA",
    .default = iso_a3
  ))

mapdata |> 
  left_join(mapdf) |> 
  ggplot() +
  geom_sf(aes(fill = improvement)) +
  paletteer::scale_fill_paletteer_c("ggthemes::Temperature Diverging",
                                    direction = -1)
  scale_fill_gradient2(
    mid = bg_col,
    low = "#BE2A3EFF",
    high = "#22763FFF",
    na.value = bg_col,
    midpoint = 0
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


# Note this code picks up from the previous chunk of code


# =============================================================================#
# Animation: Female Schooling vs. Female Political Participation ---------------
# =============================================================================#

# Another EDA: Schooling years of females vs. Share of Seats in parliament held by women
df3 <- hdi |> 
  select(country, region, contains("mys_f_"), (contains("pr_f_") & !contains("lfpr_f"))) |> 
  pivot_longer(
    cols = -c(country, region),
    names_to = "var_full",
    values_to = "value"
  ) |> 
  separate_wider_delim(
    cols = var_full,
    delim = "_",
    names = c("indicator", NA, "year")
  ) |> 
  mutate(
    year = as.numeric(year),
    indicator = case_when(
      indicator == "mys" ~ "mean_yrs_school",
      indicator == "pr" ~ "parl_rep",
      .default = NA
    )
  ) |> 
  pivot_wider(
    id_cols = c(country, year, region),
    names_from = indicator,
    values_from = value
  )

###### Start Making animation: Parameters ######  
library(gganimate)

palcol2 <- "grey"
mypal1 <- paletteer::paletteer_d("ggthemes::excel_Ion_Boardroom")
font_add_google("Dosis", family = "anim_font")
font_add_google("Saira Extra Condensed", family = "caption_font") 

showtext_auto()

regions <- c(
  "Arab States",
  "East Asia & Pacific",
  "Europe and Central Asia",
  "Latin America & Caribbean",
  "South Asia",
  "Sub-Saharan Africa"
)
abbr_regions <- c("AS", "EAP", "ECA", "LAC", "SA", "SSA")

strip_region_labels <- paste0(
  "<b style='color:",
  mypal1,
  "'>", 
  regions,
  "</b>"
)
names(strip_region_labels) <- abbr_regions

simple_labels <- regions
names(simple_labels) <- abbr_regions

##### The Plot to animate ########

ganim <- ggplot(
  data = df3 |> filter(!is.na(region)),
  mapping = aes(
    x = mean_yrs_school,
    y = parl_rep,
    colour = region
  )
) +
  geom_point(
    aes(group = country),
    size = 4
  ) +
  geom_smooth(
    method = "lm", 
    se = FALSE,
    colour = "darkgrey",
    linewidth = 1
  ) +
  facet_wrap(~ region,labeller = labeller(region = simple_labels)) +
  scale_x_continuous(
    limits = c(0, 14),
    breaks = seq(0, 12, 4),
    expand = expansion(0)
  ) +
  scale_y_continuous(
    limits = c(0, 55),
    breaks = seq(0, 50, 10),
    labels = label_number(suffix = " %"),
    expand = expansion(c(0.05, 0))
  ) +
  labs(
    title = "Female Schooling vs. Political Representation: Year {frame_time}",
    subtitle = "Each dot represents a Country over time",
    y = "Females' Representation in Parliament",
    x = "Mean years of Schooling for females",
    caption = "Data: UNDP  |  Code: (GitHub) @aditya-dahiya"
  ) +
  scale_color_manual(values = mypal1) +
  theme_bw(
    base_family = "anim_font",
    base_size = 16
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linewidth = 0.5,
      linetype = 2,
      colour = "lightgrey"
    ),
    plot.title = element_text(
      face = "bold",
      family = "anim_font",
      hjust = 0.5,
      size = 21
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    legend.position = "none",
    plot.caption = element_text(
      hjust = 1, 
      family = "caption_font"
    ),
    strip.text = element_text(
      family = "caption_font",
      face = "bold",
      colour = "black",
      size = 16, 
      margin = margin(10, 0, 10, 0, "pt")
    ),
    strip.background = element_rect(
      fill = "transparent"
    ),
    axis.text = element_text(
      colour = "black"
    ),
    axis.title = element_text(
      margin = margin(5, 5, 5, 5, "pt")
    )
  ) +
  transition_time(as.integer(year)) +
  ease_aes('linear') 

anim_save(
  filename = "dip_hdi_anim.gif",
  animation = ganim,
  path = here("docs"),
  nframes = 200,
  end_pause = 10,
  start_pause = 2,
  width = 720,
  height = 750
  
)


# =============================================================================#
# Animation: Gross National Income vs. CO2 emissions per capita ----------------
# =============================================================================#

df4 <- hdi |>
  mutate(country = if_else(country == "T\xfcrkiye", "Turkiye", country),
         country = if_else(country == "Syrian Arab Republic", "Syria", country),
         country = if_else(country == "'C<f4>te d'Ivoire'", "Ivory Coast", country)
         ) |> 
  select(
    iso3,
    country,
    contains("co2_prod"),
    contains("gnipc")
  ) |> 
  pivot_longer(
    cols = -c(iso3, country),
    names_to = "variable",
    values_to = "value"
  ) |> 
  separate_wider_delim(
    cols = variable,
    delim = "_",
    names = c(NA, "variable", "year"),
    too_few = "align_end"
  ) |> 
  mutate(variable = if_else(variable == "prod", "co2prod", variable)) |> 
  fill(value, .direction = "downup")

# Code to find out which countries have changed the most
# library(ggiraph)
# tempg <- df4 |>
#   mutate(com_var = paste0(variable, "_", year), .keep = "unused") |>
#   pivot_wider(
#     id_cols = c(iso3, country),
#     names_from = com_var,
#     values_from = value
#   ) |>
#   mutate(
#     iso3 = iso3,
#     country = country,
#     change_co2 = co2prod_2022 - co2prod_1990,
#     change_inc = gnipc_2022 - gnipc_1990,
#     .keep = "none"
#   ) |>
#   filter(change_co2 > -100) |>
#   ggplot(aes(
#     x = change_inc,
#     y = change_co2,
#     tooltip = iso3,
#     data_id = iso3)) +
#   geom_point_interactive()
# girafe(tempg)
# Selected countries to view
view_iso3 <- c("QAT", "IRL", "LUX", "ARE", "USA", "CHN", "IND", "BRN")
icon_code <- c("qa", "ie", "lu", "ae", "us", "cn", "in", "bn" )

temp1 <- tibble(
  iso3 = view_iso3,
  iso2 = icon_code
  )


# Installing ggflags package for flag icons
# install.packages("ggflags", repos = c(
#   "https://jimjam-slam.r-universe.dev",
#   "https://cloud.r-project.org"))

library(ggflags)
library(gganimate)

font_add_google("Barlow Semi Condensed", "flag_font")
font_add_google("Saira Extra Condensed", "caption_font")
showtext_auto()

subtitle_text <- "Comparison of effect of rising incomes on carbon dioxide emissions in different countries.\nWhile Ireland and Luxembourg have grown per capita incomes without increasing emissions, Qatar & Brunei have massively rising emissions, without commesurate per capita income rise."
subtitle_anim <- str_wrap(subtitle_text, 40)
str_view(subtitle_anim)

gfanim <- df4 |> 
  filter(iso3 %in% view_iso3) |> 
  pivot_wider(
    id_cols = c(iso3, country, year),
    names_from = variable,
    values_from = value
  ) |>
  left_join(temp1) |> 
  ggplot(
    aes(
      x = gnipc,
      y = co2prod,
      country = iso2,
      label = country
    )
  ) +
  ggflags::geom_flag(
    size = 20
  ) +
  geom_text(
    aes(y = co2prod + 5),
    family = "flag_font",
    size = 5
  ) +
  annotate(
    geom = "text",
    x = 0, 
    y = 80,
    label = subtitle_anim,
    family = "flag_font",
    size = 6,
    hjust = 0,
    vjust = 1,
    colour = "grey30",
    lineheight = 1.5
  ) +
  scale_x_continuous(
    expand = expansion(c(0.02, 0)),
    labels = label_number(
      prefix = "$",
      scale_cut = cut_short_scale()
    )
  ) +
  scale_y_continuous(
    expand = expansion(c(0.02, 0.05)),
    breaks = seq(0, 75, 15)
  ) +
  labs(
    y = "Annual carbon dioxide emissions, per capita (in Tonnes)",
    x = "Gross National Income, per capita (in 2017 PPP US $)",
    title = "Emissions & Incomes: {frame_time}",
    caption = "Data: UNDP  |  Code: (GitHub) @aditya-dahiya"
  ) +
  theme_classic(
    base_size = 16, 
    base_family = "flag_font"
    ) +
  theme(
    plot.title = element_text(
      hjust = 0,
      face = "bold",
      size = 42, 
      colour = "grey30"
    ),
    plot.caption = element_text(
      family = "caption_font",
      colour = "grey20"),
    axis.title = element_text(
      colour = "grey20"
    ),
    axis.line = element_line(
      linewidth = 0.5,
      arrow = arrow(length = unit(0.25, "cm")),
      colour = "grey50"
    ),
    axis.ticks = element_line(linewidth = 0.15, colour = "grey50"),
    axis.ticks.length = unit(0.5, "cm"),
    plot.title.position = "panel",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "lightgrey",
      linewidth = 0.25,
      linetype = "dotted"
    )
  ) +
  transition_time(as.integer(year)) +
  ease_aes('linear')

anim_save(
  filename = "hdi_anim_flags.gif",
  path = here("docs"),
  animation = gfanim,
  duration = 20,
  fps = 12,
  start_pause = 5,
  end_pause = 20,
  height = 800,
  width = 700,
  units = "px"
  )  


# A revised attempt to make the animation smoother, and not jagged by using
# extrapolations in the middle years

# Do loess by each group, i.e. country
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

df5 <- df4 |> 
  filter(iso3 %in% view_iso3) |> 
  pivot_wider(
    id_cols = c(iso3, country, year),
    names_from = variable,
    values_from = value
  ) |>
  group_by(country) |> 
  nest(data = c(co2prod, gnipc, year)) |> 
  mutate(
    # Perform loess for CO2 production on each Country as a group
    m1 = purrr::map(data, loess,
                   formula = co2prod ~ year, span = .9),
    # Retrieve the fitted values from each model
    est_co2prod = purrr::map(m1, `[[`, "fitted"),
    # Perform loess for GNI Income on each Country as a group
    m2 = purrr::map(data, loess,
                    formula = gnipc ~ year, span = .9),
    # Retrieve the fitted values from each model
    est_gnipc = purrr::map(m1, `[[`, "fitted")
  ) |> 
  dplyr::select(-m1, -m2) |> 
  tidyr::unnest(cols = c(data, est_co2prod, est_gnipc)) |> 
  left_join(temp1)


gfanim <- df5 |> 
  ggplot(
    aes(
      x = est_gnipc,
      y = est_co2prod,
      country = iso2,
      label = country
    )
  ) +
  ggflags::geom_flag(
    size = 20
  ) +
  geom_text(
    aes(y = est_co2prod + 5),
    family = "flag_font",
    size = 5
  ) +
  annotate(
    geom = "text",
    x = 0, 
    y = 80,
    label = subtitle_anim,
    family = "flag_font",
    size = 6,
    hjust = 0,
    vjust = 1,
    colour = "grey30",
    lineheight = 1.5
  ) +
  scale_x_continuous(
    expand = expansion(c(0.02, 0)),
    labels = label_number(
      prefix = "$",
      scale_cut = cut_short_scale()
    )
  ) +
  scale_y_continuous(
    expand = expansion(c(0.02, 0.05)),
    breaks = seq(0, 75, 15)
  ) +
  labs(
    y = "Annual carbon dioxide emissions, per capita (in Tonnes)",
    x = "Gross National Income, per capita (in 2017 PPP US $)",
    title = "Emissions & Incomes: {frame_time}",
    caption = "Data: LOESS Smoothed estimates from UNDP  |  Code: (GitHub) @aditya-dahiya"
  ) +
  theme_classic(
    base_size = 16, 
    base_family = "flag_font"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0,
      face = "bold",
      size = 42, 
      colour = "grey30"
    ),
    plot.caption = element_text(
      family = "caption_font",
      colour = "grey20"),
    axis.title = element_text(
      colour = "grey20"
    ),
    axis.line = element_line(
      linewidth = 0.5,
      arrow = arrow(length = unit(0.25, "cm")),
      colour = "grey50"
    ),
    axis.ticks = element_line(linewidth = 0.15, colour = "grey50"),
    axis.ticks.length = unit(0.5, "cm"),
    plot.title.position = "panel",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "lightgrey",
      linewidth = 0.25,
      linetype = "dotted"
    )
  ) +
  transition_time(as.integer(year)) +
  ease_aes('linear')

anim_save(
  filename = "hdi_anim_flags.gif",
  path = here("docs"),
  animation = gfanim,
  duration = 20,
  fps = 12,
  start_pause = 5,
  end_pause = 20,
  height = 700,
  width = 700,
  units = "px"
)  
