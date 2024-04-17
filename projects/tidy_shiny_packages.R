#======================= #TidyTuesday : Shiny Packages ========================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(janitor)        # Cleaning names

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(scales)         # Scale Labels in ggplot2
library(treemapify)     # Tree-Map in the ggplot2
library(colorspace)     # To lighten and darken colours
library(magick)         # Editing images

# Other optional libraries
library(glue)           # To paste together text for ggtext
#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
# install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2024, week = 16)

shiny_revdeps <- tuesdata$shiny_revdeps
package_details <- tuesdata$package_details
# rm(tuesdata)
shiny_revdeps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-16/shiny_revdeps.csv')

shiny_logo_url <- "https://cache.sessionize.com/image/d4a2-1140o400o3-73VQ929AGYNKiVSALpF67z.png"

#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Changa", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Nova Mono", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Colour palette
mypal <- paletteer::paletteer_d("RColorBrewer::Pastel2")
fillpal <- mypal
colpal <- mypal |> darken(0.5)
mypal <- mypal |> darken(0.5)

# Define colours
text_col <- "grey5"               # Colour for the text
text_hil <- "grey25"               # Colour for highlighted text
bg_col <- "white"                    # Background Colour

# Define Text Size
ts =  90             # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_hil}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_hil}'>{xtwitter_username}</span>")

# Add text to plot
plot_title <- "<img src='temp.png' width='600'/>"
subtitle_text <- glue::glue("Over 146,000 packages connect to Shiny. Most of these <b style='color:{mypal[1]}'> depend on </b>, or <b style='color:{mypal[2]}'> import</b>,<br>or, are <b style='color:{mypal[3]}'>linked to</b>, or, <b style='color:{mypal[4]}'>suggest</b>, some popular “parent” R packages. The most<br>popular “parent” packages are shown below, with child pacakge-numbers in bottom-right.")
plot_subtitle <- str_wrap(subtitle_text, 75)
plot_caption <- paste0("**Data:** ShinyConf2024 - Tracy Teal and Jon Harmon |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)

#==============================================================================#
# Exploratory Data Analysis & Data Wrangling -----------------------------------
#==============================================================================#

# library(summarytools)
# dfSummary(shiny_revdeps) |> view()

# # Number of parent packages to visualize
# nos_parent_pckgs <- 10
# # Number of child packages to visualize
# nos_child_pckgs <- 50
# 
# top_pkgs <- shiny_revdeps |> 
#   count(parent, sort = T) |> 
#   slice_max(order_by = n, n = nos_parent_pckgs) |> 
#   pull(parent)
# 
# filter_pckgs <- shiny_revdeps |> 
#   filter(parent %in% top_pkgs) |> 
#   count(child, sort = T) |> 
#   slice_max(order_by = n, n = nos_child_pckgs, with_ties = FALSE) |> 
#   pull(child)
# 
# graph_df <- shiny_revdeps |> 
#   filter(parent %in% top_pkgs) |> 
#   filter(child %in% filter_pckgs) |> 
#   rename(
#     from = child,
#     to = parent
#   ) |> 
#   relocate(dependency_type, .after = everything())
# 
# count_pckgs <- graph_df |> 
#   select(from, to) |> 
#   pivot_longer(
#     cols = everything(),
#     names_to = NULL, 
#     values_to = "name"
#   ) |> 
#   count(name, sort = TRUE) |> 
#   rename(importance = n)
# 
# plot_df <- as_tbl_graph(graph_df, directed = TRUE) |> 
#   activate(nodes) |> 
#   left_join(count_pckgs) |> 
#   mutate(display_name = if_else(
#     name %in% top_pkgs,
#     name,
#     NA
#   ))

# Second Concept Exploration and Data Viz Attempt

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#


# Title image
title_image <- image_read(shiny_logo_url) |> 
  image_crop("560X150+300+70") |> 
  image_write(path = here("temp.png"))

g <- shiny_revdeps |> 
  count(dependency_type, parent) |> 
  filter(n > 100) |> 
  mutate(dependency_type = case_when(
    dependency_type == "depends" ~ glue("<b style='color:{mypal[1]}'> Depends on </b>"),
    dependency_type == "imports" ~ glue("<b style='color:{mypal[2]}'> Imports</b>"),
    dependency_type == "suggests" ~ glue("<b style='color:{mypal[4]}'>Suggests</b>"),
    dependency_type == "linkingto" ~ glue("<b style='color:{mypal[3]}'>Linked to</b>"),
    .default = NA
  )) |> 
  ggplot(
    aes(
      label = parent,
      area = n,
      subgroup = dependency_type,
      fill = dependency_type,
      colour = dependency_type
    )
  ) +
  geom_treemap(
    layout = "scol",
    alpha = 0.9
  ) +
  geom_treemap_subgroup_border(
    colour = bg_col,
    layout = "scol",
    size = 2
  ) +
  geom_treemap_text(
    layout = "scol",
    place = "center",
    grow = TRUE,
    reflow = TRUE,
    min.size = ts/20,
    padding.x = unit(1, "mm"),
    family = "body_font"
  ) +
  geom_treemap_text(
    aes(label = n),
    layout = "scol",
    place = "bottomright",
    size = ts / 4,
    #min.size = ts/20,
    padding.x = unit(1, "mm"),
    padding.y = unit(1, "mm"),
    family = "body_font"
  ) +
  scale_colour_manual(values = colpal) +
  scale_fill_manual(values = fillpal) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  theme_void(
    base_size = ts,
    base_family = "body_font"
  ) +
  theme(
    legend.position = "none",
    legend.direction = "horizontal",
    plot.margin = margin(2, 2, 2, 2, "cm"),
    legend.title = element_blank(),
    legend.text = element_markdown(
      margin = margin(0.4, 0.7, 0.4, 0.05, "cm"),
      size = 1.2 * ts,
      colour = text_hil,
      family = "title_font"
    ),
    plot.title = element_markdown(
      margin = margin(0,0,0,0, "cm"),
      hjust = 0.5
    ),
    plot.subtitle = element_markdown(
      size = ts,
      hjust = 0.5,
      colour = text_hil,
      family = "title_font",
      lineheight = 0.3,
      margin = margin(0, 0, 0.5, 0, "cm")
    ),
    plot.caption = element_textbox(
      family = "caption_font",
      size = 0.75 * ts,
      hjust = 0.5,
      colour = text_hil
    ),
    legend.key.width = unit(2, "cm")
  )
#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_shiny_packages.png"),
  plot = g,
  width = 40, 
  height = 52, 
  units = "cm",
  bg = "white"
)

unlink(here("temp.png"))

shiny_revdeps |> 
  count(dependency_type, parent) |> 
  filter(n > 100) |> 
  group_by(dependency_type) |> 
  slice_max(order_by = n, n = 2)



#==============================================================================#
# Attempt 2nd Viz---------------------------------------------------------------
#==============================================================================#

# Load libraries + Data Wrangling ----------------------------------------------
library(tidygraph)
library(ggraph)
# from CRAN
# install.packages("PNWColors")
library(PNWColors)
library(colorspace)
mypal <- pnw_palette("Winter", 4)
font_add_google("Thasadith", family = "first_font")
font_add_google("Saira Extra Condensed", family = "second_font")

object.size(package_details)

df5 <- package_details |> 
  clean_names() |> 
  select(
    package, date, date_publication, 
    maintainer, reverse_imports,
  ) |> 
  mutate(
    date = as_date(date),
    date_time = as_datetime(date_publication)
  ) |> 
  select(-date_publication) |> 
  separate_wider_delim(
    cols = maintainer,
    names = c("maintainer", NA),
    delim = " <",
    too_few = "align_start"
  )
  

# Network Graph amongst top "top_m" maintainers --------------------------------
top_m <- 20

selected_maintainers <- df5 |> 
  count(maintainer, sort = T) |> 
  slice_max(order_by = n, n = top_m)

network1 <- df5 |> 
  select(-date, -date_time) |> 
  filter(maintainer %in% selected_maintainers$maintainer) |> 
  separate_longer_delim(
    cols = reverse_imports,
    delim = ", "
  ) |> 
  left_join(
    df5 |> select(package, maintainer) |> rename(ri_maintainer = maintainer),
    by = join_by(reverse_imports == package)
  ) |> 
  filter(ri_maintainer %in% selected_maintainers$maintainer) |> 
  rename(
    to = ri_maintainer,
    from = maintainer
  ) |> 
  select(-c(package, reverse_imports)) |> 
  count(from, to)

# Plotting ---------------------------------------------------------------------
set.seed(42)
g <- as_tbl_graph(network1, directed = TRUE) |>
  activate(nodes) |> 
  mutate(
    popularity = centrality_degree(mode = "in", weights = n)
  ) |>
  left_join(selected_maintainers, by = join_by(name == maintainer)) |> 
  rename(n_pckgs = n) |> 
  ggraph(layout = "stress") +
  geom_edge_arc(
    aes(width = n),
    alpha = 0.3,
    lineend = "round",
    strength = -0.05,
    colour = mypal[3],
    arrow = arrow()
  ) +
  geom_node_label(
    aes(
      label = paste0(name, " (", n_pckgs, ")"),
      size = popularity
    ),
    alpha = 1,
    family = "second_font",
    colour = mypal[2],
    fill  = mypal[4] |> lighten(0.2),
    label.padding = unit(2, "mm"),
    label.r = unit(2, "mm"), repel = T
  ) +
  scale_edge_width(range = c(0.5, 10)) +
  scale_size_continuous(range = c(ts/8, ts/2)) +
  labs(
    title = paste0("Top ", top_m, " R Packages' Maintainers"),
    subtitle = "Linkages (imports) amongst packages maintained by each author.\nNumber in brackets represents number of packages maintained by each.",
    caption = plot_caption
  ) +
  theme_void(
    base_family = "first_font",
    base_size = ts
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = ts * 3,
      colour = mypal[1],
      margin = margin(2,0,0,1, "cm")
    ),
    plot.subtitle = element_text(
      hjust = 0,
      size = 1.2 * ts,
      lineheight = 0.35,
      colour = mypal[1],
      margin = margin(0.5,0,0,1, "cm")
    ),
    plot.caption = element_textbox(
      hjust = 0.5,
      colour = mypal[1],
      margin = margin(0,0,1.5,0, "cm"),
      family = "caption_font"
    ),
  )

# Saving -----------------------------------------------------------------------
ggsave(
  filename = here::here("docs", "tidy_shiny_packages2.png"),
  plot = g,
  width = 45, 
  height = 45, 
  units = "cm",
  bg = mypal[4]
)
