# TidyTuesday: This week's 
# dataset is sourced from the Fiscal Sponsor Directory. The Fiscal Sponsor 
# Directory conducted an analysis of their listings in March 2023. 
# Here are some key facts and statistics about fiscal sponsors from the 
# Fiscal Sponsor Directory:

# 60% (227) of the sponsors in the Directory operate under Model C, 
# the pre-approved grant relationship.

# The 380 sponsors featured in the Fiscal Sponsor Directory provided detailed 
# information after participating in a survey about their experience, 
# eligibility requirements, fee structure, services, and philosophy. This 
# allows prospective projects to better gauge which fiscal sponsors might 
# suit their needs.

# Launched in November 2008, the Directory has become the most extensive 
# database of essential information about individual fiscal sponsors. It 
# encompasses most major players in the field and an increasing number of 
# startup sponsors.

# These sponsors are spread across 40 states, including the District of Columbia, 
# Puerto Rico, and Canada. California hosts the highest number of sponsors, with 
# 116, comprising almost a third of the total.

# Public Health Solutions holds the title of the most experienced fiscal sponsor 
# in the Directory, operating since 1957 and currently sponsoring three projects. 
# TSNE MissionWorks, established two years later, now sponsors 47 projects.

# Fractured Atlas, located in New York City, emerges as the largest fiscal 
# sponsor with 4,000 projects. Operating exclusively online, 
# it adheres solely to Model C fiscal sponsorship.

# ==============================================================================#
# Library Load-in---------------------------------------------------------------
# ==============================================================================#
library(tidyverse) # Data Wrangling and Plotting
library(here) # Files location and loading

library(showtext) # Using Fonts More Easily in R Graphs
library(fontawesome) # Social Media icons
library(ggtext) # Markdown Text in ggplot2
library(scales) # Labeling the axes
library(colorspace) # Lighten and Darken Colours
library(patchwork) # For compiling plots
library(ggstream) # Stream plots

# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#

# Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load(2024, week = 11)

fsd <- tuesdata$fiscal_sponsor_directory
rm(tuesdata)

# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#

library(summarytools)
dfSummary(fsd) |> view()

visdat::vis_dat(fsd)

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#

# A long format of the data with separated out individual components of 2 
# columns that we are interested in: Model Types and Project types
fsd_long <- fsd |> 
  mutate(id = row_number(), .before = everything()) |> 
  separate_longer_delim(project_types, "|") |> 
  separate_longer_delim(fiscal_sponsorship_model, "|") |> 
  filter(!is.na(fiscal_sponsorship_model)) |> 
  separate_wider_delim(
    cols = fiscal_sponsorship_model,
    delim = ", ",
    names = c("model_type", "model_description"),
    too_few = "align_start",
    too_many = "merge"
  ) |> 
  filter(str_detect(model_type, "Model "))

# A description of Common Sponsorship Models
model_descrp <- fsd_long |> 
  count(model_type, model_description, sort = T) |> 
  slice_head(n = 7) |> 
  select(-n)

# The long data-frame to use for getting subsets 
df <- fsd_long |> 
  select(-model_description) |> 
  left_join(model_descrp, relationship = "many-to-many") |> 
  mutate(
    model_type = case_when(
      str_detect(model_type, "Model A") ~ "Model A",
      str_detect(model_type, "Model C") ~ "Model C",
      str_detect(model_type, "Most of our") ~ "Model A",
      .default = model_type
    )
  ) |> 
  rename(year = year_fiscal_sponsor)

# Checking most common project types
df |> 
  count(project_types, sort = T)

# Combining data on 4 important project types I wish to plot as stream graph
plotdf <- bind_rows(
  df |> 
    filter(str_detect(project_types, "(?i)Children|(?i)Youth")) |> 
    count(year, model_type) |> 
    mutate(project_category = "Children / Youth Development"),
  
  df |> 
    filter(str_detect(project_types, "(?i)Arts and culture")) |> 
    count(year, model_type) |> 
    mutate(project_category = "Arts and Culture"),
  
  df |> 
    filter(str_detect(project_types, "(?i)Environment")) |> 
    count(year, model_type) |> 
    mutate(project_category = "Environment"),
  
  df |> 
    filter(str_detect(project_types, "(?i)Economic Development")) |> 
    count(year, model_type) |> 
    mutate(project_category = "Economic Development")
) |> 
  filter(year >= 1990)

pc_levels <- plotdf |> 
  distinct(project_category) |> 
  arrange(project_category) |> 
  pull(project_category)

model_levels <- plotdf |> 
  distinct(model_type) |> 
  arrange(model_type) |> 
  pull(model_type)

# Adding zero values before and after years to smoothen the stream plot
plotdf2 <- plotdf |> 
  mutate(
    project_category = fct(project_category, levels = pc_levels),
    model_type = fct(model_type, levels = model_levels)
  ) |> 
  bind_rows(
    expand.grid(pc_levels, model_levels) |> 
      as_tibble() |> 
      rename(
        project_category = Var1,
        model_type = Var2
      ) |> 
      mutate(
        year = 1987,
        n = 0
      )
  ) |> 
  bind_rows(
    expand.grid(pc_levels, model_levels) |> 
      as_tibble() |> 
      rename(
        project_category = Var1,
        model_type = Var2
      ) |> 
      mutate(
        year = 2026,
        n = 0
      )
  )

plot_labels <- model_descrp |> 
  arrange(model_type) |> 
  mutate(label = paste0(model_type,"\n",model_description)) |> 
  pull(label)

# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Felipa",
  family = "title_font"
) # Font for titles
font_add_google("Tulpen One",
  family = "caption_font"
) # Font for the caption
font_add_google("Ruluko",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Define colours
mypal <- paletteer::paletteer_d("ghibli::PonyoLight")

bg_col <- "#f5f5f5" # Background Colour
text_col <- mypal[3] |> darken(0.7) # Colour for the text
text_hil <- mypal[3] |> darken(0.4) # Colour for highlighted text

# Define Text Size
ts <- unit(40, units = "cm") # Text Size

# Plot Title, Subtitle and Caption
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
plot_caption <- paste0("**Data:** Fiscal Sponsor Directory (fiscalsponsordirectory.org) ", " | ", " #TidyTuesday | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

plot_title <- "Fiscal Sponsorship Models"

plot_subtitle <- str_wrap(
  "The fiscalsponsordirectory.org lists various sponsor organizations that funds projects in different areas of work. The graphic shows the evolution of sponsorship models (from the book Fiscal Sponsorship: 6 Ways To Do It Right, 3rd Edition) over time in the 4 major fields of work.\nModel C has become the most popular model over time!", 
  width = 100
  )

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- plotdf2 |> 
  ggplot(aes(x = year, y = n)) +
  geom_stream(
    aes(fill = model_type),
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    data = tibble(year = 1990, 
                  n = 15, 
                  project_category = pc_levels),
    aes(label = project_category),
    hjust = 0,
    family = "title_font",
    fontface = "bold",
    size = ts,
    colour = text_hil) +
  facet_wrap(~ project_category, ncol = 1) +
  scale_x_continuous(
    expand = expansion(0),
    position = "top",
    breaks = seq(1990, 2020, 5)
  ) +
  paletteer::scale_fill_paletteer_d(
    "ghibli::PonyoLight",
    labels = plot_labels
  ) +
  guides(
    fill = guide_legend(
      title = "Fiscal Sponsorship Model",
      position = "bottom",
      direction = "horizontal",
      override.aes = list(),
      nrow = 1
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text.position = "bottom",
    legend.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    legend.direction = "horizontal",
    plot.title = element_text(
      family = "title_font",
      hjust = 0.5,
      size = 8 * ts,
      face = "bold",
      colour = text_hil,
      margin = margin(1, 0, 1, 0, unit = "cm")
    ),
    plot.subtitle = element_text(
      family = "body_font",
      hjust = 0.5,
      size = 2 * ts,
      lineheight = 0.3,
      margin = margin(0, 0, 0, 0, unit = "cm"),
      colour = text_col
    ),
    plot.caption = element_textbox(
      family = "caption_font",
      size = 2 * ts,
      hjust = 0.5,
      colour = text_hil
    ),
    axis.text.x = element_text(
      family = "body_font",
      hjust = 0.5,
      size = 3 * ts,
      margin = margin(0, 0, 0, 0, unit = "cm"),
      colour = text_col
    ),
    legend.title = element_text(
      family = "caption_font",
      hjust = 0.5,
      vjust = 1,
      size = 4 * ts,
      lineheight = 0.3,
      margin = margin(0, 0.5, 0, 0, unit = "cm"),
      colour = text_col
    ),
    legend.text = element_text(
      family = "caption_font",
      hjust = 0.5,
      size = 2 * ts,
      lineheight = 0.2,
      margin = margin(0.3, 0, 0, 0, unit = "cm"),
      colour = text_col
    ),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(3.5, "cm")
  )


# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "tidy_fiscal_sponsor.png"),
  plot = g,
  width = 40,
  height = 50,
  units = "cm",
  bg = bg_col
  )



