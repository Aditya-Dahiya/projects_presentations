# TidyTuesday: TidyTuesday, facilitated by the R4DS Online Learning Community, 
# is currently in search of a new fiscal sponsor (as of March 2024). This week's 
# dataset is sourced from the Fiscal Sponsor Directory. The Fiscal Sponsor 
# Directory conducted an analysis of their listings in March 2023. 
# Here are some key facts and statistics about fiscal sponsors from the 
# Fiscal Sponsor Directory:

# 60% (227) of the sponsors in the Directory operate under Model C, 
# the preapproved grant relationship.

# The 380 sponsors featured in the Fiscal Sponsor Directory provided detailed 
# information after participating in a survey about their experience, eligibility 
# requirements, fee structure, services, and philosophy. This allows 
# prospective projects to better gauge which fiscal sponsors might suit their needs.

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

# A long format of the data with separated out individual components of 4 columns
fsd_long <- fsd |> 
  mutate(id = row_number(), .before = everything()) |> 
  separate_longer_delim(
    cols = c(eligibility_criteria),
    delim = "|"
  ) |> 
  mutate(
    eligibility_criteria = if_else(
      eligibility_criteria == "Aligned mission/values",
      "Aligned mission values",
      eligibility_criteria
    )
  ) |> 
  separate_longer_delim(project_types, "|") |> 
  separate_longer_delim(fiscal_sponsorship_model, "|") |> 
  separate_longer_delim(services, "|")

# Looking closely at Financial Sponsorship Models
fsd_model <- fsd |> 
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

model_descrp <- fsd_model |> 
  count(model_type, model_description, sort = T) |> 
  slice_head(n = 8) |> 
  select(-n)

plotdf <- fsd_model |> 
  select(-model_description) |> 
  left_join(model_descrp, relationship = "many-to-many") |> 
  mutate(
    model_type = case_when(
      str_detect(model_type, "Model A") ~ "Model A",
      str_detect(model_type, "Model C") ~ "Model C",
      str_detect(model_type, "Most of our") ~ "Model A",
      .default = model_type
    )
  )


# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Cabin Sketch",
  family = "title_font"
) # Font for titles
font_add_google("Oswald",
  family = "caption_font"
) # Font for the caption
font_add_google("Abel",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Define colours
bg_col <-    # Background Colour
text_col <-  # Colour for the text
text_hil <-  # Colour for highlighted text

# Define Text Size
ts <- unit(20, units = "cm") # Text Size

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
plot_caption <- paste0("**Data:** ", " | ", " #TidyTuesday | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
rm(c(social_caption_1, social_caption_2, xtwitter_username, xtwitter, github_username, github))

plot_title <- ""

plot_subtitle <- str_wrap(
  "...", 
  width = 100
  )

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

g <- 
  
# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#

ggsave(
  filename = here::here("docs", "tidy_fiscal_sponsor.png"),
  plot = g,
  width = 40,
  height = 45,
  units = "cm",
  bg = bg_col
  )

