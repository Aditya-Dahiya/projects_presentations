# ==============================================================================#
# Library Load-in---------------------------------------------------------------
# ==============================================================================#
library(tidyverse) # Data Wrangling and Plotting
library(here) # Files location and loading
library(showtext) # Using Fonts More Easily in R Graphs
library(ggimage) # Using Images in ggplot2
library(fontawesome) # Social Media icons
library(ggtext) # Markdown Text in ggplot2
library(patchwork) # For compiling plots
library(figpatch) # Images in patchwork
library(magick) # Work with Images and Logos
library(rnaturalearth) # Maps of countries
library(colorspace) # Lighten and darken Colours
library(ggstream) # Stream Graph


# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#

# Option 1: tidytuesdayR package

tuesdata <- tidytuesdayR::tt_load(2024, week = 7)

historical_spending <- tuesdata$historical_spending
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender


# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#

library(summarytools)
dfSummary(historical_spending) |> view()

library(janitor)
historical_spending |>
  select(-c(PercentCelebrating, PerPerson)) |>
  mutate(Year = as.character(Year)) |>
  adorn_totals(where = "col")

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#

df1 <- historical_spending |>
  select(-c(PercentCelebrating, PerPerson)) |>
  pivot_longer(
    cols = -Year,
    names_to = "category",
    values_to = "value"
  ) |>
  group_by(Year) |>
  mutate(total = sum(value)) |>
  ungroup()

# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Love Light",
  family = "title_font"
) # Font for titles
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) # Font for the caption
font_add_google("Kotta One",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Options of Colour palettes to use in visualization
mypal1 <- paletteer::paletteer_d("rcartocolor::Tropic")
mypal2 <- paletteer::paletteer_d("rcartocolor::Temps")
mypal3 <- paletteer::paletteer_d("tvthemes::RoseQuartz")
mypal4 <- paletteer::paletteer_d("PNWColors::Sunset")

# Chossing one Palette for the Visualization
mypal <- mypal3

# Define colours
bg_col <- "#ffe3fd" |> lighten(0.5) # Background Colour
text_col <- "#420F75FF" # Colour for the text
text_hil <- "#B53737FF" # Colour for highlighted text

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
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "Love ain't cheap !"
subtitle_text <- "Valentine's Day Spending trends: From 2010 to 2022, the average person's Valentine's Day budget has ballooned by a whopping 70%, soaring from $91 to $156! Despite the changing times, jewelry remains the undisputed champion of affectionate spending, while the top 7 expenditure categories have maintained their share."
plot_subtitle <- paste(strwrap(subtitle_text, 100), collapse = "\n")

plot_caption <- paste0("**Data:** Valentine's Day Data Center - National Retail Federation (USA)", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

library(ggchicklet)
g <- ggplot(
  data = df1,
  mapping = aes(
    x = Year,
    y = value,
    fill = category,
    color = category,
    label = category
    )
  ) +
  geom_area(
    alpha = 0.5
  ) +
  geom_chicklet(
    position = "stack",
    width = 0.1,
    color = "transparent"
  ) +
  geom_text(
    data = df1 |> filter(Year == 2022),
    mapping = aes(label = category),
    hjust = -0.1,
    vjust = +2, position = "stack",
    family = "body_font",
    size = 0.8 * ts
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    x = NULL,
    y = "Average Spending"
  ) +
  scale_x_continuous(
    expand = expansion(c(0.01, 0.25)),
    breaks = 2010:2022
  ) +
  scale_y_continuous(
    labels = scales::label_dollar(),
    expand = expansion(0),
    breaks = seq(0, 150, 25)
  ) +
  scale_color_manual(
    values = mypal |> darken(0.3)
  ) +
  scale_fill_manual(
    values = mypal
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_textbox(
      family = "caption_font",
      hjust = 1,
      colour = text_col,
      size = 1.5 * ts,
      margin = margin(1, 0, 0, 1,
        unit = "cm"
      )
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 12 * ts,
      family = "title_font",
      face = "bold",
      colour = text_hil,
      margin = margin(0, 0, 0, 0,
        unit = "cm"
      )
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 1.8 * ts,
      family = "body_font",
      colour = text_col,
      margin = margin(0, 0, 0, 0,
        unit = "cm"
      ),
      lineheight = 0.35
    ),
    plot.background = element_rect(
      fill = bg_col,
      color = bg_col,
      linewidth = 0
    ),
    axis.text = element_text(
      size = 2 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
   axis.title.y = element_text(
      size = 3 * ts,
      family = "body_font",
      colour = text_col,
      hjust = 0.5
    ),
    axis.line.y = element_line(
      color = text_hil,
      arrow = arrow(length = unit(3, "mm"))
    ),
   panel.grid = element_blank(),
   plot.title.position = "plot"
  )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#


ggsave(
  filename = here::here("docs", "tidy_valentine_day.png"),
  plot = g,
  width = 20,
  height = 25,
  units = "cm",
  bg = bg_col
)
