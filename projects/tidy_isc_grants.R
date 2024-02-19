# ==============================================================================#
# Library Load-in---------------------------------------------------------------
# ==============================================================================#
library(tidyverse) # Data Wrangling and Plotting
library(here) # Files location and loading
library(showtext) # Using Fonts More Easily in R Graphs
library(ggimage) # Using Images in ggplot2
library(fontawesome) # Social Media icons
library(ggtext) # Markdown Text in ggplot2
library(scales) # Labeling the axes
library(ggwordcloud) # Word Clouds
library(patchwork) # For compiling plots
library(colorspace) # Lighten and Darken Colours

# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#

# Option 1: tidytuesdayR package

tuesdata <- tidytuesdayR::tt_load(2024, week = 8)

isc_grants <- tuesdata$isc_grants
rm(tuesdata)


# ==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
# ==============================================================================#

library(summarytools)
dfSummary(isc_grants) |> view()

names(isc_grants)

# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#
library(tidytext) # for text analysis

other_stop_words <- 
  tibble(
    word = c(
      "data", "project", "package", "packages",
      "aims", "provide", "existing", "tools",
      "documentation", "based", "support", "create",
      "code", "create", "system", "develop",
      "source", "symbolic", "developers", "research",
      "resources", "current", "framework", "hub", "isc",
      "movement", "process", "providing", "materials",
      "test", "user", "2"
    )
  )
  
# Creating a tidytext format for most common words in titles
common_words <- isc_grants |> 
  unnest_tokens(
    output = "word",
    input = summary
  ) |> 
  anti_join(stop_words) |> 
  anti_join(other_stop_words) |> 
  count(word, sort = TRUE) |> 
  filter(n > 2) |> 
  slice_max(order_by = n, n = 30)

# Common bigrams in summary
common_bigrams <- isc_grants |> 
  unnest_tokens(
    output = "bigram",
    input = summary,
    token = "ngrams",
    n = 2
  ) |> 
  count(bigram, sort = TRUE) |> 
  separate(
    col = bigram,
    into = c("word1", "word2"),
    sep = " "
  ) |> 
  filter(!(word1 %in% stop_words$word)) |> 
  filter(!(word2 %in% stop_words$word)) |> 
  filter(!(word1 %in% other_stop_words$word)) |> 
  filter(!(word2 %in% other_stop_words$word)) |> 
  slice_max(order_by = n, n = 20, with_ties = FALSE)

# Website hosting these projects
webpages_hosting <- isc_grants %>%
  mutate(website_name = str_extract(website, "(?<=://)[^/]+")) |> 
  count(website_name, sort = TRUE) |> 
  drop_na()

# Funding per year, number of projects funded and average per grant
fundings <- isc_grants |> 
  group_by(year) |> 
  summarise(grant = sum(funded)) |> 
  left_join(isc_grants |> count(year)) |> 
  mutate(average = grant/n) |> 
  rename(number = n) |> 
  pivot_longer(cols = -year)

# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Nova Mono",
  family = "title_font"
) # Font for titles
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) # Font for the caption
font_add_google("M PLUS Code Latin",
  family = "body_font"
) # Font for plot text
showtext_auto()

# Options of Colour palettes to use in visualization
mypal1 <- paletteer::paletteer_d("palettetown::dragonair")

# Chossing one Palette for the Visualization
mypal <- mypal1[c(1,2,4,6,7, 8)]

# Define colours
bg_col <- "white"  # Background Colour
text_col <- mypal[3] |> darken(0.2) # Colour for the text
text_hil <- mypal[3] # Colour for highlighted text

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
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "R Consortium ISC Funded Projects"
plot_caption <- paste0("**Data:** R Consortium Infrastructure Steering Committee (ISC) Grant Program", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)

# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

# Plot 1: Line Plots -----------------------------------------------------------
labels1 <- c("Average funding per project (US $)",
             "Total grant given each year (US $)",
             "Number of projects funded by ISC")
names(labels1) <- c("average", "grant", "number")
g1 <- fundings |> 
  ggplot(mapping = aes(
    x = year,
    y = value
  )) +
  geom_line(colour = text_hil,
            alpha = 0.8,
            linewidth = 1.5) +
  geom_point(size = 2.4) +
  facet_wrap(
    ~name, 
    ncol = 1, 
    scales = "free_y",
    labeller = labeller(name = labels1)
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ",",
      accuracy = 1
    )
  ) +
  scale_x_continuous(
    breaks = 2016:2023
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "ISC Grants: 2016 to 2022"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "darkgrey",
      linewidth = 0.2,
      linetype = 2
    ),
    axis.text = element_text(
      size = 1.5 * ts,
      family = "body_font",
      colour = text_col,
      face = "bold",
      margin = margin(0, 0, 0, 0)
    ),
    strip.text = element_text(
      size = 2 * ts,
      family = "body_font",
      colour = text_col,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 2.5 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil
    )
  )

# Plot 2: WordCLoud-------------------------------------------------------------
set.seed(42)
g2 <- common_words |> 
  ggplot(mapping = aes(
    label = word,
    size = n,
    color = factor(sample.int(6, nrow(common_words), replace = TRUE))
  )) +
  geom_text_wordcloud(
    eccentricity = ,
    family = "body_font",
    shape = "square"
  ) +
  scale_size(range = c(12, 35)) +
  scale_color_manual(values = mypal) +
  labs(subtitle = "Most common words in grant projects' summaries") +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(
      size = 2.5 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil
    )
  )

g3 <- common_bigrams |> 
  mutate(bigram = paste0(word1, " ", word2)) |> 
  ggplot(mapping = aes(
    label = bigram,
    size = n,
    color = factor(sample.int(6, nrow(common_bigrams), replace = TRUE))
  )) +
  geom_text_wordcloud(
    eccentricity = ,
    family = "body_font",
    shape = "square"
  ) +
  scale_size(range = c(6, 16)) +
  scale_color_manual(values = mypal) +
  labs(subtitle = "Common phrases in project summaries") +
  theme_void() +
  theme(
    plot.subtitle = element_text(
      size = 1.8 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil
    )
  )

g4 <- webpages_hosting |> 
  mutate(
    website_name = fct(website_name),
    website_name = fct_lump_n(website_name, n = 1, w = n)
  ) |> 
  group_by(website_name) |> 
  summarise(count = sum(n)) |> 
  ggplot(mapping = aes(
    x = "",
    y = count,
    fill = website_name,
    label = website_name
  )) +
  geom_col(color = "white") +
  geom_text(
    position = position_stack(
      vjust = 0.5
    ),
    family = "body_font",
    fontface = "bold",
    size = 0.75 * ts,
    color = "white"
  ) +
  coord_polar(theta = "y") + 
  labs(
    subtitle = "Websites hosting the projects' URL"
  ) +
  scale_fill_manual(values = mypal[c(2, 5)]) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(
      size = 1.8 * ts,
      family = "title_font",
      hjust = 0.5,
      colour = text_hil
    )
  )
  
my_layout <- "
  AABBBB
  AABBBB
  AABBBB
  AACCDD
  AACCDD
"

g <- g1 + g2 + g4 + g3 + 
  plot_layout(design = my_layout) +
  plot_annotation(
    title = plot_title,
    caption = plot_caption,
    theme = theme(
    plot.caption = element_textbox(
      family = "caption_font",
      hjust = 0.5,
      colour = text_col,
      size = 2 * ts,
      margin = margin(1, 0, 1, 0,
        unit = "cm"
        )
      ),
    plot.title = element_text(
      hjust = 0.5,
      size = 6.5 * ts,
      family = "title_font",
      face = "bold",
      colour = text_hil,
      margin = margin(1, 0, 1, 0,
        unit = "cm"
        )
      ),
    plot.background = element_rect(
      fill = bg_col,
      color = bg_col,
      linewidth = 0
      ),
    plot.title.position = "plot"
      )
    )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#


ggsave(
  filename = here::here("docs", "tidy_isc_grants.png"),
  plot = g,
  width = 30,
  height = 30,
  units = "cm",
  bg = bg_col
)

