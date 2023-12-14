#=================An Attempt at Shadowed Designer Bar Chart====================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(summarytools)   # Exploratory Data Analysis
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(magick)         # Work with Images and Logos
library(tidytext)       # Word Clouds and Words Analysis 
library(ggwordcloud)    # Word Cloud

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#
# Option 1: tidytuesdayR package 

tuesdata <- tidytuesdayR::tt_load('2023-12-12')

holimov <- tuesdata$holiday_movies
hmgenre <- tuesdata$holiday_movie_genres

rm(tuesdata)

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

holimov
hmgenre

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

# Idea 1: a word cloud of most common words in the title

# A tibble of words grouped by the movie title (i.e. a group for each movie)
dfnet <- holimov |>  
  select(tconst, original_title, simple_title, genres) |> 
  group_by(original_title) |> 
  separate_longer_delim(cols = simple_title,
                        delim = " ") |> 
  rename(word = simple_title) |> 
  anti_join(stop_words)
  
dfnet |> 
  ungroup() |> 
  count(word, sort = TRUE) |> 
  slice_head(n = 100) |> 
  ggplot(aes(label = word, 
             size = n)) + 
  geom_text_wordcloud() +
  scale_size(range = c(5, 15)) +
  theme_void() 


# Idea: Percentage of movies in each genre which have the word "Christmas"
# or "holiday"
hmgenre |>  
  left_join(holimov |> select(-genres), 
            by = join_by(tconst)) |> 
  group_by(genres) |> 
  summarise(
    christmas = mean(christmas),
    holiday = mean(holiday)
  ) |> 
  pivot_longer(cols = c(christmas, 
                        holiday),
               names_to = "word",
               values_to = "frequency") |> 
  ggplot(aes(y = reorder(genres, frequency), 
             x = frequency)) +
  geom_col() +
  facet_wrap(~word, scales = "free")


# Idea 2: Network graph for commonly occurring words, faceted by genre

# A tibble of nodes
dfnodes <- dfnet |> 
  ungroup() |> 
  count(word, sort = TRUE) |> 
  rename(importance = n) |> 
  filter(word != "")


# Titles with 2 or more words  
filter_title <- dfnet |>
  group_by(tconst) |> 
  count(sort = TRUE) |> 
  filter(n > 1)

# Computing the combinations of words within the titles
# And, making a tibble of edges
dfedges <- dfnet |> 
  right_join(filter_title) |> 
  group_by(original_title) |>
  do(data.frame(t(combn(.$word, 2)))) |> 
  count(X1, X2, sort = TRUE) |> 
  rename(from = X1,
         to = X2,
         weightage = n,
         group = original_title) |> 
  ungroup() |> 
  group_by(from, to) |> 
  summarise(
    weightage = sum(weightage)
    ) |> 
  filter(from != "" & to != "") |> 
  arrange(desc(weightage)) |> 
  filter(from != to) |> 
  ungroup()
  
dfnodes
dfedges

# Creating a tidygraph object

library(tidygraph)
library(ggraph)

movgraph <- tbl_graph(nodes = dfnodes,
          edges = dfedges,
          directed = FALSE,
          node_key = "word")

sel_nodes <- dfedges |> 
  filter(weightage > 2) |> 
  ungroup() |> 
  select(from, to) |> 
  pivot_longer(cols = everything()) |> 
  pull(value) |> 
  unique()
  
#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Mountains of Christmas", 
                family = "title_font",
                bold.wt = 900)               # Font for titles
font_add_google("Pragati Narrow", 
                family = "caption_font")     # Font for the caption
font_add_google("Yanone Kaffeesatz", 
                family = "body_font")        # Font for plot text
font_add_google("Lobster",
                family = "geomtext_font",
                bold.wt = 900)               # Font for nodes
showtext_auto()

# Creating a Colour Palette for the Visualization
mypal <- c("#f5473b", "#940d03", "#1fcf00", "#207510")

# Define colours
link_col <- "#F7B787"
bg_col <- "#FFF5E0"                   # Background Colour
text_col <- "#141E46"                 # Colour for the text

# Define Text Size
ts = 24                              # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
linkedin <- "&#xf08c"
linkedin_username <- "dr-aditya-dahiya-ias"
social_caption <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span> <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span> <span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin};</span> <span style='color: {text_col}'>{linkedin_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "Holiday Movies"

plot_subtitle <- "Commonly occuring words in the titles of Holiday Season movies on IMDb. Width of linkages reflect the times words occur together. \nSize of the word represents frequency of occurence. Colour of the word represents groups of densely connected words."

plot_caption <- paste0("**Data:** IMDb Non-Commercial Datasets", "  |", "  **Graphics:** ", social_caption)

# Plot background image
img <- image_read("https://img.freepik.com/premium-vector/snow-background-winter-snowfall-white-snowflakes-blue-sky-christmas-background-falling-snow_217752-1321.jpg?w=2000") |> 
  image_crop(geometry = "1200x1200") |> 
  image_blur(radius = 5, sigma = 2) |> 
  image_border(color = "lightblue",
               geometry = "2x2")

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#
set.seed(3)
movgraph |> 
  activate(nodes) |> 
  filter(importance > 10) |>
  # Drop some common words or outliers
  filter(!(word %in% c("xmas", "spirit", "2", "12"))) |> 
  mutate(
    col_var = group_edge_betweenness(
      weights = weightage,
      n_groups = 4)
  ) |> 
  ggraph(layout = "lgl") +
  geom_edge_bend(
    aes(width = weightage),
    alpha = 0.3,
    color = link_col,
    lineend = "round",
    linejoin = "bevel",
    n = 50) +
  geom_node_text(
    aes(label = word,
        size = importance, 
        col = as_factor(col_var)),
    family = "geomtext_font",
    fontface = "bold") +
  scale_size_continuous(range = c(8, 20)) +
  scale_alpha_continuous(range = c(0.1, 0.4)) +
  scale_edge_width(range = c(2, 10)) +
  scale_color_manual(values = mypal) +
  theme_void() +
  labs(title = plot_title,
       caption = plot_caption,
       subtitle = plot_subtitle) +
  theme(
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = ts/1.5),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 3*ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_col),
  plot.subtitle    = element_text(hjust = 0.5,
                                  size = ts/1.2,
                                  family = "body_font",
                                  colour = text_col),
  plot.background  = element_rect(fill = bg_col,
                                  colour = "brown",
                                  linewidth = 1),
  legend.position = "none"
)
#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "....png"),
  plot = g,
  device = "png", 
  dpi = "retina", 
  width = 10, 
  height = 7, 
  units = "px",
  bg = bg_col
)