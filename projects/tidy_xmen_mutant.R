#================= #TidyTuesday : X-Men Mutant Moneyball ======================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(janitor)

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(patchwork)      # For compiling plots
library(figpatch)       # Images in patchwork
library(magick)         # Work with Images and Logos
library(colorspace)     # Lighten and darken Colours
library(scales)         # Scale Labels in ggplot2

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 2: Read directly from GitHub

mutant_moneyball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv') |> 
  clean_names()

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

# library(summarytools)
# dfSummary(mutant_moneyball) |> view()

mutant_moneyball |> 
  select(member, total_issues) |> 
  arrange(desc(total_issues)) |> 
  print(n = Inf)

xmen_names <- tibble(
  member = c("scottSummers", "jeanGrey", "charlesXavier", "hankMcCoy",
             "ericMagnus", "loganHowlett", "ororoMunroe", "kurtWagner"),
  full_name = c("Scott Summers", "Jean Grey", "Charles Xavier", "Hank McCoy",
                "Erik Lehnsherr", "Logan Howlett", "Ororo Munroe", "Kurt Wagner"),
  superhero_name = c("Cyclops", "Phoenix", "Professor X", "Beast",
                     "Magneto", "Wolverine", "Storm", "Nightcrawler"),
  url = c(
    "https://upload.wikimedia.org/wikipedia/en/e/e9/Cyclops_%28Scott_Summers_circa_2019%29.png",
    "https://static.wikia.nocookie.net/mua/images/2/2c/Dlc_img_08.png/revision/latest?cb=20191216041152",
    "https://upload.wikimedia.org/wikipedia/en/9/9e/Charles_Xavier_%28Patrick_Stewart%29.jpg",
    "https://upload.wikimedia.org/wikipedia/en/0/0b/Beast_%28Hank_McCoy_-_circa_2019%29.png",
    "https://upload.wikimedia.org/wikipedia/en/e/e9/Magneto_%28Marvel_Comics_character%29.jpg",
    "https://upload.wikimedia.org/wikipedia/en/5/5d/Wolverine_%28James_%27Logan%27_Howlett%29.png",
    "https://upload.wikimedia.org/wikipedia/en/3/34/Storm_%28Ororo_Munroe%29.png",
    "https://upload.wikimedia.org/wikipedia/en/7/7b/Nightcrawler_%28Kurt_Wagner_circa_2018%29.png"
  )
)

df1 <- mutant_moneyball |> 
  filter(member %in% xmen_names$member) |> 
  select(member, total_issues, total_value_heritage,
         total_value_ebay) |> 
  pivot_longer(
    cols = -member,
    names_to = "facet_var",
    values_to = "value"
  ) |> 
  left_join(xmen_names) |> 
  select(-member) |> 
  mutate(names = paste0(full_name, "\n", superhero_name)) |> 
  select(names, facet_var, value, url)


#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#



#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Graduate", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Oswald", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
low_col <- "#fa0000"                  # Heat map: low colour
hi_col <- "#0800fa"                   # Heat map: high colour
bg_col <- "#e8e8e8"                   # Background Colour
text_col <- "#00004d"                 # Colour for the text
text_hil <- "#700000"                 # Colour for highlighted text

# Define Text Size
ts = unit(20, units = "cm")                             # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- ""
subtitle_text <- ""
subtitle_text <- ""
plot_subtitle <- paste(strwrap(subtitle_text, 150), collapse = "\n")

plot_caption <- paste0("**Data & Inspiration:** | ", "**Graphics:** ", social_caption_1, "**Code:**", social_caption_2)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

ggplot(
  data = df1,
  mapping = aes(
    x = reorder(names, -value),
    y = value
  )
) +
  geom_col() +
  facet_wrap(
    ~ facet_var,
    scales = "free_y",
    ncol = 1
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  )

  theme_minimal() +
  theme(
  panel.grid = element_blank(),
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = 1.5 * ts),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 3*ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_hil,
                                  margin = margin(4,0,2,0)),
  plot.subtitle    = element_text(hjust = 0,
                                  size = 1.6 * ts,
                                  family = "body_font",
                                  colour = text_col,
                                  margin = margin(5,0,2,0),
                                  lineheight = 0.35),
  plot.background =  element_rect(fill = bg_col,
                                  color = bg_col,
                                  linewidth = 0),
  axis.text = element_text(size = 1.5 * ts,
                           family = "body_font",
                           colour = text_col,
                           face = "bold",
                           margin = margin(0,0,0,0)),
  axis.title.x = element_text(size = 2 * ts,
                           family = "body_font",
                           colour = text_col,
                           margin = margin(0,0,0,0),
                           hjust = 1),
  axis.line.x = element_line(color = "grey",
                             arrow = arrow(length = unit(3, "mm"))),
  plot.title.position = "plot"
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_xmen_mutant.png"),
  plot = g,
  width = 20, 
  height = 20, 
  units = "cm",
  bg = bg_col
)
