#============= #TidyTuesday : Du Bois Visualization Challenge 2024 ============#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(janitor)        # Cleaning names etc. of messy dataset
library(glue)           # To paste together text for ggtext

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(scales)         # Scale Labels in ggplot2
library(patchwork)      # For compiling plots
library(magick)         # Work with Images and Logos

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load(2024, week = 14)
dubois <- tuesdata$dubois_week10
rm(tuesdata)

text_title <- "A series of statistical charts illustrating the condition of the descendants of former african slaves now resident in the United States of America."
text_title_french <- "Une série de cartes et diagrammes statistiques montrant la condition présente des descendants d'anciens esclaves africains résidant désormais aux États-Unis d'Amérique."

text1 <- c("Prepared and executed by african-american students under the direction of Atlanta University, Atlanta, GA, United States of America.")
text1_french <- c("Préparé et exécuté par des étudiants afro-américains sous la direction de l'Université d'Atlanta, Atlanta, GA, États-Unis d'Amérique.")

text2 <- "The University was founded in 1867. It has instructed 6000 African-American students."
text2_french <- "L'université a été fondée en 1867. Elle a donné l'instruction a'6000 étudiants afro-américains."

text3 <- "It has graduated 330 African American, among whom are:"
text3_french <- "Elle a délivre des diplômés a 330 afro-américains, parmi lesquels:"

text4 <- "The Univeristy has 20 professor and instructors and 250 students at present. It has five buildings, 60 acres of campus, and a library of 11,000 volumes. It aims to raise and civilize the sons of the freedmen by training their more capable members in the liberal arts according to the best standards of the day.\nThe proper accomplishment of this work demands an endownment fund of $500,000."
text4_french <- "L'Université compte actuellement 20 professeurs et instructeurs et 250 étudiants. Il compte cinq bâtiments, 60 acres de campus et une bibliothèque de 11 000 volumes. Il vise à élever et à civiliser les fils des affranchis en formant leurs membres les plus compétents aux arts libéraux selon les meilleurs standards du moment.\nLa bonne réalisation de ce travail nécessite un fonds de dotation de 500 000 $."

english_labels <- c(
  "Teachers",
  "Ministers",
  "Government Service",
  "Business",
  "Other Professions",
  "House Wives"
)

french_labels <- c(
  "Professeurs et Instituteurs",
  "Ministres de l'evangile",
  "Employés du Gouvernment",
  "Marchands",
  "Medons, Advocats, et étudcants",
  "Méres de famille"
  )

#==============================================================================#
# Exploratory Data Analysis & Data Wrangling -----------------------------------
#==============================================================================#
 
# library(summarytools)
# dfSummary(results) |> view()




#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Six Caps", 
                family = "caption_font")     # Font for the caption
font_add_google("Cinzel", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- c(
  "#db2a21",
  "#72a1ba",
  "#f2c4ed",
  "#96795f",
  "#577355",
  "#fccf3a"
)

# Define colours
txcl1 <- "black"               # Colour for the text
txcl2 <- "#c41202"             # Colour for highlighted text
bg_col <-   "#fae4be"          # Background Colour
text_col <- "brown"

# Define Text Size
ts = 20                        # Text Size

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
plot_caption <- paste0("**Data & Inspiration:** Du Bois Visualization Challenge 2024 |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

base_theme <- function(...){
  theme_void(
    base_family = "body_font",
    base_size = ts
  ) +
  theme(
    plot.background = element_rect(
      fill = bg_col,
      colour = NA
    ),
    plot.margin = margin(0,0,0,0),
    ...
    )
}

my_annotation <- function(label_text, wrap_length, 
                          colour, size = size, 
                          lineheight = 0.45, ...){
  annotate(
    geom = "text",
    x = 0 , y = 0,
    label = str_wrap(label_text, wrap_length),
    hjust = 0.5,
    size = size,
    family = "body_font",
    lineheight = lineheight,
    colour = colour,
    margin = margin(0,0,0,0)
  )
}
  
# Map of USA
library(usmap)
set.seed(42)

plot2 <- us_map() |> 
  mutate(fill_var = sample(1:6, size = 51, replace = T)) |> 
  ggplot(aes(fill = as_factor(fill_var))) +
  geom_sf(colour = txcl1) +
  scale_fill_manual(values = mypal) +
  labs(caption = "Centre for African-American Population\nAtlanta University") +
  cowplot::theme_map() +
  theme(
    legend.position = "none",
    plot.caption = element_text(
      hjust = 0,
      family = "text_font",
      size = ts,
      margin = margin(l = 4, unit = "cm")
    ),
    plot.background = element_rect(
      fill = bg_col, 
      colour = NA
    )
  )


plott1 <- ggplot() +
  my_annotation(text_title, 70, txcl1, size = 2 * ts, 
                lineheight = 0.25) +
  base_theme()

plott2 <- ggplot() +
  my_annotation(text_title_french, 70, size = 2 * ts, 
                lineheight = 0.25, colour = txcl2) +
  base_theme()


plot1 <- ggplot() +
  my_annotation(text1, 30, size = ts, colour = txcl1) +
  base_theme()

plot3 <- ggplot() +
  my_annotation(text1_french, 30, size = ts, colour = txcl2) +
  base_theme()

plot4 <- ggplot() +
  my_annotation(text2, 70, size = ts, colour = txcl1) +
  base_theme()

plot5 <- ggplot() +
  my_annotation(text2_french, 70, size = ts, colour = txcl2) +
  base_theme()

dubois <- dubois |> 
  mutate(Occupation = fct(Occupation, levels = english_labels))


plot6 <- ggplot(
  data = dubois,
  mapping = aes(
    x = 1, 
    y = Percentage,
    fill = Occupation,
    label = paste0(Percentage, "%"))
) +
  geom_col(colour = "black") +
  geom_text(
    aes(x = 1.3),
    position = position_stack(vjust = 0.5),
    fontface = "bold"
  ) +
  coord_polar(
    theta = "y",
    start = (pi * 90/180),
    direction = -1
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(shape = 18)
    )
  ) +
  scale_x_continuous(expand = expansion(0)) +
  scale_fill_manual(values = mypal) +
  theme_void()

plot6

design <- c(
  "
  AAAA
  BBBB
  CDDE
  CDDE
  CDDE
  FFFF
  GGGG
  "
)

g1 <- plott1 + plott2 + plot1 + plot2 + plot3 + plot4 + plot5 +
  plot_layout(design = design)


#==============================================================================#
# Image Saving -----------------------------------------------------------------
#==============================================================================#

ggsave(
  filename = here::here("docs", "tidy_dubois.png"),
  plot = g1,
  width = 40, 
  height = 25, 
  units = "cm",
  bg = bg_col
)
  