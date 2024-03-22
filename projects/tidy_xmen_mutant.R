#================= #TidyTuesday : X-Men Mutant Moneyball ======================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse) # Data Wrangling and Plotting
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
library(cropcircles)    # Circular cropping of the image
library(cowplot)        # Adding images to x-axis

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


#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#


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
  select(names, facet_var, value, url, superhero_name)

f2 <- c(
  "Total Number of Issues each character appeared in",
  "Total value of character's issues (Heritage highest sale, in US Dollars)",
  "Total value of character's issues eBay sales (2022) in US Dollars"
)

names(f2) <- df1 |> 
  distinct(facet_var) |> 
  pull(facet_var)

# Making logos for characters to display on the x-axis
mk_logo <- function(url){
  image_read(url) |> 
    image_resize("x300") |> 
    circle_crop(border_size = 2, 
                border_colour = text_col) |> 
    image_read()
}
df1 |> distinct(superhero_name)

xaxis_labels <- c(
  Beast = "<img src = {mk_logo(df1$url[1])} width = '100' /> <br> {df1$names[1]}",
  Cyclops = "<img src = {mk_logo(df1$url[2])} width = '100' /> <br> {df1$names[2]}",
  Phoenix = "<img src = {mk_logo(df1$url[3])} width = '100' /> <br> {df1$names[3]}",
  Storm = "<img src = {mk_logo(df1$url[4])} width = '100' /> <br> {df1$names[4]}",
  Nightcrawler = "<img src = {mk_logo(df1$url[5])} width = '100' /> <br> {df1$names[5]}",
  Wolverine = "<img src = {mk_logo(df1$url[6])} width = '100' /> <br> {df1$names[6]}",
  Magneto = "<img src = {mk_logo(df1$url[7])} width = '100' /> <br> {df1$names[7]}",
  `Professor X` = "<img src = {mk_logo(df1$url[8])} width = '100' /> <br> {df1$names[8]}"
)
  
  
#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Trade Winds", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Zen Dots", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("MoMAColors::Dali")
mypal

# Define colours
text_col <- "#DC3B34FF"               # Colour for the text
text_hil <- "#DC3B34FF"               # Colour for highlighted text
bg_col <- "black"                     # Background Colour

# Define Text Size
ts = unit(40, units = "cm")           # Text Size

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
plot_title <- "X-Men Mutant Moneyball"
subtitle_text <- "Comparison of the main characters in X-Men Mutant comic-books from 1960s to 1990s"
plot_subtitle <- str_wrap(subtitle_text, 100)

plot_caption <- paste0("**Data & Inspiration:** X-Men Mutant Moneyball by Anderson Evans |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g <- ggplot(
    data = df1,
    mapping = aes(
      x = reorder(superhero_name, -value),
      y = value,
      fill = superhero_name
    )
  ) +
  geom_col() +
  facet_wrap(
    ~ facet_var,
    scales = "free_y",
    ncol = 1,
    labeller = as_labeller(f2)
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  scale_x_discrete(
    labels = xaxis_labels
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  scale_fill_manual(values = mypal) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.caption =  element_textbox(family = "caption_font",
                                    hjust = 0.5,
                                    colour = text_col,
                                    size = 1.8 * ts),
    plot.title   =     element_text(hjust = 0.5,
                                    size = 6 * ts,
                                    family = "title_font",
                                    face = "bold",
                                    colour = text_hil,
                                    margin = margin(2, 0, 0, 0, "cm")),
    plot.subtitle    = element_text(size = 3 * ts,
                                    family = "caption_font",
                                    colour = text_col,
                                    margin = margin(1, 0, 1, 0, "cm"),
                                    lineheight = 0.35,
                                    hjust = 0.5),
    plot.background =  element_rect(fill = bg_col,
                                    color = bg_col,
                                    linewidth = 0),
    axis.text.y = element_text(size = 2.5 * ts,
                             family = "caption_font",
                             colour = text_col,
                             face = "bold",
                             margin = margin(0, 0, 0, 0, "cm")),
    axis.text.x = element_markdown(size = 2 * ts,
                               family = "caption_font",
                               lineheight = 0.25,
                               colour = text_col,
                               hjust = 1,
                               margin = margin(0, 0, 0, 0),
                               vjust = 0.5,
                               face = "bold"),
    strip.text = element_text(size = 4 * ts,
                              family = "caption_font",
                              colour = text_hil,
                              margin = margin(1.2, 0, 0.4, 0, "cm"),
                              hjust = 0.5),
    plot.title.position = "plot",
    legend.position = "none"
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_xmen_mutant.png"),
  plot = g,
  width = 40, 
  height = 55, 
  units = "cm",
  bg = bg_col
)










# Not to Run
scale_fac = 0.9

pimage <- axis_canvas(
  g, 
  axis = "x") + 
  draw_image(mk_logo(df1$url[1]), x = 0.5, scale = scale_fac) +
  draw_image(mk_logo(df1$url[2]), x = 1.5, scale = scale_fac) +
  draw_image(mk_logo(df1$url[3]), x = 2.5, scale = scale_fac) +
  draw_image(mk_logo(df1$url[4]), x = 3.5, scale = scale_fac) +
  draw_image(mk_logo(df1$url[5]), x = 4.5, scale = scale_fac) +
  draw_image(mk_logo(df1$url[6]), x = 5.5, scale = scale_fac) +
  draw_image(mk_logo(df1$url[7]), x = 6.5, scale = scale_fac) +
  draw_image(mk_logo(df1$url[8]), x = 7.5, scale = scale_fac)


# insert the image strip into the plot
g2 <- ggdraw(
  insert_xaxis_grob(
    g, 
    pimage, 
    position = "bottom",
    height = unit(25, "mm")
  )
)

