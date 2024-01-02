#=================An Attempt at Shadowed Designer Bar Chart====================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data wrangling and plotting
library(osmdata)        # Wrapper for Overpass API from Open Street Maps
library(janitor)        # Cleaning names
library(sf)             # For plotting maps
library(here)           # Files location and loading
library(paletteer)      # Lots of Color Palettes in R
library(colorspace)     # Lightening and Darkening Colors
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(patchwork)      # For compiling plots
library(magick)         # Work with Images and Logos
library(ggimage)        # Background Image


#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

del_mov <- read_csv("https://raw.githubusercontent.com/HarshaDevulapalli/indian-movie-theatres/master/indian-movie-theatres.csv") |> 
  filter(city == "Delhi")

main_roads <- st_read(here::here("data", 
                                 "delhi_osm",
                                 "delhi_main_roads.shp"))
st_crs(main_roads) <- "WGS 84"

minor_roads <- st_read(here::here("data", 
                                  "delhi_osm",
                                  "delhi_minor_roads.shp")) 
st_crs(minor_roads) <- "WGS 84"

very_minor_roads <- st_read(here::here("data", 
                                       "delhi_osm",
                                       "delhi_veryminor_roads.shp")) 
st_crs(very_minor_roads) <- "WGS 84"

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

coords <- del_mov |> 
  summarise(
    top = max(lat),
    bottom = min(lat),
    left = min(lon),
    right = max(lon)
  ) |> 
  as_vector()

# Adjust to remove the one leftmost (westward) cniema hall - outlier
percentage_removal_factor = 0.2
coords[3] <- coords[3] + 
  ((coords[4] - coords[3]) * percentage_removal_factor)

# Impute average value to NAs

impute_na <- median(del_mov$average_ticket_price, na.rm = TRUE)
del_mov <- del_mov |> 
  mutate(average_ticket_price = 
           if_else(is.na(average_ticket_price),
                   impute_na,
                   average_ticket_price))


#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Limelight", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Bree Serif", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Colour Palette
mypal_c <- paletteer::scale_colour_paletteer_c("ggthemes::Purple")
mypal <- paletteer::paletteer_d("rcartocolor::Purp")

# Define colours
low_col <- mypal[4]                   # Low colour
hi_col <- mypal[6]                    # High colour
bg_col <- mypal[3] |> lighten(0.9)    # Background Colour
text_col <- mypal[1] |> darken(0.6)   # Colour for the text
text_hil <- mypal[6] |> darken(0.4)   # Colour for the title

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
plot_title <- "Movie Theatres in Delhi"

subtitle_text <- "Most theatres are in North or South Delhi, with a central vacant band around Lutyens Delhi !"
plot_subtitle <- paste(strwrap(subtitle_text, 100), collapse = "\n")

plot_caption <- paste0("**Data:** Harsha Devulapalli  |  ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#




  
g <- ggplot() +
  geom_sf(
    data = 
      main_roads |> 
      mutate(geometry = st_simplify(
        geometry, 
        dTolerance = 50,
        preserveTopology = TRUE)),
    mapping = aes(geometry = geometry),
    color = low_col,
    linewidth = 1,
    alpha = 0.4) +
  geom_sf(
    data = 
      minor_roads |> 
      mutate(geometry = st_simplify(
        geometry, 
        dTolerance = 1,
        preserveTopology = TRUE)),
    color = low_col,
    linewidth = 0.7,
    alpha = 0.3) +
  geom_sf(
    data = 
      very_minor_roads |> 
      mutate(geometry = st_simplify(
        geometry, 
        dTolerance = 10,
        preserveTopology = TRUE)),
    color = low_col,
    linewidth = 0.3,
    alpha = 0.2) +
  geom_point(
    data = del_mov,
    mapping = aes(
      x = lon,
      y = lat,
      size = total_seats,
      fill = average_ticket_price
    ),
    pch = 21,
    color = text_hil,
    alpha = 0.6
  ) +
  ggrepel::geom_text_repel(
    data = del_mov,
    mapping = aes(
      x = lon,
      y = lat,
      label = theatre_name
    ),
    alpha = 0.95,
    family = "body_font",
    colour = text_col,
    seed = 42,
    size = 10,
    segment.color = text_col
  ) +
  coord_sf(
    xlim = coords[c("left", "right")],
    ylim = coords[c("bottom", "top")],
    expand = FALSE) +
  scale_fill_paletteer_c("ggthemes::Purple") +
  scale_size_continuous(range = c(1, 15)) +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       fill = "Average Ticket Price (in Rs.)",
       size = "Total Number of Seats") +
  theme_void() + 
  guides(fill = guide_colorbar(title.position = "top",
                               barheight = unit(0.5, "cm"),
                               barwidth = unit(8, "cm")),
         size = guide_legend(title.position = "top",
                             keywidth = unit(0.5, "cm"),
                             keyheight = unit(0.5, "cm"),
                             label.hjust = 0)) +
  theme(
    plot.caption =  element_textbox(family = "caption_font",
                                    hjust = 0.5,
                                    colour = text_col,
                                    size = unit(40, "cm")),
    plot.title   =     element_text(hjust = 0.5,
                                    size = unit(175, "cm"),
                                    margin = margin(0.3,0,0.2,0, 
                                                    unit = "cm"),
                                    family = "title_font",
                                    face = "bold",
                                    colour = text_col),
    plot.subtitle    = element_text(hjust = 0.5,
                                    size = unit(50, "cm"),
                                    family = "body_font",
                                    colour = text_col,
                                    margin = margin(0,0,0.2,0, 
                                                    unit = "cm")),
    plot.background =  element_rect(fill = bg_col,
                                    color = bg_col,
                                    linewidth = 0),
    legend.position = "bottom",
    legend.text = element_text(hjust = 0.5,
                               size = unit(40, "cm"),
                               family = "body_font",
                               colour = text_col),
    legend.title = element_text(hjust = 0.5,
                                size = 50,
                                family = "body_font",
                                colour = text_col,
                                margin = margin(0,0,0,0)),
    legend.box.margin = margin(0,0,0.5,0, unit = "cm"),
    legend.box = "horizontal",
    legend.spacing.y = unit(0.2, "cm")
  )




#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "delhimovies_tidy.png"),
  plot = g,
  device = "png", 
  dpi = "retina", 
  width = unit(10, "cm"), 
  height = unit(10, "cm"),
  bg = bg_col
)



#=============================================================================#
# Data Collection Work---------------------------------------------------------
#=============================================================================#


###########################################
# DO NOT RUN CODE: To download initial Delhi data
###########################################

# Saving the coordinates bounding box for Delhi Map
coords <- del_mov |> 
  summarize(
    top = max(lat),
    bottom = min(lat),
    left = min(lon),
    right = max(lon)
  ) |> 
  as_vector()

coords

Code used for Delhi area: Downloading the Delhi map (1.4 GB !!) 
cty <- opq(bbox = coords)


cty_roads <- cty |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

main_roads <- cty_roads$osm_lines |>
  filter(highway %in% c("primary", "trunk")) |> 
  clean_names()

minor_roads <- cty_roads$osm_lines |>
  filter(highway %in% c("tertiary", "secondary"))

very_minor_roads <- cty_roads$osm_lines |>
  filter(highway %in% c("residential"))


st_write(
  obj = main_roads |> select(geometry),
  dsn = here::here("data", "delhi_main_roads.shp"),
  append = FALSE
)

st_write(
  obj = minor_roads |> select(geometry),
  dsn = here::here("data", "delhi_minor_roads.shp"),
  append = FALSE
)

st_write(
  obj = very_minor_roads |> select(geometry),
  dsn = here::here("data", "delhi_veryminor_roads.shp"),
  append = FALSE
)

# rm(main_roads)
# rm(minor_road)