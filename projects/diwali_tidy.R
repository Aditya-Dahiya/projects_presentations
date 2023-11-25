# Loading required libraries----------------------------------------------------
library(tidyverse)      # tidy tools data wrangling
library(ggtext)         # text into ggplot2
library(sf)             # maps and plotting
library(here)           # files location and loading
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using images in ggplot2
library(rvest)          # Get states population data
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2

# Loading the data
diwali <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv') |> 
  janitor::clean_names()

india_map <- st_read(here("data", "india_map", "StateBoundary.shp")) |> 
  mutate(state = str_to_title(state))


# Getting population Data from web scraping
state_pop <- rvest::read_html("https://www.indiacensus.net/") |> 
  html_nodes("table")

state_pop <- state_pop[1] |> 
  html_table() 

state_pop <- state_pop[[1]] |> 
  janitor::clean_names() |> 
  select(2, 4) |> 
  rename(
    state = state_name,
    population = estimated_population_in_2023
  )

state_pop <- state_pop |> 
  mutate(population = parse_number(population))

# Number of customers per capita and Avg. Purchase per customer
df1 <- diwali |> 
  count(state, sort = TRUE) |> 
  full_join(state_pop) |> 
  mutate(cust_m_pop = 1000000 * n / population) |> 
  arrange(desc(cust_m_pop)) |>
  rename(customers = n) |> 
  select(state, customers, cust_m_pop)

df2 <- diwali |> 
  group_by(state) |> 
  summarise(purchase = sum(amount, na.rm = TRUE)) |> 
  full_join(df1) |> 
  mutate(purc_cust = purchase / customers) |> 
  select(state, cust_m_pop, purc_cust) |> 
  mutate(
    state = case_when(
      state == "Jammu and Kashmir" ~ "Jammu & Kashmir",
      state == "Orissa" ~ "Odisha",
      .default = state)
  )
  
  
mapdf <- df2 |> 
  pivot_longer(cols = -state,
               names_to = "indicator", 
               values_to = "value") |> 
  full_join(india_map, relationship = "many-to-many") |> 
  filter(!is.na(indicator))


# Visualization parameters------------------------------------------------------
# Social Media Fonts
# Credits: https://nrennie.rbind.io/blog/adding-social-media-icons-ggplot2/

# Load fonts
font_add_google("Pragati Narrow")
font_add_google("Pacifico")
font_add_google("Roboto")
showtext_auto()
body_font <- "Roboto"              # Font for plot legends, body etc.
title_font <- "Pacifico"              # Font for titles, subtitles
caption_font <- "Pragati Narrow"   # Font for the caption

# Define colours
map1_col = c("yellow",             # Colours for Chloropleth g1
             "red")
map2_col = c("#cdeff7",          # Colours for Chloropleth g2 
             "#1f76f0")
ts = 45                            # Text Size
bg_col <- "white"                  # Background Colour
text_col <- "black"                # Colour for the text
text_hil <- "red"                  # Colour for highlighted text
  
# Add text to plot
plot_title <- "Diwali Sales: Insights"

plot_subtitle <- "#TidyTuesday. Insights about the Diwali sales data."

sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
mastodon <- "&#xf4f6"
mastodon_username <- "@adityadahiya@mastodon.social"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: #000000'>{github_username}  </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: #000000'>{xtwitter_username}</span>"
)
plot_caption <- paste0("**Data**: kaggle.com<br>", social_caption)

# Actual Plots------------------------------------------------------------------
g1 <- mapdf |> 
      filter(indicator == "cust_m_pop") |> 
      ggplot(aes(fill = value, 
                 geometry = geometry,
                 label = state)) +
      geom_sf() +
      geom_sf_text(aes(alpha = !is.na(value)),
                   size = ts/9) +
      coord_sf() +
      scale_fill_continuous(low = map1_col[1], 
                            high = map1_col[2],
                            na.value = bg_col,
                            trans = "log10") +
      scale_alpha_discrete(range = c(0, 1)) +
      guides(alpha = "none", fill = "colorbar") +
      
      ggthemes::theme_map() +
      labs(fill = "",
           subtitle = "Customer numbers (per million population)") +
      theme(plot.subtitle = element_text(size = ts/4,
                                      family = body_font,
                                      hjust = 0.5),
            legend.text = element_text(size = ts/2,
                                       family = body_font),
            legend.title = element_text(size = ts/2,
                                        family = body_font,
                                        vjust = 0.5),
            legend.position = "right",
            legend.background = element_rect(fill = NULL))

g2 <- mapdf |> 
      filter(indicator == "purc_cust") |> 
      ggplot(aes(fill = value, 
                 geometry = geometry,
                 label = state)) +
      geom_sf() +
      geom_sf_text(aes(alpha = !is.na(value)),
                   size = ts/9) +
      coord_sf() +
      scale_fill_continuous(low = map2_col[1], 
                            high = map2_col[2],
                            na.value = bg_col,
                            labels = scales::label_comma(prefix = "Rs."),
                            breaks = c(8000, 10000)) +
      scale_alpha_discrete(range = c(0, 1)) +
      guides(alpha = "none", fill = "colorbar") +
      
      ggthemes::theme_map() +
      labs(fill = "",
           subtitle = "Average customer spending (in Rupees)") +
      theme(plot.subtitle = element_text(size = ts/3,
                                      family = body_font,
                                      hjust = 0.5),
            legend.text = element_text(size = ts/2,
                                       family = body_font,
                                       vjust = 0.5),
            legend.title = element_text(size = ts/3,
                                         family = body_font),
            legend.position = "right",
            legend.background = element_rect(fill = NULL))
g3 <- diwali |> 
  count(age_group) |> 
  mutate(fill_var = age_group == "26-35") |> 
  ggplot(aes(x = n, y = age_group, fill = fill_var)) +
  geom_col() +
  labs(subtitle = "Maximum customers are in the 26-35 age group",
       y = "Customer Age Group",
       x = "Number of customers") +
  scale_x_continuous(labels = scales::label_number_si()) +
  scale_fill_manual(values = c("grey", "orange")) +
  cowplot::theme_minimal_vgrid() +
  theme(axis.ticks.y = element_blank(),
        panel.grid = element_line(linetype = 2),
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = ts/2),
        legend.position = "none",
        axis.text = element_text(size = ts/2, 
                                 family = body_font,
                                 vjust = 1),
        axis.title = element_text(size = ts/2, 
                                  family = body_font))


  # Create ordering of groups
  st_vec <- diwali |> count(state, sort = TRUE) |> pull(state) |> rev()
  pr_vec <- diwali |> count(product_category, sort = TRUE) |>  pull(product_category)

g4 <- diwali |> 
  count(state, product_category, wt = orders, sort = TRUE) |> 
  mutate(
    state = fct(state, levels = st_vec),
    product_category = fct(product_category, levels = pr_vec)
  ) |> 
  ggplot(aes(y = state, x = product_category, fill = n)) +
  geom_tile(col = "white") +
  geom_text(aes(label = n), size = ts/9) +
  scale_fill_gradient(low = "white", 
                      high = "red",
                      na.value = "white",
                      trans = "log2",
                      breaks = c(1, 10, 50, 200, 500)) +
  labs(x = NULL, y = NULL, 
       fill = "Number of products sold",
       subtitle = "Certain items are more popular in some states") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1,
                                   size = ts/3, 
                                   family = body_font),
        axis.text.y = element_text(size = ts/3, family = body_font),
        legend.position = "right",
        legend.title = element_text(angle = 90, 
                                    hjust = 0.5, vjust = 0.5,
                                    size = ts/4))


# Arrange Plots in final output-------------------------------------------------
library(patchwork)

g = (g3 + g4) / (g1 + g2) + 
  plot_layout(nrow = 2,
              heights = c(1, 3)) +
  plot_annotation(
    title = plot_title,
    subtitle = plot_subtitle
  ) &
  theme(plot.title = element_text(size = 2*ts,
                                  family = body_font,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = ts/1.5,
                                     hjust = 0.5,
                                     family = body_font))

# Saving the final image--------------------------------------------------------
ggsave(
    filename = here::here("docs", "diwali_tidy.png"),
    plot = g,
    device = "png", dpi = "retina", 
    width = 20, height = 20, units = "cm",
    bg = bg_col)
    )