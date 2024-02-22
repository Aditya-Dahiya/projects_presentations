# ==============================================================================#
# Library Load-in---------------------------------------------------------------
# ==============================================================================#
library(tidyverse)
library(openxlsx)
library(janitor)
library(rnaturalearth)
library(ggthemes)
library(scales)
library(sf)
library(patchwork)
library(fontawesome)
library(ggtext)
library(showtext)


# ==============================================================================#
# Data Load-in------------------------------------------------------------------
# ==============================================================================#
usdef <- openxlsx::read.xlsx("https://www.dsca.mil/sites/default/files/EDA_Public_Report_2020-06-15.xlsx") |> 
  as_tibble()


# ==============================================================================#
# Data Wrangling----------------------------------------------------------------
# ==============================================================================#

df <- usdef |> 
  clean_names() |> 
  select(fiscal_year_of_request,
         country_transfer_to,
         total_delivered_qty,
         total_current_value) |> 
  mutate(total_delivered_qty = as.numeric(total_delivered_qty)) |> 
  filter(!is.na(total_delivered_qty)) |> 
  filter(total_delivered_qty >= 1)

countries <- df |> 
  group_by(country_transfer_to) |> 
  summarise(total_current_value = sum(total_current_value)) |> 
  arrange(desc(total_current_value))

select_con <- countries |> 
  slice_head(n = 10) |> 
  pull(country_transfer_to)

df1 <- df |> 
  mutate(country = if_else(
    country_transfer_to %in% select_con,
    country_transfer_to,
    "Others"
  )) |> 
  group_by(fiscal_year_of_request, country) |> 
  summarize(total = sum(total_current_value))


df2 <- usdef |> 
  clean_names() |> 
  select(-c(
    transfer_authority, implementing_agency, item_description, status, status_date,
    qty_accepted, qty_allocated, qty_rejected, total_acquisition_value
  )) |> 
  mutate(total_delivered_qty = as.numeric(total_delivered_qty)) |> 
  mutate(total_delivered_qty = if_else(
    (is.na(total_delivered_qty) | total_delivered_qty == 0),
    0,
    total_delivered_qty
  )) |> 
  mutate(total_current_value = if_else(
    total_delivered_qty == 0,
    1,
    total_current_value
  )) |> 
  rename(year = fiscal_year_of_request,
         country = country_transfer_to) |> 
  group_by(country) |> 
  summarise(total_value = sum(total_current_value))


mapdf <- ne_countries(
  type = "countries",
  continent = c("asia", "africa", "europe", "oceania", 
                "north america", "south america"),
  returnclass = "sf",
  scale = "large"
) |> 
  st_transform(crs = st_crs("ESRI:54030")) |> 
  mutate(country = admin)

# ==============================================================================#
# Options & Visualization Parameters--------------------------------------------
# ==============================================================================#

# Load fonts
font_add_google("Gotu",
                family = "title_font"
) # Font for titles
font_add_google("Saira Extra Condensed",
                family = "caption_font"
) # Font for the caption
font_add_google("Abel",
                family = "body_font"
) # Font for plot text
showtext_auto()

# Define colours
bg_col <- "#fff1d4"  # Background Colour
text_col <- "#753501" # Colour for the text
text_hil <- "#b85200" # Colour for highlighted text

# Define Text Size
ts <- unit(10, units = "cm") # Text Size

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
plot_title <- "USA's Excess Defense Articles' global sales"
plot_caption <- paste0("**Data:** CRS Report for Congress. Excess Defense Articles: Grants and Sales to Allies and Friendly Countries", " | ", " **Code:** ", social_caption_1, " | ", " **Graphics:** ", social_caption_2)
subtitle_text <- "This map visualizes the distribution of U.S. Excess Defense Articles (EDA) sales at reduced prices to allied and friendly nations. Highlighting top recipients like Israel, Morocco, and the UAE, the graph illustrates the strategic allocation of military equipment to support modernization efforts, peacekeeping missions, and counter-narcotics operations worldwide."
plot_subtitle <- str_wrap(subtitle_text, width = 170)
# ==============================================================================#
# Data Visualization------------------------------------------------------------
# ==============================================================================#

# Combining the World Map data and Sales Data-----------------------------------

g1 <- mapdf |> 
  left_join(
    df2,
    by = join_by(country == country)
  ) |> ggplot(aes(
    fill = total_value
  )) +
  geom_sf(linewidth = 0.1) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Orange",
    na.value = "white",
    trans = "log10",
    labels = scales::label_number(
      prefix = "$",
      scale_cut = cut_short_scale()
    ),
    name = "Total Value of items sold (2010-2019)") +
  guides(
    fill = guide_colorbar(
      title = "Total Value of items sold (2010-2019)",
      title.position = "top",
      title.theme = element_blank(),
      label.theme = element_text(
        family = "body_font",
        colour = text_col,
        size = 3 * ts,
        margin = margin(0,0,0,0)
      ),
      title.hjust = 0.5,
      barwidth = unit(7, "cm"),
      barheight = unit(0.3, "cm"),
      direction = "horizontal",
      frame.colour = bg_col
    )
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  theme_map() +
  theme(legend.position = c(0.45, 0.01)) +
  theme(
    plot.caption = element_textbox(
      family = "caption_font",
      hjust = 0.5,
      colour = text_col,
      size = 3 * ts,
      margin = margin(0.4, 0, 0, 0.5,
                      unit = "cm"
      )
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 11 * ts,
      family = "title_font",
      face = "bold",
      colour = text_hil,
      margin = margin(0, 0, 0.5, 0,
                      unit = "cm"
      )
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 4 * ts,
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
    plot.margin = margin(0,0,0,0),
    legend.background = element_rect(
      fill = bg_col,
      color = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = bg_col
    )
  )


g2 <- countries |> 
  slice_max(order_by = total_current_value, n = 10) |> 
  mutate(country_transfer_to = if_else(
    country_transfer_to == "United Arab Emirates",
    "UAE",
    country_transfer_to
  )) |> 
  ggplot(aes(x = total_current_value,
             y = reorder(country_transfer_to,
                         -total_current_value),
             label = country_transfer_to,
             fill = total_current_value)) +
  geom_col(col = "black",
           linewidth = 0.1) +
  geom_text(
    aes(x = 1e6),
    hjust = 0,
    vjust = 0.5,
    family = "caption_font",
    size = 1 * ts,
    color = "white"
  ) +
  scale_x_continuous(
    expand = expansion(0, 0),
    labels = label_number(
      prefix = "$",
      scale_cut = cut_short_scale(),
    ),
    breaks = c(0, 2e8, 4e8),
    limits = c(0, 4.5e8)
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Orange",
    na.value = "white",
    trans = "log10",
    labels = scales::label_number(
      prefix = "$",
      scale_cut = cut_short_scale()
    ),
    limits = c(1, 4e8)
    ) +
  labs(title = "Highest sales") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(0,0,0,0),
    plot.title = element_text(
      hjust = 0,
      size = 8 * ts,
      family = "title_font",
      colour = text_hil,
      margin = margin(0, 0, 0.2, 0,
                      unit = "cm")
    ),
    axis.text.x = element_text(
      family = "body_font",
      size = 3 * ts,
      color = text_col,
      hjust = 0
    ),
    axis.ticks.x = element_line()
  )

g <- g1 + 
  patchwork::inset_element(
    g2,
    top = 0.54,
    left = 0,
    right = 0.35,
    bottom = 0
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = bg_col,
        colour = bg_col
      )
    )
  )

# =============================================================================#
# Image Saving-----------------------------------------------------------------
# =============================================================================#


ggsave(
  filename = here::here("docs", "dip_us_arms_export.png"),
  plot = g,
  width = 30,
  height = 18,
  units = "cm",
  bg = bg_col
)



