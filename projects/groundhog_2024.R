library(tidyverse)
library(here)
library(fs)
library(httr2)

working_dir <- here::here("data")

groundhogs_all <- httr2::request("https://groundhog-day.com/api/v1/groundhogs/") |> 
  httr2::req_perform() |> 
  httr2::resp_body_json() |> 
  _$groundhogs |> 
  tibble::tibble(data = _) |> 
  tidyr::unnest_wider(data)

groundhogs <- groundhogs_all |> 
  dplyr::select(
    -predictions,
    -successor, # Only 1 groundhog has a successor, we'll mention it in the post.
    -contact
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    # At least one has a stray comma at the end
    coordinates = stringr::str_remove(coordinates, ",$")
  ) |> 
  tidyr::separate_wider_delim(
    coordinates,
    ",",
    names = c("latitude", "longitude")
  ) |> 
  dplyr::mutate(
    # Correct types
    latitude = as.double(latitude),
    longitude = as.double(longitude),
    is_groundhog = as.logical(is_groundhog),
    active = as.logical(active)
  )

predictions <- groundhogs_all |> 
  dplyr::select(id, predictions) |> 
  tidyr::unnest_longer(predictions) |> 
  tidyr::unnest_wider(predictions) |> 
  dplyr::mutate(shadow = as.logical(shadow))

readr::write_csv(
  groundhogs,
  fs::path(working_dir, "groundhogs.csv")
)
readr::write_csv(
  predictions,
  fs::path(working_dir, "predictions2024.csv")
)
