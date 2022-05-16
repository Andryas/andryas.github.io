library(aw)
library(sf)
library(rgdal)
library(tidyverse)

options(dplyr.width = Inf)

readRenviron("~/Documents/scrapingAW/.env")

m <- mongolite::mongo(
  collection = "centris_ca",
  db = Sys.getenv("MONGO_DATABASE"),
  url = Sys.getenv("MONGO_URI")
)
data <- m$find()
m$disconnect()
data <- as_tibble(data)

glimpse(data)

data |>
  group_by(category) |>
  count(sort = TRUE) |>
  print(n = 100)

data <- data |>
  filter(
    category %in% c(
      "house for sale",
      "house for rent",

      "condo for sale",
      "condo apartment for sale",

      "condo apartment for rent",

      "duplex for sale",
      "triplex for sale",
      "quadruplex for sale",
      "quintuplex for sale"
    )
  )

features <- as_tibble(data$features)

colnames(features) <- colnames(features) |>
  tolower() |>
  str_replace_all("[[:space:]|[:punct:]]+", " ") |>
  trimws() |>
  str_replace_all("\\s+", "_")

attributes <- map(data$attributes$data$attributes, ~ .x[["value"]]) |>
  as_tibble()
geo <- data$location$geocode |>
  mutate_all(as.numeric)

agents <- map2(data$agents, data$url, function(.x, .y) {
  .x$url <- .y
  .x
}) |>
  bind_rows() |>
  as_tibble()

data <- data |>
  select(-features, -attributes, -location, -agents) |>
  bind_cols(features |>
    select(walkscore, year_built, lot_area, parking_total),
  attributes,
  geo) |>
  mutate(
    category = case_when(
      category == "condo apartment for sale" ~ "condo for sale",
      category == "condo apartment for rent" ~ "condo for rent",
      TRUE ~ category
    ),
    year_built = as.integer(str_extract(year_built, "[0-9]+")),
    lot_area_sqft = as.numeric(str_replace(str_extract(lot_area, "[0-9,]+"), ",", "")),
    lot_area_m2 = lot_area_sqft / 10.764,
    parking_driveway = str_extract(parking_total, "driveway \\([0-9]+\\)"),
    parking_driveway = as.integer(str_extract(parking_driveway, "[0-9]+")),
    parking_garage = str_extract(parking_total, "garage \\([0-9]+\\)"),
    parking_garage = as.integer(str_extract(parking_garage, "[0-9]+")),
    parking_carport = str_extract(parking_total, "carport \\([0-9]+\\)"),
    parking_carport = as.integer(str_extract(parking_carport, "[0-9]+")),

    bedrooms_basement = as.integer(str_extract(bedrooms, "[0-9]+(?= in basement)")),
    bedrooms_basement = replace_na(bedrooms_basement, 0),
    bedrooms = as.integer(str_extract(bedrooms, "[0-9]+(?= bedroom)")),
    bedrooms = replace_na(bedrooms, 0),
    bedrooms = bedrooms - bedrooms_basement,

    powder_rooms = as.integer(str_extract(bathrooms, "[0-9]+(?= powder room)")),
    powder_rooms = replace_na(powder_rooms, 0),
    bathrooms = as.integer(str_extract(bathrooms, "[0-9]+(?= bathroom)")),
    bathrooms = replace_na(bathrooms, 0),

    rooms = as.integer(str_extract(rooms, "[0-9]+")),
    rooms = replace_na(rooms, 0),

    tipology = trimws(str_replace_all(category, "for sale|for rent", "")),
    walkscore = as.integer(walkscore)
  ) |>
  select(-parking_total, -lot_area, -description, -startPosition, -created_at)

montreal <- readOGR("content/post/montreal-real-estate/data/limadmin-shp/LIMADMIN.shp") |>
  st_as_sf(crs = 4326)

neigh <- data |>
  select(url, lng, lat)

neigh$neighbourhood <- apply(neigh, 1, function(row) {
  x_sf <- st_sfc(st_point(as.numeric(c(row[["lng"]], row[["lat"]]))), crs = 4326)
  montreal[which(st_intersects(x_sf, montreal, sparse = FALSE)), ]$NOM
})

neigh$neighbourhood <- unlist(map(neigh$neighbourhood, ~ if (length(.x) == 0) NA else unlist(.x)))

data_montreal <- data |>
  inner_join(
    neigh |>
      select(url, neighbourhood) |>
      filter(!is.na(neighbourhood))
  )

# data_montreal |>
#   distinct(url, .keep_all = TRUE)

# ONLY NEIGHBOURHOODS WITH 50 OBSEVARTION OR MORE.
data_montreal <- data_montreal |>
  filter(neighbourhood %in%
      (data_montreal |>
        group_by(neighbourhood) |>
        count() |>
        ungroup() |>
        filter(n > 50) |>
        pull(neighbourhood)))

agents_montreal <- agents |>
  filter(url %in% unique(data_montreal$url))

saveRDS(data_montreal, "content/post/montreal-real-estate/data/properties.rds")
saveRDS(agents_montreal, "content/post/montreal-real-estate/data/agents.rds")
