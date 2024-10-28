options(width = 80)

library(httr2)
library(jsonlite)
library(glue)
library(stringr)
library(tidygeocoder)
library(elmer)

#' Get station information from NOAA API using lat/lon coordinates
#' @param lat Latitude
#' @param lon Longitude
#' @return List containing station details
get_nearest_station <- function(lat, lon) {
  # Make request to NOAA stations endpoint
  response <- request(glue("https://api.weather.gov/points/{lat},{lon}")) |>
    req_headers(
      "Accept" = "application/json",
      "User-Agent" = "R Weather App (contact@example.com)"
    ) |>
    req_perform() |>
    resp_body_json()

  # Extract relevant station data
  list(
    station_id = response$properties$gridId,
    grid_x = response$properties$gridX,
    grid_y = response$properties$gridY,
    forecast_url = response$properties$forecast,
    hourly_forecast_url = response$properties$forecastHourly,
    location = if (!is.null(response$properties$relativeLocation)) {
      response$properties$relativeLocation$properties$city
    } else {
      "Unknown"
    }
  )
}

#' Get coordinates for a city using tidygeocoder
#' @param city The name of the city
#' @param state State name or abbreviation (required for US locations)
#' @return List containing lat, lon and formatted location name
get_coordinates <- function(city, state) {
  if (missing(state)) {
    stop("State parameter is required for US locations")
  }

  # Format address for geocoding
  address <- paste(city, state, "USA", sep = ", ")

  # Get coordinates using tidygeocoder (uses Nominatim/OpenStreetMap by default)
  result <- geo(address = address, method = "osm")

  # Check if location was found
  if (is.na(result$lat) || is.na(result$long)) {
    stop(glue("Location not found: {city}, {state}. Please check spelling."))
  }

  list(
    lat = result$lat,
    lon = result$long,
    location = paste(city, state, sep = ", ")
  )
}

#' Get current weather for a location using NOAA API
#' @param city The name of the city
#' @param state State name or abbreviation (required for US locations)
#' @return Current weather conditions
get_weather <- function(city, state) {
  # Get coordinates first
  coords <- get_coordinates(city, state)

  # Get nearest weather station
  station <- get_nearest_station(coords$lat, coords$lon)

  # Get current conditions
  response <- request(station$forecast_url) |>
    req_headers(
      "Accept" = "application/json",
      "User-Agent" = "R Weather App (contact@example.com)"
    ) |>
    req_perform() |>
    resp_body_json()

  # Extract current period
  current <- response$properties$periods[[1]]

  # Convert temperature to numeric if it's not already
  temp <- as.numeric(gsub("[^0-9.-]", "", current$temperature))

  # Return formatted weather data
  list(
    location = coords$location,
    coordinates = list(lat = coords$lat, lon = coords$lon),
    temp = temp, # Already in Fahrenheit from NOAA
    conditions = current$shortForecast,
    wind_speed = current$windSpeed,
    wind_direction = current$windDirection
  )
}

#' Get forecast for a location using NOAA API
#' @param city The name of the city
#' @param state State name or abbreviation (required for US locations)
#' @return 7-day forecast data
get_forecast <- function(city, state) {
  # Get coordinates first
  coords <- get_coordinates(city, state)

  # Get nearest weather station
  station <- get_nearest_station(coords$lat, coords$lon)

  # Get forecast data
  response <- request(station$forecast_url) |>
    req_headers(
      "Accept" = "application/json",
      "User-Agent" = "R Weather App (contact@example.com)"
    ) |>
    req_perform() |>
    resp_body_json()

  # Process forecast periods (NOAA provides 7-day forecast)
  forecasts <- lapply(response$properties$periods, function(period) {
    # Convert temperature to numeric if it's not already
    temp <- as.numeric(gsub("[^0-9.-]", "", period$temperature))

    list(
      location = coords$location,
      coordinates = list(lat = coords$lat, lon = coords$lon),
      date = period$startTime,
      name = period$name,
      temp = temp, # Already in Fahrenheit from NOAA
      conditions = period$shortForecast,
      detailed_forecast = period$detailedForecast
    )
  })

  forecasts
}

# Create chat instance with weather-focused system prompt
chat <- chat_openai(
  model = "gpt-4",
  system_prompt = glue::glue("You are a helpful weather assistant. Use the provided
  tools to get current weather information and forecasts. When giving temperatures,
  note that they are in Fahrenheit. When asked about multiple cities, compare
  their conditions. Always mention if weather conditions might impact daily activities.") |>
    stringr::str_squish()
)

# Register tools
chat$register_tool(tool(
  get_weather,
  "Gets current weather conditions for a city",
  city = type_string("The name of the city"),
  state = type_string("State name or abbreviation (required)")
))

chat$register_tool(tool(
  get_forecast,
  "Gets 7-day weather forecast for a city",
  city = type_string("The name of the city"),
  state = type_string("State name or abbreviation (required)")
))


halloween <-
  chat$chat("Do I need an umbrella to go trick-or-treating in Midland MI?", 
            echo = FALSE)

cat(halloween)