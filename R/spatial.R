# Geocoder Wrapper
#
# This provides a wrapper for Google's geocoding service
#

#' Geocode Wrapper
#'
#' This provides a wrapper for Google's geocoding service
#'
#' @return dataframe
#' @param addresses_to_geocode A vector of addresses
#' @param google_api_key OPTIONAL API Key
#' @param clean TRUE if you want to delete the temp file, FALSE if you want it
#'     to stay
#' @import rjson
#' @import ggmap
#' @export
geocode <- function(addresses_to_geocode, google_api_key = NA, clean = TRUE) {
  # Check to see if we are going to use the API key or not
  use_api_key <- ifelse(is.na(google_api_key), FALSE, TRUE)

  # Set the row number we want to start from
  current_row <- 1

  # Since the geocoding can get interrupted and there are daily limits we will
  # use a temporary file to hold the results.  Let's check to see if we have
  # some of the geocoding done.

  temp_filename <- "geocode_temp.rds"
  if (file.exists(temp_filename)) {
    # Load what has been geocoded into a data frame
    geocoded <- readRDS(temp_filename)
    # Update the current row count
    current_row <- nrow(geocoded)
    # Give a message to the user
    print(paste("Found temp file - resuming from row:", current_row))
  } else {
    # Initialize the data frame that will hold the geocoded info
    geocoded <- data.frame("Address" = as.character(),
                           "Status" = as.character(),
                           "Location Type" = as.character(),
                           "Lat" = as.character(),
                           "Lng" = as.character(),
                           "Formatted Address" = as.character(),
                           "Level 1" = as.character(),
                           "Level 2" = as.character(),
                           "Level 3" = as.character(),
                           "Accuracy" = as.character(),
                           stringsAsFactors = FALSE)
  }

  # This function helps insert rows into the geocoded data frame
  # Source: http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
  insert_row <- function(existing_data_frame, new_row, row) {
    existing_data_frame[row, ] <- new_row
    return(existing_data_frame)
  }

  # The function geocodes a single address and returns the results
  geocode_me <- function(address){
    if (use_api_key){
      url <- paste0(
        "https://maps.googleapis.com/maps/api/geocode/json?address=",
        address, "&sensor=false&key=", api_key)
      response <- rjson::fromJSON(file = url, method = "C")
    } else {
      response <- ggmap::geocode(address, output = "all",
                                 override_limit = TRUE,
                                 messaging = FALSE)
    }

    # Initialize return varriables with NA's incase we don't have any results
    location_type <- lat <- lng <- formatted_address <- NA
    administrative_area_level_1 <- administrative_area_level_2 <- NA
    administrative_area_level_3 <- accuracy <- NA

    # Handle Going Over the Query Limit
    if (response$status == "OVER_QUERY_LIMIT"){
      message("OVER QUERY LIMIT - ", appendLF = FALSE)
      if (use_api_key == FALSE){
        message("Pausing for 1 hour at:", as.character(Sys.time()))
        Sys.sleep(60 * 60)
        return (geocode_me(address))
      } else {
        # 2500 requests with API key used up so switch modes
        message("Switching to requests without API key")
        use_api_key <<- FALSE
        return (geocode_me(address))
      }
    }

    # We have a response from the geocoder
    if (response$status == "OK"){
      # Get a clean address
      formatted_address <- response$results[[1]]$formatted_address

      # Get lat & lng
      location_type <- response$results[[1]]$geometry$location_type
      lat <- response$results[[1]]$geometry$location$lat
      lng <- response$results[[1]]$geometry$location$lng

      accuracy <- tryCatch(response$results[[1]]$types[[1]],
                           error = function(e) NA)

      # Loop through the address components
      ac <- response$results[[1]]$address_components
      for (ii in seq(1, length(ac))){
        if (ac[[ii]]$types[1] == "administrative_area_level_3"){
          administrative_area_level_3 <- ac[[ii]]$short_name
        }
        if (ac[[ii]]$types[1] == "administrative_area_level_2"){
          administrative_area_level_2 <- ac[[ii]]$short_name
        }
        if (ac[[ii]]$types[1] == "administrative_area_level_1"){
          administrative_area_level_1 <- ac[[ii]]$short_name
        }
      }
    }

    # Return the data as a vector
    return(c(address, response$status, location_type, lat, lng,
             formatted_address, administrative_area_level_1,
             administrative_area_level_2, administrative_area_level_3,
             accuracy))
  }

  total <- length(addresses_to_geocode)

  # Loop through the addresses
  for (i in seq(start_index, total)){
    # Get the gecoded info
    row <- geocode_me(addresses_to_geocode[i])
    # Append it to the data frame
    geocoded <- insert_row(geocoded, row, i)
    # Save it to the temp files
    saveRDS(geocoded, temp_filename)
  }

  # Write the output
  write.table(geocoded,
              file = "geocoded.csv",
              sep = ",",
              row.names = FALSE)

  # Clean up the working environment
  if (clean){
    unlink(temp_filename)
  }

  print("Results are save to geocoded.csv")
}
