# Wesley Noble
# AIS Ship Function

# Define the function to convert AIS points into a line shapefile
convert_ais_to_lines <- function(ais_data, output_file = "ship_tracks.shp") {
  
  # Error handling: Check if required columns exist
  required_cols <- c("lat", "lon", "time", "mmsi")
  if (!all(required_cols %in% colnames(ais_data))) {
    stop("Error: The input data must contain at least the following columns: Latitude, Longitude, Timestamp, MMSI")
  }
  
  # Convert the dataframe to an sf (simple features) object with a geographic coordinate system
  ais_sf <- ais_data %>%
    drop_na(lat, lon) %>%  # Remove missing values
    arrange(mmsi, time) %>%  # Ensure data is sorted by time for correct track ordering
    st_as_sf(coords = c("lat", "lon"), crs = 4326)  # Set CRS to WGS84
  
  # Group points by Ship_ID and convert to lines
  ship_tracks <- ais_sf %>%
    group_by(mmsi) %>%
    summarise(geometry = st_combine(geometry)) %>%  # Combine points into a single geometry
    st_cast("LINESTRING")  # Convert to lines
  
  # Save the result as a shapefile
  st_write(ship_tracks, output_file, delete_layer = TRUE)
  
  # Return the sf object for further use
  return(ship_tracks)
}

# Example usage
# ais_data <- read.csv("ais_ship_data.csv")  # Load your AIS data
# ship_tracks <- convert_ais_to_lines(ais_data, "ship_tracks.shp")  # Run function
# plot(ship_tracks["Ship_ID"])  # Visualize tracks