box::use(
  terra[rast],
  sf[read_sf, st_intersects, crop],
  purrr[list_rbind, map2],
  hdf5r[h5file],
  stringr[str_replace_all, str_detect],
  readr[read_csv],
  dplyr[mutate, across, summarise],
  httr2[req_headers, request, req_perform, req_user_agent],
  lubridate[year, month, yday]
)

map_black_marble_tiles <- function(){
  # library(sf)
  # library(leaflet)
  #
  # # blackmarble_tiles <- read_sf("blackmarbletiles/BlackMarbleTiles.shp")
  # #
  # # blackmarble_geojson <- geojsonio::geojson_json(blackmarble_tiles, as.character = TRUE)
  # #
  # # #save blackmarble to geojson
  # # geojsonio::geojson_write(blackmarble_tiles, file = "app/static/blackmarble.geojson")
  #
  # # Define the center coordinates and zoom level
  # center <- c(19.432608, -99.133209)  # Mexico City coordinates
  # zoom <- 8  # Adjust the zoom level as needed
  #
  #
  # map <- leaflet() |>
  #   addProviderTiles("Stadia.AlidadeSmooth") |>
  #   addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", options = providerTileOptions(opacity = 0.5)) |>
  #   addGeoJSON(blackmarble_geojson) |>
  #   addMarkers(lng = center[2], lat = center[1], popup = paste("Latitude:", center[1], "<br>Longitude:", center[2])) |>  # Add marker with lat lon info
  #   setView(lng = center[2], lat = center[1], zoom = zoom)
  #
  #
  #
  # return(map)

}


#' Translate Julian Dates to Regular Month Representation
#'
#' This function translates Julian dates to regular month representations.
#'
#' @param julian_date A character string representing the day of the year in Julian format (e.g., "001" for January 1st).
#' @return A character string representing the month corresponding to the given Julian date (e.g., "01" for January).
#' @examples
#' julian_to_month("001")
#' # [1] "01"
#'
#' julian_to_month("032")
#' # [1] "02"
#' @references
#' For more information on the Julian day system, see: https://en.wikipedia.org/wiki/Julian_day
#' @export
julian_to_month <- function(julian_date) {
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  days <- as.integer(julian_date)
  month_index <- findInterval(days, c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366))
  return(months[month_index])
}

#' Remove Artifact Values from Satellite Data
#'
#' This function removes artifact values from satellite data, replacing them with NA.
#' Artifact values are commonly used in satellite data to represent fill values,
#' invalid measurements, or missing data.
#'
#' The artifact values and their corresponding variables are based on the data format
#' described in the Black Marble User Guide (https://viirsland.gsfc.nasa.gov/PDF/BlackMarbleUserGuide_v1.2_20220916.pdf).
#'  # * Table 3 (page 12)
# * Table 6 (page 16)
# * Table 9 (page 18)
#' @param data A matrix or data frame containing satellite data.
#' @param variable A character string specifying the variable for which artifact values should be removed.
#' @return The input data with artifact values replaced by NA.
#' @examples
#' # Remove artifact values from UTC_Time variable in satellite data
#' cleaned_data <- remove_fill_value_from_satellite_data(satellite_data, "UTC_Time")
#' @references
#' For more information on artifact values in satellite data, refer to the Black Marble User Guide.
#'
#' @export
remove_fill_value_from_satellite_data <- function(data, variable) {
  artifact_values_mapping <- list(
    c(255, c("Granule", "Mandatory_Quality_Flag", "Latest_High_Quality_Retrieval",
             "Snow_Flag", "DNB_Platform", "Land_Water_Mask",
             "AllAngle_Composite_Snow_Covered_Quality", "AllAngle_Composite_Snow_Free_Quality",
             "NearNadir_Composite_Snow_Covered_Quality", "NearNadir_Composite_Snow_Free_Quality",
             "OffNadir_Composite_Snow_Covered_Quality", "OffNadir_Composite_Snow_Free_Quality")),
    c(-999.9, "UTC_Time"),
    c(-32768, c("Sensor_Azimuth", "Sensor_Zenith", "Solar_Azimuth", "Solar_Zenith",
                "Lunar_Azimuth", "Lunar_Zenith", "Glint_Angle", "Moon_Illumination_Fraction",
                "Moon_Phase_Angle")),
    c(65535, c("DNB_At_Sensor_Radiance_500m", "BrightnessTemperature_M12", "BrightnessTemperature_M13",
               "BrightnessTemperature_M15", "BrightnessTemperature_M16", "QF_Cloud_Mask", "QF_DNB",
               "QF_VIIRS_M10", "QF_VIIRS_M11", "QF_VIIRS_M12", "QF_VIIRS_M13", "QF_VIIRS_M15", "QF_VIIRS_M16",
               "Radiance_M10", "Radiance_M11", "DNB_BRDF-Corrected_NTL", "DNB_Lunar_Irradiance",
               "Gap_Filled_DNB_BRDF-Corrected_NTL", "AllAngle_Composite_Snow_Covered",
               "AllAngle_Composite_Snow_Covered_Num", "AllAngle_Composite_Snow_Free",
               "AllAngle_Composite_Snow_Free_Num", "NearNadir_Composite_Snow_Covered",
               "NearNadir_Composite_Snow_Covered_Num", "NearNadir_Composite_Snow_Free",
               "NearNadir_Composite_Snow_Free_Num", "OffNadir_Composite_Snow_Covered",
               "OffNadir_Composite_Snow_Covered_Num", "OffNadir_Composite_Snow_Free",
               "OffNadir_Composite_Snow_Free_Num", "AllAngle_Composite_Snow_Covered_Std",
               "AllAngle_Composite_Snow_Free_Std", "NearNadir_Composite_Snow_Covered_Std",
               "NearNadir_Composite_Snow_Free_Std", "OffNadir_Composite_Snow_Covered_Std",
               "OffNadir_Composite_Snow_Free_Std"))
  )

  mapping_found <- FALSE


  for (mapping in artifact_values_mapping) {
    variables <- mapping[[2]]
    if (variable %in% variables) {
      value <- mapping[[1]]
      data[data == value] <- NA
      mapping_found <- TRUE
      break  # exit loop once artifact value is found
    }
  }

  if (!mapping_found) {
    warning(paste("Variable", variable, "not found in artifact values mapping. No action taken."))
  }

  return(data)
}


#' Apply Scaling Factor to VIIRS Data
#'
#' Apply scaling factor to variables according to Black Marble user guide.
#' The scaling factor is 0.1 for specific VIIRS variables.
#'
#' @param x A numeric vector or matrix representing the VIIRS data.
#' @param variable A character string specifying the variable name.
#'
#' @return A numeric vector or matrix with the scaling factor applied to the specified variables.
#'
#' @details
#' This function applies a scaling factor of 0.1 to specific VIIRS variables
#' according to the Black Marble user guide.
#'
#' The following VIIRS variables are affected:
#' \itemize{
#'   \item DNB_At_Sensor_Radiance (VNP46A1)
#'   \item DNB_BRDF-Corrected_NTL (VNP46A2)
#'   \item Gap_Filled_DNB_BRDF-Corrected_NTL (VNP46A2)
#'   \item DNB_Lunar_Irradiance (VNP46A2)
#'   \item AllAngle_Composite_Snow_Covered (VNP46A3/4)
#'   \item AllAngle_Composite_Snow_Covered_Std (VNP46A3/4)
#'   \item AllAngle_Composite_Snow_Free (VNP46A3/4)
#'   \item AllAngle_Composite_Snow_Free_Std (VNP46A3/4)
#'   \item NearNadir_Composite_Snow_Covered (VNP46A3/4)
#'   \item NearNadir_Composite_Snow_Covered_Std (VNP46A3/4)
#'   \item NearNadir_Composite_Snow_Free (VNP46A3/4)
#'   \item NearNadir_Composite_Snow_Free_Std (VNP46A3/4)
#'   \item OffNadir_Composite_Snow_Covered (VNP46A3/4)
#'   \item OffNadir_Composite_Snow_Covered_Std (VNP46A3/4)
#'   \item OffNadir_Composite_Snow_Free (VNP46A3/4)
#'   \item OffNadir_Composite_Snow_Free_Std (VNP46A3/4)
#' }
#'
#' @references
#' Black Marble User Guide - https://viirsland.gsfc.nasa.gov/PDF/BlackMarbleUserGuide_v1.2_20220916.pdf
#'
#' @export
apply_scaling_factor_to_viirs_data <- function(x, variable) {
  # Apply scaling factor to VIIRS data
  scaling_variables <- c(
    "DNB_At_Sensor_Radiance",
    "DNB_BRDF-Corrected_NTL",
    "Gap_Filled_DNB_BRDF-Corrected_NTL",
    "DNB_Lunar_Irradiance",
    "AllAngle_Composite_Snow_Covered",
    "AllAngle_Composite_Snow_Covered_Std",
    "AllAngle_Composite_Snow_Free",
    "AllAngle_Composite_Snow_Free_Std",
    "NearNadir_Composite_Snow_Covered",
    "NearNadir_Composite_Snow_Covered_Std",
    "NearNadir_Composite_Snow_Free",
    "NearNadir_Composite_Snow_Free_Std",
    "OffNadir_Composite_Snow_Covered",
    "OffNadir_Composite_Snow_Covered_Std",
    "OffNadir_Composite_Snow_Free",
    "OffNadir_Composite_Snow_Free_Std"
  )

  if (variable %in% scaling_variables) {
    x <- x * 0.1
  }

  return(x)
}
#' Convert HDF5 File to Raster
#'
#' Converts an HDF5 file to a raster object.
#'
#' @param file_path A character string representing the filepath to the HDF5 file.
#' @param variable_name A character string specifying the variable name to extract from the HDF5 file.
#' @param quality_flags_to_remove A numeric vector containing quality flag values to be removed from the data (optional).
#'
#' @return A raster object containing the extracted variable data from the HDF5 file.
#'
#' @details
#' This function converts an HDF5 file to a raster object. It extracts the specified variable
#' from the HDF5 file and optionally removes specific quality flag values from the data.
#'
#' @references
#' Black Marble User Guide - https://viirsland.gsfc.nasa.gov/PDF/BlackMarbleUserGuide_v1.2_20220916.pdf
#'
#' @export
convert_h5_to_raster <- function(file_path, variable_name, quality_flags_to_remove = numeric()) {
  # Load HDF5 file
  h5_data <- h5file(file_path, "r+")

  # Extract data and metadata
  data <- extract_data(h5_data, file_path, variable_name, quality_flags_to_remove)

  metadata <- extract_metadata(data)

  # Convert data to raster
  raster_obj <- create_raster(data, metadata)

  # Clean raster data
  raster_obj <- clean_raster_data(raster_obj, variable_name)

  # Close HDF5 file
  h5_data$close_all()

  return(raster_obj)
}



#' Extract Data from HDF5 File
extract_data <- function(h5_data, file_path, variable_name, quality_flags_to_remove) {
  if (grepl("VNP46A1|VNP46A2", file_path)) {
    # Extract data for daily files
    data <- extract_daily_data(h5_data, variable_name, quality_flags_to_remove)
  } else {
    # Extract data for monthly/annually files
    data <- extract_monthly_data(h5_data, variable_name, quality_flags_to_remove)
  }
  return(data)
}


#' Extract Daily Data from HDF5 File
extract_daily_data <- function(h5_data, variable_name, quality_flags_to_remove) {
  # Extracting daily data logic from the original function
  if(variable_name %in% c(
    "DNB_At_Sensor_Radiance",
    "DNB_BRDF-Corrected_NTL",
    "Gap_Filled_DNB_BRDF-Corrected_NTL",
    "DNB_Lunar_Irradiance",
    "AllAngle_Composite_Snow_Covered",
    "AllAngle_Composite_Snow_Covered_Std",
    "AllAngle_Composite_Snow_Free",
    "AllAngle_Composite_Snow_Free_Std",
    "NearNadir_Composite_Snow_Covered",
    "NearNadir_Composite_Snow_Covered_Std",
    "NearNadir_Composite_Snow_Free",
    "NearNadir_Composite_Snow_Free_Std",
    "OffNadir_Composite_Snow_Covered",
    "OffNadir_Composite_Snow_Covered_Std",
    "OffNadir_Composite_Snow_Free",
    "OffNadir_Composite_Snow_Free_Std"
  )) {
    out <- h5_data[[paste0("HDFEOS/GRIDS/VNP_Grid_DNB/Data Fields/", variable_name)]][,]
    qf <- h5_data[["HDFEOS/GRIDS/VNP_Grid_DNB/Data Fields/Mandatory_Quality_Flag"]][,]

    if(length(quality_flags_to_remove) > 0) {
      if(variable_name %in% c("DNB_BRDF-Corrected_NTL",
                              "Gap_Filled_DNB_BRDF-Corrected_NTL",
                              "Latest_High_Quality_Retrieval")) {
        for(val in quality_flags_to_remove) {
          out[qf == val] <- NA
        }
      }
    }
  }

  return(out)
}

#' Extract Monthly Data from HDF5 File
extract_monthly_data <- function(h5_data, variable_name, quality_flags_to_remove) {
  # Extracting monthly data logic from the original function
  lat <- h5_data[["HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lat"]][]
  lon <- h5_data[["HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lon"]][]

  out <- h5_data[[paste0("HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/", variable_name)]][,]

  if(length(quality_flags_to_remove) > 0) {
    variable_short <- variable_name |>
      str_replace_all("_Num", "") |>
      str_replace_all("_Std", "")

    qf_name <- paste0(variable_short, "_Quality")

    if(qf_name %in% variable_name) {
      qf <- h5_data[[paste0("HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/", qf_name)]][,]

      for(val in quality_flags_to_remove) {
        out[qf == val] <- NA
      }
    }
  }

  if(class(out[1,1])[1] != "numeric") {
    out <- matrix(as.numeric(out), ncol = ncol(out))
  }

  return(out)
}

#' Extract Metadata from Data
extract_metadata <- function(data) {
  nRows <- nrow(data)
  nCols <- ncol(data)
  res <- nRows
  nodata_val <- NA
  myCrs <- 4326

  metadata <- list(nRows = nRows, nCols = nCols, res = res, nodata_val = nodata_val, myCrs = myCrs)
  return(metadata)
}

#' Create Raster Object using terra
create_raster <- function(data, metadata) {
  xMin <- 0
  yMin <- 0
  xMax <- metadata$nCols
  yMax <- metadata$nRows

  # Transpose data
  data <- t(data)

  # Create terra raster object
  rast <- rast(data, xmin = xMin, ymin = yMin, xmax = xMax, ymax = yMax)

  return(rast)
}

#' Clean Raster Data
clean_raster_data <- function(raster_obj, variable_name) {
  # Remove fill values
  raster_obj <- remove_fill_value_from_satellite_data(raster_obj, variable_name)

  # Apply scaling factor
  raster_obj <- apply_scaling_factor(raster_obj, variable_name)

  return(raster_obj)
}


#' Read Black Marble CSV Data
#'
#' Reads Black Marble CSV data for a specific year and day.
#'
#' @param year The year of the data.
#' @param day The day of the year (1-365 or 1-366 for leap years).
#' @param product_id The product ID specifying the type of data to retrieve.
#'
#' @return A data frame containing the Black Marble CSV data for the specified year and day.
#'
#' @details This function reads Black Marble CSV data from the NASA LADS website for the given \code{year},
#' \code{day}, and \code{product_id}. It constructs the URL based on the provided parameters and attempts
#' to read the CSV file using \code{readr::read_csv}. If successful, it adds columns for \code{year} and \code{day}
#' to the data frame. If an error occurs during the reading process, it returns an empty data frame and issues a warning.
#' Additionally, to avoid overloading the server with rapid requests, the function includes a small delay (0.1 seconds) between
#' consecutive requests using \code{Sys.sleep(0.1)}.
#'
#' @examples
#' # Read Black Marble CSV data for the year 2023, day 150, and product ID "VIIRS_SNPP_CorrectedReflectance_BandM3"
#' data <- read_bm_csv(2023, 150, "VIIRS_SNPP_CorrectedReflectance_BandM3")
#'
#' @export
read_bm_csv <- function(year, day, product_id) {

  df_out <- tryCatch(
    {
      df <- read_csv(paste0("https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/", product_id, "/", year, "/", day, ".csv"),
                            show_col_types = FALSE)

      df$year <- year
      df$day <- day

      df
    },
    error = function(e){
      warning(paste0("Error with year: ", year, "; day: ", day))
      data.frame(NULL)
    }
  )

  Sys.sleep(0.1)  # Adding a small delay to avoid overloading the server

  return(df_out)
}

#' Create Black Marble Dataset DataFrame
#'
#' Creates a data frame containing Black Marble dataset filenames based on the specified parameters.
#'
#' @param product_id The product ID specifying the type of Black Marble data.
#' @param all Logical; indicating whether to create filenames for all available data or only for specific years, months, or days (default: TRUE).
#' @param years A numeric vector specifying the years for which to create filenames (optional).
#' @param months A numeric vector specifying the months for which to create filenames (optional).
#' @param days A numeric vector specifying the days for which to create filenames (optional).
#'
#' @return A data frame containing the filenames of Black Marble datasets based on the specified parameters.
#'
#' @details This function generates a data frame with filenames of Black Marble datasets. It allows filtering
#' by year, month, and day based on the provided parameters. Depending on the \code{product_id}, it generates
#' filenames for daily, monthly, or yearly data. The generated filenames are based on the specified product ID,
#' year, and day or month. The resulting data frame includes columns for the year and day or month, depending on the
#' type of data.
#'
#' @examples
#' # Generate filenames for all available Black Marble data
#' all_data <- create_black_marble_dataset_df("VNP46A1")
#'
#' # Generate filenames for Black Marble data for specific years and months
#' specific_data <- create_black_marble_dataset_df("VNP46A2", years = c(2018, 2019), months = 1:6)
#'
#' @export
create_black_marble_dataset_df <- function(product_id,
                                           all = TRUE,
                                           years = NULL,
                                           months = NULL,
                                           days = NULL) {

  # Define product-specific parameters and conditions
  product_params <- list(
    "VNP46A1" = list(months = NULL, days = 1:366, add_month = TRUE),
    "VNP46A2" = list(months = NULL, days = 1:366, add_month = TRUE),
    "VNP46A3" = list(months = NULL, days = c("001", "032", "061", "092", "122", "153", "183", "214", "245", "275", "306", "336",
                                             "060", "091", "121", "152", "182", "213", "244", "274", "305", "335")),
    "VNP46A4" = list(months = NULL, days = "001", add_month = FALSE)
  )

  # Retrieve product-specific parameters
  params <- product_params[[product_id]]

  # Determine end year
  year_end <- as.numeric(format(Sys.Date(), "%Y"))

  # Generate parameter dataframe
  param_df <- expand.grid(year = 2012:year_end, day = pad3(params$days))

  # Add month if required
  if (params$add_month) {
    param_df <- param_df |>
      mutate(month = day |>  month_start_day_to_month() |>
               as.numeric()
             )
  }

  # Subset time period
  if (!is.null(years)) {
    param_df <- param_df[param_df$year %in% years, ]
  }

  if (!is.null(months)) {
    param_df <- param_df[as.numeric(param_df$month) %in% as.numeric(months), ]
  }

  if (!is.null(days)) {
    param_df <- param_df[as.numeric(param_df$day) %in% as.numeric(days), ]
  }

  # Create data
  files_df <- map2(param_df$year,
                              param_df$day,
                              read_black_marble_csv,
                              product_id) |>
    list_rbind()

  return(files_df)
}

#' Download and Convert Raster Data
#'
#' Downloads raster data from NASA's LADSWeb and converts it to a raster object.
#'
#' @param file_name A character string representing the name of the file to download.
#' @param temp_dir A character string specifying the temporary directory where the file will be saved.
#' @param variable A character string specifying the variable to extract from the raster data.
#' @param bearer A character string containing the authorization token for accessing NASA's LADSWeb.
#' @param quality_flags_to_remove A numeric vector containing quality flag values to be removed from the data (optional).
#' @param quiet Logical; indicating whether to suppress progress messages (default: FALSE).
#'
#' @return A raster object containing the downloaded and processed raster data.
#'
#' @details This function downloads raster data from NASA's LADSWeb based on the provided file name.
#' It then converts the downloaded data to a raster object, extracting the specified variable and removing
#' quality flag values if specified. The function also provides an option to suppress progress messages.
#'
#' @export
download_and_convert_raster <- function(file_name,
                                        temp_dir,
                                        variable,
                                        bearer,
                                        quality_flags_to_remove = numeric(),
                                        quiet = FALSE) {
  # Extract file metadata
  year <- substr(file_name, 10, 13)
  day <- substr(file_name, 14, 16)
  product_id <- substr(file_name, 1, 7)

  # Construct download URL
  url <- paste0('https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/',
                product_id, '/', year, '/', day, '/', file_name)

    # Define url request

  versions <- c(
    httr2 = utils::packageVersion("httr2"),
    `r-curl` = utils::packageVersion("curl"),
    libcurl = sub("-DEV", "", curl::curl_version()$version)
  )
  string <- paste0(names(versions), "/", versions, collapse = " ")

  request <- request(url) |>
    req_headers(
      'Authorization' = paste('Bearer', bearer)
    ) |>
    req_user_agent(string)

  # Define download path
  download_path <- file.path(temp_dir, file_name)

  # Display processing message if not quiet
  if (!quiet) message(paste0("Processing: ", file_name))



  # Perform the download
  if (quiet) {
    options(httr2_progress = FALSE)

    # automaticallly overwrites
    response <- request |>
      req_perform(
        path = download_path
      )

  } else {
    response <- request |>
      req_perform(
        path = download_path
      )

  }

  # Check for successful download
  if (resp_status(response) != 200) {
    message("Error in downloading data")
    message(response |>
              resp_status_desc()
            )
  }

  # Convert downloaded file to raster
  raster_data <- convert_h5_to_raster(download_path,
                                variable,
                                quality_flags_to_remove)

  return(raster_data)
}


#' Define Black Marble Variable
#'
#' Defines the variable based on the Black Marble product ID if it is NULL.
#'
#' @param variable A character string specifying the variable to define.
#' @param product_id A character string representing the product ID.
#'
#' @return A character string representing the defined variable.
#'
#' @export
define_blackmarble_variable <- function(variable, product_id){
  if (is.null(variable)) {
    variable <- switch(product_id,
                       "VNP46A1" = "DNB_At_Sensor_Radiance_500m",
                       "VNP46A2" = "Gap_Filled_DNB_BRDF-Corrected_NTL",
                       "VNP46A3" | "VNP46A4" = "NearNadir_Composite_Snow_Free",
                       variable)
  }

  return(variable)
}

#' Define Raster Name by Date
#'
#' Generates a name for the raster based on the given date and product ID.
#'
#' @param date_string A character string representing the date in the format "YYYY-MM-DD".
#' @param product_id A character string representing the product ID (e.g., "VNP46A1", "VNP46A2").
#'
#' @return A character string representing the generated raster name.
#'
#' @export
define_raster_name <- function(date_string, product_id){
  raster_name <- switch(product_id,
                        "VNP46A1" | "VNP46A2" = paste0("t", date_string |> str_replace_all("-", "_")),
                        "VNP46A3" = paste0("t", date_string |> str_replace_all("-", "_") |> substring(1,7)),
                        "VNP46A4" = paste0("t", date_string |> str_replace_all("-", "_") |> substring(1,4))
  )

  return(raster_name)
}

#' Count Observations for Exact Extract
#'
#' Counts observations for each variable, considering coverage fraction, for use in exact_extract.
#'
#' @param values A data frame containing the values.
#' @param coverage_fraction A numeric value representing the coverage fraction.
#'
#' @return A data frame with the count of non-NA pixels and total pixels for each variable.
#'
#' @export
count_n_obs <- function(values) {
  #coverage_fraction was a param but not used pehaps the other functions needs it
  orig_vars <- names(values)

  values |>
    mutate(across(orig_vars, ~ as.numeric(!is.na(.)) )) |>
    summarise(across(orig_vars, sum, .names = "n_non_na_pixels.{.col}"),
                     across(orig_vars, ~length(.), .names = "n_pixels.{.col}"))
}

process_tiles <- function(bm_files_df, grid_use_sf, check_all_tiles_exist, temp_dir, product_id, variable, bearer, quality_flag_rm, quiet) {
  tile_ids_rx <- grid_use_sf$TileID %>% paste(collapse = "|")
  bm_files_df <- bm_files_df[bm_files_df$name %>% str_detect(tile_ids_rx), ]

  if ((nrow(bm_files_df) < nrow(grid_use_sf)) && check_all_tiles_exist) {
    message("Not all satellite imagery tiles for this location exist, so skipping. To ignore this error and process anyway, set check_all_tiles_exist = FALSE")
    stop("Not all satellite imagery tiles for this location exist, so skipping. To ignore this error and process anyway, set check_all_tiles_exist = FALSE")
  }

  unlink(file.path(temp_dir, product_id), recursive = TRUE)

  if (!quiet) {
    message(paste0("Processing ", nrow(bm_files_df), " nighttime light tiles"))
  }

  r_list <- lapply(bm_files_df$name, function(name_i) {
    download_and_convert_raster(name_i, temp_dir, variable, bearer, quality_flag_rm, quiet)
  })

  if (length(r_list) == 1) {
    return(r_list[[1]])
  } else {
    names(r_list) <- NULL
    r_list$fun <- max
    return(do.call(raster::mosaic, r_list))
  }
}


#' Retrieve and Process Nightlight Data
#'
#' Retrieves and processes nighttime light raster data from the Black Marble dataset.
#'
#' @param roi_sf An sf object representing the region of interest.
#' @param product_id A character string specifying the product ID.
#' @param date A character string representing the date.
#' @param bearer A character string representing the authorization bearer token.
#' @param variable A character string specifying the variable to extract.
#' @param quality_flag_rm A numeric vector containing quality flag values to be removed from the data (optional).
#' @param check_all_tiles_exist A logical value indicating whether to check if all satellite imagery tiles for the location exist (default is TRUE).
#' @param quiet A logical value indicating whether to suppress progress messages (default is FALSE).
#' @param temp_dir A character string representing the temporary directory path.
#'
#' @return A raster object containing the processed nighttime light data.
#'
#' @details
#' This function retrieves and processes nighttime light raster data from the Black Marble dataset.
#' It downloads the data for the specified region of interest and date, removes any quality-flagged
#' pixels as specified, and mosaics the tiles together if necessary. It then crops the raster to the
#' region of interest.
#'
#' @export
retrieve_and_process_nightlight_data <- function(roi_sf,
                                                 product_id,
                                                 date,
                                                 bearer,
                                                 variable,
                                                 quality_flag_rm,
                                                 check_all_tiles_exist = TRUE,
                                                 quiet = FALSE,
                                                 temp_dir) {
  # Checks ---------------------------------------------------------------------
  if(!("sf" %in% class(roi_sf))){
    stop("roi must be an sf object")
  }

  # Fetch Black marble grid ----------------------------------------------------------
  bm_tiles_sf <- read_sf("https://raw.githubusercontent.com/worldbank/blackmarbler/main/data/blackmarbletiles.geojson")

  # Prep dates -----------------------------------------------------------------
  date <- switch(product_id,
                 "VNP46A3" = ifelse(nchar(date) %in% 7, paste0(date, "-01"), date),
                 "VNP46A4" = ifelse(nchar(date) %in% 4, paste0(date, "-01-01"), date),
                 TRUE = date)

  # Grab tile dataframe --------------------------------------------------------
  year  <- date |>  year()
  month <- date |>  month()
  day   <- date |>  yday()

  bm_files_df <- create_black_marble_dataset_df(product_id = product_id,
                                        all = T,
                                        years = year,
                                        months = month,
                                        days = day)

  # Intersecting tiles ---------------------------------------------------------
  # Remove grid along edges, which causes st_intersects to fail

  bm_tiles_sf <- bm_tiles_sf[!(str_detect(bm_tiles_sf$TileID, "h00") | str_detect(bm_tiles_sf$TileID, "v00")), ]


  inter <- tryCatch(
    {
      inter <- st_intersects(bm_tiles_sf, roi_sf, sparse = F) |>
        apply(1, sum)

      inter
    },
    error = function(e){
      warning("Issue with `roi_sf` intersecting with blackmarble tiles; try buffering by a width of 0: eg, st_buffer(roi_sf, 0)")
      stop("Issue with `roi_sf` intersecting with blackmarble tiles; try buffering by a width of 0: eg, st_buffer(roi_sf, 0)")
    }
  )

  grid_use_sf <- bm_tiles_sf[inter > 0,]

  # Make Raster ----------------------------------------------------------------
  raster <- process_tiles(bm_files_df, grid_use_sf, check_all_tiles_exist, temp_dir, product_id, variable, bearer, quality_flag_rm, quiet)

  ## Crop
  raster <- raster |>
    crop(roi_sf)

  unlink(file.path(temp_dir, product_id), recursive = T)

  return(raster)
}

# missing
#bm_raster
