#' @title Summarizing tracked sustainability data
#' @description Function for summarizing the tracked sustainability data with a tracker of the python library
#'   'codecarbon'.
#'
#' @param sustainability_tracker Object of class `codecarbon.emissions_tracker.OfflineEmissionsTracker` of the python
#'   library codecarbon.
#' @return Returns a `list` which contains the tracked sustainability data.
#'
#' @family Auxiliary Functions
#'
#' @keywords internal
summarize_tracked_sustainability <- function(sustainability_tracker) {
  if (is.null(sustainability_tracker$final_emissions_data$region)) {
    region <- NA
  } else {
    region <- sustainability_tracker$final_emissions_data$region
  }

  if (is.null(sustainability_tracker$final_emissions_data$gpu_count)) {
    gpu_count_tmp <- NA
  } else {
    gpu_count_tmp <- sustainability_tracker$final_emissions_data$gpu_count
  }

  if (is.null(sustainability_tracker$final_emissions_data$gpu_model)) {
    gpu_model_tmp <- NA
  } else {
    gpu_model_tmp <- sustainability_tracker$final_emissions_data$gpu_model
  }

  results <- list(
    sustainability_tracked = TRUE,
    date = date(),
    sustainability_data = list(
      duration_sec = sustainability_tracker$final_emissions_data$duration,
      co2eq_kg = sustainability_tracker$final_emissions_data$emissions,
      cpu_energy_kwh = sustainability_tracker$final_emissions_data$cpu_energy,
      gpu_energy_kwh = sustainability_tracker$final_emissions_data$gpu_energy,
      ram_energy_kwh = sustainability_tracker$final_emissions_data$ram_energy,
      total_energy_kwh = sustainability_tracker$final_emissions_data$energy_consumed
    ),
    technical = list(
      tracker = "codecarbon",
      py_package_version = codecarbon$"__version__",
      cpu_count = sustainability_tracker$final_emissions_data$cpu_count,
      cpu_model = sustainability_tracker$final_emissions_data$cpu_model,
      gpu_count = gpu_count_tmp,
      gpu_model = gpu_model_tmp,
      ram_total_size = sustainability_tracker$final_emissions_data$ram_total_size
    ),
    region = list(
      country_name = sustainability_tracker$final_emissions_data$country_name,
      country_iso_code = sustainability_tracker$final_emissions_data$country_iso_code,
      region = region
    )
  )
}
