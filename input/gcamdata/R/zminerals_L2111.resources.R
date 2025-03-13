# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_minerals_L2111.resources
#'
#' Set up data tables for mineral supply curves
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' @details Set up data tables for mineral supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na pivot_longer pivot_wider expand_grid
#' @author BY March 2025
#'
module_minerals_L2111.resources <- function(command, ...) {
if(command == driver.DECLARE_INPUTS) {
  return(c(FILE = "common/GCAM_region_names",
           FILE = "minerals/supply/A10.mineral_rsrc_info",
           "L1111.mineral_production_R_Y_hist",
           "L1111.mineral_AnnProdLimit_R_Y",
           "L1111.mineral_ResSupplyCurves_R_Y",
           "L1111.mineral_AvgProdLifetime"))
} else if(command == driver.DECLARE_OUTPUTS) {
  return(c())
} else if(command == driver.MAKE) {

  all_data <- list(...)[[1]]

  # Load required inputs
  GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A10.mineral_rsrc_info <- get_data(all_data, "minerals/supply/A10.mineral_rsrc_info", strip_attributes = TRUE) %>%
    gather_years
  L1111.mineral_production_R_Y_hist <- get_data(all_data, "L1111.mineral_production_R_Y_hist")
  L1111.mineral_AnnProdLimit_R_Y <- get_data(all_data, "L1111.mineral_AnnProdLimit_R_Y")
  L1111.mineral_ResSupplyCurves_R_Y <- get_data(all_data, "L1111.mineral_ResSupplyCurves_R_Y")
  L1111.mineral_AvgProdLifetime <- get_data(all_data, "L1111.mineral_AvgProdLifetime")

  # Check for calibrated resource prices for final historical model year.
  # Otherwise, price behavior is undefined, and so stop process.
  # There should be calibrated prices for all historical model years for
  # full consistency, however.
  if(!(MODEL_FINAL_BASE_YEAR %in% c(unique(A10.mineral_rsrc_info$year)))){
    stop("No calibrated prices for resources in final historical year")
  }

  # ===================================================
  # ------- MINERAL RESOURCE RESERVE ADDITIONS
  # Kind of a level 1.5 we are going to calculate / update historical energy
  # but the years we choose as the model base years matter

  GCAM_timesteps <- diff(MODEL_BASE_YEARS)
  start.year.timestep <- modeltime.PERIOD0_TIMESTEP
  model_year_timesteps <- tibble(year = MODEL_BASE_YEARS, timestep = c(start.year.timestep, GCAM_timesteps))

  # a pipelne helper function to help back calculate new additions to reserve
  # from historical production
  lag_prod_helper <- function(year, value, year_operate, final_year) {
    ret <- value
    for(i in seq_along(year)) {
      if(i == 1) {
        # first year assume all production in this vintage
        ret[i] <- value[i]
      } else if( year_operate[i] > final_year[i]) {
        if(year_operate[i -1] >= final_year[i]) {
          # retired
          ret[i] <- 0
        } else {
          # final timestep that is operating so we must adjust the production
          # by the number of years into the timestep it should have operated
          # incase lifetime and timesteps do not neatly overlap
          ret[i] <- ret[i - 1] * (year_operate[i] - final_year[i]) / (year_operate[i] - year_operate[i-1])
        }
      } else if(year_operate[i] > year[i]) {
        # assume a vintage that as already invested continues at full
        # capacity
        ret[i] <- ret[i -1]
      } else {
        # to determine new investment we take the difference between
        # what the total should be and subtract off production from
        # previous vintages that are still operating
        ret[i] <- 0
        ret[i] <- pmax(value[i] - sum(ret[year_operate == year[i]]), 0)
      }
    }
    ret
  }
  # Back calculate reserve additions to be exactly enough given our historical production
  # and assumed production lifetime.  Note production lifetimes may not cover the entire
  # historical period making the calculation a bit more tricky.  We use the lag_prod_helper
  # to help project forward production by each historical vintage so we can take this into
  # account.
  L2111.mineral_Reserve_Mt_R_Yh <- L111.Prod_EJ_R_F_Yh %>%
    filter(year %in% MODEL_BASE_YEARS) %>%
    left_join_error_no_match(select(A10.ResSubresourceProdLifetime, resource, lifetime = avg.prod.lifetime, reserve.subresource) %>% distinct(),
                             by=c("fuel" = "resource", "technology" = "reserve.subresource")) %>%
    left_join_error_no_match(model_year_timesteps, by = c("year")) %>%
    repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
    mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (year - timestep + lifetime))) %>%
    filter(year_operate >= year - timestep + 1) %>%
    group_by(GCAM_region_ID, sector, fuel, technology) %>%
    mutate(value = lag_prod_helper(year, value, year_operate, final_year)) %>%
    ungroup() %>%
    filter(year == year_operate) %>%
    mutate(value = value * lifetime) %>%
    select(-lifetime, -timestep, -year_operate)

  return_data()
} else {
  stop("Unknown command")
}

}

