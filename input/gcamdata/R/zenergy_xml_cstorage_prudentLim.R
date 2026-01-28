# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_cstorage_variations_xml
#'
#' Construct XML data structures for \code{cstorage_cost_*xml}
#' and \code{cstorage_volume_*xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
module_energy_cstorage_variations_xml <- function(command, ...) {

  # --- combinations ---
  storage_locales <- c("Onshore", "Offshore")
  storage_categories <- c("technical_potential", "prudent_potential", "current_oil_and_gas_potential")
  vol_combos <- expand.grid(locale = storage_locales, category = storage_categories)
  cost_multipliers <- c("high", "default", "low", "lowest")
  cost_combos <- expand.grid(locale = storage_locales, kind = cost_multipliers)

  # --- inputs ---
  vol_df_names  <- paste0("L263.cstorage_volume_", vol_combos$category, "_", vol_combos$locale)
  cost_df_names <- paste0("L263.cstorage_cost_", cost_combos$kind, "_", cost_combos$locale)
  MODULE_INPUTS <- c(vol_df_names, cost_df_names)

  # --- outputs ---
  vol_fnames  <- paste0("cstorage_volume_", vol_combos$category, "_", vol_combos$locale, ".xml")
  cost_fnames <- paste0("cstorage_cost_", cost_combos$kind, "_", cost_combos$locale, ".xml")
  MODULE_OUTPUTS <- c(vol_fnames, cost_fnames)

  # --- logic ---
  if(command == driver.DECLARE_INPUTS) {

    return(MODULE_INPUTS)

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cstorage_volume_technical_potential_Onshore.xml",
             XML = "cstorage_volume_technical_potential_Offshore.xml",
             XML = "cstorage_volume_prudent_potential_Onshore.xml",
             XML = "cstorage_volume_prudent_potential_Offshore.xml",
             XML = "cstorage_volume_current_oil_and_gas_potential_Onshore.xml",
             XML = "cstorage_volume_current_oil_and_gas_potential_Offshore.xml",
             XML = "cstorage_cost_high_Onshore.xml",
             XML = "cstorage_cost_high_Offshore.xml",
             XML = "cstorage_cost_default_Onshore.xml",
             XML = "cstorage_cost_default_Offshore.xml",
             XML = "cstorage_cost_low_Onshore.xml",
             XML = "cstorage_cost_low_Offshore.xml",
             XML = "cstorage_cost_lowest_Onshore.xml",
             XML = "cstorage_cost_lowest_Offshore.xml"))

  } else if(command == driver.MAKE) {

    # Silence package checks (predeclare objects)
    cstorage_volume_technical_potential_Onshore.xml <-
      cstorage_volume_technical_potential_Offshore.xml <-
      cstorage_volume_prudent_potential_Onshore.xml <-
      cstorage_volume_prudent_potential_Offshore.xml <-
      cstorage_volume_current_oil_and_gas_potential_Onshore.xml <-
      cstorage_volume_current_oil_and_gas_potential_Offshore.xml <-
      cstorage_cost_high_Onshore.xml <-
      cstorage_cost_high_Offshore.xml <-
      cstorage_cost_default_Onshore.xml <-
      cstorage_cost_default_Offshore.xml <-
      cstorage_cost_low_Onshore.xml <-
      cstorage_cost_low_Offshore.xml <-
      cstorage_cost_lowest_Onshore.xml <-
      cstorage_cost_lowest_Offshore.xml <- NULL

    all_data <- list(...)[[1]]

    # --- build volume XMLs ---
    for(i in seq_along(vol_fnames)) {
      create_xml(vol_fnames[[i]]) %>%
        add_node_equiv_xml("subresource") %>%
        add_node_equiv_xml("technology") %>%
        add_xml_data(get_data(all_data, vol_df_names[[i]]), "RsrcCurvesAvail") %>%
        add_node_equiv_xml("resource") %>%
        add_node_equiv_xml("subresource") %>%
        add_precursors(vol_df_names[[i]]) ->
        x
      assign(vol_fnames[[i]], x)
    }

    # --- build cost XMLs ---
    for(i in seq_along(cost_fnames)) {
      create_xml(cost_fnames[[i]]) %>%
        add_node_equiv_xml("subresource") %>%
        add_node_equiv_xml("technology") %>%
        add_xml_data(get_data(all_data, cost_df_names[[i]]), "RsrcCurvesExtCost") %>%
        add_precursors(cost_df_names[[i]]) ->
        y
      assign(cost_fnames[[i]], y)
    }

    # return all outputs explicitly
    return_data(
      cstorage_volume_technical_potential_Onshore.xml,
      cstorage_volume_technical_potential_Offshore.xml,
      cstorage_volume_prudent_potential_Onshore.xml,
      cstorage_volume_prudent_potential_Offshore.xml,
      cstorage_volume_current_oil_and_gas_potential_Onshore.xml,
      cstorage_volume_current_oil_and_gas_potential_Offshore.xml,
      cstorage_cost_high_Onshore.xml,
      cstorage_cost_high_Offshore.xml,
      cstorage_cost_default_Onshore.xml,
      cstorage_cost_default_Offshore.xml,
      cstorage_cost_low_Onshore.xml,
      cstorage_cost_low_Offshore.xml,
      cstorage_cost_lowest_Onshore.xml,
      cstorage_cost_lowest_Offshore.xml
    )

  } else {
    stop("Unknown command")
  }
}
