# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2611.Cstorage_variations
#'
#' Create carbon storage resource supply curves from Gidden et. al (2025)
#' For offshore we scale the existing cumulative capacity for each region; for offshore we replace the unlimited resource with supply curves in each region
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#'  @author JF, CB and MG April 2026

module_energy_L2611.Cstorage_variations <- function(command, ...) {
  # --- inputs ---
  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names", # mapping region ids to names
      FILE = "energy/A61.Cstorage_curves_prudentLim", # for offshore curves
      "L161_Gidden2025_MtC_Totals",

      "L261.ResSubresourceProdLifetime",
      "L261.ResReserveTechLifetime",
      "L261.ResReserveTechDeclinePhase",
      "L261.ResReserveTechProfitShutdown",
      "L261.ResReserveTechInvestmentInput",
      "L261.Rsrc")

  # --- outputs ---
  # different kinds of storage mapping to what is in L161_Gidden2025_MtC_Totals
  storage_locales <- c("Onshore", "Offshore")
  storage_categories <- c("technical_potential", "prudent_potential", "current_oil_and_gas_potential")
  vol_combos <- expand.grid(locale=storage_locales, category=storage_categories)
  vol_df_names <- paste0("L263.cstorage_volume_", vol_combos$category, "_", vol_combos$locale)

  cost_multipliers <- data.frame(
    multiplier = c("high", "default", "low", "lowest"),
    value = c(10, 1.0, 0.5, 0.1)
  )
  cost_combos <- expand.grid(locale=storage_locales, kind=cost_multipliers$multiplier)
  cost_df_names <- paste0("L263.cstorage_cost_", cost_combos$kind, "_", cost_combos$locale)

  # order is volume_kind_locale then cost_kind_locale
  MODULE_OUTPUTS <- c(vol_df_names, cost_df_names)

  # --- volumetric data function ---
  create_volume_df <- function(data, curve_data, locale, regions) {
    data %>%
      # Filter data where storage_type matches `locale`
      filter(storage_type == locale) %>%
      # Melt the DataFrame (convert wide to long format)
      pivot_longer(
        cols = storage_categories, # Cande: changed from storage_kinds to storage_categories
        names_to = "category",
        values_to = "volume"
      ) %>%
      # Merge (join) with the curve_data for the given `locale`
      # Equivalent to pandas merge(..., how='cross')
      mutate(dummy_key = 1) %>% # Create a key for cross join
      left_join(curve_data[[locale]] %>% mutate(dummy_key = 1), by = "dummy_key") %>%
      select(-c(dummy_key)) %>% # Remove temporary column after cross join
      # Merge with regions on GCAM_region_ID
      left_join_error_no_match(regions, by = "GCAM_region_ID") %>%
      # Create new column based on formula
      mutate(available = round(fraction * volume, energy.DIGITS_COST)) %>% #Cande: changed DIGITS_COST to energy.DIGITS_COST
      # Drop unnecessary columns
      select(-c(storage_type, volume, fraction, GCAM_region_ID, cost_2005USDtCO2)) %>%
      # Sort by specific columns
      arrange(category, region, grade)
  }

  # --- cost data function ---
  create_cost_df <- function(curve_data, locale, regions) {
    factor <- emissions.CONV_C_CO2 / gdp_deflator(2005, 1990)

    curve_data[[locale]] %>%
      # Cross join with `regions`
      mutate(dummy_key = 1) %>%
      left_join(regions %>% mutate(dummy_key = 1), by = "dummy_key") %>%
      select(-dummy_key) %>%  # Remove temporary key column

      # Cross join with `multipliers`
      mutate(dummy_key = 1) %>%
      left_join(cost_multipliers %>% mutate(dummy_key = 1), by = "dummy_key") %>% #Cande changed this from multipliers to cost_multipliers
      select(-dummy_key) %>%  # Remove temporary key column

      # Compute extraction cost dynamically, similar to assign(lambda ...)
      mutate(extractioncost = round(cost_2005USDtCO2 * value * factor, energy.DIGITS_COST)) %>%

      # Drop unnecessary columns
      select(-c(fraction, value, GCAM_region_ID, cost_2005USDtCO2)) %>%

      # Sort by specified columns
      arrange(multiplier, region, grade)
  }

  # --- module body ---
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    #return(MODULE_OUTPUTS)
    return(c("L263.cstorage_volume_current_oil_and_gas_potential_Offshore",
             "L263.cstorage_volume_current_oil_and_gas_potential_Onshore",
             "L263.cstorage_volume_prudent_potential_Offshore",
             "L263.cstorage_volume_prudent_potential_Onshore",
             "L263.cstorage_volume_technical_potential_Offshore",
             "L263.cstorage_volume_technical_potential_Onshore",
             "L263.cstorage_cost_high_Onshore",
             "L263.cstorage_cost_high_Offshore",
             "L263.cstorage_cost_default_Onshore",
             "L263.cstorage_cost_default_Offshore",
             "L263.cstorage_cost_low_Onshore",
             "L263.cstorage_cost_low_Offshore",
             "L263.cstorage_cost_lowest_Onshore",
             "L263.cstorage_cost_lowest_Offshore",

             "L261.ResSubresourceProdLifetimeOffshore",
             "L261.ResReserveTechLifetimeOffshore",
             "L261.ResReserveTechDeclinePhaseOffshore",
             "L261.ResReserveTechProfitShutdownOffshore",
             "L261.ResReserveTechInvestmentInputOffshore",
             "L261.RsrcOffshore",
             "L261.DeleteUnlimitRsrc",
             "L261.DeleteRsrc"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    #A61.Cstorage_curves <- get_data(all_data, "energy/A61.Cstorage_curves")
    A61.Cstorage_curves_prudentLim <- get_data(all_data, "energy/A61.Cstorage_curves_prudentLim")
    L161_Gidden2025_MtC_Totals <- get_data(all_data, "L161_Gidden2025_MtC_Totals")


    L261.ResSubresourceProdLifetime <- get_data(all_data,"L261.ResSubresourceProdLifetime", strip_attributes = TRUE)

    L261.ResSubresourceProdLifetimeOffshore <- L261.ResSubresourceProdLifetime %>%
      mutate(resource = "offshore carbon-storage",
             reserve.subresource = resource) %>%
      add_precursors("L261.ResSubresourceProdLifetime")

    L261.ResReserveTechLifetime <- get_data(all_data,"L261.ResReserveTechLifetime", strip_attributes = TRUE)

    L261.ResReserveTechLifetimeOffshore <- L261.ResReserveTechLifetime %>%
      mutate(resource = "offshore carbon-storage",
             reserve.subresource = resource,
             resource.reserve.technology = reserve.subresource) %>%
      add_precursors("L261.ResReserveTechLifetime")

    L261.ResReserveTechDeclinePhase <- get_data(all_data, "L261.ResReserveTechDeclinePhase", strip_attributes = TRUE)

    L261.ResReserveTechDeclinePhaseOffshore <-  L261.ResReserveTechDeclinePhase %>%
      mutate(resource = "offshore carbon-storage",
             reserve.subresource = resource,
             resource.reserve.technology = reserve.subresource) %>%
      add_precursors("L261.ResReserveTechDeclinePhase")

    L261.ResReserveTechProfitShutdown <- get_data(all_data,"L261.ResReserveTechProfitShutdown", strip_attributes = TRUE)

    L261.ResReserveTechProfitShutdownOffshore <- L261.ResReserveTechProfitShutdown %>%
      mutate(resource = "offshore carbon-storage",
             reserve.subresource = resource,
             resource.reserve.technology = reserve.subresource) %>%
      add_precursors("L261.ResReserveTechProfitShutdown")

    L261.ResReserveTechInvestmentInput <- get_data(all_data,"L261.ResReserveTechInvestmentInput", strip_attributes = TRUE)

    L261.ResReserveTechInvestmentInputOffshore <-  L261.ResReserveTechInvestmentInput %>%
      mutate(resource = "offshore carbon-storage",
             reserve.subresource = resource,
             resource.reserve.technology = reserve.subresource) %>%
      add_precursors("L261.ResReserveTechInvestmentInput")

    L261.RsrcOffshore <- get_data(all_data, "L261.Rsrc", strip_attributes = TRUE) %>%
      mutate(resource = "offshore carbon-storage") %>%
      add_precursors("L261.Rsrc")

    L261.DeleteUnlimitRsrc <- tibble(unlimited.resource = "offshore carbon-storage") %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["DeleteUnlimitRsrc"]],GCAM_region_names) %>%
      add_precursors("common/GCAM_region_names")

    L261.DeleteRsrc <- tibble(resource = "onshore carbon-storage") %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["DeleteRsrc"]],GCAM_region_names) %>%
      add_precursors("common/GCAM_region_names")

    # Create helper variables
    curve_data <- list(
      Onshore = A61.Cstorage_curves_prudentLim %>% filter(resource == "onshore carbon-storage"),
      Offshore = A61.Cstorage_curves_prudentLim %>% filter(resource == "offshore carbon-storage")
    )

    # --- Produce outputs ---
    # order is volume_kind_locale then cost_kind_locale
    for (locale in storage_locales) {
      vol_df = create_volume_df(L161_Gidden2025_MtC_Totals, curve_data, locale, GCAM_region_names)
      cost_df = create_cost_df(curve_data, locale, GCAM_region_names)
      for (cat in storage_categories) { # volume
        df_name <- paste0("L263.cstorage_volume_", cat, "_", locale)
        title <- paste0("Supply curve volumes for ", locale, " storage of type: ", cat)
        vol_df %>%
          filter(category == cat) %>%
          select(-category) %>%
          add_title(title) %>%
          add_units("MtCO2") %>%
          add_comments("Volumes taken from Gidden et al 2025 mapped to GCAM regions") %>%
          add_legacy_name(df_name) %>%
          add_precursors("common/GCAM_region_names", "L161_Gidden2025_MtC_Totals", "energy/A61.Cstorage_curves_prudentLim") ->
          x
        assign(df_name, x)
      }
      for (kind in cost_multipliers$multiplier) { # costs
        df_name_cost <- paste0("L263.cstorage_cost_", kind, "_", locale)
        title <- paste0("Supply curve costs for ", locale, " using a multiplier of type: ", kind)
        cost_df %>%
          filter(multiplier == kind) %>%
          select(-multiplier) %>%
          add_title(title, overwrite=T) %>%
          add_units("1990$/tCO2") %>%
          add_comments("Costs based on A61 values with simple multiplier assumptions and mapped to GCAM regions") %>%
          add_legacy_name(df_name_cost) %>%
          add_precursors("common/GCAM_region_names", "energy/A61.Cstorage_curves_prudentLim") ->
          y
        assign(df_name_cost, y)
      }
    }

    # return_data(MODULE_OUTPUTS)
    return_data(L263.cstorage_volume_current_oil_and_gas_potential_Offshore,
                L263.cstorage_volume_current_oil_and_gas_potential_Onshore,
                L263.cstorage_volume_prudent_potential_Offshore,
                L263.cstorage_volume_prudent_potential_Onshore,
                L263.cstorage_volume_technical_potential_Offshore,
                L263.cstorage_volume_technical_potential_Onshore,
                L263.cstorage_cost_high_Onshore,
                L263.cstorage_cost_high_Offshore,
                L263.cstorage_cost_default_Onshore,
                L263.cstorage_cost_default_Offshore,
                L263.cstorage_cost_low_Onshore,
                L263.cstorage_cost_low_Offshore,
                L263.cstorage_cost_lowest_Onshore,
                L263.cstorage_cost_lowest_Offshore,

                L261.ResSubresourceProdLifetimeOffshore,
                L261.ResReserveTechLifetimeOffshore,
                L261.ResReserveTechDeclinePhaseOffshore,
                L261.ResReserveTechProfitShutdownOffshore,
                L261.ResReserveTechInvestmentInputOffshore,


                L261.RsrcOffshore,
                L261.DeleteUnlimitRsrc,
                L261.DeleteRsrc)
  } else {
    stop("Unknown command")
  }
}
