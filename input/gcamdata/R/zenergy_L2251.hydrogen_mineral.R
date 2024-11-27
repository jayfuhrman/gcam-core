# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2251.hydrogen_mineral
#'
#' Mineral inputs required for hydrogen production technologies.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: #' \code{L2251.StubTechMineralCoef}, \code{L2251.GlobalTechMineralCoef},
#' \code{L2251.GlobalTechCost_h2}, \code{L2251.StubTechCost_h2}
#' @details Mineral inputs required for hydrogen production technologies.
#' @author BY Feb 2024
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#'
module_energy_L2251.hydrogen_mineral <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "minerals/supply/A10.mineral_rsrc_info",
             FILE = "minerals/h2/H2.globaltech_mineral_coef_kg_kw_long",
             FILE = "minerals/h2/H2A.globaltech_capFactor",
            "L223.StubTechCapFactor_elec",
            "L225.GlobalTechCost_h2",
            "L225.StubTechCost_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2251.StubTechMineralCoef",
             "L2251.GlobalTechMineralCoef",
             "L2251.GlobalTechCost_h2",
             "L2251.StubTechCost_h2"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    ## ===================================================================
    ## Section 1 -- Load data
    ## ===================================================================

#
#
#     all_data <- load_csv_files(c("minerals/supply/A10.mineral_rsrc_info",
#                                 "minerals/h2/H2.globaltech_mineral_coef_kg_kw_long",
#                                 "minerals/h2/H2A.globaltech_capFactor"),
#                                optionals = c(FALSE, FALSE, FALSE))
    A10.mineral_rsrc_info <- get_data(all_data, "minerals/supply/A10.mineral_rsrc_info", strip_attributes = TRUE)
    H2.globaltech_mineral_coef_kg_kw_long <- get_data(all_data, "minerals/h2/H2.globaltech_mineral_coef_kg_kw_long", strip_attributes = TRUE)
    H2A.globaltech_capFactor <- get_data(all_data, "minerals/h2/H2A.globaltech_capFactor", strip_attributes = TRUE)

    # all_data <- load_from_cache("L223.StubTechCapFactor_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)

    # all_data <- load_from_cache(outputs_of("module_energy_L225.hydrogen"))
    L225.GlobalTechCost_h2 <- get_data(all_data, "L225.GlobalTechCost_h2", strip_attributes = TRUE)
    L225.StubTechCost_h2 <- get_data(all_data, "L225.StubTechCost_h2", strip_attributes = TRUE)


    ## ===================================================================
    ## Section 2 -- Process data
    ## ===================================================================

    #------------------------------------------------------------------------------------------------------------------

    ##  2.2 process the mineral use intensity data

    #   2.2.1 This step is expand the mineral intensity data to all the model year (1975 -- 2100)
    #   For hydrogen production technologies, here all the numbers are based on kg/kW
    #   minicam.non.energy.input differentiates mineral intensity for different components of certain H2 production techs (e.g. solar panels vs. electrolyzers in solar electrolysis)
    L2251.globaltech_mineral_coef <- H2.globaltech_mineral_coef_kg_kw_long %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input, minicam.energy.input), year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input, minicam.energy.input) %>%
      mutate(value = approx_fun(year, value, rule = 1),
             value = round(value, energy.DIGITS_COST)) %>%
      ungroup()


    ##  2.2.2 Convert the mineral coefficient from kg/kW to Mt/EJ
    #   need to do this by different group of technologies

    # Group 1: for solar and wind electrolysis technologies
    # This group will be processed using regional capacity factor, and the output will be added to regional database
    #------------------------------------------------------------------------------------------------------------------

    #Solar electrolysis
    L2251.RegionalTechCapFac_pv <-
      L223.StubTechCapFactor_elec %>% filter(stub.technology %in% c("PV")) %>%
      mutate(supplysector = "H2 central production",
             subsector = "solar",
             technology = "electrolysis")

    #Wind electrolysis
    L2251.RegionalTechCapFac_wind <-
      L223.StubTechCapFactor_elec %>% filter(stub.technology %in% c("wind")) %>%
      mutate(supplysector = "H2 central production",
             subsector = "wind",
             technology = "electrolysis")

    L2251.RegionalTechCapFac_pv_wind <- bind_rows(L2251.RegionalTechCapFac_pv,
                                          L2251.RegionalTechCapFac_wind)

    # mineral coefs (split by minicam.non.energy.input)
    L2251.Regionaltech_mineral_coef_pv_wind_Mt_EJ <-
      L2251.globaltech_mineral_coef %>%
      right_join(L2251.RegionalTechCapFac_pv_wind,
                 by = c("supplysector", "subsector", "technology", "year"),
                 relationship = "many-to-many") %>%
      select(region, supplysector, subsector, technology, year, capacity.factor, minicam.non.energy.input, minicam.energy.input, value) %>%
      mutate(mineral_intensity_Mt_kW = value * CONV_KG_T * CONV_T_MT,
             mineral_intensity_Mt_kWh = mineral_intensity_Mt_kW/(8760 * capacity.factor),
             mineral_intensity_Mt_EJ = mineral_intensity_Mt_kWh / (CONV_KWH_GJ * CONV_GJ_EJ),
             stub.technology = technology) %>%
      select(region, supplysector, subsector, stub.technology, minicam.non.energy.input, minicam.energy.input, year, current.coef = mineral_intensity_Mt_EJ) %>%
      mutate(model.year = year)

    # # add zeros for current coef in every year except the vintage year
    # L2251.Regionaltech_mineral_coef_pv_wind_Mt_EJ_final <-
    #   L2251.Regionaltech_mineral_coef_pv_wind_Mt_EJ %>%
    #   select(region, supplysector, subsector, stub.technology, minicam.non.energy.input, minicam.energy.input, year) %>%
    #   unique() %>%
    #   repeat_add_columns(tibble(model.year = MODEL_YEARS)) %>%
    #   left_join(L2251.Regionaltech_mineral_coef_pv_wind_Mt_EJ,
    #             by = c("region", "supplysector", "subsector", "stub.technology", "minicam.non.energy.input", "minicam.energy.input", "year", "model.year")) %>%
    #   mutate(current.coef = if_else(is.na(current.coef), 0, current.coef))


    # --OUTPUT--
    # combined mineral coef (e.g. solar panels and electrolyzer mineral coefs combined for solar electrolysis technology)
    L2251.StubTechMineralCoef <- L2251.Regionaltech_mineral_coef_pv_wind_Mt_EJ %>%
      group_by(region, supplysector, subsector, stub.technology, year, minicam.energy.input, model.year) %>%
      dplyr::summarise(current.coef = sum(current.coef)) %>%
      ungroup() %>%
      # coefficient = 0 sets current.coef to 0 in all years except the model year.
      mutate(coefficient = 0) %>%
      select(LEVEL2_DATA_NAMES[["RegionalStubTechMineralCurCoef"]])

    # Group 4: All other technologies including coal, gas, biomass, nuclear,
    # This group will be processed based on average capacity factor, and the output will be added to global database
    #------------------------------------------------------------------------------------------------------------------

    L2251.GlobalTechCapFac <- H2A.globaltech_capFactor %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      mutate(capacity.factor = value) %>%
      ungroup() %>%
      select(-value)


    # mineral coefs (split by minicam.non.energy.input)
    L2251.Globaltech_mineral_coef_Mt_EJ <-
      L2251.globaltech_mineral_coef %>%
      right_join(L2251.GlobalTechCapFac,
                 by = c("supplysector", "subsector", "technology", "year"),
                 relationship = "many-to-many") %>%
      select(supplysector, subsector, technology, year, capacity.factor, minicam.non.energy.input, minicam.energy.input, value) %>%
      mutate(mineral_intensity_Mt_kW = value * CONV_KG_T * CONV_T_MT,
             mineral_intensity_Mt_kWh = mineral_intensity_Mt_kW/(8760 * capacity.factor),
             mineral_intensity_Mt_EJ = mineral_intensity_Mt_kWh / (CONV_KWH_GJ * CONV_GJ_EJ),
             sector.name = supplysector,
             subsector.name = subsector) %>%
      select(sector.name, subsector.name, technology, minicam.non.energy.input, minicam.energy.input, year, current.coef = mineral_intensity_Mt_EJ) %>%
      mutate(model.year = year)

    # # add zeros for current coef in every year except the vintage year
    # L2251.Globaltech_mineral_coef_Mt_EJ_final <-
    #   L2251.Globaltech_mineral_coef_Mt_EJ %>%
    #   select(sector.name, subsector.name, technology, minicam.non.energy.input, minicam.energy.input, year) %>%
    #   unique() %>%
    #   repeat_add_columns(tibble(model.year = MODEL_YEARS)) %>%
    #   left_join(L2251.Globaltech_mineral_coef_Mt_EJ,
    #             by = c("sector.name", "subsector.name", "technology", "minicam.non.energy.input", "minicam.energy.input", "year", "model.year")) %>%
    #   mutate(current.coef = if_else(is.na(current.coef), 0, current.coef))

    # --OUTPUT--
    # combined mineral coef (e.g. nuclear generation and electrolyzer mineral coefs combined for nuclear electrolysis technology)
    L2251.GlobalTechMineralCoef <- L2251.Globaltech_mineral_coef_Mt_EJ %>%
      group_by(sector.name, subsector.name, technology, year, minicam.energy.input, model.year) %>%
      dplyr::summarise(current.coef = sum(current.coef)) %>%
      ungroup() %>%
      # coefficient = 0 sets current.coef to 0 in all years except the model year.
      mutate(coefficient = 0) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechMineralCurCoef"]])


    #------------------------------------------------------------------------------------------------------------------


    ##  2.3 process the mineral price and mineral costs

    #  2.3.1 Calculate the mineral costs based on the mineral price information
    #  collect mineral price data and expand it to all model years (assuming mineral price remain constant for now)
    A10.mineral_price <-
      A10.mineral_rsrc_info %>%
      gather_years %>%
      select(resource, price_unit = `price-unit`, year, value) %>%
      complete(nesting(resource,  price_unit), year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(resource,  price_unit, year) %>%
      group_by(resource, price_unit) %>%
      mutate(value = approx_fun(year, value, rule = 2),
             mineral_price = round(value, energy.DIGITS_COST)) %>%
      select(resource, price_unit, year, mineral_price) %>%
      ungroup()

    #  Calculate the mineral cost for hydrogen production technologies.
    #  Need to convert to a levelized cost using a fixed charge rate
    #  Mt/EJ * $1975/kg * 1 EJ/10^9 GJ * 1000 t/1 Mt * 1000 kg/1 t * 0.13 (fixed charge rate) = $/GJ (per year)
    L2251.globaltech_mineral_cost <- L2251.Globaltech_mineral_coef_Mt_EJ %>%
      left_join(A10.mineral_price, by = c("minicam.energy.input" = "resource", "year")) %>%
      mutate(mineral_cost = (current.coef * mineral_price) * 0.13/ (CONV_T_MT*CONV_KG_T * CONV_EJ_GJ)) %>%
      select(sector.name, subsector.name, technology, minicam.non.energy.input, minicam.energy.input, year, mineral_cost)

    L2251.stubtech_mineral_cost <- L2251.Regionaltech_mineral_coef_pv_wind_Mt_EJ %>%
      left_join(A10.mineral_price, by = c("minicam.energy.input" = "resource", "year")) %>%
      mutate(mineral_cost = (current.coef * mineral_price) * 0.13/ (CONV_T_MT*CONV_KG_T * CONV_EJ_GJ)) %>%
      select(region, supplysector, subsector, stub.technology, minicam.non.energy.input, minicam.energy.input, year, mineral_cost)

    L2251.globaltech_mineral_cost_total <- L2251.globaltech_mineral_cost %>%
      group_by(sector.name, subsector.name, technology, minicam.non.energy.input, year) %>%
      dplyr::summarise(mineral.cost = sum(mineral_cost)) %>%
      ungroup()

    L2251.stubtech_mineral_cost_total <- L2251.stubtech_mineral_cost %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.non.energy.input, year) %>%
      dplyr::summarise(mineral.cost = sum(mineral_cost)) %>%
      ungroup()

    #  2.3.2 Subtract the mineral cost from the hydrogen non-energy costs to calculate the H2 non-mineral, non-energy cost, this data will be used to replace the original
    #  non-energy cost in the global database. The unit remains as 1975$US/GJ H2
    L2251.GlobalTechCost_h2 <- L225.GlobalTechCost_h2 %>%
      # filter out solar and wind electrolyzer techs, these costs are read in by region in L224.StubTechCost_h2
      filter(!subsector.name %in% c("solar", "wind")) %>%
      left_join(L2251.globaltech_mineral_cost_total, by = c("sector.name", "subsector.name", "technology", "minicam.non.energy.input", "year")) %>%
      mutate(cost = input.cost - mineral.cost,
             cost = if_else(is.na(cost), input.cost, cost)) %>%
      select(-input.cost, -mineral.cost) %>%
      rename(input.cost = cost) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    L2251.StubTechCost_h2 <- L225.StubTechCost_h2 %>%
      left_join(L2251.stubtech_mineral_cost_total, by = c("region", "supplysector", "subsector", "stub.technology", "minicam.non.energy.input", "year")) %>%
      mutate(cost = input.cost - mineral.cost,
             cost = if_else(is.na(cost), input.cost, cost)) %>%
      select(-input.cost, -mineral.cost) %>%
      rename(input.cost = cost) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]])


    ## ===================================================================
    ## Section 3 -- Produce outputs, add appropriate flags and comments
    ## ===================================================================
    L2251.GlobalTechMineralCoef %>%
      add_title("Mineral intensity data for non-solar and non-wind H2 production technologies") %>%
      add_units("Mt/EJ") %>%
      add_comments("Mineral intensity for most H2 production techs (except solar and wind) are globally specified") %>%
      add_precursors("minerals/supply/A10.mineral_rsrc_info", "minerals/h2/H2.globaltech_mineral_coef_kg_kw_long",
                     "minerals/h2/H2A.globaltech_capFactor") ->
      L2251.GlobalTechMineralCoef

    L2251.StubTechMineralCoef %>%
      add_title("Mineral intensity data for solar and wind electrolysis H2 production technologies") %>%
      add_units("Mt/EJ") %>%
      add_comments("Mineral intensity for solar and wind electrolysis techs are regionally specified (Stub tech)") %>%
      add_precursors("minerals/supply/A10.mineral_rsrc_info", "minerals/h2/H2.globaltech_mineral_coef_kg_kw_long",
                     "L223.StubTechCapFactor_elec") ->
      L2251.StubTechMineralCoef

    L2251.GlobalTechCost_h2 %>%
      add_title("Non-mineral non-energy cost for non-solar and non-wind H2 production technologies") %>%
      add_units("$1975/GJ H2") %>%
      add_comments("Costs for most H2 production techs (except solar and wind) are globally specified") %>%
      add_precursors("minerals/supply/A10.mineral_rsrc_info", "minerals/h2/H2.globaltech_mineral_coef_kg_kw_long",
                     "minerals/h2/H2A.globaltech_capFactor", "L225.GlobalTechCost_h2") ->
      L2251.GlobalTechCost_h2

    L2251.StubTechCost_h2 %>%
      add_title("Non-mineral non-energy cost for solar and wind electrolysis H2 production technologies") %>%
      add_units("$1975/GJ H2") %>%
      add_comments("Costs for solar and wind electrolysis are regionally specified (Stub tech)") %>%
      add_precursors("minerals/supply/A10.mineral_rsrc_info", "minerals/h2/H2.globaltech_mineral_coef_kg_kw_long",
                     "L223.StubTechCapFactor_elec", "L225.StubTechCost_h2") ->
      L2251.StubTechCost_h2

    return_data(L2251.GlobalTechMineralCoef,
                L2251.StubTechMineralCoef,
                L2251.GlobalTechCost_h2,
                L2251.StubTechCost_h2)
  } else {
    stop("Unknown command")
  }
}
