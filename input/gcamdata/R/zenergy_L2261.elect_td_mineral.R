# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2261.elect_td_mineral
#'
#' Generate the level 2 data tables for a vintaged electricity transmission and distribution sector (with mineral coefficients)
#' including capital costs, shareweights, logits, and interpolations as well as energy use coefficients
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2261.StubTechCost_elect_td}, \code{L2261.StubTechCoef_elect_td_mineral_final},
#' \code{L2261.StubTechLifetime_elect_td},\code{L2261.StubTechSCurve_elect_td}, \code{L2261.StubTechProfitShutdown_elect_td}
#' @details Prepares Level 2 data on electricity T&D sector for the generation of elect_td_mineral.xml.
#' Creates global technology database info--cost, shareweight, logit, efficiencies, and interpolations--and regional values where applicable for transmission and distribution.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @importFrom tibble tibble
#' @author BY August 2024
module_energy_L2261.elect_td_mineral <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "minerals/td/A26.td_technology_vintage",
             FILE = "minerals/td/A26.td_mineral_coef_Mt_EJ",
             FILE = "minerals/td/A26.td_nonenergy_cost_nonmineral",
             "L126.out_EJ_R_electd_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2261.StubTechCost_elect_td",
             "L2261.StubTechCoef_elect_td_mineral_final",
             "L2261.StubTechLifetime_elect_td",
             "L2261.StubTechSCurve_elect_td",
             "L2261.StubTechProfitShutdown_elect_td"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check

    all_data <- list(...)[[1]]
    # all_data <- load_from_cache(outputs_of(c("module_energy_L226.en_distribution",
    #                                        "module_energy_L2392.gas_trade")))

    # Load required inputs
    A26.td_technology_vintage <- get_data(all_data, "minerals/td/A26.td_technology_vintage", strip_attributes = TRUE)
    A26.td_mineral_coef_Mt_EJ <- get_data(all_data, "minerals/td/A26.td_mineral_coef_Mt_EJ", strip_attributes = TRUE)
    A26.td_nonenergy_cost_nonmineral <- get_data(all_data, "minerals/td/A26.td_nonenergy_cost_nonmineral", strip_attributes = TRUE)
#
#     A26.td_technology_vintage <- readr::read_csv("inst/extdata/minerals/td/A26.td_technology_vintage.csv", skip = 7)
#     A26.td_mineral_coef_Mt_EJ <- readr::read_csv("inst/extdata/minerals/td/A26.td_mineral_coef_Mt_EJ.csv", skip = 7)
#     A26.td_nonenergy_cost_nonmineral <- readr::read_csv("inst/extdata/minerals/td/A26.td_nonenergy_cost_nonmineral.csv", skip = 7)

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    L126.out_EJ_R_electd_F_Yh <- get_data(all_data, "L126.out_EJ_R_electd_F_Yh", strip_attributes = TRUE)

    # ===================================================

    # 2. Build tables for CSVs

    #L2261.StubTechCost_elect_td: non-mineral cost for elec T&D technologies
    #Add historical base years
    L2261.StubTechCost_elect_td_base_years <- A26.td_nonenergy_cost_nonmineral %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS)))

    L2261.StubTechCost_elect_td <- A26.td_nonenergy_cost_nonmineral %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      bind_rows(L2261.StubTechCost_elect_td_base_years) %>%
      arrange(supplysector, subsector, stub.technology, region, year)

    #L2261.StubTechCoef_elect_td_mineral: mineral intensities for elec T&D technologies
    #add historical base years
    A26.td_mineral_coef_Mt_EJ_base_years <- A26.td_mineral_coef_Mt_EJ %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS)))

    L2261.StubTechCoef_elect_td_mineral <- A26.td_mineral_coef_Mt_EJ %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      bind_rows(A26.td_mineral_coef_Mt_EJ_base_years) %>%
      arrange(region, year, mineral) %>%
      #using left join as the number of rows is changing x 3 for T&D technologies: elect_td_bld, elect_td_ind, elect_td_trn
      left_join(select(L2261.StubTechCost_elect_td, -minicam.non.energy.input, -input.cost), by = c("region", "year")) %>%
      rename(minicam.energy.input = mineral,
             current.coef = value) %>%
      mutate(model.year = year,
             coefficient = 0,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["RegionalStubTechMineralCurCoef"]])

    #Vintage the T&D technologies
    L2261.StubTech_elect_td_vintage <- L2261.StubTechCost_elect_td %>%
      select(-minicam.non.energy.input, -input.cost) %>%
      filter(year %in% c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
      left_join_error_no_match(A26.td_technology_vintage, by = c("supplysector", "subsector", "stub.technology" = "technology"))

    L2261.StubTechLifetime_elect_td <- L2261.StubTech_elect_td_vintage %>%
      select(LEVEL2_DATA_NAMES[["StubTechLifetime"]])

    L2261.StubTechSCurve_elect_td <- L2261.StubTech_elect_td_vintage %>%
      select(LEVEL2_DATA_NAMES[["StubTechSCurve"]])

    L2261.StubTechProfitShutdown_elect_td <- L2261.StubTech_elect_td_vintage %>%
      select(LEVEL2_DATA_NAMES[["StubTechProfitShutdown"]])

    #------------------------------------------------------------------------------------------------------------------

    ## BY 7-7-2025: Regionalize demands
    ## For minerals that are now traded, we need to differentiate mineral supply and demand
    # Mineral supplies are named as: copper, lithium, nickel
    # Mineral demands are named as: regional copper, regional lithium, regional nickel
    L2261.StubTechCoef_elect_td_mineral_regMineralInput <- regionalize_mineral_inputs(L2261.StubTechCoef_elect_td_mineral)

    ## BY 7-28-2025: Modify mineral intensities in the base years such that we would have the equivalent mineral demands if we
    # had the service demand representing solely the new investment (i.e. if base years were vintaged)

    # First, calculate the "new investment" in each base year.
    # We assume a depreciation rate of 5% annually.
    L226.NewInvestment_electd <- L126.out_EJ_R_electd_F_Yh %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID")) %>%
      select(-GCAM_region_ID, -sector, -fuel) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(output = value) %>%
      group_by(region) %>%
      arrange(year) %>%
      mutate(lag_output = lag(output),
             lag_year = lag(year),
             years_elapsed = year - lag_year,
             remaining_stock = lag_output * (1 - 0.05)^years_elapsed,
             new_investment = output - remaining_stock,
             new_investment = pmax(new_investment, 0),
             new_investment = if_else(year == 1975, output, new_investment)) %>%
      ungroup()


    L2261.StubTechCoef_elect_td_mineral_modified <- L226.NewInvestment_electd %>%
      # Join in the mineral intensity coefficient
      # Using LJ as it is not a 1-to-1 mapping
      left_join(filter(L2261.StubTechCoef_elect_td_mineral_regMineralInput, year %in% MODEL_BASE_YEARS),
                by = c("region", "year")) %>%
      # adjust the mineral intensities by the ratio between the incremental service demand and the original service demand
      mutate(current.coef_new = if_else(output == 0, 0, current.coef * (new_investment / output))) %>%
      # replace the current coef with the incremental current coef
      mutate(current.coef = current.coef_new) %>%
      select(LEVEL2_DATA_NAMES[["RegionalStubTechMineralCurCoef"]])

    L2261.StubTechCoef_elect_td_mineral_final <- bind_rows(L2261.StubTechCoef_elect_td_mineral_modified,
                                                           filter(L2261.StubTechCoef_elect_td_mineral_regMineralInput, !(year %in% MODEL_BASE_YEARS)))
    # ===================================================
    L2261.StubTechCost_elect_td  %>%
      add_title("Regional-specific non-mineral non-energy cost for elect_td technologies") %>%
      add_units("1975$/GJ electricity") %>%
      add_comments("calculated based on scalars from previous global elect_td costs and subtract mineral costs") %>%
      add_precursors("minerals/td/A26.td_nonenergy_cost_nonmineral") ->
      L2261.StubTechCost_elect_td

    L2261.StubTechCoef_elect_td_mineral_final  %>%
      add_title("Mineral intensity data for elect_td technologies") %>%
      add_units("Mt/EJ") %>%
      add_comments("Regional-specific and time-varying mineral intensity for elect_td technologies. Assume same for elect_td_bld, _ind, and _trn.") %>%
      add_precursors("minerals/td/A26.td_mineral_coef_Mt_EJ",
                     "common/GCAM_region_names",
                     "L126.out_EJ_R_electd_F_Yh") ->
      L2261.StubTechCoef_elect_td_mineral_final

    L2261.StubTechLifetime_elect_td %>%
      add_title("Lifetimes for elect_td technologies") %>%
      add_units("NA") %>%
      add_comments("Lifetimes for elect_td technologies") %>%
      add_precursors("minerals/td/A26.td_technology_vintage") ->
      L2261.StubTechLifetime_elect_td

    L2261.StubTechSCurve_elect_td %>%
      add_title("S-Curve for elect_td technologies") %>%
      add_units("NA") %>%
      add_comments("S-Curve for elect_td technologies") %>%
      add_precursors("minerals/td/A26.td_technology_vintage") ->
      L2261.StubTechSCurve_elect_td

    L2261.StubTechProfitShutdown_elect_td %>%
      add_title("Profit shutdown for elect_td technologies") %>%
      add_units("NA") %>%
      add_comments("Profit shutdown for elect_td technologies") %>%
      add_precursors("minerals/td/A26.td_technology_vintage") ->
      L2261.StubTechProfitShutdown_elect_td

    return_data(L2261.StubTechCost_elect_td,
                L2261.StubTechCoef_elect_td_mineral_final,
                L2261.StubTechLifetime_elect_td,
                L2261.StubTechSCurve_elect_td,
                L2261.StubTechProfitShutdown_elect_td)
  } else {
    stop("Unknown command")
  }
}
