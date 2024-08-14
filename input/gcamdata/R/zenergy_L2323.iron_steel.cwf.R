# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2323.iron_steel
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for iron and steel-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2323.SectorLogitTables[[ curr_table ]]$data}, \code{L2323.Supplysector_iron_steel}, \code{L2323.FinalEnergyKeyword_iron_steel},
#' \code{L2323.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2323.SubsectorLogit_iron_steel}, \code{L2323.SubsectorShrwtFllt_iron_steel},
#' \code{L2323.SubsectorInterp_iron_steel}, \code{L2323.StubTech_iron_steel}, \code{L2323.GlobalTechShrwt_iron_steel}, \code{L2323.GlobalTechCoef_iron_steel},
#' \code{L2323.GlobalTechCost_iron_steel}, \code{L2323.GlobalTechCapture_iron_steel}, \code{L2323.StubTechProd_iron_steel}, \code{L2323.StubTechCalInput_iron_steel},
#' \code{L2323.StubTechCoef_iron_steel}, \code{L2323.PerCapitaBased_iron_steel}, \code{L2323.BaseService_iron_steel}, \code{L2323.PriceElasticity_iron_steel},
#' \code{L2323.GlobalTechCoef_iron_steel_cwf}, \code{L2323.StubTechCoef_iron_steel_cwf}, \code{L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios}, \code{object}. The corresponding file in the
#' original data system was \code{L2323.iron_steel.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for iron and steel sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019, Siddarth Durga April 2023
module_energy_L2323.iron_steel_cwf <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "cwf/A323.globaltech_shrwt_cwf",
			       FILE = "cwf/A323.globaltech_coef_cwf_adj",
			       FILE = "cwf/A323.globaltech_shrwt_cwf_H2_scenarios",
			       "L2323.GlobalTechCoef_iron_steel",
			       "L1323.IO_GJkg_R_iron_steel_F_Yh",
			       "L2323.StubTechProd_iron_steel"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2323.GlobalTechCoef_iron_steel_cwf",
			       "L2323.StubTechCoef_iron_steel_cwf",
			       "L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios",
			       "L2323.GlobalTechShrwt_iron_steel_cwf"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A323.globaltech_shrwt_cwf <- get_data(all_data, "cwf/A323.globaltech_shrwt_cwf", strip_attributes = TRUE)
    A323.globaltech_coef_cwf_adj <- get_data(all_data, "cwf/A323.globaltech_coef_cwf_adj", strip_attributes = TRUE)
    A323.globaltech_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A323.globaltech_shrwt_cwf_H2_scenarios", strip_attributes = TRUE)
    L2323.GlobalTechCoef_iron_steel <- get_data(all_data, "L2323.GlobalTechCoef_iron_steel", strip_attributes = TRUE)
    L1323.IO_GJkg_R_iron_steel_F_Yh <- get_data(all_data, "L1323.IO_GJkg_R_iron_steel_F_Yh", strip_attributes = TRUE)
    L2323.StubTechProd_iron_steel <- get_data(all_data, "L2323.StubTechProd_iron_steel", strip_attributes = TRUE)

    # ===================================================
    # Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- year.x <- year.y <- tech.share.weight <- stub.technology <-
      market.name <- sector.name <- subsector.name <- terminal_coef <- share.weight.year <- coeff <-
      Plant_ID <- Model_Year <- Primary_Production_Route <- Category <- Component <- Value <-
      Unit <- Country <- capital_cost_frac <- ccs_cost_adder <- NULL


    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs, by = c("supplysector", "subsector", "technology", "fuel")) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT),
             stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2323.StubTechCoef_iron_steel_tmp

    # ===================================================
    # Make CWF adjustments

    # GLOBAL TECH COEF: L2323.GlobalTechCoef_iron_steel_cwf
    # get adjustments
    A323.globaltech_coef_cwf_adj %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient_adj = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      dplyr::select(-value) ->
      L2323.globaltech_coef_cwf_adj

    # apply adjustments to global tech coefficients
    L2323.GlobalTechCoef_iron_steel %>%
      left_join_error_no_match(L2323.globaltech_coef_cwf_adj) %>%
      mutate(coefficient = round(coefficient * coefficient_adj, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2323.GlobalTechCoef_iron_steel_cwf

    # STUB TECH COEF: L2323.StubTechCoef_iron_steel_cwf
    # some of these converge to the global tech values, so the convergence needs to be updated.
    # also need to apply the reduction in coefficients to these values
    L2323.StubTechCoef_iron_steel_tmp %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(L2323.GlobalTechCoef_iron_steel %>% rename(terminal_coef = coefficient,supplysector = sector.name,subsector = subsector.name),
                       supplysector, subsector, technology, minicam.energy.input, terminal_coef, year),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input","year")) %>%
      left_join(L2323.StubTechCoef_iron_steel_tmp %>%mutate(coeff = coefficient,coefficient=NULL),
                by = c("region", "supplysector", "subsector", "stub.technology", "minicam.energy.input", "market.name", "year")) %>%
      left_join(L2323.StubTechProd_iron_steel %>% select(-share.weight.year,-subs.share.weight,-tech.share.weight),
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(coefficient = if_else(year > MODEL_FINAL_BASE_YEAR , coeff, coefficient),
             coefficient = if_else(year > MODEL_FINAL_BASE_YEAR & stub.technology == energy.IRON_STEEL.DEFAULT_COEF[1] , terminal_coef, coefficient),
             coefficient = if_else(year > MODEL_FINAL_BASE_YEAR & minicam.energy.input == energy.IRON_STEEL.DEFAULT_COEF[2] , terminal_coef, coefficient),
             coefficient = if_else(year > MODEL_FINAL_BASE_YEAR & minicam.energy.input == energy.IRON_STEEL.DEFAULT_COEF[3] , terminal_coef, coefficient)) %>%
      select(-terminal_coef,-coeff,-calOutputValue) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient,rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      # apply CWF adjustments
      left_join(L2323.globaltech_coef_cwf_adj %>%
                  rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology)) %>%
      mutate(coefficient = round(coefficient * coefficient_adj, energy.DIGITS_COEFFICIENT)) %>%
      filter(year %in% MODEL_YEARS) %>% # drop the terminal coef year if it's outside of the model years
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2323.StubTechCoef_iron_steel_cwf

    A323.globaltech_shrwt_cwf %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L2323.GlobalTechShrwt_iron_steel_cwf

    # TECHNOLOGY SHARE WEIGHTS: L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios
    A323.globaltech_shrwt_cwf_H2_scenarios %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, scenario), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(scenario, supplysector, subsector, technology, year) %>%
      group_by(scenario, supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight", "scenario") ->
      L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios

    # ===================================================
    # Produce outputs

    L2323.GlobalTechShrwt_iron_steel_cwf %>%
      add_title("Shareweights of global iron and steel technologies") %>%
      add_units("Unitless") %>%
      add_comments("Phase out new blast furnace and EAF without CCS by 2050") %>%
      add_legacy_name("L2323.GlobalTechShrwt_iron_steel") %>%
      add_precursors("cwf/A323.globaltech_shrwt_cwf") ->
      L2323.GlobalTechShrwt_iron_steel_cwf


    L2323.GlobalTechCoef_iron_steel_cwf %>%
      add_title("Energy inputs and coefficients of iron and steel technologies") %>%
      add_units("scrap input is unitless (Mt scrap per Mt steel); all others are GJ per kg (EJ of energy per Mt of steel)") %>%
      add_comments("For iron and steel sector, the energy use coefficients from A323.globaltech_coef are interpolated into all model years, with CWF adjustments") %>%
      add_legacy_name("L2323.GlobalTechCoef_iron_steel") %>%
      add_precursors("L2323.GlobalTechCoef_iron_steel", "cwf/A323.globaltech_coef_cwf_adj") ->
      L2323.GlobalTechCoef_iron_steel_cwf

    L2323.StubTechCoef_iron_steel_cwf %>%
      add_title("region-specific coefficients of iron and steel production technologies") %>%
      add_units("scrap input is unitless (Mt scrap per Mt steel); all others are GJ per kg (EJ of energy per Mt of steel)") %>%
      add_comments("Coefficients are calculated using L1323.IO_GJkg_R_iron_steel_F_Yh") %>%
      add_legacy_name("L2323.StubTechCoef_iron_steel") %>%
      add_precursors("energy/calibrated_techs", "L1323.IO_GJkg_R_iron_steel_F_Yh", "common/GCAM_region_names") ->
      L2323.StubTechCoef_iron_steel_cwf

    L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios %>%
      add_title("Shareweights of global iron and steel technologies") %>%
      add_units("Unitless") %>%
      add_comments("For iron and steel sector, the share weights from A323.globaltech_shrwt_cwf_H2_scenarios are interpolated into all base years and future years") %>%
      add_legacy_name("L2323.GlobalTechShrwt_iron_steel") %>%
      add_precursors("cwf/A323.globaltech_shrwt_cwf_H2_scenarios") ->
      L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios

      return_data(L2323.GlobalTechShrwt_iron_steel_cwf, L2323.GlobalTechCoef_iron_steel_cwf,
                  L2323.StubTechCoef_iron_steel_cwf, L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios)
  } else {
    stop("Unknown command")
  }
}
