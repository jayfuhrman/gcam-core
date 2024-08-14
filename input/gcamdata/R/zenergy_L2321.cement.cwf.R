# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2321.cement_cwf
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for cement-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2321.SectorLogitTables[[ curr_table ]]$data}, \code{L2321.Supplysector_cement}, \code{L2321.FinalEnergyKeyword_cement},
#' \code{L2321.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2321.SubsectorLogit_cement}, \code{L2321.SubsectorShrwtFllt_cement},
#' \code{L2321.SubsectorInterp_cement}, \code{L2321.StubTech_cement}, \code{L2321.GlobalTechShrwt_cement}, \code{L2321.GlobalTechCoef_cement}, \code{L2321.GlobalTechCoef_cement_cwf},
#' \code{L2321.GlobalTechCost_cement}, \code{L2321.GlobalTechCapture_cement}, \code{L2321.StubTechProd_cement}, \code{L2321.StubTechCalInput_cement_heat},
#' \code{L2321.StubTechCoef_cement},  \code{L2321.StubTechCoef_cement_cwf}, \code{L2321.PerCapitaBased_cement}, \code{L2321.BaseService_cement}, \code{L2321.PriceElasticity_cement},
#' \code{L2321.IncomeElasticity_cement_gcam3}, \code{L2321.IncomeElasticity_cement_gssp1}, \code{L2321.IncomeElasticity_cement_gssp2},
#' \code{L2321.IncomeElasticity_cement_gssp3}, \code{L2321.IncomeElasticity_cement_gssp4}, \code{L2321.IncomeElasticity_cement_gssp5},
#' \code{L2321.IncomeElasticity_cement_ssp1}, \code{L2321.IncomeElasticity_cement_ssp2}, \code{L2321.IncomeElasticity_cement_ssp3},
#' \code{L2321.IncomeElasticity_cement_ssp4}, \code{L2321.IncomeElasticity_cement_ssp5}, \code{L2321.IncomeElasticity_cement_cwf},
#' \code{L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios},  \code{L2321.SubsectorInterp_cement_cwf_H2_scenarios}, \code{object}. The corresponding file in the
#' original data system was \code{L2321.cement.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for cement sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr complete nesting
#' @author LF October 2017
module_energy_L2321.cement_cwf <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             "L2321.GlobalTechCoef_cement",
             "L2321.StubTechCoef_cement",
			 FILE = "cwf/A321.globaltech_coef_cwf_adj",
			 FILE = "cwf/A321.incelas_cwf",
			 FILE = "cwf/A321.subsector_interp_cwf",
			 FILE = "cwf/A321.subsector_shrwt_cwf",
			 FILE = "cwf/A321.subsector_interp_cwf_H2_scenarios",
			 FILE = "cwf/A321.subsector_shrwt_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2321.GlobalTechCoef_cement_cwf",
			 "L2321.StubTechCoef_cement_cwf",
			 "L2321.IncomeElasticity_cement_cwf",
			 "L2321.SubsectorShrwtFllt_cement_cwf",
			 "L2321.SubsectorInterp_cement_cwf",
			 "L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios",
			 "L2321.SubsectorInterp_cement_cwf_H2_scenarios"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A321.globaltech_coef_cwf_adj <- get_data(all_data, "cwf/A321.globaltech_coef_cwf_adj")
    A321.incelas_cwf <- get_data(all_data, "cwf/A321.incelas_cwf")

    A321.subsector_interp_cwf <- get_data(all_data, "cwf/A321.subsector_interp_cwf", strip_attributes = TRUE)
    A321.subsector_shrwt_cwf <- get_data(all_data, "cwf/A321.subsector_shrwt_cwf", strip_attributes = TRUE)

    A321.subsector_interp_cwf_H2_scenarios <- get_data(all_data, "cwf/A321.subsector_interp_cwf_H2_scenarios", strip_attributes = TRUE)
    A321.subsector_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A321.subsector_shrwt_cwf_H2_scenarios", strip_attributes = TRUE)
    L2321.GlobalTechCoef_cement <- get_data(all_data, "L2321.GlobalTechCoef_cement", strip_attributes = TRUE)
    L2321.StubTechCoef_cement <- get_data(all_data, "L2321.StubTechCoef_cement", strip_attributes = TRUE)
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost <- PrimaryFuelCO2Coef.name <-
      PrimaryFuelCO2Coef <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- L2321.IncomeElasticity_cement_gcam3 <-
      L2321.IncomeElasticity_cement_gssp1 <- L2321.IncomeElasticity_cement_gssp2 <-
      L2321.IncomeElasticity_cement_gssp3 <- L2321.IncomeElasticity_cement_gssp4 <-
      L2321.IncomeElasticity_cement_gssp5 <- L2321.IncomeElasticity_cement_ssp1 <-
      L2321.IncomeElasticity_cement_ssp2 <- L2321.IncomeElasticity_cement_ssp3 <-
      L2321.IncomeElasticity_cement_ssp4 <- L2321.IncomeElasticity_cement_ssp5 <-
      L2321.IncomeElasticity_cement_cwf <- year.x <- year.y <- NULL

    # ===================================================
    # Make CWF adjustments

    # GLOBAL TECH COEFFICIENT: L2321.GlobalTechCoef_cement_cwf
    # get coefficient adjustments
    A321.globaltech_coef_cwf_adj %>%
      gather_years(value_col = "coefficient_adj") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient_adj = approx_fun(year, coefficient_adj, rule = 2)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2321.globaltech_coef_cwf_adj # intermediate tibble

    # apply to the original global tech coefficients
    L2321.GlobalTechCoef_cement %>%
      left_join_error_no_match(L2321.globaltech_coef_cwf_adj) %>%
      mutate(coefficient = round(coefficient * coefficient_adj, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2321.GlobalTechCoef_cement_cwf

    # STUB TECH COEFFICIENT: L2321.StubTechCoef_cement_cwf
    # NOTE: we don't need to adjust stub tech coefficients because these are only historical
    L2321.StubTechCoef_cement_cwf <- L2321.StubTechCoef_cement

    # INCOME ELASTICITY: L2321.IncomeElasticity_cement_cwf
    # read in income elasticity values
    A321.incelas_cwf %>%
      gather_years(value_col = "income.elasticity") %>%
      rename(energy.final.demand = `energy-final-demand`) %>%
      select(LEVEL2_DATA_NAMES[["IncomeElasticity"]]) ->
      L2321.IncomeElasticity_cement_cwf

    # L2321.SubsectorShrwtFllt_cement_cwf
    A321.subsector_shrwt_cwf %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]), GCAM_region_names) ->
      L2321.SubsectorShrwtFllt_cement_cwf

    # L2321.SubsectorInterp_cement_cwf_H2_scenarios
    A321.subsector_interp_cwf %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]]), GCAM_region_names) ->
      L2321.SubsectorInterp_cement_cwf

    # HYDROGEN SCENARIOS, SUBSECTOR SHARE WEIGHTS AND INTERPOLATION
    # L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios
    A321.subsector_shrwt_cwf_H2_scenarios %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], "scenario"), GCAM_region_names) ->
      L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios

    # L2321.SubsectorInterp_cement_cwf_H2_scenarios
    A321.subsector_interp_cwf_H2_scenarios %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]], "scenario"), GCAM_region_names) ->
      L2321.SubsectorInterp_cement_cwf_H2_scenarios

    # ===================================================
    # Produce outputs

    L2321.GlobalTechCoef_cement_cwf %>%
      add_title("Energy inputs and coefficients of cement technologies") %>%
      add_units("limestone input is unitless (Mt limestone per Mt cement); all others are GJ per kg (EJ of energy per Mt of cement)") %>%
      add_comments("For cement sector, the energy use coefficients from A321.globaltech_coef are interpolated into all model years, with CWF adjustments") %>%
      add_legacy_name("L2321.GlobalTechCoef_cement_cwf") %>%
      add_precursors("L2321.GlobalTechCoef_cement", "cwf/A321.globaltech_coef_cwf_adj") ->
      L2321.GlobalTechCoef_cement_cwf

    L2321.StubTechCoef_cement_cwf %>%
      add_title("region-specific coefficients of cement production technologies") %>%
      add_units("limestone input is unitless (Mt limestone per Mt cement); all others are GJ per kg (EJ of energy per Mt of cement)") %>%
      add_comments("Coefficients are calculated using L1321.IO_GJkg_R_cement_F_Yh") %>%
      add_legacy_name("L2321.StubTechCoef_cement_cwf") %>%
      add_precursors("L2321.StubTechCoef_cement") ->
      L2321.StubTechCoef_cement_cwf

    L2321.IncomeElasticity_cement_cwf %>%
      add_title(paste("Income elasticity of cement -", "cwf")) %>%
      add_units("Unitless") %>%
      add_comments("Read in as assumptions") %>%
      add_precursors("cwf/A321.incelas_cwf") ->
      L2321.IncomeElasticity_cement_cwf

    L2321.SubsectorShrwtFllt_cement_cwf %>%
      add_title("Subsector shareweights of cement sector of cwf") %>%
      add_units("unitless") %>%
      add_comments("For cement sector, the subsector shareweights from A321.subsector_shrwt_cwf are expanded into all GCAM regions") %>%
      add_legacy_name("L2321.SubsectorShrwtFllt_cement_cwf") %>%
      add_precursors("cwf/A321.subsector_shrwt_cwf", "common/GCAM_region_names") ->
      L2321.SubsectorShrwtFllt_cement_cwf

    L2321.SubsectorInterp_cement_cwf %>%
      add_title("Subsector shareweight interpolation of cement sector of cwf") %>%
      add_units("NA") %>%
      add_comments("For cement sector, the subsector shareweight interpolation function infromation from A321.subsector_interp_cwf is expanded into all GCAM regions") %>%
      add_legacy_name("L2321.SubsectorInterp_cement_cwf") %>%
      add_precursors("cwf/A321.subsector_interp_cwf", "common/GCAM_region_names") ->
      L2321.SubsectorInterp_cement_cwf

    L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios %>%
      add_title("Subsector shareweights of cement sector") %>%
      add_units("unitless") %>%
      add_comments("For cement sector, the subsector shareweights from A321.subsector_shrwt_cwf_H2_scenarios are expanded into all GCAM regions") %>%
      add_legacy_name("L2321.SubsectorShrwtFllt_cement") %>%
      add_precursors("cwf/A321.subsector_shrwt_cwf_H2_scenarios", "common/GCAM_region_names") ->
      L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios

    L2321.SubsectorInterp_cement_cwf_H2_scenarios %>%
      add_title("Subsector shareweight interpolation of cement sector") %>%
      add_units("NA") %>%
      add_comments("For cement sector, the subsector shareweight interpolation function infromation from A321.subsector_interp_cwf_H2_scenarios is expanded into all GCAM regions") %>%
      add_legacy_name("L2321.SubsectorInterp_cement") %>%
      add_precursors("cwf/A321.subsector_interp_cwf_H2_scenarios", "common/GCAM_region_names") ->
      L2321.SubsectorInterp_cement_cwf_H2_scenarios

    return_data(L2321.GlobalTechCoef_cement_cwf, L2321.StubTechCoef_cement_cwf, L2321.IncomeElasticity_cement_cwf,
                L2321.SubsectorShrwtFllt_cement_cwf, L2321.SubsectorInterp_cement_cwf,
                L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios, L2321.SubsectorInterp_cement_cwf_H2_scenarios)
  } else {
    stop("Unknown command")
  }
}
