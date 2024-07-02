# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2324.Off_road
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for Off_road-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2324.SectorLogitTables[[ curr_table ]]$data}, \code{L2324.Supplysector_Off_road}, \code{L2324.FinalEnergyKeyword_Off_road},
#' \code{L2324.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2324.SubsectorLogit_Off_road}, \code{L2324.SubsectorShrwtFllt_Off_road},
#' \code{L2324.SubsectorInterp_Off_road}, \code{L2324.StubTech_Off_road}, \code{L2324.GlobalTechShrwt_Off_road}, \code{L2324.GlobalTechCoef_Off_road},
#' \code{L2324.GlobalTechCost_Off_road}, \code{L2324.GlobalTechCapture_Off_road}, \code{L2324.StubTechProd_Off_road}, \code{L2324.StubTechCalInput_Off_road},
#' \code{L2324.StubTechCoef_Off_road}, \code{L2324.PerCapitaBased_Off_road}, \code{L2324.BaseService_Off_road}, \code{L2324.PriceElasticity_Off_road},\code{L2324.GlobalTechCSeq_ind},
#' \code{L2324.GlobalTechEff_Off_road_cwf}, \code{L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios}, \code{L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios},
#' \code{object}. The corresponding file in the
#' original data system was \code{L2324.Off_road.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for Off_road sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019, Molly Charles 2020-21, 2022 modifications from Jay Fuhrman, Siddarth Durga, Page Kyle
module_energy_L2324.Off_road_cwf <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "cwf/A324.globaltech_shrwt_cwf",
             FILE = "cwf/A324.globaltech_interp_cwf",
             FILE = "cwf/A324.globaltech_eff_cwf_adj",
             FILE = "cwf/A324.globaltech_shrwt_cwf_H2_scenarios",
             FILE = "cwf/A324.globaltech_interp_cwf_H2_scenarios",
             "L2324.GlobalTechEff_Off_road"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2324.GlobalTechShrwt_Off_road_cwf",
             "L2324.GlobalTechInterp_Off_road_cwf",
			 "L2324.GlobalTechEff_Off_road_cwf",
			 "L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios",
			 "L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A324.globaltech_shrwt_cwf <- get_data(all_data, "cwf/A324.globaltech_shrwt_cwf", strip_attributes = TRUE)
    A324.globaltech_interp_cwf <- get_data(all_data, "cwf/A324.globaltech_interp_cwf", strip_attributes = TRUE)
    A324.globaltech_eff_cwf_adj <- get_data(all_data, "cwf/A324.globaltech_eff_cwf_adj", strip_attributes = TRUE)
    A324.globaltech_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A324.globaltech_shrwt_cwf_H2_scenarios", strip_attributes = TRUE)
    A324.globaltech_interp_cwf_H2_scenarios <- get_data(all_data, "cwf/A324.globaltech_interp_cwf_H2_scenarios", strip_attributes = TRUE)
    L2324.GlobalTechEff_Off_road <- get_data(all_data, "L2324.GlobalTechEff_Off_road", strip_attributes = TRUE)
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- to.year <- from.year <- efficiency <- year.x <- year.y <-
      sector.name <- subsector.name <- stub.technology <- calOutputValue.x <- calOutputValue.y <- output_tot <-
      market.name <- terminal_coef <- share.weight <- interpolation.function <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel

    A324.globaltech_shrwt_cwf %>%
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
      L2324.GlobalTechShrwt_Off_road_cwf


    L2324.GlobalTechInterp_Off_road_cwf <- A324.globaltech_interp_cwf %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector)

    # GLOBAL TECH EFFICIENCY: L2324.GlobalTechEff_Off_road_cwf
    # get efficiency adjustments
    A324.globaltech_eff_cwf_adj %>%
      gather_years(value_col = "efficiency_adj") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input,  year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(efficiency_adj = approx_fun(year, efficiency_adj, rule = 2)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2324.globaltech_eff_cwf_adj # intermediate tibble

    # apply to the original global tech efficiencies
    L2324.GlobalTechEff_Off_road %>%
      left_join_error_no_match(L2324.globaltech_eff_cwf_adj, by = c('sector.name','subsector.name','technology','minicam.energy.input','year')) %>%
      mutate(efficiency = round(efficiency * efficiency_adj, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2324.GlobalTechEff_Off_road_cwf

    # HYDROGEN SCENARIOS, global tech share weights and interpolation rules
    # L2324.GlobalTechShrwt_Off_road: Shareweights of global Off_road technologies for CWF hydrogen scenarios
    A324.globaltech_shrwt_cwf_H2_scenarios %>%
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
      L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios

    L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios <- A324.globaltech_interp_cwf_H2_scenarios %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector)


    # ===================================================
    # Produce outputs

    L2324.GlobalTechInterp_Off_road_cwf %>%
      add_title("Technology shareweight interpolation of Off_road sector") %>%
      add_units("NA") %>%
      add_comments("Rules from global technology database are applied to all regions") %>%
      add_precursors("cwf/A324.globaltech_interp_cwf") ->
      L2324.GlobalTechInterp_Off_road_cwf

    L2324.GlobalTechShrwt_Off_road_cwf %>%
      add_title("Shareweights of global Off_road technologies") %>%
      add_units("Unitless") %>%
      add_comments("For Off_road sector, the share weights from A324.globaltech_shrwt_cwf are interpolated into all base years and future years") %>%
      add_legacy_name("L2324.GlobalTechShrwt_Off_road_cwf") %>%
      add_precursors("energy/A324.globaltech_shrwt_cwf") ->
      L2324.GlobalTechShrwt_Off_road_cwf


    L2324.GlobalTechEff_Off_road_cwf %>%
      add_title("Energy inputs and efficiency of global Off_road energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("For Off_road sector, the efficiency values from A324.globaltech_eff are interpolated into all base years and future years, with CWF adjustments") %>%
      add_legacy_name("L2324.GlobalTechEff_Off_road") %>%
      add_precursors("energy/A324.globaltech_eff", "cwf/A324.globaltech_eff_cwf_adj") ->
      L2324.GlobalTechEff_Off_road_cwf

    L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios %>%
      add_title("Shareweights of global Off_road technologies") %>%
      add_units("Unitless") %>%
      add_comments("For Off_road sector, the share weights from A324.globaltech_shrwt_cwf_H2_scenarios are interpolated into all base years and future years") %>%
      add_legacy_name("L2324.GlobalTechShrwt_Off_road") %>%
      add_precursors("cwf/A324.globaltech_shrwt_cwf_H2_scenarios") ->
      L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios

    L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios %>%
      add_title("Technology shareweight interpolation of Off_road sector") %>%
      add_units("NA") %>%
      add_comments("Rules from global technology database are applied to all regions") %>%
      add_precursors("cwf/A324.globaltech_interp_cwf_H2_scenarios") ->
      L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios

      return_data(L2324.GlobalTechInterp_Off_road_cwf,
                  L2324.GlobalTechEff_Off_road_cwf,
                  L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios,
                  L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios,
                  L2324.GlobalTechShrwt_Off_road_cwf)

  } else {
    stop("Unknown command")
  }
}
