# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2325.chemical_cwf
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for chemical-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2325.SectorLogitTables[[ curr_table ]]$data}, \code{L2325.Supplysector_chemical}, \code{L2325.FinalEnergyKeyword_chemical},
#' \code{L2325.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2325.SubsectorLogit_chemical}, \code{L2325.SubsectorShrwtFllt_chemical},
#' \code{L2325.SubsectorInterp_chemical}, \code{L2325.StubTech_chemical}, \code{L2325.GlobalTechShrwt_chemical}, \code{L2325.GlobalTechCoef_chemical},
#' \code{L2325.GlobalTechCost_chemical}, \code{L2325.GlobalTechCapture_chemical}, \code{L2325.StubTechProd_chemical}, \code{L2325.StubTechCalInput_chemical},
#' \code{L2325.StubTechCoef_chemical}, \code{L2325.PerCapitaBased_chemical}, \code{L2325.BaseService_chemical}, \code{L2325.PriceElasticity_chemical},
#' \code{L2325.GlobalTechCapture_chemical}, \code{L2325.GlobalTechEff_chemical},\code{L2325.GlobalTechCSeq_ind},
#' \code{L2325.GlobalTechEff_chemical_cwf}, \code{L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios}, \code{L2325.SubsectorInterp_chemical_cwf_H2_scenarios},
#' \code{L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios}, \code{object}. The corresponding file in the
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for chemical sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Dec 2019
module_energy_L2325.chemical_cwf <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A325.globaltech_eff",
             FILE = "cwf/A325.globaltech_eff_cwf_adj",
             FILE = "cwf/A325.subsector_interp_cwf",
             FILE = "cwf/A325.subsector_shrwt_cwf",
             FILE = "cwf/A325.globaltech_shrwt_cwf",
             FILE = "cwf/A325.subsector_interp_cwf_H2_scenarios",
             FILE = "cwf/A325.subsector_shrwt_cwf_H2_scenarios",
             FILE = "cwf/A325.globaltech_shrwt_cwf_H2_scenarios",
             "L1325.in_EJ_R_chemical_F_Y",
             "L2325.GlobalTechEff_chemical"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(
      "L2325.GlobalTechEff_chemical_cwf",
      "L2325.SubsectorShrwtFllt_chemical_cwf",
      "L2325.SubsectorInterp_chemical_cwf",
      "L2325.GlobalTechShrwt_chemical_cwf",
      "L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios",
      "L2325.SubsectorInterp_chemical_cwf_H2_scenarios",
      "L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios"
			       ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A325.globaltech_eff <- get_data(all_data, "energy/A325.globaltech_eff", strip_attributes = TRUE)
    A325.globaltech_eff_cwf_adj <- get_data(all_data, "cwf/A325.globaltech_eff_cwf_adj", strip_attributes = TRUE)
    A325.subsector_interp_cwf <- get_data(all_data, "cwf/A325.subsector_interp_cwf", strip_attributes = TRUE)
    A325.subsector_shrwt_cwf <- get_data(all_data, "cwf/A325.subsector_shrwt_cwf", strip_attributes = TRUE)
    A325.globaltech_shrwt_cwf <- get_data(all_data, "cwf/A325.globaltech_shrwt_cwf", strip_attributes = TRUE)
    A325.subsector_interp_cwf_H2_scenarios <- get_data(all_data, "cwf/A325.subsector_interp_cwf_H2_scenarios", strip_attributes = TRUE)
    A325.subsector_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A325.subsector_shrwt_cwf_H2_scenarios", strip_attributes = TRUE)
    A325.globaltech_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A325.globaltech_shrwt_cwf_H2_scenarios", strip_attributes = TRUE)
    L1325.in_EJ_R_chemical_F_Y <- get_data(all_data, "L1325.in_EJ_R_chemical_F_Y")
    L2325.GlobalTechEff_chemical <- get_data(all_data, "L2325.GlobalTechEff_chemical", strip_attributes = TRUE)

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- secondary.output <- efficiency <- elec_ratio <- output.ratio <-
      year.x <- year.y <- output.ratio.x <- output.ratio.y <- sector.name <- subsector.name <-
      calOutputValue.x <- calOutputValue.y <- output_tot <- stub.technology <- market.name <- terminal_coef <-
      share.weight <- interpolation.function <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(grepl("chemical energy", sector) & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2325.rm_heat_techs_R # intermediate tibble

    L1325.in_EJ_R_chemical_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      complete(nesting(fuel,year,sector),region = GCAM_region_names$region) %>%
      mutate(GCAM_region_ID = NULL,value = replace_na(value,0)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L2325.in_EJ_R_chemical_F_Yh # intermediate tibble

    A325.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 1),
             efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2325.globaltech_eff.long # intermediate tibble

    # L2325.StubTechProd_chemical: calibrated output of chemical sector
    # First, calculate service output by technology, for energy-use and feedstocks
    L2325.in_EJ_R_chemical_F_Yh %>%
      left_join_error_no_match(select(L2325.globaltech_eff.long, sector.name, subsector.name, technology, year,efficiency),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
      L2325.out_EJ_R_ind_serv_F_Yh # intermediate tibble

    #For regions with 0 production in base year, modify Subsector shareweight interpolation (from fixed to linear)
    L2325.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region,supplysector, GCAM_region_ID, year) %>%
      summarise(value = sum(calOutputValue)) %>%
      ungroup %>%
      select(region, year, supplysector,value) %>%
      filter(value == 0, year == MODEL_FINAL_BASE_YEAR)  ->
      nobaseyear

    # ===================================================
    # Make CWF adjustments

    # EFFICIENCY: L2325.GlobalTechEff_chemical_cwf
    # get efficiency adjustments
    A325.globaltech_eff_cwf_adj %>%
      gather_years(value_col = "efficiency_adj") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(efficiency_adj = approx_fun(year, efficiency_adj, rule = 2)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      dplyr::select(-secondary.output) ->
      L2325.globaltech_eff_cwf_adj # intermediate tibble

    # apply to the original global tech efficiencies
    L2325.GlobalTechEff_chemical %>%
      left_join_error_no_match(L2325.globaltech_eff_cwf_adj) %>%
      mutate(efficiency = round(efficiency * efficiency_adj, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2325.GlobalTechEff_chemical_cwf

    A325.subsector_shrwt_cwf %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]), GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorShrwtFllt_chemical_cwf

    #For regions with 0 in base year, modify Subsector shareweight and interpolation (for non-hydrogen subsectors)
    L2325.SubsectorShrwtFllt_chemical_cwf %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      # only adjust for non-hydrogen subsectors
      mutate(value = replace_na(value,1),share.weight = if_else(value ==0 & subsector != "hydrogen",0.5,share.weight),year = NULL,value = NULL) ->
      L2325.SubsectorShrwtFllt_chemical_cwf

    A325.subsector_interp_cwf %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]]), GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorInterp_chemical_cwf

    L2325.SubsectorInterp_chemical_cwf %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      # only adjust for non-hydrogen subsectors
      mutate(value = replace_na(value,1),interpolation.function = if_else(value ==0 & subsector != "hydrogen","linear",interpolation.function),year = NULL,value = NULL) ->
      L2325.SubsectorInterp_chemical_cwf

    A325.globaltech_shrwt_cwf %>%
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
      L2325.GlobalTechShrwt_chemical_cwf

    # HYDROGEN SCENARIOS, SUBSECTOR SHARE WEIGHTS AND INTERPOLATION, GLOBAL TECH SHARE WEIGHTS
    # L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios: Subsector shareweights of chemical sector
    A325.subsector_shrwt_cwf_H2_scenarios %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], "scenario"), GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios

    #For regions with 0 in base year, modify Subsector shareweight and interpolation (for non-hydrogen subsectors)
    L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      # only adjust for non-hydrogen subsectors
      mutate(value = replace_na(value,1),share.weight = if_else(value ==0 & subsector != "hydrogen",0.5,share.weight),year = NULL,value = NULL) ->
      L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios

    # L2325.SubsectorInterp_chemical_cwf_H2_scenarios: Subsector shareweight interpolation of chemical sector
    A325.subsector_interp_cwf_H2_scenarios %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]], "scenario"), GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorInterp_chemical_cwf_H2_scenarios

    L2325.SubsectorInterp_chemical_cwf_H2_scenarios %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      # only adjust for non-hydrogen subsectors
      mutate(value = replace_na(value,1),interpolation.function = if_else(value ==0 & subsector != "hydrogen","linear",interpolation.function),year = NULL,value = NULL) ->
      L2325.SubsectorInterp_chemical_cwf_H2_scenarios

    # L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios: Shareweights of global chemical technologies
    A325.globaltech_shrwt_cwf_H2_scenarios %>%
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
      L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios


    # ===================================================
    # Produce outputs

    L2325.GlobalTechEff_chemical_cwf %>%
      add_title("Energy inputs and efficiency of global chemical energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the efficiency values from A325.globaltech_eff are interpolated into all base years and future years, with CWF adjustments") %>%
      add_legacy_name("L2325.GlobalTechEff_chemical") %>%
      add_precursors("energy/A325.globaltech_eff", "cwf/A325.globaltech_eff_cwf_adj") ->
      L2325.GlobalTechEff_chemical_cwf

    L2325.SubsectorShrwtFllt_chemical_cwf %>%
      add_title("Subsector shareweights of chemical sector") %>%
      add_units("unitless") %>%
      add_comments("For chemical sector, the subsector shareweights from A325.subsector_shrwt_cwf_H2_scenarios are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorShrwtFllt_chemical_cwf") %>%
      add_precursors("cwf/A325.subsector_shrwt_cwf", "energy/A_regions","common/GCAM_region_names") ->
      L2325.SubsectorShrwtFllt_chemical_cwf

    L2325.SubsectorInterp_chemical_cwf %>%
      add_title("Subsector shareweight interpolation of chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the subsector shareweight interpolation function infromation from A325.subsector_interp_cwf_H2_scenarios is expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorInterp_chemical_cwf") %>%
      add_precursors("cwf/A325.subsector_interp_cwf", "energy/A_regions", "common/GCAM_region_names") ->
      L2325.SubsectorInterp_chemical_cwf

    L2325.GlobalTechShrwt_chemical_cwf %>%
      add_title("Shareweights of global chemical technologies") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the share weights from A325.globaltech_shrwt_cwf are interpolated into all base years and future years") %>%
      add_legacy_name("L2325.GlobalTechShrwt_chemical_cwf") %>%
      add_precursors("cwf/A325.globaltech_shrwt_cwf") ->
      L2325.GlobalTechShrwt_chemical_cwf

    L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios %>%
      add_title("Subsector shareweights of chemical sector") %>%
      add_units("unitless") %>%
      add_comments("For chemical sector, the subsector shareweights from A325.subsector_shrwt_cwf_H2_scenarios are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorShrwtFllt_chemical") %>%
      add_precursors("cwf/A325.subsector_shrwt_cwf_H2_scenarios", "energy/A_regions","common/GCAM_region_names") ->
      L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios

    L2325.SubsectorInterp_chemical_cwf_H2_scenarios %>%
      add_title("Subsector shareweight interpolation of chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the subsector shareweight interpolation function infromation from A325.subsector_interp_cwf_H2_scenarios is expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorInterp_chemical") %>%
      add_precursors("cwf/A325.subsector_interp_cwf_H2_scenarios", "energy/A_regions", "common/GCAM_region_names") ->
      L2325.SubsectorInterp_chemical_cwf_H2_scenarios

    L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios %>%
      add_title("Shareweights of global chemical technologies") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the share weights from A325.globaltech_shrwt_cwf_H2_scenarios are interpolated into all base years and future years") %>%
      add_legacy_name("L2325.GlobalTechShrwt_chemical") %>%
      add_precursors("cwf/A325.globaltech_shrwt_cwf_H2_scenarios") ->
      L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios

      return_data(L2325.GlobalTechEff_chemical_cwf,
                  L2325.SubsectorShrwtFllt_chemical_cwf,
                  L2325.SubsectorInterp_chemical_cwf,
                  L2325.GlobalTechShrwt_chemical_cwf,
                  L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios,
                  L2325.SubsectorInterp_chemical_cwf_H2_scenarios,
                  L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios
                  )

  } else {
    stop("Unknown command")
  }
}
