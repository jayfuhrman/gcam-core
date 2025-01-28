# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L232.other_industry_cwf
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for industry-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.SectorLogitTables[[ curr_table ]]$data},
#' \code{L232.Supplysector_ind}, \code{L232.SubsectorLogitTables[[ curr_table ]]$data},
#' \code{L232.SubsectorLogit_ind}, \code{L232.FinalEnergyKeyword_ind},
#' \code{L232.SubsectorShrwtFllt_ind}, \code{L232.SubsectorInterp_ind},
#' \code{L232.StubTech_ind}, \code{L232.GlobalTechShrwt_ind}, \code{L232.StubTechInterp_ind},
#' \code{L232.GlobalTechEff_ind}, \code{L232.GlobalTechCoef_ind},
#' \code{L232.GlobalTechEff_ind_cwf},
#' \code{L232.GlobalTechCost_ind},
#' \code{L232.GlobalTechSecOut_ind}, \code{L232.GlobalTechCSeq_ind},
#' \code{L232.StubTechCalInput_indenergy}, \code{L232.StubTechCalInput_indfeed},
#' \code{L232.StubTechProd_industry}, \code{L232.StubTechCoef_industry},
#' \code{L232.FuelPrefElast_indenergy}, \code{L232.PerCapitaBased_ind},
#' \code{L232.PriceElasticity_ind}, \code{L232.BaseService_ind},
#' \code{L232.IncomeElasticity_ind_gcam3}, \code{L232.IncomeElasticity_ind_gssp1},
#' \code{L232.IncomeElasticity_ind_gssp2}, \code{L232.IncomeElasticity_ind_gssp3},
#' \code{L232.IncomeElasticity_ind_gssp4}, \code{L232.IncomeElasticity_ind_gssp5},
#' \code{L232.IncomeElasticity_ind_ssp1}, \code{L232.IncomeElasticity_ind_ssp2},
#' \code{L232.IncomeElasticity_ind_ssp3}, \code{L232.IncomeElasticity_ind_ssp4},
#' \code{L232.IncomeElasticity_ind_ssp5},\code{L232.IncomeElasticity_ind_cwf},
#' \code{L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios}, \code{L232.SubsectorInterp_ind_cwf_H2_scenarios},
#' \code{object}. The corresponding file in the
#' original data system was \code{L232.industry.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, supplysector/subsector share weights, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows distinct filter if_else group_by lag left_join mutate right_join select summarise
#' @importFrom tidyr complete nesting
#' @author LF October 2017
module_energy_L232.other_industry_cwf <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "cwf/A32.globaltech_eff_cwf_adj",
             FILE = "cwf/A32.incelas_cwf",
             FILE = "cwf/A32.subsector_interp_cwf",
             FILE = "cwf/A32.subsector_shrwt_cwf",
             FILE = "cwf/A32.subsector_interp_cwf_H2_scenarios",
             FILE = "cwf/A32.subsector_shrwt_cwf_H2_scenarios",
             "L232.GlobalTechEff_ind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.SubsectorShrwtFllt_ind_cwf",
             "L232.SubsectorInterp_ind_cwf",
             "L232.GlobalTechEff_ind_cwf",
             "L232.IncomeElasticity_ind_cwf",
             "L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios",
             "L232.SubsectorInterp_ind_cwf_H2_scenarios"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "energy/A_regions")
    A32.subsector_interp_cwf <- get_data(all_data, "cwf/A32.subsector_interp_cwf", strip_attributes = TRUE)
    A32.subsector_shrwt_cwf <- get_data(all_data, "cwf/A32.subsector_shrwt_cwf", strip_attributes = TRUE)
    A32.globaltech_eff_cwf_adj <- get_data(all_data, "cwf/A32.globaltech_eff_cwf_adj")
    A32.incelas_cwf <- get_data(all_data, "cwf/A32.incelas_cwf")
    A32.subsector_interp_cwf_H2_scenarios <- get_data(all_data, "cwf/A32.subsector_interp_cwf_H2_scenarios", strip_attributes = TRUE)
    A32.subsector_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A32.subsector_shrwt_cwf_H2_scenarios", strip_attributes = TRUE)
    L232.GlobalTechEff_ind <- get_data(all_data, "L232.GlobalTechEff_ind", strip_attributes = TRUE)
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- sector <- fuel <- supplysector <- subsector <-
      technology <- year.fillout <- to.value <- year <- share.weight <-
      efficiency <- minicam.energy.input <- secondary.output <- coefficient <-
      elec_ratio <- output.ratio <- . <- year.x <- output.ratio.x <- output.ratio.y <-
      input.cost <- minicam.non.energy.input <- GCAM_region_ID <- value <-
      calibrated.value <- sector.name <- subsector.name <- region <-
      calOutputValue <- subs.share.weight <- calOutputValue.x <- calOutputValue.y <-
      output_tot <- value.x <- value.y <- total <- fuelprefElasticity <-
      terminal_coef <- criteria <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      parameter <- income.elasticity <- L232.IncomeElasticity_ind_gcam3 <-
      L232.IncomeElasticity_ind_gssp1 <- L232.IncomeElasticity_ind_gssp2 <-
      L232.IncomeElasticity_ind_gssp3 <- L232.IncomeElasticity_ind_gssp4 <-
      L232.IncomeElasticity_ind_gssp5 <- L232.IncomeElasticity_ind_ssp1 <-
      L232.IncomeElasticity_ind_ssp2 <- L232.IncomeElasticity_ind_ssp3 <-
      L232.IncomeElasticity_ind_ssp4 <- L232.IncomeElasticity_ind_ssp5 <-
      market.name <- stub.technology <- year.y <- NULL

    # ===================================================

    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(grepl("industry", sector) & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L232.rm_heat_techs_R # intermediate tibble

    A32.subsector_shrwt_cwf %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorShrwtFllt_ind_cwf

    A32.subsector_interp_cwf %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorInterp_ind_cwf


    # ===================================================
    # Make CWF adjustments

    # EFFICIENCY: L232.GlobalTechEff_ind_cwf
    # get efficiency adjustments
    A32.globaltech_eff_cwf_adj %>%
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
      L232.globaltech_eff_cwf_adj # intermediate tibble

    # apply to the original global tech efficiencies
    L232.GlobalTechEff_ind %>%
      left_join_error_no_match(L232.globaltech_eff_cwf_adj) %>%
      mutate(efficiency = round(efficiency * efficiency_adj, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L232.GlobalTechEff_ind_cwf

    # note we don't need to adjust L232.GlobalTechSecOut_ind since it carries over historical data into future
    # periods, so this should match for CWF. similarly, don't need to adjust L232.StubTechCalInput_indenergy,
    # L232.StubTechCalInput_indfeed, or L232.StubTechProd_industry since these are historical values only

    # INCOME ELASTICITY: L232.IncomeElasticity_ind_cwf
    # read in income elasticity values
    A32.incelas_cwf %>%
      gather_years(value_col = "income.elasticity") %>%
      rename(energy.final.demand = `energy-final-demand`) %>%
      select(LEVEL2_DATA_NAMES[["IncomeElasticity"]]) ->
    L232.IncomeElasticity_ind_cwf

    # HYDROGEN SCENARIOS, SUBSECTOR SHARE WEIGHTS AND INTERPOLATION
    # L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios: Subsector shareweights of industry sector for CWF hydrogen scenarios
    A32.subsector_shrwt_cwf_H2_scenarios %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], "scenario"), GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios

    # L232.SubsectorInterp_ind_cwf_H2_scenarios: Subsector shareweight interpolation of industry sector for CWF hydrogen scenarios
    A32.subsector_interp_cwf_H2_scenarios %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]], "scenario"), GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorInterp_ind_cwf_H2_scenarios


    # ===================================================
    # Produce outputs


    L232.IncomeElasticity_ind_cwf %>%
      add_title(paste("Income elasticity of industry -", "cwf")) %>%
      add_units("Unitless") %>%
      add_comments("Read in as assumptions") %>%
      add_precursors("cwf/A32.incelas_cwf") ->
      L232.IncomeElasticity_ind_cwf

    L232.SubsectorShrwtFllt_ind_cwf %>%
      add_title("Subsector shareweights of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector shareweights from A32.subsector_shrwt are expanded into all GCAM regions with non-existent heat technologies") %>%
      add_legacy_name("L232.SubsectorShrwtFllt_ind_cwf") %>%
      add_precursors("cwf/A32.subsector_shrwt_cwf", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorShrwtFllt_ind_cwf

    L232.SubsectorInterp_ind_cwf %>%
      add_title("Subsector shareweight interpolation of industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the subsector shareweight interpolation function infromation from A32.subsector_interp is expanded into all GCAM regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.SubsectorInterp_ind_cwf") %>%
      add_precursors("cwf/A32.subsector_interp_cwf", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorInterp_ind_cwf

    L232.GlobalTechEff_ind_cwf %>%
      add_title("Energy inputs and efficiency of global industrial energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the efficiency values from A32.globaltech_eff are interpolated into all base years and future years") %>%
      add_precursors("L232.GlobalTechEff_ind", "cwf/A32.globaltech_eff_cwf_adj") ->
      L232.GlobalTechEff_ind_cwf


    L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios %>%
      add_title("Subsector shareweights of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector shareweights from A32.subsector_shrwt_cwf_H2_scenarios are expanded into all GCAM regions with non-existent heat technologies") %>%
      add_legacy_name("L232.SubsectorShrwtFllt_ind") %>%
      add_precursors("cwf/A32.subsector_shrwt_cwf_H2_scenarios", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios

    L232.SubsectorInterp_ind_cwf_H2_scenarios %>%
      add_title("Subsector shareweight interpolation of industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the subsector shareweight interpolation function infromation from A32.subsector_interp_cwf_H2_scenarios is expanded into all GCAM regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.SubsectorInterp_ind") %>%
      add_precursors("cwf/A32.subsector_interp_cwf_H2_scenarios", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorInterp_ind_cwf_H2_scenarios

    return_data(L232.SubsectorInterp_ind_cwf,
                L232.SubsectorShrwtFllt_ind_cwf,
                L232.GlobalTechEff_ind_cwf,
                L232.IncomeElasticity_ind_cwf,
                L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios,
                L232.SubsectorInterp_ind_cwf_H2_scenarios)
  } else {
    stop("Unknown command")
  }
}
