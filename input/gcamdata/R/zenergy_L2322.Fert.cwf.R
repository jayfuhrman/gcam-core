# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2322.Fert_cwf
#'
#' Provide supply sector information/keywords, subsector shareweights, global technology lifetime,
#' energy inputs and coefficients, global fertilizer manufacturing technologies, etc. for the fertilizer sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:  \code{L2322.Supplysector_Fert}, \code{L2322.FinalEnergyKeyword_Fert}, \code{L2322.SubsectorLogit_Fert},
#' \code{L2322.SubsectorShrwtFllt_Fert}, \code{L2322.SubsectorInterp_Fert}, \code{L2322.StubTech_Fert}, \code{L2322.GlobalTechShrwt_Fert},
#' \code{L2322.GlobalTechCoef_Fert}, \code{L2322.GlobalTechCost_Fert}, \code{L2322.GlobalTechCapture_Fert}, \code{L2322.GlobalTechSCurve_Fert},
#' \code{L2322.GlobalTechProfitShutdown_Fert}, \code{L2322.StubTechProd_FertProd}, \code{L2322.StubTechCoef_Fert}. The corresponding file in the
#' original data system was \code{L2322.Fert.R} (energy level2).
#' @details This chunk provides supply sector information/keywords, subsector shareweights, global technology lifetime,
#' energy inputs and coefficients, global fertilizer manufacturing technologies, etc. for the fertilizer sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author LF September 2017
module_energy_L2322.Fert_cwf <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "cwf/A322.subsector_interp_cwf_adj",
             FILE = "cwf/A322.subsector_shrwt_cwf_adj",
             FILE = "cwf/A322.subsector_interp_cwf_H2_scenarios",
             FILE = "cwf/A322.subsector_shrwt_cwf_H2_scenarios",
             FILE = "cwf/A322.globaltech_shrwt_cwf",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.SubsectorShrwtFllt_Fert_cwf",
             "L2322.SubsectorInterp_Fert_cwf",
             "L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios",
             "L2322.SubsectorInterp_Fert_cwf_H2_scenarios",
             "L2322.GlobalTechShrwt_Fert_cwf"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A322.subsector_interp_cwf_adj <- get_data(all_data, "cwf/A322.subsector_interp_cwf_adj", strip_attributes = TRUE)
    A322.subsector_shrwt_cwf_adj <- get_data(all_data, "cwf/A322.subsector_shrwt_cwf_adj", strip_attributes = TRUE)
    A322.subsector_interp_cwf_H2_scenarios <- get_data(all_data, "cwf/A322.subsector_interp_cwf_H2_scenarios", strip_attributes = TRUE)
    A322.subsector_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A322.subsector_shrwt_cwf_H2_scenarios", strip_attributes = TRUE)
    A322.globaltech_shrwt_cwf <- get_data(all_data, "cwf/A322.globaltech_shrwt_cwf", strip_attributes = TRUE)
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert", strip_attributes = TRUE)
    L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert", strip_attributes = TRUE)
    # ===================================================
    # 0. Give binding for variable names used in pipeline

    year.fillout <- to.value <- technology <- year <-
      share.weight <- supplysector <- subsector <- coefficient <- minicam.energy.input <-
      NEcost_75USDkgNH3 <- input.cost <- remove.fraction <- half.life <- median.shutdown.point <-
      value <- calOutputValue <- sector <- fuel <- subs.share.weight <- region <- fixedOutput <- . <- NULL

    # ===================================================
    # CWF adjustments

    A322.globaltech_shrwt_cwf %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L2322.GlobalTechShrwt_Fert_cwf

    # L2322.SubsectorShrwtFllt_Fert_cwf
    L2322.SubsectorShrwtFllt_Fert_cwf <- L2322.SubsectorShrwtFllt_Fert %>%
      # keep only default values that we don't want to overwrite with CWF ones
      filter(! subsector %in% unique(A322.subsector_shrwt_cwf_adj$subsector)) %>%
      # add CWF values
      bind_rows(A322.subsector_shrwt_cwf_adj %>%
                  write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],
                                       GCAM_region_names))

    # L2322.SubsectorInterp_Fert_cwf
    L2322.SubsectorInterp_Fert_cwf <- L2322.SubsectorInterp_Fert %>%
      # keep only default values that we don't want to overwrite with CWF ones
      filter(! subsector %in% unique(A322.subsector_interp_cwf_adj$subsector)) %>%
      # add CWF values
      bind_rows(A322.subsector_interp_cwf_adj %>%
                  write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]],
                                       GCAM_region_names))

    # L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios
    L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios <- A322.subsector_shrwt_cwf_H2_scenarios %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], "scenario"), GCAM_region_names)

    # L2322.SubsectorInterp_Fert_cwf_H2_scenarios
    L2322.SubsectorInterp_Fert_cwf_H2_scenarios <- A322.subsector_interp_cwf_H2_scenarios %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]], "scenario"), GCAM_region_names)

    # ===================================================
    # Produce outputs

    L2322.SubsectorShrwtFllt_Fert_cwf %>%
      add_title("Subsector shareweights of fertilizer") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the subsector shareweights from A322.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert_cwf") %>%
      add_precursors("cwf/A322.subsector_shrwt_cwf_adj", "L2322.SubsectorShrwtFllt_Fert", "common/GCAM_region_names") ->
      L2322.SubsectorShrwtFllt_Fert_cwf

    L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios %>%
      add_title("Subsector shareweights of fertilizer") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the subsector shareweights from A322.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios") %>%
      add_precursors("cwf/A322.subsector_shrwt_cwf_H2_scenarios", "common/GCAM_region_names") ->
      L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios

    L2322.SubsectorInterp_Fert_cwf %>%
      add_title("Subsector shareweight interpolation of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the subsector shareweight interpolation function infromation from A322.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert_cwf") %>%
      add_precursors("cwf/A322.subsector_interp_cwf_adj", "L2322.SubsectorInterp_Fert", "common/GCAM_region_names") ->
      L2322.SubsectorInterp_Fert_cwf

    L2322.SubsectorInterp_Fert_cwf_H2_scenarios %>%
      add_title("Subsector shareweight interpolation of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the subsector shareweight interpolation function infromation from A322.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert_cwf_H2_scenarios") %>%
      add_precursors("cwf/A322.subsector_interp_cwf_H2_scenarios", "common/GCAM_region_names") ->
      L2322.SubsectorInterp_Fert_cwf_H2_scenarios

    L2322.GlobalTechShrwt_Fert_cwf %>%
      add_title("Tech shareweight of fertilizer tech") %>%
      add_units("NA") %>%
      add_comments("Tech shareweight of fertilizer tech") %>%
      add_legacy_name("L2322.GlobalTechShrwt_Fert_cwf") %>%
      add_precursors("cwf/A322.globaltech_shrwt_cwf") ->
      L2322.GlobalTechShrwt_Fert_cwf


    return_data(L2322.SubsectorShrwtFllt_Fert_cwf,
                L2322.SubsectorInterp_Fert_cwf,
                L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios,
                L2322.SubsectorInterp_Fert_cwf_H2_scenarios,
                L2322.GlobalTechShrwt_Fert_cwf)
  } else {
    stop("Unknown command")
  }
}
