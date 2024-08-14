# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L225.hydrogen_cwf
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.Supplysector_h2}, \code{L225.SubsectorLogit_h2}, \code{L225.SubsectorShrwtFllt_cwf_no_pipeline}, \code{L225.StubTech_h2}, \code{L225.GlobalTechCoef_h2}, \code{L225.GlobalTechCost_h2}, \code{L225.GlobalTechShrwt_cwf_no_pipeline}, \code{L225.PrimaryRenewKeyword_h2}, \code{L225.GlobalTechCapture_h2}, \code{L225.StubTechCost_h2}, \code{L225.GlobalTechProfitShutdown_h2}, \code{L225.GlobalTechSCurve_h2}. The corresponding file in the
#' original data system was \code{L225.hydrogen.R} (energy level2).
#' @details Provides supply sector information, subsector information, technology information for hydrogen sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select
#' @importFrom tidyr complete nesting
#' @author LF Augest 2017
module_energy_L225.hydrogen_cwf <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",

             FILE = "cwf/A25.subsector_shrwt_cwf_no_pipeline",
             FILE = "cwf/A25.globaltech_shrwt_cwf_no_pipeline"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.SubsectorShrwtFllt_cwf_no_pipeline",
             "L225.GlobalTechShrwt_cwf_no_pipeline"))
  } else if(command == driver.MAKE) {

    # Silencing package checks
    region <- coefficient <- cost <- price.unit.conversion <- sector.name <- subsector.name <-
      stub.technology <- capacity.factor <- IdleRatio <- `2040` <- `2020` <- `2050` <-
      intermittent.technology <- capital.overnight <- fixed.charge.rate <- OM.fixed <-
      cost_75USD_kW_yr <- kWh_elec_per_kgH2 <- output_kgh2_d <- cost_75USD_kgH2 <- NULL

    all_data <- list(...)[[1]]

    year.fillout <- technology <- year <- efficiency <- supplysector <- value <-
      subsector <- minicam.energy.input <- input.cost <- minicam.non.energy.input <-
      share.weight <- primary.renewable <- average.fossil.efficiency <-
      remove.fraction <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A25.subsector_shrwt_cwf_no_pipeline <- get_data(all_data, "cwf/A25.subsector_shrwt_cwf_no_pipeline", strip_attributes = TRUE)
    A25.globaltech_shrwt_cwf_no_pipeline <- get_data(all_data, "cwf/A25.globaltech_shrwt_cwf_no_pipeline", strip_attributes = TRUE)

    # ===================================================

    # 1. Build tables for CSVs
    # 1a. Supply sector information

    # L225.SubsectorShrwtFllt_cwf_no_pipeline: Subsector shareweights of hydrogen sectors
    A25.subsector_shrwt_cwf_no_pipeline %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L225.SubsectorShrwtFllt_cwf_no_pipeline

    # 1c. Technology information

    # L225.GlobalTechShrwt_cwf_no_pipeline: Shareweights of global technologies for hydrogen
    # Shareweights of global technologies
    A25.globaltech_shrwt_cwf_no_pipeline %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(-value) ->
      L225.GlobalTechShrwt_cwf_no_pipeline

    # ===================================================
    # Produce outputs

    L225.SubsectorShrwtFllt_cwf_no_pipeline %>%
      add_title("Subsector shareweights of hydrogen sectors -- for cwf low H2 scenario") %>%
      add_units("Unitless") %>%
      add_comments("Expand Subsector shareweights for all GCAM regions") %>%
      add_legacy_name("L225.SubsectorShrwtFllt_cwf_no_pipeline") %>%
      add_precursors("common/GCAM_region_names", "cwf/A25.subsector_shrwt_cwf_no_pipeline") ->
      L225.SubsectorShrwtFllt_cwf_no_pipeline

    L225.GlobalTechShrwt_cwf_no_pipeline %>%
      add_title("Shareweights of global technologies for hydrogen -- for cwf low H2 scenario") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechShrwt_cwf_no_pipeline") %>%
      add_precursors("cwf/A25.globaltech_shrwt_cwf_no_pipeline") ->
      L225.GlobalTechShrwt_cwf_no_pipeline


    return_data(L225.SubsectorShrwtFllt_cwf_no_pipeline,
                L225.GlobalTechShrwt_cwf_no_pipeline)
  } else {
    stop("Unknown command")
  }
}
