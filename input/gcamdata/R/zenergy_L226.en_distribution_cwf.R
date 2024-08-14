# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L226.en_distribution_cwf
#'
#' Generate the level 2 data tables for the energy distribution sector,
#' including capital costs, shareweights, logits, and interpolations as well as energy use coefficients for electricity and gas pipeline
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.SectorLogitTables[[ curr_table ]]$data}, \code{L226.Supplysector_en}, \code{L226.SubsectorLogitTables[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en}, \code{L226.SubsectorShrwt_en}, \code{L226.SubsectorShrwtFllt_en}, \code{L226.SubsectorInterp_en}, \code{L226.SubsectorInterpTo_en}, \code{L226.StubTech_en}, \code{L226.GlobalTechEff_en}, \code{L226.GlobalTechCost_en}, \code{L226.GlobalTechShrwt_en}, \code{L226.StubTechCoef_elecownuse}, \code{L226.StubTechCoef_electd}, \code{L226.StubTechCoef_gaspipe}, \code{L226.GlobalTechTrackCapital_en}, \code{L226.SubsectorShrwtFllt_en_cwf_no_gas_2050}, \code{L226.SubsectorInterp_en_cwf_no_gas_2050}. The corresponding file in the
#' original data system was \code{L226.en_distribution.R} (energy level2).
#' @details Prepares Level 2 data on energy distribution sector for the generation of en_distribution.xml.
#' Creates global technology database info--cost, shareweight, logit, efficiencies, and interpolations--and regional values where applicable for electricity net ownuse, gas pipelines, and transmission and distribution.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author CWR August 2017
module_energy_L226.en_distribution_cwf <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "cwf/A26.subsector_shrwt_cwf_no_gas_2050",
             FILE = "cwf/A26.subsector_interp_cwf_no_gas_2050"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L226.SubsectorShrwtFllt_en_cwf_no_gas_2050",
             "L226.SubsectorInterp_en_cwf_no_gas_2050"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    year <- year.fillout <- to.value <- technology <- efficiency <- supplysector <- subsector <-
      minicam.energy.input <- input.cost <- share.weight <- calibration <-
      secondary.output <- year.x <- year.y <- . <- value <- region <- coefficient <- GCAM_region_ID <-
      sector <- fuel <- minicam.non.energy.input <- elect_td_techchange <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A26.subsector_shrwt_cwf_no_gas_2050 <- get_data(all_data, "cwf/A26.subsector_shrwt_cwf_no_gas_2050", strip_attributes = TRUE)
    A26.subsector_interp_cwf_no_gas_2050 <- get_data(all_data, "cwf/A26.subsector_interp_cwf_no_gas_2050", strip_attributes = TRUE)

    # ===================================================
    # Make CWF adjustments
    NAMES_SUBSECTORSHRWTFLLT <- c("region", "supplysector", "subsector", "year.fillout", "share.weight")
    NAMES_SUBSECTORINTERP <- c("region", "supplysector", "subsector", "apply.to", "from.year", "to.year", "interpolation.function")

    A26.subsector_shrwt_cwf_no_gas_2050 %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(c(NAMES_SUBSECTORSHRWTFLLT), GCAM_region_names) ->
      L226.SubsectorShrwtFllt_en_cwf_no_gas_2050

    A26.subsector_interp_cwf_no_gas_2050 %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(NAMES_SUBSECTORINTERP), GCAM_region_names) ->
      L226.SubsectorInterp_en_cwf_no_gas_2050


    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all

    L226.SubsectorShrwtFllt_en_cwf_no_gas_2050 %>%
      add_title("regional energy distribution subsector shareweights for CWF hydrogen scenarios") %>%
      add_units("unitless") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_en_cwf_no_gas_2050") %>%
      add_precursors("cwf/A26.subsector_shrwt_cwf_no_gas_2050", "common/GCAM_region_names") ->
      L226.SubsectorShrwtFllt_en_cwf_no_gas_2050

    L226.SubsectorInterp_en_cwf_no_gas_2050 %>%
      add_title("interpolation functions for subsector shareweights for CWF hydrogen scenarios") %>%
      add_units("unitless") %>%
      add_legacy_name("L226.SubsectorInterp_en_cwf_no_gas_2050") %>%
      add_precursors("cwf/A26.subsector_interp_cwf_no_gas_2050", "common/GCAM_region_names") ->
      L226.SubsectorInterp_en_cwf_no_gas_2050

    return_data(L226.SubsectorShrwtFllt_en_cwf_no_gas_2050,
                L226.SubsectorInterp_en_cwf_no_gas_2050
                )
  } else {
    stop("Unknown command")
  }
}
