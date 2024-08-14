# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L254.transportation_UCD_cwf
#'
#' Calculate transportation data using information from the global UCD transportation technology database.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.Supplysector_trn}, \code{L254.FinalEnergyKeyword_trn}, \code{L254.tranSubsectorLogit},
#' \code{L254.tranSubsectorShrwt}, \code{L254.tranSubsectorShrwtFllt}, \code{L254.tranSubsectorInterp},
#' \code{L254.tranSubsectorInterpTo}, \code{L254.tranSubsectorSpeed}, \code{L254.tranSubsectorSpeed_passthru},
#' \code{L254.tranSubsectorSpeed_noVOTT}, \code{L254.tranSubsectorSpeed_nonmotor}, \code{L254.tranSubsectorVOTT},
#' \code{L254.tranSubsectorFuelPref}, \code{L254.StubTranTech}, \code{L254.StubTech_passthru}, \code{L254.StubTech_nonmotor},
#' \code{L254.GlobalTechShrwt_passthru}, \code{L254.GlobalTechShrwt_nonmotor}, \code{L254.GlobalTechCoef_passthru},
#' \code{L254.GlobalRenewTech_nonmotor}, \code{L254.GlobalTranTechInterp}, \code{L254.GlobalTranTechShrwt},
#' \code{L254.GlobalTranTechSCurve}, \code{L254.StubTranTechCalInput}, \code{L254.StubTranTechLoadFactor},
#' \code{L254.StubTranTechCost}, \code{L254.StubTranTechCoef}, \code{L254.StubTechCalInput_passthru},
#' \code{L254.StubTechProd_nonmotor}, \code{L254.PerCapitaBased_trn}, \code{L254.PriceElasticity_trn},
#' \code{L254.IncomeElasticity_trn}, \code{L254.BaseService_trn}, \code{L254.GlobalTranTechInterp_cwf}, \code{L254.GlobalTranTechShrwt_cwf}, \code{L254.tranSubsectorVOTT_cwf}. The corresponding file in the
#' original data system was \code{L254.transportation_UCD.R} (energy level2).
#' @details Due to the asymmetrical nature of the transportation sectors in the various regions, we can't simply write
#' generic information to all regions. Instead, technology information is read from the global UCD transportation
#' technology database, and supplysector and subsector attributes are matched in from lookup tables.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows filter if_else group_by left_join mutate one_of pull select semi_join summarise contains desc
#' @importFrom tidyr complete nesting
#' @author AJS September 2017
module_energy_L254.transportation_UCD_cwf <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(
             FILE = "cwf/A54.demand_ssp1_cwf_adj",
             FILE = "cwf/A54.globaltranTech_shrwt_cwf_low_h2",
             FILE=  "cwf/A54.globaltranTech_interp_cwf_low_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L254.GlobalTranTechInterp_cwf_low_h2",
             "L254.GlobalTranTechShrwt_cwf_low_h2"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    GCAM_region_ID <- tranTechnology <- region <- supplysector <- . <- technology <- minicam.energy.input <- r_mei <-
      year <- year.fillout <- to.value <- value <- speed.source <- tranSubsector.x <- addTimeValue <- time.value.multiplier <-
      fuelprefElasticity <- tranSubsector <- share.weight <- calibrated.value <- subs.share.weight <- loadFactor <-
      coefficient <- stub.technology <- output <- output_agg <- output_cum <- share.weight.year <- tech.share.weight <-
      calOutputValue <- energy.final.demand <- base.service <- object <- r_ss <- UCD_region <- size.class <- sce <-
      steepness <- profit.shutdown.steepness <- NULL

    # Load required inputs
    # GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes = TRUE)
    #
    # A54.demand_SSP1_cwf_adj <- get_data(all_data, "cwf/A54.demand_ssp1_cwf_adj",strip_attributes = TRUE)
    A54.globaltranTech_shrwt_cwf_low_h2 <- get_data(all_data, "cwf/A54.globaltranTech_shrwt_cwf_low_h2",strip_attributes = TRUE)
    A54.globaltranTech_interp_cwf_low_h2 <- get_data(all_data, "cwf/A54.globaltranTech_interp_cwf_low_h2",strip_attributes = TRUE)

    # ===================================================

    # PART A: BUILDING TRANSPORTATION SECTORS FROM THE TECHNOLOGY LEVEL UP
    # L254.StubTranTech: Transportation stub technologies (built from technologies with coefficients in the UCD database)

    # CWF adjustments to share weights and interpolation rules
    # L254.GlobalTranTechInterp_cwf
    A54.globaltranTech_interp_cwf_low_h2 %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechInterp"]]) ->
      L254.GlobalTranTechInterp_cwf_low_h2

    # L254.GlobalTranTechShrwt_cwf: Shareweights of global tranTechnologies
    A54.globaltranTech_shrwt_cwf_low_h2 %>%
      gather_years %>%
      # Expand table to include all model years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology)) %>%
      arrange(supplysector, tranSubsector, tranTechnology, year) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, tranSubsector, tranTechnology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2),
             share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]]) ->
      L254.GlobalTranTechShrwt_cwf_low_h2 # OUTPUT


    # ===================================================

    L254.GlobalTranTechInterp_cwf_low_h2 %>%
      add_title("Shareweight interpolation of global tranTechnologies") %>%
      add_units("NA") %>%
      add_comments("Populated placeholders for final calibration year and end year") %>%
      add_legacy_name("L254.GlobalTranTechInterp_cwf_low_h2") %>%
      add_precursors("cwf/A54.globaltranTech_interp_cwf_low_h2") ->
      L254.GlobalTranTechInterp_cwf_low_h2

    L254.GlobalTranTechShrwt_cwf_low_h2 %>%
      add_title("Shareweights of global tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Data was subsetted for model years") %>%
      add_legacy_name("L254.GlobalTranTechShrwt_cwf_low_h2") %>%
      add_precursors("cwf/A54.globaltranTech_shrwt_cwf_low_h2") ->
      L254.GlobalTranTechShrwt_cwf_low_h2

    return_data(
      L254.GlobalTranTechInterp_cwf_low_h2, L254.GlobalTranTechShrwt_cwf_low_h2)
  } else {
    stop("Unknown command")
  }
}
