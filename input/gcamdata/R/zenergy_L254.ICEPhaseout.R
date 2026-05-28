# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L254.transportation_UCD_liquids
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
#' \code{L254.IncomeElasticity_trn}, \code{L254.BaseService_trn}, \code{L254.GlobalTranTechInterp_liquids}, \code{L254.GlobalTranTechShrwt_liquids}, \code{L254.tranSubsectorVOTT_liquids}. The corresponding file in the
#' original data system was \code{L254.transportation_UCD.R} (energy level2).
#' @details Due to the asymmetrical nature of the transportation sectors in the various regions, we can't simply write
#' generic information to all regions. Instead, technology information is read from the global UCD transportation
#' technology database, and supplysector and subsector attributes are matched in from lookup tables.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows filter if_else group_by left_join mutate one_of pull select semi_join summarise contains desc
#' @importFrom tidyr complete nesting
#' @author AJS September 2017
module_energy_L254.transportation_UCD_ICEPhaseout <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A54.transport_interp_liquids",
             FILE = "energy/A54.transport_shrwt_liquids",
             "L254.StubTranTechCalInput"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L254.StubTranTechInterpTo_liquids",
             "L254.StubTranTechShrwt_liquids"))
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
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes = TRUE)
    A54.transport_interp_liquids <- get_data(all_data, "energy/A54.transport_interp_liquids",strip_attributes = TRUE)
    A54.transport_shrwt_liquids <- get_data(all_data, "energy/A54.transport_shrwt_liquids",strip_attributes = TRUE)
    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput", strip_attributes = TRUE)
    # ===================================================


    # adjustments to share weights and interpolation rules

    L254.StubTranTechInterpTo_all_region <-
      A54.transport_interp_liquids %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTranTechInterpTo"]]), GCAM_region_names = GCAM_region_names)

    L254.StubTranTechShrwt_all_region <-
      A54.transport_shrwt_liquids %>%
      gather_years() %>%
      rename(share.weight = value) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTranTechShrwt"]]), GCAM_region_names = GCAM_region_names)

    # 2W and 3W using NG does not exist in all regions, if we apply the cwf interpolation rule and shareweight assumptions for 2W and 3W using NG for all regions,
    # that will add non-zero historical share-weight for 2W and 3W (NG) in regions that do not have them in historical years, this causes calibration errors in the
    # historical years, so here we use the L254.StubTranTechCalInput to get those regions that do have 2W and 3W (NG) in historical years, then use it to filter out
    # regions that don't have 2W and 3W (NG) in historical years for L254.StubTranTechInterpTo_all_region, and L254.StubTranTechShrwt_all_region.
    trans_2w3w_keep <-
      L254.StubTranTechCalInput %>% filter(tranSubsector == "2W and 3W") %>%
      select(region, supplysector, tranSubsector, stub.technology) %>%
      distinct()

    L254.StubTranTechInterpTo_liquids <-
      L254.StubTranTechInterpTo_all_region %>%
      filter(tranSubsector == "2W and 3W") %>%
      semi_join(trans_2w3w_keep,
                by = c("region", "supplysector", "tranSubsector", "stub.technology")) %>%
      rbind(L254.StubTranTechInterpTo_all_region %>%
              filter(tranSubsector != "2W and 3W"))

    L254.StubTranTechShrwt_liquids <-
      L254.StubTranTechShrwt_all_region %>%
      filter(tranSubsector == "2W and 3W") %>%
      semi_join(trans_2w3w_keep,
                by = c("region", "supplysector", "tranSubsector", "stub.technology")) %>%
      rbind(L254.StubTranTechShrwt_all_region %>%
              filter(tranSubsector != "2W and 3W"))

    # ===================================================

    L254.StubTranTechInterpTo_liquids %>%
      add_title("Shareweight adjustments to ICE (phase-outs) and FCEV (low H2 only) vehicles") %>%
      add_units("Unitless") %>%
      add_comments("Note there are no shareweight adjustments for the CWF_low, nor for the H2 med or H2 high scenarios") %>%
      add_legacy_name("L254.StubTranTechInterpTo_liquids") %>%
      add_precursors("common/GCAM_region_names", "cwf/A54.transport_interp_liquids", "L254.StubTranTechCalInput") ->
      L254.StubTranTechInterpTo_liquids

    L254.StubTranTechShrwt_liquids %>%
      add_title("Shareweight adjustments to ICE (phase-outs) and FCEV (low H2 only) vehicles") %>%
      add_units("Unitless") %>%
      add_comments("Note there are no shareweight adjustments for the CWF_low, nor for the H2 med or H2 high scenarios") %>%
      add_legacy_name("L254.StubTranTechShrwt_liquids") %>%
      add_precursors("common/GCAM_region_names", "cwf/A54.transport_shrwt_liquids", "L254.StubTranTechCalInput") ->
      L254.StubTranTechShrwt_liquids

    return_data(
      L254.StubTranTechInterpTo_liquids,
      L254.StubTranTechShrwt_liquids)
  } else {
    stop("Unknown command")
  }
}
