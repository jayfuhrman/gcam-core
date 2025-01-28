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
    return(c(FILE = "common/GCAM_region_names",
             FILE = "cwf/A54.demand_ssp1_cwf_adj",
             FILE = "cwf/A54.globaltranTech_shrwt_cwf_low_h2",
             FILE=  "cwf/A54.globaltranTech_interp_cwf_low_h2",
             FILE = "cwf/A54.globaltranTech_shrwt_revised_cwf",
             FILE = "cwf/A54.globaltranTech_interp_revised_cwf",
             FILE = "cwf/A54.transport_ICE_phaseout_cwf",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.IncomeElasticity_trn",
             "L254.StubTranTech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L254.GlobalTranTechInterp_cwf_low_h2",
             "L254.GlobalTranTechShrwt_cwf_low_h2",
             "L254.GlobalTranTechInterp_cwf",
             "L254.GlobalTranTechShrwt_cwf",
             "L254.tranSubsectorVOTT_cwf",
             "L254.tranSubsectorFuelPref_cwf",
             "L254.IncomeElasticity_trn_cwf",
             "L254.StubTranTechInterpTo_ICEPhaseout"))
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
    #

    A54.globaltranTech_shrwt_cwf_low_h2 <- get_data(all_data, "cwf/A54.globaltranTech_shrwt_cwf_low_h2",strip_attributes = TRUE)
    A54.globaltranTech_interp_cwf_low_h2 <- get_data(all_data, "cwf/A54.globaltranTech_interp_cwf_low_h2",strip_attributes = TRUE)
    A54.demand_SSP1_cwf_adj <- get_data(all_data, "cwf/A54.demand_ssp1_cwf_adj",strip_attributes = TRUE)
    A54.globaltranTech_shrwt_cwf <- get_data(all_data, "cwf/A54.globaltranTech_shrwt_revised_cwf",strip_attributes = TRUE)
    A54.globaltranTech_interp_cwf <- get_data(all_data, "cwf/A54.globaltranTech_interp_revised_cwf",strip_attributes = TRUE)
    A54.transport_ICE_phaseout_cwf <- get_data(all_data, "cwf/A54.transport_ICE_phaseout_cwf",strip_attributes = TRUE)
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech", strip_attributes = TRUE)
    L254.IncomeElasticity_trn <- get_data(all_data,'L254.IncomeElasticity_trn',strip_attributes = TRUE)
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT",strip_attributes = TRUE)
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref",strip_attributes = TRUE)
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

    # CWF adjustments to income elasticity, adding to L254.IncomeElasticity_trn for CWF scenario
    L254.IncomeElasticity_trn_cwf <- L254.IncomeElasticity_trn %>%
      bind_rows(L254.IncomeElasticity_trn %>%
                  filter(sce == "SSP1") %>%
                  left_join(A54.demand_SSP1_cwf_adj) %>%
                  mutate(income.elasticity = income.elasticity * income.elasticity_adj,
                         sce = "CWF") %>%
                  dplyr::select(-income.elasticity_adj))

    # L254.tranSubsectorVOTT_cwf: Value of time in transit parameterization, using SSP1 values but without subsetting
    # NOTE: These are currently considered time- and region-independent characteristics
    L254.tranSubsectorVOTT %>%
      # Subset only the combinations of region, supplysector, and tranSubsector
      filter(sce == "SSP1") %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], year.fillout, addTimeValue, time.value.multiplier,sce) %>%
      na.omit()->
      L254.tranSubsectorVOTT_cwf

    L254.tranSubsectorFuelPref %>%
      filter(sce == "SSP1") ->
      L254.tranSubsectorFuelPref_cwf

    # CWF adjustments to share weights and interpolation rules

    tmp <- L254.StubTranTech %>%
      distinct(region,supplysector,tranSubsector,stub.technology)

    A54.transport_ICE_phaseout_cwf %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[['StubTranTechInterpTo']],"sce"),GCAM_region_names = GCAM_region_names) %>%
      # make sure we aren't creating subsectors + technologies that aren't defined in UCD core.
      right_join(L254.StubTranTech %>%
                  distinct(region,supplysector,tranSubsector,stub.technology) %>%
                  filter(stub.technology %in% A54.transport_ICE_phaseout_cwf$stub.technology,
                         supplysector %in% A54.transport_ICE_phaseout_cwf$supplysector),
                by = c('region','supplysector','tranSubsector','stub.technology')) -> L254.StubTranTechInterpTo_ICEPhaseout

    # L254.GlobalTranTechInterp_cwf
    A54.globaltranTech_interp_cwf %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechInterp"]],"sce") ->
      L254.GlobalTranTechInterp_cwf

    # L254.GlobalTranTechShrwt_cwf: Shareweights of global tranTechnologies
    A54.globaltranTech_shrwt_cwf %>%
      gather_years %>%
      # Expand table to include all model years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology, sce)) %>%
      arrange(supplysector, tranSubsector, tranTechnology, sce, year) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, tranSubsector, tranTechnology, sce) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2),
             share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]],sce) ->
      L254.GlobalTranTechShrwt_cwf # OUTPUT






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

    L254.GlobalTranTechInterp_cwf %>%
      add_title("Shareweight interpolation of global tranTechnologies") %>%
      add_units("NA") %>%
      add_comments("Populated placeholders for final calibration year and end year") %>%
      #add_legacy_name("L254.GlobalTranTechInterp") %>%
      add_precursors("cwf/A54.globaltranTech_interp_revised_cwf") ->
      L254.GlobalTranTechInterp_cwf

    L254.GlobalTranTechShrwt_cwf %>%
      add_title("Shareweights of global tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Data was subsetted for model years") %>%
      add_legacy_name("L254.GlobalTranTechShrwt") %>%
      add_precursors("cwf/A54.globaltranTech_shrwt_revised_cwf") ->
      L254.GlobalTranTechShrwt_cwf

    L254.tranSubsectorVOTT_cwf %>%
      add_title("Value of time in transit parameterization") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all regions") %>%
      add_comments("Year.fillout was populated with minimum model year") %>%
      add_legacy_name("L254.tranSubsectorVOTT") %>%
      add_precursors("L254.tranSubsectorVOTT") ->
      L254.tranSubsectorVOTT_cwf

    L254.tranSubsectorFuelPref_cwf %>%
      add_title("Subsector preferences that are tied to GDP (unrelated to time value)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all regions") %>%
      add_comments("Year.fillout was populated with minimum model year") %>%
      add_legacy_name("L254.tranSubsectorFuelPref") %>%
      add_precursors("L254.tranSubsectorFuelPref") ->
      L254.tranSubsectorFuelPref_cwf

    L254.IncomeElasticity_trn_cwf %>%
      add_title("Income elasticity of transportation final demand - cwf adjustments") %>%
      add_units("Unitless") %>%
      add_comments("Income elasticity information written for all GCAM regions and model future years") %>%
      add_legacy_name("L254.IncomeElasticity_trn_cwf") %>%
      add_precursors("L254.IncomeElasticity_trn", "cwf/A54.demand_ssp1_cwf_adj") ->
      L254.IncomeElasticity_trn_cwf

    L254.StubTranTechInterpTo_ICEPhaseout %>%
      add_title("Shareweight phaseout of ICE vehicles in rail and road transport") %>%
      add_units("Unitless") %>%
      add_comments("Note there are no shareweight adjustments for the CWF_low scenario") %>%
      add_legacy_name("L254.StubTranTechInterpTo_ICEPhaseout") %>%
      add_precursors("common/GCAM_region_names", "cwf/A54.transport_ICE_phaseout_cwf","L254.StubTranTech") ->
      L254.StubTranTechInterpTo_ICEPhaseout

    return_data(
      L254.GlobalTranTechInterp_cwf_low_h2, L254.GlobalTranTechShrwt_cwf_low_h2,
      L254.GlobalTranTechInterp_cwf, L254.GlobalTranTechShrwt_cwf,
      L254.tranSubsectorVOTT_cwf,L254.tranSubsectorFuelPref_cwf, L254.IncomeElasticity_trn_cwf,
      L254.StubTranTechInterpTo_ICEPhaseout)
  } else {
    stop("Unknown command")
  }
}
