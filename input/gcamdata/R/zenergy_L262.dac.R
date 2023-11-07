# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L262.dac
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for dac-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L262.SectorLogitTables[[ curr_table ]]$data}, \code{L262.Supplysector_dac}, \code{L262.FinalEnergyKeyword_dac},
#' \code{L262.SubsectorLogitTables[[ curr_table ]]$data}, \code{L262.SubsectorLogit_dac}, \code{L262.SubsectorShrwtFllt_dac},
#' \code{L262.SubsectorInterp_dac}, \code{L262.StubTech_dac}, \code{L262.GlobalTechShrwt_dac}, \code{L262.GlobalTechCoef_dac},
#' \code{L262.GlobalTechCost_dac}, \code{L262.GlobalTechCapture_dac}, \code{L262.StubTechProd_dac}, \code{L262.StubTechCalInput_dac_heat},
#' \code{L262.StubTechCoef_dac}, \code{L262.PerCapitaBased_dac}, \code{L262.BaseService_dac}, \code{L262.PriceElasticity_dac}, \code{object}.
#' The corresponding file in the original data system was \code{L262.dac.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for dac sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr complete nesting
#' @author JF March 2021
module_energy_L262.dac <- function(command, ...) {

  TECH_PARAMETRIZATION_OUTPUTS <- append(paste0("ssp", 1:5),c('EMF_ref','EMF_adv'))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs_cdr",

             FILE = "energy/A62.PrimaryFuelCCoef",
             FILE = "energy/A62.sector",
             FILE = "energy/A62.subsector_interp",
             FILE = "energy/A62.subsector_logit",
             FILE = "energy/A62.subsector_shrwt",

             FILE = "energy/A62.globaltech_coef_ssp1",
             FILE = "energy/A62.globaltech_coef_ssp2",
             FILE = "energy/A62.globaltech_coef_ssp3",
             FILE = "energy/A62.globaltech_coef_ssp4",
             FILE = "energy/A62.globaltech_coef_ssp5",

             FILE = "energy/A62.globaltech_coef_EMF_ref",
             FILE = "energy/A62.globaltech_coef_EMF_adv",


             FILE = "energy/A62.globaltech_cost_ssp1",
             FILE = "energy/A62.globaltech_cost_ssp2",
             FILE = "energy/A62.globaltech_cost_ssp3",
             FILE = "energy/A62.globaltech_cost_ssp4",
             FILE = "energy/A62.globaltech_cost_ssp5",

             FILE = "energy/A62.globaltech_cost_EMF_ref",
             FILE = "energy/A62.globaltech_cost_EMF_adv",

             FILE = "energy/A62.globaltech_shrwt",

             FILE = "energy/A62.globaltech_shrwt_EMF",


             FILE = "energy/A62.globaltech_co2capture",

             FILE = "energy/A62.globaltech_co2capture_EMF",

             FILE = "energy/A62.demand",
             FILE = "energy/A62.globaltech_retirement",
             FILE = "energy/A62.globaltech_retirement_EMF",
             "L162.out_Mt_R_dac_Yh",
             "L225.GlobalTechCost_h2",
             "L225.GlobalTechCoef_h2",
             "L225.StubTechCost_h2",
             "L223.StubTechCapFactor_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L262.CarbonCoef_dac",
             "L262.Supplysector_dac",
             "L262.FinalEnergyKeyword_dac",
             "L262.SubsectorLogit_dac",
             "L262.SubsectorShrwtFllt_dac",
             "L262.SubsectorInterp_dac",

             "L262.GlobalTechCost_dac",

             "L262.GlobalTechCost_dac_ssp1",
             "L262.GlobalTechCost_dac_ssp2",
             "L262.GlobalTechCost_dac_ssp3",
             "L262.GlobalTechCost_dac_ssp4",
             "L262.GlobalTechCost_dac_ssp5",
             "L262.GlobalTechCost_dac_EMF_ref",
             "L262.GlobalTechCost_dac_EMF_adv",

             "L262.StubTech_dac",
             "L262.GlobalTechShrwt_dac",

             "L262.StubTech_dac_EMF",
             "L262.GlobalTechShrwt_dac_EMF",


             'L262.GlobalTechCoef_dac',

             "L262.GlobalTechCoef_dac_ssp1",
             "L262.GlobalTechCoef_dac_ssp2",
             "L262.GlobalTechCoef_dac_ssp3",
             "L262.GlobalTechCoef_dac_ssp4",
             "L262.GlobalTechCoef_dac_ssp5",
             "L262.GlobalTechCoef_dac_EMF_ref",
             "L262.GlobalTechCoef_dac_EMF_adv",

             "L262.GlobalTechCapture_dac",
             "L262.GlobalTechCapture_dac_EMF",
             "L262.PerCapitaBased_dac",
             "L262.PriceElasticity_dac",
             "L262.StubTechProd_dac",
             "L262.BaseService_dac",
             "L262.GlobalTechSCurve_dac",
             "L262.GlobalTechProfitShutdown_dac",

             "L262.GlobalTechSCurve_dac_EMF",
             "L262.GlobalTechProfitShutdown_dac_EMF",
             "L262.StubTechCost_dac"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs_cdr")
    A62.PrimaryFuelCCoef <- get_data(all_data, "energy/A62.PrimaryFuelCCoef", strip_attributes = TRUE)
    A62.sector <- get_data(all_data, "energy/A62.sector", strip_attributes = TRUE)
    A62.subsector_interp <- get_data(all_data, "energy/A62.subsector_interp", strip_attributes = TRUE)
    A62.subsector_logit <- get_data(all_data, "energy/A62.subsector_logit", strip_attributes = TRUE)
    A62.subsector_shrwt <- get_data(all_data, "energy/A62.subsector_shrwt", strip_attributes = TRUE)

    A62.globaltech_co2capture <- get_data(all_data, "energy/A62.globaltech_co2capture")
    A62.globaltech_co2capture_EMF <- get_data(all_data, "energy/A62.globaltech_co2capture_EMF")

    A62.demand <- get_data(all_data, "energy/A62.demand", strip_attributes = TRUE)
    A62.globaltech_retirement <- get_data(all_data, "energy/A62.globaltech_retirement", strip_attributes = TRUE)
    A62.globaltech_retirement_EMF <- get_data(all_data, "energy/A62.globaltech_retirement_EMF", strip_attributes = TRUE)
    L162.out_Mt_R_dac_Yh <- get_data(all_data, "L162.out_Mt_R_dac_Yh", strip_attributes = TRUE)

    L225.GlobalTechCoef_h2 <- get_data(all_data, "L225.GlobalTechCoef_h2", strip_attributes = TRUE)
    L225.GlobalTechCost_h2 <- get_data(all_data, "L225.GlobalTechCost_h2", strip_attributes = TRUE)
    L225.StubTechCost_h2 <- get_data(all_data, "L225.StubTechCost_h2", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)

    #load ssp parametrizations
    A62.globaltech_coef_ssp1 <- get_data(all_data, "energy/A62.globaltech_coef_ssp1")%>% gather_years %>% mutate(scenario=paste0("ssp1"))
    A62.globaltech_coef_ssp2 <- get_data(all_data, "energy/A62.globaltech_coef_ssp2")%>% gather_years %>% mutate(scenario=paste0("ssp2"))
    A62.globaltech_coef_ssp3 <- get_data(all_data, "energy/A62.globaltech_coef_ssp3")%>% gather_years %>% mutate(scenario=paste0("ssp3"))
    A62.globaltech_coef_ssp4 <- get_data(all_data, "energy/A62.globaltech_coef_ssp4")%>% gather_years %>% mutate(scenario=paste0("ssp4"))
    A62.globaltech_coef_ssp5 <- get_data(all_data, "energy/A62.globaltech_coef_ssp5")%>% gather_years %>% mutate(scenario=paste0("ssp5"))
    A62.globaltech_coef_EMF_ref <- get_data(all_data, "energy/A62.globaltech_coef_EMF_ref")%>% gather_years %>% mutate(scenario=paste0("EMF_ref"))
    A62.globaltech_coef_EMF_adv <- get_data(all_data, "energy/A62.globaltech_coef_EMF_adv")%>% gather_years %>% mutate(scenario=paste0("EMF_adv"))

    A62.globaltech_cost_ssp1 <- get_data(all_data, "energy/A62.globaltech_cost_ssp1")%>% gather_years %>% mutate(scenario=paste0("ssp1"))
    A62.globaltech_cost_ssp2 <- get_data(all_data, "energy/A62.globaltech_cost_ssp2")%>% gather_years %>% mutate(scenario=paste0("ssp2"))
    A62.globaltech_cost_ssp3 <- get_data(all_data, "energy/A62.globaltech_cost_ssp3")%>% gather_years %>% mutate(scenario=paste0("ssp3"))
    A62.globaltech_cost_ssp4 <- get_data(all_data, "energy/A62.globaltech_cost_ssp4")%>% gather_years %>% mutate(scenario=paste0("ssp4"))
    A62.globaltech_cost_ssp5 <- get_data(all_data, "energy/A62.globaltech_cost_ssp5")%>% gather_years %>% mutate(scenario=paste0("ssp5"))
    A62.globaltech_cost_EMF_ref <- get_data(all_data, "energy/A62.globaltech_cost_EMF_ref")%>% gather_years %>% mutate(scenario=paste0("EMF_ref"))
    A62.globaltech_cost_EMF_adv <- get_data(all_data, "energy/A62.globaltech_cost_EMF_adv")%>% gather_years %>% mutate(scenario=paste0("EMF_adv"))

    A62.globaltech_shrwt <- get_data(all_data, "energy/A62.globaltech_shrwt")%>% gather_years
    A62.globaltech_shrwt_EMF <- get_data(all_data, "energy/A62.globaltech_shrwt_EMF")%>% gather_years



    # ===================================================
    # 0. Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost <- PrimaryFuelCO2Coef.name <-
      PrimaryFuelCO2Coef <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- half.life <- median.shutdown.point <-
      L262.GlobalTechCoef_dac_ssp1 <- L262.GlobalTechCoef_dac_ssp2 <-L262.GlobalTechCoef_dac_ssp3 <-L262.GlobalTechCoef_dac_ssp4 <-L262.GlobalTechCoef_dac_ssp5 <-
      L262.GlobalTechCost_dac_ssp1 <- L262.GlobalTechCost_dac_ssp2 <- L262.GlobalTechCost_dac_ssp3 <- L262.GlobalTechCost_dac_ssp4 <- L262.GlobalTechCost_dac_ssp5 <-
      L262.GlobalTechShrwt_dac_ssp1 <- L262.GlobalTechShrwt_dac_ssp2 <- L262.GlobalTechShrwt_dac_ssp3 <- L262.GlobalTechShrwt_dac_ssp4 <- L262.GlobalTechShrwt_dac_ssp5 <- NULL


    # ===================================================
    # 1. Perform computations
    # Prelim - carbon coefficient (emissions factor)
    A62.PrimaryFuelCCoef %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["CarbonCoef"]]), GCAM_region_names = GCAM_region_names, has_traded = TRUE) -> L262.CarbonCoef_dac

    # 1a. Supplysector information
    # L262.Supplysector_dac: Supply sector information for CO2 removal sector containing dac subsectors and technologies
    A62.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L262.Supplysector_dac

    # L262.FinalEnergyKeyword_dac: Supply sector keywords for CO2 removal sector
    A62.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L262.FinalEnergyKeyword_dac

    # 1b. Subsector information
    # L262.SubsectorLogit_dac: Subsector logit exponents of CO2 removal sector
    A62.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L262.SubsectorLogit_dac

    # and L262.SubsectorShrwtFllt_dac: Subsector shareweights of CO2 removal sector
    A62.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L262.SubsectorShrwtFllt_dac

    # L262.SubsectorInterp_dac: Subsector shareweight interpolation of CO2 removal sector
    A62.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      L262.SubsectorInterp_dac

    # 1c. Technology information
    # L262.StubTech_dac: Identification of stub technologies of dac
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)


    A62.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L262.StubTech_dac

    # L262.GlobalTechShrwt_dac: Shareweights of global dac technologies
    A62.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology,year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L262.GlobalTechShrwt_dac


    A62.globaltech_shrwt_EMF %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L262.StubTech_dac_EMF

    # L262.GlobalTechShrwt_dac: Shareweights of global dac technologies
    A62.globaltech_shrwt_EMF %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology,year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L262.GlobalTechShrwt_dac_EMF

    #L262.GlobalTechCoef_dac: Energy inputs and coefficients of dac technologies
    #concatenate tables of all ssps into one table for later filtering
    A62.globaltech_coef <- bind_rows(A62.globaltech_coef_ssp1,A62.globaltech_coef_ssp2,A62.globaltech_coef_ssp3,A62.globaltech_coef_ssp4,A62.globaltech_coef_ssp5,A62.globaltech_coef_EMF_ref,A62.globaltech_coef_EMF_adv)

    A62.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input,scenario), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, scenario,year) %>%
      group_by(scenario,supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]],'scenario') ->
      L262.GlobalTechCoef_dac

    H2_elec_for_efuels <- L225.GlobalTechCoef_h2 %>%
      filter(sector.name == 'H2 central production',
             subsector.name == 'electricity',
             minicam.energy.input == 'elect_td_ind') %>%
      mutate(sector.name = 'refining',
             subsector.name = 'dac to liquids',
             coefficient = coefficient * efuels_H2_coef,
             units = 'GJ elec / GJ refined liquids') %>%
      select(sector.name,subsector.name,year,coefficient,units)

    on_site_H2_electrolysis_techs <- L262.GlobalTechCoef_dac %>%
      filter(subsector.name == 'dac to liquids',
             stringr::str_detect(technology,'on-site electrolysis'),
             minicam.energy.input %in% c('elect_td_ind','global solar resource','onshore wind resource')) %>%
      distinct(technology,minicam.energy.input,scenario) %>%
      mutate(sector.name = 'refining',
             subsector.name = 'dac to liquids')

    H2_elec_coef_for_efuels <- on_site_H2_electrolysis_techs %>%
      left_join(H2_elec_for_efuels, by = c('sector.name','subsector.name'))

    L262.GlobalTechCoef_dac <- L262.GlobalTechCoef_dac %>%
      bind_rows(H2_elec_coef_for_efuels) %>%
      group_by(sector.name,subsector.name,technology,minicam.energy.input,year,scenario) %>%
      summarize(coefficient = sum(coefficient)) %>%
      ungroup()


    # Carbon capture rates for dac.
    # L262.GlobalTechCapture_dac: defines CO2 capture fractions for dac (by definition 1, as all inputs are defined per tonne C removed from the atmosphere),
    # as well as a separately-defined process heat dac sector, which has slightly lower capture rates for natural gas combustion emissions.
    # This allows separate consideration of the capture fraction of any combustion emissions resulting from the process heat input
    # No need to consider historical periods here
    A62.globaltech_co2capture %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L262.GlobalTechCapture_dac

    A62.globaltech_co2capture_EMF %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L262.GlobalTechCapture_dac_EMF



    # L262.GlobalTechCost_dac: Non-energy costs of global dac technologies
    #first we concatenate all the ssp cost parametrizations into one table, for later filtering
    A62.globaltech_cost <- bind_rows(A62.globaltech_cost_ssp1,A62.globaltech_cost_ssp2,A62.globaltech_cost_ssp3,A62.globaltech_cost_ssp4,A62.globaltech_cost_ssp5,A62.globaltech_cost_EMF_ref,A62.globaltech_cost_EMF_adv)


    A62.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input,scenario), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, scenario,year) %>%
      group_by(scenario,supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]],'scenario') ->
      L262.GlobalTechCost_dac # intermediate tibble


    H2_elec_cost_for_efuels <- L225.GlobalTechCost_h2 %>%
      filter(sector.name %in% c('H2 central production'),
             subsector.name %in% c('electricity','solar','wind'),
             technology == 'electrolysis') %>%
      mutate(sector.name = 'refining',
             input.cost = input.cost * efuels_H2_coef,
             units = '$1975/GJ refined liquids (H2 only)',
             technology = if_else(subsector.name == 'electricity','on-site electrolysis (grid electricity)',
                                  if_else(subsector.name == 'solar','on-site electrolysis (solar)',
                                          if_else(subsector.name == 'wind','on-site electrolysis (wind)',NA_character_))),
             subsector.name = 'dac to liquids') %>%
      right_join(on_site_H2_electrolysis_techs %>%
                  select(-minicam.energy.input),by = c('sector.name','subsector.name','technology')) %>%
      select(sector.name,subsector.name,technology,minicam.non.energy.input,year,input.cost,units,scenario)



    L262.GlobalTechCost_dac <- L262.GlobalTechCost_dac %>%
      bind_rows(H2_elec_cost_for_efuels) %>%
      group_by(sector.name,subsector.name,technology,year,minicam.non.energy.input,scenario) %>%
      summarize(input.cost = sum(input.cost)) %>%
      ungroup() %>%
      filter(minicam.non.energy.input != 'electrolyzer')
      #electrolyzer costs will be overwritten by the regionally-defined costs in the stubtech so we remove from the global tech table to avoid confusion in the final xml


    L262.GlobalTechCapture_dac %>%
      pull(remove.fraction) %>%
      mean -> dac_CO2_capture_frac

    L262.GlobalTechCoef_dac %>%
      filter(minicam.energy.input == "airCO2") %>%
      pull(coefficient) %>%
      mean ->
      coef_mean # temporary value


     #Calibration and region-specific data
    # L262.StubTechProd_dac: calibrated CO2 removal (cdr) production (arbitrarily high value met entirely by no-capture technology in history)
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L162.out_Mt_R_dac_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(value = daccs+weathering) %>% #set scale based on weathering + daccs potential in each region
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = "sector") %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L262.StubTechProd_dac


    # L262.PerCapitaBased_dac: per-capita based flag for dac exports final demand.  Note that this should be zero as the amount of DAC shouldn't be explicitly tied to population
    A62.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names) ->
      L262.PerCapitaBased_dac


#     L262.BaseService_dac: base-year service output of dac (arbitrarily high)
      L262.StubTechProd_dac %>%
      select(region, year, base.service = calOutputValue) %>%
      mutate(energy.final.demand = A62.demand[["energy.final.demand"]]) ->
      L262.BaseService_dac

    # L262.PriceElasticity_dac: price elasticity (zero to represent the backstop nature of dac technology)
    A62.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L262.PriceElasticity_dac


    # Retirement information
    A62.globaltech_retirement %>%
      set_years %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A62.globaltech_retirement_with_years

    # Copy the data in the last base year period through to the end year
    A62.globaltech_retirement_with_years %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      A62.globaltech_retirement_max_baseyear

    A62.globaltech_retirement_with_years %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(A62.globaltech_retirement_max_baseyear) ->
      L262.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed

    # L262.GlobalTechSCurve_dac: Global tech lifetime and s-curve retirement function
    L262.globaltech_retirement %>%
      filter(!is.na(half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L262.GlobalTechSCurve_dac

    # L262.GlobalTechProfitShutdown_dac: Global tech profit shutdown decider.
    L262.globaltech_retirement %>%
      filter(!is.na(median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L262.GlobalTechProfitShutdown_dac


    # Repeat retirement information for EMF
    A62.globaltech_retirement_EMF %>%
      set_years %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A62.globaltech_retirement_with_years

    # Copy the data in the last base year period through to the end year
    A62.globaltech_retirement_with_years %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      A62.globaltech_retirement_max_baseyear

    A62.globaltech_retirement_with_years %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(A62.globaltech_retirement_max_baseyear) ->
      L262.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed

    # L262.GlobalTechSCurve_dac: Global tech lifetime and s-curve retirement function
    L262.globaltech_retirement %>%
      filter(!is.na(half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L262.GlobalTechSCurve_dac_EMF

    # L262.GlobalTechProfitShutdown_dac: Global tech profit shutdown decider.
    L262.globaltech_retirement %>%
      filter(!is.na(median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L262.GlobalTechProfitShutdown_dac_EMF

    L225.StubTechCost_h2 %>%
      mutate(input.cost = input.cost * efuels_H2_coef,
             stub.technology = if_else(subsector == 'electricity','on-site electrolysis (grid electricity)',
                                  if_else(subsector == 'solar','on-site electrolysis (solar)',
                                          if_else(subsector == 'wind','on-site electrolysis (wind)',NA_character_))),
             subsector = 'dac to liquids',
             supplysector = 'refining') -> L262.StubTechCost_dac

    L223.StubTechCapFactor_elec %>%
      mutate(stub.technology = if_else(stub.technology == 'PV','on-site electrolysis (solar)',
                                       if_else(stub.technology == 'wind','on-site electrolysis (wind)',NA_character_)),
             subsector = 'dac to liquids',
             supplysector = 'refining') -> L262.StubTechCapFactor_dac

    capex_frac_of_dac_NE_cost <- 0.75

    L262.GlobalTechCost_dac %>%
      filter(technology %in% c('on-site electrolysis (solar)','on-site electrolysis (wind)')) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      right_join(L262.StubTechCapFactor_dac %>%
                   filter(stub.technology %in% c('on-site electrolysis (solar)','on-site electrolysis (wind)')),
                   by = c('supplysector','subsector','stub.technology','year')) %>%
      mutate(input.cost = input.cost * capex_frac_of_dac_NE_cost / capacity.factor + input.cost * (1 - capex_frac_of_dac_NE_cost),
             minicam.non.energy.input = 'direct air capture') -> L262.GlobalTechCost_dac_renewable_efuels


    L262.StubTechCost_dac %>%
      right_join(L262.GlobalTechCost_dac_renewable_efuels %>% distinct(scenario),by = character()) %>%#repeat for all scenarios
      bind_rows(L262.GlobalTechCost_dac_renewable_efuels %>% select(-capacity.factor)) -> L262.StubTechCost_dac

    L262.GlobalTechCost_dac <- L262.GlobalTechCost_dac %>%
      filter(!(technology %in% c('on-site electrolysis (solar)','on-site electrolysis (wind)'))) %>%
      mutate(minicam.non.energy.input = if_else(sector.name == 'refining','direct air capture',minicam.non.energy.input))

    # ===================================================
    # Produce outputs

    # Extract SSP and EMF data and assign to separate tables

    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L262.GlobalTechCost_dac %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        add_title(paste("Cost coefficients of dac -", sce)) %>%
        add_units("1975$/kg for supplysector dac; 1975$/GJ for supplysector process heat dac") %>%
        add_comments(sce) %>%
        add_comments("Includes non-energy related capture costs only per kgC captured from the atmosphere. Storage costs will be computed endogenously through the carbon storage markets. Additional non-energy cost of process heat dac assumed zero.") %>%
        add_legacy_name(paste0("L262.GlobalTechCost_dac_", sce)) %>%
        add_precursors(paste0("energy/A62.globaltech_cost_", sce),"L225.GlobalTechCost_h2") ->
        x
      assign(paste0("L262.GlobalTechCost_dac_", sce), x)
    }



    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L262.GlobalTechCoef_dac %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        add_title(paste("Tech coefficients of dac -", sce)) %>%
        add_units("airCO2 input is unitless (Mt airCO2 per Mt dac); all others are GJ per kg (EJ of energy per Mt of dac)") %>%
        add_comments(sce) %>%
        add_comments("For dac sector, the energy use coefficients from A62.globaltech_coef are interpolated into all model years") %>%
        add_legacy_name(paste0("L262.GlobalTechCoef_dac_", sce)) %>%
        add_precursors(paste0("energy/A62.globaltech_coef_", sce),"L225.GlobalTechCoef_h2") ->
        x
      assign(paste0("L262.GlobalTechCoef_dac_", sce), x)
    }


    L262.StubTechCost_dac %>%
      add_title("Regional hydrogen production costs for efuels") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity for hydrogen production is multiplied by the H2 input coefficient for efuels") %>%
      add_precursors("L225.StubTechCost_h2","L223.StubTechCapFactor_elec") -> L262.StubTechCost_dac



      L262.GlobalTechShrwt_dac %>%
        add_units("Unitless") %>%
        add_comments("For dac sector, the share weights from A62.globaltech_shrwt are interpolated into all base years and future years") %>%
        add_legacy_name("L262.GlobalTechShrwt_dac_") %>%
        add_precursors("energy/A62.globaltech_shrwt") ->
        L262.GlobalTechShrwt_dac


    L262.CarbonCoef_dac %>%
      add_title("Assumed carbon coefficient of air-derived CO2") %>%
      add_units("Unitless C/C ratio") %>%
      add_comments("Set in exogenous data table A62.PrimaryFuelCCoef") %>%
      add_precursors("energy/A62.PrimaryFuelCCoef", "common/GCAM_region_names") ->
      L262.CarbonCoef_dac


    L262.Supplysector_dac %>%
      add_title("Supply sector information for CO2 removal sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A62.sector is expended into all GCAM regions") %>%
      add_legacy_name("L262.Supplysector_dac") %>%
      add_precursors("energy/A62.sector", "common/GCAM_region_names") ->
      L262.Supplysector_dac

    L262.FinalEnergyKeyword_dac %>%
      add_title("Supply sector keywords for dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector final energy keywords from A62.sector are expended into all GCAM regions") %>%
      add_legacy_name("L262.FinalEnergyKeyword_dac") %>%
      add_precursors("energy/A62.sector", "common/GCAM_region_names") ->
      L262.FinalEnergyKeyword_dac

    L262.SubsectorLogit_dac %>%
      add_title("Subsector logit exponents of dac sector") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the subsector logit exponents from A62.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L262.SubsectorLogit_dac") %>%
      add_precursors("energy/A62.subsector_logit", "common/GCAM_region_names") ->
      L262.SubsectorLogit_dac

    L262.SubsectorShrwtFllt_dac %>%
      add_title("Subsector shareweights of dac sector") %>%
      add_units("unitless") %>%
      add_comments("For dac sector, the subsector shareweights from A62.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L262.SubsectorShrwtFllt_dac") %>%
      add_precursors("energy/A62.subsector_shrwt", "common/GCAM_region_names") ->
      L262.SubsectorShrwtFllt_dac

    L262.SubsectorInterp_dac %>%
      add_title("Subsector shareweight interpolation of dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the subsector shareweight interpolation function infromation from A62.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L262.SubsectorInterp_dac") %>%
      add_precursors("energy/A62.subsector_interp", "common/GCAM_region_names") ->
      L262.SubsectorInterp_dac

    L262.StubTech_dac %>%
      add_title("Identification of stub technologies of dac") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the stub technologies from A62.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L262.StubTech_dac") %>%
      add_precursors("energy/A62.globaltech_shrwt","common/GCAM_region_names") ->
      L262.StubTech_dac

    L262.StubTech_dac_EMF %>%
      add_title("Identification of stub technologies of dac") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the stub technologies from A62.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L262.StubTech_dac") %>%
      add_precursors("energy/A62.globaltech_shrwt","common/GCAM_region_names") ->
      L262.StubTech_dac_EMF

    L262.GlobalTechShrwt_dac %>%
      add_title("Shareweights of global dac technologies") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the share weights from A62.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_precursors("energy/A62.globaltech_shrwt_EMF") ->
      L262.GlobalTechShrwt_dac

    L262.GlobalTechShrwt_dac_EMF %>%
      add_title("Shareweights of global dac technologies") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the share weights from A62.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_precursors("energy/A62.globaltech_shrwt_EMF") ->
      L262.GlobalTechShrwt_dac_EMF

    L262.GlobalTechCoef_dac %>%
      add_title("Energy inputs and coefficients of dac technologies") %>%
      add_units("airCO2 input is unitless (Mt airCO2 per Mt dac); all others are GJ per kg (EJ of energy per Mt of dac)") %>%
      add_comments("For dac sector, the energy use coefficients from A62.globaltech_coef are interpolated into all model years") %>%
      add_precursors("energy/A62.globaltech_coef_ssp1","energy/A62.globaltech_coef_ssp2","energy/A62.globaltech_coef_ssp3","energy/A62.globaltech_coef_ssp4","energy/A62.globaltech_coef_ssp5") ->
      L262.GlobalTechCoef_dac

    L262.GlobalTechCost_dac %>%
      add_title("Non-energy costs of global dac manufacturing technologies") %>%
      add_units("1975$/kg for supplysector dac; 1975$/GJ for supplysector process heat dac") %>%
      add_comments("Includes non-energy related capture costs only per kgC captured from the atmosphere. Storage costs will be computed endogenously through the carbon storage markets. Additional non-energy cost of process heat dac assumed zero.") %>%
      add_precursors("energy/A62.globaltech_cost_ssp1","energy/A62.globaltech_cost_ssp2","energy/A62.globaltech_cost_ssp3","energy/A62.globaltech_cost_ssp4","energy/A62.globaltech_cost_ssp5") ->
      L262.GlobalTechCost_dac

    L262.GlobalTechCapture_dac %>%
      add_title("CO2 capture fractions from global dac production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the remove fractions from A62.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L262.GlobalTechCapture_dac") %>%
      add_precursors("energy/A62.globaltech_co2capture") ->
      L262.GlobalTechCapture_dac

    L262.GlobalTechCapture_dac_EMF %>%
      add_title("CO2 capture fractions from global dac production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the remove fractions from A62.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L262.GlobalTechCapture_dac_EMF") %>%
      add_precursors("energy/A62.globaltech_co2capture_EMF") ->
      L262.GlobalTechCapture_dac_EMF


    L262.PerCapitaBased_dac %>%
      add_title("per-capita based flag for dac exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for dac from A62.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L262.PerCapitaBased_dac") %>%
      add_precursors("energy/A62.demand", "common/GCAM_region_names") ->
      L262.PerCapitaBased_dac


    L262.PriceElasticity_dac %>%
      add_title("price elasticity for dac") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A62.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L262.PriceElasticity_dac") %>%
      add_precursors("energy/A62.demand", "common/GCAM_region_names") ->
      L262.PriceElasticity_dac

    L262.StubTechProd_dac %>%
      add_title("calibrated cdr values") %>%
      add_units("Mt") %>%
      add_comments("Values are calculated using L162.out_Mt_R_dac_Yh then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L262.StubTechProd_dac") %>%
      add_precursors("energy/calibrated_techs_cdr", "L162.out_Mt_R_dac_Yh", "common/GCAM_region_names") ->
      L262.StubTechProd_dac

    L262.BaseService_dac %>%
      add_title("base-year service output of dac") %>%
      add_units("Mt") %>%
      add_comments("Transformed from L262.StubTechProd_dac by adding energy.final.demand from A62.demand") %>%
      add_legacy_name("L262.BaseService_dac") %>%
      add_precursors("energy/A62.demand", "energy/calibrated_techs_cdr", "L162.out_Mt_R_dac_Yh", "common/GCAM_region_names") ->
      L262.BaseService_dac

    L262.GlobalTechSCurve_dac %>%
      add_title("Global tech lifetime and s-curve retirement function") %>%
      add_units("year for lifetime and halflife; Unitless for steepness") %>%
      add_comments("The values are extracted from L262.globaltech_retirement for entries that half life value is not NA") %>%
      add_legacy_name("L262.GlobalTechSCurve_dac") %>%
      add_precursors("energy/A62.globaltech_retirement") ->
      L262.GlobalTechSCurve_dac

    L262.GlobalTechProfitShutdown_dac %>%
      add_title("Global tech profit shutdown decider") %>%
      add_units("Unitless") %>%
      add_comments("The values are extracted from L262.globaltech_retirement for entries that median shutdown point is not NA") %>%
      add_legacy_name("L262.GlobalTechProfitShutdown_dac") %>%
      add_precursors("energy/A62.globaltech_retirement") ->
      L262.GlobalTechProfitShutdown_dac

    L262.GlobalTechSCurve_dac_EMF %>%
      add_title("Global tech lifetime and s-curve retirement function") %>%
      add_units("year for lifetime and halflife; Unitless for steepness") %>%
      add_comments("The values are extracted from L262.globaltech_retirement for entries that half life value is not NA") %>%
      add_legacy_name("L262.GlobalTechSCurve_dac_EMF") %>%
      add_precursors("energy/A62.globaltech_retirement_EMF") ->
      L262.GlobalTechSCurve_dac_EMF

    L262.GlobalTechProfitShutdown_dac_EMF %>%
      add_title("Global tech profit shutdown decider") %>%
      add_units("Unitless") %>%
      add_comments("The values are extracted from L262.globaltech_retirement for entries that median shutdown point is not NA") %>%
      add_legacy_name("L262.GlobalTechProfitShutdown_dac_EMF") %>%
      add_precursors("energy/A62.globaltech_retirement_EMF") ->
      L262.GlobalTechProfitShutdown_dac_EMF





    return_data(L262.CarbonCoef_dac,
                L262.Supplysector_dac, L262.FinalEnergyKeyword_dac, L262.SubsectorLogit_dac,
                L262.SubsectorShrwtFllt_dac, L262.SubsectorInterp_dac,
                L262.GlobalTechCost_dac,
                L262.GlobalTechCost_dac_ssp1,L262.GlobalTechCost_dac_ssp2,L262.GlobalTechCost_dac_ssp3,L262.GlobalTechCost_dac_ssp4,L262.GlobalTechCost_dac_ssp5,
                L262.GlobalTechCost_dac_EMF_ref,L262.GlobalTechCost_dac_EMF_adv,
                L262.StubTech_dac,L262.StubTech_dac_EMF,
                L262.GlobalTechShrwt_dac,L262.GlobalTechShrwt_dac_EMF,
                L262.GlobalTechCoef_dac,
                L262.GlobalTechCoef_dac_ssp1, L262.GlobalTechCoef_dac_ssp2, L262.GlobalTechCoef_dac_ssp3, L262.GlobalTechCoef_dac_ssp4, L262.GlobalTechCoef_dac_ssp5,
                L262.GlobalTechCoef_dac_EMF_ref,L262.GlobalTechCoef_dac_EMF_adv,
                L262.GlobalTechCapture_dac,
                L262.GlobalTechCapture_dac_EMF,
                L262.PerCapitaBased_dac,
                L262.PriceElasticity_dac,L262.StubTechProd_dac,L262.BaseService_dac,L262.GlobalTechSCurve_dac,
                L262.GlobalTechProfitShutdown_dac,
                L262.GlobalTechSCurve_dac_EMF,
                L262.GlobalTechProfitShutdown_dac_EMF,
                L262.StubTechCost_dac)
  } else {
    stop("Unknown command")
  }
}
