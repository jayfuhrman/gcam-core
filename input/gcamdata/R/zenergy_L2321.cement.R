# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2321.cement
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for cement-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2321.SectorLogitTables[[ curr_table ]]$data}, \code{L2321.Supplysector_cement}, \code{L2321.FinalEnergyKeyword_cement},
#' \code{L2321.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2321.SubsectorLogit_cement}, \code{L2321.SubsectorShrwtFllt_cement},
#' \code{L2321.SubsectorInterp_cement}, \code{L2321.StubTech_cement}, \code{L2321.GlobalTechShrwt_cement}, \code{L2321.GlobalTechCoef_cement},
#' \code{L2321.GlobalTechCost_cement}, \code{L2321.GlobalTechCapture_cement}, \code{L2321.StubTechProd_cement}, \code{L2321.StubTechCalInput_cement_heat},
#' \code{L2321.StubTechCoef_cement}, \code{L2321.PerCapitaBased_cement}, \code{L2321.BaseService_cement}, \code{L2321.PriceElasticity_cement}, \code{L2321.IncomeElasticity_cement_Scen},
#' \code{object}. The corresponding file in the
#' original data system was \code{L2321.cement.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for cement sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr complete nesting
#' @author LF October 2017
module_energy_L2321.cement <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/calibrated_techs",
      FILE = "energy/A321.sector",
      FILE = "emissions/A_PrimaryFuelCCoef",
      FILE = "energy/A321.sector",
      FILE = "energy/A321.subsector_interp",
      FILE = "energy/A321.subsector_logit",
      FILE = "energy/A321.subsector_shrwt",
      FILE = "energy/A321.globaltech_coef",
      FILE = "energy/A321.globaltech_cost",
      FILE = "energy/A321.globaltech_shrwt",
      FILE = "energy/A321.globaltech_co2capture",
      FILE = "energy/A321.demand",
      FILE = "energy/A321.globaltech_retirement",
      FILE = "socioeconomics/A321.inc_elas_output",
      FILE = "energy/A321.nonenergy_Cseq",
      "L1321.out_Mt_R_cement_Yh",
      "L1321.IO_GJkg_R_cement_F_Yh",
      "L1321.in_EJ_R_cement_F_Y",
      "L101.Pop_thous_R_Yh",
      "L102.pcgdp_thous90USD_Scen_R_Y",

      "L2323.StubTechProd_iron_steel",
      "L2233.StubTechProd_elecPassthru",
      "L2233.GlobalTechEff_elecPassthru",
      "L223.GlobalTechEff_elec")

  MODULE_OUTPUTS <-
    c("L2321.Supplysector_cement",
      "L2321.FinalEnergyKeyword_cement",
      "L2321.SubsectorLogit_cement",
      "L2321.SubsectorShrwtFllt_cement",
      "L2321.SubsectorInterp_cement",
      "L2321.StubTech_cement",
      "L2321.GlobalTechShrwt_cement",
      "L2321.GlobalTechCoef_cement",
      "L2321.GlobalTechCost_cement",
      "L2321.GlobalTechTrackCapital_cement",
      "L2321.GlobalTechCapture_cement",
      "L2321.GlobalTechShutdown_en",
      "L2321.GlobalTechSCurve_en",
      "L2321.GlobalTechLifetime_en",
      "L2321.GlobalTechProfitShutdown_en",
      "L2321.StubTechProd_cement",
      "L2321.StubTechCalInput_cement_heat",
      "L2321.StubTechCoef_cement",
      "L2321.PerCapitaBased_cement",
      "L2321.BaseService_cement",
      "L2321.PriceElasticity_cement",
      "L2321.IncomeElasticity_cement_Scen",
      "L2321.GlobalTechCSeq_ind",

      "L2321.StubTechFractSecOut",
      "L2321.StubTechFractProd",
      "L2321.StubTechFractCalPrice")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost <- PrimaryFuelCO2Coef.name <-
      PrimaryFuelCO2Coef <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <-
      L2321.IncomeElasticity_cement_Scen <- year.x <- year.y <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    # 1a. Supplysector information
    # L2321.Supplysector_cement: Supply sector information for cement sector
    A321.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2321.Supplysector_cement

    # L2321.FinalEnergyKeyword_cement: Supply sector keywords for cement sector
    A321.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2321.FinalEnergyKeyword_cement

    # 1b. Subsector information
    # L2321.SubsectorLogit_cement: Subsector logit exponents of cement sector
    A321.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2321.SubsectorLogit_cement

    # and L2321.SubsectorShrwtFllt_cement: Subsector shareweights of cement sector
    A321.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L2321.SubsectorShrwtFllt_cement

    # L2321.SubsectorInterp_cement: Subsector shareweight interpolation of cement sector
    A321.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      L2321.SubsectorInterp_cement

    # 1c. Technology information
    # L2321.StubTech_cement: Identification of stub technologies of cement
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A321.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2321.StubTech_cement

    # L2321.GlobalTechShrwt_cement: Shareweights of global cement technologies
    A321.globaltech_shrwt %>%
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
      L2321.GlobalTechShrwt_cement

    # L2321.GlobalTechCoef_cement: Energy inputs and coefficients of cement technologies
    A321.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2321.GlobalTechCoef_cement

    # Carbon capture rates from technologies with CCS
    # L2321.GlobalTechCapture_cement: CO2 capture fractions from global cement production technologies with CCS
    # No need to consider historical periods or intermittent technologies here
    A321.globaltech_co2capture %>%
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
      L2321.GlobalTechCapture_cement

    # L2321.GlobalTechCost_cement: Non-energy costs of global cement manufacturing technologies
    A321.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L2321.GlobalTechCost_cement # intermediate tibble

    # Note: adjusting non-energy costs of technologies with CCS to include CO2 capture costs
    # update, CCS non-energy cost is now reflected in .csv file
    #       The additional CCS-related non-energy costs are not included in the global technology assessment.
    #       Calculate here in two steps:
    #       (1) calculate the additional CCS costs per unit of carbon produced in 1975$
    #       (2) calculate the quantity of CO2 produced per unit of cement produced (in kgC per kg cement)
    cement_CCS_cost_total_1975USDtC <- energy.CEMENT_CCS_COST_2000USDTCO2 * gdp_deflator(1975, base_year = 2000) * emissions.CONV_C_CO2
    CO2_storage_cost_1975USDtC <- energy.CO2_STORAGE_COST_1990_USDTC * gdp_deflator(1975, base_year = 1990)
    cement_CCS_cost_1975USDtC <- cement_CCS_cost_total_1975USDtC - CO2_storage_cost_1975USDtC

    L2321.GlobalTechCapture_cement %>%
      pull(remove.fraction) %>%
      mean -> cement_CO2_capture_frac

    L2321.GlobalTechCoef_cement %>%
      filter(minicam.energy.input == "limestone") %>%
      pull(coefficient) %>%
      mean ->
      coef_mean # temporary value

    A_PrimaryFuelCCoef %>%
      filter(PrimaryFuelCO2Coef.name == "limestone") %>%
      pull(PrimaryFuelCO2Coef) %>%
      mean ->
      PrimaryFuelCO2Coef_mean # temporary value

    CO2_IO_kgCkgcement <- coef_mean * PrimaryFuelCO2Coef_mean
    CO2stored_IO_kgCkgcement <- CO2_IO_kgCkgcement * cement_CO2_capture_frac
    cement_CCS_cost_75USD_tcement <- cement_CCS_cost_1975USDtC * CO2stored_IO_kgCkgcement / CONV_T_KG

    # Adjust the non-energy costs in the table for model input
    L2321.GlobalTechCost_cement %>%
      filter(technology %in% L2321.GlobalTechCapture_cement[["technology"]]) %>%
      mutate(input.cost = input.cost + cement_CCS_cost_75USD_tcement) %>%
      bind_rows(filter(L2321.GlobalTechCost_cement, !(technology %in% L2321.GlobalTechCapture_cement[["technology"]]))) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
      L2321.GlobalTechCost_cement

    A321.nonenergy_Cseq %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCSeq"]]) ->
      L2321.GlobalTechCSeq_ind

    FCR <- (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS) /
      ((1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS -1)
    L2321.GlobalTechCost_cement %>%
      # we need to track investments in "energy" only, cement is technically materials
      filter(sector.name == "process heat cement" | sector.name == "process heat cement ccs") %>%
      mutate(capital.coef = socioeconomics.INDUSTRY_CAPITAL_RATIO / FCR,
             tracking.market = socioeconomics.EN_CAPITAL_MARKET_NAME,
             # vintaging is active in cement so no need for depreciation
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']]) ->
      L2321.GlobalTechTrackCapital_cement

    # Calibration and region-specific data
    # L2321.StubTechProd_cement: calibrated cement production
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1321.out_Mt_R_cement_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = c("sector"="supplysector","subsector","technology")) %>%
      mutate(supplysector = sector,
			       stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]])  ->
      L2321.StubTechProd_cement

    # L2321.StubTechCoef_cement: region-specific coefficients of cement production technologies
    # Take this as a given in all years for which data is available
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct %>%
      filter(!is.na(minicam.energy.input))->
      calibrated_techs_export # temporary tibble

    L1321.IO_GJkg_R_cement_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS[HISTORICAL_YEARS %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)]) %>%
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = c("sector"="supplysector", "subsector","technology","fuel")) %>%
      mutate(supplysector = sector,
			       stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2321.StubTechCoef_cement

    # Carry forward last historical year coefs to future years to avoid sharp discontinuities when jumping to globaltech values.
    # We may want to add assumptions about future improvement rates in the future but for now we hold fixed.
    L2321.StubTechCoef_cement_fut <- L2321.StubTechCoef_cement %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      group_by(region,supplysector,subsector,stub.technology,minicam.energy.input,market.name) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    L2321.StubTechCoef_cement <- L2321.StubTechCoef_cement %>%
      bind_rows(L2321.StubTechCoef_cement_fut)

    # L2321.StubTechCalInput_cement_heat: calibrated cement production
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct %>%
      filter(!is.na(minicam.energy.input))->
      calibrated_techs_export # temporary tibble

    L1321.in_EJ_R_cement_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = c("sector"="supplysector", "subsector","technology","fuel")) %>%
      # This table should only be the technologies for producing heat - drop the electricity inputs to the cement production technology
      rename(supplysector = sector) %>%
      filter(!(supplysector %in% L2321.StubTechCoef_cement[["supplysector"]])) %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2321.StubTechCalInput_cement_heat

    # L2321.PerCapitaBased_cement: per-capita based flag for cement exports final demand
    A321.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names) ->
      L2321.PerCapitaBased_cement

    # L2321.BaseService_cement: base-year service output of cement
    L2321.StubTechProd_cement %>%
      filter(supplysector == "cement") %>%
      group_by(region,year,supplysector) %>%
      summarise(base.service = sum(calOutputValue)) %>%
      ungroup() %>%
      select(region, year, base.service, energy.final.demand = supplysector) ->
      L2321.BaseService_cement


    # Retirement information
    A321.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2321.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L2321.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2321.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2321.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      bind_rows(L2321.globaltech_retirement_future) ->
      L2321.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2321.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L2321.globaltech_retirement$shutdown.rate))) {
      L2321.globaltech_retirement %>%
        filter(!is.na(L2321.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L2321.GlobalTechShutdown_en
    }

    if(any(!is.na(L2321.globaltech_retirement$half.life))) {
      L2321.globaltech_retirement %>%
        filter(!is.na(L2321.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2321.GlobalTechSCurve_en
    }

    # L2321.GlobalTechLifetime_en: Global tech lifetime
    if(any(is.na(L2321.globaltech_retirement$shutdown.rate) & is.na(L2321.globaltech_retirement$half.life))) {
      L2321.globaltech_retirement %>%
        filter(is.na(L2321.globaltech_retirement$shutdown.rate) & is.na(L2321.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L2321.GlobalTechLifetime_en
    }

    # L2321.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2321.globaltech_retirement$median.shutdown.point))) {
      L2321.globaltech_retirement %>%
        filter(!is.na(L2321.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2321.GlobalTechProfitShutdown_en
    }
    # L2321.PriceElasticity_cement: price elasticity
    A321.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2321.PriceElasticity_cement

    # L2321.IncomeElasticity_cement_scen: income elasticity of cement (scenario-specific)
    # First, calculate the per-capita GDP pathways of every GDP scenario and combine
  L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
      # Per-capita GDP ratios, which are used in the equation for demand growth
      group_by(GCAM_region_ID, scenario) %>%
      mutate(temp_lag = lag(value, 1),
             value = value / temp_lag) %>%
      ungroup %>%
      select(-temp_lag) %>%
      filter(year %in% MODEL_FUTURE_YEARS) ->
      L2321.pcgdpRatio_ALL_R_Y # intermediate tibble

    # Calculate the cement output as the base-year cement output times the GDP ratio raised to the income elasticity
    # The income elasticity is looked up based on the prior year's output
    L2321.pcgdpRatio_ALL_R_Y %>%
      select(GCAM_region_ID, scenario) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L2321.BaseService_cement, by = c("year", "region")) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("year", "GCAM_region_ID")) %>%
      mutate(value = base.service * CONV_MIL_THOUS / value) %>%
      select(-base.service, -energy.final.demand) ->
      L2321.Output_cement # intermediate tibble

    # At each time, the output is equal to the prior period's output times the GDP ratio, raised to the elasticity
    # that corresponds to the output that was observed in the prior time period. This method prevents (ideally) runaway
    # production/consumption.
    elast_years <- c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)
    for(i in seq_along(elast_years)[-1]) {
      L2321.Output_cement %>%
        filter(year == elast_years[i - 1]) %>%
        # strick left join fails timeshift test due to NAs in L102.pcgdp_thous90USD_Scen_R_Y under timeshift mode
        left_join(filter(L2321.pcgdpRatio_ALL_R_Y, year == elast_years[i]), by = c("GCAM_region_ID", "scenario")) ->
        intermediate

      intermediate %>%
        mutate(parameter = approx(x = A321.inc_elas_output[["pc.output_t"]],
                                  y = A321.inc_elas_output[["inc_elas"]],
                                  xout = intermediate[["value.x"]],
                                  rule = 2)[['y']],
               value = value.x * value.y ^ parameter,
               year = elast_years[i]) %>%
        select(GCAM_region_ID, scenario, region, year, value) %>%
        bind_rows(L2321.Output_cement) ->
        L2321.Output_cement
    }

    # Now that we have cement output, we can back out the appropriate income elasticities
    L2321.Output_cement %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(value = approx( x = A321.inc_elas_output[["pc.output_t"]],
                             y = A321.inc_elas_output[["inc_elas"]],
                             xout = value, rule = 2)[["y"]],
             value = round(value, energy.DIGITS_INCELAS_IND)) %>%
      rename(income.elasticity = value) %>%
      mutate(energy.final.demand = A321.demand[["energy.final.demand"]]) ->
      L2321.IncomeElasticity_cement_Scen # intermediate tibble


    #create markets for waste GBFS and fly ash used in cement production

    GBFS_price <- 0.017905458 # $1975/kg; replace with A10 resource value
    FA_price <- 0.003442343 # $1975/kg; replace with A10 resource value

    L2323.StubTechProd_iron_steel %>% filter(str_detect(subsector,"BLASTFUR")) %>%
      group_by(region,supplysector,subsector,year) %>%
      summarize(calOutputValue = sum(calOutputValue)) %>%
      ungroup() -> blastfurnace_prod

    L2323.StubTechProd_iron_steel %>% filter(str_detect(subsector,"BLASTFUR")) %>%
      distinct(stub.technology) -> blastfur_techs


    GBFS_input_cement <- L2321.StubTechProd_cement %>%
      filter(stub.technology == "cement SCMGBFS") %>%
      left_join_error_no_match(L2321.GlobalTechCoef_cement %>%
                                 rename(stub.technology = technology,
                                        supplysector = sector.name,
                                        subsector = subsector.name) %>%
                                 filter(minicam.energy.input == "GBFSlag",
                                        year %in% c(MODEL_BASE_YEARS)), by = c("supplysector","subsector","stub.technology","year")) %>%
      mutate(GBFS_input = calOutputValue * coefficient) %>%
      select(region,year,GBFS_input)


   blastfurnace_prod %>%
      left_join_error_no_match(GBFS_input_cement, by = c("region","year")) %>%
      mutate(output.ratio = GBFS_input / calOutputValue) %>%
      group_by(year) %>%
      mutate(output.ratio = if_else(calOutputValue == 0,0,output.ratio), # first set NAN output ratios to zero so they don't mess up weighted sum
             output.ratio = if_else(calOutputValue == 0, sum(calOutputValue * output.ratio) / sum(calOutputValue), output.ratio),
             fractional.secondary.output = "GBFSlag") %>%
     ungroup() -> L2321.StubTechFractSecOut_GBFS_hist

   L2321.StubTechFractSecOut_GBFS_hist %>%
      ungroup() %>%
      repeat_add_columns(tibble(stub.technology = unique(blastfur_techs$stub.technology))) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFractSecOut"]]) %>%
      complete(nesting(region, supplysector, subsector, stub.technology,fractional.secondary.output),
               year = c(unique(year), MODEL_FUTURE_YEARS)) %>%
     group_by(region, supplysector, subsector, stub.technology,fractional.secondary.output) %>%
     mutate(output.ratio = approx_fun(year, output.ratio, rule = 2)) %>%
     ungroup() -> L2321.StubTechFractSecOut_GBFS

   GBFS_Baseyear_outputRatio <- L2321.StubTechFractSecOut_GBFS_hist %>%
     filter(year == MODEL_FINAL_BASE_YEAR) %>%
     distinct(region, subsector, .keep_all = TRUE) %>%
     summarize(output.ratio = sum(calOutputValue * output.ratio) / sum(calOutputValue))

   GBFS_outputRatio_fut <- GBFS_Baseyear_outputRatio[[1]]

   #Set future GBFS secondary out
   L2321.StubTechFractSecOut_GBFS <- L2321.StubTechFractSecOut_GBFS %>%
     mutate(output.ratio = if_else(year > MODEL_FINAL_BASE_YEAR,GBFS_outputRatio_fut, output.ratio))

   L2321.StubTechFractProd_GBFS <- L2321.StubTechFractSecOut_GBFS %>%
     repeat_add_columns(tibble(fraction.produced = c(0,1))) %>%
     mutate(price = if_else(fraction.produced == 0, 0, GBFS_price)) %>%
     select(LEVEL2_DATA_NAMES[["StubTechFractProd"]]) %>%
     add_precursors("L2323.StubTechProd_iron_steel")


   L2321.StubTechFractCalPrice_GBFS <- L2321.StubTechFractSecOut_GBFS %>%
     mutate(calPrice = GBFS_price) %>%
     filter(year %in% c(MODEL_BASE_YEARS)) %>%
     select(LEVEL2_DATA_NAMES[["StubTechFractCalPrice"]]) %>%
     add_precursors("L2323.StubTechProd_iron_steel")


   no_blastfur_prod_region_years <- blastfurnace_prod %>%
     filter(calOutputValue == 0) %>%
     select(region,year,calOutputValue)

   L2321.StubTechCoef_GBFS <- L2321.GlobalTechCoef_cement %>%
     rename(supplysector = sector.name,
            subsector = subsector.name,
            stub.technology = technology) %>%
     filter(minicam.energy.input == "GBFSlag") %>%
     write_to_all_regions(LEVEL2_DATA_NAMES[["StubTechCoef"]], GCAM_region_names) %>%
     left_join(no_blastfur_prod_region_years, by = c("region","year")) %>%
     mutate(minicam.energy.input = if_else(!is.na(calOutputValue) & calOutputValue == 0, "GBFSlag calibration", "GBFSlag")) %>%
     select(LEVEL2_DATA_NAMES[["StubTechCoef"]])


   #Fly Ash Calibration

   L2233.StubTechProd_elecPassthru %>%
     filter(subsector == "coal") -> coal_elec

   FA_input_cement <- L2321.StubTechProd_cement %>%
     filter(stub.technology == "cement SCMFA") %>%
     left_join_error_no_match(L2321.GlobalTechCoef_cement %>%
                                rename(stub.technology = technology,
                                       supplysector = sector.name,
                                       subsector = subsector.name) %>%
                                filter(minicam.energy.input == "flyash",
                                       year %in% c(MODEL_BASE_YEARS)), by = c("supplysector","subsector","stub.technology","year")) %>%
     mutate(FA_input = calOutputValue * coefficient) %>%
     select(region,year,FA_input)

   coal_elec %>%
     left_join_error_no_match(FA_input_cement, by = c("region","year")) %>%
     mutate(output.ratio = FA_input / calOutputValue) %>%
     group_by(year) %>%
     mutate(output.ratio = if_else(calOutputValue == 0,0,output.ratio), # first set NAN output ratios to zero so they don't mess up weighted sum
            output.ratio = if_else(calOutputValue == 0, sum(calOutputValue * output.ratio) / sum(calOutputValue), output.ratio),
            fractional.secondary.output = "flyash") %>%
     ungroup() -> L2321.StubTechFractSecOut_FA_hist

   coal_elec_techs <- L2233.GlobalTechEff_elecPassthru %>%
     filter(subsector.name == "coal") %>%
     distinct(technology)

   L2321.StubTechFractSecOut_FA_hist %>%
     select(-stub.technology) %>%
     repeat_add_columns(tibble(stub.technology = unique(coal_elec_techs$technology))) %>%
     select(LEVEL2_DATA_NAMES[["StubTechFractSecOut"]]) %>%
     complete(nesting(region, supplysector, subsector, stub.technology,fractional.secondary.output),
              year = c(unique(year), MODEL_FUTURE_YEARS)) %>%
     group_by(region, supplysector, subsector, stub.technology,fractional.secondary.output) %>%
     mutate(output.ratio = approx_fun(year, output.ratio, rule = 2)) %>%
     ungroup() %>%
     add_precursors("L2233.StubTechProd_elecPassthru","L2233.GlobalTechEff_elecPassthru") -> L2321.StubTechFractSecOut_FA

   FA_Baseyear_outputRatio <- L2321.StubTechFractSecOut_FA_hist %>%
     filter(year == MODEL_FINAL_BASE_YEAR) %>%
     distinct(region, subsector, .keep_all = TRUE) %>%
     summarize(output.ratio = sum(calOutputValue * output.ratio) / sum(calOutputValue))

   FA_outputRatio_fut <- FA_Baseyear_outputRatio[[1]]


   L2321.StubTechFractSecOut_FA <- L2321.StubTechFractSecOut_FA %>%
     mutate(output.ratio = if_else(year > MODEL_FINAL_BASE_YEAR,FA_outputRatio_fut, output.ratio)) %>%
     left_join(L2233.GlobalTechEff_elecPassthru %>% filter(subsector.name == "coal"),
                              by = c("supplysector" = "sector.name","subsector" = "subsector.name", "stub.technology" = "technology","year")) %>%
     select(-efficiency) %>%
     mutate(minicam.energy.input = stringr::str_replace(minicam.energy.input, "elec_", "")) %>%
     distinct(region, supplysector, subsector, stub.technology, year, .keep_all = TRUE) %>%
     left_join(L223.GlobalTechEff_elec %>%
                 filter(subsector.name == "coal") %>%
                 select(technology,year,efficiency),
               by = c("minicam.energy.input" = "technology","year")) %>%
     group_by(region,supplysector,subsector) %>%
     # More efficient power plants in the future will use less coal and correspondingly produce less fly ash
     # However, efficiency losses for CCS would imply more coal use and therefore more fly ash production.
     # Therefore we scale future output.ratio by efficiency relative to final base year conventional coal
     mutate(output.ratio = if_else(year <= MODEL_FINAL_BASE_YEAR, output.ratio, output.ratio * efficiency[year == MODEL_FINAL_BASE_YEAR & stub.technology == "coal (conv pul)"] / efficiency)) %>%
     ungroup() %>%
     add_precursors("L223.GlobalTechEff_elec")

   L2321.StubTechFractProd_FA <- L2321.StubTechFractSecOut_FA %>%
     repeat_add_columns(tibble(fraction.produced = c(0,1))) %>%
     mutate(price = if_else(fraction.produced == 0, 0, FA_price)) %>%
     select(LEVEL2_DATA_NAMES[["StubTechFractProd"]]) %>%
     add_precursors("L2233.StubTechProd_elecPassthru")


   L2321.StubTechFractCalPrice_FA <- L2321.StubTechFractSecOut_FA %>%
     mutate(calPrice = FA_price) %>%
     filter(year %in% c(MODEL_BASE_YEARS)) %>%
     select(LEVEL2_DATA_NAMES[["StubTechFractCalPrice"]]) %>%
     add_precursors("L2233.StubTechProd_elecPassthru")


   no_coal_elec_region_years <- coal_elec %>%
     filter(calOutputValue == 0) %>%
     select(region,year,calOutputValue)

   L2321.StubTechCoef_FA <- L2321.GlobalTechCoef_cement %>%
     rename(supplysector = sector.name,
            subsector = subsector.name,
            stub.technology = technology) %>%
     filter(minicam.energy.input == "flyash") %>%
     write_to_all_regions(LEVEL2_DATA_NAMES[["StubTechCoef"]], GCAM_region_names) %>%
     left_join(no_coal_elec_region_years, by = c("region","year")) %>%
     mutate(minicam.energy.input = if_else(!is.na(calOutputValue) & calOutputValue == 0, "flyash calibration", "flyash")) %>%
     select(LEVEL2_DATA_NAMES[["StubTechCoef"]])


  # remove flyash and GBFS from globaltech database and replace with stubtechs so we don't cause issues in cases where we need the respective calibration resources

   L2321.GlobalTechCoef_cement <- L2321.GlobalTechCoef_cement %>%
     filter(!(minicam.energy.input %in% c("GBFSlag","flyash")))

   L2321.StubTechCoef_cement <- L2321.StubTechCoef_cement %>%
     bind_rows(L2321.StubTechCoef_GBFS, L2321.StubTechCoef_FA)

   L2321.StubTechFractSecOut <- bind_rows(L2321.StubTechFractSecOut_GBFS,L2321.StubTechFractSecOut_FA) %>%
     add_precursors("L2323.StubTechProd_iron_steel","L2233.StubTechProd_elecPassthru")
   L2321.StubTechFractCalPrice <- bind_rows(L2321.StubTechFractCalPrice_GBFS,L2321.StubTechFractCalPrice_FA)
   L2321.StubTechFractProd <- bind_rows(L2321.StubTechFractProd_GBFS,L2321.StubTechFractProd_FA)

    # ===================================================
    # Produce outputs

    # Extract scenario data and assign to separate tables
    L2321.IncomeElasticity_cement_Scen %>%
        add_title(paste("Income elasticity of cement - SSPs")) %>%
        add_units("Unitless") %>%
        add_comments("First calculate cement output as the base-year cement output times the GDP ratio raised to the income elasticity") %>%
        add_comments("Then back out the appropriate income elasticities from cement output") %>%
        add_legacy_name("L2321.IncomeElasticity_cement_Scen") %>%
        add_precursors("L101.Pop_thous_R_Yh", "L102.pcgdp_thous90USD_Scen_R_Y", "common/GCAM_region_names", "energy/A321.demand", "energy/calibrated_techs",
                       "L1321.out_Mt_R_cement_Yh", "socioeconomics/A321.inc_elas_output") ->
      L2321.IncomeElasticity_cement_Scen

    L2321.Supplysector_cement %>%
      add_title("Supply sector information for cement sector") %>%
      add_units("NA") %>%
      add_comments("For cement sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A321.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2321.Supplysector_cement") %>%
      add_precursors("energy/A321.sector", "common/GCAM_region_names") ->
      L2321.Supplysector_cement

    L2321.FinalEnergyKeyword_cement %>%
      add_title("Supply sector keywords for cement sector") %>%
      add_units("NA") %>%
      add_comments("For cement sector, the supply sector final energy keywords from A321.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2321.FinalEnergyKeyword_cement") %>%
      add_precursors("energy/A321.sector", "common/GCAM_region_names") ->
      L2321.FinalEnergyKeyword_cement

    L2321.SubsectorLogit_cement %>%
      add_title("Subsector logit exponents of cement sector") %>%
      add_units("Unitless") %>%
      add_comments("For cement sector, the subsector logit exponents from A321.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2321.SubsectorLogit_cement") %>%
      add_precursors("energy/A321.subsector_logit", "common/GCAM_region_names") ->
      L2321.SubsectorLogit_cement

    L2321.SubsectorShrwtFllt_cement %>%
      add_title("Subsector shareweights of cement sector") %>%
      add_units("unitless") %>%
      add_comments("For cement sector, the subsector shareweights from A321.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2321.SubsectorShrwtFllt_cement") %>%
      add_precursors("energy/A321.subsector_shrwt", "common/GCAM_region_names") ->
      L2321.SubsectorShrwtFllt_cement

    L2321.SubsectorInterp_cement %>%
      add_title("Subsector shareweight interpolation of cement sector") %>%
      add_units("NA") %>%
      add_comments("For cement sector, the subsector shareweight interpolation function infromation from A321.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2321.SubsectorInterp_cement") %>%
      add_precursors("energy/A321.subsector_interp", "common/GCAM_region_names") ->
      L2321.SubsectorInterp_cement

    L2321.StubTech_cement %>%
      add_title("Identification of stub technologies of cement") %>%
      add_units("NA") %>%
      add_comments("For cement sector, the stub technologies from A321.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2321.StubTech_cement") %>%
      add_precursors("energy/A321.globaltech_shrwt", "common/GCAM_region_names") ->
      L2321.StubTech_cement

    L2321.GlobalTechShrwt_cement %>%
      add_title("Shareweights of global cement technologies") %>%
      add_units("Unitless") %>%
      add_comments("For cement sector, the share weights from A321.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2321.GlobalTechShrwt_cement") %>%
      add_precursors("energy/A321.globaltech_shrwt") ->
      L2321.GlobalTechShrwt_cement

    L2321.GlobalTechCoef_cement %>%
      add_title("Energy inputs and coefficients of cement technologies") %>%
      add_units("limestone input is unitless (Mt limestone per Mt cement); all others are GJ per kg (EJ of energy per Mt of cement)") %>%
      add_comments("For cement sector, the energy use coefficients from A321.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2321.GlobalTechCoef_cement") %>%
      add_precursors("energy/A321.globaltech_coef") ->
      L2321.GlobalTechCoef_cement

    L2321.GlobalTechCost_cement %>%
      add_title("Non-energy costs of global cement manufacturing technologies") %>%
      add_units("1975$/kg for supplysector cement; 1975$/GJ for supplysector process heat cement") %>%
      add_comments("For cement sector, the Non-energy costs of global cement manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_legacy_name("L2321.GlobalTechCost_cement") %>%
      add_precursors("energy/A321.globaltech_cost", "energy/A321.globaltech_co2capture", "energy/A321.globaltech_coef", "emissions/A_PrimaryFuelCCoef") ->
      L2321.GlobalTechCost_cement

    L2321.GlobalTechTrackCapital_cement %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      same_precursors_as(L2321.GlobalTechCost_cement) ->
      L2321.GlobalTechTrackCapital_cement

    L2321.GlobalTechCSeq_ind %>%
      add_title("CO2 capture fractions from global electricity generation technologies") %>%
      add_units("Unitless") %>%
      add_comments("Remove fractions from A321.nonenergy_Cseq are expanded into all model years") %>%
      add_legacy_name("L2321.GlobalTechCSeq_ind") %>%
      add_precursors("energy/A321.nonenergy_Cseq") ->
      L2321.GlobalTechCSeq_ind

    L2321.GlobalTechCapture_cement %>%
      add_title("CO2 capture fractions from global cement production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For cement sector, the remove fractions from A321.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2321.GlobalTechCapture_cement") %>%
      add_precursors("energy/A321.globaltech_co2capture") ->
      L2321.GlobalTechCapture_cement

    L2321.StubTechProd_cement %>%
      add_title("calibrated cement production") %>%
      add_units("Mt") %>%
      add_comments("Values are calculated using L1321.out_Mt_R_cement_Yh then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2321.StubTechProd_cement") %>%
      add_precursors("energy/calibrated_techs", "L1321.out_Mt_R_cement_Yh", "common/GCAM_region_names") ->
      L2321.StubTechProd_cement

    L2321.StubTechCalInput_cement_heat %>%
      add_title("calibrated cement production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1321.in_EJ_R_cement_F_Y then added GCAM region information and supplysector, subsector, technology, and input information") %>%
      add_legacy_name("L2321.StubTechCalInput_cement_heat") %>%
      add_precursors("energy/calibrated_techs", "L1321.in_EJ_R_cement_F_Y", "common/GCAM_region_names") ->
      L2321.StubTechCalInput_cement_heat

    L2321.StubTechCoef_cement %>%
      add_title("region-specific coefficients of cement production technologies") %>%
      add_units("limestone input is unitless (Mt limestone per Mt cement); all others are GJ per kg (EJ of energy per Mt of cement)") %>%
      add_comments("Coefficients are calculated using L1321.IO_GJkg_R_cement_F_Yh") %>%
      add_legacy_name("L2321.StubTechCoef_cement") %>%
      add_precursors("energy/calibrated_techs", "L1321.IO_GJkg_R_cement_F_Yh", "common/GCAM_region_names") ->
      L2321.StubTechCoef_cement

    L2321.PerCapitaBased_cement %>%
      add_title("per-capita based flag for cement exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for cement from A321.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2321.PerCapitaBased_cement") %>%
      add_precursors("energy/A321.demand", "common/GCAM_region_names") ->
      L2321.PerCapitaBased_cement


    if(exists("L2321.GlobalTechShutdown_en")) {
      L2321.GlobalTechShutdown_en %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2321.GlobalTechShutdown_en") %>%
        add_precursors("energy/A321.globaltech_retirement") ->
        L2321.GlobalTechShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2321.GlobalTechShutdown_en") ->
        L2321.GlobalTechShutdown_en
    }

    if(exists("L2321.GlobalTechSCurve_en")) {
      L2321.GlobalTechSCurve_en %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2321.GlobalTechSCurve_en") %>%
        add_precursors("energy/A321.globaltech_retirement") ->
        L2321.GlobalTechSCurve_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2321.GlobalTechSCurve_en") ->
        L2321.GlobalTechSCurve_en
    }

    if(exists("L2321.GlobalTechLifetime_en")) {
      L2321.GlobalTechLifetime_en %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2321.GlobalTechLifetime_en") %>%
        add_precursors("energy/A321.globaltech_retirement") ->
        L2321.GlobalTechLifetime_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2321.GlobalTechLifetime_en") ->
        L2321.GlobalTechLifetime_en
    }

    if(exists("L2321.GlobalTechProfitShutdown_en")) {
      L2321.GlobalTechProfitShutdown_en %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2321.GlobalTechProfitShutdown_en") %>%
        add_precursors("energy/A321.globaltech_retirement") ->
        L2321.GlobalTechProfitShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2321.GlobalTechProfitShutdown_en") ->
        L2321.GlobalTechProfitShutdown_en
    }

    L2321.BaseService_cement %>%
      add_title("base-year service output of cement") %>%
      add_units("Mt") %>%
      add_comments("Transformed from L2321.StubTechProd_cement by adding energy.final.demand from A321.demand") %>%
      add_legacy_name("L2321.BaseService_cement") %>%
      add_precursors("energy/A321.demand", "energy/calibrated_techs", "L1321.out_Mt_R_cement_Yh", "common/GCAM_region_names") ->
      L2321.BaseService_cement

    L2321.PriceElasticity_cement %>%
      add_title("price elasticity for cement") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A321.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2321.PriceElasticity_cement") %>%
      add_precursors("energy/A321.demand", "common/GCAM_region_names") ->
      L2321.PriceElasticity_cement

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
