#' module_gcamusa_L244.building_det_USA
#'
#' Construct XML data structure for \code{building_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: .
module_gcamusa_L244.building_det_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.sector",
             "L244.GenericBaseService",
             FILE = "gcam-usa/A44.demand_satiation_mult",
             FILE = "gcam-usa/A44.globaltech_intgains",
             "L101.Pop_thous_R_Yh",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L144.flsp_bm2_state_res",
             "L144.flsp_bm2_state_comm",
             "L143.HDDCDD_scen_state",
             "L244.DeleteSupplysector_USAbld",
             "L244.PriceExp_IntGains_gcamusa",
             "L244.Satiation_flsp_gcamusa",
             "L244.SatiationAdder_gcamusa",
             "L244.ThermalBaseService_gcamusa",
             "L244.GenericBaseService_gcamusa",
             "L244.ThermalServiceSatiation_gcamusa",
             "L244.GenericServiceSatiation_gcamusa",
             "L244.Intgains_scalar_gcamusa",
             "L244.ShellConductance_bld_gcamusa",
             "L244.Supplysector_bld_gcamusa",
             "L244.FinalEnergyKeyword_bld_gcamusa",
             "L244.SubsectorShrwtFllt_bld_gcamusa",
             "L244.SubsectorInterp_bld_gcamusa",
             "L244.SubsectorInterpTo_bld_gcamusa",
             "L244.SubsectorLogit_bld_gcamusa",
             "L244.StubTech_bld_gcamusa",
             "L244.StubTechCalInput_bld_gcamusa",
             "L244.StubTechCalInput_bld",
             "L244.StubTechMarket_bld",
             "L244.GlobalTechEff_bld",
             # emissions
             FILE = "gcam-usa/A44.sector_emiss_map",
             "L201.en_pol_emissions",
             "L201.en_ghg_emissions",
             "L201.nonghg_max_reduction",
             "L201.nonghg_steepness",
             "L241.hfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units",
             "L252.MAC_higwp",
             "L252.MAC_higwp_phaseInTime",
             "L252.MAC_higwp_tc_average"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.DeleteSupplysector_USAonly",
             "L244.DeleteGenericService_USAonly",
             "L244.PriceExp_IntGains_USAonly",
             "L244.Floorspace_USAonly",
             "L244.SatiationAdder_USAonly",
             "L244.ThermalBaseService_USAonly",
             "L244.GenericBaseService_USAonly",
             "L244.ThermalServiceSatiation_USAonly",
             "L244.GenericServiceSatiation_USAonly",
             "L244.Intgains_scalar_USAonly",
             "L244.ShellConductance_bld_USAonly",
             "L244.Supplysector_bld_USAonly",
             "L244.FinalEnergyKeyword_bld_USAonly",
             "L244.SubsectorShrwtFllt_bld_USAonly",
             "L244.SubsectorInterp_bld_USAonly",
             "L244.SubsectorInterpTo_bld_USAonly",
             "L244.SubsectorLogit_bld_USAonly",
             "L244.StubTech_bld_USAonly",
             "L244.StubTechCalInput_bld_USAonly",
             "L244.StubTechMarket_bld_USAonly",
             "L244.InputEmissions_bld_pol_USAonly",
             "L244.InputEmissions_bld_ghg_USAonly",
             "L244.GDPCtrlMax_bld_ghg_USAonly",
             "L244.GDPCtrlSteep_bld_ghg_USAonly",
             "L244.StbTechOutputEmissions_bld_hfc_USAonly",
             'L244.OutputEmissCoeff_bld_hfc_future_USAonly',
             "L244.StubTechEmissUnits_bld_hfc_USAonly",
             "L244.MAC_bld_hfc_USAonly",
             "L244.MACPhaseIn_bld_hfc_USAonly",
             "L244.MACTC_bld_hfc_USAonly"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.gcam_consumer  <- get_data(all_data, "energy/A44.gcam_consumer")
    A44.sector <- get_data(all_data, "energy/A44.sector")
    A44.sector_emiss_map <- get_data(all_data, "gcam-usa/A44.sector_emiss_map")
    # A44.sector_emiss_map <- read.csv("./inst/extdata/gcam-usa/A44.sector_emiss_map.csv", comment.char = "#")

    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService")
    A44.demand_satiation_mult <- get_data(all_data, "gcam-usa/A44.demand_satiation_mult")
    A44.globaltech_intgains <- get_data(all_data, "gcam-usa/A44.globaltech_intgains")

    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L102.pcgdp_thous90USD_Scen_R_Y  <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    L144.flsp_bm2_state_res  <- get_data(all_data, "L144.flsp_bm2_state_res")
    L144.flsp_bm2_state_comm  <- get_data(all_data, "L144.flsp_bm2_state_comm")
    L143.HDDCDD_scen_state <- get_data(all_data, "L143.HDDCDD_scen_state")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp_gcamusa")

    L244.DeleteSupplysector_USAbld <- get_data(all_data, "L244.DeleteSupplysector_USAbld")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains_gcamusa")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder_gcamusa")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService_gcamusa")
    L244.GenericBaseService_gcamusa <- get_data(all_data, "L244.GenericBaseService_gcamusa")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation_gcamusa")
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation_gcamusa")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar_gcamusa")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld_gcamusa")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld_gcamusa")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld_gcamusa")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamusa")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld_gcamusa")
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamusa")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld_gcamusa")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld_gcamusa")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld_gcamusa")
    L244.StubTechCalInput_bld_USA <- get_data(all_data, "L244.StubTechCalInput_bld") %>%
      filter(region == gcam.USA_REGION)
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_bld")

    # # Add these to batch, no need to recreate here
    # L244.GlobalTechIntGainOutputRatio <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio")
    # L244.GlobalTechInterpTo_bld <- get_data(all_data, "L244.GlobalTechInterpTo_bld")
    # L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamusa")
    # L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld_gcamusa")
    # L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")
    # L244.GlobalTechSCurve_bld <- get_data(all_data, "L244.GlobalTechSCurve_bld")

    # Emissions
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions")
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions")
    L201.nonghg_max_reduction <- get_data(all_data, "L201.nonghg_max_reduction")
    L201.nonghg_steepness <- get_data(all_data, "L201.nonghg_steepness")
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.hfc_future <- get_data(all_data, "L241.hfc_future")
    L241.fgas_all_units <- get_data(all_data, "L241.fgas_all_units")
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp")
    L252.MAC_higwp_phaseInTime <- get_data(all_data, "L252.MAC_higwp_phaseInTime")
    L252.MAC_higwp_tc_average <- get_data(all_data, "L252.MAC_higwp_tc_average")


    # ===================================================
    # Data Processing

    L244.DeleteSupplysector_USAbld %>%
      # filter(!grepl("others", supplysector)) %>%
      mutate(drop = "drop") %>%
      select(-drop) -> L244.DeleteSupplysector_USAonly

    L244.GenericBaseService %>%
      filter(region == gcam.USA_REGION,
             grepl("others", building.service.input)) %>%
      mutate(supplysector = building.service.input) %>%
      select(LEVEL2_DATA_NAMES[["DeleteGenericService"]]) -> L244.DeleteGenericService_USAonly

    L244.PriceExp_IntGains %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["PriceExp_IntGains"]]) -> L244.PriceExp_IntGains_USAonly

    # Heating and cooling degree days (thermal services only)
    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.globaltech_intgains$supplysector)
    thermal_services <- setdiff(unique(A44.sector$supplysector), generic_services)

    # Residential floorspace
    L144.flsp_bm2_state_res %>%
      group_by(sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(base.building.size = value,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]]) -> L244.Floorspace_resid

    # Commercial floorspace
    L144.flsp_bm2_state_comm %>%
      group_by(sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(base.building.size = value,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]]) -> L244.Floorspace_comm

    L244.Floorspace_USAonly <- bind_rows(L244.Floorspace_resid, L244.Floorspace_comm) %>%
      filter(year %in% MODEL_BASE_YEARS)


    # L244.SatiationAdder_gcamusa: Satiation adders in floorspace demand function
    # Required for shaping the future floorspace growth trajectories in each region
    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)

    # We will filter GDP to energy.SATIATION_YEAR, but this may be greater than the historical years present
    # under timeshift conditions. So we adjust energy.SATIATION_YEAR
    energy.SATIATION_YEAR <- min(max(MODEL_BASE_YEARS), energy.SATIATION_YEAR)

    L244.pcGDP_gSSP2_USA <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(scenario == "gSSP2",
             GCAM_region_ID == gcam.USA_CODE,
             year %in% HISTORICAL_YEARS) %>%
      rename(pcGDP = value) %>%
      select(-scenario) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh %>%
                                 filter(year %in% HISTORICAL_YEARS) %>%
                                 # filter(year == energy.SATIATION_YEAR) %>%
                                 rename(pop = value),
                               by = c("GCAM_region_ID", "year"))

    L244.SatiationAdder_USAonly <- L244.Satiation_flsp %>%
      # Summarize to USA level
      group_by(gcam.consumer, nodeInput, building.node.input) %>%
      summarise(satiation.level = median(satiation.level)) %>%
      ungroup() %>%
      mutate(region = gcam.USA_REGION,
             year = energy.SATIATION_YEAR) %>%
      # Add GDP & population
      left_join_error_no_match(L244.pcGDP_gSSP2_USA, by = c("year")) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_USAonly, by = c("region", "gcam.consumer", "year", "nodeInput", "building.node.input")) %>%
      # Calculate per capita floorspace
      mutate(pcFlsp_mm2 = base.building.size / pop,
             # Calculate the satiation adders
             satiation.adder = round(satiation.level - (
               exp(log(2) * pcGDP / energy.GDP_MID_SATIATION) * (satiation.level - pcFlsp_mm2)),
               energy.DIGITS_SATIATION_ADDER),
             # The satiation adder (million square meters of floorspace per person) needs to be less than the per-capita demand in the final calibration year
             satiation.adder = if_else(satiation.adder > pcFlsp_mm2, pcFlsp_mm2 * 0.999, satiation.adder)) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])


    L244.ThermalBaseService %>%
      group_by(gcam.consumer, nodeInput, building.node.input, thermal.building.service.input, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]]) -> L244.ThermalBaseService_USAonly

    L244.GenericBaseService_gcamusa %>%
      group_by(gcam.consumer, nodeInput, building.node.input, building.service.input, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]]) -> L244.GenericBaseService_USAonly

    L244.StubTechCalInput_bld %>%
      select(-region) %>%
      mutate(region = gcam.USA_REGION) %>%
      group_by(region, supplysector, subsector, stub.technology, year, minicam.energy.input, share.weight.year) %>%
      summarise(calibrated.value = sum(calibrated.value),
                subs.share.weight = max(subs.share.weight),
                tech.share.weight = max(tech.share.weight)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) -> L244.StubTechCalInput_bld_USAonly

    # # GCAM-USA energy flows do not include energy for water (EFW), while GCAM-32 region does
    # # Thus, summing up GCAM-USA caloutputvalues, as above, causes calibration issues (related to electricity)
    # # Scale energy consumption to match USA region totals
    # L244.StubTechCalInput_bld_USA %>%
    #   group_by(region, year, minicam.energy.input) %>%
    #   summarise(core_cons = sum(calibrated.value)) %>%
    #   ungroup() -> L244.StubTechCalInput_bld_USA
    #
    # L244.StubTechCalInput_bld_USAonly %>%
    #   group_by(region, year, minicam.energy.input) %>%
    #   mutate(gcamusa_cons = sum(calibrated.value)) %>%
    #   ungroup() %>%
    #   left_join_error_no_match(L244.StubTechCalInput_bld_USA,
    #                            by = c("region", "year", "minicam.energy.input")) %>%
    #   mutate(calibrated.value = calibrated.value * core_cons / gcamusa_cons) %>%
    #   select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) -> L244.StubTechCalInput_bld_USAonly

    # L244.GenericServiceSatiation: Satiation levels assumed for non-thermal building services
    # Just multiply the base-service by an exogenous multiplier
    L244.GenericServiceSatiation_USAonly <- L244.GenericBaseService_USAonly %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_USAonly, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # L244.ThermalServiceSatiation: Satiation levels assumed for thermal building services
    L244.ThermalServiceSatiation_USAonly <- L244.ThermalBaseService_USAonly %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_USAonly, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("thermal.building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    # L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    scalar <- c(energy.INTERNAL_GAINS_SCALAR_USA_H, energy.INTERNAL_GAINS_SCALAR_USA_C)
    DDnorm <- c(gcamusa.BASE_HDD_USA, gcamusa.BASE_CDD_USA)
    US.base.scalar <- tibble(variable, scalar, DDnorm)
    threshold_HDD <- 500

    # L244.HDDCDD: Heating and cooling degree days by scenario
    L244.HDDCDD_scen_state <- L143.HDDCDD_scen_state %>%
      rename(region = state,
             degree.days = value)

    # Subset the heating and cooling services, separately
    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    # Let's make a climate normal (historical average) for each region, using a selected interval of years
    # Don't want to just set one year, because we want average values for all regions
    L244.HDDCDD_normal_USA <- L244.HDDCDD_scen_state %>%
      filter(year %in% seq(1981, 2000),
             # The AEO_2015 scenario changes this "normal climate" for each region,
             # which is not desirable since it does not incldue historical data
             # and is not the standard reference assumption.  Thus, we remove it
             # from this calculation.
             Scen != "AEO_2015") %>%
      group_by(variable) %>%
      summarise(degree.days = mean(degree.days)) %>%
      ungroup() %>%
      mutate(region = gcam.USA_REGION)

    L244.Intgains_scalar_USAonly <- L244.ThermalServiceSatiation_USAonly %>%
      # Assign HDD or CDD
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add DDnorm & scalar
      left_join_error_no_match(US.base.scalar, by = "variable") %>%
      # Add degree days
      left_join_error_no_match(L244.HDDCDD_normal_USA, by = c("region", "variable")) %>%
      mutate(internal.gains.scalar = round(scalar * degree.days / DDnorm, energy.DIGITS_HDDCDD),
             # Prevent very warm places from having negative heating demands, using exogenous threshold
             internal.gains.scalar = if_else(variable == "HDD" & degree.days < threshold_HDD, 0, internal.gains.scalar)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])


    L244.ShellConductance_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]]) -> L244.ShellConductance_bld_USAonly

    L244.Supplysector_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) -> L244.Supplysector_bld_USAonly

    L244.FinalEnergyKeyword_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]]) -> L244.FinalEnergyKeyword_bld_USAonly

    L244.SubsectorShrwtFllt_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) -> L244.SubsectorShrwtFllt_bld_USAonly

    L244.SubsectorInterp_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]]) -> L244.SubsectorInterp_bld_USAonly

    L244.SubsectorInterpTo_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]]) -> L244.SubsectorInterpTo_bld_USAonly

    L244.SubsectorLogit_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) -> L244.SubsectorLogit_bld_USAonly

    L244.StubTech_bld %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["StubTech"]]) -> L244.StubTech_bld_USAonly

    L244.StubTechMarket_bld %>%
      select(-region, -market.name) %>%
      distinct() %>%
      mutate(region = gcam.USA_REGION,
             market.name = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) -> L244.StubTechMarket_bld_USAonly

    # Emissions
    # The basic approach here is to allocate input emissions across relevant techs according to sector or subsector share
    # Air pollutants and most GHGs are allocated according to subsector (fuel) share
    # HFCs are are allocated according to sector share because detailed buildings has gas cooling, which would still emit HFCs
    # NOTE: There are a few limitations to this approach (described below); we may want to improve this eventually
    # NOTE: Refrigeration technologies, not represented in core building sector structure, should emit HFCs, but do not
    # in this implementation
    # NOTE: 4 hydrocarbon technologies that have zero historical share but can operate in future (gas furnace hi-eff,
    # gas water heater hi-eff, gas oven hi-eff, lpg oven hi-eff) are not assigned emissions

    L244.StubTechCalInput_bld_USAonly %>%
      left_join_error_no_match(A44.sector_emiss_map, by = "supplysector") %>%
      group_by(region, emiss_sector, subsector, minicam.energy.input, year) %>%
      mutate(subs_share = calibrated.value / sum(calibrated.value)) %>%
      ungroup() %>%
      group_by(region, emiss_sector, year) %>%
      mutate(sector_share = calibrated.value / sum(calibrated.value)) %>%
      ungroup() -> L244.emiss_share

    # L244.InputEmissions_bld_pol_USAonly - pollutant emissions
    L244.emiss_share %>%
      # join is intended to duplicate rows (by a number of emissions species)
      # LJENM will throw an error, so left_join() is used
      left_join(L201.en_pol_emissions %>%
                  select(-stub.technology),
                by = c("region", "emiss_sector" = "supplysector", "subsector",
                       "minicam.energy.input" = "input.name", "year")) %>%
      # some NAs are generated but they are all electricity techs (which don't produce air pollutants)
      drop_na(Non.CO2) %>%
      mutate(input.emissions = input.emissions * subs_share) %>%
      rename(input.name = minicam.energy.input) %>%
      select(LEVEL2_DATA_NAMES[["InputEmissions"]]) -> L244.InputEmissions_bld_pol_USAonly

    # L244.InputEmissions_bld_ghg_USAonly - GHG emissions
    L244.emiss_share %>%
      # join is intended to duplicate rows (by a number of emissions species)
      # LJENM will throw an error, so left_join() is used
      left_join(L201.en_ghg_emissions %>%
                  select(-stub.technology),
                by = c("region", "emiss_sector" = "supplysector", "subsector",
                       "minicam.energy.input" = "input.name", "year")) %>%
      # some NAs are generated but they are all electricity techs (which don't produce air pollutants)
      drop_na(Non.CO2) %>%
      mutate(input.emissions = input.emissions * subs_share) %>%
      rename(input.name = minicam.energy.input) %>%
      select(LEVEL2_DATA_NAMES[["InputEmissions"]]) -> L244.InputEmissions_bld_ghg_USAonly

    # L244.GDPCtrlMax_bld_ghg_USAonly
    L244.emiss_share %>%
      # join is intended to duplicate rows (by a number of emissions species)
      # LJENM will throw an error, so left_join() is used
      left_join(L201.nonghg_max_reduction %>%
                  select(-stub.technology),
                by = c("region", "emiss_sector" = "supplysector", "subsector", "year")) %>%
      # some NAs are generated but they are all electricity techs (which don't produce air pollutants)
      drop_na(Non.CO2) %>%
      select(LEVEL2_DATA_NAMES[["GDPCtrlMax"]]) -> L244.GDPCtrlMax_bld_ghg_USAonly

    # L244.GDPCtrlSteep_bld_ghg_USAonly
    L244.emiss_share %>%
      # join is intended to duplicate rows (by a number of emissions species)
      # LJENM will throw an error, so left_join() is used
      left_join(L201.nonghg_steepness %>%
                  select(-stub.technology),
                by = c("region", "emiss_sector" = "supplysector", "subsector", "year")) %>%
      # some NAs are generated but they are all electricity techs (which don't produce air pollutants)
      drop_na(Non.CO2) %>%
      select(LEVEL2_DATA_NAMES[["GDPCtrlSteep"]]) -> L244.GDPCtrlSteep_bld_ghg_USAonly

    # L244.StbTechOutputEmissions_bld_hfc_USAonly - HFC emissions
    L244.emiss_share %>%
      # join is intended to duplicate rows (by a number of emissions species)
      # LJENM will throw an error, so left_join() is used
      left_join(L241.hfc_all %>%
                  select(-subsector, -stub.technology),
                by = c("region", "emiss_sector" = "supplysector", "year")) %>%
      # HFC are only generated by cooling techs; other techs are not mapped (NA); drop
      drop_na(Non.CO2) %>%
      mutate(input.emissions = input.emissions * sector_share) %>%
      rename(input.name = minicam.energy.input) %>%
      select(LEVEL2_DATA_NAMES[["StbTechOutputEmissions"]]) -> L244.StbTechOutputEmissions_bld_hfc_USAonly

    # L244.OutputEmissCoeff_bld_hfc_future_USAonly - HFC emissions for techs that don't deploy historically
    L244.StbTechOutputEmissions_bld_hfc_USAonly %>%
      # isolate techs that have zero deployment in final base year
      filter(year == max(MODEL_BASE_YEARS),
             input.emissions == 0) %>%
      # join is intended to duplicate rows (by a number of emissions species)
      # LJENM will throw an error, so left_join() is used
      left_join(L241.hfc_future %>%
                  group_by(supplysector, subsector, stub.technology, year, Non.CO2) %>%
                  filter(emiss.coeff == min(emiss.coeff)) %>%
                  ungroup() %>%
                  select(-region, -subsector, -stub.technology) %>%
                  distinct(),
                by = c("supplysector", "year", "Non.CO2")) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]]) -> L244.OutputEmissCoeff_bld_hfc_future_USAonly

    # L244.StubTechEmissUnits_bld_hfc_USAonly
    L244.StbTechOutputEmissions_bld_hfc_USAonly %>%
      bind_rows(L244.OutputEmissCoeff_bld_hfc_future_USAonly) %>%
      distinct(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(L241.fgas_all_units %>%
                                 distinct(Non.CO2, emissions.unit),
                               by = "Non.CO2") %>%
      select(LEVEL2_DATA_NAMES[["StubTechEmissUnits"]]) -> L244.StubTechEmissUnits_bld_hfc_USAonly

    # L244.MAC_bld_hfc_USAonly
    L244.StubTechEmissUnits_bld_hfc_USAonly %>%
      distinct(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      # join is intended to duplicate rows (by a number of points on the MAC curve)
      # LJENM will throw an error, so left_join() is used
      left_join(L252.MAC_higwp %>%
                  select(-subsector, -stub.technology),
                               by = c("region", "supplysector", "Non.CO2")) %>%
      select(LEVEL2_DATA_NAMES[["MAC"]]) -> L244.MAC_bld_hfc_USAonly

    # L244.MACPhaseIn_bld_hfc_USAonly
    L244.StubTechEmissUnits_bld_hfc_USAonly %>%
      distinct(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      # join is intended to duplicate rows (by a number of points on the MAC curve)
      # LJENM will throw an error, so left_join() is used
      left_join(L252.MAC_higwp_phaseInTime %>%
                  select(-subsector, -stub.technology),
                by = c("region", "supplysector", "Non.CO2")) %>%
      select(LEVEL2_DATA_NAMES[["MACPhaseIn"]]) -> L244.MACPhaseIn_bld_hfc_USAonly

    # L244.MACTC_bld_hfc_USAonly
    L244.StubTechEmissUnits_bld_hfc_USAonly %>%
      distinct(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      # join is intended to duplicate rows (by a number of points on the MAC curve)
      # LJENM will throw an error, so left_join() is used
      left_join(L252.MAC_higwp_tc_average %>%
                  select(-subsector, -stub.technology),
                by = c("region", "supplysector", "Non.CO2")) %>%
      select(LEVEL2_DATA_NAMES[["MACTC"]]) -> L244.MACTC_bld_hfc_USAonly


    # ===================================================
    # Produce outputs

    L244.DeleteSupplysector_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      add_precursors("energy/A44.gcam_consumer",
                     "energy/A44.sector",
                     "L244.GenericBaseService",
                     "gcam-usa/A44.demand_satiation_mult",
                     "gcam-usa/A44.globaltech_intgains",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "L144.flsp_bm2_state_res",
                     "L144.flsp_bm2_state_comm",
                     "L143.HDDCDD_scen_state",
                     "L244.Satiation_flsp_gcamusa",
                     "L244.DeleteSupplysector_USAbld",
                     "L244.PriceExp_IntGains_gcamusa",
                     "L244.SatiationAdder_gcamusa",
                     "L244.ThermalBaseService_gcamusa",
                     "L244.GenericBaseService_gcamusa",
                     "L244.ThermalServiceSatiation_gcamusa",
                     "L244.GenericServiceSatiation_gcamusa",
                     "L244.Intgains_scalar_gcamusa",
                     "L244.ShellConductance_bld_gcamusa",
                     "L244.Supplysector_bld_gcamusa",
                     "L244.FinalEnergyKeyword_bld_gcamusa",
                     "L244.SubsectorShrwtFllt_bld_gcamusa",
                     "L244.SubsectorInterp_bld_gcamusa",
                     "L244.SubsectorInterpTo_bld_gcamusa",
                     "L244.SubsectorLogit_bld_gcamusa",
                     "L244.StubTech_bld_gcamusa",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L244.StubTechCalInput_bld",
                     "L244.StubTechMarket_bld",
                     "L244.GlobalTechEff_bld",
                     "gcam-usa/A44.sector_emiss_map",
                     "L201.en_pol_emissions",
                     "L201.en_ghg_emissions",
                     "L201.nonghg_max_reduction",
                     "L201.nonghg_steepness",
                     "L201.nonghg_max_reduction",
                     "L201.nonghg_steepness",
                     "L241.hfc_all",
                     "L241.hfc_future",
                     "L241.fgas_all_units",
                     "L252.MAC_higwp",
                     "L252.MAC_higwp_phaseInTime",
                     "L252.MAC_higwp_tc_average") %>%
      add_comments("TODO:  add comments") ->
      L244.DeleteSupplysector_USAonly

    L244.DeleteGenericService_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.DeleteGenericService_USAonly

    L244.PriceExp_IntGains_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.PriceExp_IntGains_USAonly

    L244.Floorspace_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.Floorspace_USAonly

    L244.SatiationAdder_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.SatiationAdder_USAonly

    L244.ThermalBaseService_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.ThermalBaseService_USAonly

    L244.GenericBaseService_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.GenericBaseService_USAonly

    L244.ThermalServiceSatiation_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.ThermalServiceSatiation_USAonly

    L244.GenericServiceSatiation_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.GenericServiceSatiation_USAonly

    L244.Intgains_scalar_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.Intgains_scalar_USAonly

    L244.ShellConductance_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.ShellConductance_bld_USAonly

    L244.Supplysector_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.Supplysector_bld_USAonly

    L244.FinalEnergyKeyword_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.FinalEnergyKeyword_bld_USAonly

    L244.SubsectorShrwtFllt_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.SubsectorShrwtFllt_bld_USAonly

    L244.SubsectorInterp_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.SubsectorInterp_bld_USAonly

    L244.SubsectorInterpTo_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.SubsectorInterpTo_bld_USAonly

    L244.SubsectorLogit_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.SubsectorLogit_bld_USAonly

    L244.StubTech_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.StubTech_bld_USAonly

    L244.StubTechCalInput_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.StubTechCalInput_bld_USAonly

    L244.StubTechMarket_bld_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.StubTechMarket_bld_USAonly

    # emissions
    L244.InputEmissions_bld_pol_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.InputEmissions_bld_pol_USAonly

    L244.InputEmissions_bld_ghg_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.InputEmissions_bld_ghg_USAonly

    L244.GDPCtrlMax_bld_ghg_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.GDPCtrlMax_bld_ghg_USAonly

    L244.GDPCtrlSteep_bld_ghg_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.GDPCtrlSteep_bld_ghg_USAonly

    L244.StbTechOutputEmissions_bld_hfc_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.StbTechOutputEmissions_bld_hfc_USAonly

    L244.OutputEmissCoeff_bld_hfc_future_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.OutputEmissCoeff_bld_hfc_future_USAonly

    L244.StubTechEmissUnits_bld_hfc_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.StubTechEmissUnits_bld_hfc_USAonly

    L244.MAC_bld_hfc_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.MAC_bld_hfc_USAonly

    L244.MACPhaseIn_bld_hfc_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.MACPhaseIn_bld_hfc_USAonly

    L244.MACTC_bld_hfc_USAonly %>%
      add_title("TODO:  add title") %>%
      add_units("TODO:  add units") %>%
      same_precursors_as("L244.DeleteSupplysector_USAonly") %>%
      add_comments("TODO:  add comments") ->
      L244.MACTC_bld_hfc_USAonly

    return_data(L244.DeleteSupplysector_USAonly,
                L244.DeleteGenericService_USAonly,
                L244.PriceExp_IntGains_USAonly,
                L244.Floorspace_USAonly,
                L244.SatiationAdder_USAonly,
                L244.ThermalBaseService_USAonly,
                L244.GenericBaseService_USAonly,
                L244.ThermalServiceSatiation_USAonly,
                L244.GenericServiceSatiation_USAonly,
                L244.Intgains_scalar_USAonly,
                L244.ShellConductance_bld_USAonly,
                L244.Supplysector_bld_USAonly,
                L244.FinalEnergyKeyword_bld_USAonly,
                L244.SubsectorShrwtFllt_bld_USAonly,
                L244.SubsectorInterp_bld_USAonly,
                L244.SubsectorInterpTo_bld_USAonly,
                L244.SubsectorLogit_bld_USAonly,
                L244.StubTech_bld_USAonly,
                L244.StubTechCalInput_bld_USAonly,
                L244.StubTechMarket_bld_USAonly,
                # emissions
                L244.InputEmissions_bld_pol_USAonly,
                L244.InputEmissions_bld_ghg_USAonly,
                L244.GDPCtrlMax_bld_ghg_USAonly,
                L244.GDPCtrlSteep_bld_ghg_USAonly,
                L244.StbTechOutputEmissions_bld_hfc_USAonly,
                L244.OutputEmissCoeff_bld_hfc_future_USAonly,
                L244.StubTechEmissUnits_bld_hfc_USAonly,
                L244.MAC_bld_hfc_USAonly,
                L244.MACPhaseIn_bld_hfc_USAonly,
                L244.MACTC_bld_hfc_USAonly)

  } else {
    stop("Unknown command")
  }
}
