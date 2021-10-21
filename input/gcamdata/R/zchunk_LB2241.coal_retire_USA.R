# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB2241.coal_retire_USA
#'
#' Generates GCAM-USA model input for removing coal capacity retired between 2011 and 2015, 2016 and 2020, and vintaging capacity which continues to operate beyond 2020.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2241.StubTechCalInput_elec_coalret_USAagg}, \code{L2241.StubTechEff_elec_coalret_USAagg}, \code{L2241.StubTechSCurve_elec_coalret_USAagg},
#' \code{L2241.GlobalTechShrwt_elec_coalret_USAagg}, \code{L2241.GlobalTechCapFac_elec_coalret_USAagg}, \code{L2241.GlobalTechCapital_elec_coalret_USAagg},
#' \code{L2241.GlobalTechOMfixed_elec_coalret_USAagg}, \code{L2241.GlobalTechOMvar_elec_coalret_USAagg}, \code{L2241.GlobalTechEff_elec_coalret_USAagg},
#' \code{L2241.GlobalTechProfitShutdown_elec_coalret_USAagg}, \code{L2241.StubTechCalInput_coal_vintage_USAagg}, \code{L2241.StubTechEff_coal_vintage_USAagg},
#' \code{L2241.StubTechSCurve_coal_vintage_USAagg}, \code{L2241.StubTechProfitShutdown_coal_vintage_USAagg}, \code{L2241.GlobalTechShrwt_coal_vintage_USAagg},
#' \code{L2241.GlobalTechEff_coal_vintage_USAagg}, \code{L2241.GlobalTechCapFac_coal_vintage_USAagg}, \code{L2241.GlobalTechCapital_coal_vintage_USAagg},
#' \code{L2241.GlobalTechOMfixed_coal_vintage_USAagg}, \code{L2241.GlobalTechOMvar_coal_vintage_USAagg}, \code{L2241.OutputEmissions_elec_coalret_USAagg},
#' \code{L2241.OutputEmissions_coal_vintage_USAagg}.
#' @details This chunk creates add-on files to take the fraction of reduction in coal electricity generation between 2010 and 2015 for each state and
#' forces that generation to retire in 2015. It also tempers retirement assumptions for the remaining coal fleet to allow
#' most 2015 generation to continue through mid-century.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC Aug 2018
module_gcamusa_LB2241.coal_retire_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A23.elec_tech_mapping_coal_retire",
             FILE = "gcam-usa/A23.elec_tech_coal_retire_SCurve",
             FILE = "gcam-usa/EIA_coal_generation_2018",
             # electricity
             "L223.StubTechCalInput_elec",
             "L223.StubTechEff_elec",
             "L223.GlobalTechShrwt_elec",
             "L223.GlobalTechCapFac_elec",
             "L223.GlobalTechCapital_elec",
             "L223.GlobalTechOMfixed_elec",
             "L223.GlobalTechOMvar_elec",
             "L223.GlobalTechEff_elec",
             "L223.GlobalTechProfitShutdown_elec",
             "L201.OutputEmissions_elec",
             # # electricity + cooling technologies
             # "L2233.StubTechProd_elec_cool",
             # "L2233.StubTechEff_elec_cool",
             # "L2233.GlobalTechShrwt_elec_cool",
             # "L2233.GlobalTechCapFac_elec_cool",
             # "L2233.GlobalTechCapital_elecPassthru",
             # "L2233.GlobalTechCapital_elec_cool",
             # "L2233.GlobalTechOMfixed_elecPassthru",
             # "L2233.GlobalTechOMvar_elecPassthru",
             # "L2233.GlobalTechEff_elec_cool",
             # "L2233.GlobalTechProfitShutdown_elec_cool",
             # "L2233.GlobalTechCoef_elec_cool",
             FILE = "gcam-usa/prime_mover_map",
             FILE = "gcam-usa/EIA_860_generators_existing_2018",
             FILE = "gcam-usa/EIA_860_generators_retired_2018",
             FILE = "gcam-usa/EIA_923_generator_gen_fuel_2018"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2241.StubTechCalInput_elec_coalret_USAagg",
             "L2241.StubTechEff_elec_coalret_USAagg",
             "L2241.StubTechSCurve_elec_coalret_USAagg",
             "L2241.GlobalTechShrwt_elec_coalret_USAagg",
             "L2241.GlobalTechCapFac_elec_coalret_USAagg",
             "L2241.GlobalTechCapital_elec_coalret_USAagg",
             "L2241.GlobalTechOMfixed_elec_coalret_USAagg",
             "L2241.GlobalTechOMvar_elec_coalret_USAagg",
             "L2241.GlobalTechEff_elec_coalret_USAagg",
             "L2241.GlobalTechProfitShutdown_elec_coalret_USAagg",
             "L2241.StubTechCalInput_coal_vintage_USAagg",
             "L2241.StubTechEff_coal_vintage_USAagg",
             "L2241.StubTechSCurve_coal_vintage_USAagg",
             "L2241.StubTechProfitShutdown_coal_vintage_USAagg",
             "L2241.GlobalTechShrwt_coal_vintage_USAagg",
             "L2241.GlobalTechEff_coal_vintage_USAagg",
             "L2241.GlobalTechCapFac_coal_vintage_USAagg",
             "L2241.GlobalTechCapital_coal_vintage_USAagg",
             "L2241.GlobalTechOMfixed_coal_vintage_USAagg",
             "L2241.GlobalTechOMvar_coal_vintage_USAagg",
             "L2241.OutputEmissions_elec_coalret_USAagg",
             "L2241.OutputEmissions_coal_vintage_USAagg"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    region <- year <- supplysector <- subsector <- stub.technology <- technology <- tech <- stub.technology.new  <-
      Electric.sector <- Electric.sector.technology <- source.key <- state <- state_name <- value <- retire_frac <-
      fuel <- sector.name <- subsector.name <- median.shutdown.point <- profit.shutdown.steepness <-
      share.weight.year <- subs.share.weight <- share.weight <- tech.share.weight <- share.vintage <-
      calOutputValue <- calOutput_OLD <- capacity.factor <- input.capital <- capital.overnight <- fixed.charge.rate <-
      input.OM.fixed <- OM.fixed <- input.OM.var <- OM.var <- minicam.energy.input <- efficiency <-
      unit.capacity.MW <- Retirement.Year <- Nameplate.Capacity..MW. <- Summer.Capacity..MW. <- State <-
      Plant.Code <- Generator.ID <- Operating.Year <- Planned.Retirement.Year <- Net.Generation.Year.To.Date <-
      Status <- gen_share <- Prime.Mover <- Reported.Prime.Mover <- Energy.Source.1 <- Reported.Fuel.Type.Code <-
      vintage.bin <- capacity <- generation <- gcam_fuel <- cap.lifetime <- lifetime <- avg.lifetime <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A23.elec_tech_mapping_coal_retire <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_coal_retire", strip_attributes = TRUE) %>%
    filter(!grepl("generation", Electric.sector))
    A23.elec_tech_coal_retire_SCurve <- get_data(all_data, "gcam-usa/A23.elec_tech_coal_retire_SCurve", strip_attributes = TRUE) %>%
      filter(!grepl("generation", Electric.sector)) %>%
      select(-Electric.sector)
    EIA_coal_generation_2018 <- get_data(all_data, "gcam-usa/EIA_coal_generation_2018", strip_attributes = TRUE)
    # electricity
    L223.StubTechCalInput_elec <- get_data(all_data, "L223.StubTechCalInput_elec", strip_attributes = TRUE)
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec", strip_attributes = TRUE)
    L223.GlobalTechShrwt_elec <- get_data(all_data, "L223.GlobalTechShrwt_elec", strip_attributes = TRUE)
    L223.GlobalTechCapFac_elec <- get_data(all_data, "L223.GlobalTechCapFac_elec", strip_attributes = TRUE)
    L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec", strip_attributes = TRUE)
    L223.GlobalTechOMfixed_elec <- get_data(all_data, "L223.GlobalTechOMfixed_elec", strip_attributes = TRUE)
    L223.GlobalTechOMvar_elec <- get_data(all_data, "L223.GlobalTechOMvar_elec", strip_attributes = TRUE)
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec", strip_attributes = TRUE)
    L223.GlobalTechProfitShutdown_elec <- get_data(all_data, "L223.GlobalTechProfitShutdown_elec", strip_attributes = TRUE)
    L201.OutputEmissions_elec <- get_data(all_data, "L201.OutputEmissions_elec", strip_attributes = TRUE)
    # # electricity + cooling technologies
    # L2233.StubTechProd_elec_cool <- get_data(all_data, "L2233.StubTechProd_elec_cool", strip_attributes = TRUE)
    # L2233.StubTechEff_elec_cool <- get_data(all_data, "L2233.StubTechEff_elec_cool", strip_attributes = TRUE)
    # L2233.GlobalTechShrwt_elec_cool <- get_data(all_data, "L2233.GlobalTechShrwt_elec_cool", strip_attributes = TRUE)
    # L2233.GlobalTechCapFac_elec_cool <- get_data(all_data, "L2233.GlobalTechCapFac_elec_cool", strip_attributes = TRUE)
    # L2233.GlobalTechCapital_elecPassthru <- get_data(all_data, "L2233.GlobalTechCapital_elecPassthru", strip_attributes = TRUE)
    # L2233.GlobalTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalTechCapital_elec_cool", strip_attributes = TRUE)
    # L2233.GlobalTechOMfixed_elecPassthru <- get_data(all_data, "L2233.GlobalTechOMfixed_elecPassthru", strip_attributes = TRUE)
    # L2233.GlobalTechOMvar_elecPassthru <- get_data(all_data, "L2233.GlobalTechOMvar_elecPassthru", strip_attributes = TRUE)
    # L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool", strip_attributes = TRUE)
    # L2233.GlobalTechProfitShutdown_elec_cool <- get_data(all_data, "L2233.GlobalTechProfitShutdown_elec_cool", strip_attributes = TRUE)
    # L2233.GlobalTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalTechCoef_elec_cool", strip_attributes = TRUE)

    prime_mover_map <- get_data(all_data, "gcam-usa/prime_mover_map")
    eia_860_data_2018 <- get_data(all_data, "gcam-usa/EIA_860_generators_existing_2018")
    eia_923_data_2018 <- get_data(all_data, "gcam-usa/EIA_923_generator_gen_fuel_2018")

    # ===================================================
    # Perform computations

    # Prepare a table for stub technologies with all states and base years
    A23.elec_tech_mapping_coal_retire %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      mutate(region = gcam.USA_REGION) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) ->
      L2241.elec_USA_coalret_base

    # Calculate fraction of historical (2015) vintage retired by 2018
    EIA_coal_gen_years <- EIA_coal_generation_2018 %>%
      gather_years()
    EIA_coal_gen_years <- unique(EIA_coal_gen_years$year)

    EIA_coal_generation_2018 %>%
      select(-source.key) %>%
      gather_years() %>%
      replace_na(list(value = 0)) %>%
      # The EIA coal generation data set (EIA_coal_generation_2018) begins in 2001
      # This is fine for the data's intended purposes, but causes test_timeshift to fail when max(MODEL_BASE_YEARS) < 2001
      # Backfill historical values with 2001 values to avoid test_timeshift failure
      complete(nesting(fuel, state, units), year = c(HISTORICAL_YEARS, EIA_coal_gen_years))  %>%
      group_by(fuel, state, units) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      rename(state_name = state) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, state_name),
                               by = "state_name") %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), max(year))) %>%
      select(state, year, units, value) %>%
      group_by(year, units) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(retire_frac = 1 - (value / value[year==max(MODEL_BASE_YEARS)])) %>%
      filter(year == max(year)) %>%
      replace_na(list(retire_frac = 0)) %>%
      mutate(region = gcam.USA_REGION) %>%
      select(region, retire_frac) -> fraction_coal_gen_retire


    ##### ELECTRICITY SECTOR WITHOUT ENDOGENOUS COOLING TECHNOLOGIES #####

    # Create two technologies: coal_base_conv pul and coal_base_conv pul_retire_2020
    # L2241.StubTechCalInput_elec_USA_coalret:  Calibration outputs for conventional coal electricity plants by U.S. state
    L2241.elec_USA_coalret_base %>%
      left_join_error_no_match(L223.StubTechCalInput_elec,
                               by = c("region", "supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
      left_join_error_no_match(fraction_coal_gen_retire, by = "region") %>%
      rename(calibrated.value_OLD = calibrated.value) %>%
      mutate(calibrated.value = if_else(!grepl("_retire_", stub.technology),
                                        calibrated.value_OLD * (1 - retire_frac),
                                        calibrated.value_OLD * retire_frac),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2241.StubTechCalInput_elec_coalret_USA

    # Create a table to read in efficiencies for the new technologies in calibration years
    # L2241.StubTechEff_elec_USA_coalret: Efficiencies of U.S. conventional coal electricity plants in calibration years
    L2241.elec_USA_coalret_base %>%
      left_join_error_no_match(L223.StubTechEff_elec,
                               by = c("region", "supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
      L2241.StubTechEff_elec_coalret_USA

    # Create a table to read in s-curve retirement parameters for the new technologies
    # L2241.StubTechSCurve_elec_coalret:  s-curve shutdown decider for historic U.S. conventional coal electricity plants
    # Note that this updates s-curve retirement parameters for both the existing technologies and the new "retire_2020" technologies
    L2241.StubTechCalInput_elec_coalret_USA %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(A23.elec_tech_coal_retire_SCurve,
                               by = c("stub.technology" = "Electric.sector.technology")) -> L2241.StubTechSCurve_elec_coalret_USA

    # Prepare a table for global technologies with all model years
    A23.elec_tech_mapping_coal_retire %>%
      # this is only needed for the new "retire_2020" technologies
      filter(grepl("_retire_", Electric.sector.technology)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(supplysector = Electric.sector, tech = technology, technology = Electric.sector.technology) ->
      L2241.elec_USA_coalret

    # Share-weights
    # L2241.GlobalTechShrwt_elec_coalret: Shareweights for historic U.S. conventional coal electricity plants
    # Global techology shareweights set to zero for all model periods
    # Shareweights for stub.technologies which produce in base years are reset to 1 by L2241.StubTechCalInput_elec_coalret_USA
    # Shareweights for all future years are set to zero, as new deployment of the "retire" technologies will not occur
    L2241.elec_USA_coalret %>%
      left_join_error_no_match(L223.GlobalTechShrwt_elec, by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) %>%
      mutate(share.weight = 0) ->
      L2241.GlobalTechShrwt_elec_coalret_USA

    # Capacity factor
    # L2241.GlobalTechCapFac_elec_coalret: Capacity factors for historic U.S. conventional coal electricity plants
    L2241.elec_USA_coalret %>%
      left_join_error_no_match(L223.GlobalTechCapFac_elec,
                               by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapFac"]]) ->
      L2241.GlobalTechCapFac_elec_coalret_USA

    # Capital costs
    # L2241.GlobalTechCapital_elec_coalret: Capital costs of historic U.S. conventional coal electricity plants
    L2241.elec_USA_coalret %>%
      left_join_error_no_match(L223.GlobalTechCapital_elec,
                               by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
      L2241.GlobalTechCapital_elec_coalret_USA

    # Fixed OM costs
    # L2241.GlobalTechOMfixed_elec_coalret: Fixed OM costs of historic U.S. conventional coal electricity plants
    L2241.elec_USA_coalret %>%
      left_join_error_no_match(L223.GlobalTechOMfixed_elec,
                               by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]) ->
      L2241.GlobalTechOMfixed_elec_coalret_USA

    # Variable OM costs
    # L2241.GlobalTechOMvar_elec_coalret: Variable OM costs of historic U.S. conventional coal electricity plants
    L2241.elec_USA_coalret %>%
      left_join_error_no_match(L223.GlobalTechOMvar_elec,
                               by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]) ->
      L2241.GlobalTechOMvar_elec_coalret_USA

    # Efficiencies for future years - read in in the global technology database
    # L2241.GlobalTechEff_elec_coalret: Efficiencies of historic U.S. conventional coal electricity plants
    L2241.elec_USA_coalret %>%
      left_join_error_no_match(L223.GlobalTechEff_elec,
                               by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2241.GlobalTechEff_elec_coalret_USA

    # Profit Shutdown decider
    # L2241.GlobalTechProfitShutdown_elec_coalret: Profit shut-down decider for historic U.S. conventional coal electricity plants
    L2241.elec_USA_coalret %>%
      filter(year >= max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L223.GlobalTechProfitShutdown_elec,
                               by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechProfitShutdown"]]) ->
      L2241.GlobalTechProfitShutdown_elec_coalret_USA

    # non-CO2 emissions
    L201.OutputEmissions_elec %>%
      filter(region == gcam.USA_REGION) %>%
      semi_join(A23.elec_tech_mapping_coal_retire,
                by = c("stub.technology" = "technology")) %>%
      # join is intended to duplicate rows so emissions can be allocated across vintages; use left_join to avoid error
      left_join(A23.elec_tech_mapping_coal_retire %>%
                  select(technology, new.technology = Electric.sector.technology),
                by = c("stub.technology" = "technology"))  %>%
      select(-stub.technology) %>%
      left_join_error_no_match(L2241.StubTechCalInput_elec_coalret_USA %>%
                                 group_by(year) %>%
                                 mutate(share = calibrated.value / sum(calibrated.value)) %>%
                                 ungroup() %>%
                                 select(stub.technology, year, share),
                               by = c("new.technology" = "stub.technology", "year")) %>%
      mutate(input.emissions = input.emissions * share) %>%
      rename(stub.technology = new.technology) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissions"]]) ->
      L2241.OutputEmissions_elec_coalret_USA


    # ===================================================
    # Vintage coal plants which operate beyond 2015

    # Create tables of coal units operating in 2018 that include vintage, capacity, generation and planned retirement year.
    eia_860_data_2018 %>%
      # Use nameplate capacity for now. Not sure if summer capacity makes more sense
      mutate(unit.capacity.MW = if_else(is.na(Nameplate.Capacity..MW.), Summer.Capacity..MW., Nameplate.Capacity..MW.)) %>%
      left_join_error_no_match(prime_mover_map,
                by = c("Prime.Mover" = "Reported.Prime.Mover", "Energy.Source.1" = "Reported.Fuel.Type.Code")) %>%
      filter(gcam_fuel == "coal") %>%
      select(State, Plant.Code, Generator.ID, Operating.Year, Planned.Retirement.Year, capacity = unit.capacity.MW) %>%
      # Obtain unit/generator-level generation in 2015 from generator-level generation data in Form 923.
      # 10 units are missing geneation data in eia_923_data_2018.  LJENM throws error; use left_join for now
      left_join(eia_923_data_2018 %>%
                  rename(generation = Net.Generation.Year.To.Date),
                by = c("State", "Generator.ID" = "Generator.Id", "Plant.Code" = "Plant.Id")) %>%
      replace_na(list(generation = 0)) %>%
      # a couple of plants in MO, kY, and MI have negative generation values - reset to zero
      mutate(generation = if_else(generation < 0, 0 , generation)) ->
      L2241.coal_units_gen_2018

    # The Planned.Retirement.Year variable reflects planned retirements.
    # When no Planned.Retirement.Year is available, we assume a maximum lifetime of 80 years
    L2241.coal_units_gen_2018 %>%
      # Categorize coal units by vintage bins. We stick with 5-year bins for now
      mutate(vintage.bin = cut(Operating.Year, breaks = gcamusa.COAL_VINTAGE_BREAKS, labels = gcamusa.COAL_VINTAGE_LABELS),
             Planned.Retirement.Year = as.numeric(Planned.Retirement.Year)) %>%
      # For units without a Planned.Retirement.Year, we assume a maximum lifetime of 80 years
      mutate(Retirement.Year = if_else(is.na(Planned.Retirement.Year),
                                       Operating.Year + gcamusa.AVG_COAL_PLANT_LIFETIME,
                                       Planned.Retirement.Year)) %>%
      # coal retirement is updated to 2020, so this section should consider only plants expected to retire after 2020
      filter(Retirement.Year > 2020) ->
      L2241.coal_units_ret_2018

    # Create a table of generation and capacity-weighted lifetime from 2015 and by state and vintage
    L2241.coal_units_ret_2018 %>%
      # Calculate lifetime from 2018, which is model base year
      mutate(cap.lifetime = capacity * (Retirement.Year - max(MODEL_BASE_YEARS))) %>%
      group_by(State, vintage.bin) %>%
      summarise(lifetime = round(sum(cap.lifetime) / sum(capacity), 0), generation = sum(generation)) %>%
      mutate(generation = generation * CONV_MWH_GJ * CONV_GJ_EJ,
             # For units with 0 or negative expected lifetime, we will set lifetime to 1 to avoid calibration issues
             lifetime = replace(lifetime, lifetime < 0, 1)) %>%
      ungroup() %>%
      # Keep share of each vintage bin of the total generation in each state ready to be applied to
      # calibrated value in 2015
      group_by(vintage.bin) %>%
      mutate(lifetime = lifetime * (generation / sum(generation)))  %>%
      summarise(lifetime = sum(lifetime),
                generation = sum(generation)) %>%
      ungroup() %>%
      mutate(share.vintage = generation / sum(generation),
             region = gcam.USA_REGION) ->
      L2241.coal_vintage_gen_2018

    # Apply vintage share by state to calibrated values for slow retire component and create table to be read in
    L2241.coal_vintage_gen_2018 %>%
      # LJENM is intended to duplicate rows so production can be allocated across vintages; use left_join to avoid error
      left_join(L2241.StubTechCalInput_elec_coalret_USA %>%
                  filter(subsector == "coal",
                         year == max(MODEL_BASE_YEARS),
                         !grepl("retire", stub.technology)),
                by = "region") %>%
      filter(!is.na(calibrated.value), calibrated.value != 0) %>%
      mutate(calibrated.value = calibrated.value * share.vintage,
             # Create new technologies. Naming the variable as stub.technology.new so that we can use stub.technology as reference later
             stub.technology.new = paste(stub.technology, vintage.bin, sep = " "),
             year = max(MODEL_BASE_YEARS), share.weight.year = max(MODEL_BASE_YEARS),
             subs.share.weight = 1, tech.share.weight = 1) %>%
      # Select variables. For now, include lifetime and vintage.bin as well. We'll remove it later
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]], stub.technology.new, lifetime, vintage.bin) ->
      L2241.StubTechCalInput_coal_vintage_USA

    # Create a table to read in S-curve parameters for vintage bin techs by state
    L2241.StubTechCalInput_coal_vintage_USA %>%
      mutate(stub.technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["StubTechLifetime"]]) %>%
      mutate(steepness = gcamusa.COAL_RETIRE_STEEPNESS,
             half.life = round(lifetime * (gcamusa.AVG_COAL_PLANT_HALFLIFE / gcamusa.AVG_COAL_PLANT_LIFETIME), 0),
             lifetime = round(lifetime, 0)) ->
      L2241.StubTechSCurve_coal_vintage_USA

    # Read in profit shutdown decider for vintage technologies
    L2241.StubTechCalInput_coal_vintage_USA %>%
      mutate(stub.technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["StubTechLifetime"]]) %>%
      mutate(median.shutdown.point = gcamusa.MEDIAN_SHUTDOWN_POINT,
             profit.shutdown.steepness = gcamusa.PROFIT_SHUTDOWN_STEEPNESS) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProfitShutdown"]]) ->
      L2241.StubTechProfitShutdown_coal_vintage_USA

    # Create a table to read in energy inputs and efficiencies for the new technologies in calibration years.
    # Assuming state average efficiency for all vintages since historically, there is not much
    # correlation between efficiency of a generator and its vintage.
    L2241.StubTechCalInput_coal_vintage_USA %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], stub.technology.new) %>%
      complete(nesting(region, supplysector, subsector, stub.technology, stub.technology.new), year = MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2241.StubTechEff_elec_coalret_USA,
                               by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(stub.technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
      L2241.StubTechEff_coal_vintage_USA

    # Create tables to read in energy and non-energy inputs for future years in global technology database
    # Create a basic strucure with common variables
    L2241.StubTechCalInput_coal_vintage_USA %>%
      select(supplysector, subsector, stub.technology, stub.technology.new, year) %>%
      unique() %>%
      complete(nesting(supplysector, subsector, stub.technology, stub.technology.new), year = MODEL_YEARS) ->
      L2241.GlobalTech

    # Energy inputs: Efficiency
    L2241.GlobalTech %>%
      left_join_error_no_match(L223.GlobalTechEff_elec,
                               by = c("stub.technology" = "technology", "year")) %>%
      rename(technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2241.GlobalTechEff_coal_vintage_USA

    # Non-energy inputs: capacity factor, capital costs, fixed and variable OM costs
    # Capacity factor:
    L2241.GlobalTech %>%
      left_join_error_no_match(L223.GlobalTechCapFac_elec,
                               by = c("stub.technology" = "technology", "year")) %>%
      rename(technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapFac"]]) ->
      L2241.GlobalTechCapFac_coal_vintage_USA

    # Capital costs:
    L2241.GlobalTech %>%
      left_join_error_no_match(L223.GlobalTechCapital_elec,
                               by = c("stub.technology" = "technology", "year")) %>%
      rename(technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
      L2241.GlobalTechCapital_coal_vintage_USA

    # Fixed OM costs:
    L2241.GlobalTech %>%
      left_join_error_no_match(L223.GlobalTechOMfixed_elec,
                               by = c("stub.technology" = "technology", "year")) %>%
      rename(technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]) ->
      L2241.GlobalTechOMfixed_coal_vintage_USA

    # Variable OM costs
    L2241.GlobalTech %>%
      left_join_error_no_match(L223.GlobalTechOMvar_elec,
                               by = c("stub.technology" = "technology", "year")) %>%
      rename(technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]) ->
      L2241.GlobalTechOMvar_coal_vintage_USA

    # Create table to read in shareweights in future years in global technology database
    L2241.GlobalTech %>%
      mutate(share.weight = 0) %>%
      rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L2241.GlobalTechShrwt_coal_vintage_USA

    # Clean up StubTechCalInput table
    L2241.StubTechCalInput_coal_vintage_USA %>%
      mutate(stub.technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input), year = MODEL_BASE_YEARS) %>%
      mutate(share.weight.year = year) %>%
      # Read in zero caloutputvalue for other base years
      replace_na(list(calibrated.value = 0, subs.share.weight = 1, tech.share.weight = 0)) ->
      L2241.StubTechCalInput_coal_vintage_USA

    # Read in zero calOutputValue for 2010 for existing coal conv pul technology
    L2241.StubTechCalInput_elec_coalret_USA %>%
      filter(subsector == "coal",
             year == max(MODEL_BASE_YEARS),
             calibrated.value != 0,
             !grepl("_retire_", stub.technology)) %>%
      mutate(calibrated.value = 0, tech.share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) %>%
      bind_rows(L2241.StubTechCalInput_coal_vintage_USA) %>%
      arrange(region, year) ->
      L2241.StubTechCalInput_coal_vintage_USA

    # MB:  LEFT OFF HERE
    # non-CO2 emissions
    L201.OutputEmissions_elec %>%
      filter(region == gcam.USA_REGION,
             # coal vintage technologies only exist in final calibration year (2015); filter for these years
             year == max(MODEL_BASE_YEARS)) %>%
      semi_join(L2241.StubTechCalInput_coal_vintage_USA,
                by = c("stub.technology")) %>%
      repeat_add_columns(tibble::tibble(new.technology = unique(L2241.StubTechCalInput_coal_vintage_USA$stub.technology))) %>%
      select(-stub.technology) %>%
      left_join_error_no_match(L2241.StubTechCalInput_coal_vintage_USA %>%
                                 group_by(year) %>%
                                 mutate(share = calibrated.value / sum(calibrated.value)) %>%
                                 ungroup() %>%
                                 select(stub.technology, year, share),
                               by = c("new.technology" = "stub.technology", "year")) %>%
      # drop_na(calibrated.value) %>%
      mutate(input.emissions = input.emissions * share) %>%
      rename(stub.technology = new.technology) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissions"]]) ->
      L2241.OutputEmissions_coal_vintage_USA


    # ===================================================
    # Produce outputs

    L2241.StubTechCalInput_elec_coalret_USA %>%
      add_title("Calibration outputs for conventional coal electricity plants and U.S. state") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity outpts are allocated to fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "gcam-usa/states_subregions",
                     "gcam-usa/EIA_coal_generation_2018",
                     "L223.StubTechCalInput_elec",
                     "L223.StubTechEff_elec",
                     "L223.GlobalTechShrwt_elec",
                     "L223.GlobalTechCapFac_elec",
                     "L223.GlobalTechCapital_elec",
                     "L223.GlobalTechOMfixed_elec",
                     "L223.GlobalTechOMvar_elec",
                     "L223.GlobalTechEff_elec",
                     "L223.GlobalTechProfitShutdown_elec") ->
                     # # electricity + cooling technologies
                     # "L2233.StubTechProd_elec_cool",
                     # "L2233.StubTechEff_elec_cool",
                     # "L2233.GlobalTechShrwt_elec_cool",
                     # "L2233.GlobalTechCapFac_elec_cool",
                     # "L2233.GlobalTechCapital_elecPassthru",
                     # "L2233.GlobalTechCapital_elec_cool",
                     # "L2233.GlobalTechOMfixed_elecPassthru",
                     # "L2233.GlobalTechOMvar_elecPassthru",
                     # "L2233.GlobalTechEff_elec_cool",
                     # "L2233.GlobalTechProfitShutdown_elec_cool",
                     # "L2233.GlobalTechCoef_elec_cool") ->
      L2241.StubTechCalInput_elec_coalret_USAagg

    L2241.StubTechEff_elec_coalret_USA %>%
      add_title("Efficiencies of U.S. conventional coal electricity plants in calibration years") %>%
      add_units("Unitless") %>%
      add_comments("Set the same efficiency values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.StubTechEff_elec") ->
      L2241.StubTechEff_elec_coalret_USAagg

    L2241.StubTechSCurve_elec_coalret_USA %>%
      add_title("S-curve shutdown decider for historic U.S. conventional coal electricity plants") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "gcam-usa/A23.elec_tech_coal_retire_SCurve") ->
      L2241.StubTechSCurve_elec_coalret_USAagg

    L2241.GlobalTechShrwt_elec_coalret_USA %>%
      add_title("Shareweights for historic U.S. conventional coal electricity plants") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.GlobalTechShrwt_elec") ->
      L2241.GlobalTechShrwt_elec_coalret_USAagg

    L2241.GlobalTechCapFac_elec_coalret_USA %>%
      add_title("Capacity factors for historic U.S. conventional coal electricity plants") %>%
      add_units("Unitless") %>%
      add_comments("Set the same capacity factor values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.GlobalTechCapFac_elec") ->
      L2241.GlobalTechCapFac_elec_coalret_USAagg

    L2241.GlobalTechCapital_elec_coalret_USA %>%
      add_title("Capital costs of historic U.S. conventional coal electricity plants") %>%
      add_units("1975$ per kW; unitless (fixed.charge.rate)") %>%
      add_comments("Set the same capital cost values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.GlobalTechCapital_elec") ->
      L2241.GlobalTechCapital_elec_coalret_USAagg

    L2241.GlobalTechOMfixed_elec_coalret_USA %>%
      add_title("Fixed OM costs of historic U.S. conventional coal electricity plants") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Set the same fixed OM cost values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.GlobalTechOMfixed_elec") ->
      L2241.GlobalTechOMfixed_elec_coalret_USAagg

    L2241.GlobalTechOMvar_elec_coalret_USA %>%
      add_title("Variable OM costs of historic U.S. conventional coal electricity plants") %>%
      add_units("1975$/MWh") %>%
      add_comments("Set the same variable OM cost values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.GlobalTechOMvar_elec") ->
      L2241.GlobalTechOMvar_elec_coalret_USAagg

    L2241.GlobalTechEff_elec_coalret_USA %>%
      add_title("Efficiencies of historic U.S. conventional coal electricity plants") %>%
      add_units("Unitless") %>%
      add_comments("Set the same efficiency values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.GlobalTechEff_elec") ->
      L2241.GlobalTechEff_elec_coalret_USAagg

    L2241.GlobalTechProfitShutdown_elec_coalret_USA %>%
      add_title("Profit shut-down decider for historic U.S. conventional coal electricity plants") %>%
      add_units("Unitless") %>%
      add_comments("Set the same values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L223.GlobalTechProfitShutdown_elec") ->
      L2241.GlobalTechProfitShutdown_elec_coalret_USAagg


    L2241.StubTechCalInput_coal_vintage_USA %>%
      add_title("Calibration outputs for conventional coal electricity plants by detailed vintage") %>%
      add_units("EJ") %>%
      add_comments("Generation shares by vintage are calculated based on EIA unit-level 2015 data") %>%
      add_comments("Generation shares by vintage are then applied to coal (conv pul) stub-technology for final historical period generation in each state") %>%
      add_comments("Generation in other base years are set to zero to each vintage stub-technology") %>%
      add_precursors("gcam-usa/prime_mover_map",
                     "gcam-usa/EIA_860_generators_existing_2018",
                     "gcam-usa/EIA_923_generator_gen_fuel_2018",
                     "L223.StubTechCalInput_elec") ->
      L2241.StubTechCalInput_coal_vintage_USAagg

    L2241.StubTechEff_coal_vintage_USA %>%
      add_title("Efficiencies of conventional coal electricity plants by detailed vintage in calibration years") %>%
      add_units("Unitless") %>%
      add_comments("Apply the same efficiencies to all vintage groups") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
      add_precursors("L223.StubTechEff_elec") ->
      L2241.StubTechEff_coal_vintage_USAagg

    L2241.StubTechSCurve_coal_vintage_USA %>%
      add_title("Lifetime and retirement parameters for conventional coal electricity plants from 2010 by detailed vintage") %>%
      add_units("Years") %>%
      add_comments("Average lifetime for each vintage group is weighted by capacity, based on EIA unit-level 2015 data") %>%
      add_comments("Only for vintage groups with greater than 20 years of lifetime remaining") %>%
        same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
        add_precursors("gcam-usa/EIA_860_generators_retired_2018") ->
      L2241.StubTechSCurve_coal_vintage_USAagg

    L2241.StubTechProfitShutdown_coal_vintage_USA %>%
      add_title("Profit shutdown decider for conventional coal electricity plants by detailed vintage") %>%
      add_units("Unitless") %>%
      add_comments("Same to all vintage groups") %>%
      same_precursors_as("L2241.StubTechSCurve_coal_vintage_USAagg") ->
      L2241.StubTechProfitShutdown_coal_vintage_USAagg

    L2241.GlobalTechShrwt_coal_vintage_USA %>%
      add_title("Shareweights for conventional coal electricity plants by detailed vintage") %>%
      add_units("Unitless") %>%
      add_comments("Set zero shareweights for all vintage stub-technologies") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") ->
      L2241.GlobalTechShrwt_coal_vintage_USAagg

    L2241.GlobalTechEff_coal_vintage_USA %>%
      add_title("Efficiencies for conventional coal electricity plants by detailed vintage") %>%
      add_units("Unitless") %>%
      add_comments("Same to all vintage groups") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
      add_precursors("L223.GlobalTechEff_elec") ->
      L2241.GlobalTechEff_coal_vintage_USAagg

    L2241.GlobalTechCapFac_coal_vintage_USA %>%
      add_title("Capacity factors for conventional coal electricity plants by detailed vintage") %>%
      add_units("Unitless") %>%
      add_comments("Same to all vintage groups") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
      add_precursors("L223.GlobalTechCapFac_elec") ->
      L2241.GlobalTechCapFac_coal_vintage_USAagg

    L2241.GlobalTechCapital_coal_vintage_USA %>%
      add_title("Capital costs for conventional coal electricity plants by detailed vintage") %>%
      add_units("1975$ per kW; unitless (fixed.charge.rate)") %>%
      add_comments("Same to all vintage groups") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
      add_precursors("L223.GlobalTechCapital_elec") ->
      L2241.GlobalTechCapital_coal_vintage_USAagg

    L2241.GlobalTechOMfixed_coal_vintage_USA %>%
      add_title("Fixed OM costs for conventional coal electricity plants by detailed vintage") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Same to all vintage groups") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
      add_precursors("L223.GlobalTechOMfixed_elec") ->
      L2241.GlobalTechOMfixed_coal_vintage_USAagg

    L2241.GlobalTechOMvar_coal_vintage_USA %>%
      add_title("Variable OM costs for conventional coal electricity plants by detailed vintage") %>%
      add_units("1975$/MWh") %>%
      add_comments("Same to all vintage groups") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
      add_precursors("L223.GlobalTechOMvar_elec") ->
      L2241.GlobalTechOMvar_coal_vintage_USAagg

    L2241.OutputEmissions_elec_coalret_USA %>%
      add_title("GHG and pollutant emissions for historic U.S. conventional coal electricity plants") %>%
      add_units("Tg") %>%
      add_comments("Emissions from L201.OutputEmissions_elec shared out on basis of calibrated inputs") %>%
      same_precursors_as("L2241.StubTechCalInput_elec_coalret_USA_agg") %>%
      add_precursors("L201.OutputEmissions_elec") ->
      L2241.OutputEmissions_elec_coalret_USAagg

    L2241.OutputEmissions_coal_vintage_USA %>%
      add_title("GHG and pollutant emissions for conventional coal electricity plants by detailed vintage") %>%
      add_units("Tg") %>%
      add_comments("Emissions from L201.OutputEmissions_elec shared out on basis of calibrated inputs") %>%
      same_precursors_as("L2241.StubTechCalInput_coal_vintage_USAagg") %>%
      add_precursors("L201.OutputEmissions_elec") ->
      L2241.OutputEmissions_coal_vintage_USAagg


    return_data(L2241.StubTechCalInput_elec_coalret_USAagg,
                L2241.StubTechEff_elec_coalret_USAagg,
                L2241.StubTechSCurve_elec_coalret_USAagg,
                L2241.GlobalTechShrwt_elec_coalret_USAagg,
                L2241.GlobalTechCapFac_elec_coalret_USAagg,
                L2241.GlobalTechCapital_elec_coalret_USAagg,
                L2241.GlobalTechOMfixed_elec_coalret_USAagg,
                L2241.GlobalTechOMvar_elec_coalret_USAagg,
                L2241.GlobalTechEff_elec_coalret_USAagg,
                L2241.GlobalTechProfitShutdown_elec_coalret_USAagg,
                # vintage of existing techs
                L2241.StubTechCalInput_coal_vintage_USAagg,
                L2241.StubTechEff_coal_vintage_USAagg,
                L2241.StubTechSCurve_coal_vintage_USAagg,
                L2241.StubTechProfitShutdown_coal_vintage_USAagg,
                L2241.GlobalTechShrwt_coal_vintage_USAagg,
                L2241.GlobalTechEff_coal_vintage_USAagg,
                L2241.GlobalTechCapFac_coal_vintage_USAagg,
                L2241.GlobalTechCapital_coal_vintage_USAagg,
                L2241.GlobalTechOMfixed_coal_vintage_USAagg,
                L2241.GlobalTechOMvar_coal_vintage_USAagg,
                L2241.OutputEmissions_elec_coalret_USAagg,
                L2241.OutputEmissions_coal_vintage_USAagg)

  } else {
    stop("Unknown command")
  }
}
