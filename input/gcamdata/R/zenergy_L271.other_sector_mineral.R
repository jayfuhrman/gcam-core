# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# module_energy_L271.other_sector_mineral

#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author Author name
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
module_energy_L271.other_sector_mineral <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "minerals/other/A271.cmm_historical_demand_all.csv",
             FILE = "minerals/other/A271.cmm_historical_demand_sector.csv",
             FILE = "minerals/other/A271.cmm_other_sector_region_share.csv",
             FILE = "minerals/other/A271.cmm_sector_2020_demand.csv",
             FILE = "minerals/other/A271.cmm_sector_retire.csv",
             FILE = "minerals/other/A271.sector.csv",
             FILE = "minerals/other/A271.tech_input.csv",
             FILE = "minerals/other/A271.demand.csv",
             "L201.Pop_gSSP2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.Supplysector_mineral_other_sector",
             "L271.SubsectorLogit_mineral_other_sector",
             "L271.SubsectorShrwtFllt_mineral_other_sector",
             "L271.TechShrwt_mineral_other_sector",
             "L271.TechCoef_mineral_other_sector",
             "L271.TechCost_mineral_other_sector",
             "L271.PerCapitaBased_mineral_other_sector",
             "L271.IncomeElasticity_mineral_other_sector",
             "L271.PriceElasticity_mineral_other_sector",
             "L271.aeei_mineral_other_sector",
             "L271.regional_cmm_historical_demand_other_sector",
             "L271.GlobalTechSCurve_en",
             "L271.GlobalTechProfitShutdown_en"
             ))  # input produced by another chunk
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # printlog("Historical GDP and per-capita GDP by state")

    # Load data
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes = TRUE)
    A271.cmm_historical_demand_all <- get_data(all_data, "minerals/other/A271.cmm_historical_demand_all.csv",strip_attributes = TRUE)
    A271.cmm_historical_demand_sector <- get_data(all_data, "minerals/other/A271.cmm_historical_demand_sector.csv",strip_attributes = TRUE)
    A271.cmm_other_sector_region_share <- get_data(all_data, "minerals/other/A271.cmm_other_sector_region_share.csv",strip_attributes = TRUE)
    A271.cmm_sector_2020_demand <- get_data(all_data, "minerals/other/A271.cmm_sector_2020_demand.csv",strip_attributes = TRUE)
    A271.cmm_sector_retire <- get_data(all_data, "minerals/other/A271.cmm_sector_retire.csv",strip_attributes = TRUE)
    A271.sector <- get_data(all_data, "minerals/other/A271.sector.csv",strip_attributes = TRUE)
    A271.tech_input <- get_data(all_data, "minerals/other/A271.tech_input.csv",strip_attributes = TRUE)
    A271.demand <- get_data(all_data, "minerals/other/A271.demand.csv",strip_attributes = TRUE)
    L201.Pop_gSSP2 <- get_data(all_data, "L201.Pop_gSSP2",strip_attributes = TRUE)

    A271.sector %>%
      left_join_error_no_match(A271.tech_input, by = "supplysector") %>%
      bind_cols(A271.demand) %>%
      mutate(logit.year.fillout = MODEL_YEARS[1]) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(logit.type = NA) ->
      L271.assumptions_all

    L271.assumptions_all %>%
      select(LEVEL2_DATA_NAMES$Supplysector, logit.type) ->
      L271.Supplysector_mineral_other_sector
    # Outputs -- Supply sector information

    L271.assumptions_all %>%
      select(LEVEL2_DATA_NAMES$SubsectorLogit, logit.type) ->
      L271.SubsectorLogit_mineral_other_sector
    # Outputs -- Subsector logit detail

    L271.assumptions_all %>%
      mutate(year.fillout = MODEL_YEARS[1],
             share.weight = 1) %>%
      # ^^ share weights are 1 due to no competition
      select(LEVEL2_DATA_NAMES$SubsectorShrwtFllt) ->
      L271.SubsectorShrwtFllt_mineral_other_sector
    # Outputs -- Subsector shareweights

    L271.assumptions_all %>%
      mutate(share.weight = 1) %>%
      # ^^ share weights are 1 due to no competition
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = stub.technology) %>%

      # If a xml file does not have the global database, typically we use "technology" in the regional database,
      # If a xml file has the global database, typically we use "stub.technology" in the regional database
      select(LEVEL2_DATA_NAMES$TechShrwt) ->
      L271.TechShrwt_mineral_other_sector
    # Outputs -- Technology shareweights

    L271.assumptions_all %>%
      select(GCAM_region_ID, region, supplysector, subsector, technology = stub.technology, minicam.energy.input) %>%
      # ^^ unrestricted left_join allows row expansion for all model years
      mutate(market.name = region,
             coefficient = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$TechCoef) ->
      L271.TechCoef_mineral_other_sector

    # Outputs -- Other sector mineral demand coefficient.

    L271.assumptions_all %>%
      mutate(input.cost = 60) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = stub.technology) %>%
      select(LEVEL2_DATA_NAMES$TechCost) ->
      L271.TechCost_mineral_other_sector

    # Outputs -- Other sector mineral non-energy cost

    L271.assumptions_all %>%
      select(LEVEL2_DATA_NAMES$PerCapitaBased) ->
      L271.PerCapitaBased_mineral_other_sector

    # Outputs -- Other sector mineral final demand as per-capita based

    L271.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$IncomeElasticity) ->
      L271.IncomeElasticity_mineral_other_sector

    # Outputs -- income elasticity projections mineral other sector

    L271.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$PriceElasticity) ->
      L271.PriceElasticity_mineral_other_sector

    # Outputs -- price elasticity projections mineral other sector

    L271.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$aeei) ->
      L271.aeei_mineral_other_sector

    # Outputs -- demand efficiency projections mineral other sector


    A271.cmm_historical_sector_demand_ratio <-
      A271.cmm_historical_demand_all %>%
      # we don't have 1975 data, so we just assume 1975 demand = 1990 demand * 0.6
      mutate(`1975` = `1990`*0.6,
             ratio_1975 = `1975`/`2020`,
             ratio_1990 = `1990`/`2020`,
             ratio_2005 = `2005`/`2020`,
             ratio_2010 = `2010`/`2020`,
             ratio_2015 = `2015`/`2020`) %>%
      select(resource, unit, `1975` = ratio_1975 , `1990` = ratio_1990 , `2005` = ratio_2005 , `2010` = ratio_2010 , `2015` = ratio_2015)%>%
      gather_years() %>%
      rename(ratio = value)

    A271.cmm_historical_sector_demand <-
      A271.cmm_historical_sector_demand_ratio %>%
      left_join(A271.cmm_sector_2020_demand,
                by = c("resource",  "unit")) %>%
      rename(y2020 = `2020`) %>%
      mutate(demand = ratio * y2020) %>%
      select(resource,  unit,   year, value = demand)


    A271.cmm_historical_demand_other_sector <-
      A271.cmm_historical_demand_all %>%
      # we don't have 1975 data, so we just assume 1975 demand = 1990 demand * 0.6
      mutate(`1975` = `1990`*0.6) %>%
      gather_years() %>%
      filter(year != 2020) %>%
      rename(annual_total_demand = value) %>%
      left_join(A271.cmm_historical_sector_demand %>%
                  rename(sector_demand = value),
                by = c("resource", "unit", "year")) %>%
      mutate(value = (annual_total_demand - sector_demand)*0.85) %>%
      select(resource, unit, year, value)

    A271.pop_region_share <-
      L201.Pop_gSSP2 %>%
      filter(year %in% c(seq(1975, 2015, 5))) %>%
      group_by(year) %>%
      mutate(share = totalPop/sum(totalPop)) %>%
      select(-totalPop) %>%
      left_join(GCAM_region_names, by = c("region")) %>%
      repeat_add_columns(A271.cmm_historical_demand_other_sector %>% select(resource) %>% distinct())

    L271.regional_cmm_historical_demand_other_sector <-
      A271.pop_region_share %>%
      left_join(A271.cmm_historical_demand_other_sector,
                by = c("resource", "year")) %>%
      mutate(value = value * share) %>%
      left_join(A271.tech_input %>%
                  select(supplysector, minicam.energy.input),
                by = c("resource" = "minicam.energy.input")) %>%
      select(region, energy.final.demand = supplysector, year, base.service = value) %>%
      ungroup()



    # Retirement information
    A271.cmm_sector_retire %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A271.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    A271.globaltech_retirement_base %>%
      # mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      A271.globaltech_retirement_future

    # filters base years from original and then appends future years
    A271.globaltech_retirement_base %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(A271.globaltech_retirement_future) ->
      A271.globaltech_retirement


    A271.globaltech_retirement %>%
      filter(!is.na(A271.globaltech_retirement$half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L271.GlobalTechSCurve_en
    # Outputs -- lifetime s-curve mineral other sector

    A271.globaltech_retirement %>%
      filter(!is.na(A271.globaltech_retirement$median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L271.GlobalTechProfitShutdown_en
    # Outputs -- profit shutdown mineral other sector



    # Process...



    # Produce outputs, add appropriate flags and comments
    L271.Supplysector_mineral_other_sector %>%
      add_title("supply sector logit info for mineral demand in other sector") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.Supplysector_mineral_other_sector") %>%
      add_comments("supply sector logit info for mineral demand in other sector") ->
      L271.Supplysector_mineral_other_sector

    L271.SubsectorLogit_mineral_other_sector %>%
      add_title("subsector logit info for mineral demand in other sector") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.SubsectorLogit_mineral_other_sector") %>%
      add_comments("subsector logit info for mineral demand in other sector") ->
      L271.SubsectorLogit_mineral_other_sector

    L271.SubsectorShrwtFllt_mineral_other_sector %>%
      add_title("subsector share-weight info for mineral demand in other sector") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.SubsectorShrwtFllt_mineral_other_sector") %>%
      add_comments("subsector share-weight info for mineral demand in other sector") ->
      L271.SubsectorShrwtFllt_mineral_other_sector

    L271.TechShrwt_mineral_other_sector %>%
      add_title("technology share-weight info for mineral demand in other sector") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.TechShrwt_mineral_other_sector") %>%
      add_comments("technology share-weight info for mineral demand in other sector") ->
      L271.TechShrwt_mineral_other_sector

    L271.TechCoef_mineral_other_sector %>%
      add_title("mineral demand coefficient in other sector") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.TechCoef_mineral_other_sector") %>%
      add_comments("mineral demand coefficient in other sector") ->
      L271.TechCoef_mineral_other_sector

    # L271.TechCoef_mineral_other_sector

    L271.TechCost_mineral_other_sector %>%
      add_title("non energy cost of other sector that have mineral demand") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.TechCost_mineral_other_sector") %>%
      add_comments("non energy cost of other sector that have mineral demand") ->
      L271.TechCost_mineral_other_sector


    L271.PerCapitaBased_mineral_other_sector %>%
      add_title("Per-capital based final energy demand switch (mineral demand in other sector)") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.PerCapitaBased_mineral_other_sector") %>%
      add_comments("Per-capital based final energy demand switch (mineral demand in other sector)") ->
      L271.PerCapitaBased_mineral_other_sector

    L271.IncomeElasticity_mineral_other_sector %>%
      add_title("Income elasticity projections (mineral demand in other sector)") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.IncomeElasticity_mineral_other_sector") %>%
      add_comments("Income elasticity projections (mineral demand in other sector)") ->
      L271.IncomeElasticity_mineral_other_sector

    L271.PriceElasticity_mineral_other_sector %>%
      add_title("Price elasticity projections (mineral demand in other sector)") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.PriceElasticity_mineral_other_sector") %>%
      add_comments("Price elasticity projections (mineral demand in other sector)") ->
      L271.PriceElasticity_mineral_other_sector

    L271.aeei_mineral_other_sector %>%
      add_title("Demand efficiency projections (mineral demand in other sector)") %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names",
                     "minerals/other/A271.sector.csv",
                     "minerals/other/A271.tech_input.csv",
                     "minerals/other/A271.demand.csv") %>%
      add_legacy_name("L271.aeei_mineral_other_sector") %>%
      add_comments("Demand efficiency projections (mineral demand in other sector)") ->
      L271.aeei_mineral_other_sector

    L271.regional_cmm_historical_demand_other_sector %>%
      add_title("mineral demand in other sector in historical year by region") %>%
      add_units("None") %>%
      add_precursors("minerals/other/A271.cmm_historical_demand_all.csv",
                     "minerals/other/A271.cmm_historical_demand_sector.csv",
                     "minerals/other/A271.cmm_other_sector_region_share.csv",
                     "minerals/other/A271.cmm_sector_2020_demand.csv",
                     "L201.Pop_gSSP2") %>%
      add_legacy_name("L271.regional_cmm_historical_demand_other_sector") %>%
      add_comments("mineral demand in other sector in historical year by region") ->
      L271.regional_cmm_historical_demand_other_sector

    L271.GlobalTechSCurve_en %>%
      add_title("lifetime s-curve for other mineral sector") %>%
      add_units("None") %>%
      add_precursors("minerals/other/A271.cmm_sector_retire.csv") %>%
      add_legacy_name("L271.GlobalTechSCurve_en") %>%
      add_comments("lifetime s-curve for other mineral sector") ->
      L271.GlobalTechSCurve_en

    L271.GlobalTechProfitShutdown_en %>%
      add_title("profit shutdown for other mineral sector") %>%
      add_units("None") %>%
      add_precursors("minerals/other/A271.cmm_sector_retire.csv") %>%
      add_legacy_name("L271.GlobalTechProfitShutdown_en") %>%
      add_comments("profit shutdown for other mineral sector") ->
      L271.GlobalTechProfitShutdown_en


    return_data(L271.Supplysector_mineral_other_sector,
                L271.SubsectorLogit_mineral_other_sector,
                L271.SubsectorShrwtFllt_mineral_other_sector,
                L271.TechShrwt_mineral_other_sector,
                L271.TechCoef_mineral_other_sector,
                L271.TechCost_mineral_other_sector,
                L271.PerCapitaBased_mineral_other_sector,
                L271.IncomeElasticity_mineral_other_sector,
                L271.PriceElasticity_mineral_other_sector,
                L271.aeei_mineral_other_sector,
                L271.regional_cmm_historical_demand_other_sector,
                L271.GlobalTechSCurve_en,
                L271.GlobalTechProfitShutdown_en
                )
  } else {
    stop("Unknown command")
  }
}
