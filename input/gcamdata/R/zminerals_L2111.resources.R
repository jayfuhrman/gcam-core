# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_minerals_L2111.resources
#'
#' Set up data tables for mineral supply curves
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2111.Rsrc}, \code{L2111.RsrcPrice}, \code{L2111.SubresourcePriceAdder},
#' \code{L2111.RsrcCalProd}, \code{L2111.ReserveCalReserve}, \code{L2111.RsrcCurves_minerals},
#' \code{L2111.ResSubresourceProdLifetime}, \code{L2111.ResReserveTechLifetime}, \code{L2111.ResReserveTechDeclinePhase},
#' \code{L2111.ResReserveTechProfitShutdown}, \code{L2111.ResReserveTechInvestmentInput}, \code{L2111.ResTechShrwt},
#' \code{L2111.Supplysector_dyn}, \code{L2111.SubsectorLogit_dyn}, \code{L2111.SubsectorShrwtFllt_dyn},
#' \code{L2111.StubTech_dyn}, \code{L2111.GlobalTechCoef_dyn}, \code{L2111.GlobalTechShrwt_dyn}, \code{L2111.StubTechEfficiency_dyn}
#' @details Set up data tables for mineral supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na pivot_longer pivot_wider expand_grid
#' @author BY March 2025
#'
module_minerals_L2111.resources <- function(command, ...) {
if(command == driver.DECLARE_INPUTS) {
  return(c(FILE = "common/GCAM_region_names",
           FILE = "minerals/supply/A10.mineral_rsrc_info",
           FILE = "minerals/supply/A10.mineral_subrsrc_info",
           FILE = "minerals/supply/A10.mineral_SubresourcePriceAdder",
           FILE = "minerals/supply/A10.mineral_ResReserveTechDeclinePhase",
           FILE = "minerals/supply/A10.mineral_ResReserveTechLifetime",
           FILE = "minerals/supply/A10.mineral_ResReserveTechProfitShutdown",
           FILE = "minerals/supply/A10.sector_mineral_supply_dynamic",
           FILE = "minerals/supply/A10.subsector_mineral_supply_dynamic",
           FILE = "minerals/supply/A10.tech_coef_mineral_supply_dynamic",
           FILE = "minerals/supply/A10.tech_shrwt_mineral_supply_dynamic",
           "L1111.mineral_production_R_Y_hist",
           "L1111.mineral_AnnProdLimit_R_Y",
           "L1111.mineral_ResSupplyCurves_R_Y",
           "L1111.mineral_AvgProdLifetime"))
} else if(command == driver.DECLARE_OUTPUTS) {
  return(c("L2111.Rsrc",
           "L2111.RsrcPrice",
           "L2111.SubresourcePriceAdder",
           "L2111.RsrcCalProd",
           "L2111.ReserveCalReserve",
           "L2111.RsrcCurves_minerals",
           "L2111.ResSubresourceProdLifetime",
           "L2111.ResReserveTechLifetime",
           "L2111.ResReserveTechDeclinePhase",
           "L2111.ResReserveTechProfitShutdown",
           "L2111.ResReserveTechInvestmentInput",
           "L2111.ResTechShrwt",
           "L2111.Supplysector_dyn",
           "L2111.SubsectorLogit_dyn",
           "L2111.SubsectorShrwtFllt_dyn",
           "L2111.StubTech_dyn",
           "L2111.GlobalTechCoef_dyn",
           "L2111.GlobalTechShrwt_dyn",
           "L2111.StubTechEfficiency_dyn"))
} else if(command == driver.MAKE) {

  all_data <- list(...)[[1]]

  # Load required inputs
  GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A10.mineral_rsrc_info <- get_data(all_data, "minerals/supply/A10.mineral_rsrc_info", strip_attributes = TRUE) %>%
    gather_years
  A10.mineral_subrsrc_info <- get_data(all_data, "minerals/supply/A10.mineral_subrsrc_info", strip_attributes = TRUE)
  A10.mineral_SubresourcePriceAdder <- get_data(all_data, "minerals/supply/A10.mineral_SubresourcePriceAdder", strip_attributes = TRUE) %>%
    gather_years
  A10.mineral_ResReserveTechDeclinePhase <- get_data(all_data, "minerals/supply/A10.mineral_ResReserveTechDeclinePhase", strip_attributes = TRUE)
  A10.mineral_ResReserveTechLifetime <- get_data(all_data, "minerals/supply/A10.mineral_ResReserveTechLifetime", strip_attributes = TRUE)
  A10.mineral_ResReserveTechProfitShutdown <- get_data(all_data, "minerals/supply/A10.mineral_ResReserveTechProfitShutdown", strip_attributes = TRUE)
  A10.sector_mineral_supply_dynamic <- get_data(all_data, "minerals/supply/A10.sector_mineral_supply_dynamic", strip_attributes = TRUE)
  A10.subsector_mineral_supply_dynamic <- get_data(all_data, "minerals/supply/A10.subsector_mineral_supply_dynamic", strip_attributes = TRUE)
  A10.tech_coef_mineral_supply_dynamic <- get_data(all_data, "minerals/supply/A10.tech_coef_mineral_supply_dynamic", strip_attributes = TRUE)
  A10.tech_shrwt_mineral_supply_dynamic <- get_data(all_data, "minerals/supply/A10.tech_shrwt_mineral_supply_dynamic", strip_attributes = TRUE)

  L1111.mineral_production_R_Y_hist <- get_data(all_data, "L1111.mineral_production_R_Y_hist", strip_attributes = TRUE) %>%
    mutate(resource = case_when(Mineral == "Cu" ~ "copper",
                                Mineral == "Li" ~ "lithium",
                                Mineral == "Ni" ~ "nickel"))
  L1111.mineral_AnnProdLimit_R_Y <- get_data(all_data, "L1111.mineral_AnnProdLimit_R_Y", strip_attributes = TRUE) %>%
    mutate(resource = case_when(Mineral == "Cu" ~ "copper",
                                Mineral == "Li" ~ "lithium",
                                Mineral == "Ni" ~ "nickel"))
  L1111.mineral_ResSupplyCurves_R_Y <- get_data(all_data, "L1111.mineral_ResSupplyCurves_R_Y", strip_attributes = TRUE) %>%
    mutate(resource = case_when(Mineral == "Cu" ~ "copper",
                                Mineral == "Li" ~ "lithium",
                                Mineral == "Ni" ~ "nickel"))
  L1111.mineral_AvgProdLifetime <- get_data(all_data, "L1111.mineral_AvgProdLifetime", strip_attributes = TRUE) %>%
    mutate(resource = case_when(Mineral == "Cu" ~ "copper",
                                Mineral == "Li" ~ "lithium",
                                Mineral == "Ni" ~ "nickel"))

  # Check for calibrated resource prices for final historical model year.
  # Otherwise, price behavior is undefined, and so stop process.
  # There should be calibrated prices for all historical model years for
  # full consistency, however.
  if(!(MODEL_FINAL_BASE_YEAR %in% c(unique(A10.mineral_rsrc_info$year)))){
    stop("No calibrated prices for resources in final historical year")
  }

  # ===================================================
  # ------- MINERAL RESOURCE RESERVE ADDITIONS
  # Kind of a level 1.5 we are going to calculate / update historical energy
  # but the years we choose as the model base years matter

  GCAM_timesteps <- diff(MODEL_BASE_YEARS)
  start.year.timestep <- modeltime.PERIOD0_TIMESTEP
  model_year_timesteps <- tibble(year = MODEL_BASE_YEARS, timestep = c(start.year.timestep, GCAM_timesteps))

  # a pipelne helper function to help back calculate new additions to reserve
  # from historical production
  lag_prod_helper <- function(year, value, year_operate, final_year) {
    ret <- value
    for(i in seq_along(year)) {
      if(i == 1) {
        # first year assume all production in this vintage
        ret[i] <- value[i]
      } else if( year_operate[i] > final_year[i]) {
        if(year_operate[i -1] >= final_year[i]) {
          # retired
          ret[i] <- 0
        } else {
          # final timestep that is operating so we must adjust the production
          # by the number of years into the timestep it should have operated
          # incase lifetime and timesteps do not neatly overlap
          ret[i] <- ret[i - 1] * (year_operate[i] - final_year[i]) / (year_operate[i] - year_operate[i-1])
        }
      } else if(year_operate[i] > year[i]) {
        # assume a vintage that as already invested continues at full
        # capacity
        ret[i] <- ret[i -1]
      } else {
        # to determine new investment we take the difference between
        # what the total should be and subtract off production from
        # previous vintages that are still operating
        ret[i] <- 0
        ret[i] <- pmax(value[i] - sum(ret[year_operate == year[i]]), 0)
      }
    }
    ret
  }

  # Back calculate reserve additions to be exactly enough given our historical production
  # and assumed production lifetime.  Note production lifetimes may not cover the entire
  # historical period making the calculation a bit more tricky.  We use the lag_prod_helper
  # to help project forward production by each historical vintage so we can take this into
  # account.
  L2111.mineral_Reserve_Mt_R_Yh <- L1111.mineral_production_R_Y_hist %>%
    filter(Year %in% MODEL_BASE_YEARS) %>%
    # Use LJ because there are some regions with NAs
    left_join(L1111.mineral_AvgProdLifetime, by = c("Mineral", "resource", "region")) %>%
    # omit NAs for now. These are regions with no future resources but very tiny historical production.
    na.omit() %>%
    mutate(Lifetime = round(Lifetime, digits = 0)) %>%
    left_join_error_no_match(model_year_timesteps, by = c("Year" = "year")) %>%
    repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
    mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (Year - timestep + Lifetime))) %>%
    filter(year_operate >= Year - timestep + 1) %>%
    group_by(region, Mineral) %>%
    mutate(value = lag_prod_helper(Year, value, year_operate, final_year)) %>%
    ungroup() %>%
    filter(Year == year_operate) %>%
    mutate(value = value * Lifetime,
           year = Year,
           technology = resource) %>%
    select(-Lifetime, -timestep, -year_operate, -Year, -Mineral, -resource, -Units)


  ReserveTotal_Mt_R_F <- L2111.mineral_Reserve_Mt_R_Yh %>%
    group_by(region, technology) %>%
    summarize(value = sum(value)) %>%
    ungroup()


  # ===================================================
  # LEVEL2 TABLES

  # A. Output unit, price unit, market
  L2111.mineral_rsrc_info <- A10.mineral_rsrc_info %>%
    # Repeat and add region to resource assumptions table
    repeat_add_columns(select(GCAM_region_names, region)) %>%
    # Reset regional markets to the names of the specific regions
    mutate(market = if_else(market == "regional", region, market))
    # TO-DO: filter to only the regions that have supply curves for that particular mineral.

  # All mineral resources will be treated as depletable resources.
  # L2111.Rsrc: output unit, price unit, and market for depletable resources
  L2111.Rsrc <- L2111.mineral_rsrc_info %>%
    filter(resource_type == "resource") %>%
    select(region, resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
    distinct()

  # L2111.RsrcPrice: historical prices for depletable resources
  # For now, assuming uniform calibration prices across all regions
  L2111.RsrcPrice <- L2111.mineral_rsrc_info %>%
    filter(resource_type == "resource",
           year %in% MODEL_BASE_YEARS) %>%
    select(region, resource = resource, year, price = value)

  # B. Tech change
  # NO TECH CHANGE FOR NOW, REVISIT LATER

  # C. Calibrated production (depletable resources only)
  # L2111.RsrcCalProd: calibrated production of depletable resources
  # NOTE: Assuming only one calibrated subresource per depletable resource
  L2111.RsrcCalProd <- L1111.mineral_production_R_Y_hist %>%
    filter(Year %in% MODEL_BASE_YEARS) %>%
    mutate(resource_type = "resource",
           subresource = resource,
           subresource_type = "subresource") %>%
    mutate(cal.production = round(value, energy.DIGITS_CALPRODUCTION)) %>%
    select(region, resource, subresource, year= Year, cal.production)

  L2111.ReserveCalReserve <- L2111.mineral_Reserve_Mt_R_Yh %>%
    rename(cal.reserve = value) %>%
    mutate(resource = technology,
           resource_type = "resource",
           subresource = resource,
           subresource_type = "subresource") %>%
    select(region, resource, reserve.subresource = technology, year, cal.reserve)


  # D. Resource supply curves
  # L2111.RsrcCurves_mineral: supply curves of fossil resources

  # first, make sure supply curve table format is correct
  # Note still have "Year" column here
  L2111.mineral_ResSupplyCurves_R_Y <- L1111.mineral_ResSupplyCurves_R_Y %>%
    mutate(grade = case_when(percentile == 10 ~ "grade 1",
                             percentile == 50 ~ "grade 2",
                             percentile == 90 ~ "grade 3",
                             percentile == 100 ~ "grade 4"),
           subresource = resource,
           available = Q,
           extractioncost = P) %>%
    select(region, resource, subresource, Year, grade, available, extractioncost)

  # We need to calculate "efficiency" parameter (table for Step E), which reflects what fraction of eventual supply curve is available in each year.
  # Thus 2100 (or the max model future year) "efficiency" would be 1
  L2111.mineral_ResSupplyCurves_R_2100 <- L2111.mineral_ResSupplyCurves_R_Y %>%
    filter(Year == max(MODEL_FUTURE_YEARS)) %>%
    select(region, resource, subresource, Year, grade, available)

  L2111.RsrcCurves_minerals_Efficiency_calc <- L2111.mineral_ResSupplyCurves_R_Y %>%
    left_join_error_no_match(L2111.mineral_ResSupplyCurves_R_2100,
                             by = c("region", "resource", "subresource", "grade"), suffix = c("", ".final")) %>%
    mutate(efficiency = available/available.final)

  # The supply curve itself will be based on 2100 supply curve
  L2111.RsrcCurves_minerals <- L2111.mineral_ResSupplyCurves_R_Y %>%
    filter(Year == max(MODEL_FUTURE_YEARS)) %>%
    mutate(available = round(available, energy.DIGITS_RESOURCE)) %>%
    select(region, resource, subresource, grade, available, extractioncost)


  # E. Other resource-reserve assumptions
  # L2111.ResSubresourceProdLifetime : we have regionally differentiated average production lifetime
  L2111.ResSubresourceProdLifetime <- L1111.mineral_AvgProdLifetime %>%
    mutate(reserve.subresource = resource,
           avg.prod.lifetime = Lifetime) %>%
    select(LEVEL2_DATA_NAMES[["ResSubresourceProdLifetime"]])

  L2111.SubresourcePriceAdder <- A10.mineral_SubresourcePriceAdder %>%
    repeat_add_columns(GCAM_region_names) %>%
    rename(price.adder = value) %>%
    select(LEVEL2_DATA_NAMES[["SubresourcePriceAdder"]])

  L2111.ResReserveTechLifetime <- A10.mineral_ResReserveTechLifetime %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechLifetime"]])

  L2111.ResReserveTechDeclinePhase <- A10.mineral_ResReserveTechDeclinePhase %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechDeclinePhase"]])

  L2111.ResReserveTechProfitShutdown <- A10.mineral_ResReserveTechProfitShutdown %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechProfitShutdown"]])

  L2111.ResReserveTechInvestmentInput <- L2111.ResSubresourceProdLifetime %>%
    mutate(resource.reserve.technology = reserve.subresource,
           invest_lifetime = avg.prod.lifetime / 2,
           FCR = (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^invest_lifetime) / ((1+socioeconomics.DEFAULT_INTEREST_RATE)^invest_lifetime -1),
           capital.coef = socioeconomics.RESOURCE_CAPITAL_RATIO / FCR,
           minicam.non.energy.input = "investment-cost",
           tracking.market = "capital") %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechInvestmentInput"]])

  # We need to make sure we have at least a shell technology for ALL resources
  # and so we will just use the share weight table to facilitate doing that.
  L2111.ResTechShrwt <- A10.mineral_subrsrc_info %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    left_join(L2111.RsrcCalProd %>%
                mutate(prod_value = as.double(cal.production)),
              by = c("region", "resource", "subresource", "year")) %>%
    mutate(prod_value = if_else(is.na(prod_value), 0, prod_value),
           technology = subresource,
           share.weight = if_else(year > MODEL_FINAL_BASE_YEAR | prod_value > 0, 1, 0)) %>%
    filter(year %in% MODEL_YEARS) %>%
    select(LEVEL2_DATA_NAMES[["ResTechShrwt"]])


# DYNAMIC SUPPLY CURVE SECTOR ---------------------------------------------

  # F. "Dynamic capacity" supplysector / subsector / technology information
  # Efficiency is set in a supplysector ("dynamic capacity") which takes minicam.energy.input from the resource

  # SUPPLYSECTOR INFO
  # L2111.Supplysector_dyn
  L2111.Supplysector_dyn <- A10.sector_mineral_supply_dynamic %>%
    write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names)

  # SUBSECTOR INFO
  # L2111.SubsectorLogit_dyn
  L2111.SubsectorLogit_dyn <- A10.subsector_mineral_supply_dynamic %>%
    write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names)

  # L2111.SubsectorShrwtFllt_dyn
  L2111.SubsectorShrwtFllt_dyn <- A10.subsector_mineral_supply_dynamic %>%
    write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], LOGIT_TYPE_COLNAME), GCAM_region_names)

  # TECHNOLOGY INFO
  # L2111.GlobalTechShrwt_dyn
  L2111.GlobalTechShrwt_dyn <- A10.tech_shrwt_mineral_supply_dynamic %>%
    gather_years %>%
    # Expand table to include all model base and future years
    complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology)) %>%
    # Extrapolate to fill out values for all years
    # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
    mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
    filter(year %in% MODEL_YEARS) %>% # This will drop 1971
    # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
    select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight)

  # L2111.GlobalTechCoef_dyn
  L2111.GlobalTechCoef_dyn <- A10.tech_coef_mineral_supply_dynamic %>%
    gather_years %>%
    # Expand table to include all model base and future years
    complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
    # Extrapolate to fill out values for all years
    # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
    group_by(supplysector, subsector, technology, minicam.energy.input) %>%
    mutate(coefficient = approx_fun(year, value, rule = 2)) %>%
    ungroup() %>%
    filter(year %in% MODEL_YEARS) %>% # This will drop 1971
    # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
    select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.energy.input, coefficient)

  # L2111.StubTech_dyn
  L2111.StubTech_dyn <- A10.tech_shrwt_mineral_supply_dynamic %>%
    write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
    rename(stub.technology = technology) %>%
    select(LEVEL2_DATA_NAMES[["StubTech"]])

  # L2111.StubTechEfficiency_dyn
  L2111.StubTechEfficiency_dyn <- L2111.RsrcCurves_minerals_Efficiency_calc %>%
    mutate(supplysector = paste(resource, "dynamic-capacity"),
           subsector = paste(resource, "dynamic-capacity"),
           stub.technology = paste(resource, "dynamic-capacity"),
           minicam.energy.input = resource,
           year = Year,
           market.name = region,
           # pMultiplier is set to the same value as the efficiency
           pMult = efficiency) %>%
    distinct() %>%
    select(c(LEVEL2_DATA_NAMES[['StubTechEff']]), pMult)


  # TBD - do we need maxSubResource?


  # ===================================================

  # Produce outputs
  L2111.Rsrc %>%
    add_title("Market information for depletable mineral resources") %>%
    add_units("NA") %>%
    add_comments("A10.mineral_rsrc_info written to all regions") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.minerals_rsrc_info") ->
    L2111.Rsrc

  L2111.RsrcPrice %>%
    add_title("Historical prices for depletable mineral resources") %>%
    add_units("1975$/kg for minerals") %>%
    add_comments("A10.mineral_rsrc_info written to all regions") %>%
    same_precursors_as(L2111.Rsrc) ->
    L2111.RsrcPrice

  L2111.SubresourcePriceAdder %>%
    add_title("Adjust calibration price adders in future model years") %>%
    add_units("1975$/kg") %>%
    add_comments("A10.mineral_SubresourcePriceAdder written to all regions") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_SubresourcePriceAdder") ->
    L2111.SubresourcePriceAdder

  L2111.RsrcCalProd %>%
    add_title("Calibrated production of depletable mineral resources") %>%
    add_units("Mt/yr") %>%
    add_comments("Data from L1111.mineral_production_R_Y_hist") %>%
    add_precursors("L1111.mineral_production_R_Y_hist", "common/GCAM_region_names") ->
    L2111.RsrcCalProd

  L2111.ReserveCalReserve %>%
    add_title("Calibrated reserves of depletable mineral resource") %>%
    add_units("Mt cumulative") %>%
    add_comments("Calibrated reserve additions in each model year from which") %>%
    add_comments("the vintage will produce from for the assumed lifetime") %>%
    add_precursors("L1111.mineral_production_R_Y_hist", "common/GCAM_region_names") ->
    L2111.ReserveCalReserve

  L2111.RsrcCurves_minerals %>%
    add_title("Supply curves of minerals resources") %>%
    add_units("available: Mt; extractioncost: 1975$/kg") %>%
    add_comments("Data from L111.RsrcCurves_EJ_R_Ffos") %>%
    add_precursors("L1111.mineral_ResSupplyCurves_R_Y", "L1111.mineral_production_R_Y_hist", "common/GCAM_region_names") ->
    L2111.RsrcCurves_minerals


  L2111.ResSubresourceProdLifetime %>%
    add_title("Average production lifetime for reserve subresource") %>%
    add_units("Years") %>%
    add_comments("Used to annualize production of the cumulative resource reserve") %>%
    add_precursors("common/GCAM_region_names", "L1111.mineral_AvgProdLifetime") ->
    L2111.ResSubresourceProdLifetime

  L2111.ResReserveTechLifetime %>%
    add_title("Resource reserve technology lifetime") %>%
    add_units("Years") %>%
    add_comments("Resource well / mine lifetime over which the reserve will be produced / depleted") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_ResReserveTechLifetime") ->
    L2111.ResReserveTechLifetime

  L2111.ResReserveTechDeclinePhase %>%
    add_title("Resource reserve technology decline phase percent") %>%
    add_units("fraction") %>%
    add_comments("When the total reserve has been depleted to this percent the production") %>%
    add_comments("will move into a linear decline phase.") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_ResReserveTechDeclinePhase") ->
    L2111.ResReserveTechDeclinePhase

  L2111.ResReserveTechProfitShutdown %>%
    add_title("Resource reserve technology profit shutdown decider") %>%
    add_units("NA") %>%
    add_comments("Resource profit shutdown to characterize a well / mine's ability scale back") %>%
    add_comments("production under unprofitable conditions") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_ResReserveTechProfitShutdown") ->
    L2111.ResReserveTechProfitShutdown

  L2111.ResReserveTechInvestmentInput %>%
    add_title("Non-energy input to keep track of resource curve investment cost") %>%
    add_units("NA") %>%
    add_comments("A resource reserve tech needs an input to keep track of the resource") %>%
    add_comments("curves cost that was used when the tech was invested.  It will be") %>%
    add_comments("for calculating shutdown deciders but also, this input will track") %>%
    add_comments("capital demands so those parameters are also read in") %>%
    same_attributes_as(L2111.ResSubresourceProdLifetime) ->
    L2111.ResReserveTechInvestmentInput

  L2111.ResTechShrwt %>%
    add_title("Share weights for technologies in resources") %>%
    add_units("NA") %>%
    add_comments("Share weights won't matter for resource technologies as there") %>%
    add_comments("is no competetion between technologies.") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.subrsrc_info") ->
    L2111.ResTechShrwt

  L2111.Supplysector_dyn %>%
    add_title("Dynamic minerals supply sector information") %>%
    add_units("Output, input, and price units are as listed; exponent is unitless") %>%
    add_comments("Dynamic minerals supply sector information was expanded to include GCAM region names") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.sector_mineral_supply_dynamic") ->
    L2111.Supplysector_dyn

  L2111.SubsectorLogit_dyn %>%
    add_title("Subsector logit exponents of dynamic minerals supply sector") %>%
    add_units("Unitless") %>%
    add_comments("Table on subsector logit exponents was expanded to include GCAM region names") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.subsector_mineral_supply_dynamic") ->
    L2111.SubsectorLogit_dyn

  L2111.SubsectorShrwtFllt_dyn %>%
    add_title("Subsector shareweights of dynamic minerals supply sectors") %>%
    add_units("Unitless") %>%
    add_comments("Table on subsector shareweights was expanded to include GCAM region names") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.subsector_mineral_supply_dynamic") ->
    L2111.SubsectorShrwtFllt_dyn

  L2111.StubTech_dyn %>%
    add_title("Identification of stub technologies of dynamic minerals supply sectors") %>%
    add_units("Not Applicable") %>%
    add_comments("Technology list in the global shareweight table for dynamic minerals supply was expanded to include GCAM regions") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.tech_shrwt_mineral_supply_dynamic") ->
    L2111.StubTech_dyn

  L2111.GlobalTechCoef_dyn %>%
    add_title("Dynamic minerals supply global technology coefficients across base model years") %>%
    add_units("Unitless") %>%
    add_comments("Global technology coefficients were interpolated across all base model years") %>%
    add_precursors("minerals/supply/A10.tech_coef_mineral_supply_dynamic") ->
    L2111.GlobalTechCoef_dyn

  L2111.GlobalTechShrwt_dyn %>%
    add_title("Shareweights of carbon storage technologies across base model years") %>%
    add_units("Unitless") %>%
    add_comments("Shareweights of global technologies were interpolated across all base model years") %>%
    add_precursors("energy/A10.tech_shrwt_mineral_supply_dynamic") ->
    L2111.GlobalTechShrwt_dyn

  L2111.StubTechEfficiency_dyn %>%
    add_title("Dynamic minerals supply efficiencies and pMultiplier") %>%
    add_units("Unitless") %>%
    add_comments("Regionally calibrated scaling limits for minerals relative to maximum regional resources") %>%
    add_comments("pMultiplier scales down costs by the same factor as the efficiency parameter in order to make supply curves have same slope as original.") ->
    L2111.StubTechEfficiency_dyn

  return_data(L2111.Rsrc,
              L2111.RsrcPrice,
              L2111.SubresourcePriceAdder,
              L2111.RsrcCalProd,
              L2111.ReserveCalReserve,
              L2111.RsrcCurves_minerals,
              L2111.ResSubresourceProdLifetime,
              L2111.ResReserveTechLifetime,
              L2111.ResReserveTechDeclinePhase,
              L2111.ResReserveTechProfitShutdown,
              L2111.ResReserveTechInvestmentInput,
              L2111.ResTechShrwt,
              L2111.Supplysector_dyn,
              L2111.SubsectorLogit_dyn,
              L2111.SubsectorShrwtFllt_dyn,
              L2111.StubTech_dyn,
              L2111.GlobalTechCoef_dyn,
              L2111.GlobalTechShrwt_dyn,
              L2111.StubTechEfficiency_dyn)
} else {
  stop("Unknown command")
}

}

