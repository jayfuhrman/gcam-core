# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_minerals_L2111.resources
#'
#' Set up data tables for mineral supply curves
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2111.Rsrc}, \code{L2111.UnlimitRsrc}, \code{L2111.RsrcPrice}, \code{L2111.UnlimitRsrcPrice}, \code{L2111.SubresourcePriceAdder},
#' \code{L2111.RsrcCalProd}, \code{L2111.ReserveCalReserve}, \code{L2111.RsrcCurves_minerals}, \code{L2111.mineral_regions}
#' \code{L2111.ResSubresourceProdLifetime}, \code{L2111.ResReserveTechLifetime}, \code{L2111.ResReserveTechDeclinePhase},
#' \code{L2111.ResReserveTechProfitShutdown}, \code{L2111.ResReserveTechInvestmentInput}, \code{L2111.ResTechShrwt},
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
           "L1111.mineral_production_R_Yb",
           "L1111.mineral_AnnProdLimit_R_Y",
           "L1111.mineral_AnnResourceLimit_R_Y",
           "L1111.ResSupplyCurves_PricePoints",
           "L1111.mineral_AvgProdLifetime"))
} else if(command == driver.DECLARE_OUTPUTS) {
  return(c("L2111.Rsrc",
           "L2111.UnlimitRsrc",
           "L2111.RsrcPrice",
           "L2111.UnlimitRsrcPrice",
           "L2111.SubresourcePriceAdder",
           "L2111.RsrcCalProd",
           "L2111.ReserveCalReserve",
           "L2111.RsrcCurves_minerals",
           "L2111.mineral_regions",
           "L2111.ResSubresourceProdLifetime",
           "L2111.ResReserveTechLifetime",
           "L2111.ResReserveTechDeclinePhase",
           "L2111.ResReserveTechProfitShutdown",
           "L2111.ResReserveTechInvestmentInput",
           "L2111.ResTechShrwt"))
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

  L1111.mineral_production_R_Yb <- get_data(all_data, "L1111.mineral_production_R_Yb", strip_attributes = TRUE) %>%
    mutate(resource = case_when(Mineral == "Cu" ~ "copper",
                                Mineral == "Li" ~ "lithium",
                                Mineral == "Ni" ~ "nickel"))
  L1111.mineral_AnnProdLimit_R_Y <- get_data(all_data, "L1111.mineral_AnnProdLimit_R_Y", strip_attributes = TRUE) %>%
    mutate(resource = case_when(Mineral == "Cu" ~ "copper",
                                Mineral == "Li" ~ "lithium",
                                Mineral == "Ni" ~ "nickel"))
  L1111.mineral_AnnResourceLimit_R_Y <- get_data(all_data, "L1111.mineral_AnnResourceLimit_R_Y", strip_attributes = TRUE) %>%
    mutate(resource = case_when(Mineral == "Cu" ~ "copper",
                                Mineral == "Li" ~ "lithium",
                                Mineral == "Ni" ~ "nickel"))
  L1111.ResSupplyCurves_PricePoints <- get_data(all_data, "L1111.ResSupplyCurves_PricePoints", strip_attributes = TRUE) %>%
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
  # Taken from zenergy_L210.Resources (fossil resources),
  # we are modeling minerals with the similar resource-reserve approach

  # Kind of a level 1.5 we are going to calculate / update historical energy
  # but the years we choose as the model base years matter
  GCAM_timesteps <- diff(MODEL_BASE_YEARS)
  start.year.timestep <- modeltime.PERIOD0_TIMESTEP
  model_year_timesteps <- tibble(year = MODEL_BASE_YEARS, timestep = c(start.year.timestep, GCAM_timesteps))

  # a pipeline helper function to help back calculate new additions to reserve
  # from historical production
  lag_prod_helper <- function(data) {
    data %>%
      arrange(year_operate, year) %>%
      mutate(max.annual.prod = 0,
             annual.prod = 0,
             reserve = 0,
             cumul.prod = 0) ->
      data_proc

    # operate each model base year one at a time
    for(year_i in MODEL_BASE_YEARS) {
      curr_slice <- data_proc[data_proc$year_operate == year_i, ]
      if(year_i == MODEL_BASE_YEARS[1]) {
        # first year assume all production in this vintage
        curr_slice %>%
          mutate(max.annual.prod = value,
                 annual.prod = value,
                 reserve = annual.prod * lifetime,
                 cumul.prod = annual.prod * timestep) ->
          curr_slice
      } else {
        # pull out the new vintage slice
        curr_slice %>%
          filter(year == year_operate) ->
          new_inv_slice
        # grab the annual production in this year and the timestep which will be needed
        # to calculate production from existing vintages as well
        curr_demand <- new_inv_slice %>% pull(value)
        curr_timestep = new_inv_slice %>% pull(timestep)
        # calculate production from existing vintages first
        prev_slice %>%
          mutate(remain = reserve - cumul.prod,
                 # save previous production so as to be able to linearly adjust depletion
                 prev.annual.prod = annual.prod,
                 # the max annual production may need to get scaled down if this reserve is about
                 # to run out in this timestep
                 max.annual.prod = pmin(max.annual.prod,
                                        pmax((remain - curr_timestep * prev.annual.prod) * 2.0 / curr_timestep + prev.annual.prod, 0.0)),
                 # calculate the production short fall which if positive will drive new  investment
                 supply.shortfall = curr_demand - sum(max.annual.prod),
                 # if the short fall is negative we have over capacity so annual production will need
                 # to scale down from the max
                 annual.prod = if_else(supply.shortfall >= 0, max.annual.prod,
                                       max.annual.prod * (curr_demand / sum(max.annual.prod)))) ->
          prev_slice
        # use the shortfall to set the new vintage production and reserves
        new_prod <- pmax(unique(prev_slice$supply.shortfall), 0.0)
        new_inv_slice %>%
          mutate(max.annual.prod = new_prod,
                 annual.prod = new_prod,
                 reserve = annual.prod * lifetime,
                 cumul.prod = annual.prod * timestep) ->
          new_inv_slice
        # update previous vintage values in the current "slice"
        curr_slice %>%
          filter(year != year_operate) %>%
          mutate(annual.prod = prev_slice$annual.prod,
                 # update cumulative depletion assuming linear change from previous production to current production
                 cumul.prod = prev_slice$cumul.prod + prev_slice$prev.annual.prod * timestep + 0.5 * ( annual.prod - prev_slice$prev.annual.prod) * timestep,
                 # copy forward the rest
                 max.annual.prod = prev_slice$max.annual.prod,
                 reserve = prev_slice$reserve) %>%
          # add back new investment
          bind_rows(new_inv_slice) ->
          curr_slice
      }
      # set the current "slice" back into the original DF
      prev_slice = curr_slice
      data_proc[data_proc$year_operate == year_i, ] = curr_slice
    }
    # ultimately we just need the new vintage reserves
    data_proc %>%
      filter(year == year_operate) %>%
      select(year, value = reserve)
  }
  # Back calculate reserve additions to be exactly enough given our historical production
  # and assumed production lifetime.  Note production lifetimes may not cover the entire
  # historical period and production may dip below capacity making the calculation a bit more
  # tricky.  We use the lag_prod_helper to help project forward production by each historical
  # vintage so we can take this into account.
  # Note: because we are back calculating this our choice of MODEL_BASE_YEARS matters, which is
  # why this is Level2 processing.

  L2111.mineral_production_R_Yb <- L1111.mineral_production_R_Yb %>%
    filter(Year %in% MODEL_BASE_YEARS) %>%
    rename(year = Year) %>%
    # Use LJ because there are some regions with NAs
    left_join(L1111.mineral_AvgProdLifetime, by = c("Mineral", "resource", "region")) %>%
    # omit NAs for now. These are regions with no future resources but very tiny historical production.
    na.omit()


    L2111.mineral_Reserve_Mt_R_Yh <- L2111.mineral_production_R_Yb %>%
      mutate(lifetime = round(Lifetime, digits = 0)) %>%
      rename(technology = resource) %>%
      select(-Mineral, - Lifetime) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year_operate" = "year")) %>%
      filter(year_operate >= year) %>%
      tidyr::nest(data = -c(region, technology)) %>%
      mutate(data = lapply(data, lag_prod_helper)) %>%
      tidyr::unnest(cols = data) %>%
      mutate(value = if_else(is.na(value), 0, value))

  ReserveTotal_Mt_R_F <- L2111.mineral_Reserve_Mt_R_Yh %>%
    group_by(region, technology) %>%
    summarize(value = sum(value)) %>%
    ungroup()


  # CUMULATIVE RESOURCE SUPPLY CURVES ---------------------------------------

  # Put together the price-quantity pairs that comprise the resource supply curves
  L2111.mineral_ResSupplyCurves_R_Y <- L1111.mineral_AnnResourceLimit_R_Y %>%
    # get Q10, Q50, Q90 (based on 10th, 50th, 90th percentile of total resources)
    mutate(Q10 = Resource * 0.1,
           Q50 = Resource * 0.5,
           Q90 = Resource * 0.9,
           Q100 = Resource) %>%
    tidyr::pivot_longer(cols = c(`Q10`, `Q50`, `Q90`, `Q100`), names_to = "percentile", values_to = "Q", values_drop_na = TRUE) %>%
    mutate(percentile = gsub("Q", "", percentile)) %>%
    # There are a couple of NA price values, so use left_join
    left_join(L1111.ResSupplyCurves_PricePoints, by = c("Mineral", "resource", "region", "percentile")) %>%
    # omit NA rows
    na.omit() %>%
    # omit rows with 0 Quantity available
    filter(Q != 0) %>%
    select(resource, region, Year, Units, Q, P, percentile)

# TIME EVOLVING SUPPLY CURVE ----------------------------------------------

# to create time-evolving supply curves, we will treat each additional resource capacity
# that becomes available over time as its own technology. This will be like having "vintaged" resource capacity

  # Calculate the incremental new quantity that becomes available each year:
  ## START IN BASE YEAR - CHECK WITH PRALIT???
  L2111.mineral_incrementResSupplyCurves_R_Y <- L2111.mineral_ResSupplyCurves_R_Y %>%
    filter(Year >= max(MODEL_BASE_YEARS)) %>%
    mutate(percentile = as.numeric(percentile)) %>%
    arrange(resource, region, Units, P, percentile, Year) %>%
    group_by(resource, region, Units, P, percentile) %>%
    mutate(incr_Q = Q - lag(Q, default = 0)) %>%
    ungroup()

  #Assemble the table with each subresource, grade, available, and extractioncost
  L2111.RsrcCurves_minerals_main <- L2111.mineral_incrementResSupplyCurves_R_Y %>%
    mutate(subresource = paste0(resource, "_", Year)) %>%
    arrange(region, resource, subresource, percentile) %>%
    group_by(region, resource, subresource) %>%
    mutate(grade = paste0("grade ", row_number())) %>%
    ungroup() %>%
    mutate(available = incr_Q,
           extractioncost = P) %>%
    select(region, resource, subresource, grade, available, extractioncost) %>%
  # For grades that have the same cost, collapse them into a single grade
    group_by(region, resource, subresource, extractioncost) %>%
    summarise(available = sum(available),
              grade = first(grade),
              .groups = "drop") %>%
    arrange(region, resource, subresource, grade)


  # We need to add a "grade" below the grade 1 called "grade 0" in the first subresource year
  # This will cover all of the production in historical years and will be available at cost 0

  # First calculate total historical production by linearly interpolating between base year cal.production
  L2111.mineral_hist_prod_total <- L2111.mineral_production_R_Yb %>%
    filter(year %in% MODEL_BASE_YEARS) %>%
    group_by(Mineral, resource, region, Units) %>%
    arrange(year, .by_group = TRUE) %>%
    complete(year = c(seq(min(MODEL_BASE_YEARS), max(MODEL_BASE_YEARS),by=1))) %>%
    mutate(value = approx_fun(year, value, rule = 2)) %>%
    dplyr::summarise(value = sum(value)) %>%
    ungroup()

  L2111.RsrcCurves_minerals_grade_historical <- L2111.mineral_hist_prod_total %>%
    mutate(subresource = paste0(resource, "_", MODEL_FINAL_BASE_YEAR),
           grade = "grade 0",
           available = value,
           extractioncost = 0) %>%
    select(region, resource, subresource, grade, available, extractioncost)

  # bind historical grade in with the main supply curve grades
  L2111.RsrcCurves_minerals_main_grade_historical <- bind_rows(L2111.RsrcCurves_minerals_main,
                                                               L2111.RsrcCurves_minerals_grade_historical)

  # We need to add a grade above the final grade, with available 0 and cost higher than the final grade cost
  L2111.RsrcCurves_minerals_final_grade <- L2111.RsrcCurves_minerals_main_grade_historical %>%
    group_by(region, resource, subresource) %>%
    filter(row_number() == n()) %>% # last row per group
    mutate(grade = paste0("grade ", as.numeric(gsub("grade ", "", grade))+1),
           available = 0,
           extractioncost = (extractioncost+0.1) * 1.1) %>% # arbitrary, just need the cost to be higher than the final grade cost
    ungroup()


  # bind all of the grades together for final-output
  L2111.RsrcCurves_minerals <- bind_rows(L2111.RsrcCurves_minerals_main_grade_historical,
                                         L2111.RsrcCurves_minerals_final_grade) %>%
    arrange(region, resource, subresource, grade) %>%
    # convert to the final units needed in the xml
      #available: Mt
      #extractioncost: 1975$/kg
    mutate(available = available/1000, #from kt to Mt
           extractioncost = (extractioncost/1000)*gdp_deflator(1975, base_year = 2020)) #from 2020$/t to 1975$/kg
           ##final-output

  # ===================================================
  # LEVEL2 TABLES

  # Make a generic table that lists region/mineral resource combinations that exist
  # We will use this to filter out combinations for which supply curve data does not exist
  # Also, write this out so we can use this to filter in the trade chunk.
  L2111.mineral_regions <- L2111.RsrcCurves_minerals %>%
    select(region, resource) %>%
    distinct() ##final-output

  # A. Output unit, price unit, market
  L2111.mineral_rsrc_info <- A10.mineral_rsrc_info %>%
    # Repeat and add region to resource assumptions table
    repeat_add_columns(select(GCAM_region_names, region)) %>%
    # Reset regional markets to the names of the specific regions
    mutate(market = if_else(market == "regional", region, market))

  # Currently, copper, lithium and nickel will be treated as depletable resources.
  # All other minerals are still unlimited resources (FOR NOW)

  # L2111.Rsrc: output unit, price unit, and market for depletable resources
  L2111.Rsrc <- L2111.mineral_rsrc_info %>%
    filter(resource_type == "resource") %>%
    select(region, resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
    distinct() %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource"))##final-output

  # L2111.UnlimitRsrc: output unit, price unit, and market for unlimited resources
  L2111.UnlimitRsrc <- L2111.mineral_rsrc_info %>%
    filter(resource_type == "unlimited-resource") %>%
    select(region, unlimited.resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
    distinct() ##final-output

  # L2111.RsrcPrice: historical prices for depletable resources
  # For now, assuming uniform calibration prices across all regions
  L2111.RsrcPrice <- L2111.mineral_rsrc_info %>%
    filter(resource_type == "resource",
           year %in% MODEL_BASE_YEARS) %>%
    select(region, resource = resource, year, price = value) %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output

  # L2111.UnlimitRsrcPrice: prices for unlimited resources
  # update the mineral price by multiplying the fixed-charge-rate (assumed to be 0.13). The mineral cost is considered part of the capital cost,
  # so the mineral prices are multiplied by the fixed-charge-rate to get the annuity, which will later be used for calculating technology levelized
  # cost.
  L2111.UnlimitRsrcPrice <- L2111.mineral_rsrc_info %>%
    filter(resource_type == "unlimited-resource", resource %in% energy.RSRC_MINERAL,
           year %in% MODEL_BASE_YEARS) %>%
    mutate(price = value * 0.13) %>%
    select(region, unlimited.resource = resource, year, price) ##final-output


  # B. Tech change
  # NO TECH CHANGE FOR NOW, REVISIT LATER

  # C. Calibrated production (depletable resources only)
  # L2111.RsrcCalProd: calibrated production of depletable resources
  # NOTE: Assuming all calibrated production goes in the final model base year for now.
  L2111.RsrcCalProd <- L2111.mineral_production_R_Yb %>%
    filter(year %in% MODEL_BASE_YEARS) %>%
    mutate(resource = resource,
           subresource = paste0(resource, "_", max(MODEL_BASE_YEARS))) %>%
    mutate(cal.production = round(value, energy.DIGITS_CALPRODUCTION)) %>%
    select(region, resource, subresource, year, cal.production) %>%
    # Convert to Mt for final-output
    mutate(cal.production = cal.production/1000)  #kt to Mt
  ##final-output

  L2111.ReserveCalReserve <- L2111.mineral_Reserve_Mt_R_Yh %>%
    filter(year %in% MODEL_BASE_YEARS) %>%
    mutate(resource = technology,
           reserve.subresource = paste0(resource, "_", max(MODEL_BASE_YEARS))) %>%
    rename(cal.reserve = value) %>%
    select(region, resource, reserve.subresource, year, cal.reserve) %>%
    # Convert to Mt for final-output
    mutate(cal.reserve = cal.reserve/1000)  #kt to Mt
  ##final-output

  # D. Resource supply curves
  # L2111.RsrcCurves_mineral: supply curves of fossil resources
  # See  L2111.RsrcCurves_minerals above

  # E. Other resource-reserve assumptions
  # L2111.ResSubresourceProdLifetime : we have regionally differentiated average production lifetime
  L2111.ResSubresourceProdLifetime <- L1111.mineral_AvgProdLifetime %>%
    repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
    mutate(reserve.subresource = paste0(resource, "_", year),
           avg.prod.lifetime = Lifetime) %>%
    select(LEVEL2_DATA_NAMES[["ResSubresourceProdLifetime"]]) %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output

  L2111.SubresourcePriceAdder <- A10.mineral_SubresourcePriceAdder %>%
    repeat_add_columns(GCAM_region_names) %>%
    rename(price.adder = value) %>%
    repeat_add_columns(tibble(vintage = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
    mutate(subresource = paste0(resource, "_", vintage)) %>%
    select(LEVEL2_DATA_NAMES[["SubresourcePriceAdder"]]) %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output

  L2111.ResReserveTechLifetime <- A10.mineral_ResReserveTechLifetime %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(vintage = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
    mutate(reserve.subresource = paste0(resource, "_", vintage),
           resource.reserve.technology = reserve.subresource) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechLifetime"]]) %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output

  L2111.ResReserveTechDeclinePhase <- A10.mineral_ResReserveTechDeclinePhase %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(vintage = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
    mutate(reserve.subresource = paste0(resource, "_", vintage),
           resource.reserve.technology = reserve.subresource) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechDeclinePhase"]]) %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output

  L2111.ResReserveTechProfitShutdown <- A10.mineral_ResReserveTechProfitShutdown %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(vintage = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
    mutate(reserve.subresource = paste0(resource, "_", vintage),
           resource.reserve.technology = reserve.subresource) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechProfitShutdown"]]) %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output

  L2111.ResReserveTechInvestmentInput <- L2111.ResSubresourceProdLifetime %>%
    mutate(resource.reserve.technology = reserve.subresource,
           invest_lifetime = avg.prod.lifetime / 2,
           FCR = (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^invest_lifetime) / ((1+socioeconomics.DEFAULT_INTEREST_RATE)^invest_lifetime -1),
           capital.coef = socioeconomics.RESOURCE_CAPITAL_RATIO / FCR,
           minicam.non.energy.input = "investment-cost",
           tracking.market = "capital",
           vintage = as.integer(str_extract(reserve.subresource, "\\d{4}"))) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    select(LEVEL2_DATA_NAMES[["ResReserveTechInvestmentInput"]]) %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output


  # We need to make sure we have at least a shell technology for ALL resources
  # and so we will just use the share weight table to facilitate doing that.
  # For base years, shareweight is set to 1 if there is a production value for the given year
  # For future years, shareweight is set to 1 after the vintage year
  L2111.ResTechShrwt <- A10.mineral_subrsrc_info %>%
    repeat_add_columns(GCAM_region_names) %>%
    repeat_add_columns(tibble(vintage = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
    mutate(subresource = paste0(resource, "_", vintage)) %>%
    # need to write all vintages out to all years, and then remove the irrelevant ones
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    left_join(L2111.RsrcCalProd %>%
                mutate(prod_value = as.double(cal.production)),
              by = c("region", "resource", "subresource", "year")) %>%
    mutate(prod_value = if_else(is.na(prod_value), 0, prod_value),
           technology = subresource,
           share.weight = if_else((year > MODEL_FINAL_BASE_YEAR | prod_value > 0)
                                  & (year >= vintage | vintage == MODEL_FINAL_BASE_YEAR), 1, 0)) %>%
    filter(year %in% MODEL_YEARS) %>%
    select(LEVEL2_DATA_NAMES[["ResTechShrwt"]])  %>%
    # filter to only the regions that have supply curves for that particular mineral.
    semi_join(L2111.mineral_regions, by = c("region", "resource")) ##final-output


  # ===================================================
  # Set up annual production limit constraint as a policy portfolio standard (this will be in a separate XML)

  # ===================================================

  # Produce outputs
  L2111.Rsrc %>%
    add_title("Market information for depletable mineral resources") %>%
    add_units("NA") %>%
    add_comments("A10.mineral_rsrc_info written to all regions") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_rsrc_info") ->
    L2111.Rsrc

  L2111.UnlimitRsrc %>%
    add_title("Market information for unlimited mineral resources") %>%
    add_units("NA") %>%
    add_comments("A10.mineral_rsrc_info written to all regions") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_rsrc_info") ->
    L2111.UnlimitRsrc

  L2111.RsrcPrice %>%
    add_title("Historical prices for depletable mineral resources") %>%
    add_units("1975$/kg for minerals") %>%
    add_comments("A10.mineral_rsrc_info written to all regions") %>%
    same_precursors_as(L2111.Rsrc) ->
    L2111.RsrcPrice

  L2111.UnlimitRsrcPrice %>%
    add_title("Historical prices for depletable mineral resources") %>%
    add_units("1975$/kg for minerals") %>%
    add_comments("A10.mineral_rsrc_info written to all regions") %>%
    same_precursors_as(L2111.UnlimitRsrc) ->
    L2111.UnlimitRsrcPrice

  L2111.SubresourcePriceAdder %>%
    add_title("Adjust calibration price adders in future model years") %>%
    add_units("1975$/kg") %>%
    add_comments("A10.mineral_SubresourcePriceAdder written to all regions") %>%
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_SubresourcePriceAdder") ->
    L2111.SubresourcePriceAdder

  L2111.RsrcCalProd %>%
    add_title("Calibrated production of depletable mineral resources") %>%
    add_units("Mt/yr") %>%
    add_comments("Data from L1111.mineral_production_R_Yb") %>%
    add_precursors("L1111.mineral_production_R_Yb", "common/GCAM_region_names") ->
    L2111.RsrcCalProd

  L2111.ReserveCalReserve %>%
    add_title("Calibrated reserves of depletable mineral resource") %>%
    add_units("Mt cumulative") %>%
    add_comments("Calibrated reserve additions in each model year from which") %>%
    add_comments("the vintage will produce from for the assumed lifetime") %>%
    add_precursors("L1111.mineral_production_R_Yb", "common/GCAM_region_names") ->
    L2111.ReserveCalReserve

  L2111.RsrcCurves_minerals %>%
    add_title("Supply curves of minerals resources") %>%
    add_units("available: Mt; extractioncost: 1975$/kg") %>%
    add_comments("Data from L111.RsrcCurves_EJ_R_Ffos") %>%
    add_precursors("L1111.ResSupplyCurves_PricePoints", "L1111.mineral_production_R_Yb", "common/GCAM_region_names") ->
    L2111.RsrcCurves_minerals

  L2111.mineral_regions %>%
    add_title("Set of regions/mineral resource combinations that exist") %>%
    add_units("NA") %>%
    add_comments("NA") %>%
    same_precursors_as("L2111.RsrcCurves_minerals") ->
    L2111.mineral_regions

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
    add_precursors("common/GCAM_region_names", "minerals/supply/A10.mineral_subrsrc_info") ->
    L2111.ResTechShrwt


  return_data(L2111.Rsrc,
              L2111.UnlimitRsrc,
              L2111.RsrcPrice,
              L2111.UnlimitRsrcPrice,
              L2111.SubresourcePriceAdder,
              L2111.RsrcCalProd,
              L2111.ReserveCalReserve,
              L2111.RsrcCurves_minerals,
              L2111.mineral_regions,
              L2111.ResSubresourceProdLifetime,
              L2111.ResReserveTechLifetime,
              L2111.ResReserveTechDeclinePhase,
              L2111.ResReserveTechProfitShutdown,
              L2111.ResReserveTechInvestmentInput,
              L2111.ResTechShrwt)
} else {
  stop("Unknown command")
}

}

