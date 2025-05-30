# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L144.building_det_en
#'
#' Calculates global detailed buildings energy data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.end_use_eff}, \code{L144.shell_eff_R_Y}, \code{L144.in_EJ_R_bld_serv_F_Yh}, \code{L144.NEcost_75USDGJ},
#'  \code{L144.internal_gains}, \code{L144.base_service_EJ_serv},\code{L144.base_service_EJ_serv_fuel}, \code{L144.prices_bld}.
#'  The corresponding file in the original data system was \code{LA144.building_det_en.R} (energy level1).
#' @details Calculates building energy consumption, non-energy costs, energy output by service, internal gains, and end-use technology and shell efficiency
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join lag mutate pull select summarise
#' @importFrom tidyr complete replace_na
#' @author AJS July 2017
module_energy_L144.building_det_en_cwf <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/A_regions",
             FILE = "energy/calibrated_techs_bld_det",
             FILE = "energy/A44.cost_efficiency",
             FILE = "energy/A44.internal_gains",

             FILE = "energy/A44.shell_eff_mult_RG3",
             FILE = "energy/A44.tech_eff_mult_RG3",
             FILE = "energy/A44.USA_TechChange",


             FILE = "cwf/A44.USA_TechChange_cwf_adj"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(
             "L144.end_use_eff_cwf",
             "L144.shell_eff_R_Y_cwf",
             "L144.internal_gains_cwf"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_regions <- get_data(all_data, "energy/A_regions")
    calibrated_techs_bld_det <- get_data(all_data, "energy/calibrated_techs_bld_det")
    A44.cost_efficiency <- get_data(all_data, "energy/A44.cost_efficiency", strip_attributes = TRUE)
    A44.internal_gains <- get_data(all_data, "energy/A44.internal_gains")
    
    A44.shell_eff_mult_RG3 <- get_data(all_data, "energy/A44.shell_eff_mult_RG3")
    A44.tech_eff_mult_RG3 <- get_data(all_data, "energy/A44.tech_eff_mult_RG3")
    A44.USA_TechChange <- get_data(all_data, "energy/A44.USA_TechChange")
    A44.USA_TechChange_cwf_adj <- get_data(all_data, "cwf/A44.USA_TechChange_cwf_adj")

    # ===================================================

    . <- CRF <- CapitalCost <- Energy_EJ <- Energy_EJ_SectorFuel <- Energy_adj_EJ <- Energy_final_EJ <-
      Energy_tot_EJ <- Energy_unadj_EJ <- GCAM_region_ID <- GCM <- NEcostPerService <- NonEnergyCost <-
      `O&M cost` <- SRES <- ServiceOutput <- ServiceShare <- UEC <- adjustment <- country <- country_name <-
      curr_table <- efficiency <- fuel <- fuel_share_of_TFEbysector <- has_district_heat <- input.ratio <-
      `installed cost` <- iso <- lifetime <- normal <- normal_RG3 <- region_GCAM3 <- region_subsector <-
      regions_fuel <- scaler <- sector <- sector_fuel <- service <- share_TFEbysector <- share_serv_fuel <-
      share_serv_fuel_RG3 <- subsector <- supp_tech_2 <- supplysector <- technology <- tradbio_region <-
      value_eff <- value_ratio <- value_ratio_2000 <- value_shell <- value_tech <- variable <- year <-
      value <- exponent <- NULL

    # Create list spanning historical and future years
    HIST_FUT_YEARS <- c(HISTORICAL_YEARS, FUTURE_YEARS)

    # Note that RG3, region_GCAM3, and GCAM 3.0 region are used interchangeably.

    # 1A
    # Calculate building end-use shell efficiency by GCAM region ID / GCAM 3.0 region names / supplysector / subsector / technology / year
    # Years will span historical and future time period

    # Write out the tech change table to all desired years, and convert to ratios from a base year
    # A44.USA_TechChange reports improvement rates of technology (annual rate)
    A44.USA_TechChange %>%
      gather_years %>% # Year needs to be integer (or numeric) for the interpolation step below
      # Expand table to include all historical and future years
      group_by(supplysector, technology) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      # NAs will be introduced in residential and commercial shell technology rows
      left_join(calibrated_techs_bld_det, by = c("supplysector", "technology")) %>%
      # Add hydrogen subsector, gas (not calibrated)
      mutate(subsector = if_else(technology == "hydrogen", "gas",subsector)) %>%
      select(supplysector, subsector, technology, year, value) ->
      L144.USA_TechChange

    # Convert the tech change table into ratios (multipliers) from a base year.
    # This will be a step-dependent process
    L144.USA_TechChange %>%
      # Set exponent to incremental year step (i.e., 1 for historical years, 5 for future)
      # Note that using lag in this way will calculate wrong exponent values for the base
      # historical year, but that will be addressed two steps later
      mutate(exponent = year - lag(year, n = 1L),
             value_ratio = (1 + value) ^ exponent,
             # Set base year to 1
             value_ratio = replace(value_ratio, year == HISTORICAL_YEARS[1], 1)) %>%
      # Apply cumprod to each grouping
      group_by(supplysector, subsector, technology) %>%
      mutate(value_ratio = cumprod(value_ratio)) %>%
      ungroup() ->
      L144.USA_TechMult_unadj

    # These technology multipliers assume a base year of the first historical year. However most of the efficiencies are based on data
    # from more recent years. This next part adjusts the scale so that the index year is not the first historical year.
    BASE_TECH_EFF_INDEX_YEAR <- 2000

    L144.USA_TechMult_unadj %>%
      filter(year == BASE_TECH_EFF_INDEX_YEAR) %>%
      select(supplysector, technology, subsector, value_ratio_2000 = value_ratio) ->
      L144.USA_TechMult_2000


    # This table can then be repeated by the number of regions, and multiplied by region-specific
    # adjustment factors (interpolated)

    # Repeat table by number of regions and match in the associated GCAM 3.0 region name
    # NOTE: This just uses an approximate match between the new regions and the GCAM 3.0 regions, based on the first country alphabetically that is
    # matched between the new and old regions. For new composite regions that are quite different from before, this can cause inconsistent mappings

    # Create table to match in GCAM 3.0 region names in next step.
    RG3_GCAMregionID <- unique(select(iso_GCAM_regID, -iso, -country_name))
    # Shell Efficiency Calculation

    # First, interpolate region specific adjustment factors to historical and future years
    # A44.shell_eff_mult_RG3 reports GCAM 3.0 multipliers from USA to other regions for shell efficiency
    # Calculated based on per-capita GDP and heating degree days
    A44.shell_eff_mult_RG3 %>%
      gather_years %>%
      # Expand table to include all historical and future years
      group_by(region_GCAM3) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value_shell = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      select(region_GCAM3, year, value_shell) ->
      A44.shell_eff_mult_RG3_complete
    # 1B
    # Calculate building end-use technology efficiency by GCAM region ID / GCAM 3.0 region names / supplysector / subsector / technology / year
    # Years will span historical and future time period

    # A44.tech_eff_mult_RG3 reports efficiency multipliers from the USA to the given GCAM 3.0 regions.
    # These efficiency multipliers will be used for non-shell technologies, as multipliers for shell technologies
    # were calculated above.
    A44.tech_eff_mult_RG3 %>%
      gather_years %>%
      # Expand table to include all historical and future years
      group_by(region_GCAM3) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value_tech = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      select(region_GCAM3, year, value_tech) ->
      LA44.tech_eff_mult_RG3_complete
    # ===================================================
    # CWF adjustments
    # apply the multipliers to the standard TechChange values to generate L144.USA_TechMult_cwf
    A44.USA_TechChange_cwf_adj %>%
      gather_years %>% # Year needs to be integer (or numeric) for the interpolation step below
      # Expand table to include all historical and future years
      group_by(supplysector, technology) %>%
      complete(year = HIST_FUT_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used in case there are years outside of min-max range, which will be assigned values from closest data
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      # NAs will be introduced in residential and commercial shell technology rows
      left_join(calibrated_techs_bld_det, by = c("supplysector", "technology")) %>%
      select(supplysector, subsector, technology, year, value) ->
      L144.USA_TechChange_cwf_adj





    # apply the adjustments to L144.USA_TechChange to get L144.USA_TechChange_cwf
    L144.USA_TechChange %>%
      left_join_error_no_match(L144.USA_TechChange_cwf_adj %>% rename(adj = value) %>%
                                 select(-subsector), by = c("supplysector","technology","year")) %>%
      mutate(value = value * adj) %>%
      dplyr::select(-adj) ->
      L144.USA_TechChange_cwf

    # Convert the tech change table into ratios (multipliers) from a base year.
    # This will be a step-dependent process
    L144.USA_TechChange_cwf %>%
      # Set exponent to incremental year step (i.e., 1 for historical years, 5 for future)
      # Note that using lag in this way will calculate wrong exponent values for the base
      # historical year, but that will be addressed two steps later
      mutate(exponent = year - lag(year, n = 1L),
             value_ratio = (1 + value) ^ exponent,
             # Set base year to 1
             value_ratio = replace(value_ratio, year == HISTORICAL_YEARS[1], 1)) %>%
      # Apply cumprod to each grouping
      group_by(supplysector, subsector, technology) %>%
      mutate(value_ratio = cumprod(value_ratio)) %>%
      ungroup() ->
      L144.USA_TechMult_unadj_cwf

    # These technology multipliers assume a base year of the first historical year. However most of the efficiencies are based on data
    # from more recent years. This next part adjusts the scale so that the index year is not the first historical year.
    L144.USA_TechMult_unadj_cwf %>%
      # Add column for base year efficiency
      left_join_error_no_match(L144.USA_TechMult_2000, by = c("supplysector", "technology", "subsector")) %>%
      # Adjust efficiencies for all years by dividing by base year efficiency
      mutate(value = value_ratio / value_ratio_2000) %>%
      select(supplysector, technology, subsector, year, value) ->
      L144.USA_TechMult_cwf

    # expand to regions
    L144.USA_TechMult_cwf %>%
      # Expand table by GCAM region IDs
      repeat_add_columns(GCAM_region_names) %>%
      # Match GCAM 3.0 region names using GCAM region ID
      # Some IDs can span multiple regions, as stated above (e.g., 1 covers both USA and Latin America). Select first one.
      left_join_keep_first_only(RG3_GCAMregionID, by = "GCAM_region_ID") %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.TechMult_R_cwf

    # Apply shell efficiency multipliers (by GCAM 3.0 region and year) to get shell efficiency.
    # Note that this produces a final output table.
    L144.TechMult_R_cwf %>%
      # Subset the technology multiplier table so that it includes only shells
      filter(grepl("shell", technology)) %>%
      # Join shell efficiency multipliers (by GCAM 3.0 region and year)
      left_join_error_no_match(A44.shell_eff_mult_RG3_complete, by = c("region_GCAM3", "year")) %>%
      # Multiply value by shell efficiency multiplier
      mutate(value = value * value_shell,
             year = as.integer(year)) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.shell_eff_R_Y_cwf # This is a final output table.

    # Apply efficiency multipliers (by GCAM 3.0 region and year) to get efficiency of energy-consuming techs (no shells)
    L144.TechMult_R_cwf %>%
      # Subset the technology multiplier table so that it includes only energy-consuming techs (no shells)
      filter(!grepl("shell", technology)) %>%
      # Join efficiency multipliers (by GCAM 3.0 region and year)
      left_join_error_no_match(LA44.tech_eff_mult_RG3_complete, by = c("region_GCAM3", "year")) %>%
      # Multiply value by efficiency multiplier
      mutate(value = value * value_tech) ->
      L144.end_use_eff_Index_cwf

    # These values are indexed to the USA in the base year. Unlike shells, the end-use technology values read to the model
    # are not just indices, so need to multiply through by assumed base efficiency levels for each technology

    # First, create two lists, which will be used to exclude district heat and traditional biomass in regions where these
    # are not modeled.
    regions_NoDistHeat <- A_regions %>%
      # 0 indicates district heat is not modeled
      filter(has_district_heat == 0) %>%
      mutate(regions_NoDistHeat = paste(GCAM_region_ID, "district heat")) %>%
      pull(regions_NoDistHeat)

    regions_NoTradBio <- A_regions %>%
      # 0 indicates traditional biomass is not modeled
      filter(tradbio_region == 0) %>%
      mutate(regions_NoTradBio = paste(GCAM_region_ID, "traditional biomass")) %>%
      pull(regions_NoTradBio)

    # Note that this produces a final output table.
    L144.end_use_eff_Index_cwf %>%
      # Join efficiency values (by sector and technology)
      left_join_error_no_match(A44.cost_efficiency, by = c("supplysector", "subsector", "technology")) %>%
      # Multiply by efficiency values
      mutate(value = value * efficiency,
             # Prepare to drop region/subsector combinations where district heat and traditional biomass are not modeled
             region_subsector = paste(GCAM_region_ID, subsector),
             year = as.integer(year)) %>%
      # Drop district heat and traditional biomass in regions where these are not modeled
      filter(!region_subsector %in% c(regions_NoDistHeat, regions_NoTradBio)) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.end_use_eff_cwf # This is a final output table.

    # First, create list pairing supplysector with technology, for which to filter by
    A44.internal_gains %>%
      mutate(supp_tech = paste(supplysector, technology)) %>%
      pull(supp_tech) ->
      supp_tech

    # get internal gains
    L144.end_use_eff_cwf %>%
      # Prepare for filtering
      mutate(supp_tech_2 = paste(supplysector, technology)) %>%
      # Subset only for those in the internal gains assumptions table
      filter(supp_tech_2 %in% supp_tech) ->
      L144.end_use_eff_for_intgains_cwf

    # This is for both historical and future years
    # Note that this produces a final output table.
    L144.end_use_eff_for_intgains_cwf %>%
      left_join_error_no_match(A44.internal_gains, by = c("supplysector", "subsector", "technology")) %>%
      mutate(value = input.ratio / value) %>%
      select(GCAM_region_ID, region_GCAM3, supplysector, subsector, technology, year, value) ->
      L144.internal_gains_cwf # This is a final output table.



    # ===================================================


    L144.end_use_eff_cwf %>%
      add_title("Building end-use technology efficiency by GCAM region ID / GCAM 3.0 region name / supplysector / subsector / technology / year") %>%
      add_units("Unitless efficiency") %>%
      add_comments("End-use tech efficiency is the product of region-specific adjustment factors, tech-specific improvement rates, and tech-specific efficiency levels; with CWF adjustments") %>%
      add_legacy_name("L144.end_use_eff") %>%
      add_precursors("energy/A44.USA_TechChange", "cwf/A44.USA_TechChange_cwf_adj", "energy/calibrated_techs_bld_det", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3") ->  L144.end_use_eff_cwf



    L144.shell_eff_R_Y_cwf %>%
      add_title("Building end-use shell efficiency by GCAM region ID / GCAM 3.0 region name / supplysector / subsector / technology / year") %>%
      add_units("Unitless efficiency") %>%
      add_comments("Shell efficiency is the product of region-specific adjustment factors and tech-specific improvement rates; with CWF adjustments") %>%
      add_legacy_name("L144.shell_eff_R_Y") %>%
      add_precursors("energy/A44.USA_TechChange", "cwf/A44.USA_TechChange_cwf_adj", "energy/calibrated_techs_bld_det", "common/iso_GCAM_regID", "energy/A44.shell_eff_mult_RG3",
                     "common/GCAM_region_names") ->
      L144.shell_eff_R_Y_cwf

    L144.internal_gains_cwf %>%
      add_title("Building Internal Gains by supplysector / subsector / technology / year") %>%
      add_units("Unitless output ratio") %>%
      add_comments("Divide by efficiency of each technology to get internal gain energy released") %>%
      add_comments("Start with table of efficiencies. Subset only the supplysector / subsector / technologies that are in the internal gains assumptions table.") %>%
      add_comments("Then divide the intgains assumptions by the efficiency, matching on supplysector / subsector / technology; with CWF adjustments") %>%
      add_legacy_name("L144.internal_gains") %>%
      add_precursors("energy/A44.USA_TechChange", "cwf/A44.USA_TechChange_cwf_adj", "energy/calibrated_techs_bld_det", "common/iso_GCAM_regID", "energy/A44.tech_eff_mult_RG3",
                     "energy/A_regions", "energy/A44.cost_efficiency", "energy/A44.internal_gains", "common/GCAM_region_names") ->
      L144.internal_gains_cwf




    return_data(L144.end_use_eff_cwf, L144.shell_eff_R_Y_cwf, L144.internal_gains_cwf)
  } else {
    stop("Unknown command")
  }
}
