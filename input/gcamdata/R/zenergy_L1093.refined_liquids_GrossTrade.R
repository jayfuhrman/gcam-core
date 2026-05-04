# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1093.refined_liquids_GrossTrade
#'
#' Reads pre-processed IEA refined liquids production, consumption, imports and exports data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{LB1092.Tradebalance_refined_liquids_EJ_R_Y}, \code{L1093.en_bal_EJ_liquids_total}, \code{LB1092.GCAM_REG_LIQUIDS_PROD_agg}, \code{LB1092.GCAM_BIO_LIQUIDS_PROD_agg"}, \code{LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg}, \code{L1093.IO_R_oilrefining_F_Yh}.
#' @importFrom dplyr filter if_else mutate select distinct coalesce across summarise left_join full_join group_by ungroup rename replace_na bind_rows
#' @author Siddarth Durga, Maggie Liu (Jan 2025)
module_energy_L1093.refined_liquids_GrossTrade <- function(command, ...){
  MODULE_INPUTS <- c(
    FILE = "common/GCAM_region_names",
    FILE = "energy/A22.globaltech_coef_ctlgtl",
    FILE = "energy/mappings/IEA_product_fuel_liquids",
    FILE = "energy/mappings/Liquids_Trade_GCAM_regID",
    #FILE = "energy/mappings/IEA_product_LHV",
    FILE = "energy/mappings/liquids_mapping",
    FILE = "energy/Resourcetradeearth_RefinedLiquids_2015",
    "L121.in_EJ_R_TPES_liq_Yh",
    "L122.in_EJ_R_refining_F_Yh",
    "L122.out_EJ_R_refining_F_Yh",
    "L101.detailed_refined_liquids_EJ_R_Yh",
    "L1012.en_bal_EJ_R_Si_Fi_Yh",

    # REFLIQ INDUSTRIAL
    "L2326.StubTechCalInput_aluminum",
    "L2321.StubTechCalInput_cement_heat",
    "L2325.StubTechCalInput_chemical",
    "L271.StubTechProd_desal",
    "L271.GlobalTechCoef_desal",
    "L223.StubTechCalInput_elec",
    "L2322.StubTechProd_FertProd",
    "L2322.GlobalTechCoef_Fert",
    "L2322.StubTechCoef_Fert",
    "L2328.StubTechCalInput_food_heat",
    "L224.StubTechCalInput_heat",
    "L232.StubTechCalInput_indenergy",
    "L232.StubTechCalInput_indfeed",
    "L2323.StubTechCoef_iron_steel",
    "L2323.StubTechProd_iron_steel",
    "L2324.StubTechCalInput_Off_road",
    "L2327.StubTechCalInput_paper_heat",

    # REFLIQ ENDUSE
    "L254.StubTranTechCalInput",
    "L242.StubTechCalInput_bld"
  )

  MODULE_OUTPUTS <- c(
    "LB1092.Tradebalance_refined_liquids_EJ_R_Y",
    "LB1092.GCAM_REG_LIQUIDS_PROD_agg",
    "LB1092.GCAM_BIO_LIQUIDS_PROD_agg",
    "LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg",
    "L1093.en_bal_EJ_liquids_total",
    "L1093.IO_R_oilrefining_F_Yh"
  )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence data-masked variable package check
    Year <- Exporter <- Importer <- region_GCAM3 <- production <- consumption <-
      exports <- imports <- metric <- exports.un <- imports.un <- transfers <-
      consumption.un <- sector_2 <- region <- value <- sector <- fuel <- year <-
      production.un <- . <- NULL

    all_data <- list(...)[[1]]
    #all_data <- load_from_cache(MODULE_INPUTS)
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    #==========================================================================================
    # Estimate refined liquids production by product categories (e.g, Gasoline, DFO, RFO etc.)
    #==========================================================================================

    # Aggregate by GCAM region, sector, and aggregate fuel category
    L1093.detailed_refined_liquids_EJ_R_Yh <- L101.detailed_refined_liquids_EJ_R_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(IEA_product_fuel_liquids, by = "PRODUCT") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # don't include direct crude consumption in the refining sector
      # include biofuels so can compare total production to total consumption
      filter(fuel_category != "Unrefined_Liquids") %>%
      group_by(region, sector, fuel_category, year) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      # TODO: south america northern has 4 periods with LPG as a net refining input
      # Convert net_oil refining (TREFINER + EREFINER) to positive values
      # because the transfers category more than covers all consumption sectors
      # setting this value to 0 and will settle globally with the trade balance
      mutate(value = if_else(sector == "net_oil refining", -value, value),
             value = if_else(sector == "net_oil refining" & value < 0, 0, value),
             year = as.integer(year))

    L1093.out_EJ_R_liquids_prod_F_Yh <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
      # CTL/GTL outputs 'other hydrocarbons', removed above as part of
      # 'unrefined liquids'. These are then moved to products through the
      # Transfers FLOW code (aggregated with other transfer volumes).
      filter(sector %in% c("net_oil refining", "transfers")) %>%
      group_by(region, fuel_category, year) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      # some regions have more product transfers than refining output (large
      # imports for transfer) can't produce a negative so shift that volume into
      # the trade balance
      mutate(value = if_else(value < 0, 0, value)) %>%
      rename(production = value)


    #===========================================================================
    # Calculate refined liquids consumption by GCAM sector and overall
    #===========================================================================
    # TODO: update this constant with a mapping file
    REFLIQ_BUCKETS <- "refined liquids|industry feedstock|needle coke|refined biofuel"

    # TODO: refactor all of this
    # REFLIQ INDUSTRIAL
    a_aluminum <- L2326.StubTechCalInput_aluminum %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_cement <- L2321.StubTechCalInput_cement_heat %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value)  %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_chem <- L2325.StubTechCalInput_chemical %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value)  %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_elec <- L223.StubTechCalInput_elec %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value)  %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_food <- L2328.StubTechCalInput_food_heat %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_heat <- L224.StubTechCalInput_heat %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_offroad <- L2324.StubTechCalInput_Off_road %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value)  %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_paper <- L2327.StubTechCalInput_paper_heat %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_indfeed <- L232.StubTechCalInput_indfeed %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_inden <- L232.StubTechCalInput_indenergy %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    # saved as a coef for irnstl, fert, desal so need to calc from production
    irnstl_coef <- L2323.StubTechCoef_iron_steel %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, stub.technology, minicam.energy.input, coefficient)
    irnstl_prod <- L2323.StubTechProd_iron_steel %>%
      select(region, year, supplysector, subsector, stub.technology, calOutputValue)

    a_irnstl <- irnstl_prod %>%
      left_join(irnstl_coef, by = c("region", "year", "supplysector", "subsector", "stub.technology")) %>%
      mutate(calibrated.value = calOutputValue * coefficient) %>%
      filter(calibrated.value > 0) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    fert_coef <- (L2322.StubTechCoef_Fert) %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, stub.technology, minicam.energy.input, coefficient)
    fert_prod <- L2322.StubTechProd_FertProd %>%
      select(region, year, supplysector, subsector, stub.technology, calOutputValue)
    a_fert <- fert_prod %>%
      left_join(fert_coef, by = c("region", "year", "supplysector", "subsector", "stub.technology")) %>%
      mutate(calibrated.value = calOutputValue * coefficient) %>%
      filter(calibrated.value > 0) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    desal_coef <- (L271.GlobalTechCoef_desal) %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(year, supplysector = sector.name, subsector = subsector.name,
             stub.technology = technology, minicam.energy.input, coefficient)
    desal_prod <- L271.StubTechProd_desal %>%
      select(region, year, supplysector, subsector, stub.technology, calOutputValue)

    a_desal <- desal_prod %>%
      left_join(desal_coef, by = c("year", "supplysector", "subsector", "stub.technology")) %>%
      mutate(calibrated.value = calOutputValue * coefficient) %>%
      filter(calibrated.value > 0) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    # REFLIQ ENDUSE
    a_trn <- L254.StubTranTechCalInput %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, tranSubsector, minicam.energy.input, calibrated.value) %>%
      distinct() %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    a_bld <- L242.StubTechCalInput_bld %>%
      filter(grepl(REFLIQ_BUCKETS, minicam.energy.input)) %>%
      select(region, year, supplysector, subsector, minicam.energy.input, calibrated.value) %>%
      distinct() %>%
      group_by(region, year, minicam.energy.input) %>%
      summarize(value = sum(calibrated.value), .groups = "drop")

    zL1093.en_bal_EJ_liquids_cons_ind <- a_chem %>%
      bind_rows(a_cement,a_aluminum,a_elec,a_food,a_heat,a_inden,a_indfeed,a_offroad,a_paper,a_irnstl,a_desal,a_fert) %>%
      group_by(region,year,minicam.energy.input) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      replace(., is.na(.), 0) %>%
      select(region, year, type = minicam.energy.input, value)

    zL1093.en_bal_EJ_liquids_cons_end <-
      bind_rows(a_trn,a_bld) %>%
      group_by(region,year,minicam.energy.input) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      select(region, year, type = minicam.energy.input, value)

    zL1093.en_bal_EJ_liquids_cons_type <- zL1093.en_bal_EJ_liquids_cons_ind %>%
      bind_rows(zL1093.en_bal_EJ_liquids_cons_end) %>%
      arrange(region, year, type, value)

    # Also remove direct crude use when adjusting consumption values
    direct_crude <- L121.in_EJ_R_TPES_liq_Yh %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(fuel == "Feedstock", year %in% MODEL_BASE_YEARS) %>%
      select(region, year, type = sector, direct_crude = value)

    # Apply corrections for direct crude consumption
    L1093.en_bal_EJ_liquids_cons_type <- zL1093.en_bal_EJ_liquids_cons_type %>%
      bind_rows(direct_crude %>% mutate(value = -direct_crude)) %>%
      group_by(region,year,type) %>%
      summarize(value = sum(value))%>%
      ungroup() %>%
      select(region, year, type, value)


    #===========================================================================
    # Calculate shares and apply to total refined liquids consumption
    #===========================================================================

    # TODO: also need to split out calibration for needle coke here
    # filter sectors and aggregate them by region, year, and fuel category
    detailed_data <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
      filter(sector %in% liquids_mapping$sector, value != 0) %>%
      left_join_error_no_match(liquids_mapping, by = "sector") %>%
      group_by(region, year, fuel_category, type) %>%
      summarise(value = sum(value), .groups = "drop")

    # calculate the shares by refined liquids fuel categories
    detailed_data_shares <- detailed_data %>%
      group_by(region, year, type) %>%
      mutate(total = sum(value, na.rm = TRUE),
             shares = if_else(total > 0, value / total, NA_real_)) %>%
      ungroup()

    # Harmonized total refined liquids by type
    L1093.en_bal_EJ_liquids_total <- detailed_data_shares %>%
      left_join_error_no_match(L1093.en_bal_EJ_liquids_cons_type,
                               by = c("year", "region", "type")) %>%
      mutate(value = shares * value.y) %>%
      select(region, year, fuel_category, value, type)

    # Harmonized total refined liquids by product
    harmonized_refined_liquids_total <- L1093.en_bal_EJ_liquids_total %>%
      #filter(value > 0) %>% # only balance what was actually made in history
      group_by(region, year, fuel_category) %>%
      summarise(consumption = sum(value), .groups = "drop")

    #=============================================================================
    # Conduct domestic trade balance (consumption = production - exports + imports)
    #=============================================================================

    # Extract liquids exports
    liquids_exports_orig <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
      filter(sector == "exports") %>%
      rename(exports = value) %>%
      select(-sector)

    # Extract liquids imports
    liquids_imports_orig <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
      filter(sector == "imports") %>%
      rename(imports = value) %>%
      select(-sector)

    # Combine imports, exports, production, and consumption into one data frame
    liquids_trade_balance_orig <- harmonized_refined_liquids_total %>%
      left_join(L1093.out_EJ_R_liquids_prod_F_Yh, by = c("region", "fuel_category", "year")) %>%
      left_join(liquids_exports_orig, by = c("region", "fuel_category", "year")) %>%
      left_join(liquids_imports_orig, by = c("region", "fuel_category", "year")) %>%
      replace(., is.na(.), 0)

    # Assign imports equal to consumption in countries where the reported consumption
    # is positive, but the reported imports, exports, and production are zero.
    liquids_trade_balance_orig <- liquids_trade_balance_orig %>%
      rename(fuel = fuel_category) %>%  # TODO: just call it fuel to start in L101
      mutate(imports = if_else(consumption > 0 & (imports == 0 & exports == 0 & production == 0),
                               consumption, imports)) %>%
      select(region, fuel, year, production, consumption, exports, imports) %>%
      replace(., is.na(.), 0)

    #===========================================================================
    # Remove intraregional trade
    #===========================================================================
    # Want to remove trade between countries in the same region from import/export
    # totals, as this can inappropriately skew flow calibration and share weights
    # IEA does not provide bilateral trade data, so this is inferred using
    # publicly-available data from resourcetrade.earth. This data only comes in
    # aggregate, however, as a sum of oil products by mass. We apply the mass
    # ratio of intraregional trade to total trade for a region in order to remove
    # intraregional trade globally.

    bilateral_trade <- Resourcetradeearth_RefinedLiquids_2015 %>%
      select(c(`Exporter ISO3`, Exporter, `Importer ISO3`, Importer, Year,
               `Weight (1000kg)`)) %>%
      filter(Year %in% MODEL_BASE_YEARS) %>%         # only 2015 in current set
      rename(iso_ex = `Exporter ISO3`, iso_imp = `Importer ISO3`,
             `tonnes` = `Weight (1000kg)`) %>%
      mutate(iso_ex = tolower(iso_ex), iso_imp = tolower(iso_imp))

    # relabel trade data imp and exp with gcam region names
    gcam_regions <- Liquids_Trade_GCAM_regID %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-region_GCAM3)

    gcam_agg_trade <- bilateral_trade %>%
      left_join(gcam_regions, by = c("iso_ex" = "iso")) %>%
      left_join(gcam_regions, by = c("iso_imp" = "iso"), suffix = c("_ex", "_im")) %>%
      select(Year, Exporter, Exporter_GCAM = region_ex,
             Importer, Importer_GCAM = region_im, tonnes) %>%
      filter(Exporter != "Bunkers" & Importer != "Bunkers" ) %>%
      mutate(Exporter_GCAM = if_else(is.na(Exporter_GCAM), Exporter, Exporter_GCAM),
             Importer_GCAM = if_else(is.na(Importer_GCAM), Importer, Importer_GCAM)) %>%
      group_by(Year, Exporter_GCAM, Importer_GCAM) %>%
      summarise(tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop")

    # Now to filter out trade to and from unknown parties / bunkers
    # Know the importer, don't know where it came from
    mystery_import <- gcam_agg_trade %>%
      filter(!(Exporter_GCAM %in% GCAM_region_names$region) &
               (Importer_GCAM %in% GCAM_region_names$region)) %>%
      group_by(Year, Importer_GCAM) %>%
      summarise(source_unknown = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
      rename(GCAM_region = Importer_GCAM)

    # Know the exporter, don't know where it's going
    mystery_export <- gcam_agg_trade %>%
      filter(!(Importer_GCAM %in% GCAM_region_names$region) &
               (Exporter_GCAM %in% GCAM_region_names$region)) %>%
      group_by(Year, Exporter_GCAM) %>%
      summarise(dest_unknown = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
      rename(GCAM_region = Exporter_GCAM)

    # Trade within a single GCAM region
    intra_trade <- gcam_agg_trade %>%
      filter(Exporter_GCAM == Importer_GCAM) %>%
      group_by(Year, Exporter_GCAM, Importer_GCAM) %>%
      summarise(intra_known = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
      select(Year, GCAM_region = Exporter_GCAM, intra_known)

    # Trade outside a GCAM region to another known GCAM region
    exporter_trade <- gcam_agg_trade %>%
      filter(Exporter_GCAM != Importer_GCAM &
               Exporter_GCAM %in% GCAM_region_names$region &
               Importer_GCAM %in% GCAM_region_names$region) %>%
      group_by(Year, Exporter_GCAM) %>%
      summarise(export_total_known = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
      rename(GCAM_region = Exporter_GCAM)

    # Trade into a GCAM region from a known source
    importer_trade <- gcam_agg_trade %>%
      filter(Exporter_GCAM != Importer_GCAM &
               Exporter_GCAM %in% GCAM_region_names$region &
               Importer_GCAM %in% GCAM_region_names$region) %>%
      group_by(Year, Importer_GCAM) %>%
      summarise(import_total_known = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
      rename(GCAM_region = Importer_GCAM)

    # Calc the mass percentage of known non-intraregional trade to apply to
    # gross trade as a discount factor. Also have to add in Taiwan as it does
    # not appear in the trade data. Allow regions with no intraregional trade
    # to have discount ratios of 1 (i.e. no discount).
    trade_ratios <- importer_trade %>%
      full_join(exporter_trade, by = c("Year", "GCAM_region")) %>%
      full_join(intra_trade, by = c("Year", "GCAM_region")) %>%
      full_join(mystery_import, by = c("Year", "GCAM_region")) %>%
      full_join(mystery_export, by = c("Year", "GCAM_region")) %>%
      mutate(import_total = import_total_known + source_unknown + intra_known,
             export_total = export_total_known + dest_unknown + intra_known,
             import_discount_ratio = (import_total - intra_known) / import_total,
             export_discount_ratio = (export_total - intra_known) / export_total) %>%
      select(GCAM_region, import_discount_ratio, export_discount_ratio) %>%
      replace(., is.na(.), 1) %>%
      rbind(c("Taiwan", 1, 1))

    # Apply discount factors uniformly to previously-balanced oil products trade
    # data. This assumes a consistent intra vs inter regional trade for all
    # products, which is unlikely. It also assumes the mass percentage is
    # representative enough of what individual energy percentages would look be.
    # These assumptions were chosen based on available data.
    liquids_trade_balance_no_intra <- liquids_trade_balance_orig %>%
      left_join_error_no_match(trade_ratios, by = c("region" = "GCAM_region")) %>%
      mutate(exports = exports * as.numeric(export_discount_ratio),
             imports = imports * as.numeric(import_discount_ratio)) %>%
      select(-c(export_discount_ratio, import_discount_ratio))

    #===========================================================================
    # Conduct regional trade balance
    #===========================================================================

    ########### ORIGINAL TRADEBAL ###
    liquids_trade_balance_orig_recalc <- liquids_trade_balance_no_intra %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(

        # Force trade to the difference between prod and cons if trade not reported
        imports = if_else((imports == 0 & exports == 0) & (consumption - production > 0), consumption - production, imports),
        exports = if_else((imports == 0 & exports == 0) & (production - consumption > 0), production - consumption, exports),

        # Calculate positive imports / exports if only missing one of them
        imports = if_else(imports == 0 & (consumption - production + exports >= 0), consumption - production + exports, imports),
        exports = if_else(exports == 0 & (production - consumption + imports >= 0), production - consumption + imports, exports),

        # Compute initial scaling factor such that
        # Consumption = production - scaling_factor * (exports - imports)
        num = (production - consumption),    # if only negative then consuming more than making up for with imports [increase imp]
        den = (exports - imports),           # if only negative then producing more than relieving with exports [increase exp]
        scaling_factor =  num / den,
        scaling_factor = if_else(is.nan(scaling_factor),1,scaling_factor),

        # Scale imports and exports
        exports_reval = scaling_factor * exports,
        imports_reval = scaling_factor * imports,

        # Ensure exports/imports are non-negative, while avoiding changing production data
        # If the scaling factor numerator is negative, calculate imports. If the
        # scaling factor denominator is negative, calculate exports.
        exports_reval = if_else(scaling_factor < 0 & den <= 0, production - consumption + imports, exports_reval),
        imports_reval = if_else(scaling_factor < 0 & num <= 0, consumption - production + exports, imports_reval),

        # Estimate the % increase in imports and exports after scaling
        diff_exports = if_else(exports == 0, 0, ((exports_reval - exports) / exports) * 100),
        diff_imports = if_else(imports == 0, 0, ((imports_reval - imports) / imports) * 100),

        # for regions with % increase in imports and exports greater than 50%
        # (tolerance level) use the reported IEA imports and exports
        exports_reval = if_else(diff_exports >= 50 | diff_exports <= -50, exports, exports_reval),
        imports_reval = if_else(diff_imports >= 50 | diff_imports <= -50, imports, imports_reval),

        # Compute re-evaluated production without losing production data from
        # regions with no reported trade. Production should only change when
        # modifying exports/imports would be a > 50% change
        production_reval = consumption + exports_reval - imports_reval,
        #production_reval = production,

        # Adjust exports to ensure production >= exports
        adjustment_needed = pmax(0, exports_reval - production_reval, na.rm = TRUE),
        # Cap adjustment so that imports_reval does not go negative
        adjustment_capped = pmin(adjustment_needed, imports_reval),
        exports_reval = exports_reval - adjustment_needed,
        imports_reval = imports_reval - adjustment_needed,  # Adjust imports to maintain trade balance

        # Final recalculation of production after adjustments
        production_reval = consumption + exports_reval - imports_reval,

        # Compute domestic supply without reverting to NA when no trade reported
        domestic_supply = if_else(exports_reval == 0, production_reval, production_reval - exports_reval),

        # Final safeguards: reset to minimal values (production = consumption, imports and exports = 0) if anything goes negative
        production_reval = if_else(production_reval < 0 | exports_reval < 0 | imports_reval < 0 | domestic_supply < 0, consumption, production_reval),
        exports_reval    = if_else(production_reval < 0 | exports_reval < 0 | imports_reval < 0 | domestic_supply < 0, 0, exports_reval),
        imports_reval    = if_else(production_reval < 0 | exports_reval < 0 | imports_reval < 0 | domestic_supply < 0, 0, imports_reval),

        #if production_reval = consumption (set imports and exports = 0)
        imports_reval = if_else(production_reval == consumption, 0, imports_reval),
        exports_reval = if_else(production_reval == consumption, 0, exports_reval),

        domestic_supply = production_reval-exports_reval)%>%
      select(-c(diff_exports, diff_imports))

    #==============================================================
    # Global Balancing of imports, exports, production, consumption
    #==============================================================

    #Scale the regional trade data to ensure global imports and exports are equal.
    #Estimate the global scaling factors by region and year
    Global_scaling_factors <- liquids_trade_balance_orig_recalc %>%
      group_by(fuel, year) %>%
      summarise(exports_reval = sum(exports_reval),
                imports_reval = sum(imports_reval)) %>%
      ungroup() %>%
      mutate(scaling = imports_reval / exports_reval) %>%
      select(-exports_reval, -imports_reval)

    #Scale regional exports and production
    liquids_trade_balance_orig_scaled <- liquids_trade_balance_orig_recalc %>%
      left_join(Global_scaling_factors, by = c("fuel","year")) %>%
      mutate(
        production_reval = production_reval - (exports_reval - exports_reval * scaling),
        exports_reval = exports_reval * scaling,
        domestic_supply = production_reval - exports_reval)

    ## MEL: testing balancing trade by minimizing change to production

    # #===========================================================================
    # # Conduct regional and global trade balance
    # #===========================================================================
    #
    # # Calibrate regional production as shares of global consumption. This
    # # changes production by +/- 15%. Don't change production beyond this point.
    # liquids_trade_balance_cal_prod <- liquids_trade_balance_no_intra %>%
    #   group_by(fuel, year) %>%
    #   mutate(global_prod = sum(production),
    #          global_cons = sum(consumption),
    #          bal = global_prod - global_cons,
    #          prod_share = production / global_prod,
    #          cal_prod = prod_share * global_cons,
    #          global_exp = sum(exports),
    #          global_imp = sum(imports),
    #          trade_bal = global_imp - global_exp) %>%
    #   ungroup() %>%
    #   # TODO: this is for checks, doesn't need to stay in final pipe
    #   mutate(pct_change = round((production - cal_prod) / production * 100, 1),
    #          # round to 8 digits because eventual rounding is 7, otherwise will
    #          # try to scale values that are effectively 0
    #          dplyr::across(where(is.numeric), ~ round(., energy.DIGITS_CALOUTPUT + 1))) %>%
    #   select(region, year, fuel, orig_prod = production, production = cal_prod,
    #          pct_change, consumption, imports, exports, global_exp, global_imp)
    #
    # liquids_trade_balance_orig_scaled <- liquids_trade_balance_cal_prod %>%
    #   mutate(
    #     # Force trade to the difference between prod and cons if trade not reported
    #     imports = if_else((imports == 0 & exports == 0) & (consumption - production > 0), consumption - production, imports),
    #     exports = if_else((imports == 0 & exports == 0) & (production - consumption > 0), production - consumption, exports),
    #
    #     # Calculate positive imports / exports if only missing one of them
    #     imports = if_else(imports == 0 & (consumption - production + exports >= 0), consumption - production + exports, imports),
    #     exports = if_else(exports == 0 & (production - consumption + imports >= 0), production - consumption + imports, exports),
    #
    #     off = round(consumption - (production - exports + imports), energy.DIGITS_CALOUTPUT + 1),
    #
    #     # If off is negative, supply is too big so increase exports. Otherwise,
    #     # if off is positive, increase imports. Avoid negatives by only adding.
    #     exports_reval = exports + if_else(off < 0, -off, 0),
    #     imports_reval = imports + if_else(off >= 0, off, 0),
    #     domestic_supply = production - exports_reval,
    #
    #     # don't let domestic supply be negative in GCAM (will be if regions
    #     # are exporting some of their imports)
    #     exports_reval = if_else(domestic_supply < 0, exports_reval + domestic_supply, exports_reval),
    #     imports_reval = if_else(domestic_supply < 0, imports_reval + domestic_supply, imports_reval),
    #     domestic_supply = production - exports_reval,
    #     recalc_off = round(consumption - (production - exports_reval + imports_reval), energy.DIGITS_CALOUTPUT)
    #    ) %>%
    #   rename(production_reval = production) %>%
    #   # not required in the final pipe but good to keep commented out for ref
    #   group_by(fuel, year) %>%
    #   mutate( global_exp_recalc = sum(exports_reval),
    #           global_imp_recalc = sum(imports_reval),
    #           tradebal_recalc = round(global_exp_recalc - global_imp_recalc, energy.DIGITS_CALOUTPUT),
    #           pct_imp_increase = (global_imp_recalc - global_imp) / global_imp * 100,
    #           pct_exp_increase = (global_exp_recalc - global_exp) / global_exp * 100) %>%
    #   ungroup()

    #===========================================================================
    # Re-calibrate refined liquids production
    #===========================================================================

    # Aggregate liquids production (crude + bio + ctl/gtl)
    # TODO: should we be rounding at this step or in the level 2 chunks?
    total_liquids_production <- liquids_trade_balance_orig_scaled %>%
      select(region, year, fuel, value = production_reval) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT))

    # Biofuel production as determined by upstream chunk
    LB1092.GCAM_BIO_LIQUIDS_PROD_agg <- L122.out_EJ_R_refining_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS,
             sector %in% c("biodiesel", "corn ethanol", "sugar cane ethanol")) %>%
      mutate(fuel = if_else(sector == "biodiesel",
                            "Distillate_FuelOil", "Gasoline"))

    # Disaggregate CTL and GTL-based liquids production to individual products
    # such as Gasoline, Distillate_FuelOil, Jet_Kerosene, and Other
    LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg <- L122.out_EJ_R_refining_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS, sector %in% c("ctl","gtl")) %>%
      left_join(A22.globaltech_coef_ctlgtl %>% select(-fuel), by = "sector") %>%
      mutate(value = value * ratio) %>%
      select(GCAM_region_ID, year, sector, fuel = product, value)

    # Calculate total biorefining and ctl/gtl production
    non_crude_liquids <- LB1092.GCAM_BIO_LIQUIDS_PROD_agg %>%
      bind_rows(LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) %>%
      group_by(year, fuel, region) %>%
      summarise(value = sum(value), .groups ="drop")

    # Subtract total non_crude_liquids production from total liquids production
    LB1092.GCAM_REG_LIQUIDS_PROD_agg <- total_liquids_production %>%
      rename(total = value) %>%
      left_join(non_crude_liquids, by = c("year", "fuel", "region")) %>%
      mutate(value = total - replace_na(value, 0)) %>%
      select(-total)

    # Calculate regional crude oil to refined liquids IO coefficients
    # If production changed during balancing then this ratio will be
    # artificially greater or smaller than it should be
    L1093.IO_R_oilrefining_F_Yh <- LB1092.GCAM_REG_LIQUIDS_PROD_agg %>%
      group_by(year, region) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      left_join(L122.in_EJ_R_refining_F_Yh %>%
                  filter(sector == "oil refining") %>%
                  left_join(GCAM_region_names, by = c("GCAM_region_ID")) %>%
                  select(-GCAM_region_ID),
                by = c("year", "region")) %>%
      # each input / sum of products
      mutate(IO_coeff = value.y / value.x) %>%
      select(-value.y, -value.x)

    # COMBINE ADJUSTED REFINED LIQUIDS PRODUCTION, CONSUMPTION and TRADE DATA
    liquids_imports <- liquids_trade_balance_orig_scaled %>%
      select(region, year, fuel, value = imports_reval) %>%
      mutate(metric = "imports")

    liquids_exports <- liquids_trade_balance_orig_scaled %>%
      select(region, year, fuel, value = exports_reval) %>%
      mutate(metric = "exports")

    # refined product consumption (NOTE: not changed in the trade balance)
    liquids_consumption <- liquids_trade_balance_orig_scaled %>%
      select(region, year, fuel, value = consumption) %>%
      mutate(metric = "consumption")

    liquids_domestic_supply <- liquids_trade_balance_orig_scaled %>%
      select(region, year, fuel, value = domestic_supply) %>%
      mutate(metric = "domestic_supply")

     LB1092.Tradebalance_refined_liquids_EJ_R_Y <- total_liquids_production %>%
       mutate(metric = "production") %>%
       bind_rows(liquids_consumption, liquids_imports,
                 liquids_exports, liquids_domestic_supply) %>%
       rename(GCAM_region = region, GCAM_mapping = fuel) %>%
       complete(GCAM_region, GCAM_mapping, metric, year = MODEL_BASE_YEARS) %>%
       mutate(value = round(replace_na(value, 0), energy.DIGITS_CALOUTPUT))


    #===========================================================================
    # Produce outputs
    #===========================================================================

     LB1092.Tradebalance_refined_liquids_EJ_R_Y %>%
       add_title("Gross trade of refined liquids, by region / year.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L132.in_EJ_R_indheat_F_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") ->
       LB1092.Tradebalance_refined_liquids_EJ_R_Y

     LB1092.GCAM_REG_LIQUIDS_PROD_agg %>%
       add_title("Conventional refined liquids production by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L132.in_EJ_R_indheat_F_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") ->
       LB1092.GCAM_REG_LIQUIDS_PROD_agg

     LB1092.GCAM_BIO_LIQUIDS_PROD_agg %>%
       add_title("Bio-based refined liquids production by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") ->
       LB1092.GCAM_BIO_LIQUIDS_PROD_agg

     LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg %>%
       add_title("CTL/GTL refined liquids production by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names",
                      "energy/A22.globaltech_coef_ctlgtl") ->
       LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg

     L1093.en_bal_EJ_liquids_total %>%
       add_title("Conventional refined liquids enduse consumption by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L132.in_EJ_R_indheat_F_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") ->
       L1093.en_bal_EJ_liquids_total

     L1093.IO_R_oilrefining_F_Yh %>%
       add_title("Crude-based refined liquids production IO coefficients by region / year") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "L132.in_EJ_R_indheat_F_Yh",
                      "L122.in_EJ_R_refining_F_Yh",
                      "common/GCAM_region_names") ->
       L1093.IO_R_oilrefining_F_Yh

    return_data(LB1092.Tradebalance_refined_liquids_EJ_R_Y,
                LB1092.GCAM_REG_LIQUIDS_PROD_agg,
                LB1092.GCAM_BIO_LIQUIDS_PROD_agg,
                LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg,
                L1093.en_bal_EJ_liquids_total,
                L1093.IO_R_oilrefining_F_Yh)

  } else {
    stop("Unknown command")
  }
}
