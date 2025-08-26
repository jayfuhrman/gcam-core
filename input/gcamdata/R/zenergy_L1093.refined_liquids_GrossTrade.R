# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1093.refined_liquids_GrossTrade
#'
#' Reads pre-processed IEA refined liquids production, consumption, imports and exports data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{LB1092.Tradebalance_refined_liquids_EJ_R_Y}, \code{L1093.en_bal_EJ_liquids_enduse_total}, \code{L1093.en_bal_EJ_liquids_industrial_total}, \code{LB1092.GCAM_REG_LIQUIDS_PROD_agg}, \code{LB1092.GCAM_BIO_LIQUIDS_PROD_agg"}, \code{LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg}, \code{L1093.IO_R_oilrefining_F_Yh}.
#' @importFrom dplyr filter if_else mutate select distinct coalesce
#' @importFrom tidyr gather spread
#' @author Siddarth Durga, Maggie Liu (Jan 2025)
module_energy_L1093.refined_liquids_GrossTrade <- function(command, ...){
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L121.in_EJ_R_TPES_liq_Yh",
             "L122.in_EJ_R_refining_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L101.detailed_refined_liquids_EJ_R_Yh",
             "L1012.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "energy/A22.globaltech_coef_ctlgtl",
             FILE = "energy/mappings/IEA_product_fuel_liquids",
             FILE = "energy/mappings/Liquids_Trade_GCAM_regID",
             FILE = "energy/mappings/IEA_product_LHV",
             FILE = "energy/Resourcetradeearth-RefinedLiquids-2015"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("LB1092.Tradebalance_refined_liquids_EJ_R_Y",
             "LB1092.GCAM_REG_LIQUIDS_PROD_agg",
             "LB1092.GCAM_BIO_LIQUIDS_PROD_agg",
             "LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg",
             "L1093.en_bal_EJ_liquids_enduse_total",
             "L1093.en_bal_EJ_liquids_industrial_total",
             "L1093.IO_R_oilrefining_F_Yh"))
  } else if(command == driver.MAKE) {

    # Silence data-masked variable package check
    Year <- Exporter <- Importer <- region_GCAM3 <- production <- consumption <-
      exports <- imports <- metric <- exports.un <- imports.un <- transfers <-
      consumption.un <- sector_2 <- region <- value <- sector <- fuel <- year <-
      production.un <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    GCAM_region_iso_mapping <- get_data(all_data, "energy/mappings/Liquids_Trade_GCAM_regID", strip_attributes = TRUE)
    IEA_product_fuel_liquids <- get_data(all_data,"energy/mappings/IEA_product_fuel_liquids", strip_attributes = TRUE)
    L121.in_EJ_R_TPES_liq_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_liq_Yh", strip_attributes = TRUE)
    L122.out_EJ_R_refining_F_Yh <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh", strip_attributes = TRUE)
    L122.in_EJ_R_refining_F_Yh <-  get_data(all_data,"L122.in_EJ_R_refining_F_Yh", strip_attributes = TRUE)
    L101.detailed_refined_liquids_EJ_R_Yh <- get_data(all_data, "L101.detailed_refined_liquids_EJ_R_Yh", strip_attributes = TRUE)
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    convert_lhv <- get_data(all_data, "energy/mappings/IEA_product_LHV", strip_attributes = TRUE)
    raw_liquids_trade <- get_data(all_data, "energy/Resourcetradeearth-RefinedLiquids-2015", strip_attributes = TRUE)
    A22.globaltech_coef_ctlgtl <- get_data(all_data,"energy/A22.globaltech_coef_ctlgtl", strip_attributes = TRUE)

    #==========================================================================================
    # Estimate refined liquids production by product categories (e.g, Gasoline, DFO, RFO etc.)
    #==========================================================================================

    #Aggregate by GCAM region, sector, and aggregate fuel category
    L1093.detailed_refined_liquids_EJ_R_Yh <- L101.detailed_refined_liquids_EJ_R_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(IEA_product_fuel_liquids,by=c("PRODUCT"))%>%
      left_join(GCAM_region_names,by=c("GCAM_region_ID"))%>%
      group_by(region,sector,fuel_category,year)%>%
      summarize(value=sum(value))%>%
      ungroup()

    L1093.detailed_refined_liquids_EJ_R_Yh$year <- as.integer(L1093.detailed_refined_liquids_EJ_R_Yh$year)

    # Convert net_oil refining (refinery output = TREFINER + EREFINER) to positive values
    L1093.detailed_refined_liquids_EJ_R_Yh <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
      filter(fuel_category != "Unrefined_Liquids") %>%      # don't include direct crude consumption in the refining sector
    # TODO: south america northern has 4 periods with LPG as a net refining input
    # because the transfers category more than covers all consumption sectors
    # setting this value to 0 and will settle globally with the trade balance
      mutate(value = if_else(sector == "net_oil refining", -value, value),
             value = if_else(sector == "net_oil refining" & value < 0, 0, value))


    # Estimate total refined liquids production in a region (net_oil refining + transfers)
    # Here we add transfers to total refined liquids production. This is especially important for LPG
    # as a substantial amount of LPG is produced by gas processing but is currently captured in GCAM's refining sector.
    L1093.out_EJ_R_liquids_prod_F_Yh <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
      filter(sector %in% c("net_oil refining","transfers")) %>%
      group_by(region,fuel_category,year)%>%
      summarize(value=sum(value))%>%
      ungroup() %>%
      # some regions have more product transfers than refining output (large imports for transfer)
      # can't produce a negative so shift that volume into the trade balance
      mutate(value=if_else(value<0,0,value))

    #============================================================================================
    # Estimate refined liquids consumption across all GCAM sectors (total and by sector)
    #============================================================================================

    # Estimate total refined liquids consumption by sector from L1012.en_bal_EJ_R_Si_Fi_Yh
     L1093.en_bal_EJ_liquids_cons_sector <- L1012.en_bal_EJ_R_Si_Fi_Yh %>%
       left_join(GCAM_region_names,by = c("GCAM_region_ID")) %>%
       select(-GCAM_region_ID) %>%
        filter(fuel %in% energy.REFINED_LIQUIDS_AGG,
               year %in% MODEL_BASE_YEARS,
               sector %in% c(energy.LIQUIDS_ENDUSE_SECTORS,
                             energy.LIQUIDS_INDUSTRIAL_SECTORS,
                             energy.LIQUIDS_EFW_SECTORS))%>%
       group_by(sector,year,region)%>%
       summarize(value=sum(value))%>%
       ungroup()


     # Estimate liquids end-use consumption
     L1093.en_bal_EJ_liquids_enduse_total <- L1093.en_bal_EJ_liquids_cons_sector %>%
       filter(sector %in% energy.LIQUIDS_ENDUSE_SECTORS) %>%
       group_by(year, region) %>%
       summarize(value = sum(value), .groups = "drop")

     # TODO: liqsplit has taken 'inputs by tech', and filtered+grouped for inputs
     # in the refined liquids enduse/industrial buckets; adder is the difference
     # between the expected query output and L1012 'refined liquids enduse' consumption
     # TODO: Need to figure out where these values are coming from and being added
     # to liquids end-use consumption from industry
     liqsplit <- read.csv("liqsplit.csv") %>% filter(input == "refined liquids enduse") %>% select(-input)
     end_use_adder <- left_join(L1093.en_bal_EJ_liquids_enduse_total, liqsplit,
                    by = c("region", "year")) %>%
       mutate(adder = value.y - value.x) %>%
       filter(year > 1975) %>%
       select(year, region, adder)

     # Also remove direct crude use when adjusting end use values with the adder
     direct_crude <- L121.in_EJ_R_TPES_liq_Yh %>%
       left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
       filter(fuel == "Feedstock", year %in% MODEL_BASE_YEARS) %>%
       select(region, year, sector, value)

     L1093.en_bal_EJ_liquids_enduse_total <- L1093.en_bal_EJ_liquids_enduse_total %>%
       left_join(end_use_adder, by = c("year", "region")) %>%
       # mutate(value = value - replace_na(adder, 0)) %>%
       # select(region, year, value)
       left_join(direct_crude %>%
                   filter(sector == "refined liquids enduse") %>%
                   select(-sector),
                 by = c("region", "year")) %>%
       mutate(total = value.x + replace_na(adder, 0),
              value = total - replace_na(value.y, 0)) %>%
       select(region, year, value)


     # Estimate liquids industrial consumption
     L1093.en_bal_EJ_liquids_industrial_total <- L1093.en_bal_EJ_liquids_cons_sector %>%
       filter(sector %in% c(energy.LIQUIDS_INDUSTRIAL_SECTORS,
                            energy.LIQUIDS_EFW_SECTORS)) %>%
       group_by(year, region) %>%
       summarize(value = sum(value), .groups = "drop")

     # Remove direct crude use for industry as well
     L1093.en_bal_EJ_liquids_industrial_total <- L1093.en_bal_EJ_liquids_industrial_total %>%
       left_join(end_use_adder,by=c("year","region"))%>%
       # mutate(value = value - replace_na(adder, 0)) %>%
       # select(region, year, value)
       left_join(direct_crude %>%
                   filter(sector == "refined liquids industrial") %>%
                   select(-sector),
                 by = c("region", "year")) %>%
       mutate(total = value.x - replace_na(adder, 0),
              value = total - replace_na(value.y, 0)) %>%
       select(region, year, value)


     #=========================================================================================
     # Calculate shares and apply to total refined liquids consumption
     #=========================================================================================

     #Function to calculate shares of refined liquids products across end-use sectors
     get_harmonized_liquids_data <- function(base_data,harmonization_data,sector_filter,label) {

       #filter sectors and aggregate them by region, year, and fuel category
       detailed_data <- base_data %>% filter(sector %in% sector_filter) %>%
         group_by(region, year, fuel_category) %>%
         summarize(value = sum(value)) %>%
         mutate(type = label) %>%
         filter(value != 0)

       #calculate the shares by refined liquids fuel categories
       detailed_data_shares <- detailed_data %>%
         group_by(region, year, type) %>%
         mutate(shares = (value/sum(value))) %>%
         ungroup()

       harmonized_data <- detailed_data_shares %>%
         left_join(harmonization_data,by=c("year","region"))%>%
         mutate(value=shares*value.y)%>%
         select(region,year,fuel_category,value,type)

       return(harmonized_data)
     }

     #Harmonized refined liquids end use
     harmonized_liquids_enduse <- get_harmonized_liquids_data(L1093.detailed_refined_liquids_EJ_R_Yh,
                                                             L1093.en_bal_EJ_liquids_enduse_total,
                                                             energy.LIQUIDS_ENDUSE_SECTORS,
                                                             "refined liquids enduse")

     #Harmonized refined liquids industrial
     harmonized_liquids_industrial <- get_harmonized_liquids_data(L1093.detailed_refined_liquids_EJ_R_Yh,
                                                                 L1093.en_bal_EJ_liquids_industrial_total,
                                                                 c(energy.LIQUIDS_INDUSTRIAL_SECTORS,energy.LIQUIDS_EFW_SECTORS),
                                                                "refined liquids industrial")

     #Harmonized total refined liquids by product
     harmonized_refined_liquids_total <- harmonized_liquids_enduse %>%
       rbind(harmonized_liquids_industrial)%>%
       group_by(region,year,fuel_category)%>%
       summarize(value=sum(value))%>%
       ungroup()

     #=============================================================================
     # Conduct domestic trade balance (consumption = production - exports + imports)
     #=============================================================================

    #Estimate total liquids production from refining
     L1093.out_EJ_R_liquids_prod_F_Yh %>%
       rename(production=value) -> liquids_total_prod_orig

    # Extract liquids exports
    liquids_exports_orig <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
       filter(sector=="exports",fuel_category!="Unrefined_Liquids") %>%
       rename(exports=value) %>%
       select(-sector)

    # Extract liquids imports
     liquids_imports_orig <- L1093.detailed_refined_liquids_EJ_R_Yh %>%
       filter(sector=="imports",fuel_category!="Unrefined_Liquids") %>%
       rename(imports=value) %>%
       select(-sector)

    #Combine imports, exports, production, and consumption into a single data frame
    liquids_trade_balance_orig <- harmonized_refined_liquids_total %>%
       rename(consumption=value) %>%
       left_join(liquids_total_prod_orig, by = c("region","fuel_category","year")) %>%
       left_join(liquids_exports_orig, by = c("region","fuel_category","year")) %>%
       left_join(liquids_imports_orig, by = c("region","fuel_category","year")) %>%
       replace(., is.na(.), 0)

    # Assign imports equal to consumption in countries where the reported consumption
    # is positive, but the reported imports, exports, and production are zero.
    liquids_trade_balance_orig <- liquids_trade_balance_orig %>%
       rename(fuel=fuel_category)%>%
       mutate(imports = if_else(consumption > 0 & (imports == 0 & exports == 0 & production == 0),consumption,imports)) %>%
       select(region, fuel, year, production, consumption, exports, imports) %>%
       replace(., is.na(.), 0)      # zero out NA

    #===========================================================================
    # REMOVE INTRAREGIONAL TRADE
    #===========================================================================
    # Want to remove trade between countries in the same region from import/export
    # totals, as this can inappropriately skew flow calibration and share weights
    # IEA does not provide bilateral trade data, so this is inferred using
    # publicly-available data from resourcetrade.earth. This data only comes in
    # aggregate, however, as a sum of oil products by mass. We apply the mass
    # ratio of intraregional trade to total trade for a region in order to remove
    # intraregional trade globally.

    bilateral_trade <- raw_liquids_trade %>%
       select(c(`Exporter ISO3`, Exporter, `Importer ISO3`, Importer, Year,
               `Weight (1000kg)`)) %>%
       filter(Year %in% MODEL_BASE_YEARS) %>%          # only 2015 in current set
       rename(iso_ex = `Exporter ISO3`, iso_imp = `Importer ISO3`,
              `tonnes` = `Weight (1000kg)`) %>%
       mutate(iso_ex = tolower(iso_ex), iso_imp = tolower(iso_imp))

    # relabel trade data imp and exp with gcam region names
    gcam_regions <- GCAM_region_iso_mapping %>%
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
     summarize(tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop")

    # Now to filter out trade to and from unknown parties / bunkers
    # Know the importer, don't know where it came from
    mystery_import <- gcam_agg_trade %>%
     filter(!(Exporter_GCAM %in% GCAM_region_names$region) &
              (Importer_GCAM %in% GCAM_region_names$region)) %>%
     group_by(Year, Importer_GCAM) %>%
     summarize(source_unknown = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
     rename(GCAM_region = Importer_GCAM)

    # Know the exporter, don't know where it's going
    mystery_export <- gcam_agg_trade %>%
     filter(!(Importer_GCAM %in% GCAM_region_names$region) &
              (Exporter_GCAM %in% GCAM_region_names$region)) %>%
     group_by(Year, Exporter_GCAM) %>%
     summarize(dest_unknown = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
     rename(GCAM_region = Exporter_GCAM)

    # Trade within a single GCAM region
    intra_trade <- gcam_agg_trade %>%
       filter(Exporter_GCAM == Importer_GCAM) %>%
       group_by(Year, Exporter_GCAM, Importer_GCAM) %>%
       summarize(intra_known = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
       select(Year, GCAM_region = Exporter_GCAM, intra_known)

    # Trade outside a GCAM region to another known GCAM region
    exporter_trade <- gcam_agg_trade %>%
       filter(Exporter_GCAM != Importer_GCAM &
                Exporter_GCAM %in% GCAM_region_names$region &
                Importer_GCAM %in% GCAM_region_names$region) %>%
       group_by(Year, Exporter_GCAM) %>%
       summarize(export_total_known = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
       rename(GCAM_region = Exporter_GCAM)

    # Trade into a GCAM region from a known source
    importer_trade <- gcam_agg_trade %>%
       filter(Exporter_GCAM != Importer_GCAM &
                Exporter_GCAM %in% GCAM_region_names$region &
                Importer_GCAM %in% GCAM_region_names$region) %>%
       group_by(Year, Importer_GCAM) %>%
       summarize(import_total_known = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
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
       left_join(trade_ratios, by = c("region" = "GCAM_region")) %>%
       mutate(exports = exports * as.numeric(export_discount_ratio),
              imports = imports * as.numeric(import_discount_ratio)) %>%
       select(-c(export_discount_ratio, import_discount_ratio))


    #===========================================================================
    # Conduct regional trade balance
    #===========================================================================

    a_no_intra_regional <- liquids_trade_balance_no_intra %>%
      group_by(fuel, year) %>%
      mutate(global_prod = sum(production),
             global_cons = sum(consumption),
             bal = global_prod - global_cons,
             global_exp = sum(exports),
             global_imp = sum(imports),
             trade_bal = global_imp - global_exp,
             prod_share = production / global_prod,
             cal_prod = prod_share * global_cons) %>%
      ungroup() %>%
      select(region, year, fuel, production, cal_prod, consumption, imports, exports)


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

    # TODO: clean this up
    # a_scaled <- liquids_trade_balance_orig_scaled %>%
    #   group_by(fuel, year) %>%
    #   mutate(global_prod = sum(production_reval),
    #          global_cons = sum(consumption),
    #          bal = global_prod - global_cons,
    #          global_exp = sum(exports_reval),
    #          global_imp = sum(imports_reval),
    #          trade_bal = global_imp - global_exp) %>%
    #   ungroup() %>%
    #   select(region, year, fuel, production, production_reval, consumption, imports_reval, exports_reval, bal, trade_bal)



    #===========================================================================
    # Re-calibrate refined liquids production
    #===========================================================================
    #
    # Aggregate liquids production (crude + bio + ctl/gtl)
    liquids_trade_balance_orig_scaled %>%
       select(production_reval,year,fuel,region) %>%
       rename(value=production_reval) %>%
       mutate(value=round(value,energy.DIGITS_CALOUTPUT)) -> total_liquids_production

    # Filter bioliquids production
    L122.out_EJ_R_refining_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      filter(sector %in% c("biodiesel","corn ethanol","sugar cane ethanol"))-> bioliquids_production

    # Disaggregate CTL and GTL-based liquids production to individual products
    # such as gasoline, distillate_fueloil, Jet_Kerosene, and other
    ctl_gtl_production <- L122.out_EJ_R_refining_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      filter(sector %in% c("ctl","gtl")) %>%
      left_join(A22.globaltech_coef_ctlgtl %>% select(-fuel), by = "sector") %>%
      mutate(value = value * ratio) %>%
      select(-ratio,-fuel) %>%
      rename(fuel = product)

    # Estimate crude-based liquids production (by subtracting total - bioliquids - ctl/gtl)
    bioliquids_production %>%
      mutate(fuel=if_else(sector=="biodiesel","Distillate_FuelOil","Gasoline")) %>%
      rbind(ctl_gtl_production)%>%
      group_by(year,fuel,GCAM_region_ID)%>%
      summarize(value=sum(value),.groups ="drop")%>%
      left_join(GCAM_region_names,by=c("GCAM_region_ID"))%>%
      select(-GCAM_region_ID)-> non_crude_liquids

    # Subtract total non_crude_liquids production from total liquids production
    crude_liquids_production <- total_liquids_production %>%
      rename(total=value)%>%
      left_join(non_crude_liquids,by=c("year","fuel","region"))%>%
      mutate(value=total-value)%>%
      select(-total)

    # Calculate regional crude oil to refined liquids IO coefficients
    # TODO: if production changed during balancing then this ratio will be
    # artificially greater or smaller than it should be
    L122.in_EJ_R_refining_F_Yh <- L122.in_EJ_R_refining_F_Yh %>%
      left_join(GCAM_region_names,by=c("GCAM_region_ID"))%>%
      select(-GCAM_region_ID)

    L1093.IO_R_oilrefining_F_Yh <- crude_liquids_production %>%
      group_by(year,region)%>%
      summarize(value=sum(value))%>%
      ungroup() %>%
      left_join(L122.in_EJ_R_refining_F_Yh %>%
                  filter(sector=="oil refining"),by=c("year","region")) %>%
      mutate(IO_coeff=value.y/value.x)%>%
      select(-value.y,-value.x)#has to be oil/total liquids


    #liquids imports
     liquids_trade_balance_orig_scaled%>%
       select(imports_reval,year,fuel,region) %>%
       rename(value=imports_reval) %>%
       mutate(metric="Imports") -> liquids_imports

    #liquids exports
     liquids_trade_balance_orig_scaled%>%
       select(exports_reval,year,fuel,region) %>%
      rename(value=exports_reval) %>%
       mutate(metric="Exports") -> liquids_exports

    #liquids consumption
     liquids_trade_balance_orig_scaled%>%
       select(consumption,year,fuel,region) %>%
       rename(value=consumption) %>%
       mutate(metric="consumption_reval") -> liquids_consumption

    #liquids domestic supply
     liquids_trade_balance_orig_scaled%>%
       select(domestic_supply,year,fuel,region) %>%
       rename(value=domestic_supply) %>%
       mutate(metric="domestic_supply") -> liquids_domestic_supply


     #COMBINE ADJUSTED REFINED LIQUIDS PRODUCTION, CONSUMPTION and TRADE DATA
     LB1092.Tradebalance_refined_liquids_EJ_R_Y <- rbind(total_liquids_production %>%
                                                           mutate(metric="production"),
                                                         liquids_consumption,
                                                         liquids_imports,
                                                         liquids_exports,
                                                         liquids_domestic_supply) %>%
       rename(GCAM_region=region,GCAM_mapping=fuel) %>%
       mutate(value=round(value, energy.DIGITS_CALOUTPUT))


    # Produce outputs
     LB1092.Tradebalance_refined_liquids_EJ_R_Y %>%
       add_title("Gross trade of refined liquids, by region / year.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") -> LB1092.Tradebalance_refined_liquids_EJ_R_Y

     crude_liquids_production %>%
       add_title("Conventional refined liquids production by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") -> LB1092.GCAM_REG_LIQUIDS_PROD_agg

     bioliquids_production %>%
       add_title("Bio-based refined liquids production by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") -> LB1092.GCAM_BIO_LIQUIDS_PROD_agg

     ctl_gtl_production %>%
       add_title("CTL/GTL refined liquids production by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names",
                      "energy/A22.globaltech_coef_ctlgtl") -> LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg

     harmonized_liquids_enduse %>%
       add_title("Conventional refined liquids enduse consumption by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") -> L1093.en_bal_EJ_liquids_enduse_total

     harmonized_liquids_industrial %>%
       add_title("Conventional refined liquids industrial consumption by region / year / type.") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "common/GCAM_region_names") -> L1093.en_bal_EJ_liquids_industrial_total

     L1093.IO_R_oilrefining_F_Yh %>%
       add_title("Crude-based refined liquids production IO coefficients by region / year") %>%
       add_units("EJ") %>%
       add_comments("Determined from IEA energy balances data") %>%
       add_precursors("L122.out_EJ_R_refining_F_Yh",
                      "L101.detailed_refined_liquids_EJ_R_Yh",
                      "L1012.en_bal_EJ_R_Si_Fi_Yh",
                      "L122.in_EJ_R_refining_F_Yh",
                      "common/GCAM_region_names") -> L1093.IO_R_oilrefining_F_Yh

    return_data(LB1092.Tradebalance_refined_liquids_EJ_R_Y,
                 LB1092.GCAM_REG_LIQUIDS_PROD_agg,
                LB1092.GCAM_BIO_LIQUIDS_PROD_agg,
                LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg,
                L1093.en_bal_EJ_liquids_enduse_total,
                L1093.en_bal_EJ_liquids_industrial_total,
                L1093.IO_R_oilrefining_F_Yh)

  } else {
    stop("Unknown command")
  }
}
