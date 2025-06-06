# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L240.refined_liquids_trade
#'
#' Model input for regional and (globally) traded iron and steel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L240A.Supplysector_tra},
#'   \code{L240A.SectorUseTrialMarket_tra}, \code{L240A.SubsectorAll_tra}, \code{L240A.TechShrwt_tra},
#'   \code{L240A.TechCost_tra}, \code{L240A.TechCoef_tra}, \code{L240A.Production_tra}, \code{L240A.Supplysector_reg},
#'   \code{L240A.SubsectorAll_reg}, \code{L240A.TechShrwt_reg}, \code{L240A.TechCoef_reg}, \code{L240A.Production_reg_imp},
#'   \code{L240A.Production_reg_dom}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author Siddarth Durga Jan 2025
module_energy_L240.refined_liquids_trade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_liquids_RegionalSector",
             FILE = "energy/A_liquids_RegionalSubsector",
             FILE = "energy/A_liquids_RegionalTechnology",
             FILE = "energy/A_liquids_TradedSector",
             FILE = "energy/A_liquids_TradedSubsector",
             FILE = "energy/A_liquids_TradedTechnology",
             "LB1092.Tradebalance_refined_liquids_EJ_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L240A.Supplysector_tra",
             "L240A.SectorUseTrialMarket_tra",
             "L240A.SubsectorAll_tra",
             "L240A.TechShrwt_tra",
             "L240A.TechCost_tra",
             "L240A.TechCoef_tra",
             "L240A.Production_tra",
             "L240A.Supplysector_reg",
             "L240A.SubsectorAll_reg",
             "L240A.TechShrwt_reg",
             "L240A.TechCoef_reg",
             "L240A.Production_reg_imp",
             "L240A.Production_reg_dom"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    #=== Refining =======

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_liquids_RegionalSector <- get_data(all_data, "energy/A_liquids_RegionalSector", strip_attributes = TRUE)
    A_liquids_RegionalSubsector <- get_data(all_data, "energy/A_liquids_RegionalSubsector", strip_attributes = TRUE)
    A_liquids_RegionalTechnology <- get_data(all_data, "energy/A_liquids_RegionalTechnology", strip_attributes = TRUE)
    A_liquids_TradedSector <- get_data(all_data, "energy/A_liquids_TradedSector", strip_attributes = TRUE)
    A_liquids_TradedSubsector <- get_data(all_data, "energy/A_liquids_TradedSubsector", strip_attributes = TRUE)
    A_liquids_TradedTechnology <- get_data(all_data, "energy/A_liquids_TradedTechnology", strip_attributes = TRUE)
    LB1092.Tradebalance_refined_liquids_EJ_R_Y <- get_data(all_data, "LB1092.Tradebalance_refined_liquids_EJ_R_Y")

     # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
    # L240A.Supplysector_tra: generic supplysector info for traded refined liquids
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_liquids_TradedSector$region <- gcam.USA_REGION

    # L240A.Supplysector_tra: generic supplysector info for traded refined liquids
    L240A.Supplysector_tra <- mutate(A_liquids_TradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L240A.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L240A.SectorUseTrialMarket_tra <- select(A_liquids_TradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L240A.SubsectorAll_tra: generic subsector info for traded refined liquids
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L240A.SubsectorAll_tra <- write_to_all_regions(A_liquids_TradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names,
                                                  has_traded = TRUE)


    # Base technology-level table for several tables to be written out")
    A_liquids_TradedTechnology_R_Y <- repeat_add_columns(A_liquids_TradedTechnology,
                                                        tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

    # L240A.TechShrwt_tra: Share-weights of traded technologies
    L240A.TechShrwt_tra <- select(A_liquids_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L240A.TechCost_tra: Costs of traded technologies
    L240A.TechCost_tra <- A_liquids_TradedTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L240A.TechCoef_tra: Coefficient and market name of traded technologies
    L240A.TechCoef_tra <- select(A_liquids_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])


    # L240A.Production_tra: Output (gross exports) of traded technologies
    L240A.GrossExports_EJ_R_Y <- left_join_error_no_match(LB1092.Tradebalance_refined_liquids_EJ_R_Y %>%
                                                           filter(metric=="Exports") %>%
                                                           rename(GrossExp_EJ=value,region=GCAM_region),
                                                         GCAM_region_names,
                                                         by = "region") %>%
      select(region, year, GrossExp_EJ,GCAM_mapping)

    L240A.Production_tra <- filter(A_liquids_TradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join(L240A.GrossExports_EJ_R_Y,
                               by = c(market.name = "region", "year",minicam.energy.input="GCAM_mapping")) %>%
      rename(calOutputValue = GrossExp_EJ) %>%
      mutate(calOutputValue = ifelse(is.na(calOutputValue), 0, calOutputValue))%>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight)%>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # PART 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
    # L240A.Supplysector_reg: generic supplysector info for refined liquids
    L240A.Supplysector_reg <- mutate(A_liquids_RegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names)

    # L240A.SubsectorAll_reg: generic subsector info for regional refined liquids (competing domestic prod vs intl imports)
    L240A.SubsectorAll_reg <- write_to_all_regions(A_liquids_RegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names)

    # Base technology-level table for several tables to be written out")
    A_liquids_RegionalTechnology_R_Y <- repeat_add_columns(A_liquids_RegionalTechnology,
                                                          tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names["region"]) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L240A.TechShrwt_tra: Share-weights of traded technologies
    L240A.TechShrwt_reg <- select(A_liquids_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L240A.TechCoef_reg: Coefficient and market name of traded technologies
    L240A.TechCoef_reg <- select(A_liquids_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L240A.Production_reg_imp: Output (flow) of gross imports
    # Imports are equal to the gross imports calculated in LB1092
    L240A.GrossImports_EJ_R_Y <- left_join_error_no_match(LB1092.Tradebalance_refined_liquids_EJ_R_Y %>%
                                                           filter(metric=="Imports") %>%
                                                           mutate(minicam.energy.input=GCAM_mapping)%>%
                                                           rename(GrossImp_EJ=value,region=GCAM_region),
                                                         GCAM_region_names,
                                                         by = "region")%>%
      left_join(select(A_liquids_TradedTechnology, supplysector, minicam.energy.input),
                by = c("minicam.energy.input")) %>%
      select(region, supplysector, year, GrossImp_EJ)

    L240A.Production_reg_imp <- A_liquids_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join(L240A.GrossImports_EJ_R_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_EJ) %>%
      mutate(calOutputValue = ifelse(is.na(calOutputValue), 0, calOutputValue))%>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L240A.Production_reg_dom: Output (flow) of domestic

    #### DOMESTIC TECHNOLOGY OUTPUT = iron and steel PRODUCTION - GROSS EXPORTS
    L240A.DomSup_EJ_R_Y <- left_join_error_no_match(LB1092.Tradebalance_refined_liquids_EJ_R_Y %>%
                                                     filter(metric=="domestic_supply") %>%
                                                     mutate(minicam.energy.input=GCAM_mapping)%>%
                                                     rename(DomSup_EJ=value,region=GCAM_region),
                                                   GCAM_region_names,
                                                   by = "region") %>%
      select(region, minicam.energy.input, year, DomSup_EJ)

    L240A.Production_reg_dom <- A_liquids_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join(L240A.DomSup_EJ_R_Y,
                               by = c("region", "minicam.energy.input", "year")) %>%
      rename(calOutputValue = DomSup_EJ)%>%
      mutate(calOutputValue = ifelse(is.na(calOutputValue), 0, calOutputValue),
             calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])


    # Produce outputs
    L240A.Supplysector_tra %>%
      add_title("Supplysector info for refined liquids") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_TradedSector") ->
      L240A.Supplysector_tra

    L240A.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets") %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with refined liquids trade") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_TradedSector") ->
      L240A.SectorUseTrialMarket_tra

    L240A.SubsectorAll_tra %>%
      add_title("Subsector info for traded refined liquids") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_TradedSubsector") ->
      L240A.SubsectorAll_tra

    L240A.TechShrwt_tra %>%
      add_title("Technology share-weights for traded liquids") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_TradedTechnology") ->
      L240A.TechShrwt_tra

    L240A.TechCost_tra %>%
      add_title("Technology costs for traded refined liquids") %>%
      add_units("1975$/GJ") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_TradedTechnology") ->
      L240A.TechCost_tra

    L240A.TechCoef_tra %>%
      add_title("Technology input-output coefficients for traded refined liquids") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_TradedTechnology") -> L240A.TechCoef_tra

    L240A.Production_tra %>%
      add_title("Technology calibration for traded refined liquids") %>%
      add_units("EJ") %>%
      add_comments("Regional exports of refined liquids that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_refined_liquids_EJ_R_Y") -> L240A.Production_tra

    L240A.Supplysector_reg %>%
      add_title("Supplysector info for regional refined liquids") %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced refined liquids versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_RegionalSector") ->
      L240A.Supplysector_reg

    L240A.SubsectorAll_reg %>%
      add_title("Subsector info for traded refined liquids") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_RegionalSubsector") ->
      L240A.SubsectorAll_reg

    L240A.TechShrwt_reg %>%
      add_title("Technology share-weights for traded liquids") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_RegionalTechnology") ->
      L240A.TechShrwt_reg

    L240A.TechCoef_reg %>%
      add_title("Technology input-output coefficients for regional liquids") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_liquids_RegionalTechnology") ->
      L240A.TechCoef_reg

    L240A.Production_reg_imp %>%
      add_title("Technology calibration for regional iron and steel commodities: imports") %>%
      add_units("GJ") %>%
      add_comments("Consumption of iron and steelthat are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_refined_liquids_EJ_R_Y") ->
      L240A.Production_reg_imp

    L240A.Production_reg_dom %>%
      add_title("Technology calibration for regional refined liquids: consumption of domestic production") %>%
      add_units("GJ") %>%
      add_comments("Consumption of refined liquids produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_refined_liquids_EJ_R_Y") ->
      L240A.Production_reg_dom


    return_data(L240A.Supplysector_tra,
                L240A.SectorUseTrialMarket_tra,
                L240A.SubsectorAll_tra,
                L240A.TechShrwt_tra,
                L240A.TechCost_tra,
                L240A.TechCoef_tra,
                L240A.Production_tra,
                L240A.Supplysector_reg,
                L240A.SubsectorAll_reg,
                L240A.TechShrwt_reg,
                L240A.TechCoef_reg,
                L240A.Production_reg_imp,
                L240A.Production_reg_dom)
  } else {
    stop("Unknown command")
  }
}
