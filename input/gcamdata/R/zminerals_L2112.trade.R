# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_minerals_L2112.trade
#'
#' Set up data tables for minerals trade
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' @details Set up data tables for mineral supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na pivot_longer pivot_wider expand_grid
#' @author BY March 2025

module_minerals_L2112.trade <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "minerals/trade/A_mineral_RegionalSector",
      FILE = "minerals/trade/A_mineral_RegionalSubsector",
      FILE = "minerals/trade/A_mineral_RegionalTechnology",
      FILE = "minerals/trade/A_mineral_TradedSector",
      FILE = "minerals/trade/A_mineral_TradedSubsector",
      FILE = "minerals/trade/A_mineral_TradedTechnology",
      "L2111.RsrcCalProd")

  MODULE_OUTPUTS <-
    c()

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    lapply(MODULE_INPUTS, function(d){
      # get name as the char after last /
      nm <- tail(strsplit(d, "/")[[1]], n = 1)
      # get data and assign
      assign(nm, get_data(all_data, d, strip_attributes = T),
             envir = parent.env(environment()))  })

    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
    # L2112.Supplysector_tra: generic supplysector info for traded mineral commodities
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_mineral_TradedSector$region <- gcam.USA_REGION

    # L2112.Supplysector_tra: generic supplysector info for traded mineral commodities
    L2112.Supplysector_tra <- mutate(A_mineral_TradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L2112.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L2112.SectorUseTrialMarket_tra <- select(A_mineral_TradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L2112.SubsectorAll_tra: generic subsector info for traded mineral commodities
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L2112.SubsectorAll_tra <- write_to_all_regions(A_mineral_TradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names,
                                                  has_traded = TRUE)

    # Base technology-level table for several tables to be written out")
    A_mineral_TradedTechnology_R_Y <- repeat_add_columns(A_mineral_TradedTechnology,
                                                    tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

    # L2112.TechShrwt_tra: Share-weights of traded technologies
    L2112.TechShrwt_tra <- select(A_mineral_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L2112.TechCost_tra: Costs of traded technologies
   # NO COST FOR NOW

    # L2112.TechCoef_tra: Coefficient and market name of traded technologies
    L2112.TechCoef_tra <- select(A_mineral_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L2112.Production_tra: Output (gross exports) of traded technologies
    # For now, gross exports = gross production, because everything is being sent to a single global market
    L2112.Production_tra <- A_mineral_TradedTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # use LJ as there will be several NAs (regions that do not produce a given mineral). We need to filter those out
      left_join(L2111.RsrcCalProd, by = c("market.name" = "region",
                                                         "year",
                                          "minicam.energy.input" = "resource")) %>%
      na.omit() %>%
      mutate(calOutputValue = cal.production,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # PART 2: REGIONAL SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
    # L2112.Supplysector_reg: generic supplysector info for regional mineral commodities
    L2112.Supplysector_reg <- mutate(A_mineral_RegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names)

    # L2112.SubsectorAll_reg: generic subsector info for regional mineral commodities (competing domestic prod vs intl imports). For now, this is just imports.
    L2112.SubsectorAll_reg <- write_to_all_regions(A_mineral_RegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names)

    # Base technology-level table for several tables to be written out")
    A_mineral_RegionalTechnology_R_Y <- repeat_add_columns(A_mineral_RegionalTechnology,
                                                      tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names["region"]) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L2112.TechShrwt_reg: Share-weights of regional technologies
    L2112.TechShrwt_reg <- select(A_mineral_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L2112.TechCoef_reg: Coefficient and market name of regional technologies
    L2112.TechCoef_reg <- select(A_mineral_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L2112.Production_reg_imp: Output (flow) of gross imports. Calibrated "imports" here needs to match the sum of demands across all mineral-demanding sectors
    L239.Production_reg_imp <- A_ff_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L239.GrossImports_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_EJ) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])





    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
