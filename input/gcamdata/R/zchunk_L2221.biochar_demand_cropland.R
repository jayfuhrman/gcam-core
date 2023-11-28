# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2221.biochar_demand_cropland
#'
#' Prepare the assumptions for teh demand of biochar in croppland; similar to fertilizer demand
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2221.LN5_HistMgdAllocation_crop_biochar}, \code{L2221.LN5_HistMgdAllocation_bio_biochar},
#' \code{L2221.LN5_MgdAllocation_crop_biochar}, \code{L2221.LN5_MgdAllocation_bio_biochar},
#' \code{L2221.LN5_MgdCarbon_crop_biochar}, \code{L2221.LN5_MgdCarbon_bio_biochar},
#' \code{L2221.LN5_LeafGhostShare}, \code{L2221.LN5_LeafGhostShare_bio},
#' \code{L2221.AgProduction_ag_irr_mgmt}, \code{L2221.AgHAtoCL_irr_mgmt},
#' \code{L2221.AgYield_biochar_ref}, \code{L2221.AgYield_bio_biochar_ref},
#' \code{L2221.AgCost_ag_irr_mgmt}, \code{L2221.AgCost_bio_irr_mgmt},
#' \code{L2221.AgProdChange_ag_irr_ref}, \code{L2221.AgProdChange_bio_irr_ref},
#' \code{L2221.AgCoef_Fert_ag_irr_mgmt}, \code{L2221.AgCoef_Fert_bio_irr_mgmt},
#' \code{L2221.AgCost_ag_irr_mgmt_adj}, \code{L2221.AgCost_bio_irr_mgmt_adj}, \code{L2221.AgResBio_ag_irr_mgmt},
#' \code{L2221.AgResBioCurve_ag_irr_mgmt}, \code{L2221.AgCoef_Biochar_ag_irr_mgmt}, \code{L2221.AgCost_Biochar_irr_mgmt_adj},
#' \code{L2221.AgCoef_Biochar_bio_irr_mgmt}, \code{L2221.AgCost_Biochar_irr_mgmt_adj_bio},
#' \code{L2221.CarbonCoef}, \code{L2221.AgCoef_IrrBphysWater_ag_mgmt}, \code{L2221.AgCoef_BphysWater_bio_mgmt},
#' \code{L2221.AgCoef_IrrWaterWdraw_ag_mgmt}, \code{L2221.AgCoef_IrrWaterWdraw_bio_mgmt},
#' \code{L2221.AgCoef_IrrWaterCons_ag_mgmt}, \code{L2221.AgCoef_IrrWaterCons_bio_mgmt},
#' \code{L2221.AgCoef_RfdBphysWater_ag_mgmt},
#' @details This chunk sets up the biochar demand in agreiculture. We assume an applicaiton rate of 20 tons of
#' biochar per hectare. We follow similar approach to fertilizer to calculate IO coefficients.
#' Applies only to future years
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author MCB March 2021
module_energy_L2221.biochar_demand_cropland <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(# PART 1: biochar_land_input_5_IRR_MGMT.xml
      "L2252.LN5_HistMgdAllocation_crop",
      "L2252.LN5_HistMgdAllocation_bio",
      "L2252.LN5_MgdAllocation_crop",
      "L2252.LN5_MgdAllocation_bio",
      "L2252.LN5_MgdCarbon_crop",
      "L2252.LN5_MgdCarbon_bio",
      FILE = "energy/A221.ghost_shrwt",
      # PART 2: biochar_ag_For_Past_bio_base_IRR_MGMT.xml
      "L2012.AgProduction_ag_irr_mgmt",
      "L2012.AgHAtoCL_irr_mgmt",
      "L2012.AgYield_bio_ref",
      # PART 3: biochar_ag_cost_IRR_MGMT.xml
      "L2052.AgCost_ag_irr_mgmt",
      "L2052.AgCost_bio_irr_mgmt",
      # PART 4: ag_prodchange_ref_IRR_MGMT.xml
      "L2052.AgProdChange_ag_irr_ref",
      "L2052.AgProdChange_bio_irr_ref",
      FILE = "common/GCAM_region_names_climate_zone",
      FILE = "aglu/A221.biochar_yield_increase",
      FILE = "aglu/A221.biochar_land_alloc",
      # PART 5: biochar_ag_Fert_IRR_MGMT.xml
      "L2062.AgCoef_Fert_ag_irr_mgmt",
      "L2062.AgCoef_Fert_bio_irr_mgmt",
      "L2062.AgCost_ag_irr_mgmt_adj",
      "L2062.AgCost_bio_irr_mgmt_adj",
      # PART 6: biochar_resbio_input_IRR_MGMT.xml
      "L2042.AgResBio_ag_irr_mgmt",
      "L2042.AgResBioCurve_ag_irr_mgmt",
      # PART 7: biochar_ag_demand.xml
      FILE = "energy/A221.A_PrimaryFuelCCoef",
      FILE = "common/GCAM_region_names",
      FILE = "aglu/A221.bio_tech_prod",
      FILE = "aglu/A221.bio_tech_land_alloc",
      # PART 8: biochar_ag_water_input_IRR_MGMT.xml
      "L2072.AgCoef_IrrBphysWater_ag_mgmt",
      "L2072.AgCoef_IrrWaterWdraw_ag_mgmt",
      "L2072.AgCoef_IrrWaterCons_ag_mgmt",
      "L2072.AgCoef_RfdBphysWater_ag_mgmt",
      "L2072.AgCoef_BphysWater_bio_mgmt",
      "L2072.AgCoef_IrrWaterWdraw_bio_mgmt",
      "L2072.AgCoef_IrrWaterCons_bio_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(#1. biochar_land_input_5_IRR_MGMT.xml
      "L2221.LN5_HistMgdAllocation_crop_biochar",
      "L2221.LN5_HistMgdAllocation_bio_biochar",
      "L2221.LN5_MgdAllocation_crop_biochar",
      "L2221.LN5_MgdAllocation_bio_biochar",
      "L2221.LN5_MgdCarbon_crop_biochar",
      "L2221.LN5_MgdCarbon_bio_biochar",
      "L2221.LN5_LeafGhostShare",
      "L2221.LN5_LeafGhostShare_bio",
      #2. biochar_ag_For_Past_bio_base_IRR_MGMT.xml
      "L2221.AgProduction_ag_irr_mgmt",
      "L2221.AgHAtoCL_irr_mgmt",
      "L2221.AgYield_biochar_ref",
      "L2221.AgYield_bio_biochar_ref",
      #3. biochar_ag_cost_IRR_MGMT.xml
      "L2221.AgCost_ag_irr_mgmt",
      "L2221.AgCost_bio_irr_mgmt",
      #4. ag_prodchange_ref_IRR_MGMT.xml
      "L2221.AgProdChange_ag_irr_ref",
      "L2221.AgProdChange_bio_irr_ref",
      #5. biochar_ag_Fert_IRR_MGMT.xml
      "L2221.AgCoef_Fert_ag_irr_mgmt",
      "L2221.AgCoef_Fert_bio_irr_mgmt",
      "L2221.AgCost_ag_irr_mgmt_adj",
      "L2221.AgCost_bio_irr_mgmt_adj",
      #6. biochar_resbio_input_IRR_MGMT.xml
      "L2221.AgResBio_ag_irr_mgmt",
      "L2221.AgResBioCurve_ag_irr_mgmt",
      #7. biochar_ag_demand.xml
      "L2221.AgCoef_Biochar_ag_irr_mgmt",
      "L2221.AgCost_Biochar_irr_mgmt_adj",
      "L2221.AgCoef_Biochar_bio_irr_mgmt",
      "L2221.AgCost_Biochar_irr_mgmt_adj_bio",
      "L2221.CarbonCoef",
      #8. biochar_ag_water_input_IRR_MGMT.xml
      "L2221.AgCoef_IrrBphysWater_ag_mgmt",
      "L2221.AgCoef_BphysWater_bio_mgmt",
      "L2221.AgCoef_IrrWaterWdraw_ag_mgmt",
      "L2221.AgCoef_IrrWaterWdraw_bio_mgmt",
      "L2221.AgCoef_IrrWaterCons_ag_mgmt",
      "L2221.AgCoef_IrrWaterCons_bio_mgmt",
      "L2221.AgCoef_RfdBphysWater_ag_mgmt"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silencing global variable package checks
    GCAM_region_ID <- calOutputValue <- coefficient <- fuel <- half.life <- input.cost <- lifetime <-
      median.shutdown.point <- minicam.energy.input <- minicam.non.energy.input <- object <-
      profit.shutdown.steepness <- region <- remove.fraction <- sector <- sector.name <- share.weight <-
      shutdown.rate <- steepness <- stub.technology <- subsector <- subsector.name <- supplysector <-
      technology <- to.value <- value <- year <- year.fillout <- year.share.weight <- year.x <- year.y <-
      primary.consumption <- NULL

    # Load required inputs
    #PART 1: biochar_land_input_5_IRR_MGMT.xml
    L2252.LN5_HistMgdAllocation_crop <- get_data(all_data, "L2252.LN5_HistMgdAllocation_crop")
    L2252.LN5_HistMgdAllocation_bio <- get_data(all_data, "L2252.LN5_HistMgdAllocation_bio")
    L2252.LN5_MgdAllocation_crop <- get_data(all_data, "L2252.LN5_MgdAllocation_crop")
    L2252.LN5_MgdAllocation_bio <- get_data(all_data, "L2252.LN5_MgdAllocation_bio")
    L2252.LN5_MgdCarbon_crop <- get_data(all_data, "L2252.LN5_MgdCarbon_crop")
    L2252.LN5_MgdCarbon_bio <- get_data(all_data, "L2252.LN5_MgdCarbon_bio")
    A221.ghost_shrwt <- get_data(all_data, "energy/A221.ghost_shrwt")

    #PART 2: biochar_ag_For_Past_bio_base_IRR_MGMT.xml
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
    L2012.AgHAtoCL_irr_mgmt <- get_data(all_data, "L2012.AgHAtoCL_irr_mgmt")
    L2012.AgYield_bio_ref <- get_data(all_data, "L2012.AgYield_bio_ref")

    #PART 3: biochar_ag_cost_IRR_MGMT.xml
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "L2052.AgCost_ag_irr_mgmt")
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt")

    #PART 4: ag_prodchange_ref_IRR_MGMT.xml
    L2052.AgProdChange_ag_irr_ref <- get_data(all_data, "L2052.AgProdChange_ag_irr_ref")
    L2052.AgProdChange_bio_irr_ref <- get_data(all_data, "L2052.AgProdChange_bio_irr_ref")
    GCAM_region_names_climate_zone <- get_data(all_data, "common/GCAM_region_names_climate_zone")
    A221.biochar_yield_increase <- get_data(all_data, "aglu/A221.biochar_yield_increase")
    A221.biochar_land_alloc <- get_data(all_data, "aglu/A221.biochar_land_alloc")

    #PART 5: biochar_ag_Fert_IRR_MGMT.xml
    L2062.AgCoef_Fert_ag_irr_mgmt <- get_data(all_data, "L2062.AgCoef_Fert_ag_irr_mgmt")
    L2062.AgCoef_Fert_bio_irr_mgmt <- get_data(all_data, "L2062.AgCoef_Fert_bio_irr_mgmt")
    L2062.AgCost_ag_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_ag_irr_mgmt_adj")
    L2062.AgCost_bio_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_bio_irr_mgmt_adj")

    #PART 6: biochar_resbio_input_IRR_MGMT.xml
    L2042.AgResBio_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBio_ag_irr_mgmt")
    L2042.AgResBioCurve_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBioCurve_ag_irr_mgmt")

    #PART 7: biochar_ag_demand.xml
    A221.A_PrimaryFuelCCoef <- get_data(all_data, "energy/A221.A_PrimaryFuelCCoef")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A221.bio_tech_prod <- get_data(all_data, "aglu/A221.bio_tech_prod")
    A221.bio_tech_land_alloc <- get_data(all_data, "aglu/A221.bio_tech_land_alloc")
    # And we also use L2252.LN5_MgdAllocation_crop, L2012.AgProduction_ag_irr_mgmt, L2062.AgCost_ag_irr_mgmt_adj

    #PART 8: biochar_ag_water_input_IRR_MGMT.xml
    L2072.AgCoef_IrrBphysWater_ag_mgmt <- get_data(all_data, "L2072.AgCoef_IrrBphysWater_ag_mgmt")
    L2072.AgCoef_IrrWaterWdraw_ag_mgmt <- get_data(all_data, "L2072.AgCoef_IrrWaterWdraw_ag_mgmt")
    L2072.AgCoef_IrrWaterCons_ag_mgmt <- get_data(all_data, "L2072.AgCoef_IrrWaterCons_ag_mgmt")
    L2072.AgCoef_RfdBphysWater_ag_mgmt <- get_data(all_data, "L2072.AgCoef_RfdBphysWater_ag_mgmt")
    L2072.AgCoef_BphysWater_bio_mgmt <- get_data(all_data, "L2072.AgCoef_BphysWater_bio_mgmt")
    L2072.AgCoef_IrrWaterWdraw_bio_mgmt <- get_data(all_data, "L2072.AgCoef_IrrWaterWdraw_bio_mgmt")
    L2072.AgCoef_IrrWaterCons_bio_mgmt <- get_data(all_data, "L2072.AgCoef_IrrWaterCons_bio_mgmt")


    # ===================================================
    test.region <- c(GCAM_region_names$region)

    # We create the biochar demand in cropland.
    # Since we are creating new technologies that demand the biochar in model years, we need to create the stucture in different
    # parts of the land system.

    #### PART 1: Historical land allocation (biochar_land_input_5_IRR_MGMT.xml) ####
    # Here we want to add to xml land_input_5 the new "technologies" that demand biochar
    # which includes all high techs (both rainfed and irrigated)
    # Even though they have no historical values, we need to include them in the structure, as it is the case
    # with purpose grown-biomass.

    ## L2221.LN5_HistMgdAllocation_crop_biochar
    L2252.LN5_HistMgdAllocation_crop %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("LandLeaf", c("LandLeaf", "suffix"), sep="_") %>%
      # Set hsitorical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0)-> L2221.LN5_HistMgdAllocation_crop_biochar # OUTPUT

    ## L2221.LN5_HistMgdAllocation_bio_biochar
    L2252.LN5_HistMgdAllocation_bio %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("LandLeaf", c("LandLeaf", "suffix"), sep="_") %>%
      # Set hsitorical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0)-> L2221.LN5_HistMgdAllocation_bio_biochar # OUTPUT

    ## L2221.LN5_MgdAllocation_crop_biochar
    L2252.LN5_MgdAllocation_crop %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("LandLeaf", c("LandLeaf", "suffix"), sep="_") %>%
      # Set hsitorical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0)-> L2221.LN5_MgdAllocation_crop_biochar # OUTPUT

    ## L2221.LN5_MgdAllocation_bio_biochar
    L2252.LN5_MgdAllocation_bio %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("LandLeaf", c("LandLeaf", "suffix"), sep="_") %>%
      # Set hsitorical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0)-> L2221.LN5_MgdAllocation_bio_biochar # OUTPUT

    ## L2221.LN5_MgdCarbon_crop_biochar
    L2252.LN5_MgdCarbon_crop %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("LandLeaf", c("LandLeaf", "suffix"), sep="_") %>%
      # Set hsitorical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0)-> L2221.LN5_MgdCarbon_crop_biochar # OUTPUT

    ## L2221.LN5_MgdCarbon_bio_biochar
    L2252.LN5_MgdCarbon_bio %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("LandLeaf", c("LandLeaf", "suffix"), sep="_") %>%
      # Set hsitorical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0)-> L2221.LN5_MgdCarbon_bio_biochar # OUTPUT

    ## L2221.LN5_LeafGhostShare
    # Since there is no historical land allocaiton for biochar technologies, we need to include this "ghost share" so that it exists in the future
    # otherwise the model does not know there should be any land for this
    A221.ghost_shrwt %>%
      write_to_all_regions(names = c("region", "year", "ghost.unnormalized.share"), GCAM_region_names) -> ghost_shrwt_region

    # We want to use the land allocation table for the crops and basins
    L2221.LN5_MgdAllocation_crop_biochar %>%
      # But we do not need neither the year nor the allocaiton amount
      select(-year, -allocation) %>%
      # Now we want to keep only 1 version of each combination, since we ar enot using year there are duplicates
      distinct() %>%
      # Now we join the table with the ghost share weights for different years
      left_join_error_no_match(ghost_shrwt_region, by = "region") %>%
      select(LEVEL2_DATA_NAMES$LN5_LeafGhostShare)-> L2221.LN5_LeafGhostShare # OUTPUT

    L2221.LN5_MgdAllocation_bio_biochar %>%
      # But we do not need neither the year nor the allocaiton amount
      select(-year, -allocation) %>%
      # Now we want to keep only 1 version of each combination, since we ar enot using year there are duplicates
      distinct() %>%
      # Now we join the table with the ghost share weights for different years
      left_join_error_no_match(ghost_shrwt_region, by = "region") %>%
      select(LEVEL2_DATA_NAMES$LN5_LeafGhostShare)-> L2221.LN5_LeafGhostShare_bio # OUTPUT

    #=============================================================================================================================

    #### PART 2: Agricultural Production (biochar_ag_For_Past_bio_base_IRR_MGMT.xml) ####
    ## L2221.AgProduction_ag_irr_mgmt
    L2012.AgProduction_ag_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") %>%
      # Set historical production to 0, and tech shareweight to 0. Subsector should not change
      mutate(calOutputValue = 0,
             tech.share.weight = 0) -> L2221.AgProduction_ag_irr_mgmt # OUTPUT

    ## L2221.AgHAtoCL_irr_mgmt
    L2012.AgHAtoCL_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgHAtoCL_irr_mgmt # OUTPUT

    ## L2221.AgYield_biochar_ref
    # We need to create this table with yields for biochar technologies, since there is no production nor land allocaiton,
    # therefore the model calculates the yield as 0. Biomass has a detrmined yield in the xml.
    # since new biochar technologies are the same as their equivalent high tech technologies,
    # we need to calculate the yields based on agricultural produciton and land allocation
    # and then manually include them
    L2012.AgProduction_ag_irr_mgmt %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      left_join(L2252.LN5_MgdAllocation_crop, by = c("AgSupplySubsector" = "LandNode4", "AgProductionTechnology" = "LandLeaf","region", "year"))%>%
      # Here we create the biochar suffix to add to the technologies; and calculate the yields
      # and we replace NAs with 0 yields
      mutate(suffix = "biochar",
             yield = calOutputValue / allocation,
             yield = if_else(is.na(yield), 0, yield)) %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") %>%
      select(LEVEL2_DATA_NAMES$AgYield)-> L2221.AgYield_biochar_ref # OUTPUT

    ## L2221.AgYield_bio_biochar_ref
    L2012.AgYield_bio_ref %>%
      # filter for high techs only
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgYield_bio_biochar_ref # OUTPUT

    #=============================================================================================================================

    #### PART 3: Agricultural production costs (biochar_ag_cost_IRR_MGMT.xml) ####
    ## L2221.AgCost_ag_irr_mgmt
    L2052.AgCost_ag_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCost_ag_irr_mgmt # OUTPUT

    ## L2221.AgCost_bio_irr_mgmt
    L2052.AgCost_bio_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCost_bio_irr_mgmt # OUTPUT

    #=============================================================================================================================

    #### PART 4: Agricultural production Change (biochar_ag_prodchange_ref_IRR_MGMT.xml) ####
    # Here we want to clauclate the yield increases from biochar application
    # This yield improvement is based on tropical/temeprate regions, and between irrigated and rainfed
    # These are teh steps we follow to calculate the increase yields, considering existing ag prod change
    # 1. Share of total biochar land  over total land:
    #We first assume that biochar is applied linearly from 2020-2100, in a period of 80 years
    #(1.25% per year, or 6.25% every 5 years)
    # 2. Average yield on biochar land:
    #Then we apply to that percentage of land the yield increase of applying the biochar
    #(we multiply both; add 1; multiply by 1)
    # 3. Cumulative base yield improvement:
    #Then we calculate the cumulative base yield improvement
    #(add 1 to the base annual yield improvement, multiply by previous cumulative base yield improvement
    #and then raise to 5)
    # 4. Biochar yield cumulative improvement:
    #Then we multiply the average yield increase (step 2) times the cumulative base yield improvement (step 3)
    # 5. Annual biochar yield improvement:
    #Finally, we turn this parameter into an annual yield improvement.


    ## L2221.AgProdChange_ag_irr_ref
    L2052.AgProdChange_ag_irr_ref %>%
      #Filter for regions we are analyzing (either all or some specific one), and for high technologies
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") %>%
      # Now get column to distinguish irrigated vs rainfed
      mutate(water = if_else(grepl("IRR", AgProductionTechnology), "irrigated", "rainfed")) %>%
      # Add climate zones tables
      left_join_error_no_match(GCAM_region_names_climate_zone, by = "region") %>%
      # Now add the table with the yield improvements as percentages, distinguishing from irrigated-rainfed, and tropical/temperate
      left_join_error_no_match(A221.biochar_yield_increase, by = c("water", "zone")) %>%
      # (Step 1): Finally, add the table with percentage of biochar land per period, which has the information for step 1
      left_join_error_no_match(A221.biochar_land_alloc, by = "year") %>%
      # There are 3 specifc GLUs that have an AgPordChange parameter of -1, which would affect our following calculaitons
      # So we just change those three from -1 to -0.9999
      # The three GLUs are: Wheat_ShebJubR_IRR_hi_biochar; OtherGrain_AfrCstNW_IRR_hi_biochar; OtherGrain_LChad_IRR_hi_biochar
      mutate(AgProdChange = if_else(AgProdChange == -1, -0.9999, AgProdChange)) %>%
      # Now everything is ready, perform calculations
      # Step 2: Get the average yield on biochar land
      # considering the percentage of land with biochar, and the yield increase of appliying the biochar
      mutate(av_yield_biochar = 1*(1 + percentage * yield_increase)) %>%
      # Step 3: Get cumulative base yield improvements (without biochar)
      # Since this calculation is recursive, we need to set a for loop
      # Each computaiton is dependent on the computaiton from teh previous year (within each basin and technology and crop)
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, water, GCAM_region_ID, zone) %>%
      mutate(cum_base_yield_improv = 1) -> temp

    for (y in unique(temp$year)) {
      temp %>%
        mutate(cum_base_yield_improv = lag(cum_base_yield_improv)*(1 + AgProdChange)^5) %>%
        replace_na(list(cum_base_yield_improv = 1))->temp
    }

    # Now continue with computations
    temp %>%
      # Step 4: Get cumulative biochar yield improvement
      mutate(cum_biochar_yield_improv = cum_base_yield_improv * av_yield_biochar) %>%
      # Step 5: Get annual biochar yield improvement
      #We need to group first so that the divisin happens wihtin the same country, basin, crop, tech, and compared to previous year
      #and not across different regions.
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, water, GCAM_region_ID, zone) %>%
      mutate(annual_biochar_yield_improv = ((cum_biochar_yield_improv / lag(cum_biochar_yield_improv))^(1/5))-1) %>%
      ungroup() %>%
      # Now replace NA in 2020 with the same ag prod change from the default technology, since the yield increase comes after the application
      mutate(annual_biochar_yield_improv = if_else(year == 2020 & is.na(annual_biochar_yield_improv), AgProdChange, annual_biochar_yield_improv)) %>%
      # Now reanme old ag prod change, and keep the new one
      rename(old_AgProdChange = AgProdChange,
             AgProdChange = annual_biochar_yield_improv) %>%
      select(LEVEL2_DATA_NAMES$AgProdChange)-> L2221.AgProdChange_ag_irr_ref # OUTPUT


    ## L2221.AgProdChange_bio_irr_ref
    L2052.AgProdChange_bio_irr_ref %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") %>%
      # Now get column to distinguish irrigated vs rainfed
      mutate(water = if_else(grepl("IRR", AgProductionTechnology), "irrigated", "rainfed")) %>%
      # Add climate zones tables
      left_join_error_no_match(GCAM_region_names_climate_zone, by = "region") %>%
      # Now add the table with the yield improvements as percentages, distinguishing from irrigated-rainfed, and tropical/temperate
      left_join_error_no_match(A221.biochar_yield_increase, by = c("water", "zone")) %>%
      # (Step 1): Finally, add the table with percentage of biochar land per period, which has the information for step 1
      left_join_error_no_match(A221.biochar_land_alloc, by = "year") %>%
      # Now everything is ready, perform calculations
      # Step 2: Get the average yield on biochar land
      # considering the percentage of land with biochar, and the yield increase of appliying the biochar
      mutate(av_yield_biochar = 1*(1 + percentage * yield_increase)) %>%
      # Step 3: Get cumulative base yield improvements (without biochar)
      # Since this calculation is recursive, we need to set a for loop
      # Each computaiton is dependent on the computaiton from teh previous year (within each basin and technology and crop)
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, water, GCAM_region_ID, zone) %>%
      mutate(cum_base_yield_improv = 1) -> temp_bio

    for (y in unique(temp_bio$year)) {
      temp_bio %>%
        mutate(cum_base_yield_improv = lag(cum_base_yield_improv)*(1 + AgProdChange)^5) %>%
        replace_na(list(cum_base_yield_improv = 1))->temp_bio
    }

    # Now continue with computations
    temp_bio %>%
      # Step 4: Get cumulative biochar yield improvement
      mutate(cum_biochar_yield_improv = cum_base_yield_improv * av_yield_biochar) %>%
      # Step 5: Get annual biochar yield improvement
      #We need to group first so that the divisin happens wihtin the same country, basin, crop, tech, and compared to previous year
      #and not across different regions.
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, water, GCAM_region_ID, zone) %>%
      mutate(annual_biochar_yield_improv = ((cum_biochar_yield_improv / lag(cum_biochar_yield_improv))^(1/5))-1) %>%
      ungroup() %>%
      # Now replace NA in 2020 with the same ag prod change from the default technology, since the yield increase comes after the application
      mutate(annual_biochar_yield_improv = if_else(year == 2020 & is.na(annual_biochar_yield_improv), AgProdChange, annual_biochar_yield_improv)) %>%
      # Now reanme old ag prod change, and keep the new one
      rename(old_AgProdChange = AgProdChange,
             AgProdChange = annual_biochar_yield_improv) %>%
      select(LEVEL2_DATA_NAMES$AgProdChange)-> L2221.AgProdChange_bio_irr_ref # OUTPUT

    #=============================================================================================================================

    #### PART 5: Fertilizer information (biochar_ag_Fert_IRR_MGMT.xml) ####
    ## L2221.AgCoef_Fert_ag_irr_mgmt
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_Fert_ag_irr_mgmt # OUTPUT

    ## L2221.AgCoef_Fert_bio_irr_mgmt
    L2062.AgCoef_Fert_bio_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_Fert_bio_irr_mgmt # OUTPUT

    ## L2221.AgCost_ag_irr_mgmt_adj
    L2062.AgCost_ag_irr_mgmt_adj %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCost_ag_irr_mgmt_adj # OUTPUT

    ## L2221.AgCost_bio_irr_mgmt_adj
    L2062.AgCost_bio_irr_mgmt_adj %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCost_bio_irr_mgmt_adj # OUTPUT

    #=============================================================================================================================

    #### PART 6: Agriculture residues from cropland (biochar_resbio_input_IRR_MGMT.xml) ####
    ## L2221.AgResBio_ag_irr_mgmt
    L2042.AgResBio_ag_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgResBio_ag_irr_mgmt # OUTPUT

    ## L2221.AgResBioCurve_ag_irr_mgmt
    L2042.AgResBioCurve_ag_irr_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgResBioCurve_ag_irr_mgmt # OUTPUT

    #=============================================================================================================================

    #### PART 7: Create Agriculture Demand for Biochar (biochar_ag_demand.xml) ####
    # This part is used to create the demand for biochar in agricultural crops
    # We follow a similar approach to fertilizer input to agriculture
    # Based on output table L142.ag_Fert_IO_R_C_Y_GLU from LB142.ag_Fert_IO_R_C_Y_GLU fertilizer is calculated as:
    # Input-output coefficients for each crop are first calculated as fertilizer demands divided by agriculture production in the base year

    # We use similar approach with biochar demand (assumed) divided by agriculture produciton in the base year
    # Steps:
    # 1. First filter for base year, and for high tech crops
    # 2. from total land allocation get total biochar applicaiton (assuming certain biochar application rate from literature)
    # 3. We assume that the total from step 2 is absolute maximum biochar application. To get the rate per year we divide total by 80
    #(which are the years modeled, since biochar application starts in 2020)
    # 4. Then that biochar per year applicaiton (in Mt) is divided by the agricultural produciton (in Mt) to get the coefficient

    ## L2221.AgCoef_Biochar_ag_irr_mgmt
    # Estimate coefficients for biochar demand
    # Get the total amount of land per crop, basin and tech for 2015 only
    # We assume that only High tech crops demand biochar
    # But we create a new ag sector that competes with the other ones.
    L2252.LN5_MgdAllocation_crop %>%
      filter(region %in% test.region,
             year == MODEL_FINAL_BASE_YEAR,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("LandLeaf", c("LandLeaf", "suffix"), sep="_") %>%
      # Units are in thousand km2 and we need them in hectares
      # Then we assume certain biochar applicaiton rate named at the constant "biochar.rate"
      # This is expected to be the absolute maximum (total amount of land times "X" tons per hectare)
      # Then divide by the remaining modeled years to get a rate (from constant "assumed.biochar.years")
      # This will give us the demand in a given year for biochar
      mutate(allocation_ha = allocation * 1000 * km2.to.ha,
             # 10^6 since we need units in megatons, and rate is in tons
             max.bicohar_MT = (allocation_ha * biochar.rate) / 10^6,
             biochar.year_MT = max.bicohar_MT / assumed.biochar.years,
             #Get allocation in kg per hectare, since units are in kg of biochar per kg of crop
             biochar.year_kg = biochar.year_MT * 10^9) %>%
      rename("AgSupplySubsector" = "LandNode4", "AgProductionTechnology" =  "LandLeaf") -> L2252.LN5_MgdAllocation_crop_biochar

    # Now prepare the table with the agricultural production
    L2012.AgProduction_ag_irr_mgmt %>%
      filter(region %in% test.region,
             year == MODEL_FINAL_BASE_YEAR,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      # Since units are in Mt we convert them to kg
      mutate(prod_kg = calOutputValue * 10^9) %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2012.AgProduction_ag_irr_mgmt_biochar

    # Merge tables
    L2252.LN5_MgdAllocation_crop_biochar %>%
      # Join table with agriculture production by crops
      left_join_error_no_match(L2012.AgProduction_ag_irr_mgmt_biochar, by = c("region", "year", "AgSupplySubsector", "AgProductionTechnology")) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, biochar.year_MT, calOutputValue,
             biochar.year_kg, prod_kg) %>%
      # Now compute coefficient as biochar.year_MT divided total crop production (as with fertilzier)
      # This is Megatons of biochar / Mt of crop produced in that land; or kf of biochar / kg of crop production
      mutate(minicam.energy.input = "biochar",
             coefficient = biochar.year_MT / calOutputValue,
             coefficient_kg = biochar.year_kg / prod_kg) %>%
      # Complete other years
      complete(nesting(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology,
                       minicam.energy.input, coefficient), year = c(year, MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$AgCoef)-> L2221.AgCoef_Biochar_ag_irr_mgmt # OUTPUT

    ## L2221.AgCost_Biochar_irr_mgmt_adj
    L2062.AgCost_ag_irr_mgmt_adj %>% # We use the adjsuted costs of applying fertilizer here
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") %>%
      # filter out non existent crops
      semi_join(L2221.AgCoef_Biochar_ag_irr_mgmt, by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year"))%>%
      filter(grepl("_hi", AgProductionTechnology)) %>%
      select(LEVEL2_DATA_NAMES$AgCost) -> L2221.AgCost_Biochar_irr_mgmt_adj # OUTPUT


    # Now do the same for biomass crops, however, biomass does not have historical production
    # One option is to use model outputs for 2020 from a GCAM v5.3 REF case, read them in here,
    # calculate coefficients and use those (revise if assumption makes sense)
    # We first prepare the table with the necessary heading
    L2252.LN5_MgdAllocation_bio %>%
      select(-year, -allocation) %>%
      distinct() -> L2252.LN5_MgdAllocation_bio_head

    ## L2221.AgCoef_Biochar_bio_irr_mgmt
    A221.bio_tech_land_alloc %>%
      filter(region %in% test.region,
             grepl("_hi", LandLeaf)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      rename(LandLeaf2 = LandLeaf) %>%
      unite("LandLeaf", c("LandLeaf2", "suffix"), sep="_", remove = FALSE) %>%
      rename(allocation = `2020`) %>%
      # Units are in thousand km2 and we need them in hectares
      # Then we assume certain biochar applicaiton rate named in the constant "biochar.rate"
      # This is expected to be the absolute maximum (total amount of land times "X" tons per hectare)
      # Then divide by the remaining modeled years to get a rate (from constant "assumed.biochar.years")
      # This will give us the demand in a given year for biochar
      mutate(allocation_ha = allocation * 1000 * km2.to.ha,
             # 10^6 since we need units in megatons, and rate is in tons
             max.bicohar_MT = (allocation_ha * biochar.rate) / 10^6,
             biochar.year_MT = max.bicohar_MT / assumed.biochar.years,
             # Get application in kg
             biochar.year_kg = biochar.year_MT * 10^9) -> A221.bio_tech_land_alloc

    # to make biochar more robust to GCAM version changes, anti_join to get all region/LandLeaf combos that are in
    # A221.bio_tech_land_alloc (based on GCAM 5.3 outputs), but not in L2252.LN5_MgdAllocation_bio_head, then bind to L2252.LN5_MgdAllocation_bio_head before joining.
    # This will prevent region / basin combos that aren't in the first table because they have zero or numerical zero biomass land allocation from causing the left_join_error_no_match to crash
    unmatched_basins_regions <- anti_join(A221.bio_tech_land_alloc,L2252.LN5_MgdAllocation_bio_head,
                      by = c("region", "LandLeaf2" = "LandLeaf")) %>%
      separate(LandLeaf, sep = "_",into = c('crop','basin','water','fert')) %>%
      mutate(LandAllocatorRoot = 'root',
             LandNode1 = paste0('AgroForestLand_',basin),
             LandNode2 = paste0('AgroForest_NonPasture_',basin),
             LandNode3 = paste0('CropLand_',basin),
             LandNode4 = paste0(crop,'_',basin),
             LandNode5 = paste0(crop,'_',basin,'_',fert)) %>%
      select(region,LandAllocatorRoot,LandNode1,LandNode2,LandNode3,LandNode4,LandNode5,LandLeaf=LandLeaf2)

    A221.bio_tech_land_alloc %>%
      left_join_error_no_match(L2252.LN5_MgdAllocation_bio_head %>% bind_rows(unmatched_basins_regions),
                               by = c("region", "LandLeaf2" = "LandLeaf")) -> L2252.LN5_MgdAllocation_bio_biochar

    # Now prepare the table with the agricultural production L2012.AgProduction_ag_irr_mgmt
    A221.bio_tech_prod %>%
      filter(region %in% test.region,
             grepl("_hi", technology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      rename(production = `2020`) %>%
      unite("AgProductionTechnology", c("technology", "suffix"), sep="_") %>%
      # We need to modify units, since fertilizer inputs to biomass are in kgN per GJ of biomass
      # since we are using the same format, we need to change units from EJ to GJ
      mutate(production_GJ = production * 10^9)->  L2012.AgProduction_bio_irr_mgmt_biochar

    # Merge tables
    L2252.LN5_MgdAllocation_bio_biochar %>%
      # Join table with agriculture production by crops
      left_join(L2012.AgProduction_bio_irr_mgmt_biochar,
                by = c("scenario", "region", "LandLeaf" = "AgProductionTechnology")) %>%
      # There are 74 technologies that do not have ag production, but do have land allocaiton as 0
      # We fill those values with a 0, since there is no land allocated to it, so there can't be production either
      mutate(production = if_else(is.na(production), 0, production),
             production_GJ = if_else(is.na(production_GJ), 0, production_GJ),
             sector = if_else(is.na(sector), "biomass", sector)) %>%
      rename("AgProductionTechnology" = "LandLeaf", "AgSupplySector" = "sector", "AgSupplySubsector" = "LandNode4")%>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, biochar.year_MT, production,
             biochar.year_kg, production_GJ) %>%
      # Now compute coefficient as biochar.year_MT divided total crop production (as with fertilzier)
      mutate(minicam.energy.input = "biochar",
             coefficient = biochar.year_MT / production,
             coefficient_kg_GJ = biochar.year_kg / production_GJ) %>%
      # Replace NaN from 0/0 with a 0
      mutate(coefficient = if_else(is.na(coefficient), 0, coefficient),
             coefficient_kg_GJ = if_else(is.na(coefficient_kg_GJ), 0, coefficient_kg_GJ),
             year = 2020) %>%
      # Replace INF with 0
      mutate(coefficient = if_else(is.infinite(coefficient), 0, coefficient),
             coefficient_kg_GJ = if_else(is.infinite(coefficient_kg_GJ), 0, coefficient_kg_GJ)) %>%
      # Complete other years
      complete(nesting(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology,
                       minicam.energy.input, coefficient), year = c(year, MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$AgCoef)-> L2221.AgCoef_Biochar_bio_irr_mgmt # OUTPUT

    ## L2221.AgCost_Biochar_irr_mgmt_adj_bio
    L2062.AgCost_bio_irr_mgmt_adj %>% # We use the adjsuted costs of applying fertilizer here
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") %>%
      # filter out non existent crops
      semi_join(L2221.AgCoef_Biochar_bio_irr_mgmt, by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year"))%>%
      filter(grepl("_hi", AgProductionTechnology)) %>%
      select(LEVEL2_DATA_NAMES$AgCost) -> L2221.AgCost_Biochar_irr_mgmt_adj_bio # OUTPUT

    ## L2221.CarbonCoef
    A221.A_PrimaryFuelCCoef %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["CarbonCoef"]]), GCAM_region_names = GCAM_region_names, has_traded = TRUE) -> L2221.CarbonCoef # OUTPUT

    #=============================================================================================================================

    #### PART 8: Create Water demand for newly created technologies (biochar_ag_water_input_IRR_MGMT.xml)####
    # Here we create the water demands for the new technologies, so that they do not have any missing information
    ## L2221.AgCoef_IrrBphysWater_ag_mgmt
    L2072.AgCoef_IrrBphysWater_ag_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_IrrBphysWater_ag_mgmt # OUTPUT

    ## L2221.AgCoef_BphysWater_bio_mgmt
    L2072.AgCoef_BphysWater_bio_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_BphysWater_bio_mgmt # OUTPUT

    ## L2221.AgCoef_IrrWaterWdraw_ag_mgmt
    L2072.AgCoef_IrrWaterWdraw_ag_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_IrrWaterWdraw_ag_mgmt # OUTPUT

    ## L2221.AgCoef_IrrWaterWdraw_bio_mgmt
    L2072.AgCoef_IrrWaterWdraw_bio_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_IrrWaterWdraw_bio_mgmt # OUTPUT

    ## L2221.AgCoef_IrrWaterCons_ag_mgmt
    L2072.AgCoef_IrrWaterCons_ag_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_IrrWaterCons_ag_mgmt # OUTPUT

    ## L2221.AgCoef_IrrWaterCons_bio_mgmt
    L2072.AgCoef_IrrWaterCons_bio_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_IrrWaterCons_bio_mgmt # OUTPUT

    ## L2221.AgCoef_RfdBphysWater_ag_mgmt
    L2072.AgCoef_RfdBphysWater_ag_mgmt %>%
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") -> L2221.AgCoef_RfdBphysWater_ag_mgmt # OUTPUT

    # ====================================================================================================================================

    # Produce outputs
    #1. biochar_land_input_5_IRR_MGMT.xml
    L2221.LN5_HistMgdAllocation_crop_biochar %>%
      add_title("Historical land allocaiton biochar techs (prior 1975)") %>%
      add_units("Mt") %>%
      add_comments("Written to all high tech regions, historical values are 0") %>%
      add_legacy_name("L2221.LN5_HistMgdAllocation_crop_biochar") %>%
      add_precursors("L2252.LN5_HistMgdAllocation_crop") ->
      L2221.LN5_HistMgdAllocation_crop_biochar

    L2221.LN5_HistMgdAllocation_bio_biochar %>%
      add_title("Historical biomass land allocaiton biochar techs (prior 1975)") %>%
      add_units("Mt") %>%
      add_comments("Written to all high tech regions, historical values are 0") %>%
      add_legacy_name("L2221.LN5_HistMgdAllocation_bio_biochar") %>%
      add_precursors("L2252.LN5_HistMgdAllocation_bio") ->
      L2221.LN5_HistMgdAllocation_bio_biochar

    L2221.LN5_MgdAllocation_crop_biochar %>%
      add_title("Historical land allocaiton biochar techs (1975 - 2015)") %>%
      add_units("Mt") %>%
      add_comments("Written to all high tech regions, historical values are 0") %>%
      add_legacy_name("L2221.LN5_MgdAllocation_crop_biochar") %>%
      add_precursors("L2252.LN5_MgdAllocation_crop") ->
      L2221.LN5_MgdAllocation_crop_biochar

    L2221.LN5_MgdAllocation_bio_biochar %>%
      add_title("Historical biomass land allocaiton biochar techs (1975 - 2015)") %>%
      add_units("Mt") %>%
      add_comments("Written to all high tech regions, historical values are 0") %>%
      add_legacy_name("L2221.LN5_MgdAllocation_bio_biochar") %>%
      add_precursors("L2252.LN5_MgdAllocation_bio") ->
      L2221.LN5_MgdAllocation_bio_biochar

    L2221.LN5_MgdCarbon_crop_biochar %>%
      add_title("Carbon informaiton for biochar techs") %>%
      add_units("various") %>%
      add_comments("Written to all high tech regions with exact info from L2252.LN5_MgdCarbon_crop") %>%
      add_legacy_name("L2221.LN5_MgdCarbon_crop_biochar") %>%
      add_precursors("L2252.LN5_MgdCarbon_crop") ->
      L2221.LN5_MgdCarbon_crop_biochar

    L2221.LN5_MgdCarbon_bio_biochar %>%
      add_title("Carbon informaiton for biomass biochar techs") %>%
      add_units("various") %>%
      add_comments("Written to all high tech regions with exact info from L2252.LN5_MgdCarbon_crop") %>%
      add_legacy_name("L2221.LN5_MgdCarbon_bio_biochar") %>%
      add_precursors("L2252.LN5_MgdCarbon_bio") ->
      L2221.LN5_MgdCarbon_bio_biochar

    L2221.LN5_LeafGhostShare %>%
      add_title("Ghost share for new biochar land") %>%
      add_units("various") %>%
      add_comments("Ghost shares for biochar land types; otherwise there is not deployment") %>%
      add_legacy_name("L2221.LN5_LeafGhostShare") %>%
      add_precursors("energy/A221.ghost_shrwt", "L2252.LN5_MgdAllocation_crop") ->
      L2221.LN5_LeafGhostShare

    L2221.LN5_LeafGhostShare_bio %>%
      add_title("Ghost share for new biochar land") %>%
      add_units("various") %>%
      add_comments("Ghost shares for biochar land types; otherwise there is not deployment") %>%
      add_legacy_name("L2221.LN5_LeafGhostShare_bio") %>%
      add_precursors("energy/A221.ghost_shrwt", "L2252.LN5_MgdAllocation_bio") ->
      L2221.LN5_LeafGhostShare_bio

    #2. biochar_ag_For_Past_bio_base_IRR_MGMT.xml
    L2221.AgProduction_ag_irr_mgmt %>%
      add_title("Biochar techs calibrated production (0)") %>%
      add_units("Mt") %>%
      add_comments("Calibrated production for biochar crop techs (0)") %>%
      add_legacy_name("L2221.AgProduction_ag_irr_mgmt") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt") ->
      L2221.AgProduction_ag_irr_mgmt

    L2221.AgHAtoCL_irr_mgmt %>%
      add_title("Harvests per yer for biochar techs") %>%
      add_units("Unitless") %>%
      add_comments("Number of harvests per year for new biochar technologies, same as L2221.AgHAtoCL_irr_mgmt") %>%
      add_legacy_name("L2221.AgHAtoCL_irr_mgmt") %>%
      add_precursors("L2012.AgHAtoCL_irr_mgmt") ->
      L2221.AgHAtoCL_irr_mgmt

    L2221.AgYield_biochar_ref %>%
      add_title("Biochar Technology Yields") %>%
      add_units("t/m2???") %>%
      add_comments("Biochar technology yields, since there is no historical production we need to determine them, as with biomass") %>%
      add_legacy_name("L2221.AgYield_biochar_ref") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt", "L2252.LN5_MgdAllocation_crop") ->
      L2221.AgYield_biochar_ref

    L2221.AgYield_bio_biochar_ref %>%
      add_title("Biochar Technology Yields for Biomass") %>%
      add_units("t/m2???") %>%
      add_comments("Biochar technology yields for biomass, since there is no historical production we need to determine them") %>%
      add_legacy_name("L2221.AgYield_bio_biochar_ref") %>%
      add_precursors("L2012.AgYield_bio_ref") ->
      L2221.AgYield_bio_biochar_ref

    #3. biochar_ag_cost_IRR_MGMT.xml
    L2221.AgCost_ag_irr_mgmt %>%
      add_title("Costs for agriculture techs for biochar") %>%
      add_units("1975$ per kg") %>%
      add_comments("Number of harvests per year for new biochar technologies, same as L2221.AgCost_ag_irr_mgmt") %>%
      add_legacy_name("L2221.AgCost_ag_irr_mgmt") %>%
      add_precursors("L2052.AgCost_ag_irr_mgmt") ->
      L2221.AgCost_ag_irr_mgmt

    L2221.AgCost_bio_irr_mgmt %>%
      add_title("Costs for biomass agriculture techs for biochar") %>%
      add_units("1975$ per kg") %>%
      add_comments("Number of harvests per year for new biochar technologies for biomass, same as L2052.AgCost_bio_irr_mgmt") %>%
      add_legacy_name("L2221.AgCost_bio_irr_mgmt") %>%
      add_precursors("L2052.AgCost_bio_irr_mgmt") ->
      L2221.AgCost_bio_irr_mgmt

    #4. ag_prodchange_ref_IRR_MGMT.xml
    L2221.AgProdChange_ag_irr_ref %>%
      add_title("Agricultural Production Change for biochar") %>%
      add_units("Unitless") %>%
      add_comments("Agricultural production change for new biochar technologies, same as L2221.AgProdChange_ag_irr_ref") %>%
      add_legacy_name("L2221.AgProdChange_ag_irr_ref") %>%
      add_precursors("L2052.AgProdChange_ag_irr_ref",
                     "common/GCAM_region_names_climate_zone",
                     "aglu/A221.biochar_yield_increase",
                     "aglu/A221.biochar_land_alloc") ->
      L2221.AgProdChange_ag_irr_ref

    L2221.AgProdChange_bio_irr_ref %>%
      add_title("Biomass agricultural Production Change for biochar") %>%
      add_units("Unitless") %>%
      add_comments("Biomass agricultural production change for new biochar technologies, same as L2221.AgProdChange_ag_irr_ref") %>%
      add_legacy_name("L2221.AgProdChange_bio_irr_ref") %>%
      add_precursors("L2052.AgProdChange_bio_irr_ref",
                     "common/GCAM_region_names_climate_zone",
                     "aglu/A221.biochar_yield_increase",
                     "aglu/A221.biochar_land_alloc") ->
      L2221.AgProdChange_bio_irr_ref

    #5. biochar_ag_Fert_IRR_MGMT.xml
    L2221.AgCoef_Fert_ag_irr_mgmt %>%
      add_title("Fertilizer inputs for biochar technologies") %>%
      add_units("Unitless") %>%
      add_comments("Fertilizer inputs for new biochar techs, same as L2062.AgCoef_Fert_ag_irr_mgmt, but could change to reflect improvements from biochar") %>%
      add_legacy_name("L2221.AgCoef_Fert_ag_irr_mgmt") %>%
      add_precursors("L2062.AgCoef_Fert_ag_irr_mgmt") ->
      L2221.AgCoef_Fert_ag_irr_mgmt

    L2221.AgCoef_Fert_bio_irr_mgmt %>%
      add_title("Fertilizer inputs for biomass biochar technologies") %>%
      add_units("Unitless") %>%
      add_comments("Fertilizer inputs for new biomass biochar techs, same as L2062.AgCoef_Fert_bio_irr_mgmt, but could change to reflect improvements from biochar") %>%
      add_legacy_name("L2221.AgCoef_Fert_bio_irr_mgmt") %>%
      add_precursors("L2062.AgCoef_Fert_bio_irr_mgmt") ->
      L2221.AgCoef_Fert_bio_irr_mgmt

    L2221.AgCost_ag_irr_mgmt_adj %>%
      add_title("Fertilizer costs for biochar land technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Fertilizer inputs for new biochar techs, same as L2062.AgCost_ag_irr_mgmt_adj") %>%
      add_legacy_name("L2221.AgCost_ag_irr_mgmt_adj") %>%
      add_precursors("L2062.AgCost_ag_irr_mgmt_adj") ->
      L2221.AgCost_ag_irr_mgmt_adj

    L2221.AgCost_bio_irr_mgmt_adj %>%
      add_title("Fertilizer costs for biochar land technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Fertilizer inputs for new biochar techs, same as L2062.AgCost_bio_irr_mgmt_adj") %>%
      add_legacy_name("L2221.AgCost_bio_irr_mgmt_adj") %>%
      add_precursors("L2062.AgCost_bio_irr_mgmt_adj") ->
      L2221.AgCost_bio_irr_mgmt_adj

    #6. biochar_resbio_input_IRR_MGMT.xml
    L2221.AgResBio_ag_irr_mgmt %>%
      add_title("Residues from crops for biochar land technologies") %>%
      add_units("Varies") %>%
      add_comments("Crop residues for new biochar techs same as L2042.AgResBio_ag_irr_mgmt") %>%
      add_legacy_name("L2221.AgResBio_ag_irr_mgmt") %>%
      add_precursors("L2042.AgResBio_ag_irr_mgmt") ->
      L2221.AgResBio_ag_irr_mgmt

    L2221.AgResBioCurve_ag_irr_mgmt %>%
      add_title("Residues resource curves for biochar land technologies") %>%
      add_units("Fraction harvested") %>%
      add_comments("Crop residues curves for new biochar techs same as L2042.AgResBioCurve_ag_irr_mgmt") %>%
      add_legacy_name("L2221.AgResBioCurve_ag_irr_mgmt") %>%
      add_precursors("L2042.AgResBioCurve_ag_irr_mgmt") ->
      L2221.AgResBioCurve_ag_irr_mgmt

    #7. biochar_ag_demand.xml
    L2221.AgCoef_Biochar_ag_irr_mgmt %>%
      add_title("Biochar inputs coefficients") %>%
      add_units("Unitless (Mt of biochar per Mt of crop, or kg per kg)") %>%
      add_comments("Written to all high tech regions from L2012.AgProduction_ag_irr_mgmt; assumed applicaiton rate for biochar") %>%
      add_legacy_name("L2221.AgCoef_Biochar_ag_irr_mgmt") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt",
                     "L2252.LN5_MgdAllocation_crop") ->
      L2221.AgCoef_Biochar_ag_irr_mgmt

    L2221.AgCost_Biochar_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for biochar in high tech crops") %>%
      add_units("1975$ per kg") %>%
      add_comments("We use the same cost of pplying fertilizer") %>%
      add_legacy_name("L2221.AgCost_Biochar_irr_mgmt_adj") %>%
      add_precursors("L2062.AgCost_ag_irr_mgmt_adj") ->
      L2221.AgCost_Biochar_irr_mgmt_adj

    L2221.AgCoef_Biochar_bio_irr_mgmt %>%
      add_title("Biochar inputs coefficients for biomass crop technologies") %>%
      add_units("Unitless (Mt of biochar per EJ of biomass, or kg of biochar per GJ of biomass)") %>%
      add_comments("Written to all high tech regions from A221.bio_tech_prod; assumed applicaiton rate for biochar") %>%
      add_legacy_name("L2221.AgCoef_Biochar_bio_irr_mgmt") %>%
      add_precursors("aglu/A221.bio_tech_land_alloc",
                     "aglu/A221.bio_tech_prod") ->
      L2221.AgCoef_Biochar_bio_irr_mgmt

    L2221.AgCost_Biochar_irr_mgmt_adj_bio %>%
      add_title("Adjusted non-land variable cost for biochar in high tech crops") %>%
      add_units("1975$ per GJ") %>%
      add_comments("We use the same cost of pplying fertilizer") %>%
      add_legacy_name("L2221.AgCost_Biochar_irr_mgmt_adj_bio") %>%
      add_precursors("L2062.AgCost_bio_irr_mgmt_adj") ->
      L2221.AgCost_Biochar_irr_mgmt_adj_bio

    L2221.CarbonCoef %>%
      #add_title("Biochar carbon coefficient") %>%
      add_units("kgC/GJ") %>%
      add_comments("Biochar coefficient is 0 to avoid emissions from cropland.") %>%
      add_legacy_name("L2221.CarbonCoef") %>%
      add_precursors("common/GCAM_region_names", "energy/A221.A_PrimaryFuelCCoef") ->
      L2221.CarbonCoef

    #8. biochar_ag_water_input_IRR_MGMT.xml
    L2221.AgCoef_IrrBphysWater_ag_mgmt%>%
      add_title("Biophysical water consumption IO coefficients by region / irrigated crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_IrrBphysWater_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrBphysWater_ag_mgmt") ->
      L2221.AgCoef_IrrBphysWater_ag_mgmt

    L2221.AgCoef_BphysWater_bio_mgmt %>%
      add_title("Biophysical water IO coefficients for biochar technologies
                by region / dedicated bioenergy crop / year / GLU / management level") %>%
      add_units("km3/EJ") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2221.AgCoef_BphysWater_bio_mgmt") %>%
      add_precursors("L2072.AgCoef_BphysWater_bio_mgmt") ->
      L2221.AgCoef_BphysWater_bio_mgmt

    L2221.AgCoef_IrrWaterWdraw_ag_mgmt %>%
      add_title("Irrigation water withdrawals IO coefficients by region / irrigated crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("Withdrawals coefs are calculated as consumption coefs divided by irrigation efficiency") %>%
      add_comments("Set a floor on profit and adjust the coefs to ensure the profit floor is met") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_IrrWaterWdraw_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrWaterWdraw_ag_mgmt") ->
      L2221.AgCoef_IrrWaterWdraw_ag_mgmt

    L2221.AgCoef_IrrWaterWdraw_bio_mgmt %>%
      add_title("Irrigation water withdrawals IO coefficients for biochar land techs
                by region / dedicated bioenergy crop / year / GLU / management level") %>%
      add_units("km3/EJ") %>%
      add_comments("Withdrawals coefs are calculated as consumption coefs divided by irrigation efficiency") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2221.AgCoef_IrrWaterWdraw_bio_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrWaterWdraw_bio_mgmt") ->
      L2221.AgCoef_IrrWaterWdraw_bio_mgmt

    L2221.AgCoef_IrrWaterCons_ag_mgmt %>%
      add_title("Irrigation water consumption IO coefficients by region / dedicated bioenergy crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("Multiply biophysical water coefs of bioenergy crops with the average % of blue water for primary crops by region / GLU") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_IrrWaterCons_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrWaterCons_ag_mgmt") ->
      L2221.AgCoef_IrrWaterCons_ag_mgmt

    L2221.AgCoef_IrrWaterCons_bio_mgmt %>%
      add_title("Irrigation water consumption IO coefficients for biochar land techs
                by region / dedicated bioenergy crop / year / GLU / management level") %>%
      add_units("km3/EJ") %>%
      add_comments("Multiply biophysical water coefs of bioenergy crops with the average % of blue water for primary crops by region / GLU") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2221.AgCoef_IrrWaterCons_bio_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrWaterCons_bio_mgmt") ->
      L2221.AgCoef_IrrWaterCons_bio_mgmt

    L2221.AgCoef_RfdBphysWater_ag_mgmt %>%
      add_title("Biophysical water consumption IO coefficients by region / rainfed crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_RfdBphysWater_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_RfdBphysWater_ag_mgmt") ->
      L2221.AgCoef_RfdBphysWater_ag_mgmt


    return_data(#1. biochar_land_input_5_IRR_MGMT.xml
      L2221.LN5_HistMgdAllocation_crop_biochar, L2221.LN5_HistMgdAllocation_bio_biochar,
      L2221.LN5_MgdAllocation_crop_biochar, L2221.LN5_MgdAllocation_bio_biochar,
      L2221.LN5_MgdCarbon_crop_biochar, L2221.LN5_MgdCarbon_bio_biochar,
      L2221.LN5_LeafGhostShare, L2221.LN5_LeafGhostShare_bio,
      #2. biochar_ag_For_Past_bio_base_IRR_MGMT.xml
      L2221.AgProduction_ag_irr_mgmt, L2221.AgHAtoCL_irr_mgmt,
      L2221.AgYield_biochar_ref, L2221.AgYield_bio_biochar_ref,
      #3. biochar_ag_cost_IRR_MGMT.xml
      L2221.AgCost_ag_irr_mgmt, L2221.AgCost_bio_irr_mgmt,
      #4. ag_prodchange_ref_IRR_MGMT.xml
      L2221.AgProdChange_ag_irr_ref, L2221.AgProdChange_bio_irr_ref,
      #5. biochar_ag_Fert_IRR_MGMT.xml
      L2221.AgCoef_Fert_ag_irr_mgmt, L2221.AgCoef_Fert_bio_irr_mgmt,
      L2221.AgCost_ag_irr_mgmt_adj, L2221.AgCost_bio_irr_mgmt_adj,
      #6. biochar_resbio_input_IRR_MGMT.xml
      L2221.AgResBio_ag_irr_mgmt, L2221.AgResBioCurve_ag_irr_mgmt,
      #7. biochar_ag_demand.xml
      L2221.AgCoef_Biochar_ag_irr_mgmt, L2221.AgCost_Biochar_irr_mgmt_adj,
      L2221.AgCoef_Biochar_bio_irr_mgmt, L2221.AgCost_Biochar_irr_mgmt_adj_bio,
      L2221.CarbonCoef,
      #8. biochar_ag_water_input_IRR_MGMT.xml
      L2221.AgCoef_IrrBphysWater_ag_mgmt, L2221.AgCoef_BphysWater_bio_mgmt,
      L2221.AgCoef_IrrWaterWdraw_ag_mgmt, L2221.AgCoef_IrrWaterWdraw_bio_mgmt,
      L2221.AgCoef_IrrWaterCons_ag_mgmt, L2221.AgCoef_IrrWaterCons_bio_mgmt,
      L2221.AgCoef_RfdBphysWater_ag_mgmt)
  } else {
    stop("Unknown command")
  }
}
