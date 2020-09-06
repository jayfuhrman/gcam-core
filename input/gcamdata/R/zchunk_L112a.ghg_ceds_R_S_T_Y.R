#' module_emissions_L112.ceds_ghg_en_R_S_T_Y
#'
#' Calculates emissions and emissions factors using EPA emissions factors and scales to EDGAR emissions. #**** is this accurate?
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.cedsghg_tg_R_en_S_F_Yh}, \code{L112.cedsghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L112.ghg_en_R_S_T_Y.R} (emissions level1).
#' @details Calculates emissions using EPA emissions factors and energy data. Then scales to EDGAR emissions and calculates emissions factors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select mutate_all
#' @importFrom tidyr gather spread
#' @author CWR Oct. 2018 / YO Mar. 2020

module_emissions_L112.ceds_ghg_en_R_S_T_Y <- function(command, ...) {
  if(driver.EMISSIONS_SOURCE == "EDGAR") {
    if(command == driver.DECLARE_INPUTS) {
      return(NULL)
    } else if(command == driver.DECLARE_OUTPUTS) {
      return(NULL)
    } else if(command == driver.MAKE) {
      return_data()
    } else {
      stop("Unknown command")
    }}
  else {
    if(command == driver.DECLARE_INPUTS) {
      return(c(FILE = "common/GCAM_region_names",
               FILE = "common/iso_GCAM_regID",
               FILE = "emissions/mappings/GCAM_sector_tech_CEDS",
               FILE = "emissions/mappings/GCAM_sector_tech_CEDS_revised",
               FILE = "energy/mappings/UCD_techs",
               FILE = "emissions/mappings/UCD_techs_emissions_revised",
               FILE = "energy/calibrated_techs",
               FILE = "energy/calibrated_techs_bld_det",
               FILE = "emissions/mappings/Trn_subsector",
               FILE = "emissions/mappings/Trn_subsector_revised",
               FILE = "emissions/CEDS/CEDS_sector_tech",
               FILE = "emissions/CEDS/CEDS_sector_tech_revised",
               FILE = "emissions/EPA_FCCC_IndProc_2005",
               FILE = "emissions/mappings/calibrated_outresources",
               FILE="emissions/CEDS/gains_iso_sector_emissions",
               FILE="emissions/CEDS/gains_iso_fuel_emissions",
               "L102.ceds_GFED_nonco2_tg_R_S_F",
               "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
               "L101.in_EJ_R_en_Si_F_Yh",
               "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
               "L101.ag_Prod_Mt_R_C_Y_GLU",
               "L111.ag_resbio_R_C",
               "L103.ghg_tgmt_USA_an_Sepa_F_2005",
               "L124.LC_bm2_R_Grass_Yh_GLU_adj",
               "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
               "L154.IEA_histfut_data_times_UCD_shares",

               FILE = "emissions/CEDS/ceds_sector_map",
               FILE = "emissions/CEDS/ceds_fuel_map",

               FILE = "emissions/EPA_country_map",
               # EPA scaling process 2020
               FILE = "emissions/EPA/EPA_2019_raw",
               FILE = "emissions/EPA_CH4N2O_map",
               FILE = "emissions/GCAM_EPA_CH4N2O_energy_map",
               FILE = "energy/rsrc_fol_prod_vintage"))
    } else if(command == driver.DECLARE_OUTPUTS) {
      return(c("L111.nonghg_tg_R_en_S_F_Yh",
               "L111.nonghg_tgej_R_en_S_F_Yh",
               "L112.ghg_tg_R_en_S_F_Yh",
               "L112.ghg_tgej_R_en_S_F_Yh",
               "L113.ghg_tg_R_an_C_Sys_Fd_Yh",
               "L115.nh3_tg_R_an_C_Sys_Fd_Yh",
               "L121.nonco2_tg_R_awb_C_Y_GLU",
               "L121.AWBshare_R_C_Y_GLU",
               "L122.ghg_tg_R_agr_C_Y_GLU",
               "L122.EmissShare_R_C_Y_GLU",
               "L124.nonco2_tg_R_grass_Y_GLU",
               "L124.nonco2_tg_R_forest_Y_GLU",
               "L124.deforest_coefs",
               "L131.nonco2_tg_R_prc_S_S_Yh",
               "L125.bcoc_tgbkm2_R_grass_2000",
               "L125.bcoc_tgbkm2_R_forest_2000",
               "L125.deforest_coefs_bcoc"))
    } else if(command == driver.MAKE) {

      all_data <- list(...)[[1]]

      L112.CEDS_GCAM <- get_data(all_data, "L102.ceds_GFED_nonco2_tg_R_S_F")

      # Optionally gets pre-built CEDS data
      if(is.null(L112.CEDS_GCAM)) {
        #Proprietary IEA energy data are not available, so used saved outputs
        L112.CEDS_GCAM <- prebuilt_data("L102.ceds_GFED_nonco2_tg_R_S_F")
      } else {
      }
      #In case of tanker loading emissions which are classified as process emissions, transfer them to refined liquids. Same for processs industrial energy emissions
      L112.CEDS_GCAM %>%
        mutate(CEDS_agg_fuel=if_else(CEDS_agg_sector=="trn_intl_ship",if_else(CEDS_agg_fuel=="process","refined liquids",CEDS_agg_fuel),CEDS_agg_fuel)) %>%
        mutate(CEDS_agg_fuel=if_else(CEDS_agg_sector=="industry_energy",if_else(CEDS_agg_fuel=="process","refined liquids",CEDS_agg_fuel),CEDS_agg_fuel)) %>%
        group_by(GCAM_region_ID, Non.CO2, CEDS_agg_sector, CEDS_agg_fuel, year) %>%
        summarise(emissions = sum(emissions)) %>%
        ungroup() %>%
        na.omit()->L112.CEDS_GCAM

      # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")

      #Get GAINS data

      GAINS_sector <- get_data(all_data,"emissions/CEDS/gains_iso_sector_emissions")
      GAINS_fuel <- get_data(all_data,"emissions/CEDS/gains_iso_fuel_emissions")
      IEA_Ctry_data <- get_data(all_data,"L154.IEA_histfut_data_times_UCD_shares")
      if (energy.TRAN_UCD_MODE=="rev.mode"){
        IEA_Ctry_data %>% rename(mode=rev.mode,size.class=rev_size.class)->IEA_Ctry_data
      }
      #kbn 2019/11/11 Add code below so that we can use revised sub-sectors from transportation model
      if (energy.TRAN_UCD_MODE=="rev.mode"){
        GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_CEDS_revised") %>% distinct()
      }else{
        GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_CEDS")}

      #kbn 2019/11/11 Add code below so that we can use revised sub-sectors from transportation model
      if (energy.TRAN_UCD_MODE=="rev.mode"){
        Trn_subsector <- get_data(all_data, "emissions/mappings/Trn_subsector_revised")}else{

          Trn_subsector <- get_data(all_data, "emissions/mappings/Trn_subsector")

        }


      if (energy.TRAN_UCD_MODE=="rev.mode"){
        UCD_techs <- get_data(all_data, "emissions/mappings/UCD_techs_emissions_revised")
      }else{
        UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")}
      calibrated_techs <- get_data(all_data, "energy/calibrated_techs")

      calibrated_techs_bld_det <- get_data(all_data, "energy/calibrated_techs_bld_det")
      L101.in_EJ_R_en_Si_F_Yh <- get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh") %>%
        gather_years(value_col = "energy")
      CEDS_sector_map <- get_data(all_data, "emissions/CEDS/ceds_sector_map")
      CEDS_fuel_map <- get_data(all_data, "emissions/CEDS/ceds_fuel_map")

      #kbn 2019/11/11 Add in transport flexibility below
      if (energy.TRAN_UCD_MODE=="rev.mode"){
        CEDS_sector_tech <- get_data(all_data, "emissions/CEDS/CEDS_sector_tech_revised") %>% distinct()}else{

          CEDS_sector_tech <- get_data(all_data, "emissions/CEDS/CEDS_sector_tech")
        }

      calibrated_outresources <- get_data(all_data, "emissions/mappings/calibrated_outresources")

      L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_Grass_Yh_GLU_adj")
      L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")
      L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
      L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU")
      L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU")
      #L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "L142.ag_Fert_IO_R_C_Y_GLU")

      # Temporary to run for testing and matching Animal emissions
      L103.ghg_tgmt_USA_an_Sepa_F_2005 <- get_data(all_data, "L103.ghg_tgmt_USA_an_Sepa_F_2005")

      EPA_Ind <- get_data(all_data, "emissions/EPA_FCCC_IndProc_2005")
      EPA_country_map <- get_data(all_data,"emissions/EPA_country_map")
      # EPA Raw CH4 and N2O data files
      # YO 2020 EPA scaling
      EPA_master <- get_data(all_data, "emissions/EPA/EPA_2019_raw")
      EPA_CH4N2O_map <- get_data(all_data, "emissions/EPA_CH4N2O_map")
      GCAM_EPA_CH4N2O_map <- get_data(all_data, "emissions/GCAM_EPA_CH4N2O_energy_map")
      rsrc_fol_prod_vintage <- get_data(all_data, "energy/rsrc_fol_prod_vintage")

       #kbn adding notin for later calculations
      `%notin%` <- Negate(`%in%`)

      #kbn calculate emissions for different modes here

      #Filter out road emissions
      L112.CEDS_Road_emissions <- L112.CEDS_GCAM %>%  filter(CEDS_agg_sector == "trn_road")
      L112.CEDS_GCAM<- L112.CEDS_GCAM %>%  filter(CEDS_agg_sector != "trn_road")

      IEA_Ctry_data %>%
        #Use only historical years
        filter(year <= MODEL_FINAL_BASE_YEAR) %>%
        filter(UCD_category=="trn_road and rail") %>%
        filter(mode %notin% c("Rail","HSR")) %>%
        select(-UCD_fuel,-fuel,-size.class) %>%
        rename(fuel=UCD_technology) %>%
        filter(fuel %notin% c("BEV","FCEV","LA-BEV","NG")) %>%
        mutate(fuel =if_else(fuel=="Hybrid Liquids","Liquids",fuel))->Clean_IEA_ctry_data

      Clean_IEA_ctry_data %>%
        group_by(iso,UCD_sector,year) %>%
        mutate(value=sum(value)) %>%
        ungroup() %>%
        select(iso,UCD_sector,year,value,GCAM_region_ID) %>%
        distinct() %>%
        repeat_add_columns(tibble(Non.co2 = unique(GAINS_sector$Non.co2))) %>%
        left_join(GAINS_sector %>% gather("UCD_sector","em_fact","Freight":"Passenger"),by=c("iso","year","Non.co2","UCD_sector")) %>%
        na.omit() %>%
        filter(UCD_sector != "Motorcycle") %>%
        group_by(Non.co2,GCAM_region_ID,year,UCD_sector) %>%
        mutate(sector_weight=sum(em_fact*value)) %>%
        ungroup() %>%
        select(Non.co2,GCAM_region_ID,year,UCD_sector,sector_weight) %>%
        #Use these sector weights to split CEDS emissions into passenger and freight
        distinct() ->GAINS_sector_weights


      Clean_IEA_ctry_data %>%
        group_by(iso,mode,year,UCD_sector) %>%
        mutate(value=sum(value)) %>%
        ungroup() %>%
        select(iso,mode,year,value,GCAM_region_ID,UCD_sector) %>%
        distinct() %>%
        repeat_add_columns(tibble(Non.co2 = unique(GAINS_fuel$Non.co2))) %>%
        left_join(GAINS_fuel , by=c("Non.co2","iso","year")) %>%
        na.omit() %>%
        #Now calculate mode wights here
        group_by(Non.co2,GCAM_region_ID,year,mode,UCD_sector) %>%
        #Revisit this. Make it compatible with old size classes as well.
        mutate(mode_weight=if_else(mode=="Bus",sum(dieseloil*value),
                                     if_else(mode=="LDV_2W_3W",sum(((lightoil+dieseloil)/2)*value),
                                             if_else(mode=="Truck",sum(dieseloil*value),sum(lightoil*value))))) %>%
        ungroup() %>%
        select(Non.co2,GCAM_region_ID,year,mode,mode_weight,UCD_sector) %>%
        distinct()->GAINS_mode_weights


      L112.CEDS_Road_emissions %>%
        rename(Non.co2=Non.CO2) %>%
        group_by(GCAM_region_ID,year,Non.co2) %>%
        mutate(emissions=sum(emissions)) %>%
        ungroup() %>%
        select(GCAM_region_ID,year,Non.co2,emissions) %>%
        distinct() %>%
        left_join(GAINS_sector_weights, by=c("GCAM_region_ID","year","Non.co2")) %>%
        na.omit() %>%
        #First split emissions into Passenger and Freight %>%
        group_by(GCAM_region_ID,year,Non.co2) %>%
        mutate(sum_sector_weight=sum(sector_weight)) %>%
        ungroup() %>%
        mutate(emissions=(emissions*sector_weight)/sum_sector_weight) %>%
        left_join(GAINS_mode_weights, by=c("GCAM_region_ID","year","Non.co2","UCD_sector")) %>%
        na.omit() %>%
        group_by(GCAM_region_ID,year,Non.co2,UCD_sector) %>%
        mutate(sum_mode_weight=sum(mode_weight)) %>%
        ungroup() %>%
        mutate(emissions=(emissions*mode_weight)/sum_mode_weight) %>%
        ungroup() %>%
        mutate(CEDS_agg_fuel=paste0("refined liquids")) %>%
        rename(CEDS_agg_sector=mode,Non.CO2=Non.co2) %>%
        select(GCAM_region_ID,year,Non.CO2,CEDS_agg_sector,CEDS_agg_fuel,emissions) %>% distinct()->L112.CEDS_GCAM_Road_Emissions_GAINS

        L112.CEDS_GCAM %>%  bind_rows(L112.CEDS_GCAM_Road_Emissions_GAINS)->L112.CEDS_GCAM

        # ===========================
        # Part 0: clean EPA nonCO2 data
        # ===========================

        # remove Cameroon industrial process N2O emissions
        # because it is a known data error after communicating with EPA
        EPA_master_Cameroon_IP <- EPA_master %>%
          filter(country == "Cameroon" & source == "OtherIPPU" & gas == "N2O") %>%
          mutate(value = 0)

        EPA_master <- rbind(EPA_master_Cameroon_IP,
                            EPA_master %>% filter(!(country == "Cameroon" & source == "OtherIPPU" & gas == "N2O")))

        # ===========================
        # Part 1:Combustion Energy Emissions
        # ===========================

        # Filter down to combustion emissions plus fugitive process emissions from combustion resource production (out_resources)
        L112.CEDS_GCAM %>%
          filter(CEDS_agg_fuel != "process" | CEDS_agg_sector %in% c("oil_gas", "coal")) ->
          L112.CEDS_GCAM_emissions

        # PREPARE ENERGY FOR MATCHING TO EMISSIONS
        # ----------------------------------------

        # Splits energy balances out for industry sector and maps to final GCAM sectors
        L101.in_EJ_R_en_Si_F_Yh %>%
          #Need to add three missing out_resources rows that are not included in base calibrated techs file
          left_join(calibrated_techs %>% bind_rows(calibrated_outresources) %>% select(-secondary.output), by = c("sector", "fuel", "technology")) %>%
          rename(stub.technology = technology) %>%
          select(GCAM_region_ID, year, energy, supplysector, subsector, stub.technology) %>%
          na.omit() ->
          L112.in_EJ_R_en_S_F_Yh_calibtech

        # Splits energy balances out for building sector and maps to final GCAM sectors
        L101.in_EJ_R_en_Si_F_Yh %>%
          left_join(calibrated_techs_bld_det %>% select(sector, fuel, service, supplysector, subsector, technology) %>% rename(stub.technology = technology), by = c("sector" = "service", "fuel")) %>%
          na.omit() %>%
          select(GCAM_region_ID, year, energy, supplysector, subsector, stub.technology) ->
          L112.in_EJ_R_en_S_F_Yh_calib_bld

        # Splits energy balances out for transport sector and maps to final GCAM sectors
        L101.in_EJ_R_en_Si_F_Yh %>%
          left_join(Trn_subsector, by = c("fuel")) %>%
          left_join_keep_first_only(UCD_techs %>% select(UCD_sector, mode, size.class, fuel, supplysector, tranSubsector, tranTechnology) %>% rename(technology = fuel, subsector = tranSubsector, stub.technology = tranTechnology),
                                    by = c("sector" = "UCD_sector", "mode", "size.class", "technology")) %>%
          na.omit() %>%
          select(GCAM_region_ID, year, energy, supplysector, subsector, stub.technology) ->
          L112.in_EJ_R_en_S_F_Yh_calib_trn


        # Rebind separate sectors into master list
        L112.in_EJ_R_en_S_F_Yh_calibtech %>%
          bind_rows(L112.in_EJ_R_en_S_F_Yh_calib_bld, L112.in_EJ_R_en_S_F_Yh_calib_trn) ->
          L112.in_EJ_R_en_S_F_Yh_calib_all

        # Compute energy by sector, subsector, and technology
        L112.in_EJ_R_en_S_F_Yh_calib_all %>%
          group_by(GCAM_region_ID, year, supplysector, subsector, stub.technology) %>%
          summarise(energy = sum(energy)) %>%
          ungroup() ->
          L112.in_EJ_R_en_S_F_Yh_calib_all


        # MATCH ENERGY AND EMISSIONS TO AGGREGATE EMISSIONS TO SPLIT OUT EMISSIONS BY GCAM SECTORS
        # ========================================================================================

        # Append CEDS sector/fuel combinations to GCAM energy
        L112.in_EJ_R_en_S_F_Yh_calib_all %>%
          filter(stub.technology %notin% c(emissions.ZERO_EM_TECH)) %>%
          #We will drop all electricity sectors here
          left_join_error_no_match(CEDS_sector_tech, by = c("supplysector", "subsector", "stub.technology")) ->L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy

        # Aggregate GCAM energy to CEDS sector/fuel combinations and compute the total energy by CEDS sector
        L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy %>%
          group_by(GCAM_region_ID, year, CEDS_agg_sector, CEDS_agg_fuel) %>%
          summarise(totalenergy = sum(energy)) %>%
          ungroup() ->
          L112.in_EJ_R_en_S_F_Yh_calib_CEDS_NAs


        # Replace all base energy values in sectors with a total energy of 0 with 1,
        # allowing shares to be calculated for those sectors in the absence of energy
        L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy %>%
          left_join_error_no_match(L112.in_EJ_R_en_S_F_Yh_calib_CEDS_NAs,
                                   by = c("GCAM_region_ID", "year", "CEDS_agg_sector", "CEDS_agg_fuel")) %>%
          mutate(energy = if_else(totalenergy == 0, 1, energy)) %>%
          select(-totalenergy) ->
          L112.in_EJ_R_en_S_F_Yh_calib_newbase

        # Calculate new total energy including dummy values for zero total energy sectors
        L112.in_EJ_R_en_S_F_Yh_calib_newbase %>%
          group_by(GCAM_region_ID, year, CEDS_agg_sector, CEDS_agg_fuel) %>%
          summarise(totalenergy = sum(energy)) %>%
          ungroup() ->
          L112.in_EJ_R_en_S_F_Yh_calib_CEDStotals

        # Compute the share of energy in GCAM sector by CEDS sector
        L112.in_EJ_R_en_S_F_Yh_calib_newbase %>%
          left_join_error_no_match(L112.in_EJ_R_en_S_F_Yh_calib_CEDStotals,
                                   by = c("GCAM_region_ID", "year", "CEDS_agg_sector", "CEDS_agg_fuel")) %>%
          mutate(enshare = energy/totalenergy) ->
          L112.in_EJ_R_en_S_F_Yh_calib_enshare

        # Attach CEDS emissions to those sector fuel combos
        #kbn 2019/11/11 changed nomenclature below
        L112.in_EJ_R_en_S_F_Yh_calib_enshare %>%
          left_join(L112.CEDS_GCAM_emissions,
                    by = c("GCAM_region_ID", "year", "CEDS_agg_sector", "CEDS_agg_fuel")) ->
          L112.CEDSGCAM_emissions

        L112.CEDSGCAM_emissions %>%
          mutate(GCAMemissions = emissions * enshare) ->
          L112.CEDSGCAM_computedemissions

        L112.CEDSGCAM_computedemissions -> L112.CEDSGCAM_computedemissions_complete

        L112.CEDSGCAM_computedemissions_complete %>%
          select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, GCAMemissions) %>%
          rename(emissions = GCAMemissions) ->
          L112.nonco2_tg_R_en_S_F_Yh

        # REMAINING ISSUES:
        # TRN_OTHER SECTOR HAS NO MATCH. THIS IS CURRENTLY DROPPED--THIS IS LIKELY CORRECTLY DROPPED BUT COULD ASSIGNED SOMEWHERE.
        # TOTAL EMISSIONS BEFORE AND AFTER OF NON-PROCESS ENERGY SECTORS IS .4 LARGER IN INITIAL DATASET

        # GENERATE COMBUSTION EMISSIONS FACTORS BY MULTIPLYING EMISSIONS BY DRIVER


        # ========================================================================

        #---------------------------------------------------------------
        # START NEW PROCESS
        # YO 2020
        # using EPA 2019 emissions to calculate emission factors for CH4 and NO2 for resource production

        # compute EPA total by EPA_sector and all historical years
        EPA_master %>%
          filter(sector == "Energy" & gas %in% c("CH4", "N2O")) %>%
          left_join_error_no_match(EPA_CH4N2O_map, by = c("sector", "source", "subsource")) %>%
          left_join_error_no_match(EPA_country_map, by = c("country" = "EPA_country")) %>%
          group_by(GCAM_region_ID, EPA_sector, year, gas) %>%
          summarise(EPA_emissions = sum(value)) %>%
          ungroup() ->
          L112.EPA_CH4N2O_energy

        # out_resources production is by vintages, so need additional processes to get actual production
        # by vintage, and then backward calculate the actual EFs for each vintage
        # TODO: currently directly read in output from ModelInterface as prebuild data

        # 1) obtain production by vintages
        # since the first EPA nonCO2 calibration year might be greater than the first GCAM historical year
        # we assign all production prior to first EPA nonCO2 calibration year as the first EPA nonCO2 calibration year
        # a constant emissions.EPA_BAU_HIST_YEAR is handling this
        rsrc_fol_prod_vintage %>%
          filter(year <= MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)]) %>%
          gather(year_operate, value, -region, -resource, -subresource, -year) %>%
          left_join_error_no_match(GCAM_region_names, by = "region") %>%
          mutate(fuel = subresource) %>%
          mutate(fuel =ifelse(fuel == "unconventional oil", "crude oil", fuel)) %>%
          mutate(year = as.integer(year)) %>%
          mutate(year = ifelse(year < emissions.EPA_BAU_HIST_YEAR[1], emissions.EPA_BAU_HIST_YEAR[1], year)) %>%
          group_by(GCAM_region_ID, fuel, year, year_operate) %>%
          summarise(value = sum(value)) %>%
          ungroup() ->
          L111.Prod_EJ_R_F_Yh_vintage

        # 2) scaling the emissions used for calculating resource EFs
        L112.nonco2_tg_R_en_S_F_Yh %>%
          filter(supplysector == "out_resources" & Non.CO2 %in% c("CH4", "N2O")) %>%
          left_join_error_no_match(GCAM_EPA_CH4N2O_map, by = c("supplysector", "subsector", "stub.technology")) %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          summarise(tot_emissions = sum(emissions)) %>%
          ungroup() %>%
          # convert into CO2eq by GWP (AP4): CH4 is 25; N2O is 298
          mutate(tot_emissions = ifelse(Non.CO2 == "CH4", tot_emissions * 25, tot_emissions * 298)) %>%
          # using left_join becuase EPA emissions only have values for every five years
          left_join(L112.EPA_CH4N2O_energy, by = c("GCAM_region_ID", "EPA_sector", "Non.CO2" = "gas", "year")) %>%
          mutate(EPA_emissions = ifelse(is.na(EPA_emissions), tot_emissions, EPA_emissions)) %>%
          mutate(emscaler = EPA_emissions / tot_emissions) %>%
          mutate(emscaler = ifelse(is.na(emscaler) | is.infinite(emscaler), 1, emscaler)) %>%
          select(-EPA_emissions, -tot_emissions) ->
          L112.nonco2_tg_R_en_S_F_Yh_EPAscaler

        # do the actual scaling
        L112.nonco2_tg_R_en_S_F_Yh %>%
          filter(supplysector == "out_resources" & Non.CO2 %in% c("CH4", "N2O")) %>%
          left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                   by = c("supplysector", "subsector", "stub.technology")) %>%
          left_join_error_no_match(L112.nonco2_tg_R_en_S_F_Yh_EPAscaler,
                                   by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
          mutate(emissions = emscaler * emissions) %>%
          select(-EPA_sector, -emscaler) %>%
          filter(year %in% MODEL_BASE_YEARS) ->
          L112.nonco2_tg_R_en_S_F_Yh_resource

        # 3) backward calculate EFs by vintage

        for (i in seq_along(emissions.EPA_BAU_HIST_YEAR)){
          # 1990 EF, just directly calculate since EPA emissions start from 1990
          if(i == 1){
            L111.Prod_EJ_R_F_Yh_vintage %>%
              filter(year_operate == emissions.EPA_BAU_HIST_YEAR[i]) %>%
              group_by(GCAM_region_ID, fuel) %>%
              summarise(value = sum(value)) %>%
              ungroup() ->
              L111.Prod_EJ_R_F_Yh_vintage_i

            L112.nonco2_tg_R_en_S_F_Yh_resource %>%
              filter(year == emissions.EPA_BAU_HIST_YEAR[i]) ->
              L112.nonco2_tg_R_en_S_F_Yh_resource_i

            L112.nonco2_tg_R_en_S_F_Yh_resource_i %>%
              left_join_error_no_match(L111.Prod_EJ_R_F_Yh_vintage_i,
                                       by = c("GCAM_region_ID", "subsector" = "fuel")) %>%
              mutate(emfact = emissions / value) %>%
              select(-emissions, -value) ->
              L112.ghg_tgej_R_en_S_F_Yh_i_NA

            L112.ghg_tgej_R_en_S_F_Yh_i_NA %>%
              filter(!(is.na(emfact) | is.infinite(emfact))) %>%
              # mutate(emfact = ifelse(is.na(emfact) | is.infinite(emfact), 0, emfact)) %>%
              group_by(Non.CO2, stub.technology) %>%
              summarise(median = median(emfact)) %>%
              ungroup() ->
              L112.ghg_tgej_R_en_S_F_Yh_i_median

            L112.ghg_tgej_R_en_S_F_Yh_i_NA %>%
              left_join(L112.ghg_tgej_R_en_S_F_Yh_i_median,
                        by = c("Non.CO2", "stub.technology")) %>%
              mutate(emfact = ifelse(is.na(emfact) | is.infinite(emfact), median, emfact)) %>%
              select(-median) ->
              L112.ghg_tgej_R_en_S_F_Yh_i

            L112.ghg_tgej_R_en_S_F_Yh_adj <- L112.ghg_tgej_R_en_S_F_Yh_i
          } else {
            # production: year i vintages
            L111.Prod_EJ_R_F_Yh_vintage %>%
              filter(year_operate == emissions.EPA_BAU_HIST_YEAR[i]) %>%
              filter(year == emissions.EPA_BAU_HIST_YEAR[i]) %>%
              group_by(GCAM_region_ID, fuel) %>%
              summarise(ej_i = sum(value)) %>%
              ungroup() ->
              L111.Prod_EJ_R_F_Yh_vintage_i

            # production: vintages BEFORE year i builts
            L111.Prod_EJ_R_F_Yh_vintage %>%
              filter(year_operate == emissions.EPA_BAU_HIST_YEAR[i]) %>%
              filter(year < emissions.EPA_BAU_HIST_YEAR[i]) %>%
              group_by(GCAM_region_ID, year, fuel) %>%
              summarise(ej_vintage = sum(value)) %>%
              ungroup() ->
              L111.Prod_EJ_R_F_Yh_vintage_i_vintage

            # emissions in year i BUT produced by vintages prior to year i
            L112.ghg_tgej_R_en_S_F_Yh_adj %>%
              # generate NA becuase the previous vintages have all retired in the current year i
              # replace NA with 0, so no production
              left_join(L111.Prod_EJ_R_F_Yh_vintage_i_vintage,
                        by = c("GCAM_region_ID", "subsector" = "fuel", "year")) %>%
              replace_na(list(ej_vintage = 0)) %>%
              mutate(tg_vintage = emfact * ej_vintage) %>%
              group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology) %>%
              summarise(tg_vintage = sum(tg_vintage)) %>%
              ungroup() ->
              L112.nonco2_tg_R_en_S_F_Yh_resource_vintage

            # Emission factor in year i and also produced by vintage in year i
            # the resulted data frame will have NA, Inf, or unrealistic values
            # then subsequent pipes will replace them with global medians
            L112.nonco2_tg_R_en_S_F_Yh_resource %>%
              filter(year == emissions.EPA_BAU_HIST_YEAR[i]) %>%
              rename(tg_i_total = emissions) %>%
              left_join_error_no_match(L112.nonco2_tg_R_en_S_F_Yh_resource_vintage,
                                       by = c("GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology")) %>%
              mutate(tg_i = tg_i_total - tg_vintage) %>%
              # some tg_i will be negative, adjust them as 0
              mutate(tg_i = ifelse(tg_i < 0, 0, tg_i)) %>%
              # in some cases there are emissions from vintage i, but no production from vintage
              # replace with 0; thus this will result in NA/Inf emission factors, and will be updated with global median later
              left_join(L111.Prod_EJ_R_F_Yh_vintage_i,
                        by = c("GCAM_region_ID", "subsector" = "fuel")) %>%
              replace_na(list(ej_i= 0)) %>%
              mutate(emfact = tg_i / ej_i) %>%
              select(-tg_i_total, -tg_vintage, -tg_i, -ej_i) ->
              L112.ghg_tgej_R_en_S_F_Yh_i_NA

            # calculate global medians to replace extreme values
            L112.ghg_tgej_R_en_S_F_Yh_i_NA %>%
              filter(!(is.na(emfact) | is.infinite(emfact))) %>%
              group_by(Non.CO2, stub.technology) %>%
              summarise(median = median(emfact)) %>%
              ungroup() ->
              L112.ghg_tgej_R_en_S_F_Yh_i_median

            # replacing extremes with median
            # here need to have special focus on the last calibration year

            if(emissions.EPA_BAU_HIST_YEAR[i] != MODEL_FINAL_BASE_YEAR){
              L112.ghg_tgej_R_en_S_F_Yh_i_NA %>%
                left_join(L112.ghg_tgej_R_en_S_F_Yh_i_median,
                          by = c("Non.CO2", "stub.technology")) %>%
                mutate(emfact = ifelse(is.na(emfact) | is.infinite(emfact) | emfact > 1000, median, emfact)) %>%
                select(-median) ->
                L112.ghg_tgej_R_en_S_F_Yh_i
            } else {
              # if i is the final calibration year (currently is 2015)
              # based on manual check, need to special adjustment to make sure EFs are reasonable
              # Besides, since this process is backward calculated, the more steps it takes, the higher chance that unrealistic EFs would show up
              L112.ghg_tgej_R_en_S_F_Yh_i_NA %>%
                left_join(L112.ghg_tgej_R_en_S_F_Yh_i_median,
                          by = c("Non.CO2", "stub.technology")) %>%
                # China's EF in 2015 is too high (higher than all its historical values), so instead using global median
                mutate(emfact = ifelse(is.na(emfact) | is.infinite(emfact) | emfact > 5 | emfact == 0 | GCAM_region_ID == 11,
                                       median, emfact)) %>%
                select(-median) ->
                L112.ghg_tgej_R_en_S_F_Yh_i}

            L112.ghg_tgej_R_en_S_F_Yh_adj <- rbind(L112.ghg_tgej_R_en_S_F_Yh_adj,
                                                   L112.ghg_tgej_R_en_S_F_Yh_i)
          }
        }

        # if the first EPA BAU year is greater than the first GCAM historical year
        # copy and paste the first EPA BAU year EFs for those missing GCAM BASE YEARS
        # This should be consistent with how to handle resource vintage data in
        # Part: "1) obtain production by vintages"
        GCAM_hist_missing <- setdiff(MODEL_BASE_YEARS, emissions.EPA_BAU_HIST_YEAR)

        if(!is.na(GCAM_hist_missing)){
          L112.ghg_tgej_R_en_S_F_Yh_adj_HIST<- L112.ghg_tgej_R_en_S_F_Yh_adj %>%
            filter(year == emissions.EPA_BAU_HIST_YEAR[1]) %>%
            select(-year) %>%
            repeat_add_columns(tibble(year = GCAM_hist_missing))

          L112.ghg_tgej_R_en_S_F_Yh_adj <- rbind(L112.ghg_tgej_R_en_S_F_Yh_adj,
                                                 L112.ghg_tgej_R_en_S_F_Yh_adj_HIST)
        }

        L112.ghg_tgej_R_en_S_F_Yh_adj <- L112.ghg_tgej_R_en_S_F_Yh_adj %>%
          rename(value_adj = emfact)

        #END OF NEW PROCESS
        #--------------------------------------------------------------------------------------
        # Now join emissions and energy data together to calculate emissions factors
        L112.nonco2_tg_R_en_S_F_Yh %>%
          left_join_error_no_match(L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy,
                                   by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology", "year")) %>%
          mutate(emfact = emissions / energy) %>%
          select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, emfact) ->
          #Replaces NAs with zeroes (places where zero emissions and energy lead to 0/0 = NaN)
          L112.nonco2_tgej_R_en_S_F_Yh_withNAs

        # Generates global median emissions factors
        L112.nonco2_tgej_R_en_S_F_Yh_withNAs %>%
          replace_na(list(emfact = 0)) %>%
          group_by(year, Non.CO2, supplysector, subsector, stub.technology) %>%
          summarise(emfact = median(emfact)) %>%
          ungroup() %>%
          rename(globalemfact = emfact) ->
          L112.nonco2_tgej_R_en_S_F_Yh_globalmedian

        # Replaces all emissions factors above a given value (currently 1000) or that are NAs with the global median emissions factor for that year, non.CO2, and technology
        L112.nonco2_tgej_R_en_S_F_Yh_withNAs %>%
          left_join_error_no_match(L112.nonco2_tgej_R_en_S_F_Yh_globalmedian, by = c("year", "Non.CO2", "supplysector", "subsector", "stub.technology")) %>%
          mutate(emfact = if_else(emfact > 1000 | is.na(emfact), globalemfact, emfact)) %>%
          select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, emfact) %>%
          # This line is only for testing and needs to be replaced with the solution for below
          mutate(emfact = if_else(is.infinite(emfact), 1, emfact)) ->
          L112.nonco2_tgej_R_en_S_F_Yh
        # This largely works but need to figure out what's going on with trucks 1-6t in region 19.
        # Also need to set up rule to determine cutoff point for emissions factor replacement


        # =======================
        # Process Emissions
        # =======================

        # Calculate process emissions drivers
        # -----------------------------------


        # Subset CEDS process emissions and match to GCAM drivers

        L112.CEDS_GCAM %>%
          filter(CEDS_agg_sector %in% c("industry_processes", "chemicals", "landfills", "wastewater", "aerosols",
                                        "metals", "foams", "solvents", "semiconductors")) ->
          L112.CEDS_GCAM_Proc


        # Fourth: Map in all data and compute emissions (EDGAR_emissions * tech_share).

        # THIS IS THE NEW CODE TO TAKE THE SEPARATE NITRIC AND ADIPIC SECTORS AND BREAK THEM OUT AUTOMATICALLY.

        GCAM_sector_tech %>%
          select(supplysector, subsector, stub.technology, EDGAR_agg_sector, EPA_agg_sector, EPA_agg_fuel_ghg) %>%
          filter(EDGAR_agg_sector %in% c("industry_processes" , "chemicals", "landfills", "wastewater",  # Filter for the agg sectors in EDGAR for all NonCO2s.
                                         "solvents")) %>%
          repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_Proc$Non.CO2))) %>%
          group_by(supplysector, subsector, stub.technology, Non.CO2) %>%
          # left_join(L131.nonco2_pct_R_prc_S_S_2005,  # Then combine with the EPA sector information
          #           by = c("supplysector", "subsector", "stub.technology",
          #                  "Non.CO2", "EPA_agg_sector", "EDGAR_agg_sector", "EPA_agg_fuel_ghg")) %>%
          repeat_add_columns(tibble(year = emissions.CEDS_YEARS)) %>%
          repeat_add_columns(tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID)) %>%
          group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2, year) %>%
          left_join_error_no_match(L112.CEDS_GCAM_Proc, by = c("GCAM_region_ID", "EDGAR_agg_sector" = "CEDS_agg_sector", "Non.CO2", "year")) %>%
          na.omit() %>%  # delete rows with NA's
          # Have to figure out way to carry through and share out nitric and adipic acid here -- will now be coming from CEDS sector
          mutate(input.emissions = emissions) %>%  # Calculate emissions
          # select(-sector_emissions, -tech_emissions.x) %>%
          group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
          summarise(value = sum(input.emissions)) %>% # Calculate total emissions
          ungroup() %>%
          replace_na(list(value = 0)) ->
          L131.nonco2_tg_R_prc_S_S_Yh

        # YO
        # Mar 2020 - scaling to EPA 2019 total for CH4 and N2O industry processes
        # Apr 2020 - scaling to EPA 2019 total for CH4 and N2O waste (urban processes)
        # ------------------------------------------------------------------------------------------------------------
        # START NEW PROCESS

        # compute EPA total by EPA_sector and all historical years
        EPA_master %>%
          filter(sector %in% c("Industrial Processes", "Waste") & gas %in% c("CH4", "N2O")) %>%
          left_join_error_no_match(EPA_CH4N2O_map, by = c("sector", "source", "subsource")) %>%
          left_join(EPA_country_map, by = c("country" = "EPA_country")) %>%
          group_by(GCAM_region_ID, EPA_sector, year, gas) %>%
          summarise(EPA_emissions = sum(value)) %>%
          ungroup() ->
          L131.EPA_CH4N2O_proc

        # calculate scalers for process-related emissions by EPA_sector by year and region
        L131.nonco2_tg_R_prc_S_S_Yh %>%
          filter(supplysector %in% c("industrial processes", "urban processes") & Non.CO2 %in% c("CH4", "N2O")) ->
          L131.nonco2_tg_R_prc_S_S_Yh_change

        L131.nonco2_tg_R_prc_S_S_Yh_change %>%
          left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                   by = c("supplysector", "subsector", "stub.technology")) %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          summarise(tot_emissions = sum(value)) %>%
          ungroup() %>%
          # convert into CO2eq by GWP (AP4): CH4 is 25; N2O is 298
          mutate(tot_emissions = ifelse(Non.CO2 == "CH4", tot_emissions * 25, tot_emissions *  298)) %>%
          left_join(L131.EPA_CH4N2O_proc, by = c("GCAM_region_ID", "EPA_sector", "Non.CO2" = "gas", "year")) %>%
          # for non-EPA years, just keep the original emissions, thus scaler will be 1
          mutate(EPA_emissions = ifelse(is.na(EPA_emissions), tot_emissions, EPA_emissions)) %>%
          mutate(emscaler = EPA_emissions / tot_emissions) %>%
          mutate(emscaler = ifelse(is.na(emscaler) | is.infinite(emscaler), 1, emscaler)) %>%
          select(-EPA_emissions, -tot_emissions) ->
          L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler

        # leave emscaler = 1 part as it is
        L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler %>%
          filter(emscaler == 1) -> L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler_unchanged

        # adjust scalers for outliers
        L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler %>%
          filter(emscaler != 1) %>%
          group_by(EPA_sector, Non.CO2, year) %>%
          mutate(lower = quantile(emscaler, 0.25) - 1.5 * IQR(emscaler),
                 upper = quantile(emscaler, 0.75) + 1.5 * IQR(emscaler)) %>%
          ungroup() %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          mutate(emscaler = max(emscaler, lower)) %>%
          mutate(emscaler = min(emscaler, upper)) %>%
          select(-lower, -upper) %>%
          ungroup() ->
          L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler_changed

        # combine
        L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler <- rbind(L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler_unchanged,
                                                       L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler_changed)




        # do the actual scaling for process emissions
        L131.nonco2_tg_R_prc_S_S_Yh_change %>%
          left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                   by = c("supplysector", "subsector", "stub.technology")) %>%
          left_join_error_no_match(L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler,
                                   by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
          mutate(value = value * emscaler) %>%
          select(-EPA_sector, -emscaler) ->
          L131.nonco2_tg_R_prc_S_S_Yh_update

        L131.nonco2_tg_R_prc_S_S_Yh %>%
          filter(!(supplysector %in% c("industrial processes", "urban processes") & Non.CO2 %in% c("CH4", "N2O"))) %>%
          bind_rows(L131.nonco2_tg_R_prc_S_S_Yh_update) ->
          L131.nonco2_tg_R_prc_S_S_Yh_adj

        # update the origional dataset
        L131.nonco2_tg_R_prc_S_S_Yh <- L131.nonco2_tg_R_prc_S_S_Yh_adj

        # END NEW PROCESS
        # ------------------------------------------------------------------------------------------------------------

        # Final sector outputs
        # "adipic acid" "HCFC_22_Prod" "nitric acid" "other industrial processes" "solvents"
        # "landfills" "wastewater treatment"

        # ===================================================
        # Animal Emissions
        # ===================================================
        # Computing unscaled emissions by country and technology
        # using animal production from L107.an_Prod_Mt_R_C_SYS_Fd_Y
        # and EPA emissions factos.

        L112.CEDS_GCAM %>%
          filter(CEDS_agg_sector == "Animals") %>%
          mutate(Non.CO2 = paste(Non.CO2,"_AGR",sep="")) ->
          L112.CEDS_GCAM_An

        L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
          rename(production = value) %>%
          left_join(GCAM_sector_tech, by = c("GCAM_commodity" = "sector", "system" = "fuel", "feed" = "technology")) %>%
          select(GCAM_region_ID, GCAM_commodity, system, feed, year, production, EPA_agg_sector, EDGAR_agg_sector) %>%
          repeat_add_columns(tibble::tibble(Non.CO2 = unique(L112.CEDS_GCAM_An$Non.CO2))) %>%  # Add Gas Name and AGR for agriculture
          # match in emissions factors, using left_join and dropping fuel column
          # MAYBE USE EMISSIONS FACTOR FOR N20 FROM CEDS??? WOULD HAVE TO READ THIS IN AS AN INPUT ABOVE. WOULD NEED SECTOR DEFINITION IMPROVED IN CEDS FOR THAT TO BE USABLE
          left_join(L103.ghg_tgmt_USA_an_Sepa_F_2005, by = c("EPA_agg_sector" = "sector")) %>%
          mutate(epa_emissions = production * ch4_em_factor) %>%  # compute unscaled emissions
          select(-fuel) %>%
          na.omit() %>%
          rename(CEDS_agg_sector = EDGAR_agg_sector) ->
          L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt

        # Aggregate by sector and region
        L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt %>%
          group_by(GCAM_region_ID, Non.CO2, CEDS_agg_sector, year) %>%
          summarize(EPA_emissions = sum(epa_emissions)) %>%
          ungroup()  ->
          L113.ghg_tg_R_an_C_Yh.mlt

        # Scale EPA emissions by tech to match CEDS totals

        # First compute scalers
        L113.ghg_tg_R_an_C_Yh.mlt %>%
          left_join(L112.CEDS_GCAM_An, by = c("year", "GCAM_region_ID", "Non.CO2", "CEDS_agg_sector")) %>%
          rename(CEDS_emissions = emissions) %>%
          mutate(scalar = CEDS_emissions / EPA_emissions) ->
          L113.emiss_scalar

        # Second scale EPA emissions
        L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt %>%
          left_join(L113.emiss_scalar, by = c("GCAM_region_ID", "Non.CO2", "CEDS_agg_sector", "year")) %>%
          mutate(emissions = epa_emissions * scalar) %>%
          select(-EPA_emissions, -CEDS_emissions) %>%
          filter(year %in% emissions.CEDS_YEARS) %>%
          replace_na(list(emissions = 0)) %>%
          select(GCAM_region_ID, Non.CO2, supplysector = GCAM_commodity, subsector = system, stub.technology = feed,
                 value = emissions, year) ->
          L113.ghg_tg_R_an_C_Sys_Fd_Yh_full

        # ==============================
        # Agricultural Emissions
        # ==============================

        # Compute shares of regional cropland allocation by crop type, and regional production of each crop by each GLU
        # In the downscaling from (geopolitical) region to crop and GLU, we use land area to go from region to region/crop, and
        # production to go from region/crop to region/GLU/crop
        # ----------------------------------------------------

        # Land area shares (region/crop within region)
        L122.CropAreaShare_R_C_Y <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
          filter(year %in% HISTORICAL_YEARS) %>%
          group_by(year, GCAM_region_ID, GCAM_commodity) %>%
          summarise(value = sum(value)) %>%
          group_by(year, GCAM_region_ID) %>%
          # Crop area by region and year
          mutate(crop_area_total = sum(value)) %>%
          ungroup() %>%
          mutate(crop_area_share = value / crop_area_total)

        # Production shares (region/GLU/crop within region/crop)
        L122.ProdGLUshare_R_C_Y_GLU <- L101.ag_Prod_Mt_R_C_Y_GLU %>%
          filter(year %in% HISTORICAL_YEARS) %>%
          group_by(GCAM_region_ID, GCAM_commodity, year) %>%
          # Production by crop, region, and year
          mutate(Prod_R_C = sum(value)) %>%
          ungroup() %>%
          mutate(prod_share_GLU = value / Prod_R_C) %>%
          replace_na(list(prod_share_GLU = 0))

        # Emissions shares: product of region/crop shares and region/crop/glu shares
        L122.EmissShare_R_C_Y_GLU <- L122.ProdGLUshare_R_C_Y_GLU %>%
          left_join_error_no_match(L122.CropAreaShare_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
          transmute(GCAM_region_ID, GCAM_commodity, year, GLU, emiss_share = prod_share_GLU * crop_area_share)

        # Match emissions to drivers
        # --------------------------

        # Filter out agricultural sectors
        L112.CEDS_GCAM %>%
          filter(CEDS_agg_sector %in% emissions.AGR_SECTORS) %>%
          mutate(Non.CO2 = paste(Non.CO2,"_AGR",sep="")) ->
          L112.CEDS_GCAM_agr

        # Compute emissions from rice by GCAM region, commodity, and GLU
        L112.CEDS_GCAM_agr %>%
          filter(CEDS_agg_sector == "rice") ->
          L112.CEDS_GCAM_rice

        # Compute share of rice production by GLU in each region / year
        L101.ag_Prod_Mt_R_C_Y_GLU %>%
          filter(GCAM_commodity == "Rice", year %in% emissions.CEDS_YEARS) %>%
          group_by(GCAM_region_ID, GCAM_commodity, year) %>%
          # Total production by region, commodity, and year for calculating share
          mutate(total_prod = sum(value)) %>%
          ungroup() %>%
          transmute(GCAM_region_ID, GCAM_commodity, GLU, year, prod_share = value / total_prod) ->
          L122.ag_Prod_Mt_R_rice_Y_GLU

        # Multiply total emissions by production share
        L122.ag_Prod_Mt_R_rice_Y_GLU %>%
          repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_rice$Non.CO2))) %>%
          left_join_error_no_match(L112.CEDS_GCAM_rice, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%
          transmute(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2,
                    emissions = emissions * prod_share, type = "Rice") ->
          L122.ghg_tg_R_rice_Y_GLU

        # Compute emissions from soils by GCAM region, commodity, and GLU
        L112.CEDS_GCAM_agr %>%
          filter(CEDS_agg_sector == "soil") ->
          L112.CEDS_GCAM_soil

        # Multiply total emissions by production share
        L122.EmissShare_R_C_Y_GLU %>%
          filter(year %in% emissions.CEDS_YEARS) %>%
          repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_soil$Non.CO2))) %>%
          left_join_error_no_match(L112.CEDS_GCAM_soil, by = c("GCAM_region_ID", "year", "Non.CO2")) %>%
          transmute(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2,
                    emissions = emissions * emiss_share, type = "Soil") ->
          L122.ghgsoil_tg_R_C_Y_GLU



        # Bind together dataframes & aggregate
        L122.ghg_tg_R_agr_C_Y_GLU_full <- bind_rows( L122.ghg_tg_R_rice_Y_GLU, L122.ghgsoil_tg_R_C_Y_GLU#, L122.ghgfert_tg_R_C_Y_GLU
        ) %>%
          group_by(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2) %>%
          summarise(value = sum(emissions)) %>%
          ungroup()

        # Bind together dataframes & aggregate
        L122.ghg_tg_R_agr_C_Y_GLU_full <- bind_rows( L122.ghg_tg_R_rice_Y_GLU, L122.ghgsoil_tg_R_C_Y_GLU#, L122.ghgfert_tg_R_C_Y_GLU
        ) %>%
          group_by(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2) %>%
          summarise(value = sum(emissions)) %>%
          ungroup()

        # ===================================================
        # AGRICULTURAL WASTE BURNING
        # ===================================================

        # Select agricultural waste burning emissions

        L112.CEDS_GCAM %>%
          filter(CEDS_agg_sector == "ag_waste_burning") %>%
          mutate(Non.CO2 = paste(Non.CO2,"_AWB",sep="")) ->
          L112.CEDS_GCAM_awb

        # Calculate AWB Drivers
        # ---------------------

        # Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...
        # estimated from production, harvest index, and water content

        # Match weighted average residue biomass parameters with crop prodcution.
        L101.ag_Prod_Mt_R_C_Y_GLU %>%
          left_join(L111.ag_resbio_R_C, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
          select(-c(ErosCtrl_tHa, ResEnergy_GJt, Root_Shoot)) ->
          L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU

        # Set the default harvest index of 1 and water content to 0.15 for fiber and fodder crops, in order to use
        # harvest index of 1 and water content to caculate burnable excess biomass in next step.
        L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU %>%
          replace_na(list(HarvestIndex = 1, WaterContent = 0.15)) ->
          L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_replaced

        # Burnable excess biomass is equal to ((biomass production / HarvestIndex) - biomass production) * (1 - WaterContent)
        # For root crops, the calculation could be done differently, if the root mass is excluded from the denominator of the reported harvest index.
        # This doesn't seem to be the case in the literature--while for other crops, root mass is excluded from the harvest index, it is included for potatoes.
        # If excluded, then the harvest index could be greater than 1 (if the tubers weigh more than the above-ground shoots), and the above calculation would
        # return a negative number. None of the crops in the underlying harvested index database have values greater than 1 so this isn't currently an issue.
        L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_replaced %>%
          mutate(burnable = ((value / HarvestIndex) - value) * (1 - WaterContent)) ->
          L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn

        # Aggregate the burnable excess biomass by GCAM region and year.
        L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn %>%
          group_by(GCAM_region_ID, year) %>%
          summarise(value = sum(burnable)) %>%
          ungroup() ->
          L112.ag_ExcessDryBiomass_Mt_R_Y

        # Calculate the share by production technology of each region's burnable excess biomass (AWB_emiss_share).
        # This will be used to create the ag waste burning share of emissions, for downscaling regional emissions to region/GLU/crop
        L112.ag_ExcessDryBiomass_Mt_R_Y %>%
          rename(total_excess_bio = value) %>%
          left_join(L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn, by = c("GCAM_region_ID", "year")) %>%
          mutate(AWB_emiss_share = burnable / total_excess_bio) %>%
          select(GCAM_region_ID, GCAM_commodity, year, GLU, AWB_emiss_share) ->
          L112.AWBshare_R_C_GLU

        # Calculate AWB Emissions
        # -----------------------


        # Compute agricultural waste burning emissions by GCAM region, commodity, and GLU

        # Add gas name to AWB production shares to prepare for matching with emissions
        L112.AWBshare_R_C_GLU %>%
          repeat_add_columns(tibble::tibble(`Non.CO2` = unique(L112.CEDS_GCAM_awb$Non.CO2))) ->
          L112.nonco2_tg_R_awb_C_Y_GLU

        # Estimate ag waste burning emissions using the estimated share (fraction) times total regional AWB emissions
        # Emissions(R, GLU, crop) =  regional total  * AWB share
        L112.nonco2_tg_R_awb_C_Y_GLU %>%
          left_join(L112.CEDS_GCAM_awb, by = c("GCAM_region_ID", "year", "Non.CO2")) %>%
          rename(total_emiss = emissions) %>%
          mutate(emissions = total_emiss * AWB_emiss_share) ->
          L112.nonco2_tg_R_awb_C_Y_GLU_total

        # Subset only the historical years in CEDS, and reshape for write-out
        L112.nonco2_tg_R_awb_C_Y_GLU_total %>%
          filter(year %in% emissions.CEDS_YEARS) %>%
          select(GCAM_region_ID, Non.CO2, GCAM_commodity, GLU, year, value = emissions) ->
          L112.nonco2_tg_R_awb_C_Y_GLU

        # Calculates BC/OC emissions factors for a separate data frame output. bc-oc emissions / ag residue
        L112.nonco2_tg_R_awb_C_Y_GLU %>%
          filter(Non.CO2 %in% c("BC_AWB", "OC_AWB")) %>%
          rename(awb_emission = value) %>%
          # NEED TO REEXAMINE THE DRIVER HERE. IT LIKELY ACTUALLY NEEDS TO BE EXCESS BURNABLE DRY BIOMASS, POSSIBLY L112.ag_ExcessDryBiomass_Mt_R_Y
          left_join_error_no_match(L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
          # Calculate emission factor, which is
          mutate(emfact = awb_emission / burnable) %>%
          select(GCAM_region_ID, Non.CO2, GCAM_commodity, GLU, year, emfact) %>%
          # Replace NaNs with zeros
          mutate_all(funs(replace(., is.na(.), 0))) ->
          L112.bcoc_tgej_R_awb_C_Y_GLU
        # END AGRICULTURAL WASTE BURNING
        # ==============================


        # UNMANAGED LAND EMISSIONS
        # ========================

        # Select agricultural waste burning emissions

        L112.CEDS_GCAM %>%
          filter(CEDS_agg_sector %in% c("forest", "grassland", "deforest")) %>%
          select(-CEDS_agg_fuel) %>%
          rename(sector = CEDS_agg_sector) ->
          L112.CEDS_GCAM_unmgd

        # Part 1: Grassland burning
        # Downscale regional grassland burning emissions to GLU based on the share of land in each GLU
        L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
          group_by(GCAM_region_ID, year) %>%
          mutate(land_share = value / sum(value)) %>%                                                           # Compute the share of regional grassland in each GLU
          select(-value) %>%
          # There are regions (e.g., region #3) where we have grassland area, but no emissions. Use inner join to remove
          inner_join(filter(L112.CEDS_GCAM_unmgd, sector == "grassland"), by = c("GCAM_region_ID", "year")) %>%         # Map in EDGAR grassland emissions
          mutate(emissions = emissions * land_share) %>%                                                                # Compute emissions by GLU using EDGAR totals and land shares
          ungroup() %>%
          select(-sector, -land_share) ->
          L124.nonco2_tg_R_grass_Y_GLU_full

        # Part 2: Forest fires and deforestation
        # Calculate share of forest emissions from forest fires versus deforestation using GFED data.
        # Bind all GFED data together, aggregate by GCAM region/gas/year, calculate share of forest fire versus deforestation
        # Note the odd spaces after some of the mutates below is to avoid tripping test for consecutive mutates
        L112.CEDS_GCAM_unmgd %>%
          filter(sector == "forest") %>%
          rename(forestfire = emissions) %>%
          left_join(L112.CEDS_GCAM_unmgd %>% filter(sector == "deforest") %>% rename(deforest = emissions),
                    by = c("GCAM_region_ID", "Non.CO2", "year")) %>%
          mutate(deforest=if_else(is.na(deforest),0,deforest)) %>%
          select(GCAM_region_ID, Non.CO2, year, forestfire, deforest) %>%
          mutate(PctForestFire = forestfire / (forestfire + deforest)) %>%                               # Compute share of emissions from forest fires
          # There are regions where GFED data is zero for both forest fires and deforestation, leading to NAs
          # Assume those missing values are places with 100% forest fires since these are easier to model in GCAM
          replace_na(list(PctForestFire = 1)) %>%
          mutate(PctDeforest = 1 - PctForestFire) %>%                                                    # Compute share of emissions from deforestation
          select(-forestfire, -deforest) ->
          FireShares_R_G_Y

        # Downscale regional forest burning emissions to GLU based on the share of land in each GLU
        # Use GFED to separate into forest fires and deforestation, which have different drivers in GCAM
        L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
          group_by(GCAM_region_ID, year) %>%
          mutate(land_share = value / sum(value)) %>%                                                      # Compute share of regional forest area in each GLU
          na.omit() %>%
          select(-value) %>%
          # There are places with land area but no emissions and vice versa. Use an inner_join to only get places with both.
          # Note: this means that some regions get zero emissions coefficients in the historic period (future deforestation emissions coefs are defined below)
          inner_join(filter(L112.CEDS_GCAM_unmgd, sector == "forest"), by = c("GCAM_region_ID", "year")) %>%       # Map in EDGAR emissions information
          mutate(emissions = emissions * land_share) %>%                                                           # Compute forest emissions from EDGAR totals and land shares
          select(-sector, -land_share) %>%
          left_join(FireShares_R_G_Y, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%                     # Map in GFED fire shares
          # Assume missing values mean 100% forest fires since these are easier to model in GCAM
          replace_na(list(PctForestFire = 1)) %>%
          replace_na(list(PctDeforest = 0)) %>%
          mutate(ForestFire = emissions * PctForestFire,
                 Deforest = emissions * PctDeforest) %>%                                                       # Compute deforestation emissions
          ungroup() %>%
          select(-emissions, -PctForestFire, -PctDeforest) %>%
          gather(technology, value, -GCAM_region_ID, -GLU, -Land_Type, -Non.CO2, -year) ->
          L124.nonco2_tg_R_forest_Y_GLU_full

        # Compute global average deforestation emissions coefficients
        # These coefficients are used for future model time periods.
        # Compute total change in forest area from 2000 to 2005, total global emissions, and average annualized coefficients (emissions / change in land area / number of years)
        L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
          filter(year %in% emissions.DEFOREST_COEF_YEARS) %>%                                             # Get years that we'll use for deforestation calculation (as of 5/14/17 this was 2000 & 2005)
          mutate(year = if_else(year == min(emissions.DEFOREST_COEF_YEARS), "year1", "year2")) %>%        # Rename years so we can use them as column headings (this also makes this robust to changes in years later)
          spread(year, value) %>%                                                                         # Spread so years are separate columns
          mutate(driver = (year1 - year2) / (emissions.DEFOREST_COEF_YEARS[2] - emissions.DEFOREST_COEF_YEARS[1]),    # Compute average annual deforestation rates (change in forest area / number of years)
                 driver = if_else(driver < 0, 0, driver)) %>%                                             # Deforestation emissions only happen if forest area decreases
          repeat_add_columns(tibble(Non.CO2 = unique(L124.nonco2_tg_R_forest_Y_GLU_full$Non.CO2))) %>%                                                                # Add in rows for all required emissions
          left_join(filter(L124.nonco2_tg_R_forest_Y_GLU_full,                                                 # Map in EDGAR deforestation emissions for the final deforestation year (as of 5/14/17 this was 2005)
                           year == emissions.DEFOREST_COEF_YEARS[2],
                           technology == "Deforest"), by = c("GCAM_region_ID", "Land_Type", "GLU", "Non.CO2")) %>%
          replace_na(list(value = 0)) %>%                                                                 # Note: "value" are the emissions calculated above
          mutate(value = if_else(driver == 0, 0, value)) %>%                                              # Zero out emissions in places where there wasn't any deforestation
          group_by(Land_Type, technology, Non.CO2) %>%
          summarize(driver = sum(driver), emissions = sum(value)) %>%                                     # Calculate global total emissions and deforestation
          mutate(emiss.coef = emissions / driver) %>%                                                     # Calculate average annual deforestation emissions coefficients
          ungroup() %>%
          na.omit() ->
          L124.deforest_coefs_full



        # CLEANING UP FINAL OUTPUTS TO MATCH OLD DATA SYSTEM LEVEL 1 OUTPUTS
        # ==================================================================

        # Because of the diverse drivers and data sources, old level 1 outputs from EDGAR were separated into multiple data frames and sources.
        # Some of these are preserved but many are different. This code slices the prepared CEDS emissions data to match the preexisting level 1 data
        # outputs to allow level two code to continue working unchanged.

        L112.nonco2_tg_R_en_S_F_Yh %>%
          filter(Non.CO2 %in% c("CH4", "N2O")) %>%
          select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emissions) ->
          L112.ghg_tg_R_en_S_F_Yh

        L112.nonco2_tg_R_en_S_F_Yh %>%
          filter(!(Non.CO2 %in% c("CH4", "N2O", "BC", "OC"))) %>%
          select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emissions) ->
          L111.nonghg_tg_R_en_S_F_Yh

        L112.nonco2_tgej_R_en_S_F_Yh %>%
          filter(Non.CO2 %in% c("CH4", "N2O")) %>%
          select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) ->
          L112.ghg_tgej_R_en_S_F_Yh

        # YO Mar 2020
        # update L112.ghg_tgej_R_en_S_F_Yh for resource production
        #--------------------------------------------------------------------------------------
        # START NEW PROCESS
        L112.ghg_tgej_R_en_S_F_Yh %>%
          # produce NA on purpose, so that we can just find those updated values
          left_join(L112.ghg_tgej_R_en_S_F_Yh_adj,
                    by = c("GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology", "year")) %>%
          mutate(value = ifelse(is.na(value_adj), value, value_adj)) %>%
          select(-value_adj) ->
          L112.ghg_tgej_R_en_S_F_Yh_update

        # make unconventional oil the same as crude oil
        L112.ghg_tgej_R_en_S_F_Yh_update %>%
          filter(subsector == "crude oil") %>%
          mutate(subsector = "unconventional oil") %>%
          mutate(stub.technology = "unconventional oil") ->
          L112.ghg_tgej_R_en_S_F_Yh_update_uncov_oil

        # add unconventional oil
        L112.ghg_tgej_R_en_S_F_Yh_update_all <- rbind(L112.ghg_tgej_R_en_S_F_Yh_update,
                                                      L112.ghg_tgej_R_en_S_F_Yh_update_uncov_oil)

        # merge back
        L112.ghg_tgej_R_en_S_F_Yh <- L112.ghg_tgej_R_en_S_F_Yh_update_all

        # END NEW PROCESS
        #--------------------------------------------------------------------------------------

        L112.nonco2_tgej_R_en_S_F_Yh %>%
          filter(!(Non.CO2 %in% c("CH4", "N2O", "BC", "OC"))) %>%
          select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) ->
          L111.nonghg_tgej_R_en_S_F_Yh

        L112.nonco2_tgej_R_en_S_F_Yh %>%
          filter(Non.CO2 %in% c("BC", "OC")) %>%
          filter(year == 2000) %>%
          select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) ->
          L114.bcoc_tgej_R_en_S_F_2000

        # Animal NH3 emissions
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_full %>%
          filter(Non.CO2 == "NH3_AGR") -> L115.nh3_tg_R_an_C_Sys_Fd_Yh

        # Animal all other emissions
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_full %>%
          filter(Non.CO2 != "NH3_AGR") -> L113.ghg_tg_R_an_C_Sys_Fd_Yh

        # YO Mar 2020
        # scale to EPA 2019 for agriculture - livestocks
        # -------------------------------------------------------------------------------------------------------
        # START NEW PROCESS

        # compute EPA total by EPA_sector and all historical years
        EPA_master %>%
          filter(sector == "Agriculture" & gas %in% c("CH4", "N2O") & source == "Livestock") %>%
          left_join_error_no_match(EPA_CH4N2O_map, by = c("sector", "source", "subsource")) %>%
          left_join_error_no_match(EPA_country_map, by = c("country" = "EPA_country")) %>%
          group_by(GCAM_region_ID, EPA_sector, year, gas) %>%
          summarise(EPA_emissions = sum(value)) %>%
          ungroup() %>%
          mutate(gas = paste0(gas, "_AGR")) ->
          L131.EPA_CH4N2O_livestocks

        # calculate scalers for livestocks emissions by EPA_sector by year and region
        L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
          filter(Non.CO2 %in% c("CH4_AGR", "N2O_AGR")) ->
          L113.ghg_tg_R_an_C_Sys_Fd_Yh_change

        L113.ghg_tg_R_an_C_Sys_Fd_Yh_change %>%
          mutate(EPA_sector = "Agriculture") %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          summarise(tot_emissions = sum(value)) %>%
          ungroup() %>%
          # convert into CO2eq by GWP (AP4): CH4 is 25; N2O is 298
          mutate(tot_emissions = ifelse(Non.CO2 == "CH4_AGR", tot_emissions * 25, tot_emissions *  298)) %>%
          left_join(L131.EPA_CH4N2O_livestocks, by = c("GCAM_region_ID", "EPA_sector", "Non.CO2" = "gas", "year")) %>%
          mutate(EPA_emissions = ifelse(is.na(EPA_emissions), tot_emissions, EPA_emissions)) %>%
          mutate(emscaler = EPA_emissions / tot_emissions) %>%
          mutate(emscaler = ifelse(is.na(emscaler) | is.infinite(emscaler), 1, emscaler)) %>%
          select(-EPA_emissions, -tot_emissions) ->
          L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler

        # leave emscaler = 1 part as it is
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler %>%
          filter(emscaler == 1) -> L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler_unchanged

        # adjust scalers for outliers
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler %>%
          filter(emscaler != 1) %>%
          group_by(EPA_sector, Non.CO2, year) %>%
          mutate(lower = quantile(emscaler, 0.25) - 1.5 * IQR(emscaler),
                 upper = quantile(emscaler, 0.75) + 1.5 * IQR(emscaler)) %>%
          ungroup() %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          mutate(emscaler = max(emscaler, lower)) %>%
          mutate(emscaler = min(emscaler, upper)) %>%
          select(-lower, -upper) %>%
          ungroup() ->
          L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler_changed

        # combine
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler <- rbind(L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler_unchanged,
                                                        L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler_changed)


        # do the actual scaling for livestock-related emissions
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_change %>%
          mutate(EPA_sector = "Agriculture") %>%
          left_join_error_no_match(L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler,
                                   by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
          mutate(value = value * emscaler) %>%
          select(-EPA_sector, -emscaler) ->
          L113.ghg_tg_R_an_C_Sys_Fd_Yh_update

        L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
          filter(!(Non.CO2 %in% c("CH4_AGR", "N2O_AGR"))) %>%
          bind_rows(L113.ghg_tg_R_an_C_Sys_Fd_Yh_update) ->
          L113.ghg_tg_R_an_C_Sys_Fd_Yh_adj

        # update the origional dataset
        L113.ghg_tg_R_an_C_Sys_Fd_Yh <- L113.ghg_tg_R_an_C_Sys_Fd_Yh_adj

        # END NEW PROCESS
        # -------------------------------------------------------------------------------------------------------

        # Ag Waste Burning
        L112.AWBshare_R_C_GLU ->
          L121.AWBshare_R_C_GLU

        L112.nonco2_tg_R_awb_C_Y_GLU %>%
          filter(!(Non.CO2 %in% c("BC_AWB", "OC_AWB"))) ->
          L121.nonco2_tg_R_awb_C_Y_GLU

        # YO Jun 2020
        # scale to EPA 2019 for agriculture waste burning
        # -------------------------------------------------------------------------------------------------------
        # START NEW PROCESS

        # compute EPA total by EPA_sector and all historical years
        EPA_master %>%
          filter(sector == "Agriculture" & gas %in% c("CH4", "N2O") & source %in% c("OtherAg")) %>%
          left_join_error_no_match(EPA_CH4N2O_map, by = c("sector", "source", "subsource")) %>%
          left_join_error_no_match(EPA_country_map, by = c("country" = "EPA_country")) %>%
          group_by(GCAM_region_ID, EPA_sector, year, gas) %>%
          summarise(EPA_emissions = sum(value)) %>%
          ungroup() %>%
          mutate(gas = paste0(gas, "_AWB")) ->
          L121.EPA_CH4N2O_awb

        # calculate scalers for agriculture emissions by EPA_sector by year and region
        L121.nonco2_tg_R_awb_C_Y_GLU %>%
          filter(Non.CO2 %in% c("CH4_AWB", "N2O_AWB")) ->
          L121.nonco2_tg_R_awb_C_Y_GLU_change

        L121.nonco2_tg_R_awb_C_Y_GLU_change %>%
          mutate(EPA_sector = "Agriculture") %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          summarise(tot_emissions = sum(value)) %>%
          ungroup() %>%
          # convert into CO2eq by GWP (AP4): CH4 is 25; N2O is 298
          mutate(tot_emissions = ifelse(Non.CO2 == "CH4_AWB", tot_emissions * 25, tot_emissions *  298)) %>%
          left_join(L121.EPA_CH4N2O_awb, by = c("GCAM_region_ID", "EPA_sector", "Non.CO2" = "gas", "year")) %>%
          #TODO: temperary fix, because the original data do not have values in 2015
          # should be due to one original CEDS data not update to date
          mutate(tot_emissions = ifelse(is.na(tot_emissions), EPA_emissions, tot_emissions)) %>%
          mutate(EPA_emissions = ifelse(is.na(EPA_emissions), tot_emissions, EPA_emissions)) %>%
          mutate(emscaler = EPA_emissions / tot_emissions) %>%
          mutate(emscaler = ifelse(is.na(emscaler) | is.infinite(emscaler), 1, emscaler)) %>%
          select(-EPA_emissions, -tot_emissions) ->
          L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler

        # leave emscaler = 1 part as it is
        L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler %>%
          filter(emscaler == 1) -> L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler_unchanged

        # adjust scalers for outliers
        L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler %>%
          filter(emscaler != 1) %>%
          group_by(EPA_sector, Non.CO2, year) %>%
          mutate(lower = quantile(emscaler, 0.25) - 1.5 * IQR(emscaler),
                 upper = quantile(emscaler, 0.75) + 1.5 * IQR(emscaler)) %>%
          ungroup() %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          mutate(emscaler = max(emscaler, lower)) %>%
          mutate(emscaler = min(emscaler, upper)) %>%
          select(-lower, -upper) %>%
          ungroup() ->
          L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler_changed

        # combine
        L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler <- rbind(L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler_unchanged,
                                                        L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler_changed)

        # do the actual scaling for agriculture emissions
        L121.nonco2_tg_R_awb_C_Y_GLU_change %>%
          mutate(EPA_sector = "Agriculture") %>%
          left_join_error_no_match(L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler,
                                   by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
          mutate(value = value * emscaler) %>%
          select(-EPA_sector, -emscaler) ->
          L121.nonco2_tg_R_awb_C_Y_GLU_update

        L121.nonco2_tg_R_awb_C_Y_GLU %>%
          filter(!(Non.CO2 %in% c("CH4_AWB", "N2O_AWB"))) %>%
          bind_rows(L121.nonco2_tg_R_awb_C_Y_GLU_update) ->
          L121.nonco2_tg_R_awb_C_Y_GLU_adj

        # update the origional dataset
        L121.nonco2_tg_R_awb_C_Y_GLU <- L121.nonco2_tg_R_awb_C_Y_GLU_adj

        # END NEW PROCESS
        # -------------------------------------------------------------------------------------------------------

        L122.EmissShare_R_C_Y_GLU -> L122.EmissShare_R_C_Y_GLU

        # AGR emissions: Filter for gases present in original data set
        L122.ghg_tg_R_agr_C_Y_GLU_full -> # %>%
          # filter(Non.CO2 %in% c("NH3_AGR", "N2O_AGR", "NOx_AGR", "CH4_AGR")) ->
          L122.ghg_tg_R_agr_C_Y_GLU

        # YO Mar 2020
        # scale to EPA 2019 for agriculture
        # -------------------------------------------------------------------------------------------------------
        # START NEW PROCESS

        # compute EPA total by EPA_sector and all historical years
        EPA_master %>%
          filter(sector == "Agriculture" & gas %in% c("CH4", "N2O") & source %in% c("Rice", "AgSoils")) %>%
          left_join_error_no_match(EPA_CH4N2O_map, by = c("sector", "source", "subsource")) %>%
          left_join_error_no_match(EPA_country_map, by = c("country" = "EPA_country")) %>%
          group_by(GCAM_region_ID, EPA_sector, year, gas) %>%
          summarise(EPA_emissions = sum(value)) %>%
          ungroup() %>%
          mutate(gas = paste0(gas, "_AGR")) ->
          L131.EPA_CH4N2O_agr

        # calculate scalers for agriculture emissions by EPA_sector by year and region
        L122.ghg_tg_R_agr_C_Y_GLU %>%
          filter(Non.CO2 %in% c("CH4_AGR", "N2O_AGR")) ->
          L122.ghg_tg_R_agr_C_Y_GLU_change

        L122.ghg_tg_R_agr_C_Y_GLU_change %>%
          mutate(EPA_sector = "Agriculture") %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          summarise(tot_emissions = sum(value)) %>%
          ungroup() %>%
          # convert into CO2eq by GWP (AP4): CH4 is 25; N2O is 298
          mutate(tot_emissions = ifelse(Non.CO2 == "CH4_AGR", tot_emissions * 25, tot_emissions *  298)) %>%
          left_join(L131.EPA_CH4N2O_agr, by = c("GCAM_region_ID", "EPA_sector", "Non.CO2" = "gas", "year")) %>%
          mutate(EPA_emissions = ifelse(is.na(EPA_emissions), tot_emissions, EPA_emissions)) %>%
          mutate(emscaler = EPA_emissions / tot_emissions) %>%
          mutate(emscaler = ifelse(is.na(emscaler) | is.infinite(emscaler), 1, emscaler)) %>%
          select(-EPA_emissions, -tot_emissions) ->
          L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler

        # leave emscaler = 1 part as it is
        L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler %>%
          filter(emscaler == 1) -> L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler_unchanged

        # adjust scalers for outliers
        L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler %>%
          filter(emscaler != 1) %>%
          group_by(EPA_sector, Non.CO2, year) %>%
          mutate(lower = quantile(emscaler, 0.25) - 1.5 * IQR(emscaler),
                 upper = quantile(emscaler, 0.75) + 1.5 * IQR(emscaler)) %>%
          ungroup() %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          mutate(emscaler = max(emscaler, lower)) %>%
          mutate(emscaler = min(emscaler, upper)) %>%
          select(-lower, -upper) %>%
          ungroup() ->
          L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler_changed

        # combine
        L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler <- rbind(L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler_unchanged,
                                                     L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler_changed)

        # do the actual scaling for agriculture emissions
        L122.ghg_tg_R_agr_C_Y_GLU_change %>%
          mutate(EPA_sector = "Agriculture") %>%
          left_join_error_no_match(L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler,
                                   by = c("GCAM_region_ID", "year", "Non.CO2", "EPA_sector")) %>%
          mutate(value = value * emscaler) %>%
          select(-EPA_sector, -emscaler) ->
          L122.ghg_tg_R_agr_C_Y_GLU_update

        L122.ghg_tg_R_agr_C_Y_GLU %>%
          filter(!(Non.CO2 %in% c("CH4_AGR", "N2O_AGR"))) %>%
          bind_rows(L122.ghg_tg_R_agr_C_Y_GLU_update) ->
          L122.ghg_tg_R_agr_C_Y_GLU_adj

        # update the origional dataset
        L122.ghg_tg_R_agr_C_Y_GLU <- L122.ghg_tg_R_agr_C_Y_GLU_adj

        # END NEW PROCESS
        # -------------------------------------------------------------------------------------------------------

        # BCOC emissions from Ag Waste Burning
        L112.bcoc_tgej_R_awb_C_Y_GLU %>%
          filter(year == 2000) %>%
          select(-year) ->
          L123.bcoc_tgmt_R_awb_2000

        L124.deforest_coefs_full %>%
          filter(!(Non.CO2 %in% c("BC", "OC"))) ->
          L124.deforest_coefs

        # Filter out BC/OC for its own output
        L124.nonco2_tg_R_grass_Y_GLU_full %>%
          rename(value = emissions) %>%
          filter(!(Non.CO2 %in% c("BC", "OC"))) ->
          L124.nonco2_tg_R_grass_Y_GLU

        #Filter out BC/OC for its own output
        L124.nonco2_tg_R_forest_Y_GLU_full %>%
          filter(!(Non.CO2 %in% c("BC", "OC"))) ->
          L124.nonco2_tg_R_forest_Y_GLU

        # Filter out BC/OC for its own output
        L124.nonco2_tg_R_grass_Y_GLU_full %>%
          rename(value = emissions) %>%
          filter(Non.CO2 %in% c("BC", "OC")) %>%
          filter(year == 2000) ->
          L124.bcoc_tg_R_grass_Y_GLU

        # IF YOU TURN THIS BACK ON YOULL NEED TO GENERATE THE EMISSIONS FACTORS HERE BY READING IN THE DRIVER
        L125.bcoc_tgbkm2_R_grass_2000 <- L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
          #filter(year == 2000) %>%
          group_by(GCAM_region_ID, Land_Type, year) %>%
          mutate(value=sum(value)) %>%
          ungroup() %>%
          select(GCAM_region_ID, Land_Type, year,value) %>%
          distinct() %>% # aggregate grassland land area by regions/land type
          repeat_add_columns(tibble::tibble(Non.CO2 = unique(L124.bcoc_tg_R_grass_Y_GLU$Non.CO2))) %>%
          filter(year %in% c(L124.bcoc_tg_R_grass_Y_GLU$year)) %>% # repeat for both BC and OC
          #revisit this. Some regions are missing.
          inner_join(L124.bcoc_tg_R_grass_Y_GLU %>% rename(em=value) %>% filter(year %in% c(L124.LC_bm2_R_Grass_Yh_GLU_adj$year)), by = c("GCAM_region_ID", "Non.CO2","year","Land_Type")) %>% # add emissions to land region area
          mutate(em_factor = em / value) %>% # calculate emission factor (emissions/area)
          select(GCAM_region_ID, Land_Type, Non.CO2, em_factor,year) %>%
          arrange(Non.CO2, Land_Type,GCAM_region_ID,year) %>%
          ungroup()

        #Filter out BC/OC for its own output
        L124.nonco2_tg_R_forest_Y_GLU_full %>%
          filter(Non.CO2 %in% c("BC", "OC"))-> L125.bcoc_tgbkm2_R_forest_2000_data
        #Calculate BCOC for forest fires and deforestattion

        #First get driver for forests
        L125.bcoc_tgbkm2_R_forestfire_2000 <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
          filter(year == 2000) %>%
          group_by(GCAM_region_ID, Land_Type) %>%
          summarise(FF_driver = sum(value))

        #Now get driver for deforest
        L125.bcoc_tgbkm2_R_GLU_defor_2000 <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
          filter(year %in% emissions.DEFOREST_COEF_YEARS) %>%      # select only deforestation coefficient years (used to estiamte rate of change - D-driver)
          group_by(GCAM_region_ID, Land_Type, GLU) %>%
          spread(year, value) %>%
          mutate(D_driver = pmax(`2000` - `2005`, 0) / (emissions.DEFOREST_COEF_YEARS[2] - emissions.DEFOREST_COEF_YEARS[1]))


        L125.bcoc_tgbkm2_R_defor_2000 <-  L125.bcoc_tgbkm2_R_GLU_defor_2000 %>%
          group_by(GCAM_region_ID, Land_Type) %>%
          summarise_at(vars(D_driver), sum)


        L125.bcoc_tgbkm2_R_forest_2000_data %>%
          filter(year==2000) %>%
          filter(technology=="ForestFire") %>%
          group_by(GCAM_region_ID,Land_Type,year,Non.CO2,technology) %>%
          mutate(value=sum(value)) %>%
          ungroup() %>%
          select(GCAM_region_ID,Land_Type,year,Non.CO2,technology,value) %>%
          distinct() %>%
          #Join in forest fire data
          inner_join(L125.bcoc_tgbkm2_R_forestfire_2000,by=c("GCAM_region_ID","Land_Type")) %>%
          group_by("GCAM_region_ID","Land_Type","technology","Non.CO2") %>%
          mutate(em_factor=(value)/(FF_driver)) %>%
          ungroup() %>%
          select(GCAM_region_ID,Land_Type,Non.CO2,technology,em_factor) %>%
          distinct()->BC_OC_Forest

        L125.bcoc_tgbkm2_R_forest_2000_data %>%
          filter(year==2000) %>%
          filter(technology=="Deforest") %>%
          group_by(GCAM_region_ID,Land_Type,year,Non.CO2,technology) %>%
          mutate(value=sum(value)) %>%
          ungroup() %>%
          select(GCAM_region_ID,Land_Type,year,Non.CO2,technology,value) %>%
          distinct() %>%
          #Join in de-forest data
          inner_join(L125.bcoc_tgbkm2_R_defor_2000,by=c("GCAM_region_ID", "Land_Type")) %>%
          group_by("GCAM_region_ID","Land_Type","technology","Non.CO2") %>%
          mutate(em_factor=(value)/(D_driver)) %>%
          ungroup() %>%
          select(GCAM_region_ID,Land_Type,Non.CO2,technology,em_factor) %>%
          distinct()->BC_OC_Deforest

        L125.bcoc_tgbkm2_R_forest_2000<-bind_rows(BC_OC_Deforest,BC_OC_Forest) %>%
          mutate(em_factor=if_else(is.finite(em_factor),em_factor,0))


        # Filter out BC OC for its own output
        L124.deforest_coefs_full %>%
          filter(Non.CO2 %in% c("BC", "OC")) ->L125.deforest_coefs_bcoc

        #---------------------------------------------------------------
        # START NEW PROCESS
        # YO 2020
        # scale combustion-related emissions to EPA 2019 BAU
        # ------------------------------------------------------------------------------------------------------------------------------
        # calculate scalers for combustion-related emissions by EPA_sector by year and region
        L112.ghg_tg_R_en_S_F_Yh %>%
          left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                   by = c("supplysector", "subsector", "stub.technology")) %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          summarise(tot_emissions = sum(value)) %>%
          ungroup() %>%
          # convert into CO2eq by GWP (AP4): CH4 is 25; N2O is 298
          mutate(tot_emissions = ifelse(Non.CO2 == "CH4", tot_emissions * 25, tot_emissions *  298)) %>%
          left_join(L112.EPA_CH4N2O_energy, by = c("GCAM_region_ID", "EPA_sector", "Non.CO2" = "gas", "year")) %>%
          mutate(EPA_emissions = ifelse(is.na(EPA_emissions), tot_emissions, EPA_emissions)) %>%
          mutate(emscaler = EPA_emissions / tot_emissions) %>%
          mutate(emscaler = ifelse(is.na(emscaler) | is.infinite(emscaler), 1, emscaler)) %>%
          select(-EPA_emissions, -tot_emissions) ->
          L112.ghg_tg_R_en_S_F_Yh_EPAscaler

        # leave emscaler = 1 part as it is
        L112.ghg_tg_R_en_S_F_Yh_EPAscaler %>%
          filter(emscaler == 1) -> L112.ghg_tg_R_en_S_F_Yh_EPAscaler_unchanged

        # adjust scalers for outliers
        L112.ghg_tg_R_en_S_F_Yh_EPAscaler %>%
          filter(emscaler != 1) %>%
          group_by(EPA_sector, Non.CO2, year) %>%
          mutate(lower = quantile(emscaler, 0.25) - 1.5 * IQR(emscaler),
                 upper = quantile(emscaler, 0.75) + 1.5 * IQR(emscaler)) %>%
          ungroup() %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          mutate(emscaler = max(emscaler, lower)) %>%
          mutate(emscaler = min(emscaler, upper)) %>%
          select(-lower, -upper) %>%
          ungroup() ->
          L112.ghg_tg_R_en_S_F_Yh_EPAscaler_changed

        # combine
        L112.ghg_tg_R_en_S_F_Yh_EPAscaler <- rbind(L112.ghg_tg_R_en_S_F_Yh_EPAscaler_unchanged,
                                                   L112.ghg_tg_R_en_S_F_Yh_EPAscaler_changed)

        # do the actual scaling for combustion-related emissions
        L112.ghg_tg_R_en_S_F_Yh %>%
          left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                   by = c("supplysector", "subsector", "stub.technology")) %>%
          left_join_error_no_match(L112.ghg_tg_R_en_S_F_Yh_EPAscaler,
                                   by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
          mutate(value = value * emscaler) %>%
          select(-EPA_sector, -emscaler) ->
          L112.ghg_tg_R_en_S_F_Yh_adj

        L112.ghg_tg_R_en_S_F_Yh <- L112.ghg_tg_R_en_S_F_Yh_adj

        # END OF NEW PROCESS
        # -------------------------------------------------------------------------------------------------------------

      # ===============
      # Produce outputs
      L111.nonghg_tg_R_en_S_F_Yh %>%
        na.omit() %>%
        add_title("Non-ghg emission totals by GCAM sector, fuel, technology, and driver type for CEDS historical years.") %>%
        add_units("Tg") %>%
        add_comments("Compute unscaled non-ghg emissions by country and technology, and CEDS emissions by region and sector.") %>%
        add_comments("Then, scale EPA emissions by tech to match EDGAR totals, compute international shipping and international aviation emissions, ") %>%
        add_comments("and finally calculate non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
        add_legacy_name("L111.nonghg_tg_R_en_S_F_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                       "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech","emissions/mappings/Trn_subsector_revised",
                       "emissions/mappings/GCAM_sector_tech_CEDS","emissions/mappings/calibrated_outresources","emissions/mappings/GCAM_sector_tech_CEDS_revised",
                       "L101.in_EJ_R_en_Si_F_Yh","emissions/CEDS/CEDS_sector_tech_revised","emissions/mappings/UCD_techs_emissions_revised","L154.IEA_histfut_data_times_UCD_shares",
                       "emissions/CEDS/gains_iso_sector_emissions","emissions/CEDS/gains_iso_fuel_emissions") ->
        L111.nonghg_tg_R_en_S_F_Yh

      L111.nonghg_tgej_R_en_S_F_Yh %>%
        na.omit() %>%
        add_title("Non-ghg emission total shares by GCAM sector, fuel, technology, and driver type for CEDS historical years.") %>%
        add_units("Tg/EJ") %>%
        add_comments("Use non-ghg emission totals by GCAM sector, fuel, technology, and driver type for CEDS historical years to derive emission shares.") %>%
        add_legacy_name("L111.nonghg_tgej_R_en_S_F_Yh") %>%
        same_precursors_as("L111.nonghg_tg_R_en_S_F_Yh") ->
        L111.nonghg_tgej_R_en_S_F_Yh

      L112.ghg_tg_R_en_S_F_Yh %>%
        add_title("GHG emissions by energy sector, gas, region, and historical year") %>%
        add_units("Tg") %>%
        add_comments("Emissions calculated with CEDS totals scaled to EPA 2019 totals") %>%
        add_legacy_name("L112.ghg_tg_R_en_S_F_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                       "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech","emissions/mappings/calibrated_outresources",
                       "L101.in_EJ_R_en_Si_F_Yh", "emissions/EPA/EPA_2019_raw", "emissions/EPA_CH4N2O_map","emissions/mappings/Trn_subsector_revised",
                       "emissions/EPA_country_map","emissions/CEDS/CEDS_sector_tech_revised","emissions/mappings/UCD_techs_emissions_revised","L154.IEA_histfut_data_times_UCD_shares",
                       "emissions/CEDS/gains_iso_sector_emissions","emissions/CEDS/gains_iso_fuel_emissions") ->
        L112.ghg_tg_R_en_S_F_Yh

      L112.ghg_tgej_R_en_S_F_Yh %>%
        add_title("GHG emissions factors by energy sector, gas, region, and historical year") %>%
        add_units("Tg/EJ") %>%
        add_comments("Emissions calculated with EPA emissions factors and scaled to EDGAR totals") %>%
        add_comments("Then, emissions factors computed by dividing calculated emissions by energy data") %>%
        add_legacy_name("L112.ghg_tgej_R_en_S_F_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                       "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech","emissions/mappings/calibrated_outresources","emissions/mappings/Trn_subsector_revised",
                       "L101.in_EJ_R_en_Si_F_Yh", "emissions/EPA/EPA_2019_raw", "emissions/EPA_CH4N2O_map",
                       "energy/rsrc_fol_prod_vintage","emissions/EPA_country_map","emissions/CEDS/CEDS_sector_tech_revised","emissions/mappings/UCD_techs_emissions_revised") ->
        L112.ghg_tgej_R_en_S_F_Yh

      L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
        add_title("Animal GHG emissions (CH4 and N2O) by GCAM region / sector / technology / historical year") %>%
        add_units("Tg") %>%
        add_comments("First: compute unscaled emissions by country and technology") %>%
        add_comments("Second: match in emissions factors from EPA") %>%
        add_comments("Third: compute unscaled emissions (production * emfactors) and aggregate by sector and region") %>%
        add_comments("Fourth: compute EDGAR emissions by region and sector") %>%
        add_comments("Fifth: scale EPA emissions by tech to match EDGAR") %>%
        add_legacy_name("L113.ghg_tg_R_an_C_Sys_Fd_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech", "emissions/CEDS/CEDS_sector_tech_revised",
                       "emissions/mappings/GCAM_sector_tech_CEDS", "L107.an_Prod_Mt_R_C_Sys_Fd_Y","emissions/mappings/GCAM_sector_tech_CEDS_revised",
                       "L103.ghg_tgmt_USA_an_Sepa_F_2005") ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh


      L115.nh3_tg_R_an_C_Sys_Fd_Yh %>%
        add_title(" Animal NH3 emissions by GCAM region / sector / technology / historical year") %>%
        add_units("Tg") %>%
        add_comments("Annual animal NH3 emissions is computed using CEDS emissions and FAO animal production.") %>%
        add_legacy_name("L115.nh3_tg_R_an_C_Sys_Fd_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L107.an_Prod_Mt_R_C_Sys_Fd_Y","L107.an_Prod_Mt_R_C_Sys_Fd_Y") ->
        L115.nh3_tg_R_an_C_Sys_Fd_Yh

      L121.AWBshare_R_C_GLU %>%
        add_title("Ag waste burning share of emissions by GCAM region / commodity / GLU / historical year") %>%
        add_units("unitless share") %>%
        add_comments("Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...") %>%
        add_comments("estimated from production, harvest index, and water content") %>%
        add_legacy_name("L121.AWBshare_R_C_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L101.ag_Prod_Mt_R_C_Y_GLU", "L111.ag_resbio_R_C") ->
        L121.AWBshare_R_C_Y_GLU

      L121.nonco2_tg_R_awb_C_Y_GLU %>%
        add_title("Ag waste burning emissions by GCAM region / commodity / GLU / historical year") %>%
        add_units("Unit = Tg") %>%
        add_comments("Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...") %>%
        add_comments("estimated from production, harvest index, and water content") %>%
        add_legacy_name("L121.nonco2_tg_R_awb_C_Y_GLU") %>%
        same_precursors_as("L121.AWBshare_R_C_Y_GLU") ->
        L121.nonco2_tg_R_awb_C_Y_GLU

      L122.EmissShare_R_C_Y_GLU %>%
        add_title("Agriculture emissions shares by GCAM region, commodity, GLU, and historical year") %>%
        add_units("unitless share") %>%
        add_comments("Multiply region/crop area shares by region/crop/GLU production shares") %>%
        add_legacy_name("L122.EmissShare_R_C_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L101.ag_Prod_Mt_R_C_Y_GLU","L111.ag_resbio_R_C","L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") ->
        L122.EmissShare_R_C_Y_GLU

      L122.ghg_tg_R_agr_C_Y_GLU %>%
        add_title("Agriculture emissions by GCAM region, commodity, GLU, and historical year") %>%
        add_units("Tg") %>%
        add_comments("EDGAR emissions shared out by crop production") %>%
        add_legacy_name("L122.ghg_tg_R_agr_C_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L124.LC_bm2_R_Grass_Yh_GLU_adj","L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
        L122.ghg_tg_R_agr_C_Y_GLU


      L124.nonco2_tg_R_grass_Y_GLU %>%
        add_title("Grassland fire emissions by GCAM region, gas, and historical year") %>%
        add_units("Tg/yr") %>%
        add_comments("EDGAR grassland emissions are downscaled to GLU using shares of grassland area.") %>%
        add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "L124.LC_bm2_R_Grass_Yh_GLU_adj") ->
        L124.nonco2_tg_R_grass_Y_GLU

      L125.bcoc_tgbkm2_R_grass_2000 %>%
        add_title("Grassland fire emissions for BCOC by GCAM region, gas, and historical year") %>%
        add_units("Tg/yr") %>%
        add_comments("EDGAR grassland emissions are downscaled to GLU using shares of grassland area.") %>%
        add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "L124.LC_bm2_R_Grass_Yh_GLU_adj")->L125.bcoc_tgbkm2_R_grass_2000

      L125.bcoc_tgbkm2_R_forest_2000 %>%
        add_title("Forest fire emissions for BCOC by GCAM region, gas, and historical year") %>%
        add_units("Tg/yr") %>%
        add_comments("EDGAR grassland emissions are downscaled to GLU using shares of grassland area.") %>%
        add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "L124.LC_bm2_R_Grass_Yh_GLU_adj")->L125.bcoc_tgbkm2_R_forest_2000

      L125.deforest_coefs_bcoc %>%
        add_title("Forest fire emissions for BCOC by GCAM region, gas, and historical year") %>%
        add_units("Tg/yr") %>%
        add_comments("EDGAR grassland emissions are downscaled to GLU using shares of grassland area.") %>%
        add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "L124.LC_bm2_R_Grass_Yh_GLU_adj")->L125.deforest_coefs_bcoc

      L124.nonco2_tg_R_forest_Y_GLU %>%
        add_title("Forest fire and deforestation emissions by GCAM region, gas, and historical year") %>%
        add_units("Tg/yr") %>%
        add_comments("EDGAR forest emissions are downscaled to GLU using shares of forest area.") %>%
        add_comments("These emissions are then separated into forest fire and deforestation using GFED data.") %>%
        add_legacy_name("L124.nonco2_tg_R_forest_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
        L124.nonco2_tg_R_forest_Y_GLU

      L124.deforest_coefs %>%
        add_title("Default deforestation coefficients by Non-CO2 species") %>%
        add_units("Tg/yr") %>%
        add_comments("Global average deforestation coefficients are calculated from global emissions and global deforestation.") %>%
        add_legacy_name("L124.deforest_coefs") %>%
        same_precursors_as("L124.nonco2_tg_R_forest_Y_GLU") ->
        L124.deforest_coefs


      L131.nonco2_tg_R_prc_S_S_Yh %>%
        add_title("GHG emissions by GCAM region / sector / technology / historical year") %>%
        add_units("Tg") %>%
        add_comments("Calculate historical emissions from the processing sector by GCAM ") %>%
        add_comments("technology computed from CEDS emissions data and scaled to EPA 2019") %>%
        add_comments("for CH4 and NO2 industry processes") %>%
        add_legacy_name("L131.nonco2_tg_R_prc_S_S_Yh") %>%
        add_precursors("emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "emissions/EPA_FCCC_IndProc_2005", "emissions/EPA/EPA_2019_raw", "emissions/EPA_CH4N2O_map",
                       "emissions/GCAM_EPA_CH4N2O_energy_map") ->
        L131.nonco2_tg_R_prc_S_S_Yh

      return_data(L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh, L112.ghg_tg_R_en_S_F_Yh, L112.ghg_tgej_R_en_S_F_Yh, L113.ghg_tg_R_an_C_Sys_Fd_Yh, L115.nh3_tg_R_an_C_Sys_Fd_Yh, L121.nonco2_tg_R_awb_C_Y_GLU,
                  L121.AWBshare_R_C_Y_GLU, L122.ghg_tg_R_agr_C_Y_GLU, L122.EmissShare_R_C_Y_GLU, L124.nonco2_tg_R_grass_Y_GLU, L124.nonco2_tg_R_forest_Y_GLU, L124.deforest_coefs,
                  L131.nonco2_tg_R_prc_S_S_Yh,L125.bcoc_tgbkm2_R_grass_2000,L125.bcoc_tgbkm2_R_forest_2000,L125.deforest_coefs_bcoc) # TURNED OFF BCOC OUTPUTS: ,,,L123.bcoc_tgmt_R_awb_2000, L114.bcoc_tgej_R_en_S_F_2000,
    } else {
      stop("Unknown command")
    }
  }
}
