# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L244.building_det
#'
#' Creates level2 data for the building sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.SubregionalShares}, \code{L244.SubregionalShares_SSP1}, \code{L244.SubregionalShares_SSP2},\code{L244.SubregionalShares_SSP3},
#' \code{L244.SubregionalShares_SSP4},\code{L244.SubregionalShares_SSP5},
#' \code{L244.PriceExp_IntGains}, \code{L244.Floorspace}, \code{L244.DemandFunction_serv},
#' \code{L244.DemandFunction_flsp}, \code{L244.Satiation_flsp}, \code{L244.SatiationAdder}, \code{L244.ThermalBaseService}, \code{L244.GenericBaseService},
#'\code{L244.ThermalServiceSatiation}, \code{L244.GenericServiceSatiation}, \code{L244.Intgains_scalar}, \code{L244.ShellConductance_bld},
#' \code{L244.Supplysector_bld}, \code{L244.FinalEnergyKeyword_bld}, \code{L244.SubsectorShrwt_bld}, \code{L244.SubsectorShrwtFllt_bld}, \code{L244.SubsectorInterp_bld},
#' \code{L244.SubsectorInterpTo_bld}, \code{L244.SubsectorLogit_bld}, \code{L244.FuelPrefElast_bld}, \code{L244.StubTech_bld}, \code{L244.StubTechEff_bld},
#' \code{L244.StubTechCalInput_bld}, \code{L244.StubTechIntGainOutputRatio}, \code{L244.GlobalTechShrwt_bld}, \code{L244.GlobalTechCost_bld},
#' \code{L244.DeleteGenericService}, \code{L244.Satiation_flsp_SSP1}, \code{L244.SatiationAdder_SSP1}, \code{L244.GenericServiceSatiation_SSP1},
#' \code{L244.Satiation_flsp_SSP2}, \code{L244.SatiationAdder_SSP2}, \code{L244.GenericServiceSatiation_SSP2}, \code{L244.Satiation_flsp_SSP3},
#' \code{L244.SatiationAdder_SSP3}, \code{L244.GenericServiceSatiation_SSP3}, \code{L244.Satiation_flsp_SSP4},
#' \code{L244.SatiationAdder_SSP4}, \code{L244.GenericServiceSatiation_SSP4},  \code{L244.Satiation_flsp_SSP5},
#' \code{L244.SatiationAdder_SSP5}, \code{L244.GenericServiceSatiation_SSP5}, \code{L244.DeleteThermalService},
#' \code{L244.HDDCDD_A2_CCSM3x}, \code{L244.HDDCDD_A2_HadCM3}, \code{L244.HDDCDD_B1_CCSM3x}, \code{L244.HDDCDD_B1_HadCM3},
#' \code{L244.HDDCDD_constdd_no_GCM} and \code{L244.GompFnParam}, \code{L244.Satiation_impedance},\code{L244.Satiation_impedance_SSP1},\code{L244.Satiation_impedance_SSP2},\code{L244.Satiation_impedance_SSP3}
#' \code{L244.Satiation_impedance_SSP4}, \code{L244.Satiation_impedance_SSP5}, \code{L244.GenericServiceImpedance}, \code{L244.GenericServiceImpedance_SSP1},
#' \code{L244.GenericServiceImpedance_SSP2}, \code{L244.GenericServiceImpedance_SSP3},\code{L244.GenericServiceImpedance_SSP4} , \code{L244.GenericServiceImpedance_SSP5}
#' \code{L244.GenericServiceAdder},\code{L244.GenericServiceAdder_SSP1}, \code{L244.GenericServiceAdder_SSP2}, \code{L244.GenericServiceAdder_SSP3}, \code{L244.GenericServiceAdder_SSP4}, \code{L244.GenericServiceAdder_SSP5}
#' \code{L244.ThermalServiceImpedance}, \code{L244.ThermalServiceAdder}
#' \code{L244.GenericServiceCoef},\code{L244.ThermalServiceCoef}, \code{L244.GenericServiceCoef_SSP1}, \code{L244.GenericServiceCoef_SSP2},
#' \code{L244.GenericServiceCoef_SSP3}, \code{L244.GenericServiceCoef_SSP4}, \code{L244.GenericServiceCoef_SSP5},
#'  \code{L244.ThermalCoalCoef}, \code{L244.GenericCoalCoef},\code{L244.ThermalTradBioCoef}, \code{L244.GenericTradBioCoef},
#' \code{L244.GenericShares}, \code{L244.ThermalShares},\code{L244.GenericServicePrice}, \code{L244.ThermalServicePrice},
#' \code{L244.GenericBaseDens}, \code{L244.ThermalBaseDens},
#' The corresponding file in the original data system was \code{L244.building_det.R} (energy level2).
#' @details Creates level2 data for the building sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete gather nesting unite
#' @author RLH September 2017

module_energy_L244.building_det_cwf <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs_bld_det",
             FILE = "energy/A_regions",
             FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.satiation_flsp",
             FILE = "energy/A44.satiation_flsp_SSPs",
             FILE = "socioeconomics/income_shares",

             FILE = "cwf/A44.subsector_interp_low_fossil",
             FILE = "cwf/A44.subsector_shrwt_low_fossil",
             FILE = "cwf/A44.satiation_flsp_cwf_adj",
             FILE = "cwf/A44.globaltech_shrwt_cwf_H2_scenarios",
             FILE = "cwf/A44.globaltech_shrwt_cwf_no_H2_building",
             FILE = "cwf/A44.res_unadj_sat_cwf_adj",
             "L144.end_use_eff",
             "L144.end_use_eff_cwf",
             'L144.shell_eff_R_Y_cwf',
             'L144.internal_gains_cwf',
             "L244.GompFnParam",
             "L244.Supplysector_bld"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.DeleteSupplySector_cwf",
             "L244.SubsectorShrwt_bld_low_fossil",
             "L244.SubsectorShrwtFllt_bld_low_fossil",
             "L244.SubsectorInterp_bld_low_fossil",
             "L244.SubsectorInterpTo_bld_low_fossil",
             "L244.ShellConductance_bld_cwf",
             "L244.StubTechEff_bld_cwf",
             "L244.StubTechIntGainOutputRatio_cwf",
             "L244.Satiation_flsp_cwf",
             "L244.GompFnParam_cwf",
             "L244.GlobalTechShrwt_bld_cwf_H2_scenarios",
             "L244.globaltech_shrwt_cwf_no_H2_building"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    building.service.input <- calibrated.value <-  comm <- degree.days <- floorspace_bm2 <- gcam.consumer <-
      internal.gains.market.name <- internal.gains.output.ratio <- multiplier <- nodeInput <- pcFlsp_mm2 <-
      pcFlsp_mm2_fby <- pcGDP_thous90USD <- pop_thous <- region <- region.class <- resid <- satiation.adder <-
      satiation.level <- scalar_mult <- sector <- service <- service.per.flsp <- share.weight <- shell.conductance <-
      subs.share.weight <- subsector <- supplysector <- technology <- thermal.building.service.input <- to.value <-
      value <- year <- year.fillout <- GCM <- NEcostPerService <- SRES <- SSP <- TRN_SSP <- base.building.size <-
      area_thouskm2<- flsp <- flsp_pc <- unadjust.satiation <- land.density.param <-
      tot.dens <- b.param <- income.param <- gdp_pc <- flsp_est <- base_flsp <- bias.adjust.param <-
      base.service <- building.node.input <- . <- GCAM_region_ID <- L244.Satiation_flsp_SSP1 <-
      L244.SatiationAdder_SSP1 <- L244.GenericServiceSatiation_SSP1 <- L244.Satiation_flsp_SSP2 <-
      L244.SatiationAdder_SSP2 <- L244.GenericServiceSatiation_SSP2 <- L244.Satiation_flsp_SSP3 <-
      L244.SatiationAdder_SSP3 <- L244.GenericServiceSatiation_SSP3 <-L244.Satiation_flsp_SSP4 <-
      L244.SatiationAdder_SSP4 <- L244.GenericServiceSatiation_SSP4 <- L244.Satiation_flsp_SSP5 <-
      L244.SatiationAdder_SSP5 <- L244.GenericServiceSatiation_SSP5 <-  L244.GenericServiceImpedance_SSP1 <-
      L244.GenericServiceImpedance_SSP2 <-L244.GenericServiceImpedance_SSP3 <- L244.GenericServiceImpedance_SSP4 <-
      L244.GenericServiceImpedance_SSP5 <-L244.GenericServiceAdder_SSP1 <- L244.GenericServiceAdder_SSP2 <-
      L244.GenericServiceAdder_SSP3 <- L244.GenericServiceAdder_SSP4 <- L244.GenericServiceAdder_SSP5 <-
      L244.GenericServiceCoef_SSP1 <- L244.GenericServiceCoef_SSP2 <-L244.GenericServiceCoef_SSP3 <-
      L244.GenericServiceCoef_SSP4 <-L244.GenericServiceCoef_SSP5 <-
      scenario <- L244.HDDCDD_A2_CCSM3x <-
      L244.HDDCDD_A2_HadCM3 <- L244.HDDCDD_B1_CCSM3x <- L244.HDDCDD_B1_HadCM3 <- L244.HDDCDD_constdd_no_GCM <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs_bld_det <- get_data(all_data, "energy/calibrated_techs_bld_det")
    A_regions <- get_data(all_data, "energy/A_regions")
    A44.satiation_flsp <- get_data(all_data,"energy/A44.satiation_flsp")
    A44.satiation_flsp_SSPs <- get_data(all_data,"energy/A44.satiation_flsp_SSPs")
    A44.gcam_consumer <- get_data(all_data,"energy/A44.gcam_consumer")
    income_shares <- get_data(all_data,"socioeconomics/income_shares", strip_attributes = TRUE)

    A44.subsector_interp_low_fossil <- get_data(all_data, "cwf/A44.subsector_interp_low_fossil", strip_attributes = TRUE)
    A44.subsector_shrwt_low_fossil <- get_data(all_data, "cwf/A44.subsector_shrwt_low_fossil", strip_attributes = TRUE)
	  A44.res_unadj_sat_cwf_adj <- get_data(all_data, "cwf/A44.res_unadj_sat_cwf_adj", strip_attributes = TRUE)
    A44.satiation_flsp_cwf_adj <- get_data(all_data, "cwf/A44.satiation_flsp_cwf_adj", strip_attributes = TRUE)
    A44.globaltech_shrwt_cwf_H2_scenarios <- get_data(all_data, "cwf/A44.globaltech_shrwt_cwf_H2_scenarios") %>% gather_years
    A44.globaltech_shrwt_cwf_no_H2_building <- get_data(all_data, "cwf/A44.globaltech_shrwt_cwf_no_H2_building") %>% gather_years
    L144.end_use_eff <- get_data(all_data,"L144.end_use_eff", strip_attributes = TRUE)
    L144.end_use_eff_cwf <- get_data(all_data, "L144.end_use_eff_cwf", strip_attributes = TRUE)
    L144.shell_eff_R_Y_cwf <- get_data(all_data, "L144.shell_eff_R_Y_cwf", strip_attributes = TRUE)
    L144.internal_gains_cwf <- get_data(all_data, "L144.internal_gains_cwf", strip_attributes = TRUE)

    L244.Supplysector_bld <- get_data(all_data,"L244.Supplysector_bld", strip_attributes = TRUE)
    L244.GompFnParam <- get_data(all_data,"L244.GompFnParam", strip_attributes = TRUE)

    # for residential, apply the adjustment factor to the unadjusted satiation values
    A44.res_unadj_sat_cwf_adj_R <- A44.res_unadj_sat_cwf_adj %>%
      repeat_add_columns(tibble(region = GCAM_region_names$region))

    L244.GompFnParam_cwf <- L244.GompFnParam %>%
      left_join(A44.res_unadj_sat_cwf_adj_R, by = c("region")) %>%
      mutate(unadjust.satiation = unadjust.satiation * adj_frac) %>%
      dplyr::select(-adj_frac)


    ## ==================================================================##
    ## Duplicated add.cg function from zenergy_L244.building_det.R file

    L144.income_shares<-income_shares %>%
      filter(model %in% c(socioeconomics.BASE_INCSHARE_BASE,socioeconomics.BASE_INCSHARE_MODEL)) %>%
      select(-gini,-gdp_pcap_decile,-model) %>%
      rename(group = category,
             scen = sce,
             share = shares) %>%
      group_by(GCAM_region_ID,year,scen) %>%
      mutate(share_agg = sum(share)) %>%
      ungroup()

    # Check income shares are correct for all regions
    if((sum(L144.income_shares$share_agg) / nrow(L144.income_shares))-1 > 0.01){
      print("WARNING:income shares not correctly asigned")
    }

    L144.income_shares<-L144.income_shares %>%
      select(-share_agg)

    A44.gcam_consumer<-A44.gcam_consumer %>%
      filter(gcam.consumer == "resid") %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(A44.gcam_consumer %>% filter(gcam.consumer == "comm"))

    A44.gcam_consumer_resid <- A44.gcam_consumer %>%
      filter(grepl("resid", gcam.consumer))

    cons.groups<-unique(A44.gcam_consumer_resid$gcam.consumer)
    n.cons.groups<-as.numeric(length(unique(A44.gcam_consumer_resid$gcam.consumer)))

    add.cg<-function(df){
      df.res<-df %>% filter(grepl("resid",supplysector))
      df.comm<-df %>% filter(grepl("comm",supplysector))

      df<- df.res %>%
        repeat_add_columns(tibble::tibble(cons.groups)) %>%
        separate(cons.groups,c("sector","cons.groups"),sep="_") %>%
        unite(supplysector,c(supplysector,cons.groups), sep="_") %>%
        select(-sector) %>%
        bind_rows(df.comm)
      return(df)
    }

    L244.Tech_bld <- add.cg(L144.end_use_eff) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector, subsector, technology) %>%
      distinct()

    ## ==============

    A44.subsector_interp_low_fossil<-add.cg(A44.subsector_interp_low_fossil)

    A44.subsector_shrwt_low_fossil<-add.cg(A44.subsector_shrwt_low_fossil)

    if(any(!is.na(A44.subsector_shrwt_low_fossil$year))) {
      L244.SubsectorShrwt_bld_low_fossil <- A44.subsector_shrwt_low_fossil %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }
    if(any(!is.na(A44.subsector_shrwt_low_fossil$year.fillout))) {
      L244.SubsectorShrwtFllt_bld_low_fossil <- A44.subsector_shrwt_low_fossil %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }

    if(any(is.na(A44.subsector_interp_low_fossil$to.value))) {
      L244.SubsectorInterp_bld_low_fossil <- A44.subsector_interp_low_fossil %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }
    if(any(!is.na(A44.subsector_interp_low_fossil$to.value))) {
      L244.SubsectorInterpTo_bld_low_fossil <- A44.subsector_interp_low_fossil %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }


    # ===================================================
    # CWF adjustments

    # L244.ShellConductance_bld_cwf: Shell conductance (inverse of shell efficiency)
    L244.ShellConductance_bld_cwf <- L144.shell_eff_R_Y_cwf %>%
      rename(shell.conductance = value) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(shell.conductance = round(shell.conductance, digits = energy.DIGITS_EFFICIENCY)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(supplysector != "resid") %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("supplysector" = "gcam.consumer")) %>%
      mutate(gcam.consumer = supplysector,
             shell.year = year,
             floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO) %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # L244.StubTechEff_bld_cwf: Assumed efficiencies (all years) of buildings technologies
    L244.StubTechEff_bld_cwf <- L144.end_use_eff_cwf %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value) %>%
      # Add region and input
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_bld_det, by = c("supplysector", "subsector", "technology")) %>%
      mutate(stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) %>%
      add.cg()


    # L244.StubTechIntGainOutputRatio_cwf: Output ratios of internal gain energy from non-thermal building services
    L244.StubTechIntGainOutputRatio_cwf <- L144.internal_gains_cwf %>%
      filter(year %in% MODEL_YEARS) %>%
      # Round and rename value
      mutate(value = round(value, energy.DIGITS_EFFICIENCY)) %>%
      rename(internal.gains.output.ratio = value) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Add building.node.input
      left_join_error_no_match(calibrated_techs_bld_det %>%
                                 select(supplysector, building.node.input) %>%
                                 distinct(), by = "supplysector") %>%
      # Add internal.gains.market.name
      left_join_error_no_match(A44.gcam_consumer %>%
                                 select(-gcam.consumer) %>%
                                 #mutate(internal.gains.market.name=paste0(nodeInput,"-internal-gains-trial-market")) %>%
                                 distinct() %>%
                                 mutate(gcam.consumer= if_else(grepl("resid",nodeInput),"resid","comm"))
                               , by = "building.node.input") %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], internal.gains.output.ratio, internal.gains.market.name) %>%
      add.cg()


    # Duplicated from zenergy_L244.building_det.R
    L244.Satiation_flsp_class <- A44.satiation_flsp %>%
      gather(sector, value, resid, comm) %>%
      # Converting from square meters per capita to million square meters per capita
      mutate(satiation.level = value * CONV_THOUS_BIL) %>%
      select(-value)

    L244.Satiation_flsp_class_SSPs <- A44.satiation_flsp_SSPs %>%
      gather(sector, value, resid, comm) %>%
      mutate(satiation.level = value * CONV_THOUS_BIL)

    L244.Satiation_flsp_class_cwf <- L244.Satiation_flsp_class %>%
      # join adjustments
      left_join(A44.satiation_flsp_cwf_adj) %>%
      # join SSP values, which will be used to replace some of the values
      left_join(L244.Satiation_flsp_class_SSPs %>%
                  rename(satiation.level.SSP = satiation.level) %>%
                  dplyr::select(-value),
                by = c("region.class" = "region.class", "sector" = "sector", "match_SSP" = "SSP")) %>%
      # either replace with SSP value or the original value times the adjustment factor
      mutate(satiation.level = case_when(!is.na(match_SSP) ~ satiation.level.SSP,
                                         !is.na(adj_frac) ~ satiation.level * adj_frac)) %>%
      dplyr::select(region.class, sector, satiation.level)


    L244.Satiation_flsp_cwf <- write_to_all_regions(A44.gcam_consumer, c("region", "gcam.consumer", "nodeInput", "building.node.input"), # replace with LEVEL2_DATA_NAMES[["BldNodes]]
                                                GCAM_region_names = GCAM_region_names) %>%
      # Match in the region class, and use this to then match in the satiation floorspace
      left_join_error_no_match(A_regions %>% select(region, region.class),
                               by = "region") %>%
      # Residential floorspace does not use the satiation demand function, so filter the commercial floorspace
      filter(!grepl("resid",gcam.consumer)) %>%
      left_join_error_no_match(L244.Satiation_flsp_class_cwf, by = c("region.class", "gcam.consumer" = "sector")) %>%
      select(LEVEL2_DATA_NAMES[["Satiation_flsp"]])


    # L244.GlobalTechShrwt_bld_cwf_H2_scenarios: Default shareweights for global building technologies for CWF hydrogen scenarios
    L244.GlobalTechShrwt_bld_cwf_H2_scenarios <- A44.globaltech_shrwt_cwf_H2_scenarios %>%
      # Repeat for all model years
      complete(nesting(scenario, supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      # Interpolate
      group_by(scenario, supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], scenario, share.weight)

    L244.globaltech_shrwt_cwf_no_H2_building <-
      A44.globaltech_shrwt_cwf_no_H2_building %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      # Interpolate
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight)

    L244.DeleteSupplySector_cwf <- L244.Supplysector_bld %>%
      filter(str_detect(supplysector,"resid heating") | str_detect(supplysector,"resid others")) %>%
      select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]])


    if(exists("L244.SubsectorShrwt_bld_low_fossil")) {
      L244.SubsectorShrwt_bld_low_fossil %>%
        add_title("Subsector shareweights for building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwt_bld_low_fossil") %>%
        add_precursors("energy/A44.subsector_shrwt_low_fossil", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorShrwt_bld_low_fossil
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwt_bld_low_fossil") ->
        L244.SubsectorShrwt_bld_low_fossil
    }


    if(exists("L244.SubsectorShrwtFllt_bld_low_fossil")) {
      L244.SubsectorShrwtFllt_bld_low_fossil %>%
        add_title("Subsector shareweights for building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld_low_fossil") %>%
        add_precursors("energy/A44.subsector_shrwt_low_fossil", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorShrwtFllt_bld_low_fossil
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld_low_fossil") ->
        L244.SubsectorShrwtFllt_bld_low_fossil
    }


    if(exists("L244.SubsectorInterp_bld_low_fossil")) {
      L244.SubsectorInterp_bld_low_fossil %>%
        add_title("Subsector shareweight interpolation for building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterp_bld") %>%
        add_precursors("energy/A44.subsector_interp_low_fossil", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorInterp_bld_low_fossil
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterp_bld_low_fossil") ->
        L244.SubsectorInterp_bld_low_fossil
    }

    if(exists("L244.SubsectorInterpTo_bld_low_fossil")) {
      L244.SubsectorInterpTo_bld_low_fossil %>%
        add_title("Subsector shareweight interpolation for building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld_low_fossil") %>%
        add_precursors("energy/A44.subsector_interp_low_fossil", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorInterpTo_bld_low_fossil
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterpTo_bld_low_fossil") ->
        L244.SubsectorInterpTo_bld_low_fossil
    }


    L244.ShellConductance_bld_cwf %>%
      add_title("Shell conductance (inverse of shell efficiency)") %>%
      add_units("Unitless") %>%
      add_comments("Shell conductance from L144.shell_eff_R_Y_cwf") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("L144.shell_eff_R_Y_cwf", "common/GCAM_region_names", "energy/A44.gcam_consumer") ->
      L244.ShellConductance_bld_cwf



    L244.StubTechEff_bld_cwf %>%
      add_title("Assumed efficiencies of buildings technologies") %>%
      add_units("Unitless efficiency") %>%
      add_comments("Efficiencies taken from L144.end_use_eff_cwf") %>%
      add_legacy_name("L244.StubTechEff_bld") %>%
      add_precursors("L144.end_use_eff_cwf", "common/GCAM_region_names", "energy/calibrated_techs_bld_det") ->
      L244.StubTechEff_bld_cwf

    L244.StubTechIntGainOutputRatio_cwf %>%
      add_title("Output ratios of internal gain energy from non-thermal building services") %>%
      add_units("Unitless output ratio") %>%
      add_comments("Values from L144.internal_gains_cwf") %>%
      add_legacy_name("L244.StubTechIntGainOutputRatio") %>%
      add_precursors("L144.internal_gains_cwf", "common/GCAM_region_names",
                     "energy/calibrated_techs_bld_det", "energy/A44.gcam_consumer") ->
      L244.StubTechIntGainOutputRatio_cwf

    L244.Satiation_flsp_cwf %>%
      #add_title("Floorspace demand satiation") %>%
      add_units("Million squared meters per capita") %>%
      add_comments("Values from A44.satiation_flsp added to A44.gcam_consumer written to all regions, with CWF adjustments") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("energy/A44.satiation_flsp", "cwf/A44.satiation_flsp_cwf_adj", "energy/A44.gcam_consumer", "common/GCAM_region_names", "energy/A_regions") ->
      L244.Satiation_flsp_cwf

    L244.DeleteSupplySector_cwf %>%
      add_title("Remove coal and TradBio sectors") %>%
      add_precursors("L244.Supplysector_bld") ->
      L244.DeleteSupplySector_cwf


    L244.GompFnParam_cwf %>%
      add_title("Parameters for the floorspace Gompertz function") %>%
      add_units("Unitless") %>%
      add_comments("Computed offline based on data from RECS and IEA with CWF adjustments") %>%
      add_legacy_name("L244.GompFnParam") %>%
      add_precursors("common/GCAM_region_names",
					 "L244.GompFnParam") ->
      L244.GompFnParam_cwf

    L244.GlobalTechShrwt_bld_cwf_H2_scenarios %>%
      add_title("Default shareweights for global building technologies for CWF hydrogen scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated from A44.globaltech_shrwt_cwf_H2_scenarios") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld_cwf_H2_scenarios") %>%
      add_precursors("cwf/A44.globaltech_shrwt_cwf_H2_scenarios") ->
      L244.GlobalTechShrwt_bld_cwf_H2_scenarios

    L244.globaltech_shrwt_cwf_no_H2_building %>%
      add_title("prevent the use of H2 in building sector by setting shareweights to be 0") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated from A44.globaltech_shrwt_cwf_no_H2_building") %>%
      add_legacy_name("L244.globaltech_shrwt_cwf_no_H2_building") %>%
      add_precursors("cwf/A44.globaltech_shrwt_cwf_no_H2_building") ->
      L244.globaltech_shrwt_cwf_no_H2_building

    return_data(
        L244.DeleteSupplySector_cwf,
				L244.SubsectorShrwtFllt_bld_low_fossil,

				L244.SubsectorShrwt_bld_low_fossil,

				L244.SubsectorInterp_bld_low_fossil,

				L244.SubsectorInterpTo_bld_low_fossil,

                L244.ShellConductance_bld_cwf, L244.StubTechEff_bld_cwf, L244.StubTechIntGainOutputRatio_cwf,
                L244.Satiation_flsp_cwf, L244.GompFnParam_cwf, L244.GlobalTechShrwt_bld_cwf_H2_scenarios,
				        L244.globaltech_shrwt_cwf_no_H2_building)
  } else {
    stop("Unknown command")
  }
}
