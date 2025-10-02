# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2233.electricity_mineral
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.Sector_elec_mineral}, \code{L2233.SubsectorLogit_elec_mineral},
#' \code{L2233.SubsectorShrwtFllt_elec_mineral}, \code{L2233.SubsectorInterp_elec_mineral},
#' \code{L2233.SubsectorInterpTo_elec_mineral}, \code{L2233.SubsectorShrwt_elec_mineral},
#' \code{L2233.SubsecShrwt_mineral_other_pv_wind}, \code{L2233.SubsecShrwt_mineral_pv_wind},
#' \code{L2233.StubTechShrwt_mineral_pv_wind}, \code{L2233.StubTechProd_mineral_pv_wind},
#' \code{L2233.StubTechShrwt_mineral_other_pv_wind}, \code{L2233.StubTechShrwt_mineral_pv_wind_future},
#' \code{L2233.StubTechInterpTo_mineral_pv_wind_tech}, \code{L2233.StubTechCapFac_mineral_pv_wind},
#' \code{L2233.Regionaltech_mineral_coef_constance_final}, \code{L2233.Regionaltech_mineral_coef_reduction_final},
#' \code{L2233.Globaltech_mineral_coef_constance_final}, \code{L2233.Globaltech_mineral_coef_reduction_final},
#' \code{L2233.Regional_Globaltech_mineral_coef_constance_Yb},\code{L2233.Regional_Globaltech_mineral_coef_reduction_Yb},
#' \code{L2233.Regionaltech_mineral_PMult}, \code{L2233.Globaltech_mineral_PMult}, \code{L2233.Regional_Globaltech_mineral_Yb_PMult},
#' \code{L2233.GlobalTechCapital_elec_subtype}, \code{L2233.StubTechCapFac_mineral_pv_wind},
#' \code{L2233.Regionaltech_mineral_coef_constance_final}, \code{L2233.GlobalTechCapital_elecPassthru_no_pv_wind},
#' \code{L2233.GlobalIntTechMineral_elecSupplySector}, \code{L2233.GlobalTechMineral_elecSupplySector},
#' \code{L2233.GlobalTechLifetimeMineral_elec}, \code{L2233.GlobalIntTechLifetimeMineral_elec},
#' \code{L2233.GlobalIntTechLifetime_CSP}, \code{L2233.GlobalTechLifetime_elec_cool_no_pv_wind},
#' @author YQ 2023
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
module_energy_L2233.electricity_mineral <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "minerals/electricity/elec_tech_mineral_map",
             FILE = "water/elec_tech_water_map",
             FILE = "common/GCAM_region_names",
             FILE = "minerals/supply/A10.mineral_rsrc_info",
             FILE = "energy/A23.globalinttech",
             FILE = "energy/calibrated_techs",
             FILE = "minerals/electricity/A23.sector_mineral",
             FILE = "minerals/electricity/A23.subsector_logit_mineral",
             FILE = "minerals/electricity/A23.globaltech_mineral_coef_kg_kw",
             FILE = "minerals/electricity/A23.globaltech_mineral_coef_ratio_constance",
             FILE = "minerals/electricity/A23.globaltech_mineral_coef_ratio_reduction",
             FILE = "minerals/electricity/A23.globaltech_storage_mineral_coef_kg_kwh",
             FILE = "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_constance",
             FILE = "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_reduction",
             FILE = "minerals/electricity/A23.globaltech_subtype_capital",
             FILE = "minerals/electricity/A23.globaltech_subtype_calibration",
             FILE = "minerals/electricity/A23.globaltech_subtype_shrwt",
             FILE = "minerals/electricity/A23.globaltech_subtype_interp_to",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.SubsectorShrwt_renew",
             "L223.GlobalIntTechLifetime_elec",
             "L223.GlobalTechLifetime_elec",
             "L2233.GlobalTechCapital_elecPassthru",
             "L2233.GlobalIntTechLifetime_elec_cool",
             "L2233.GlobalTechLifetime_elec_cool",
             "L223.StubTechCapFactor_elec",
             #"L2233.GlobalIntTechCapFac_elec_cool",
             "L2233.GlobalTechCapFac_elec_cool",
             "L2233.StubTechCapFactor_elec_cool",
             "L2233.StubTechProd_elecPassthru",
             "L2233.StubTechProd_elec_cool"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2233.Sector_elec_mineral",
             "L2233.PassThruSector_elec_mineral",
             "L2233.SubsectorLogit_elec_mineral",
             "L2233.SubsectorShrwtFllt_elec_mineral",
             "L2233.SubsectorInterp_elec_mineral",
             "L2233.SubsectorInterpTo_elec_mineral",
             "L2233.SubsectorShrwt_elec_mineral",
             "L2233.SubsecShrwt_mineral_other_pv_wind",
             "L2233.SubsecShrwt_mineral_pv_wind",
             "L2233.StubTechShrwt_mineral_pv_wind",
             "L2233.StubTechProd_mineral_pv_wind",
             "L2233.StubTechShrwt_mineral_other_pv_wind",

             "L2233.StubTechShrwt_mineral_pv_wind_future",
             "L2233.StubTechInterpTo_mineral_pv_wind_tech",
             "L2233.StubTechCapFac_mineral_pv_wind",
             "L2233.Regionaltech_mineral_coef_constance_final",
             "L2233.Globaltech_mineral_coef_constance_final",
             "L2233.Regional_Globaltech_mineral_coef_constance_Yb",
             "L2233.Regionaltech_mineral_PMult",
             "L2233.Globaltech_mineral_PMult",
             "L2233.Regional_Globaltech_mineral_Yb_PMult",
             # "L2233.GlobalTechCapital_elec_subtype",
             "L2233.GlobalTechCapital_elec_subtype_pv_wind",
             "L2233.GlobalTechCapital_elec_subtype_pv_wind_storage",
             "L2233.GlobalTechCapital_elecPassthru_no_pv_wind",
             "L2233.GlobalIntTechMineral_elecSupplySector",
             "L2233.GlobalTechMineral_elecSupplySector",
             "L2233.GlobalTechLifetimeMineral_elec",
             "L2233.GlobalIntTechLifetimeMineral_elec",
             "L2233.GlobalIntTechLifetime_CSP",
             "L2233.GlobalTechLifetime_elec_cool_no_pv_wind"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    ## ===================================================================
    ## Section 1 -- Load data
    ## ===================================================================

    # Basic dataset input
    elec_tech_mineral_map <- get_data(all_data, "minerals/electricity/elec_tech_mineral_map",strip_attributes = TRUE)
    elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map",strip_attributes = TRUE)
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A10.mineral_rsrc_info <- get_data(all_data, "minerals/supply/A10.mineral_rsrc_info", strip_attributes = TRUE)
    A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech", strip_attributes = TRUE)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)

    # Mineral new supplysector info
    A23.sector_mineral <- get_data(all_data, "minerals/electricity/A23.sector_mineral", strip_attributes = TRUE)
    A23.subsector_logit_mineral <- get_data(all_data, "minerals/electricity/A23.subsector_logit_mineral", strip_attributes = TRUE)
    A23.globaltech_mineral_coef_kg_kw <- get_data(all_data, "minerals/electricity/A23.globaltech_mineral_coef_kg_kw", strip_attributes = TRUE)
    A23.globaltech_mineral_coef_ratio_constance <- get_data(all_data, "minerals/electricity/A23.globaltech_mineral_coef_ratio_constance", strip_attributes = TRUE)
    A23.globaltech_mineral_coef_ratio_reduction <- get_data(all_data, "minerals/electricity/A23.globaltech_mineral_coef_ratio_reduction", strip_attributes = TRUE)
    A23.globaltech_storage_mineral_coef_kg_kwh <- get_data(all_data, "minerals/electricity/A23.globaltech_storage_mineral_coef_kg_kwh", strip_attributes = TRUE)
    A23.globaltech_storage_mineral_coef_ratio_constance <- get_data(all_data, "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_constance", strip_attributes = TRUE)
    A23.globaltech_storage_mineral_coef_ratio_reduction <- get_data(all_data, "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_reduction", strip_attributes = TRUE)
    A23.globaltech_subtype_capital <- get_data(all_data, "minerals/electricity/A23.globaltech_subtype_capital", strip_attributes = TRUE)
    A23.globaltech_subtype_calibration <- get_data(all_data, "minerals/electricity/A23.globaltech_subtype_calibration", strip_attributes = TRUE)
    A23.globaltech_subtype_shrwt <- get_data(all_data, "minerals/electricity/A23.globaltech_subtype_shrwt", strip_attributes = TRUE)
    A23.globaltech_subtype_interp_to <- get_data(all_data, "minerals/electricity/A23.globaltech_subtype_interp_to", strip_attributes = TRUE)

    # Datasets from previous chunks will be used in this chunk
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh", strip_attributes = TRUE)
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec", strip_attributes = TRUE)
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec", strip_attributes = TRUE)
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec", strip_attributes = TRUE)
    L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew", strip_attributes = TRUE)
    L223.GlobalIntTechLifetime_elec <- get_data(all_data, "L223.GlobalIntTechLifetime_elec", strip_attributes = TRUE)
    L223.GlobalTechLifetime_elec <- get_data(all_data, "L223.GlobalTechLifetime_elec", strip_attributes = TRUE)
    L2233.GlobalTechCapital_elecPassthru <- get_data(all_data, "L2233.GlobalTechCapital_elecPassthru", strip_attributes = TRUE)
    L2233.GlobalIntTechLifetime_elec_cool <- get_data(all_data, "L2233.GlobalIntTechLifetime_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechLifetime_elec_cool <- get_data(all_data, "L2233.GlobalTechLifetime_elec_cool", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)
    #L2233.GlobalIntTechCapFac_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCapFac_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechCapFac_elec_cool <- get_data(all_data, "L2233.GlobalTechCapFac_elec_cool", strip_attributes = TRUE)
    L2233.StubTechCapFactor_elec_cool <- get_data(all_data, "L2233.StubTechCapFactor_elec_cool", strip_attributes = TRUE)

    L2233.StubTechProd_elecPassthru <- get_data(all_data, "L2233.StubTechProd_elecPassthru", strip_attributes = TRUE)
    L2233.StubTechProd_elec_cool <- get_data(all_data,  "L2233.StubTechProd_elec_cool", strip_attributes = TRUE)

    ## ===================================================================
    ## Section 2 -- Process data
    ## ===================================================================

    ##  2.1. Creating regional database structure, the structure need to be created for new supply sectors, which includes pv_mineral,
    #   pv_storage_mineral, rooftop_pv_mineral, wind_mineral, wind_offshore_mineral, and wind_storage_mineral.


    #   2.1.1. Regional database - (new) supplysector
    #----------------------------------------------------------------------------
    #   Compile information about output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type at the supply sector level,
    #   The information remains the same as defined in existing supply sector of pv, wind technologies.
    L2233.Sector_elec_mineral <-
      write_to_all_regions(A23.sector_mineral, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names)
    # --OUTPUT--

    #   use passthrough sector for solar and wind sectors
    L2233.PassThruSector_elec_mineral <-
      L2233.Sector_elec_mineral %>%
      select (region, pass.through.sector = supplysector) %>%
      mutate(marginal.revenue.sector = "electricity",
             marginal.revenue.market = region) %>%
      select(LEVEL2_DATA_NAMES[["PassThroughSector"]])
    # --OUTPUT--
    #   2.1.2. Regional database - subsector (of new supply sector) ----
    #----------------------------------------------------------------------------
    #   Compile information about logit.year.fillout, logit.exponent, and logit.type at the subsector level,
    #   The information remains the same as defined in existing supplysector of pv, wind technologies.
    L2233.SubsectorLogit_elec_mineral <-
      write_to_all_regions(A23.subsector_logit_mineral, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names)
    # --OUTPUT--

    #   Compile information about share.weight, and year.fillout, interpolation rules information at the subsector level,
    #   The information remains the same as defined in existing supplysector of pv, wind technologies.
    L2233.SubsectorShrwtFllt_elec_mineral <-
      L223.SubsectorShrwtFllt_elec %>%
      filter(subsector %in% c("wind", "solar", "rooftop_pv")) %>%
      left_join(elec_tech_mineral_map %>%
                  select(supplysector = from.supplysector, subsector = from.subsector, to.supplysector, to.subsector) %>%
                  unique(),
                by = c("supplysector", "subsector"),
                relationship = "many-to-many") %>%
      select(region, supplysector = to.supplysector, subsector = to.subsector, year.fillout, share.weight)
    # --OUTPUT--

    #   share weight interpolation rule
    L2233.SubsectorInterp_elec_mineral <-
      L223.SubsectorInterp_elec %>%
      filter(subsector %in% c("wind", "solar", "rooftop_pv")) %>%
      left_join(elec_tech_mineral_map %>%
                  select(supplysector = from.supplysector, subsector = from.subsector, to.supplysector, to.subsector),
                by = c("supplysector", "subsector"),
                relationship = "many-to-many") %>%
      select(region, supplysector = to.supplysector, subsector = to.subsector, apply.to, from.year, to.year, interpolation.function) %>%
      unique()
    # --OUTPUT--

    # share weight interpolation rule for rooftop_pv tech only which has the to.value information.
    L2233.SubsectorInterpTo_elec_mineral <-
      L223.SubsectorInterpTo_elec %>% filter(subsector %in% c("wind", "solar", "rooftop_pv")) %>%
      left_join(elec_tech_mineral_map %>%
                  select(supplysector = from.supplysector, subsector = from.subsector, to.supplysector, to.subsector),
                by = c("supplysector", "subsector"),
                relationship = "many-to-many") %>%
      select(region, supplysector = to.supplysector, subsector = to.subsector, apply.to, from.year, to.year, to.value, interpolation.function) %>%
      unique()
    # --OUTPUT--

    # 2100 share weight information
    L2233.SubsectorShrwt_elec_mineral <-
      L223.SubsectorShrwt_renew %>% filter(subsector %in% c("wind", "solar", "rooftop_pv")) %>%
      left_join(elec_tech_mineral_map %>%
                  select(supplysector = from.supplysector, subsector = from.subsector, to.supplysector, to.subsector),
                by = c("supplysector", "subsector"),
                relationship = "many-to-many") %>%
      select(region, supplysector = to.supplysector, subsector = to.subsector, year, share.weight) %>%
      #filter(!supplysector %in% c("mineral_csp_supsec", "mineral_csp_storage_supsec")) %>%
      unique()
    # --OUTPUT--


    #   2.1.3. Regional database - technology
    #----------------------------------------------------------------------------
    # Collect the historical generation calibration numbers at the technology level
    StubTechProd_elecPassthru_mineral <-
      L1231.out_EJ_R_elec_F_tech_Yh %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs,
                                      - minicam.energy.input,
                                      -calibration, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology, calOutputValue = value) %>%
      mutate(share.weight.year = year)

    # Here we create a dataset that includes the historical (1975-2015) calibration numbers and shareweight for pv and wind subtype technologies
    # (no storage). The shareweight are calculated based on calibration numbers. We only get the calibration numbers for main technologies (e.g. pv, wind),
    # which is evenly distributed to the subtypes of pv and wind (subtype calibration number = main technology calibration number/number of subtypes), but
    # this will change once we get the real subtype calibration numbers.

    L2233.StubTechProd_mineral_pv_wind <-
      StubTechProd_elecPassthru_mineral %>%
      group_by(region, sector, subsector, year) %>%
      summarise(subs.share.weight = sum(calOutputValue)) %>%
      #mutate(calOutputinput = if_else(subs.share.weight == 0, "TRUE", "FALSE"))
      ungroup %>%
      left_join(StubTechProd_elecPassthru_mineral, by = c("region", "sector", "subsector", "year")) %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
      right_join(elec_tech_mineral_map,
                 by = c("supplysector" = "from.supplysector", "subsector" = "from.subsector", "stub.technology" = "from.technology"),
                 relationship = "many-to-many") %>%
      # Here we remove pv_storage, rooftop_pv, offshore_wind, and wind_storage, which does not have historical calibration values.
      filter(to.supplysector %in% c("pv_mineral", "wind_mineral")) %>%
      left_join(A23.globaltech_subtype_calibration %>%
                   gather_years(),
                by = c("to.supplysector" = "supplysector", "to.subsector" = "subsector", "to.technology" = "technology", "year")) %>%
      #group_by(region, year, to.supplysector, to.subsector) %>%
      mutate(calOutputValue_new = calOutputValue * value) %>%
      select(region, supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology, year, calOutputValue = calOutputValue_new,  share.weight.year, subs.share.weight, tech.share.weight) %>%
      ungroup() %>%

      # Add this mutate function and make all subsector and technology share-weights to be 1
      mutate(calOutputValue = if_else(subs.share.weight == 0, NA_real_, calOutputValue),
             subs.share.weight = 1,
             tech.share.weight = 1)


    L2233.SubsecShrwt_mineral_pv_wind <-
      L2233.StubTechProd_mineral_pv_wind %>%
      select(region, supplysector, subsector, year, share.weight = subs.share.weight) %>%
      unique()
    # --OUTPUT--

    L2233.StubTechShrwt_mineral_pv_wind <-
      L2233.StubTechProd_mineral_pv_wind %>%
      select(region, supplysector, subsector, stub.technology, year, share.weight = tech.share.weight) %>%
      unique()
    # --OUTPUT--

    L2233.StubTechProd_mineral_pv_wind <-
      L2233.StubTechProd_mineral_pv_wind %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue) %>%
      filter(!is.na(calOutputValue)) %>%
      unique()
    # --OUTPUT--

    # the historical (1975-2015) calibration numbers and shareweight for other pv and wind (rooftop_pv, offshore wind, pv and wind with storage)
    # subtype technologies (These are more advanced technologies, so their historical numbers are set to be 0)
    L2233.StubTechShrwt_mineral_other_pv_wind_raw <-
      elec_tech_mineral_map %>%
      filter(from.technology %in% c("PV_storage", "wind_offshore", "wind_storage", "rooftop_pv")) %>%
      select(supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology) %>%
      mutate(calOutputValue = 0,
             subs.share.weight = 1,
             tech.share.weight = 1) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "stub.technology", "year", "calOutputValue", "year", "subs.share.weight", "tech.share.weight"), GCAM_region_names) %>%
      rename("share.weight.year" = "year")


    L2233.SubsecShrwt_mineral_other_pv_wind <-
      L2233.StubTechShrwt_mineral_other_pv_wind_raw %>%
      select(region, supplysector, subsector, year, share.weight = subs.share.weight) %>%
      unique()

    # --OUTPUT--

    L2233.StubTechShrwt_mineral_other_pv_wind <-
      L2233.StubTechShrwt_mineral_other_pv_wind_raw %>%
      select(region, supplysector, subsector, stub.technology, year, share.weight = tech.share.weight) %>%
      unique()

    # --OUTPUT--



    # Technology level shareweights and interpolation rules after base tear (future until 2100)
    L2233.StubTech_mineral_pv_wind_tech <-
      L2233.StubTechShrwt_mineral_pv_wind %>%
      rbind(L2233.StubTechShrwt_mineral_other_pv_wind) %>%
      ungroup() %>%
      select(region, supplysector, subsector, stub.technology) %>%
      unique()

    # L2233.StubTechShrwt_mineral_pv_wind_2100 <-
    #   L2233.StubTech_mineral_pv_wind_tech %>%
    #   mutate(year = 2100,
    #          share.weight = 1)

    L2233.StubTechShrwt_mineral_pv_wind_future <-
      A23.globaltech_subtype_shrwt %>%
      gather_years() %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "stub.technology", "year", "value"),
                           GCAM_region_names = GCAM_region_names) %>%
      rename(share.weight = value)
    # --OUTPUT--

    L2233.StubTechInterpTo_mineral_pv_wind_tech <-
      A23.globaltech_subtype_interp_to %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "stub.technology", "apply.to", "from.year", "to.year", "to.value", "interpolation.function"),
                           GCAM_region_names = GCAM_region_names)
    # --OUTPUT--


    #------------------------------------------------------------------------------------------------------------------


    ##  2.2 process the mineral use intensity data

    #   2.2.1 This step is expand the mineral intensity data to all the model year (1975 -- 2100), The unit is kg/kw.
    #   For generation technologies, here all the numbers are based on kg/kW, assuming these intensity numbers are based on base-year, and remain constant
    #   over time. We can also consider the change of mineral intensity overtime in different scenarios if needed, but in the default model version, we just
    #   consider constant mineral intensity over time.
    L2233.globaltech_mineral_coef <-
      A23.globaltech_mineral_coef_kg_kw %>%
      gather(key = minicam.energy.input, value = value, 4:last_col()) %>%
      filter(!is.na(value)) %>%
      repeat_add_columns(tibble::tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))

    #   Same data processing for storage technologies, here all the numbers are based on kg/kWh
    L2233.globaltech_storage_mineral_coef <-
      A23.globaltech_storage_mineral_coef_kg_kwh %>%
      gather(key = minicam.energy.input, value = value, 4:last_col()) %>%
      filter(!is.na(value)) %>%
      repeat_add_columns(tibble::tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))


    # Group 1: for pv, pv_storage, wind, offshore_wind, wind_storage, rooftop_pv (only generation technology, no storage).
    # This group will be processed using regional capacity factor, and the output will be added to regional database
    #------------------------------------------------------------------------------------------------------------------

    L2233.RegionalTechCapFac_pv_wind <-
      L223.StubTechCapFactor_elec %>%
      filter(!stub.technology %in% c("CSP", "CSP_storage")) %>%
      left_join(elec_tech_mineral_map,
                by = c("supplysector" = "from.supplysector", "subsector" = "from.subsector", "stub.technology" = "from.technology"),
                relationship = "many-to-many")

    L2233.StubTechCapFac_mineral_pv_wind <-
      L2233.RegionalTechCapFac_pv_wind %>%
      select(region, supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology, year, capacity.factor)
    # --OUTPUT--


    L2233.Regionaltech_mineral_coef_pv_wind_Mt_EJ <-
      L2233.globaltech_mineral_coef %>%
      right_join(L2233.RegionalTechCapFac_pv_wind,
                 by = c("supplysector" = "to.supplysector", "subsector" = "to.subsector", "technology" = "to.technology", "year"),
                 relationship = "many-to-many") %>%
      select(region, supplysector, subsector, technology, year, capacity.factor, minicam.energy.input, value) %>%
      mutate(mineral_intensity_Mt_kW = value * CONV_KG_T * CONV_T_MT,
             mineral_intensity_Mt_kWh = mineral_intensity_Mt_kW/(8760 * capacity.factor),
             mineral_intensity_Mt_EJ = mineral_intensity_Mt_kWh / (CONV_KWH_GJ * CONV_GJ_EJ)) %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, year, value = mineral_intensity_Mt_EJ) %>%
      mutate(model.year = year)

    # adjust the mineral intensity for rooftop_pv, this is because rooftop PV is assumed to have 5 year life time in GCAM, but actually the lifefime
    # should be longer. Because of this, each year, the total generation from rooftop-pv is equal to the new vintage, which is too high for the mineral
    # calculation. Here we assume the annual rooftop_pv generation addition to be the adjusted new vintage, and calculated the ratio and multiply it
    # to the original mineral intensity of rooftop pv to account for that adjusted new vintage (as compared to the original new vintage under 5 year lifetime)
    # Note: this factor may change in the future.

    L2233.Regionaltech_mineral_coef_pv_wind_Mt_EJ <-
      L2233.Regionaltech_mineral_coef_pv_wind_Mt_EJ %>%
      filter(subsector %in% c("rooftop_pv_mineral")) %>%
      mutate(value = value * 0.135) %>%
      rbind(L2233.Regionaltech_mineral_coef_pv_wind_Mt_EJ %>%
              filter(!subsector %in% c("rooftop_pv_mineral")))


    # Group 2: for pv_storage, wind_storage (only storage technologies).
    # This group will be processed assuming battery storage charge and discharge once per day (365 times per year),
    # and the output will be added to regional database
    #------------------------------------------------------------------------------------------------------------------

    L2233.Regionaltech_mineral_coef_battery_Mt_EJ <-
      L2233.globaltech_storage_mineral_coef %>%
      # add the regions for the storage mineral intensity data
      right_join(L2233.Regionaltech_mineral_coef_pv_wind_Mt_EJ %>%
                   select(region, supplysector, subsector, technology) %>%
                   unique() %>%
                   filter(supplysector %in% c("pv_storage_mineral", "wind_storage_mineral")),
                 by = c("supplysector", "subsector", "technology"),
                 relationship = "many-to-many") %>%
      mutate(mineral_intensity_Mt_kWh = value * CONV_KG_T * CONV_T_MT,
             mineral_intensity_MT_kWh_day = mineral_intensity_Mt_kWh/365, ## assuming one charge and discharge per day
             mineral_intensity_Mt_EJ = mineral_intensity_MT_kWh_day / (CONV_KWH_GJ * CONV_GJ_EJ)) %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, year, value = mineral_intensity_Mt_EJ) %>%
      mutate(model.year = year)

    # consider the battery replacement at the 15 year, so we need to input non-zero mineral intensity value when model.year = model.year + 15,
    # with the exception for 2005 and 2021, the replacement year will occur at 2021, and 2035 respectively, to align with the gcam years.
    L2233.Regionaltech_mineral_coef_battery_Mt_EJ_replacement <-
      L2233.Regionaltech_mineral_coef_battery_Mt_EJ %>%
      mutate(model.year = if_else(year == 2005, year + 16, year + 15),
             model.year = if_else(year == 2021, year + 14, model.year)) %>%
      # mutate(model.year = model.year + 15) %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, year, model.year) %>%
      left_join(L2233.Regionaltech_mineral_coef_battery_Mt_EJ,
                by = c("region", "supplysector", "subsector", "technology", "minicam.energy.input", "model.year" = "year"),
                relationship = "many-to-many") %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, year, model.year, value) %>%
      filter(model.year <= 2100)


    # Group 3: for CSP and CSP_storage (with cooling).
    # This group will be processed based on regional csp capacity factor, and the output will be added to regional database
    #------------------------------------------------------------------------------------------------------------------

    L2233.RegionalTechCapFactor_csp_cool <-
      L2233.StubTechCapFactor_elec_cool %>%
      left_join(elec_tech_water_map %>%
                  select(from.supplysector, from.subsector, from.technology, to.supplysector, to.subsector, to.technology),
                by = c("supplysector" = "to.supplysector", "subsector" = "to.subsector", "stub.technology" = "to.technology"))

    L2233.Regionaltech_mineral_coef_csp_cool_Mt_EJ <-
      L2233.globaltech_mineral_coef %>%
      right_join(L2233.RegionalTechCapFactor_csp_cool,
                 by = c("supplysector" = "from.supplysector", "subsector" = "from.subsector", "technology" = "from.technology", "year"),
                 relationship = "many-to-many") %>%
      select(region, supplysector = supplysector.y, subsector = subsector.y, technology = stub.technology, year, capacity.factor, minicam.energy.input, value) %>%
      mutate(mineral_intensity_Mt_kW = value * CONV_KG_T * CONV_T_MT,
             mineral_intensity_Mt_kWh = mineral_intensity_Mt_kW/(8760 * capacity.factor),
             mineral_intensity_Mt_EJ = mineral_intensity_Mt_kWh / (CONV_KWH_GJ * CONV_GJ_EJ)) %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, year, value = mineral_intensity_Mt_EJ) %>%
      mutate(model.year = year)


    # Group 4: All other technologies including coal, gas, refined liquids, biomass, nuclear, geothermal, and add hydro.
    # This group will be processed based on average capacity factor, and the output will be added to global database
    #------------------------------------------------------------------------------------------------------------------

    L2233.GlobalTechCapFac_elec_cool_all <-
      L2233.GlobalTechCapFac_elec_cool %>%
      # get rid of all the RE with storage, these technologies will be considered in the regional database
      filter(!grepl("storage", technology)) %>%
      # filter(!technology %in% c("wind_storage", "PV_storage", "CSP_storage (recirculating)", "CSP_storage (dry_hybrid)")) %>%
      rbind(L2233.GlobalTechCapFac_elec_cool%>%
              filter(technology == "coal (conv pul) (once through)") %>%
              mutate(sector.name = "electricity",
                     subsector.name = "hydro",
                     technology = "hydro",
                     capacity.factor = 0.44)) %>%
      left_join(elec_tech_water_map %>% select(from.supplysector, from.subsector, from.technology, to.supplysector, to.subsector, to.technology),
                by = c("sector.name" = "to.supplysector", "subsector.name" = "to.subsector", "technology" = "to.technology"))

    L2233.globaltech_mineral_coef_Mt_EJ <-
      L2233.globaltech_mineral_coef %>%
      right_join(L2233.GlobalTechCapFac_elec_cool_all,
                 by = c("supplysector" = "from.supplysector", "subsector" = "from.subsector", "technology" = "from.technology", "year"),
                 relationship = "many-to-many") %>%
      select(supplysector = sector.name, subsector = subsector.name, technology = technology.y, year, capacity.factor, minicam.energy.input, value) %>%
      mutate(mineral_intensity_Mt_kW = value * CONV_KG_T * CONV_T_MT,
             mineral_intensity_Mt_kWh = mineral_intensity_Mt_kW/(8760 * capacity.factor),
             mineral_intensity_Mt_EJ = mineral_intensity_Mt_kWh / (CONV_KWH_GJ * CONV_GJ_EJ)) %>%
      select(supplysector, subsector, technology, minicam.energy.input, year, value = mineral_intensity_Mt_EJ) %>%
      mutate(model.year = year)

    # adjust for hydro -- same idea as the rooftop pv (again this factor may need to be adjusted later.)
    L2233.globaltech_mineral_coef_Mt_EJ <-
      L2233.globaltech_mineral_coef_Mt_EJ %>%
      filter(subsector %in% c("hydro")) %>%
      mutate(value = value * 0.025) %>%
      rbind(L2233.globaltech_mineral_coef_Mt_EJ %>%
              filter(!subsector %in% c("hydro")))


    ##  2.2.3 Combine processed mineral intensity data and create vintage information
    #------------------------------------------------------------------------------------------------------------------

    # Mineral intensity data of technologies being included in the regional database (sum the intensity of renewable and its storage)
    L2233.Regionaltech_mineral_coef_Mt_EJ_combine <-
      L2233.Regionaltech_mineral_coef_pv_wind_Mt_EJ %>%
      rbind(L2233.Regionaltech_mineral_coef_csp_cool_Mt_EJ) %>%
      rbind(L2233.Regionaltech_mineral_coef_battery_Mt_EJ) %>%
      rbind(L2233.Regionaltech_mineral_coef_battery_Mt_EJ_replacement) %>%
      group_by(region, supplysector, subsector, technology, minicam.energy.input, year, model.year) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup

    #curr-coef being written for all years
    L2233.Regionaltech_mineral_coef_all <-
      L2233.Regionaltech_mineral_coef_Mt_EJ_combine %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, year) %>%
      unique() %>%
      repeat_add_columns(tibble(model.year = MODEL_YEARS)) %>%
      left_join(L2233.Regionaltech_mineral_coef_Mt_EJ_combine,
                by = c("region", "supplysector", "subsector", "technology", "minicam.energy.input", "year", "model.year")) %>%
      mutate(value = if_else(is.na(value), 0, value))

    L2233.Regionaltech_mineral_coef <-
      L2233.Regionaltech_mineral_coef_all %>%
      select(region, supplysector, subsector, stub.technology = technology, year, minicam.energy.input, model.year, current.coef = value) %>%
      unique() %>% # --OUTPUT-- unit based on Mt/EJ
      select(LEVEL2_DATA_NAMES[["RegionalStubTechMineralCurCoefAllYr"]])# --OUTPUT-- unit based on Mt/EJ

    #curr-coef being written for all years
    ## Mineral intensity data of technologies being included in the global database
    L2233.Globaltech_mineral_coef <-
      L2233.globaltech_mineral_coef_Mt_EJ %>%
      select(supplysector, subsector, technology, minicam.energy.input, year) %>%
      unique() %>%
      repeat_add_columns(tibble(model.year = MODEL_YEARS)) %>%
      left_join(L2233.globaltech_mineral_coef_Mt_EJ,
                by = c("supplysector", "subsector", "technology", "minicam.energy.input", "year", "model.year")) %>%
      mutate(value = if_else(is.na(value), 0, value)) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.energy.input, model.year, current.coef = value) %>%
      unique() %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechMineralCurCoefAllYr"]])
    # --OUTPUT-- unit based on Mt/EJ

    # End of mineral intensity data processing
    #------------------------------------------------------------------------------------------------------------------


    ##  2.3 process the mineral price and mineral costs

    #  2.3.1 Calculate the mineral costs based on the mineral price information
    #  collect mineral price data and expand it to all model years (assuming mineral price remain constant for now)
    A10.mineral_price <-
      A10.mineral_rsrc_info %>%
      gather_years %>%
      select(resource, price_unit = `price-unit`, year, value) %>%
      complete(nesting(resource,  price_unit), year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(resource,  price_unit, year) %>%
      group_by(resource, price_unit) %>%
      mutate(value = approx_fun(year, value, rule = 2),
             mineral_price = round(value, energy.DIGITS_COST)) %>%
      select(resource, price_unit, year, mineral_price)


    #  Calculate the mineral cost for generation technologies (here I am using the mineral intensity data based on kg/kw)
    L2233.globaltech_mineral_cost <-
      L2233.globaltech_mineral_coef %>%
      left_join(A10.mineral_price, by = c("minicam.energy.input" = "resource", "year")) %>%
      mutate(mineral_cost = value * mineral_price) %>%
      select(supplysector, subsector, technology, minicam.energy.input, year, mineral_cost)


    #  Calculate the mineral cost for battery storage technologies (here I am using the mineral intensity data based on kg/kwh for storage)
    L2233.globaltech_storage_mineral_cost <-
      L2233.globaltech_storage_mineral_coef %>%
      left_join(A10.mineral_price, by = c("minicam.energy.input" = "resource", "year")) %>%
      mutate(mineral_cost_kwh = value * mineral_price, # After this step, the unit becomes $/kWh ($/kg * kg/kWh)
             # assuming 8 hour battery storage duration (here the unit becomes, $/kw = $/kWh * 8h)
             mineral_cost_8kwh = mineral_cost_kwh * 8,
             # considering replacement in 15th year
             mineral_cost_8kwh_repla = mineral_cost_8kwh * 2) %>%
      select(supplysector, subsector, technology, minicam.energy.input, year, mineral_cost = mineral_cost_8kwh_repla)


    #  Combine mineral cost data
    L2233.globaltech_mineral_cost_combine <-
      L2233.globaltech_mineral_cost %>%
      rbind(L2233.globaltech_storage_mineral_cost) %>%
      group_by(supplysector, subsector, technology, year) %>%
      summarize(mineral_cost = sum(mineral_cost, na.rm = TRUE))

    # L2233.globaltech_mineral_cost_combine_w_mineral <-
    #   L2233.globaltech_mineral_cost %>%
    #   rbind(L2233.globaltech_storage_mineral_cost) %>%
    #   group_by(supplysector, subsector, technology, minicam.energy.input, year) %>%
    #   summarize(mineral_cost = sum(mineral_cost, na.rm = TRUE))

    #  2.3.2 Subtract the mineral cost from the capital cost to calculate the non-mineral capital cost, this data will be used to replace the original
    #  capital cost in the global database. The unit remains as 1975$US/kW

    #  for technologies and with subtype technologies
    L2233.GlobalTechCapital_elec_subtype <-
      A23.globaltech_subtype_capital %>%
      gather_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector,
             capital.overnight.all = value, input.capital = `input-capital`) %>%
      left_join(L2233.globaltech_mineral_cost_combine %>%
                  filter(year %in% c(MODEL_BASE_YEARS)) %>%
                  mutate(year = if_else(year == 1975, 1971, year)),
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology", "year")) %>%
      # only substract mineral cost for the early year, and the capital cost (no mineral cost) component will follow the cost reduction projection.
      # This approach is to ensure that future non-mineral capital cost will not go negative as the capital cost decrease over time (while mineral
      # cost remain unchanged.)
      mutate(capital.overnight = capital.overnight.all - mineral_cost) %>%
      select(-capital.overnight.all, - mineral_cost) %>%
      spread(key = "year", value = "capital.overnight") %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(capital.overnight = value) %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))

    L2233.GlobalTechCapital_elec_subtype_pv_wind_storage <-
      L2233.GlobalTechCapital_elec_subtype %>%
      filter(subsector.name %in% c("pv_storage_mineral", "wind_storage_mineral")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]])
    #--OUTPUT--

    L2233.GlobalTechCapital_elec_subtype_pv_wind <-
      L2233.GlobalTechCapital_elec_subtype%>%
      filter(!subsector.name %in% c("pv_storage_mineral", "wind_storage_mineral")) %>%
      rename(intermittent.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechCapital"]])
    #--OUTPUT--


    # # This dataset is only calculated for diagnose purpose, the goal is to know the percentage of mineral cost within technology capital cost.
    # L2233.pv_wind_storage_subtype_mineral_capital_ratio <-
    #   A23.globaltech_subtype_capital %>%
    #   gather_years() %>%
    #   rename(sector.name = supplysector, subsector.name = subsector, capital.overnight.all = value, input.capital = `input-capital`) %>%
    #   left_join(L2233.globaltech_mineral_cost_combine %>%
    #               filter(year %in% c(1975, 1990, 2005, 2010, 2015)) %>%
    #               mutate(year = if_else(year == 1975, 1971, year)),
    #             by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology", "year")) %>%
    #   # only substract mineral cost for the early year, and the capital cost (no mineral cost) component will follow the cost reduction projection
    #   mutate(mineral_capital_ratio = mineral_cost/capital.overnight.all) %>%
    #   select(sector.name, subsector.name, technology, year, capital.overnight.all, mineral_cost, mineral_capital_ratio) %>%
    #   filter(year == 2015)
    #
    # writexl::write_xlsx(L2233.pv_wind_storage_subtype_mineral_capital_ratio, "L2233.pv_wind_storage_subtype_mineral_capital_ratio.xlsx")


    # For all other technologies with no subtypes. For other technologies, we just do the substraction for each year for now, because the mineral cost accounts for
    # much smaller percentage of the capital cost, so there is no way future non-mineral capital cost will go negative.
    L2233.GlobalTechCapital_elecPassthru_no_pv_wind <-
      L2233.GlobalTechCapital_elecPassthru %>%
      filter(!technology %in% c("wind_storage", "PV_storage")) %>%
      left_join(L2233.globaltech_mineral_cost_combine, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology", "year")) %>%
      mutate(capital.overnight = capital.overnight - mineral_cost) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]])  #--OUTPUT--


    #  End of mineral cost calculation
    #------------------------------------------------------------------------------------------------------------------



    ##  2.4   Link the new supplysector back to the existing supplysector (by creating minicam.energy.input based on new supply sectors) - in Global database

    L2233.GlobalElecMineral_elecSupplySector <-
      elec_tech_mineral_map %>%
      #filter(from.technology %in% c("PV", "PV_storage", "wind", "wind_offshore", "wind_storage", "rooftop_pv")) %>%
      repeat_add_columns(tibble(year = as.integer(MODEL_YEARS))) %>%
      mutate(minicam.energy.input = to.supplysector,
             sector.name = from.supplysector,
             subsector.name = from.subsector,
             technology = from.technology,
             efficiency = 1) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) %>%
      unique()

    #  for pv and wind (no storage)
    L2233.GlobalIntTechMineral_elecSupplySector <-
      L2233.GlobalElecMineral_elecSupplySector %>%
      semi_join(A23.globalinttech,
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology" = "intermittent.technology"))
      # rename(intermittent.technology = technology)
    # --OUTPUT--

    #  for pv and wind with storage
    L2233.GlobalTechMineral_elecSupplySector <-
      L2233.GlobalElecMineral_elecSupplySector %>%
      anti_join(A23.globalinttech,
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology" = "intermittent.technology"))
    # --OUTPUT--

    #------------------------------------------------------------------------------------------------------------------

    ## 2.5   Add the Lifetime of subtype technologies in the new supplysector -- in Global database

    L2233.GlobalTechLifetimeMineral_elec <-
      L223.GlobalTechLifetime_elec %>%
      filter(technology %in% c("PV_storage", "wind_storage")) %>%
      left_join(elec_tech_mineral_map,
                by = c("sector.name" = "from.supplysector", "subsector.name" = "from.subsector", "technology" = "from.technology"),
                relationship = "many-to-many") %>%
      select(sector.name = to.supplysector, subsector.name = to.subsector, technology = to.technology, year, lifetime)
    # --OUTPUT--

    L2233.GlobalIntTechLifetimeMineral_elec <-
      L223.GlobalIntTechLifetime_elec %>%
      filter(intermittent.technology %in% c("PV", "wind", "wind_offshore")) %>%
      left_join(elec_tech_mineral_map,
                by = c("sector.name" = "from.supplysector", "subsector.name" = "from.subsector", "intermittent.technology" = "from.technology"),
                relationship = "many-to-many") %>%
      select(sector.name = to.supplysector, subsector.name = to.subsector, intermittent.technology = to.technology, year, lifetime)
    # --OUTPUT--

    L2233.GlobalIntTechLifetime_CSP <-
      L2233.GlobalIntTechLifetime_elec_cool %>%
      filter(!technology %in% c("wind", "wind_offshore", "PV"))
    # --OUTPUT--

    L2233.GlobalTechLifetime_elec_cool_no_pv_wind <-
      L2233.GlobalTechLifetime_elec_cool %>%
      filter(!technology %in% c("wind_storage", "PV_storage"))
    # --OUTPUT--

    #------------------------------------------------------------------------------------------------------------------

    ## BY 7-7-2025: Regionalize demands
    ## For minerals that are now traded, we need to differentiate mineral supply and demand
    # Mineral supplies are named as: copper, lithium, nickel
    # Mineral demands are named as: regional copper, regional lithium, regional nickel
    L2233.Regionaltech_mineral_coef_constance_regMineralInputs <- regionalize_mineral_inputs(L2233.Regionaltech_mineral_coef)
    # L2233.Regionaltech_mineral_coef_reduction_regMineralInputs <- regionalize_mineral_inputs(L2233.Regionaltech_mineral_coef_reduction)
    L2233.Globaltech_mineral_coef_constance_regMineralInputs <- regionalize_mineral_inputs(L2233.Globaltech_mineral_coef)
    # L2233.Globaltech_mineral_coef_reduction_regMineralInputs <- regionalize_mineral_inputs(L2233.Globaltech_mineral_coef_reduction)

    ## BY 7-28-2025: Modify mineral intensities in the base years such that we would have the equivalent mineral demands if we
    # had the service demand representing solely the new investment (i.e. if base years were vintaged)

    # REGIONALTECH (solar and wind technologies)
    # First, calculate the "new investment" in each base year.
    # We assume a depreciation rate of 5% annually.

    # First, bind together the StubTechProd associated with all Regionaltechs.
    # This includes: CSP (from L2233.StubTechProd_elec_cool) and
    # pv and wind technologies (from L2233.StubTechProd_mineral_pv_wind).
    # Note that several technologies do not have associated StubTechProd in base years.
    L2233.StubTechProd_solar_wind <- L2233.StubTechProd_elec_cool %>%
      filter(subsector == "CSP") %>%
      bind_rows(L2233.StubTechProd_mineral_pv_wind) %>%
      select(-share.weight.year, -subs.share.weight, -tech.share.weight)

    L2233.NewInvestment_pv_wind <- L2233.StubTechProd_solar_wind %>%
      rename(output = calOutputValue) %>%
      group_by(region, supplysector, subsector, stub.technology) %>%
      arrange(year) %>%
      mutate(lag_output = lag(output),
             new_investment = output - lag_output,
             new_investment = pmax(new_investment, 0),
             new_investment = if_else((is.na(new_investment) & !is.na(output)), output, new_investment)) %>%
      ungroup()

    L2233.Regionaltech_mineral_coef_constance_modified <- L2233.NewInvestment_pv_wind  %>%
      # Join in the mineral intensity coefficient
      # Using LJ as it is not a 1-to-1 mapping
      left_join(filter(L2233.Regionaltech_mineral_coef_constance_regMineralInputs, year %in% MODEL_BASE_YEARS),
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      # adjust the mineral intensities by the ratio between the incremental service demand and the original service demand
      mutate(current.coef_new = if_else(output == 0, 0, current.coef * (new_investment / output))) %>%
      # replace the current coef with the incremental current coef
      mutate(current.coef = current.coef_new) %>%
      select(LEVEL2_DATA_NAMES[["RegionalStubTechMineralCurCoefAllYr"]])


    # Use modified coefficients where they exist, else default to the original coefficients.
    # modified coefficients only exist for technologies with StubTechProd calibrated values in base years.
    L2233.Regionaltech_mineral_coef_constance_modMI <- L2233.Regionaltech_mineral_coef_constance_regMineralInputs %>%
      left_join(L2233.Regionaltech_mineral_coef_constance_modified, by = c("region", "supplysector", "subsector", "stub.technology", "year",
                                                                           "minicam.energy.input", "model.year"),
                suffix = c(".original", ".new")) %>%
      mutate(current.coef = if_else(is.na(current.coef.new), current.coef.original, current.coef.new)) %>%

      select(LEVEL2_DATA_NAMES[["RegionalStubTechMineralCurCoefAllYr"]])

    #GLOBALTECH (all other technologies) -> REGIONALTECH FOR BASE YEARS
    # Historical mineral coefficients will have to be specified by region, since they depend on new investments, which vary by region.
    # First, calculate the "new investment" in each base year.
    # We assume a depreciation rate of 5% annually.
    L2233.NewInvestment_elec_cool <- L2233.StubTechProd_elec_cool %>%
      rename(output = calOutputValue) %>%
      group_by(region, supplysector, subsector, stub.technology) %>%
      arrange(year) %>%
      mutate(lag_output = lag(output),
             new_investment = output - lag_output,
             new_investment = pmax(new_investment, 0),
             new_investment = if_else((is.na(new_investment) & !is.na(output)), output, new_investment)) %>%
      ungroup()

    L2233.Regional_Globaltech_mineral_coef_constance_Yb_modMI <- L2233.NewInvestment_elec_cool %>%
      # Join in the mineral intensity coefficient
      # Using LJ as it is not a 1-to-1 mapping
      left_join(filter(L2233.Globaltech_mineral_coef_constance_regMineralInputs, year %in% MODEL_BASE_YEARS),
                by = c("supplysector" = "sector.name", "subsector" = "subsector.name", "stub.technology" = "technology", "year"),
                relationship = "many-to-many") %>%
      filter(!is.na(current.coef)) %>%
      # adjust the mineral intensities by the ratio between the incremental service demand and the original service demand
      mutate(current.coef_new = if_else(output == 0, 0, current.coef * (new_investment / output))) %>%
      # replace the current coef with the incremental current coef
      mutate(current.coef = current.coef_new) %>%
      select(LEVEL2_DATA_NAMES[["RegionalStubTechMineralCurCoefAllYr"]])

    ##BY 8-19-2025 Annualize mineral intensities
    # By default GCAM output reports the mineral demand associated with new investment for each full period (e.g. 5-years)
    # We want to view annual mineral demand, and therefore we have previously divided output by 5
    # However, to balance calibration, we now need to do this step internally
    L2233.Regionaltech_mineral_coef_constance_final <- L2233.Regionaltech_mineral_coef_constance_modMI %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, model.year) %>%
      arrange(year) %>%
      mutate(years_elapsed = if_else(is.na(lag(year)), 1, year - lag(year)),
             current.coef  = current.coef / years_elapsed) %>%
      ungroup() %>%
      select(-years_elapsed)

    L2233.Globaltech_mineral_coef_constance_final <- L2233.Globaltech_mineral_coef_constance_regMineralInputs %>%
      group_by(sector.name, subsector.name, technology, minicam.energy.input, model.year) %>%
      arrange(year) %>%
      mutate(years_elapsed = if_else(is.na(lag(year)), 1, year - lag(year)),
             current.coef  = current.coef / years_elapsed) %>%
      ungroup() %>%
      select(-years_elapsed)

    L2233.Regional_Globaltech_mineral_coef_constance_Yb <- L2233.Regional_Globaltech_mineral_coef_constance_Yb_modMI %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, model.year) %>%
      arrange(year) %>%
      mutate(years_elapsed = if_else(is.na(lag(year)), 1, year - lag(year)),
             current.coef  = current.coef / years_elapsed) %>%
      ungroup() %>%
      select(-years_elapsed)

  #BY 9-8-2025: Add price multipliers for the mineral component of cost
  # price multiplier is equivalent to 0.13 * the number of years elapsed because new additions are tracked on a timestep basis
  # 0.13 is the fixed-charge-rate. The mineral cost is considered part of the capital cost,
  # so the mineral cost are multiplied by the fixed-charge-rate to get the annuity, which will later be used for calculating technology levelized
  # cost.
  L2233.Regionaltech_mineral_PMult <- L2233.Regionaltech_mineral_coef_constance_final %>%
    group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, model.year) %>%
    arrange(year) %>%
    mutate(price.unit.conversion = 0.13*if_else(is.na(lag(year)), 1, year - lag(year))) %>%
    ungroup() %>%
    select(LEVEL2_DATA_NAMES[["StubCaloriePriceConv"]]) %>%
    distinct()

  L2233.Globaltech_mineral_PMult <- L2233.Globaltech_mineral_coef_constance_final %>%
    group_by(sector.name, subsector.name, technology, minicam.energy.input, model.year) %>%
    arrange(year) %>%
    mutate(price.unit.conversion = 0.13*if_else(is.na(lag(year)), 1, year - lag(year))) %>%
    ungroup() %>%
    select(LEVEL2_DATA_NAMES[["GlobalTechInputPMult"]]) %>%
    distinct()

  L2233.Regional_Globaltech_mineral_Yb_PMult <- L2233.Regional_Globaltech_mineral_coef_constance_Yb %>%
    group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, model.year) %>%
    arrange(year) %>%
    mutate(price.unit.conversion = 0.13*if_else(is.na(lag(year)), 1, year - lag(year))) %>%
    ungroup() %>%
    select(LEVEL2_DATA_NAMES[["StubCaloriePriceConv"]]) %>%
    distinct()


    ## ===================================================================
    ## Section 3 -- Produce outputs, add appropriate flags and comments
    ## ===================================================================

    #  supply sector
    L2233.Sector_elec_mineral %>%
      add_title("new supply sector for mineral") %>%
      add_units("unitless") %>%
      add_comments("new supply sector for mineral") %>%
      add_legacy_name("L2233.Sector_elec_mineral") %>%
      add_precursors("minerals/electricity/A23.sector_mineral") ->
      L2233.Sector_elec_mineral

    L2233.PassThruSector_elec_mineral %>%
      add_title("new pass through supply sector for mineral") %>%
      add_units("unitless") %>%
      add_comments("new pass through supply sector for mineral") %>%
      add_legacy_name("L2233.PassThruSector_elec_mineral") %>%
      add_precursors("minerals/electricity/A23.sector_mineral") ->
      L2233.PassThruSector_elec_mineral
    # subsector
    L2233.SubsectorLogit_elec_mineral %>%
      add_title("subsector (new supply sector for mineral) logit value") %>%
      add_units("unitless") %>%
      add_comments("subsector (new supply sector for mineral) logit value") %>%
      add_legacy_name("L2233.SubsectorLogit_elec_mineral") %>%
      add_precursors("minerals/electricity/A23.subsector_logit_mineral") ->
      L2233.SubsectorLogit_elec_mineral

    L2233.SubsectorShrwtFllt_elec_mineral %>%
      add_title("subsector (new supply sector for mineral) share weight, and fillout year") %>%
      add_units("unitless") %>%
      add_comments("subsector (new supply sector for mineral) share weight, and fillout year") %>%
      add_legacy_name("L2233.SubsectorShrwtFllt_elec_mineral") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "L223.SubsectorShrwtFllt_elec") ->
      L2233.SubsectorShrwtFllt_elec_mineral

    L2233.SubsectorInterp_elec_mineral %>%
      add_title("subsector (new supply sector for mineral) share weight interpolation rule") %>%
      add_units("unitless") %>%
      add_comments("subsector (new supply sector for mineral) share weight interpolation rule") %>%
      add_legacy_name("L2233.SubsectorInterp_elec_mineral") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "L223.SubsectorInterp_elec") ->
      L2233.SubsectorInterp_elec_mineral

    L2233.SubsectorInterpTo_elec_mineral %>%
      add_title("subsector (new supply sector for mineral) share weight interpolation rule for rooftop pv") %>%
      add_units("unitless") %>%
      add_comments("subsector (new supply sector for mineral) share weight interpolation rule for rooftop pv") %>%
      add_legacy_name("L2233.SubsectorInterpTo_elec_mineral") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "L223.SubsectorInterpTo_elec") ->
      L2233.SubsectorInterpTo_elec_mineral

    L2233.SubsectorShrwt_elec_mineral %>%
      add_title("subsector (new supply sector for mineral) share weight value in 2100") %>%
      add_units("unitless") %>%
      add_comments("subsector (new supply sector for mineral) share weight value in 2100") %>%
      add_legacy_name("L2233.SubsectorShrwt_elec_mineral") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "L223.SubsectorShrwt_renew") ->
      L2233.SubsectorShrwt_elec_mineral

    L2233.SubsecShrwt_mineral_other_pv_wind %>%
      add_title("subsector historical share weights for pv_storage and wind_storage subtypes") %>%
      add_units("no unit") %>%
      add_comments("subsector historical share weights for pv_storage and wind_storage subtypes") %>%
      add_legacy_name("L2233.SubsecShrwt_mineral_other_pv_wind") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map") ->
      L2233.SubsecShrwt_mineral_other_pv_wind


    L2233.SubsecShrwt_mineral_pv_wind %>%
      add_title("subsector historical share weights for pv_mineral and wind_mineral") %>%
      add_units("no unit") %>%
      add_comments("subsector historical share weights for pv_mineral and wind_mineral") %>%
      add_legacy_name("L2233.SubsecShrwt_mineral_pv_wind") %>%
      add_precursors("L1231.out_EJ_R_elec_F_tech_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "minerals/electricity/elec_tech_mineral_map",
                     "minerals/electricity/A23.globaltech_subtype_calibration") ->
      L2233.SubsecShrwt_mineral_pv_wind

    # stub-technology

    L2233.StubTechShrwt_mineral_pv_wind %>%
      add_title("stub-technology historical share weights for pv_mineral and wind_mineral") %>%
      add_units("no unit") %>%
      add_comments("stub-technology historical share weights for pv_mineral and wind_mineral subtypes") %>%
      add_legacy_name("L2233.StubTechShrwt_mineral_pv_wind") %>%
      add_precursors("L1231.out_EJ_R_elec_F_tech_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "minerals/electricity/elec_tech_mineral_map",
                     "minerals/electricity/A23.globaltech_subtype_calibration") ->
      L2233.StubTechShrwt_mineral_pv_wind

    L2233.StubTechProd_mineral_pv_wind %>%
      add_title("stub-technology historical calibration numbers and share weights for pv and wind subtypes") %>%
      add_units("no unit") %>%
      add_comments("stub-technology historical calibration numbers and share weights for pv and wind subtypes") %>%
      add_legacy_name("L2233.StubTechProd_mineral_pv_wind") %>%
      add_precursors("L1231.out_EJ_R_elec_F_tech_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "minerals/electricity/elec_tech_mineral_map",
                     "minerals/electricity/A23.globaltech_subtype_calibration") ->
      L2233.StubTechProd_mineral_pv_wind

    L2233.StubTechShrwt_mineral_other_pv_wind %>%
      add_title("stub-technology historical share weights for pv_storage and wind_storage subtypes") %>%
      add_units("no unit") %>%
      add_comments("stub-technology historical share weights for pv_storage and wind_storage subtypes") %>%
      add_legacy_name("L2233.StubTechShrwt_mineral_other_pv_wind") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map") ->
      L2233.StubTechShrwt_mineral_other_pv_wind

    L2233.StubTechShrwt_mineral_pv_wind_future %>%
      add_title("stub-technology 2100 share weights for pv and wind (with and without storage) subtypes") %>%
      add_units("no unit") %>%
      add_comments("stub-technology 2100 share weights for pv and wind (with and without storage) subtypes") %>%
      add_legacy_name("L2233.StubTechShrwt_mineral_pv_wind_future") %>%
      same_precursors_as(L2233.StubTechProd_mineral_pv_wind) %>%
      add_precursors("minerals/electricity/A23.globaltech_subtype_shrwt") ->
      L2233.StubTechShrwt_mineral_pv_wind_future

    L2233.StubTechInterpTo_mineral_pv_wind_tech %>%
      add_title("stub-technology future share weights and interpolation rule for pv and wind (with and without storage) subtypes") %>%
      add_units("no unit") %>%
      add_comments("stub-technology future share weights and interpolation rule for pv and wind (with and without storage) subtypes") %>%
      add_legacy_name("L2233.StubTechInterpTo_mineral_pv_wind_tech") %>%
      same_precursors_as(L2233.StubTechProd_mineral_pv_wind) %>%
      add_precursors("minerals/electricity/A23.globaltech_subtype_interp_to") ->
      L2233.StubTechInterpTo_mineral_pv_wind_tech

    L2233.StubTechCapFac_mineral_pv_wind %>%
      add_title("stub-technology (new supply sector for mineral) capacity factor for pv, wind, and storage") %>%
      add_units("unitless") %>%
      add_comments("stub-technology (new supply sector for mineral) capacity factor for pv, wind, and storage") %>%
      add_legacy_name("L2233.StubTechCapFac_mineral_pv_wind") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "L223.StubTechCapFactor_elec") ->
      L2233.StubTechCapFac_mineral_pv_wind

    # Mineral intensity -- regional database

    L2233.Regionaltech_mineral_coef_constance_final %>%
      add_title("Constant mineral intensity data for generation technologies (wind and solar with and without storage) -- in the regional database") %>%
      add_units("kg/EJ") %>%
      add_comments("Constant mineral intensity data for generation technologies (wind and solar with and without storage) -- in the regional database") %>%
      add_legacy_name("L2233.Regionaltech_mineral_coef_constance_final") %>%
      add_precursors("minerals/electricity/A23.globaltech_mineral_coef_kg_kw", "minerals/electricity/A23.globaltech_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_mineral_coef_ratio_reduction",
                     "minerals/electricity/A23.globaltech_storage_mineral_coef_kg_kwh", "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_reduction",
                     "minerals/electricity/elec_tech_mineral_map", "water/elec_tech_water_map",
                     "L223.StubTechCapFactor_elec", "L2233.StubTechCapFactor_elec_cool", "L2233.GlobalTechCapFac_elec_cool",
                     "L2233.StubTechProd_elec_cool") ->
      L2233.Regionaltech_mineral_coef_constance_final

    # Mineral intensity -- global database
    L2233.Globaltech_mineral_coef_constance_final %>%
      add_title("Mineral intensity data for other non-solar and non-wind generation technologies -- in the global database") %>%
      add_units("kg/EJ") %>%
      add_comments("Mineral intensity data for other non-solar and non-wind generation technologies -- in the global database") %>%
      add_legacy_name("L2233.Globaltech_mineral_coef_constance_final") %>%
      add_precursors("minerals/electricity/A23.globaltech_mineral_coef_kg_kw", "minerals/electricity/A23.globaltech_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_mineral_coef_ratio_reduction",
                     "minerals/electricity/A23.globaltech_storage_mineral_coef_kg_kwh", "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_reduction",
                     "minerals/electricity/elec_tech_mineral_map", "water/elec_tech_water_map",
                     "L223.StubTechCapFactor_elec", "L2233.StubTechCapFactor_elec_cool", "L2233.GlobalTechCapFac_elec_cool") ->
      L2233.Globaltech_mineral_coef_constance_final

    L2233.Regional_Globaltech_mineral_coef_constance_Yb %>%
      add_title("Mineral intensity data for other non-solar and non-wind generation technologies -- in the regional database (for base years, recalculated)") %>%
      add_units("kg/EJ") %>%
      add_comments("These will overwrite the globaltech database mineral intensity values for base years") %>%
      same_precursors_as("L2233.Regionaltech_mineral_coef_reduction_final") ->
      L2233.Regional_Globaltech_mineral_coef_constance_Yb

    # Price multiplier
    L2233.Regionaltech_mineral_PMult %>%
      add_title("Mineral price unit conversion for generation technologies (wind and solar with and without storage) -- in the regional database") %>%
      add_comments("Mineral price unit conversion for generation technologies (wind and solar with and without storage) -- in the regional database") %>%
      same_precursors_as("L2233.Regionaltech_mineral_coef_constance_final") ->
      L2233.Regionaltech_mineral_PMult

    L2233.Globaltech_mineral_PMult %>%
      add_title("Mineral price unit conversion for other non-solar and non-wind generation technologies -- in the global database") %>%
      add_comments("Mineral price unit conversion for other non-solar and non-wind generation technologies -- in the global database") %>%
      same_precursors_as("L2233.Globaltech_mineral_coef_constance_final") ->
      L2233.Globaltech_mineral_PMult

    L2233.Regional_Globaltech_mineral_Yb_PMult %>%
      add_title("Mineral price unit conversion for other non-solar and non-wind generation technologies -- in the regional database (for base years, recalculated)") %>%
      add_comments("Mineral price unit conversion for other non-solar and non-wind generation technologies -- in the regional database") %>%
      same_precursors_as("L2233.Regionaltech_mineral_coef_reduction_final") ->
      L2233.Regional_Globaltech_mineral_Yb_PMult

    # Mon-mineral capital cost -- pv and wind subtypes
    L2233.GlobalTechCapital_elec_subtype_pv_wind %>%
      add_title("Non-mineral capital cost for electricity generation technology (solar pv and wind with and without storage at subtype level)") %>%
      add_units("$/kW") %>%
      add_comments("Non-mineral capital cost for electricity generation technology (solar pv and wind with and without storage at subtype level)") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_subtype_pv_wind") %>%
      add_precursors("minerals/supply/A10.mineral_rsrc_info",
                     "minerals/electricity/A23.globaltech_mineral_coef_kg_kw", "minerals/electricity/A23.globaltech_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_storage_mineral_coef_kg_kwh", "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_subtype_capital") ->
      L2233.GlobalTechCapital_elec_subtype_pv_wind

    # Mon-mineral capital cost -- pv and wind subtypes with storage
    L2233.GlobalTechCapital_elec_subtype_pv_wind_storage %>%
      add_title("Non-mineral capital cost for electricity generation technology (solar pv and wind with and without storage at subtype level)") %>%
      add_units("$/kW") %>%
      add_comments("Non-mineral capital cost for electricity generation technology (solar pv and wind with and without storage at subtype level)") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_subtype_pv_wind_storage") %>%
      add_precursors("minerals/supply/A10.mineral_rsrc_info",
                     "minerals/electricity/A23.globaltech_mineral_coef_kg_kw", "minerals/electricity/A23.globaltech_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_storage_mineral_coef_kg_kwh", "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_subtype_capital") ->
      L2233.GlobalTechCapital_elec_subtype_pv_wind_storage

    #write.csv(L2233.GlobalTechCapital_elec_subtype, "L2233.GlobalTechCapital_elec_subtype.csv")

    # Mon-mineral capital cost -- non-pv and non-wind
    L2233.GlobalTechCapital_elecPassthru_no_pv_wind %>%
      add_title("Non-mineral capital cost for all other electricity generation technology (non-pv and non-wind)") %>%
      add_units("$/kW") %>%
      add_comments("Non-mineral capital cost for all other electricity generation technology (non-pv and non-wind)") %>%
      add_legacy_name("L2233.GlobalTechCapital_elecPassthru_no_pv_wind") %>%
      add_precursors("minerals/supply/A10.mineral_rsrc_info",
                     "minerals/electricity/A23.globaltech_mineral_coef_kg_kw", "minerals/electricity/A23.globaltech_mineral_coef_ratio_constance",
                     "minerals/electricity/A23.globaltech_storage_mineral_coef_kg_kwh", "minerals/electricity/A23.globaltech_storage_mineral_coef_ratio_constance",
                     "L2233.GlobalTechCapital_elecPassthru") ->
      L2233.GlobalTechCapital_elecPassthru_no_pv_wind

    # link existing supply sector with the new supplysector (pv and wind, no storage)
    L2233.GlobalIntTechMineral_elecSupplySector %>%
      add_title("Link the new supplysector back to the existing supplysector for pv and wind with out storage (creating minicam.energy.input based on new supply sectors)") %>%
      add_units("no unit") %>%
      add_comments("Link the new supplysector back to the existing supplysector for pv and wind with out storage (creating minicam.energy.input based on new supply sectors)") %>%
      add_legacy_name("L2233.GlobalIntTechMineral_elecSupplySector") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "energy/A23.globalinttech") ->
      L2233.GlobalIntTechMineral_elecSupplySector

    # link existing supply sector with the new supplysector (pv and wind, with storage)
    L2233.GlobalTechMineral_elecSupplySector %>%
      add_title("Link the new supplysector back to the existing supplysector (by creating minicam.energy.input based on new supply sectors)") %>%
      add_units("no unit") %>%
      add_comments("Link the new supplysector back to the existing supplysector (by creating minicam.energy.input based on new supply sectors)") %>%
      add_legacy_name("L2233.GlobalTechMineral_elecSupplySector") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "energy/A23.globalinttech") ->
      L2233.GlobalTechMineral_elecSupplySector

    # lifetime
    L2233.GlobalTechLifetimeMineral_elec %>%
      add_title("lifetime of subtype technologies (non-intermittent) in the new supplysectors") %>%
      add_units("year") %>%
      add_comments("lifetime of subtype technologies (non-intermittent, pv_storage, wind_storage) in the new supplysectors") %>%
      add_legacy_name("L2233.GlobalTechLifetimeMineral_elec") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "L223.GlobalTechLifetime_elec") ->
      L2233.GlobalTechLifetimeMineral_elec

    L2233.GlobalIntTechLifetimeMineral_elec %>%
      add_title("lifetime of subtype technologies (intermittent) in the new supplysectors") %>%
      add_units("year") %>%
      add_comments("lifetime of subtype technologies (intermittent, pv and wind) in the new supplysectors") %>%
      add_legacy_name("L2233.GlobalIntTechLifetimeMineral_elec") %>%
      add_precursors("minerals/electricity/elec_tech_mineral_map", "L223.GlobalIntTechLifetime_elec") ->
      L2233.GlobalIntTechLifetimeMineral_elec

    L2233.GlobalIntTechLifetime_CSP %>%
      add_title("life time for intermittent CSP (no storage)") %>%
      add_units("year") %>%
      add_comments("life time for intermittent CSP (no storage)") %>%
      add_legacy_name("L2233.GlobalIntTechLifetime_CSP") %>%
      add_precursors("L2233.GlobalIntTechLifetime_elec_cool") ->
      L2233.GlobalIntTechLifetime_CSP

    L2233.GlobalTechLifetime_elec_cool_no_pv_wind %>%
      add_title("life time for electricity generation technology that exclude pv and wind storage") %>%
      add_units("$/kWh") %>%
      add_comments("lifetime for electricity generation technology that exckude pv and wind storage") %>%
      add_legacy_name("L2233.GlobalTechLifetime_elec_cool_no_pv_wind") %>%
      add_precursors("L2233.GlobalTechLifetime_elec_cool") ->
      L2233.GlobalTechLifetime_elec_cool_no_pv_wind


    return_data(L2233.Sector_elec_mineral,
                L2233.PassThruSector_elec_mineral,
                L2233.SubsectorLogit_elec_mineral,
                L2233.SubsectorShrwtFllt_elec_mineral,
                L2233.SubsectorInterp_elec_mineral,
                L2233.SubsectorInterpTo_elec_mineral,
                L2233.SubsectorShrwt_elec_mineral,
                L2233.SubsecShrwt_mineral_other_pv_wind,
                L2233.SubsecShrwt_mineral_pv_wind,
                L2233.StubTechShrwt_mineral_pv_wind,
                L2233.StubTechProd_mineral_pv_wind,
                L2233.StubTechShrwt_mineral_other_pv_wind,
                L2233.StubTechShrwt_mineral_pv_wind_future,
                L2233.StubTechInterpTo_mineral_pv_wind_tech,
                L2233.StubTechCapFac_mineral_pv_wind,
                L2233.Regionaltech_mineral_coef_constance_final,
                L2233.Globaltech_mineral_coef_constance_final,
                L2233.Regional_Globaltech_mineral_coef_constance_Yb,
                L2233.Regionaltech_mineral_PMult,
                L2233.Globaltech_mineral_PMult,
                L2233.Regional_Globaltech_mineral_Yb_PMult,
                # L2233.GlobalTechCapital_elec_subtype,
                L2233.GlobalTechCapital_elec_subtype_pv_wind,
                L2233.GlobalTechCapital_elec_subtype_pv_wind_storage,
                L2233.GlobalTechCapital_elecPassthru_no_pv_wind,
                L2233.GlobalIntTechMineral_elecSupplySector,
                L2233.GlobalTechMineral_elecSupplySector,
                L2233.GlobalTechLifetimeMineral_elec,
                L2233.GlobalIntTechLifetimeMineral_elec,
                L2233.GlobalIntTechLifetime_CSP,
                L2233.GlobalTechLifetime_elec_cool_no_pv_wind
    )
  } else {
    stop("Unknown command")
  }
}
