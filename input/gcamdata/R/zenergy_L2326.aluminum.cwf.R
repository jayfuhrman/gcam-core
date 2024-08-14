# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2326.aluminum
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for aluminum-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2326.SectorLogitTables[[ curr_table ]]$data}, \code{L2326.Supplysector_aluminum}, \code{L2326.FinalEnergyKeyword_aluminum},
#' \code{L2326.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2326.SubsectorLogit_aluminum}, \code{L2326.SubsectorShrwtFllt_aluminum},
#' \code{L2326.SubsectorInterp_aluminum}, \code{L2326.StubTech_aluminum}, \code{L2326.GlobalTechShrwt_aluminum}, \code{L2326.GlobalTechCoef_aluminum},
#' \code{L2326.GlobalTechCost_aluminum}, \code{L2326.GlobalTechCapture_aluminum}, \code{L2326.StubTechProd_aluminum}, \code{L2326.StubTechCalInput_aluminum},
#' \code{L2326.StubTechCoef_aluminum}, \code{L2326.PerCapitaBased_aluminum}, \code{L2326.BaseService_aluminum}, \code{L2326.PriceElasticity_aluminum},
#' \code{L2326.GlobalTechSecOut_aluminum}, \code{L2326.GlobalTechCoef_aluminum_cwf}, \code{L2326.StubTechCoef_aluminum_cwf},
#' \code{object}. The corresponding file in the
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for aluminum sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Dec 2019
module_energy_L2326.aluminum_cwf <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             "L2326.GlobalTechCoef_aluminum",
             "L1326.in_EJ_R_aluminum_Yh",
             "L1326.out_Mt_R_aluminum_Yh",
             "L1326.IO_GJkg_R_aluminum_F_Yh",
			 FILE = "cwf/A326.globaltech_coef_cwf_adj"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2326.GlobalTechCoef_aluminum_cwf",
			       "L2326.StubTechCoef_aluminum_cwf"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    L1326.in_EJ_R_aluminum_Yh <- get_data(all_data, "L1326.in_EJ_R_aluminum_Yh")
    L1326.out_Mt_R_aluminum_Yh <- get_data(all_data, "L1326.out_Mt_R_aluminum_Yh")
    L1326.IO_GJkg_R_aluminum_F_Yh<- get_data(all_data, "L1326.IO_GJkg_R_aluminum_F_Yh")
    A326.globaltech_coef_cwf_adj <- get_data(all_data, "cwf/A326.globaltech_coef_cwf_adj", strip_attributes = TRUE)
    L2326.GlobalTechCoef_aluminum <- get_data(all_data, "L2326.GlobalTechCoef_aluminum", strip_attributes = TRUE)
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <- output.ratio <-
      value.x <- value.y <- parameter <- secondary.output <- elec_ratio <- year.x <- year.y <- output.ratio.x <-
      output.ratio.y <- sector.name <- subsector.name <- stub.technology <- market.name <- terminal_coef <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(sector %in% c("Alumina") & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2326.rm_heat_techs_R # intermediate tibble

    # L2326.StubTechCalInput_aluminum: calibrated input of aluminum energy use technologies (including cogen)
    L2326.GlobalTechCoef_aluminum %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2326.aluminum_tmp

    # L2326.StubTechCalInput_aluminum: calibrated aluminum input
     calibrated_techs %>%
       filter(calibration == "input") %>% # Only take the tech IDs where the calibration is identified as input
       select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
       distinct ->
       calibrated_techs_export # temporary tibble

    L1326.in_EJ_R_aluminum_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      complete(nesting(fuel,year,sector),GCAM_region_ID = GCAM_region_names$GCAM_region_ID) %>%
      filter(fuel != 'heat' | (fuel == 'heat' & value>0) ) %>%
      filter(fuel != 'electricity') %>%
      mutate(value = replace_na(value,0)) %>%
      left_join(GCAM_region_names,by = c("GCAM_region_ID")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join(calibrated_techs_export, by = c("fuel", "sector")) %>%
      mutate(stub.technology = technology,
      technology = NULL)   ->
      L2326.StubTechCalInput_aluminum_tmp

    L2326.aluminum_tmp %>%
      filter(supplysector != "aluminum") %>%
      left_join(L2326.StubTechCalInput_aluminum_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(fuel = NULL,sector = NULL, value = NULL,GCAM_region_ID  = NULL,calibrated.value = replace_na(calibrated.value,0),
             share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name changeto accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value>0 , 1, 0)) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2326.StubTechCalInput_aluminum

    # L2326.StubTechCoef_aluminum
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    #Add coefficients for aluminum and alumina
    L1326.out_Mt_R_aluminum_Yh %>%
      filter(sector == "Alumina") %>%
      mutate(sector = "Aluminum") %>%
      left_join(L1326.out_Mt_R_aluminum_Yh %>%
                                 filter(sector == "Aluminum"), by = c("GCAM_region_ID", "year", "sector")) %>%
      mutate(value = replace_na(value.x/value.y,0), value.x = NULL,value.y = NULL,fuel = "alumina") ->
      L1326.IO_GJkg_R_aluminum_F_Yh_alumina


    L1326.IO_GJkg_R_aluminum_F_Yh %>%
      bind_rows(L1326.IO_GJkg_R_aluminum_F_Yh_alumina) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      complete(nesting(fuel,year,sector),GCAM_region_ID = GCAM_region_names$GCAM_region_ID) %>%
      filter(fuel != 'heat' | (fuel == 'heat' & value>0) ) %>%
      mutate(value = replace_na(value,0)) %>%
      left_join(GCAM_region_names,by = c("GCAM_region_ID")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join(calibrated_techs_export, by = c("fuel", "sector")) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT),
             stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2326.StubTechCoef_aluminum_tmp


    # ===================================================
    # Make CWF adjustments

    # GLOBAL TECH COEF: L2326.GlobalTechCoef_aluminum_cwf
    # get adjustments
    A326.globaltech_coef_cwf_adj %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(coefficient_adj = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      dplyr::select(-value) ->
      L2326.globaltech_coef_cwf_adj

    # apply adjustments to global tech coefficients
    L2326.GlobalTechCoef_aluminum %>%
      left_join_error_no_match(L2326.globaltech_coef_cwf_adj %>% dplyr::select(-secondary.output)) %>%
      mutate(coefficient = round(coefficient * coefficient_adj, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2326.GlobalTechCoef_aluminum_cwf

    # STUB TECH COEF: L2326.StubTechCoef_aluminum_cwf
    # first remake the temporary data frame from previously
    L2326.aluminum_tmp %>%
      left_join(L2326.StubTechCoef_aluminum_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(GCAM_region_ID  = NULL,coefficient = replace_na(coefficient,0),
             share.weight.year = year,market.name = region) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2326.StubTechCoef_aluminum_tmp

    # update convergence in stub technologies
    L2326.StubTechCoef_aluminum_tmp %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(L2326.GlobalTechCoef_aluminum %>% rename(terminal_coef = coefficient,supplysector = sector.name,subsector = subsector.name),
                       supplysector, subsector, technology, minicam.energy.input, terminal_coef, year),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input","year")) %>%
      mutate(coefficient = if_else(year == 2010 & is.na(coefficient), terminal_coef, coefficient),
             coefficient = if_else(year == 2015 & coefficient ==0, terminal_coef, coefficient)) %>%
      select(-terminal_coef) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient,rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      # apply CWF adjustments
      left_join(L2326.globaltech_coef_cwf_adj %>%
                  rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
                  dplyr::select(-secondary.output)) %>%
      mutate(coefficient = round(coefficient * coefficient_adj, energy.DIGITS_COEFFICIENT)) %>%
      filter(year %in% MODEL_YEARS) %>% # drop the terminal coef year if it's outside of the model years
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2326.StubTechCoef_aluminum_cwf


    # ===================================================
    # Produce outputs

    L2326.GlobalTechCoef_aluminum_cwf %>%
      add_title("Energy inputs and coefficients of aluminum technologies") %>%
      add_units("Unitless") %>%
      add_comments("For aluminum sector, the energy use coefficients from A326.globaltech_coef are interpolated into all model years, with CWF adjustments") %>%
      add_legacy_name("L2326.GlobalTechCoef_aluminum_cwf") %>%
      add_precursors("cwf/A326.globaltech_coef_cwf_adj", "L2326.GlobalTechCoef_aluminum") ->
      L2326.GlobalTechCoef_aluminum_cwf

    L2326.StubTechCoef_aluminum_cwf %>%
      add_title("region-specific coefficients of aluminum production technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients from literature wirh CWF adjustments") %>%
      add_legacy_name("L2326.StubTechCoef_aluminum") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names",
                      "cwf/A326.globaltech_coef_cwf_adj", "L2326.GlobalTechCoef_aluminum",
                     "L1326.out_Mt_R_aluminum_Yh", "L1326.IO_GJkg_R_aluminum_F_Yh") ->
      L2326.StubTechCoef_aluminum_cwf

      return_data(L2326.GlobalTechCoef_aluminum_cwf, L2326.StubTechCoef_aluminum_cwf)

  } else {
    stop("Unknown command")
  }
}
