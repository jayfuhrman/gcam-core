#' @details Formats hfc and pfc gas emissions for input. Calculates future emission factors for hfc gases based on 2010 region emissions and USA emission factors and emission factors from Guus Velders (http://www.sciencedirect.com/science/article/pii/S135223101530488X) for the  SSP scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate select
#' @importFrom tidyr gather spread
#' @author JF Nov 2025


# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_kigali_amendment_fgas_xml
#'
#' Construct XML data structure for \code{all_fgas_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_fgas_emissions.xml}, \code{all_fgas_emissions_MAC.xml}.
#' The corresponding file in the
#' original data system was \code{batch_all_fgas_emissions.xml} (emissions XML).
module_emissions_kigali_amendment_fgas_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/kigali_phasedown_schedules",
             FILE = "common/GCAM_region_names_Montreal_Protocol",
             FILE = "emissions/kigali_phasedown_schedules",
             FILE = "common/GCAM_region_names_Montreal_Protocol",
             "L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units"))

  } else if (command == driver.DECLARE_OUTPUTS) {
    return(c( XML = "all_fgas_emissions_kigali.xml"))
  } else if(command == driver.MAKE) {

  all_data <- list(...)[[1]]

  # Load required inputs
  kigali_phasedown_schedules <- get_data(all_data, "emissions/kigali_phasedown_schedules")
  GCAM_region_names_Montreal_Protocol <- get_data(all_data, "common/GCAM_region_names_Montreal_Protocol")

  kigali_phasedown_schedules_long <- kigali_phasedown_schedules %>%
    right_join(GCAM_region_names_Montreal_Protocol,by = c("Kigali_Amendment_Party")) %>%
    gather_years

  L241.hfc_all <- get_data(all_data, "L241.hfc_all")
  L241.pfc_all <- get_data(all_data, "L241.pfc_all")
  L241.hfc_future <- get_data(all_data, "L241.hfc_future")
  L241.fgas_all_units <- get_data(all_data, "L241.fgas_all_units")

  # ===================================================

  L241.hfc_future_kigali <- L241.hfc_future %>%
    complete(year = c(year, MODEL_FUTURE_YEARS),nesting(region,supplysector,subsector,stub.technology,Non.CO2)) %>%
    group_by(Non.CO2,region,stub.technology,subsector,supplysector) %>%
    mutate(emiss.coeff = approx_fun(year, emiss.coeff, rule = 2)) %>%
    ungroup() %>%
    filter(year >= 2021)


  L241.hfc_future_kigali_other <- L241.hfc_all %>%
    complete(year = c(year, MODEL_FUTURE_YEARS),nesting(region,supplysector,subsector,stub.technology,Non.CO2)) %>%
    rename(emiss.coeff = input.emissions) %>%
    group_by(Non.CO2,region,stub.technology,subsector,supplysector) %>%
    mutate(emiss.coeff = approx_fun(year, emiss.coeff, rule = 2)) %>%
    ungroup() %>%
    filter(year > 2021) %>%
    anti_join(L241.hfc_future_kigali, by = c("region","supplysector","subsector","stub.technology","year","Non.CO2")) %>%
    select(year,region,supplysector,subsector,stub.technology,Non.CO2,emiss.coeff)

  L241.hfc_future_kigali <- bind_rows(L241.hfc_future_kigali,L241.hfc_future_kigali_other) %>%
    left_join(kigali_phasedown_schedules_long %>%
                rename(pct_phasedown = value), by = c("region","year")) %>%
    mutate(emiss.coeff = emiss.coeff * pct_phasedown) %>%
    filter(!is.na(emiss.coeff)) %>%
    anti_join(L241.hfc_all,by = c("region","supplysector","subsector","stub.technology","year","Non.CO2")) %>%
    bind_rows(L241.hfc_future %>% filter(year == 2021))

  create_xml("all_fgas_emissions_kigali.xml") %>%
    add_xml_data(L241.hfc_all, "StbTechOutputEmissions") %>%
    add_xml_data(L241.pfc_all, "StbTechOutputEmissions") %>%
    add_xml_data(L241.hfc_future_kigali, "OutputEmissCoeff") %>%
    add_xml_data(L241.fgas_all_units, "StubTechEmissUnits") %>%
    add_precursors("L241.hfc_all", "L241.pfc_all",
                   "L241.hfc_future_kigali", "L241.fgas_all_units") ->
    all_fgas_emissions_kigali.xml

  return_data(all_fgas_emissions_kigali.xml)


} else {
  stop("Unknown command")
}
}
