# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_aluminum_xml
#'
#' Construct XML data structure for \code{aluminum.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aluminum.xml}. The corresponding file in the
#' original data system was \code{batch_aluminum_xml.R} (energy XML).
module_energy_aluminum_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2326.GlobalTechCoef_aluminum_cwf",
             "L2326.StubTechCoef_aluminum_cwf",
             FILE = "cwf/A326.incelas_cwf"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "aluminum_cwf.xml",
             XML = "aluminum_cwf_LED.xml",
             XML = "aluminum_incelas_cwf.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2326.GlobalTechCoef_aluminum_cwf <- get_data(all_data, "L2326.GlobalTechCoef_aluminum_cwf")
    L2326.StubTechCoef_aluminum_cwf <- get_data(all_data, "L2326.StubTechCoef_aluminum_cwf")
    # ===================================================

    # Produce outputs
    create_xml("aluminum_cwf.xml") %>%
      add_xml_data(L2326.GlobalTechCoef_aluminum_cwf %>% filter(scenario == 'cwf'), "GlobalTechCoef") %>% # CWF version
      add_xml_data(L2326.StubTechCoef_aluminum_cwf %>% filter(scenario == 'cwf'), "StubTechCoef") %>% # CWF version
      add_precursors("L2326.StubTechCoef_aluminum_cwf",
                     "L2326.GlobalTechCoef_aluminum_cwf") ->
      aluminum_cwf.xml

    create_xml("aluminum_cwf_LED.xml") %>%
      add_xml_data(L2326.GlobalTechCoef_aluminum_cwf %>% filter(scenario == 'cwf-LED'), "GlobalTechCoef") %>% # CWF version
      add_xml_data(L2326.StubTechCoef_aluminum_cwf %>% filter(scenario == 'cwf-LED'), "StubTechCoef") %>% # CWF version
      add_precursors("L2326.StubTechCoef_aluminum_cwf",
                     "L2326.GlobalTechCoef_aluminum_cwf") ->
      aluminum_cwf_LED.xml

    # do the same for the CWF income elasticity file
    L2326.aluminum_incelas_cwf <- get_data(all_data, "cwf/A326.incelas_cwf", strip_attributes = TRUE) %>%
      gather_years() %>%
      filter(year > MODEL_FINAL_BASE_YEAR) %>%
      rename(energy.final.demand = `energy-final-demand`,
             income.elasticity = value)

    create_xml("aluminum_incelas_cwf.xml") %>%
      add_xml_data(L2326.aluminum_incelas_cwf, "IncomeElasticity") %>%
      add_precursors("cwf/A326.incelas_cwf") ->
      aluminum_incelas_cwf.xml

    return_data(aluminum_cwf.xml,
                aluminum_cwf_LED.xml,
                aluminum_incelas_cwf.xml)
  } else {
    stop("Unknown command")
  }
}

