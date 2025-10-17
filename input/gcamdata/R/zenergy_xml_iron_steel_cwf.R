# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_iron_steel_cwf_xml
#'
#' Construct XML data structure for \code{iron_steel.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel.xml}, \code{iron_steel_cwf.xml}, \code{iron_steel_cwf_low_H2.xml}, \code{iron_steel_cwf_med_H2.xml}, \code{iron_steel_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_iron_steel_xml.R} (energy XML).
module_energy_iron_steel_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2323.GlobalTechShrwt_iron_steel_cwf",
             "L2323.GlobalTechCoef_iron_steel_cwf",
             "L2323.StubTechCoef_iron_steel_cwf",
             "L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios",
             FILE = "cwf/A323.incelas_cwf"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel_cwf.xml",
             XML = "iron_steel_cwf_LED.xml",
             XML = "iron_steel_cwf_low_H2.xml",
             XML = "iron_steel_cwf_high_H2.xml",
             XML = "iron_steel_incelas_cwf.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2323.GlobalTechShrwt_iron_steel_cwf <- get_data(all_data, "L2323.GlobalTechShrwt_iron_steel_cwf")
    L2323.GlobalTechCoef_iron_steel_cwf <- get_data(all_data, "L2323.GlobalTechCoef_iron_steel_cwf")
    L2323.StubTechCoef_iron_steel_cwf <- get_data(all_data, "L2323.StubTechCoef_iron_steel_cwf")
    L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios <- get_data(all_data, "L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios")
    # ===================================================
    iron_steel_cwf_low_H2.xml <- iron_steel_cwf_med_H2.xml <- iron_steel_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs

    create_xml("iron_steel_cwf.xml") %>%
      add_xml_data(L2323.GlobalTechShrwt_iron_steel_cwf, "GlobalTechShrwt") %>%
      add_xml_data(L2323.GlobalTechCoef_iron_steel_cwf %>% filter(scenario == 'cwf'), "GlobalTechCoef") %>% # CWF version
      add_xml_data(L2323.StubTechCoef_iron_steel_cwf %>% filter(scenario == 'cwf'), "StubTechCoef") %>% # CWF version
      add_precursors("L2323.GlobalTechShrwt_iron_steel_cwf",
                     "L2323.GlobalTechCoef_iron_steel_cwf",
                     "L2323.StubTechCoef_iron_steel_cwf") ->
      iron_steel_cwf.xml

    create_xml("iron_steel_cwf_LED.xml") %>%
      add_xml_data(L2323.GlobalTechCoef_iron_steel_cwf %>% filter(scenario == 'cwf-LED'), "GlobalTechCoef") %>% # CWF version
      add_xml_data(L2323.StubTechCoef_iron_steel_cwf %>% filter(scenario == 'cwf-LED'), "StubTechCoef") ->
      iron_steel_cwf_LED.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2",
                # "cwf_med_H2",
                "cwf_high_H2")) {
      L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios_sel <- L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("iron_steel_", i, ".xml")

      create_xml(xml_name) %>%
        add_xml_data(L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios_sel, "GlobalTechShrwt") %>% # CWF version for this scenario
        add_precursors("L2323.GlobalTechShrwt_iron_steel_cwf_H2_scenarios")  %>%
        assign(xml_name, ., envir = curr_env)
    }

    L2323.iron_steel_incelas_cwf <- get_data(all_data, 'cwf/A323.incelas_cwf') %>%
      gather_years() %>%
      filter(year > MODEL_FINAL_BASE_YEAR) %>%
      rename(energy.final.demand = `energy-final-demand`,
             income.elasticity = value)

    create_xml("iron_steel_incelas_cwf.xml") %>%
      add_xml_data(L2323.iron_steel_incelas_cwf, "IncomeElasticity") %>%
      add_precursors("L2323.iron_steel_incelas_cwf") ->
      iron_steel_incelas_cwf.xml

    return_data(iron_steel_cwf.xml,
                iron_steel_cwf_LED.xml,
                iron_steel_cwf_low_H2.xml,
                iron_steel_cwf_high_H2.xml,
                iron_steel_incelas_cwf.xml)
  } else {
    stop("Unknown command")
  }
}

