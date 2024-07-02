# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_chemical_xml
#'
#' Construct XML data structure for \code{chemical.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{chemical.xml}, \code{chemical_cwf.xml}, \code{chemical_cwf_low_H2.xml}, \code{chemical_cwf_med_H2.xml}, \code{chemical_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_chemical_xml.R} (energy XML).
module_energy_chemical_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2325.GlobalTechEff_chemical_cwf",
			 "L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios",
			 "L2325.SubsectorInterp_chemical_cwf_H2_scenarios",
			 "L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "chemical_cwf.xml",
             XML = "chemical_cwf_low_H2.xml",
             XML = "chemical_cwf_med_H2.xml",
             XML = "chemical_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2325.GlobalTechEff_chemical_cwf <- get_data(all_data, "L2325.GlobalTechEff_chemical_cwf")
    L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios <- get_data(all_data, "L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios")
    L2325.SubsectorInterp_chemical_cwf_H2_scenarios <- get_data(all_data, "L2325.SubsectorInterp_chemical_cwf_H2_scenarios")
    L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios <- get_data(all_data, "L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios")
    # ===================================================

    chemical_cwf_low_H2.xml <- chemical_cwf_med_H2.xml <- chemical_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs

    create_xml("chemical_cwf.xml") %>%
      add_xml_data(L2325.GlobalTechEff_chemical_cwf, "GlobalTechEff") %>% # CWF version
      add_precursors("L2325.GlobalTechEff_chemical_cwf") ->
      chemical_cwf.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2", "cwf_med_H2", "cwf_high_H2")) {
      L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios_sel <- L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2325.SubsectorInterp_chemical_cwf_H2_scenarios_sel <- L2325.SubsectorInterp_chemical_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios_sel <- L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("chemical_", i, ".xml")

      create_xml(xml_name) %>%
        add_xml_data(L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L2325.SubsectorInterp_chemical_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        add_xml_data(L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios_sel, "GlobalTechShrwt") %>% # CWF version for this case
        add_precursors("L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios",
                       "L2325.SubsectorInterp_chemical_cwf_H2_scenarios",
                       "L2325.GlobalTechShrwt_chemical_cwf_H2_scenarios") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(chemical_cwf.xml, chemical_cwf_low_H2.xml, chemical_cwf_med_H2.xml, chemical_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}

