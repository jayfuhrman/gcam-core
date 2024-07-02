# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Off_road_xml
#'
#' Construct XML data structure for \code{Off_road.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Off_road.xml}, \code{Off_road_cwf.xml}, \code{Off_road_cwf_low_H2.xml},
#' \code{Off_road_cwf_med_H2.xml}, \code{Off_road_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_Off_road_xml.R} (energy XML).
module_energy_Off_road_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2324.GlobalTechInterp_Off_road_cwf",
             "L2324.GlobalTechShrwt_Off_road_cwf",
			        "L2324.GlobalTechEff_Off_road_cwf",
			        "L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios",
			        "L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Off_road_cwf.xml",
             XML = "Off_road_cwf_low_H2.xml",
             XML = "Off_road_cwf_med_H2.xml",
             XML = "Off_road_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2324.GlobalTechInterp_Off_road_cwf <- get_data(all_data, "L2324.GlobalTechInterp_Off_road_cwf")
    L2324.GlobalTechShrwt_Off_road_cwf <- get_data(all_data, "L2324.GlobalTechShrwt_Off_road_cwf")
    L2324.GlobalTechEff_Off_road_cwf <- get_data(all_data, "L2324.GlobalTechEff_Off_road_cwf")
    L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios <- get_data(all_data, "L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios")
    L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios <- get_data(all_data, "L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios")
    # ===================================================

    Off_road_cwf_low_H2.xml <- Off_road_cwf_med_H2.xml <- Off_road_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs

    create_xml("Off_road_cwf.xml") %>%
      add_xml_data(L2324.GlobalTechInterp_Off_road_cwf, "GlobalTechInterp") %>%
      add_xml_data(L2324.GlobalTechShrwt_Off_road_cwf, "GlobalTechShrwt") %>%
      add_xml_data(L2324.GlobalTechEff_Off_road_cwf, "GlobalTechEff") %>% # CWF version
      add_precursors("L2324.GlobalTechInterp_Off_road_cwf",
                     "L2324.GlobalTechShrwt_Off_road_cwf",
                     "L2324.GlobalTechEff_Off_road_cwf") ->
      Off_road_cwf.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2", "cwf_med_H2", "cwf_high_H2")) {
      L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios_sel <- L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios_sel <- L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("Off_road_", i, ".xml")

      create_xml(xml_name) %>%
        add_xml_data(L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios_sel, "GlobalTechInterp") %>% # CWF version for this case
        add_xml_data(L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios_sel, "GlobalTechShrwt") %>% # CWF version for this case
        add_precursors("L2324.GlobalTechShrwt_Off_road_cwf_H2_scenarios_sel",
                       "L2324.GlobalTechInterp_Off_road_cwf_H2_scenarios_sel") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(Off_road_cwf.xml, Off_road_cwf_low_H2.xml, Off_road_cwf_med_H2.xml, Off_road_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}

