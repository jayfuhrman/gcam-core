# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_Fert_xml
#'
#' Construct XML data structure for \code{en_Fert.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_Fert.xml}. The corresponding file in the
#' original data system was \code{batch_en_Fert_xml.R} (energy XML).
module_energy_en_Fert_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2322.SubsectorShrwtFllt_Fert_cwf",
             "L2322.SubsectorInterp_Fert_cwf",
             "L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios",
             "L2322.SubsectorInterp_Fert_cwf_H2_scenarios",
             "L2322.GlobalTechShrwt_Fert_cwf"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_Fert_cwf.xml",
             XML = "en_Fert_cwf_low_H2.xml",
             # XML = "en_Fert_cwf_med_H2.xml",
             XML = "en_Fert_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2322.SubsectorShrwtFllt_Fert_cwf <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_cwf")
    L2322.SubsectorInterp_Fert_cwf <- get_data(all_data, "L2322.SubsectorInterp_Fert_cwf")
    L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios")
    L2322.SubsectorInterp_Fert_cwf_H2_scenarios <- get_data(all_data, "L2322.SubsectorInterp_Fert_cwf_H2_scenarios")
    L2322.GlobalTechShrwt_Fert_cwf <- get_data(all_data, "L2322.GlobalTechShrwt_Fert_cwf")
    # ===================================================

    en_Fert_cwf_low_H2.xml <- en_Fert_cwf_med_H2.xml <- en_Fert_cwf_high_H2.xml <- NULL # silence package check notes
    curr_env <- environment()

    # Produce outputs
    create_xml("en_Fert_cwf.xml") %>%
      add_xml_data(L2322.GlobalTechShrwt_Fert_cwf, "GlobalTechShrwt") %>% # Phase out unabated fossil techs
      add_xml_data(L2322.SubsectorShrwtFllt_Fert_cwf, "SubsectorShrwtFllt") %>% # CWF version
      add_xml_data(L2322.SubsectorInterp_Fert_cwf, "SubsectorInterp") %>% # CWF version
      add_precursors("L2322.SubsectorShrwtFllt_Fert_cwf",
                     "L2322.SubsectorInterp_Fert_cwf",
                     "L2322.GlobalTechShrwt_Fert_cwf") ->
      en_Fert_cwf.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2",
                # "cwf_med_H2",
                "cwf_high_H2")) {
      L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios_sel <- L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2322.SubsectorInterp_Fert_cwf_H2_scenarios_sel <- L2322.SubsectorInterp_Fert_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("en_Fert_", i, ".xml")

      create_xml(xml_name) %>%
        add_xml_data(L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L2322.SubsectorInterp_Fert_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        add_precursors("L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios",
                       "L2322.SubsectorInterp_Fert_cwf_H2_scenarios") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(en_Fert_cwf.xml,
                en_Fert_cwf_low_H2.xml,
                # en_Fert_cwf_med_H2.xml,
                en_Fert_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}
