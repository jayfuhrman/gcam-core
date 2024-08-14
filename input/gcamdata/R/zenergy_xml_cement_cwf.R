# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_cement_xml
#'
#' Construct XML data structure for \code{cement.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement.xml}, \code{cement_cwf.xml}, \code{cement_cwf_low_H2.xml}, \code{cement_cwf_med_H2.xml}, \code{cement_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_cement_xml.R} (energy XML).
module_energy_cement_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2321.GlobalTechCoef_cement_cwf",
             "L2321.StubTechCoef_cement_cwf",
             "L2321.GlobalTechShrwt_cement",
             "L2321.SubsectorShrwtFllt_cement_cwf",
             "L2321.SubsectorInterp_cement_cwf",
             "L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios",
             "L2321.SubsectorInterp_cement_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement_cwf.xml",
             XML = "cement_all_CCS_post2030.xml",
             XML = "cement_cwf_low_H2.xml",
             # XML = "cement_cwf_med_H2.xml",
             XML = "cement_cwf_high_H2.xml"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2321.GlobalTechShrwt_cement <- get_data(all_data, "L2321.GlobalTechShrwt_cement")
    L2321.GlobalTechCoef_cement_cwf <- get_data(all_data, "L2321.GlobalTechCoef_cement_cwf")
    L2321.StubTechCoef_cement_cwf <- get_data(all_data, "L2321.StubTechCoef_cement_cwf")

    L2321.SubsectorShrwtFllt_cement_cwf <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_cwf")
    L2321.SubsectorInterp_cement_cwf <- get_data(all_data, "L2321.SubsectorInterp_cement_cwf")

    L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios")
    L2321.SubsectorInterp_cement_cwf_H2_scenarios <- get_data(all_data, "L2321.SubsectorInterp_cement_cwf_H2_scenarios")
    # ===================================================

    cement_cwf_low_H2.xml <-
      # cement_cwf_med_H2.xml <-
      cement_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs
    create_xml("cement_cwf.xml") %>%
      add_xml_data(L2321.GlobalTechCoef_cement_cwf, "GlobalTechCoef") %>% # CWF version
      add_xml_data(L2321.StubTechCoef_cement_cwf, "StubTechCoef") %>% # CWF version
      add_xml_data(L2321.SubsectorShrwtFllt_cement_cwf, "SubsectorShrwtFllt") %>% # CWF version
      add_xml_data(L2321.SubsectorInterp_cement_cwf, "SubsectorInterp") %>% # CWF version
      add_precursors("L2321.GlobalTechCoef_cement_cwf",
                     "L2321.StubTechCoef_cement_cwf",
                     "L2321.SubsectorShrwtFllt_cement_cwf",
                     "L2321.SubsectorInterp_cement_cwf") ->
      cement_cwf.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2",
                # "cwf_med_H2",
                "cwf_high_H2")) {
      L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios_sel <- L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2321.SubsectorInterp_cement_cwf_H2_scenarios_sel <- L2321.SubsectorInterp_cement_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("cement_", i, ".xml")

      create_xml(xml_name) %>%
        add_xml_data(L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L2321.SubsectorInterp_cement_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        add_precursors("L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios",
                       "L2321.SubsectorInterp_cement_cwf_H2_scenarios") %>%
        assign(xml_name, ., envir = curr_env)
    }

    L2321.GlobalTechShrwt_cement_cwf <- L2321.GlobalTechShrwt_cement %>%
      mutate(share.weight = if_else(technology == 'cement' & year > 2030, 0,share.weight))

    create_xml("cement_all_CCS_post2030.xml") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement_cwf, "GlobalTechShrwt") %>%
      add_precursors('L2321.GlobalTechShrwt_cement') ->
      cement_all_CCS_post2030.xml

    return_data(cement_cwf.xml,
                cement_all_CCS_post2030.xml,
                cement_cwf_low_H2.xml,
                # cement_cwf_med_H2.xml,
                cement_cwf_high_H2.xml)

  } else {
    stop("Unknown command")
  }
}
