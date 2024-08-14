# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_other_industry_xml
#'
#' Construct XML data structure for \code{other_industry.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{other_industry.xml}, \code{other_industry_cwf.xml},
#' \code{other_industry_cwf_low_H2.xml}, \code{other_industry_cwf_med_H2.xml},
#' \code{other_industry_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_industry_xml.R} (energy XML).
module_energy_other_industry_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.SubsectorInterp_ind_cwf",
             "L232.SubsectorShrwtFllt_ind_cwf",
             "L232.GlobalTechEff_ind_cwf",
             "L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios",
             "L232.SubsectorInterp_ind_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "other_industry_cwf.xml",
             # XML = "other_industry_cwf_low_fossil.xml",
             XML = "other_industry_cwf_low_H2.xml",
             # XML = "other_industry_cwf_med_H2.xml",
             XML = "other_industry_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    L232.GlobalTechEff_ind_cwf <- get_data(all_data, "L232.GlobalTechEff_ind_cwf")
    L232.SubsectorShrwtFllt_ind_cwf <- get_data(all_data, "L232.SubsectorShrwtFllt_ind_cwf")
    L232.SubsectorInterp_ind_cwf <- get_data(all_data, "L232.SubsectorInterp_ind_cwf")
    L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios <- get_data(all_data, "L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios")
    L232.SubsectorInterp_ind_cwf_H2_scenarios <- get_data(all_data, "L232.SubsectorInterp_ind_cwf_H2_scenarios")

    # ===================================================
    other_industry_cwf_low_H2.xml <- other_industry_cwf_med_H2.xml <- other_industry_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs
    # combine the previous cwf (which only update GlobalTechEff) and low_fossil scenario (which updates shrwts)

    create_xml("other_industry_cwf.xml") %>%
      add_xml_data(L232.GlobalTechEff_ind_cwf, "GlobalTechEff") %>% # CWF version
      add_xml_data(L232.SubsectorShrwtFllt_ind_cwf, "SubsectorShrwtFllt") %>% # CWF version
      add_xml_data(L232.SubsectorInterp_ind_cwf, "SubsectorInterp") %>% # CWF version
      add_precursors("L232.GlobalTechEff_ind_cwf",
                     "L232.SubsectorInterp_ind_cwf",
                     "L232.SubsectorShrwtFllt_ind_cwf") ->
      other_industry_cwf.xml

    # create_xml("other_industry_cwf_low_fossil.xml") %>%
    #   add_xml_data(L232.SubsectorShrwtFllt_ind_low_fossil, "SubsectorShrwtFllt") %>%
    #   add_xml_data(L232.SubsectorInterp_ind_low_fossil, "SubsectorInterp") %>%
    #   add_precursors("L232.SubsectorInterp_ind_low_fossil",
    #                  "L232.SubsectorShrwtFllt_ind_low_fossil") ->
    #   other_industry_cwf_low_fossil.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2",
                # "cwf_med_H2",
                "cwf_high_H2")) {
      L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios_sel <- L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L232.SubsectorInterp_ind_cwf_H2_scenarios_sel <- L232.SubsectorInterp_ind_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("other_industry_", i, ".xml")

      create_xml(xml_name) %>%
        add_xml_data(L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L232.SubsectorInterp_ind_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        add_precursors("L232.SubsectorInterp_ind_cwf_H2_scenarios",
                       "L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(other_industry_cwf.xml,
                # other_industry_cwf_low_fossil.xml,
                other_industry_cwf_low_H2.xml,
                # other_industry_cwf_med_H2.xml,
                other_industry_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}
