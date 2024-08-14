# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_distribution_cwf_xml
#'
#' Construct XML data structure for \code{en_distribution.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_distribution_H2_peaker_cwf_no_gas_2050.xml}. The corresponding file in the
#' original data system was \code{batch_en_distribution.xml.R} (energy XML).
module_energy_en_distribution_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.SubsectorShrwtFllt_en_cwf_no_gas_2050",
             "L226.SubsectorInterp_en_cwf_no_gas_2050"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_distribution_H2_peaker_cwf_no_gas_2050.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.SubsectorShrwtFllt_en_cwf_no_gas_2050 <- get_data(all_data, "L226.SubsectorShrwtFllt_en_cwf_no_gas_2050")
    L226.SubsectorInterp_en_cwf_no_gas_2050 <- get_data(all_data, "L226.SubsectorInterp_en_cwf_no_gas_2050")

    # ===================================================
    en_distribution_H2_peaker_cwf_low_H2.xml <- NULL # silence package check notes

    # Produce outputs
    create_xml("en_distribution_H2_peaker_cwf_no_gas_2050.xml") %>%
      add_xml_data(L226.SubsectorShrwtFllt_en_cwf_no_gas_2050, "SubsectorShrwtFllt") %>% # CWF version for this case
      add_xml_data(L226.SubsectorInterp_en_cwf_no_gas_2050, "SubsectorInterp") %>% # CWF version for this case
      add_precursors("L226.SubsectorShrwtFllt_en_cwf_no_gas_2050", "L226.SubsectorInterp_en_cwf_no_gas_2050") ->
      en_distribution_H2_peaker_cwf_no_gas_2050.xml

    return_data(en_distribution_H2_peaker_cwf_no_gas_2050.xml)
  } else {
    stop("Unknown command")
  }
}
