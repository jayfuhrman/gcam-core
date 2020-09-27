# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_all_fgas_emissions_xml
#'
#' Construct XML data structure for \code{all_fgas_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_fgas_emissions.xml}, \code{all_fgas_emissions_MAC.xml},
#' \code{all_fgas_emissions_MAC_TC.xml}, \code{all_fgas_emissions_MAC_noTC.xml},
#' \code{all_fgas_emissions_MAC_highTC.xml}, \code{all_fgas_emissions_MAC_PhaseIn.xml},
#' The corresponding file in the
#' original data system was \code{batch_all_fgas_emissions.xml} (emissions XML).
module_emissions_batch_all_fgas_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units",
             "L252.MAC_higwp",
             "L252.MAC_higwp_tc",
             "L252.MAC_higwp_phaseInTime",
             "L252.MAC_higwp_tc_average"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_fgas_emissions.xml",
             XML = "all_fgas_emissions_MAC.xml",
             XML = "all_fgas_emissions_MAC_TC.xml",
             XML = "all_fgas_emissions_MAC_PhaseIn.xml",
             XML = "all_fgas_emissions_MAC_highTC.xml",
             XML = "all_fgas_emissions_MAC_noTC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.pfc_all <- get_data(all_data, "L241.pfc_all")
    L241.hfc_future <- get_data(all_data, "L241.hfc_future")
    L241.fgas_all_units <- get_data(all_data, "L241.fgas_all_units")
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp")
    L252.MAC_higwp_tc <- get_data(all_data, "L252.MAC_higwp_tc")
    L252.MAC_higwp_phaseInTime <- get_data(all_data, "L252.MAC_higwp_phaseInTime")
    L252.MAC_higwp_tc_average <- get_data(all_data, "L252.MAC_higwp_tc_average")

    # ===================================================

    # Produce outputs
    create_xml("all_fgas_emissions.xml") %>%
      add_xml_data(L241.hfc_all, "StbTechOutputEmissions") %>%
      add_xml_data(L241.pfc_all, "StbTechOutputEmissions") %>%
      add_xml_data(L241.hfc_future, "OutputEmissCoeff") %>%
      add_xml_data(L241.fgas_all_units, "StubTechEmissUnits") %>%
      add_precursors("L241.hfc_all", "L241.pfc_all",
                     "L241.hfc_future", "L241.fgas_all_units") ->
      all_fgas_emissions.xml

    create_xml("all_fgas_emissions_MAC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_precursors("L252.MAC_higwp") ->
      all_fgas_emissions_MAC.xml

    create_xml("all_fgas_emissions_MAC_highTC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc, "MACTC") %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_tc") ->
      all_fgas_emissions_MAC_highTC.xml

    # temporarily create a "noTC" file assuming tech.change = 0 after 2050
    # for validation and sensitivity purpose, will be deleted later
    L252.MAC_higwp_tc_zero2050 <- L252.MAC_higwp_tc_average %>%
      mutate(tech.change = ifelse(tech.change.year > 2050, 0, tech.change))

    create_xml("all_fgas_emissions_MAC_noTC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc_zero2050, "MACTC") %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_tc_average") ->
      all_fgas_emissions_MAC_noTC.xml

    create_xml("all_fgas_emissions_MAC_TC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc_average, "MACTC") %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_tc_average") ->
      all_fgas_emissions_MAC_TC.xml

    create_xml("all_fgas_emissions_MAC_PhaseIn.xml") %>%
      add_xml_data(L252.MAC_higwp_phaseInTime, "MACPhaseIn") %>%
      add_precursors("L252.MAC_higwp_phaseInTime") ->
      all_fgas_emissions_MAC_PhaseIn.xml

    return_data(all_fgas_emissions.xml,
                all_fgas_emissions_MAC.xml,
                all_fgas_emissions_MAC_highTC.xml,
                all_fgas_emissions_MAC_noTC.xml,
                all_fgas_emissions_MAC_TC.xml,
                all_fgas_emissions_MAC_PhaseIn.xml)
  } else {
    stop("Unknown command")
  }
}
