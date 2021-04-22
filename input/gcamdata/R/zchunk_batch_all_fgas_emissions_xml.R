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
#' \code{all_fgas_emissions_MAC_lowTC.xml}.
#' The corresponding file in the
#' original data system was \code{batch_all_fgas_emissions.xml} (emissions XML).
module_emissions_batch_all_fgas_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units",
             "L252.MAC_higwp",
             "L252.MAC_higwp_phaseInTime",
             "L252.MAC_higwp_tc_average"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_fgas_emissions.xml",
             XML = "all_fgas_emissions_MAC_lowTC.xml",
             XML = "all_fgas_emissions_MAC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- NULL #Silence package check

    # Load required inputs
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.pfc_all <- get_data(all_data, "L241.pfc_all")
    L241.hfc_future <- get_data(all_data, "L241.hfc_future")
    L241.fgas_all_units <- get_data(all_data, "L241.fgas_all_units")
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp")
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
      add_xml_data(L252.MAC_higwp_tc_average, "MACTC") %>%
      add_xml_data(L252.MAC_higwp_phaseInTime, "MACPhaseIn") %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_tc_average", "L252.MAC_higwp_phaseInTime") ->
      all_fgas_emissions_MAC.xml

    # temporarily creat a version of MAC for reference with low tech.change
    post_2050_low <- L252.MAC_higwp_tc_average %>%
      filter(tech.change.year == 2050) %>%
      select(-tech.change.year) %>%
      repeat_add_columns(tibble(tech.change.year = seq(2055, 2100, 5)))

    L252.MAC_higwp_tc_low <- L252.MAC_higwp_tc_average %>%
      filter(tech.change.year <= 2050) %>%
      bind_rows(post_2050_low)

    create_xml("all_fgas_emissions_MAC_lowTC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc_low, "MACTC") %>%
      add_xml_data(L252.MAC_higwp_phaseInTime, "MACPhaseIn") %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_tc_average", "L252.MAC_higwp_phaseInTime") ->
      all_fgas_emissions_MAC_lowTC.xml

    return_data(all_fgas_emissions.xml,
                all_fgas_emissions_MAC_lowTC.xml,
                all_fgas_emissions_MAC.xml)
  } else {
    stop("Unknown command")
  }
}
