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
#' \code{all_fgas_emissions_MAC_TC.xml}, \code{all_fgas_emissions_MAC_smth.xml}. The corresponding file in the
#' original data system was \code{batch_all_fgas_emissions.xml} (emissions XML).
module_emissions_batch_all_fgas_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units",
             "L252.MAC_higwp",
             "L252.MAC_higwp_tc",
             "L252.MAC_higwp_phaseInFraction",
             "L252.MAC_higwp_tc_average",
             "L252.MAC_higwp_smth"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_fgas_emissions.xml",
             XML = "all_fgas_emissions_MAC.xml",
             XML = "all_fgas_emissions_MAC_TC.xml",
             XML = "all_fgas_emissions_MAC_smth_highTC.xml",
             XML = "all_fgas_emissions_MAC_smth_averageTC.xml",
             XML = "all_fgas_emissions_MAC_smth_noTC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.pfc_all <- get_data(all_data, "L241.pfc_all")
    L241.hfc_future <- get_data(all_data, "L241.hfc_future")
    L241.fgas_all_units <- get_data(all_data, "L241.fgas_all_units")
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp")
    L252.MAC_higwp_tc <- get_data(all_data, "L252.MAC_higwp_tc")
    L252.MAC_higwp_phaseInFraction <- get_data(all_data, "L252.MAC_higwp_phaseInFraction")
    L252.MAC_higwp_tc_average <- get_data(all_data, "L252.MAC_higwp_tc_average")
    L252.MAC_higwp_smth <- get_data(all_data, "L252.MAC_higwp_smth")

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

    create_xml("all_fgas_emissions_MAC_smth_highTC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_smth, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc %>% filter(tech.year >= 2055), "MACTC_allyear", NULL) %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_smth", "L252.MAC_higwp_tc") ->
      all_fgas_emissions_MAC_smth_highTC.xml

    create_xml("all_fgas_emissions_MAC_smth_averageTC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_smth, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc_average %>% filter(tech.year >= 2055), "MACTC_allyear", NULL) %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_smth", "L252.MAC_higwp_tc_average") ->
      all_fgas_emissions_MAC_smth_averageTC.xml

    create_xml("all_fgas_emissions_MAC_smth_noTC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_smth, "MAC") %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_smth") ->
      all_fgas_emissions_MAC_smth_noTC.xml

    create_xml("all_fgas_emissions_MAC_TC.xml") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc_average, "MACTC_allyear", NULL) %>%
      add_xml_data(L252.MAC_higwp_phaseInFraction, "MACTC_allyear_PhaseIn", NULL) %>%
      add_precursors("L252.MAC_higwp", "L252.MAC_higwp_tc_average",
                     "L252.MAC_higwp_phaseInFraction") ->
      all_fgas_emissions_MAC_TC.xml

    return_data(all_fgas_emissions.xml,
                all_fgas_emissions_MAC.xml,
                all_fgas_emissions_MAC_smth_highTC.xml,
                all_fgas_emissions_MAC_smth_averageTC.xml,
                all_fgas_emissions_MAC_smth_noTC.xml,
                all_fgas_emissions_MAC_TC.xml)
  } else {
    stop("Unknown command")
  }
}
