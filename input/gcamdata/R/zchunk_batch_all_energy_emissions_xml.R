# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_all_energy_emissions_xml
#'
#' Construct XML data structure for \code{all_energy_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_energy_emissions.xml}, \code{all_energy_emissions_MAC.xml} , \code{all_energy_emissions_MAC_TC.xml},
#' \code{all_energy_emissions_MAC_highTC.xml}, \code{all_energy_emissions_MAC_noTC.xml},
#' \code{all_energy_emissions_MAC_PhaseIn.xml}
#' The corresponding file in the original data system was \code{batch_all_energy_emissions.xml.R} (emissions XML).
module_emissions_batch_all_energy_emissions_xml <- function(command, ...) {
  input_names <- c("L201.en_pol_emissions",
                   "L201.en_ghg_emissions",
                   "L201.OutputEmissions_elec",
                   "L201.OutputEmissCoeff_elec",
                   "L201.nonghg_max_reduction",
                   "L201.nonghg_steepness",
                   "L201.nonghg_max_reduction_res",
                   "L201.nonghg_steepness_res",
                   "L201.nonghg_res",
                   "L201.ghg_res",
                   "L232.nonco2_prc",
                   "L232.nonco2_max_reduction",
                   "L232.nonco2_steepness",
                   "L241.nonco2_tech_coeff",
                   "L241.OutputEmissCoeff_elec",
                   "L241.nonco2_max_reduction",
                   "L241.nonco2_steepness",
                   "L252.ResMAC_fos",
                   "L252.ResMAC_fos_tc",
                   "L252.ResMAC_fos_phaseInTime",
                   "L252.ResMAC_fos_tc_average")
  if(driver.EMISSIONS_SOURCE == "EDGAR") {
    # BC/OC emissions are processed sperately in EDGAR
    input_names <- c(input_names, "L201.en_bcoc_emissions")
  }
  if(command == driver.DECLARE_INPUTS) {
    return(input_names)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_energy_emissions.xml",
             XML = "all_energy_emissions_MAC.xml",
             XML = "all_energy_emissions_MAC_TC.xml",
             XML = "all_energy_emissions_MAC_PhaseIn.xml",
             XML = "all_energy_emissions_MAC_highTC.xml",
             XML = "all_energy_emissions_MAC_noTC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    emiss.coeff <- NULL  # silence package check note

    # Load required inputs
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions")
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions")
    L201.OutputEmissions_elec <- get_data(all_data, "L201.OutputEmissions_elec")
    L201.OutputEmissCoeff_elec <- get_data(all_data, "L201.OutputEmissCoeff_elec")
    L201.nonghg_max_reduction <- get_data(all_data, "L201.nonghg_max_reduction")
    L201.nonghg_steepness <- get_data(all_data, "L201.nonghg_steepness")
    L201.nonghg_max_reduction_res <- get_data(all_data, "L201.nonghg_max_reduction_res")
    L201.nonghg_steepness_res <- get_data(all_data, "L201.nonghg_steepness_res")
    L201.nonghg_res <- get_data(all_data, "L201.nonghg_res")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res")
    L232.nonco2_prc <- get_data(all_data, "L232.nonco2_prc")
    L232.nonco2_max_reduction <- get_data(all_data, "L232.nonco2_max_reduction")
    L232.nonco2_steepness <- get_data(all_data, "L232.nonco2_steepness")
    L241.nonco2_tech_coeff <- get_data(all_data, "L241.nonco2_tech_coeff") %>% rename(emiss.coef = emiss.coeff)
    L241.OutputEmissCoeff_elec <- get_data(all_data, "L241.OutputEmissCoeff_elec")
    L241.nonco2_max_reduction <- get_data(all_data, "L241.nonco2_max_reduction")
    L241.nonco2_steepness <- get_data(all_data, "L241.nonco2_steepness")
    L252.ResMAC_fos <- get_data(all_data, "L252.ResMAC_fos")
    L252.ResMAC_fos_tc <- get_data(all_data, "L252.ResMAC_fos_tc")
    L252.ResMAC_fos_phaseInTime <- get_data(all_data, "L252.ResMAC_fos_phaseInTime")
    L252.ResMAC_fos_tc_average <- get_data(all_data, "L252.ResMAC_fos_tc_average")

    if(driver.EMISSIONS_SOURCE == "EDGAR") {
      L201.en_bcoc_emissions <- get_data(all_data, "L201.en_bcoc_emissions")
      # just include bc/oc with the rest of the GHG emissions now so the
      # processing proceeds the same reglardless of which emiss source is used
      L201.en_ghg_emissions <- bind_rows(L201.en_ghg_emissions, L201.en_bcoc_emissions)
    }

    # ===================================================
    # Produce outputs
    create_xml("all_energy_emissions.xml") %>%
      add_xml_data(L201.en_pol_emissions, "InputEmissions") %>%
      add_xml_data(L201.en_ghg_emissions, "InputEmissions") %>%
      add_xml_data(L201.OutputEmissions_elec, "OutputEmissions") %>%
      add_xml_data(L201.OutputEmissCoeff_elec, "OutputEmissCoeff") %>%
      add_xml_data(L201.nonghg_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L201.nonghg_steepness, "GDPCtrlSteep") %>%
      add_xml_data(L201.nonghg_max_reduction_res, "GDPCtrlMaxRes") %>%
      add_xml_data(L201.nonghg_steepness_res, "GDPCtrlSteepRes") %>%
      add_xml_data(L201.nonghg_res, "ResEmissCoef") %>%
      add_xml_data(L201.ghg_res, "ResEmissCoef") %>%
      add_xml_data(L232.nonco2_prc, "StbTechOutputEmissions") %>%
      add_xml_data(L232.nonco2_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L232.nonco2_steepness, "GDPCtrlSteep") %>%
      add_xml_data(L241.nonco2_tech_coeff, "InputEmissCoeff") %>%
      add_xml_data(L241.OutputEmissCoeff_elec, "OutputEmissCoeff") %>%
      add_xml_data(L241.nonco2_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L241.nonco2_steepness, "GDPCtrlSteep") %>%
      add_precursors("L201.en_pol_emissions", "L201.en_ghg_emissions",
                     "L201.OutputEmissions_elec", "L201.OutputEmissCoeff_elec",
                     "L201.nonghg_max_reduction", "L201.nonghg_steepness", "L201.nonghg_max_reduction_res",
                     "L201.nonghg_steepness_res", "L201.nonghg_res", "L201.ghg_res", "L232.nonco2_prc",
                     "L232.nonco2_max_reduction", "L232.nonco2_steepness", "L241.nonco2_tech_coeff",
                     "L241.OutputEmissCoeff_elec", "L241.nonco2_max_reduction", "L241.nonco2_steepness") ->
      all_energy_emissions.xml
    # need to call add_precursors indirectly to ensure input_names gets "unlisted"
    all_energy_emissions.xml <- do.call("add_precursors", c(list(all_energy_emissions.xml), input_names))



    create_xml("all_energy_emissions_MAC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_precursors("L252.ResMAC_fos") ->
      all_energy_emissions_MAC.xml

    create_xml("all_energy_emissions_MAC_highTC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc, "ResMACTC") %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_tc") ->
      all_energy_emissions_MAC_highTC.xml

    # temporarily create a "noTC" file assuming tech.change = 0 after 2050
    # for validation and sensitivity purpose, will be deleted later
    L252.ResMAC_fos_tc_zero2050 <- L252.ResMAC_fos_tc_average %>%
      mutate(tech.change = ifelse(tech.change.year > 2050, 0, tech.change))

    create_xml("all_energy_emissions_MAC_noTC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc_zero2050, "ResMACTC") %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_tc_average") ->
      all_energy_emissions_MAC_noTC.xml

    create_xml("all_energy_emissions_MAC_TC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc_average, "ResMACTC") %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_tc_average") ->
      all_energy_emissions_MAC_TC.xml

    create_xml("all_energy_emissions_MAC_PhaseIn.xml") %>%
      add_xml_data(L252.ResMAC_fos_phaseInTime, "ResMACPhaseIn") %>%
      add_precursors("L252.ResMAC_fos_phaseInTime") ->
      all_energy_emissions_MAC_PhaseIn.xml

    return_data(all_energy_emissions.xml,
                all_energy_emissions_MAC.xml,
                all_energy_emissions_MAC_highTC.xml,
                all_energy_emissions_MAC_noTC.xml,
                all_energy_emissions_MAC_TC.xml,
                all_energy_emissions_MAC_PhaseIn.xml)
  } else {
    stop("Unknown command")
  }
}
