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
#' \code{all_energy_emissions_MAC_smth_highTC.xml}, \code{all_energy_emissions_MAC_smth_averageTC.xml},
#' \code{all_energy_emissions_MAC_smth_noTC.xml}.
#' The corresponding file in the original data system was \code{batch_all_energy_emissions.xml.R} (emissions XML).
module_emissions_batch_all_energy_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.en_pol_emissions",
              "L201.en_ghg_emissions",
              "L201.en_bcoc_emissions",
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
              "L252.ResMAC_fos_phaseInFraction",
              "L252.ResMAC_fos_tc_average",
              "L252.ResMAC_fos_smth",
              "L252.MAC_prc",
              "L252.MAC_prc_tc",
              "L252.MAC_prc_phaseInFraction",
              "L252.MAC_prc_tc_average",
              "L252.MAC_prc_smth"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_energy_emissions.xml",
             XML = "all_energy_emissions_MAC.xml",
             XML = "all_energy_emissions_MAC_TC.xml",
             XML = "all_energy_emissions_MAC_smth_highTC.xml",
             XML = "all_energy_emissions_MAC_smth_averageTC.xml",
             XML = "all_energy_emissions_MAC_smth_noTC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    emiss.coeff <- NULL  # silence package check note

    # Load required inputs
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions")
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions")
    L201.en_bcoc_emissions <- get_data(all_data, "L201.en_bcoc_emissions")
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
    L252.ResMAC_fos_phaseInFraction <- get_data(all_data, "L252.ResMAC_fos_phaseInFraction")
    L252.ResMAC_fos_tc_average <- get_data(all_data, "L252.ResMAC_fos_tc_average")
    L252.ResMAC_fos_smth <- get_data(all_data, "L252.ResMAC_fos_smth")
    L252.MAC_prc <- get_data(all_data, "L252.MAC_prc")
    L252.MAC_prc_tc <- get_data(all_data, "L252.MAC_prc_tc")
    L252.MAC_prc_phaseInFraction <- get_data(all_data, "L252.MAC_prc_phaseInFraction")
    L252.MAC_prc_tc_average <- get_data(all_data, "L252.MAC_prc_tc_average")
    L252.MAC_prc_smth <- get_data(all_data, "L252.MAC_prc_smth")
    # ===================================================
    # Produce outputs
    create_xml("all_energy_emissions.xml") %>%
      add_xml_data(L201.en_pol_emissions, "InputEmissions") %>%
      add_xml_data(L201.en_ghg_emissions, "InputEmissions") %>%
      #add_xml_data(L201.en_bcoc_emissions, "InputEmissCoeff") %>%
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
      add_precursors("L201.en_pol_emissions", "L201.en_ghg_emissions", "L201.en_bcoc_emissions",
                     "L201.OutputEmissions_elec", "L201.OutputEmissCoeff_elec", "L241.OutputEmissCoeff_elec",
                     "L201.nonghg_max_reduction", "L201.nonghg_steepness", "L201.nonghg_max_reduction_res",
                     "L201.nonghg_steepness_res", "L201.nonghg_res", "L201.ghg_res", "L232.nonco2_prc",
                     "L232.nonco2_max_reduction", "L232.nonco2_steepness", "L241.nonco2_tech_coeff",
                     "L241.nonco2_max_reduction", "L241.nonco2_steepness") ->
      all_energy_emissions.xml

    create_xml("all_energy_emissions_MAC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.MAC_prc, "MAC") %>%
      add_precursors("L252.ResMAC_fos", "L252.MAC_prc") ->
      all_energy_emissions_MAC.xml

    create_xml("all_energy_emissions_MAC_smth_highTC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_smth, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc %>% filter(tech.year >= 2055), "ResMACTC", NULL) %>%
      add_xml_data(L252.MAC_prc, "MAC") %>%
      add_xml_data(L252.MAC_prc_smth, "MAC") %>%
      add_xml_data(L252.MAC_prc_tc %>% filter(tech.year >= 2055), "MACTC_allyear", NULL) %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_smth",
                     "L252.MAC_prc", "L252.MAC_prc_smth", "L252.ResMAC_fos_tc",
                     "L252.MAC_prc_tc") ->
      all_energy_emissions_MAC_smth_highTC.xml

    create_xml("all_energy_emissions_MAC_smth_averageTC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_smth, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc_average %>% filter(tech.year >= 2055), "ResMACTC", NULL) %>%
      add_xml_data(L252.MAC_prc, "MAC") %>%
      add_xml_data(L252.MAC_prc_smth, "MAC") %>%
      add_xml_data(L252.MAC_prc_tc_average %>% filter(tech.year >= 2055), "MACTC_allyear", NULL) %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_smth",
                     "L252.MAC_prc", "L252.MAC_prc_smth", "L252.ResMAC_fos_tc_average",
                     "L252.MAC_prc_tc_average") ->
      all_energy_emissions_MAC_smth_averageTC.xml

    create_xml("all_energy_emissions_MAC_smth_noTC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_smth, "ResMAC") %>%
      add_xml_data(L252.MAC_prc, "MAC") %>%
      add_xml_data(L252.MAC_prc_smth, "MAC") %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_smth",
                     "L252.MAC_prc", "L252.MAC_prc_smth") ->
      all_energy_emissions_MAC_smth_noTC.xml

    create_xml("all_energy_emissions_MAC_TC.xml") %>%
      add_xml_data(L252.ResMAC_fos, "ResMAC") %>%
      add_xml_data(L252.ResMAC_fos_tc_average, "ResMACTC", NULL) %>%
      add_xml_data(L252.ResMAC_fos_phaseInFraction, "ResMACPhaseIn", NULL) %>%
      add_xml_data(L252.MAC_prc, "MAC") %>%
      add_xml_data(L252.MAC_prc_tc_average, "MACTC_allyear", NULL) %>%
      add_xml_data(L252.MAC_prc_phaseInFraction, "MACTC_allyear_PhaseIn", NULL) %>%
      add_precursors("L252.ResMAC_fos", "L252.ResMAC_fos_tc_average", "L252.ResMAC_fos_phaseInFraction",
                     "L252.MAC_prc", "L252.MAC_prc_tc_average", "L252.MAC_prc_phaseInFraction") ->
      all_energy_emissions_MAC_TC.xml

    return_data(all_energy_emissions.xml,
                all_energy_emissions_MAC.xml,
                all_energy_emissions_MAC_smth_highTC.xml,
                all_energy_emissions_MAC_smth_averageTC.xml,
                all_energy_emissions_MAC_smth_noTC.xml,
                all_energy_emissions_MAC_TC.xml)
  } else {
    stop("Unknown command")
  }
}
