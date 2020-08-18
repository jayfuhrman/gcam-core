# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_all_aglu_emissions_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{all_aglu_emissions_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_aglu_emissions_IRR_MGMT.xml}, \code{all_aglu_emissions_IRR_MGMT_MAC.xml},
#' \code{all_aglu_emissions_IRR_MGMT_MAC_TC.xml}, \code{all_aglu_emissions_IRR_MGMT_MAC_smth.xml}.
#' The corresponding file in the original data system was
#' \code{batch_all_aglu_emissions_IRR_MGMT.xml} (emissions XML).
module_emissions_batch_all_aglu_emissions_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2112.AWBEmissions",
              "L2112.AGREmissions",
              "L211.AnEmissions",
              "L211.AnNH3Emissions",
              "L252.MAC_an",
              "L252.MAC_an_tc",
              "L252.MAC_an_phaseInFraction",
              "L252.MAC_an_tc_average",
              "L252.MAC_an_smth",
              "L2112.AGRBio",
              "L2112.AWB_BCOC_EmissCoeff",
              "L2112.nonghg_max_reduction",
              "L2112.nonghg_steepness",
              "L252.AgMAC",
              "L252.AgMAC_tc",
              "L252.AgMAC_phaseInFraction",
              "L252.AgMAC_tc_average",
              "L252.AgMAC_smth"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_aglu_emissions_IRR_MGMT.xml",
             XML = "all_aglu_emissions_IRR_MGMT_MAC.xml",
             XML = "all_aglu_emissions_IRR_MGMT_MAC_smth_highTC.xml",
             XML = "all_aglu_emissions_IRR_MGMT_MAC_smth_averageTC.xml",
             XML = "all_aglu_emissions_IRR_MGMT_MAC_smth_noTC.xml",
             XML = "all_aglu_emissions_IRR_MGMT_MAC_TC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2112.AWBEmissions <- get_data(all_data, "L2112.AWBEmissions")
    L2112.AGREmissions <- get_data(all_data, "L2112.AGREmissions")
    L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
    L211.AnNH3Emissions <- get_data(all_data, "L211.AnNH3Emissions")
    L252.MAC_an <- get_data(all_data, "L252.MAC_an")
    L252.MAC_an_tc <- get_data(all_data, "L252.MAC_an_tc")
    L252.MAC_an_phaseInFraction <- get_data(all_data, "L252.MAC_an_phaseInFraction")
    L252.MAC_an_tc_average <- get_data(all_data, "L252.MAC_an_tc_average")
    L252.MAC_an_smth <- get_data(all_data, "L252.MAC_an_smth")
    L2112.AGRBio <- get_data(all_data, "L2112.AGRBio")
    L2112.AWB_BCOC_EmissCoeff <- get_data(all_data, "L2112.AWB_BCOC_EmissCoeff")
    L2112.nonghg_max_reduction <- get_data(all_data, "L2112.nonghg_max_reduction")
    L2112.nonghg_steepness <- get_data(all_data, "L2112.nonghg_steepness")
    L252.AgMAC <- get_data(all_data, "L252.AgMAC")
    L252.AgMAC_tc <- get_data(all_data, "L252.AgMAC_tc")
    L252.AgMAC_phaseInFraction <- get_data(all_data, "L252.AgMAC_phaseInFraction")
    L252.AgMAC_tc_average <- get_data(all_data, "L252.AgMAC_tc_average")
    L252.AgMAC_smth <- get_data(all_data, "L252.AgMAC_smth")

    bio_N20_coef <- compVal <- bio_N2O_coef<- NULL # Silence package checks

    # ===================================================
    # Rename the tibble column names to match the header names.
    L2112.AGRBio <- rename(L2112.AGRBio, emiss.coef = bio_N2O_coef)

    # Produce outputs
    create_xml("all_aglu_emissions_IRR_MGMT.xml") %>%
      add_xml_data(L2112.AGRBio, "OutputEmissCoeffAg") %>%
      add_xml_data(L2112.AWB_BCOC_EmissCoeff, "OutputEmissCoeffAg") %>%
      add_xml_data(L2112.nonghg_max_reduction, "AgGDPCtrlMax") %>%
      add_xml_data(L2112.nonghg_steepness, "AgGDPCtrlSteep") %>%
      add_xml_data(L211.AnEmissions, "OutputEmissions") %>%
      add_xml_data(L211.AnNH3Emissions, "OutputEmissions") %>%
      add_xml_data(L2112.AWBEmissions, "OutputEmissionsAg") %>%
      add_xml_data(L2112.AGREmissions, "OutputEmissionsAg") %>%
      add_precursors("L2112.AWBEmissions",
                     "L2112.AGREmissions",
                     "L211.AnEmissions",
                     "L211.AnNH3Emissions",
                     "L2112.AGRBio",
                     "L2112.AWB_BCOC_EmissCoeff",
                     "L2112.nonghg_max_reduction",
                     "L2112.nonghg_steepness") ->
        all_aglu_emissions_IRR_MGMT.xml

    create_xml("all_aglu_emissions_IRR_MGMT_MAC.xml") %>%
      add_xml_data(L252.AgMAC, "AgMAC") %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_precursors("L252.MAC_an",
                     "L252.AgMAC") ->
      all_aglu_emissions_IRR_MGMT_MAC.xml

    create_xml("all_aglu_emissions_IRR_MGMT_MAC_smth_highTC.xml") %>%
      add_xml_data(L252.AgMAC, "AgMAC") %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_xml_data(L252.AgMAC_smth, "AgMAC") %>%
      add_xml_data(L252.AgMAC_tc %>% filter(tech.year >= 2055), "AgMACTC_allyear", NULL) %>%
      add_xml_data(L252.MAC_an_smth, "MAC") %>%
      add_xml_data(L252.MAC_an_tc %>% filter(tech.year >= 2055), "MACTC_allyear", NULL) %>%
      add_precursors("L252.MAC_an",
                     "L252.MAC_an_smth",
                     "L252.MAC_an_tc",
                     "L252.AgMAC",
                     "L252.AgMAC_smth",
                     "L252.AgMAC_tc") ->
      all_aglu_emissions_IRR_MGMT_MAC_smth_highTC.xml

    create_xml("all_aglu_emissions_IRR_MGMT_MAC_smth_averageTC.xml") %>%
      add_xml_data(L252.AgMAC, "AgMAC") %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_xml_data(L252.AgMAC_smth, "AgMAC") %>%
      add_xml_data(L252.AgMAC_tc_average %>% filter(tech.year >= 2055), "AgMACTC_allyear", NULL) %>%
      add_xml_data(L252.MAC_an_smth, "MAC") %>%
      add_xml_data(L252.MAC_an_tc_average %>% filter(tech.year >= 2055), "MACTC_allyear", NULL) %>%
      add_precursors("L252.MAC_an",
                     "L252.MAC_an_smth",
                     "L252.AgMAC",
                     "L252.AgMAC_smth",
                     "L252.AgMAC_tc_average",
                     "L252.MAC_an_tc_average") ->
      all_aglu_emissions_IRR_MGMT_MAC_smth_averageTC.xml

    create_xml("all_aglu_emissions_IRR_MGMT_MAC_smth_noTC.xml") %>%
      add_xml_data(L252.AgMAC, "AgMAC") %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_xml_data(L252.AgMAC_smth, "AgMAC") %>%
      add_xml_data(L252.MAC_an_smth, "MAC") %>%
      add_precursors("L252.MAC_an",
                     "L252.MAC_an_smth",
                     "L252.AgMAC",
                     "L252.AgMAC_smth") ->
      all_aglu_emissions_IRR_MGMT_MAC_smth_noTC.xml

    create_xml("all_aglu_emissions_IRR_MGMT_MAC_TC.xml") %>%
      add_xml_data(L252.AgMAC, "AgMAC") %>%
      add_xml_data(L252.AgMAC_tc_average, "AgMACTC_allyear", NULL) %>%
      add_xml_data(L252.AgMAC_phaseInFraction, "AgMACTC_allyear_PhaseIn", NULL) %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_xml_data(L252.MAC_an_tc_average, "MACTC_allyear", NULL) %>%
      add_xml_data(L252.MAC_an_phaseInFraction, "MACTC_allyear_PhaseIn", NULL) %>%
      add_precursors("L252.MAC_an",
                     "L252.MAC_an_tc_average",
                     "L252.AgMAC_phaseInFraction",
                     "L252.AgMAC",
                     "L252.AgMAC_tc_average",
                     "L252.MAC_an_phaseInFraction") ->
      all_aglu_emissions_IRR_MGMT_MAC_TC.xml


    return_data(all_aglu_emissions_IRR_MGMT.xml,
                all_aglu_emissions_IRR_MGMT_MAC.xml,
                all_aglu_emissions_IRR_MGMT_MAC_smth_highTC.xml,
                all_aglu_emissions_IRR_MGMT_MAC_smth_averageTC.xml,
                all_aglu_emissions_IRR_MGMT_MAC_smth_noTC.xml,
                all_aglu_emissions_IRR_MGMT_MAC_TC.xml)
  } else {
    stop("Unknown command")
  }
}

