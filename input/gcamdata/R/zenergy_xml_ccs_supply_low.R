# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_ccs_supply_low_xml
#'
#' Construct XML data structure for \code{ccs_supply_low.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ccs_supply_low.xml}. The corresponding file in the
#' original data system was \code{batch_ccs_supply_low.xml.R} (energy XML).
module_energy_ccs_supply_low_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.RsrcCurves_C_low",
             "L261.ResSubresourceProdLifetime",
             "L261.ResReserveTechLifetime",
             "L261.ResReserveTechDeclinePhase",
             "L261.ResReserveTechProfitShutdown"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ccs_supply_low.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.RsrcCurves_C_low <- get_data(all_data, "L261.RsrcCurves_C_low")
    L261.ResSubresourceProdLifetime <- get_data(all_data, "L261.ResSubresourceProdLifetime")
    L261.ResReserveTechLifetime <- get_data(all_data, "L261.ResReserveTechLifetime")
    L261.ResReserveTechDeclinePhase <- get_data(all_data, "L261.ResReserveTechDeclinePhase")
    L261.ResReserveTechProfitShutdown <- get_data(all_data, "L261.ResReserveTechProfitShutdown")

    # ===================================================

    # Produce outputs
    create_xml("ccs_supply_low.xml") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L261.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
      add_xml_data(L261.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L261.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L261.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
      add_xml_data(L261.RsrcCurves_C_low, "RsrcCurves") %>%
      add_precursors("L261.RsrcCurves_C_low",
                     "L261.ResSubresourceProdLifetime",
                     "L261.ResReserveTechLifetime",
                     "L261.ResReserveTechDeclinePhase",
                     "L261.ResReserveTechProfitShutdown") ->
      ccs_supply_low.xml

    return_data(ccs_supply_low.xml)
  } else {
    stop("Unknown command")
  }
}

