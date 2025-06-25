# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_minerals_resources_xml
#'
#' Construct XML data structure for \code{minerals_resources.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_mineral.xml}. (minerals XML).

module_minerals_resources_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2111.Rsrc",
      "L2111.UnlimitRsrc",
      "L2111.RsrcPrice",
      "L2111.UnlimitRsrcPrice",
      "L2111.SubresourcePriceAdder",
      "L2111.RsrcCalProd",
      "L2111.ReserveCalReserve",
      "L2111.RsrcCurves_minerals",
      "L2111.ResSubresourceProdLifetime",
      "L2111.ResReserveTechLifetime",
      "L2111.ResReserveTechDeclinePhase",
      "L2111.ResReserveTechProfitShutdown",
      "L2111.ResReserveTechInvestmentInput",
      "L2111.ResTechShrwt"
    )

  MODULE_OUTPUTS <-
    c(XML = "minerals_resources.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # ===================================================

    # Produce outputs
    create_xml("minerals_resources.xml") %>%
      add_xml_data(L2111.Rsrc, "Rsrc") %>%
      add_xml_data(L2111.UnlimitRsrc, "UnlimitRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2111.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
      add_xml_data(L2111.SubresourcePriceAdder, "SubresourcePriceAdder") %>%
      add_xml_data(L2111.ReserveCalReserve, "ReserveCalReserve") %>%
      add_xml_data(L2111.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
      add_xml_data(L2111.ResReserveTechInvestmentInput, "ResReserveTechInvestmentInput") %>%
      add_xml_data(L2111.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L2111.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L2111.RsrcPrice, "RsrcPrice") %>%
      add_xml_data(L2111.UnlimitRsrcPrice, "UnlimitRsrcPrice") %>%
      add_xml_data(L2111.RsrcCalProd, "RsrcCalProd") %>%
      add_xml_data(L2111.RsrcCurves_minerals, "RsrcCurves") %>%
      add_xml_data(L2111.ResTechShrwt, "ResTechShrwt") %>%
      add_precursors("L2111.Rsrc",
                   "L2111.UnlimitRsrc",
                   "L2111.RsrcPrice",
                   "L2111.UnlimitRsrcPrice",
                   "L2111.SubresourcePriceAdder",
                   "L2111.RsrcCalProd",
                   "L2111.ReserveCalReserve",
                   "L2111.RsrcCurves_minerals",
                   "L2111.ResSubresourceProdLifetime",
                   "L2111.ResReserveTechLifetime",
                   "L2111.ResReserveTechDeclinePhase",
                   "L2111.ResReserveTechProfitShutdown",
                   "L2111.ResReserveTechInvestmentInput",
                   "L2111.ResTechShrwt") ->
      minerals_resources.xml


  return_data(minerals_resources.xml)
} else {
  stop("Unknown command")
}
}
