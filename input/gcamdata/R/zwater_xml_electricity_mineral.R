# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_electricity_mineral_xml
#'
#' Construct XML data structure for \code{electricity_water.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water.xml.R} (water XML).
module_water_electricity_mineral_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(
      "L2233.Regionaltech_mineral_coef_constance_final",
      "L2233.Globaltech_mineral_coef_constance_final",
      "L2233.Regional_Globaltech_mineral_coef_constance_Yb",
      "L2233.Regionaltech_mineral_PMult",
      "L2233.Globaltech_mineral_PMult",
      "L2233.Regional_Globaltech_mineral_Yb_PMult")

  MODULE_OUTPUTS <-
    c(XML = "electricity_mineral.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Silence package checks
    technology <- NULL

    # ===================================================

    # Produce outputs
    create_xml("electricity_mineral.xml") %>%

      add_xml_data(L2233.Regionaltech_mineral_coef_constance_final, "RegionalStubTechMineralCurCoefAllYr") %>%
      add_xml_data(L2233.Globaltech_mineral_coef_constance_final, "GlobalTechMineralCurCoefAllYr") %>%
      add_xml_data(L2233.Regional_Globaltech_mineral_coef_constance_Yb, "RegionalStubTechMineralCurCoefAllYr") %>%
      add_xml_data(L2233.Regionaltech_mineral_PMult, "StubCaloriePriceConv") %>%
      add_xml_data(L2233.Globaltech_mineral_PMult, "GlobalTechInputPMult") %>%
      add_xml_data(L2233.Regional_Globaltech_mineral_Yb_PMult, "StubCaloriePriceConv") %>%

      add_precursors(MODULE_INPUTS) ->
      electricity_mineral.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
