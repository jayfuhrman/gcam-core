# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_hydrogen_cwf_xml
#'
#' Construct XML data structure for \code{hydrogen.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen.xml.R} (energy XML).
module_energy_hydrogen_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.SubsectorShrwtFllt_cwf_no_pipeline",
              "L225.GlobalTechShrwt_cwf_no_pipeline"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen_no_pipeline.xml"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    L225.SubsectorShrwtFllt_cwf_no_pipeline <- get_data(all_data, "L225.SubsectorShrwtFllt_cwf_no_pipeline")
    L225.GlobalTechShrwt_cwf_no_pipeline <- get_data(all_data, "L225.GlobalTechShrwt_cwf_no_pipeline")
    # ===================================================

    # Produce outputs
    create_xml("hydrogen_no_pipeline.xml") %>%
      add_xml_data(L225.SubsectorShrwtFllt_cwf_no_pipeline, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.GlobalTechShrwt_cwf_no_pipeline, "GlobalTechShrwt") %>%

      add_precursors("L225.SubsectorShrwtFllt_cwf_no_pipeline",
                     "L225.GlobalTechShrwt_cwf_no_pipeline") ->
      hydrogen_no_pipeline.xml

    return_data(hydrogen_no_pipeline.xml)
  } else {
    stop("Unknown command")
  }
}
