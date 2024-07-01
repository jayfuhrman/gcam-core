# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_paper_xml
#'
#' Construct XML data structure for \code{paper.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paper.xml}. The corresponding file in the
#' original data system was \code{batch_paper_xml.R} (energy XML).
module_energy_paper_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2327.GlobalTechCoef_paper_cwf",
             "L2327.GlobalTechSecOut_paper_cwf"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "paper_cwf.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2327.GlobalTechCoef_paper_cwf <- get_data(all_data, "L2327.GlobalTechCoef_paper_cwf")
    L2327.GlobalTechSecOut_paper_cwf <- get_data(all_data, "L2327.GlobalTechSecOut_paper_cwf")

    # ===================================================

    # Produce outputs
    create_xml("paper_cwf.xml") %>%
      add_xml_data(L2327.GlobalTechCoef_paper_cwf, "GlobalTechCoef") %>%
      add_xml_data(L2327.GlobalTechSecOut_paper_cwf, "GlobalTechSecOut") %>%
      add_precursors("L2327.GlobalTechCoef_paper_cwf","L2327.GlobalTechSecOut_paper_cwf") ->
      paper_cwf.xml

    return_data(paper_cwf.xml)
  } else {
    stop("Unknown command")
  }
}
