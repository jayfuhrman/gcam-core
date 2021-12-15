# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_transportation_EMF_scenarios_xml
#'
#' Construct XML data structure for \code{bio_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{bio_trade.xml}. The corresponding file in the
#' original data system was \code{batch_bio_trade.xml.R} (aglu XML).
module_energy_batch_transportation_EMF_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A54.globaltranTech_shrwt_scenarios",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt"
))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_EMF_ref.xml",
             XML = "transportation_EMF_adv.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A54.globaltranTech_shrwt_scenarios <- get_data(all_data, "energy/A54.globaltranTech_shrwt_scenarios")
    L254.GlobalTranTechInterp <- get_data(all_data, "L254.GlobalTranTechInterp")
    L254.GlobalTranTechShrwt <- get_data(all_data, "L254.GlobalTranTechShrwt")

    L254.GlobalTranTechShrwt_EMF_scenarios <- L254.GlobalTranTechShrwt %>%
      filter(sce == "CORE") %>%
      left_join_error_no_match(A54.globaltranTech_shrwt_scenarios,
                               by = c("sector.name" = "supplysector",
                                      "subsector.name" = "tranSubsector",
                                      "tranTechnology"))

    L254.GlobalTranTechShrwt_EMF_ref <- L254.GlobalTranTechShrwt_EMF_scenarios %>%
      mutate(share.weight = share.weight * reftech) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]])

      L254.GlobalTranTechShrwt_EMF_adv <- L254.GlobalTranTechShrwt_EMF_scenarios %>%
        mutate(share.weight = share.weight * advtech) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]])

    # ===================================================
    # Rename tibble columns to match header info.

    # Produce outputs
    create_xml("transportation_EMF_ref.xml") %>%
      add_xml_data(L254.GlobalTranTechShrwt_EMF_ref, "GlobalTranTechShrwt") %>%
      add_precursors("energy/A54.globaltranTech_shrwt_scenarios",
                     "L254.GlobalTranTechInterp",
                     "L254.GlobalTranTechShrwt") ->
      transportation_EMF_ref.xml

    create_xml("transportation_EMF_adv.xml") %>%
      add_xml_data(L254.GlobalTranTechShrwt_EMF_adv, "GlobalTranTechShrwt") %>%
      add_precursors("energy/A54.globaltranTech_shrwt_scenarios",
                     "L254.GlobalTranTechInterp",
                     "L254.GlobalTranTechShrwt") ->
      transportation_EMF_adv.xml

    return_data(transportation_EMF_ref.xml,
                transportation_EMF_adv.xml)
  } else {
    stop("Unknown command")
  }
}
