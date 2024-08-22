# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_transportation_UCD_CORE_cwf_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_transportation_UCD_CORE_cwf_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.GlobalTranTechInterp_cwf_low_h2",
             "L254.GlobalTranTechShrwt_cwf_low_h2",
             "L254.GlobalTranTechInterp_cwf",
             "L254.GlobalTranTechShrwt_cwf"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    #xml_files<- c("transportation_UCD_CORE.xml","transportation_UCD_SSP1.xml","transportation_UCD_SSP3.xml","transportation_UCD_SSP5.xml","transportation_UCD_highEV.xml")

    return(c(XML = "transportation_UCD_cwf_low_H2.xml",
             XML = "transportation_cwf_high_en_demand.xml"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs

    L254.GlobalTranTechInterp_cwf_low_h2 <- get_data(all_data, "L254.GlobalTranTechInterp_cwf_low_h2")
    L254.GlobalTranTechShrwt_cwf_low_h2 <- get_data(all_data, "L254.GlobalTranTechShrwt_cwf_low_h2")
    L254.GlobalTranTechInterp_cwf <- get_data(all_data, "L254.GlobalTranTechInterp_cwf") %>% filter(sce == 'CWF_high')
    L254.GlobalTranTechShrwt_cwf <- get_data(all_data, "L254.GlobalTranTechShrwt_cwf") %>% filter(sce == 'CWF_high')

    # ===================================================
    curr_env <- environment()

    # Produce outputs
    #Create xmls
    create_xml("transportation_UCD_cwf_low_H2.xml") %>%

      add_xml_data(L254.GlobalTranTechInterp_cwf_low_h2, "GlobalTranTechInterp") %>%
      add_xml_data(L254.GlobalTranTechShrwt_cwf_low_h2, "GlobalTranTechShrwt") %>%
      add_precursors("L254.GlobalTranTechInterp_cwf_low_h2",
                     "L254.GlobalTranTechShrwt_cwf_low_h2") ->
      transportation_UCD_cwf_low_H2.xml

    # has all technology assumptions re: ZEV uptake + fossil phaseout but no demand reduction
    create_xml("transportation_cwf_high_en_demand.xml") %>%
      add_xml_data(L254.GlobalTranTechInterp_cwf, "GlobalTranTechInterp") %>%
      add_xml_data(L254.GlobalTranTechShrwt_cwf, "GlobalTranTechShrwt") %>%
      add_precursors("L254.GlobalTranTechInterp_cwf",
                     "L254.GlobalTranTechShrwt_cwf") ->
      transportation_cwf_high_en_demand.xml

    return_data(transportation_UCD_cwf_low_H2.xml,
                transportation_cwf_high_en_demand.xml)

  } else {
    stop("Unknown command")
  }
}
