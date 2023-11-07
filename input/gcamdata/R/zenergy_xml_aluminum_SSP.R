# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_aluminum_incelas_SSP_xml
#'
#' Construct XML data structures for all the \code{aluminum_incelas_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aluminum_incelas_gcam3.xml}, \code{aluminum_incelas_ssp1.xml}, \code{aluminum_incelas_ssp2.xml}, \code{aluminum_incelas_ssp3.xml},
#' \code{aluminum_incelas_ssp4.xml}, \code{aluminum_incelas_ssp5.xml}, \code{aluminum_incelas_gssp1.xml}, \code{aluminum_incelas_gssp2.xml},
#' \code{aluminum_incelas_gssp3.xml}, \code{aluminum_incelas_gssp4.xml}, and \code{aluminum_incelas_gssp5.xml} and \code{aluminum_incelas_cwf.xml}.
module_energy_aluminum_incelas_SSP_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c("GCAM3",
                                paste0("gSSP", 1:5),
                                paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste("L2326.aluminum_incelas", tolower(INCOME_ELASTICITY_INPUTS), sep = "_"),
             "L2326.aluminum_incelas_cwf"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "aluminum_incelas_gcam3.xml",
             XML = "aluminum_incelas_gssp1.xml",
             XML = "aluminum_incelas_gssp2.xml",
             XML = "aluminum_incelas_gssp3.xml",
             XML = "aluminum_incelas_gssp4.xml",
             XML = "aluminum_incelas_gssp5.xml",
             XML = "aluminum_incelas_ssp1.xml",
             XML = "aluminum_incelas_ssp2.xml",
             XML = "aluminum_incelas_ssp3.xml",
             XML = "aluminum_incelas_ssp4.xml",
             XML = "aluminum_incelas_ssp5.xml",
             XML = "aluminum_incelas_cwf.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    aluminum_incelas_gcam3.xml <- aluminum_incelas_ssp1.xml <- aluminum_incelas_ssp2.xml <- aluminum_incelas_ssp3.xml <-
      aluminum_incelas_ssp4.xml <- aluminum_incelas_ssp5.xml<- aluminum_incelas_gssp1.xml<- aluminum_incelas_gssp2.xml<-
      aluminum_incelas_gssp3.xml<- aluminum_incelas_gssp4.xml <- aluminum_incelas_gssp5.xml <- aluminum_incelas_cwf.xml <- NULL

    all_data <- list(...)[[1]]

    # Loop through all the GCAM3, SSP, and gSSP objects and build the corresponding XML structure
    for(iei in INCOME_ELASTICITY_INPUTS) {
      data_obj <- paste0("L2326.aluminum_incelas_", tolower(iei))
      xmlfn <- paste0("aluminum_incelas_",tolower(iei), '.xml')

      create_xml(xmlfn) %>%
        add_xml_data(get_data(all_data, data_obj), "IncomeElasticity") %>%
        add_precursors(paste0("L2326.aluminum_incelas_", tolower(iei))) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    # do the same for the CWF income elasticity file
    L2326.aluminum_incelas_cwf <- get_data(all_data, 'L2326.aluminum_incelas_cwf')
    create_xml("aluminum_incelas_cwf.xml") %>%
      add_xml_data(L2326.aluminum_incelas_cwf, "IncomeElasticity") %>%
      add_precursors("L2326.aluminum_incelas_cwf") ->
      aluminum_incelas_cwf.xml

    return_data(aluminum_incelas_gcam3.xml,
                aluminum_incelas_ssp1.xml, aluminum_incelas_ssp2.xml, aluminum_incelas_ssp3.xml, aluminum_incelas_ssp4.xml, aluminum_incelas_ssp5.xml,
                aluminum_incelas_gssp1.xml, aluminum_incelas_gssp2.xml, aluminum_incelas_gssp3.xml, aluminum_incelas_gssp4.xml, aluminum_incelas_gssp5.xml,
                aluminum_incelas_cwf.xml)
  } else {
    stop("Unknown command")
  }
}
