# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L263.Cstorage
#'
#' Calculate carbon storage resource supply curves, shareweights, technology coefficients and costs, and other carbon storage information.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L263.Rsrc}, \code{L263.UnlimitRsrc}, \code{L263.RsrcCurves_C}, \code{L263.SectorLogitTables[[ curr_table ]]$data}, \code{L263.Supplysector_C}, \code{L263.SubsectorLogitTables[[ curr_table ]]$data}, \code{L263.SubsectorLogit_C}, \code{L263.SubsectorShrwtFllt_C}, \code{L263.StubTech_C}, \code{L263.GlobalTechCoef_C}, \code{L263.GlobalTechCost_C}, \code{L263.GlobalTechShrwt_C}, \code{L263.GlobalTechCost_C_High}, \code{L263.GlobalTechShrwt_C_nooffshore}, \code{L263.RsrcCurves_C_high}, \code{L263.RsrcCurves_C_low}, \code{L263.RsrcCurves_C_lowest}. The corresponding file in the
#' original data system was \code{L263.Cstorage.R} (energy level2).
#' @details The following tables pertaining to carbon storage properties are generated:
#' \itemize{
#'  \item{Carbon storage information}
#'  \item{Unlimited carbon storage information}
#'  \item{Supply curve of carbon storage resources}
#'  \item{High supply curve of onshore carbon storage resources}
#'  \item{Low supply curve of onshore carbon storage resources}
#'  \item{Lowest supply curve of onshore carbon storage resources}
#'  \item{Carbon storage sector information}
#'  \item{Subsector logit exponents of carbon storage sector}
#'  \item{Subsector shareweights of carbon storage sectors}
#'  \item{Identification of stub technologies of carbon storage}
#'  \item{Carbon storage global technology coefficients across base model years}
#'  \item{Carbon storage global technology costs across base model years}
#'  \item{Carbon storage global technology costs across base model years, high price scenario}
#'  \item{Shareweights of carbon storage technologies across base model years}
#'  \item{Shareweights of offshore carbon storage technologies}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter mutate select reframe
#' @importFrom tidyr complete nesting separate_longer_delim
#' @author AJS August 2017
module_energy_L200.MMRV <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/MMRV_cost_regionalised_high",
             FILE = "energy/MMRV_cost_regionalised_mid",
             FILE = "energy/MMRV_cost_regionalised_low",
             FILE = "energy/MMRV_cost_regionalised"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "MMRV_cost_regionalised_high.xml",
             XML = "MMRV_cost_regionalised_mid.xml",
             XML = "MMRV_cost_regionalised_low.xml",
             XML = "MMRV_cost_regionalised.xml"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    MMRV_cost_regionalised <- get_data(all_data, "energy/MMRV_cost_regionalised")
    MMRV_cost_regionalised_high <- get_data(all_data, "energy/MMRV_cost_regionalised_high")
    MMRV_cost_regionalised_mid <- get_data(all_data, "energy/MMRV_cost_regionalised_mid")
    MMRV_cost_regionalised_low <- get_data(all_data, "energy/MMRV_cost_regionalised_low")

    # ===================================================

    # Silence package notes
    . <- available <- capacity.factor <- curr_table <- extractioncost <-
      grade <- logit.type <- minicam.energy.input <- minicam.non.energy.input <-
      `output-unit` <- `price-unit` <- resource <- resource_type <- share.weight <-
      subresource <- subsector <- subsector.name <- supplysector <- technology <-
      value <- year <- region <- resource <- output.unit <- price.unit <-
      market <- logit.exponent <- coefficient <- input.cost <- NULL

    # Energy inputs and coefficients of regional technologies for carbon storage
    # MMRV_cost_regionalised reports MMRV costs in 1975$/kgC.
    MMRV_cost_regionalised %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[['StubTechCost']]) ->
      L200.MMRV # This is a final output table.

    create_xml("MMRV_cost_regionalised.xml") %>%
      add_xml_data(L200.MMRV, "StubTechCost") %>%
      add_precursors("energy/MMRV_cost_regionalised") ->
      MMRV_cost_regionalised.xml

    # MMRV_cost_regionalised reports MMRV costs in 1975$/kgC.
    MMRV_cost_regionalised_high %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[['StubTechCost']]) ->
      L200.MMRV # This is a final output table.

    create_xml("MMRV_cost_regionalised_high.xml") %>%
      add_xml_data(L200.MMRV, "StubTechCost") %>%
      add_precursors("energy/MMRV_cost_regionalised_high") ->
      MMRV_cost_regionalised_high.xml

    # MMRV_cost_regionalised reports MMRV costs in 1975$/kgC.
    MMRV_cost_regionalised_mid %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[['StubTechCost']]) ->
      L200.MMRV # This is a final output table.

    create_xml("MMRV_cost_regionalised_mid.xml") %>%
      add_xml_data(L200.MMRV, "StubTechCost") %>%
      add_precursors("energy/MMRV_cost_regionalised_mid") ->
      MMRV_cost_regionalised_mid.xml

    # MMRV_cost_regionalised reports MMRV costs in 1975$/kgC.
    MMRV_cost_regionalised_low %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[['StubTechCost']]) ->
      L200.MMRV # This is a final output table.

    create_xml("MMRV_cost_regionalised_low.xml") %>%
      add_xml_data(L200.MMRV, "StubTechCost") %>%
      add_precursors("energy/MMRV_cost_regionalised_low") ->
      MMRV_cost_regionalised_low.xml

    return_data(MMRV_cost_regionalised.xml,
                MMRV_cost_regionalised_high.xml,
                MMRV_cost_regionalised_mid.xml,
                MMRV_cost_regionalised_low.xml)
  } else {
    stop("Unknown command")
  }
}
