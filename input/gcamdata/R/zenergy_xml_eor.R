#' module_eor_xml
#'
#' Construct XML data structure for \code{eor.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{eor.xml}.
module_eor_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "eor.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")

    EOR_RSRC_NAME <- "CO2_For_EOR"
    # http://www.uigi.com/co2_conv.html
    CONV_CO2_Mcf_MT <- 0.05189
    CONV_BBLD_EJ   <- 6.119 * 1e-6 # billion barrels a day to EJ

    # NETL: The cost of naturally sourced CO2 is "roughly $10-15 per metric ton"
    # https://www.netl.doe.gov/file%20library/research/oil-gas/CO2_EOR_Primer.pdf
    CO2_EOR_PRICE <- 3 * emissions.CONV_C_CO2 / CONV_CO2_Mcf_MT * gdp_deflator(1990, 2016)
    # OGJ: The volume of CO2 purchased per incremental barrel is typically 4-5 Mcf,
    # although 20-25% of the CO2 is often recycled after the initial phase of injection.
    # https://www.ogj.com/articles/print/volume-92/issue-6/in-this-issue/production/co2-for-eor-is-plentiful-but-tied-to-oil-price.html
    EOR_OIL_COEF <- 5 * CONV_CO2_Mcf_MT / emissions.CONV_C_CO2 / CONV_BBLD_EJ / 1e3
    NATURAL_CO2_MIDP <- CO2_EOR_PRICE * 2.0
    NATURAL_CO2_MAX_RSRC <- 1000 # ??
    NATURAL_CO2_EXP <- 3.0 # ??

    # ===================================================

    # ===================================================
    # Natural CO2 supply curve
    # ===================================================
    GCAM_region_names %>%
      select(region) %>%
      mutate(renewresource = EOR_RSRC_NAME) %>%
      mutate(output.unit = "MTC", price.unit = "1990$/tC") %>%
      mutate(market = region) ->
      RenewRsrc_eor

    RenewRsrc_eor %>%
      select(region, renewresource) %>%
      tidyr::expand(., ., year = MODEL_BASE_YEARS) %>%
      mutate(price = CO2_EOR_PRICE) ->
      RenewRsrcPrice_eor

    RenewRsrc_eor %>%
      select(region, renewresource) %>%
      mutate(smooth.renewable.subresource = renewresource) %>%
      mutate(year.fillout = MODEL_BASE_YEARS[1]) %>%
      mutate(maxSubResource = NATURAL_CO2_MAX_RSRC) %>%
      mutate(mid.price = NATURAL_CO2_MIDP) %>%
      mutate(curve.exponent = NATURAL_CO2_EXP) ->
      SmthRenewRsrcCurves_eor

    RenewRsrc_eor %>%
      rename(resource = renewresource) %>%
      mutate(subresource = resource, technology = resource, share.weight = 1) %>%
      tidyr::expand(., ., year = MODEL_YEARS) %>%
      select(!!!LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      ResTechShrwt_eor

    # ===================================================
    # EOR input into crude oil
    # ===================================================
    GCAM_region_names %>%
      select(region) %>%
      mutate(resource = "crude oil") %>%
      mutate(`reserve.subresource` = resource) %>%
      mutate(`resource.reserve.technology` = resource) %>%
      tidyr::expand(., ., year = MODEL_YEARS) %>%
      mutate(`eor-coef` = EOR_OIL_COEF) ->
      ResReserveTechEORCoef

    ResReserveTechEORCoef %>%
      select(-`eor-coef`) %>%
      mutate(minicam.energy.input = EOR_RSRC_NAME) %>%
      mutate(`price-unit-conversion` = gdp_deflator(1975, 1990) / CONV_T_MT * CONV_GJ_EJ) ->
      ResReserveTechInputPMult

    # ===================================================
    # EOR from carbon storage market
    # ===================================================
    GCAM_region_names %>%
      select(region) %>%
      mutate(supplysector = "carbon-storage") %>%
      mutate(subsector = "EOR") %>%
      mutate(year.fillout = MODEL_BASE_YEARS[1]) %>%
      mutate(share.weight = 1.0) ->
      SubsectorShrwtFllt_Cstorage

    SubsectorShrwtFllt_Cstorage %>%
      rename(logit.year.fillout = year.fillout) %>%
      rename(logit.exponent = share.weight) %>%
      mutate(logit.exponent = -3) %>%
      mutate(logit.type = NA) -> # no tech logit competition so doesn't matter
      SubsectorLogit_Cstorage

    SubsectorShrwtFllt_Cstorage %>%
      mutate(stub.technology = "EOR") %>%
      select(!!!LEVEL2_DATA_NAMES[["StubTech"]]) ->
      StubTech_Cstorage

    SubsectorShrwtFllt_Cstorage %>%
      slice(1) %>%
      tibble::add_column(stub.technology = "EOR", .after="subsector") %>%
      tidyr::expand(., ., year = MODEL_YEARS) %>%
      select(supplysector:stub.technology, year, share.weight) ->
      GlobalTechShrwt_Cstorage
    names(GlobalTechShrwt_Cstorage) <- LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]

    GlobalTechShrwt_Cstorage %>%
      select(-share.weight) %>%
      mutate(res.secondary.output = EOR_RSRC_NAME) %>%
      mutate(output.ratio = 1.0) ->
      GlobalTechRESSecOut_Cstorage

    create_xml("eor.xml") %>%
      add_xml_data(RenewRsrc_eor, "RenewRsrc") %>%
      add_xml_data(RenewRsrcPrice_eor, "RenewRsrcPrice") %>%
      add_xml_data(SmthRenewRsrcCurves_eor, "SmthRenewRsrcCurves") %>%
      add_xml_data(ResReserveTechEORCoef, "ResReserveTechEORCoef") %>%
      add_xml_data(ResReserveTechInputPMult, "ResReserveTechInputPMult") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(ResTechShrwt_eor, "ResTechShrwt") %>%
      add_xml_data(SubsectorShrwtFllt_Cstorage, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(SubsectorLogit_Cstorage, "SubsectorLogit") %>%
      add_xml_data(StubTech_Cstorage, "StubTech") %>%
      add_xml_data(GlobalTechShrwt_Cstorage, "GlobalTechShrwt") %>%
      add_xml_data(GlobalTechRESSecOut_Cstorage, "GlobalTechRESSecOut") %>%
      add_precursors("common/GCAM_region_names") ->
      eor.xml

    return_data(eor.xml)
  } else {
    stop("Unknown command")
  }
}
