# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_transportation_UCD_liquids_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_transportation_UCD_ICEPhaseout_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.StubTranTechInterpTo_liquids",
             "L254.StubTranTechShrwt_liquids",
             "L254.FinalEnergyKeyword_trn",
             "L254.Supplysector_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp"))
  } else if(command == driver.DECLARE_OUTPUTS) {

    return(c(XML = "transportation_UCD_ICEPhaseout.xml"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")

    L254.StubTranTechInterpTo_liquids <- get_data(all_data, "L254.StubTranTechInterpTo_liquids") %>%
      left_join(L254.tranSubsectorLogit, by = c("region","supplysector","tranSubsector")) %>%
      filter(!is.na(logit.type))
    L254.StubTranTechShrwt_liquids <- get_data(all_data, "L254.StubTranTechShrwt_liquids") %>%
      left_join(L254.tranSubsectorLogit, by = c("region","supplysector","tranSubsector")) %>%
      filter(!is.na(logit.type))


    L254.Supplysector_trn_SSP  <- L254.Supplysector_trn %>% filter(sce=="CORE") %>%
      left_join(L254.StubTranTechShrwt_liquids %>% distinct(region,supplysector), by = c("region","supplysector")) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]],"logit.type")

    L254.FinalEnergyKeyword_trn_SSP <- L254.FinalEnergyKeyword_trn %>% filter(sce=="CORE") %>%
      left_join(L254.StubTranTechShrwt_liquids %>% distinct(region,supplysector), by = c("region","supplysector")) %>%
      select(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]])

    L254.tranSubsectorLogit_SSP <- L254.tranSubsectorLogit %>% filter(sce=="CORE") %>%
      left_join(L254.StubTranTechShrwt_liquids %>% distinct(region,supplysector,tranSubsector), by = c("region","supplysector","tranSubsector")) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsectorLogit"]],"logit.type")

    L254.tranSubsectorShrwtFllt_SSP <- L254.tranSubsectorShrwtFllt %>%  filter(sce=="CORE") %>%
      left_join(L254.StubTranTechShrwt_liquids %>% distinct(region,supplysector,tranSubsector), by = c("region","supplysector","tranSubsector")) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsectorShrwtFllt"]])

    L254.tranSubsectorInterp_SSP <- L254.tranSubsectorInterp %>%  filter(sce=="CORE") %>%
      left_join(L254.StubTranTechShrwt_liquids %>% distinct(region,supplysector,tranSubsector), by = c("region","supplysector","tranSubsector")) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsectorInterp"]])


    # ===================================================

    ret_data <- c()
    curr_env <- environment()

    included_regions <- c('EU-12',
                          'EU-15',
                          'European Free Trade Association',
                          'China',
                          'South Korea',
                          "Japan",
                          "Taiwan",
                          "Canada",
                          "Australia_NZ")

    for (i in c("ICEPhaseout")){

      xml_name <- paste0("transportation_UCD_", i, ".xml")

      #Create xml
        xml_obj <- create_xml(xml_name) %>%
          add_logit_tables_xml(L254.Supplysector_trn_SSP %>% filter(region %in% included_regions), "Supplysector") %>%
          add_xml_data(L254.FinalEnergyKeyword_trn_SSP %>% filter(region %in% included_regions), "FinalEnergyKeyword") %>%
          add_logit_tables_xml(L254.tranSubsectorLogit_SSP %>% filter(region %in% included_regions), "tranSubsectorLogit", "tranSubsector") %>%
          add_xml_data(L254.tranSubsectorShrwtFllt_SSP %>% filter(region %in% included_regions), "tranSubsectorShrwtFllt") %>%
          add_xml_data(L254.tranSubsectorInterp_SSP %>% filter(region %in% included_regions), "tranSubsectorInterp") %>%

          add_xml_data(L254.StubTranTechInterpTo_liquids %>% filter(region %in% included_regions,
                                                                    from.year == 2025), "DeleteStubTranTechInterpTo") %>%
          add_xml_data(L254.StubTranTechInterpTo_liquids %>% filter(region %in% included_regions,
                                                                    from.year == 2050), "StubTranTechInterpTo") %>%
          add_xml_data(L254.StubTranTechShrwt_liquids %>% filter(region %in% included_regions,
                                                                 year %in% c(2050,2100)), "StubTranTechShrwt") %>%
        add_precursors("L254.StubTranTechShrwt_liquids",
                       "L254.StubTranTechInterpTo_liquids",
                       "L254.FinalEnergyKeyword_trn",
                       "L254.Supplysector_trn",
                       "L254.tranSubsectorLogit",
                       "L254.tranSubsectorShrwtFllt",
                       "L254.tranSubsectorInterp")  %>%
        assign(xml_name, ., envir = curr_env)

      ret_data <- c(ret_data, xml_name)

    }




    return_data(transportation_UCD_ICEPhaseout.xml)

  } else {
    stop("Unknown command")
  }
}
