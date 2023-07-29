# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_en_distribution_xml
#'
#' Construct XML data structure for \code{en_distribution.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_distribution.xml}, \code{en_distribution_H2_peaker.xml},
#' \code{en_distribution_H2_peaker_cwf_low_H2.xml}, \code{en_distribution_H2_peaker_cwf_med_H2.xml}, \code{en_distribution_H2_peaker_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_en_distribution.xml.R} (energy XML).
module_energy_batch_en_distribution_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.Supplysector_en",
              "L226.SubsectorLogit_en",
              "L226.SubsectorShrwt_en",
              "L226.SubsectorShrwtFllt_en",
              "L226.SubsectorInterp_en",
              "L226.SubsectorInterpTo_en",
              "L226.StubTech_en",
              "L226.GlobalTechEff_en",
              "L226.GlobalTechCost_en",
              "L226.GlobalTechShrwt_en",
              "L226.StubTechCoef_elecownuse",
              "L226.StubTechCoef_electd",
              "L226.StubTechCoef_gaspipe",
             "L226.SubsectorShrwtFllt_en_cwf_H2_scenarios",
             "L226.SubsectorInterp_en_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_distribution.xml",
             XML = "en_distribution_H2_peaker.xml",
             XML = "en_distribution_H2_peaker_cwf_low_H2.xml",
             XML = "en_distribution_H2_peaker_cwf_med_H2.xml",
             XML = "en_distribution_H2_peaker_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en")
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
    L226.SubsectorShrwt_en <- get_data(all_data, "L226.SubsectorShrwt_en")
    L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en")
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
    L226.SubsectorInterpTo_en <- get_data(all_data, "L226.SubsectorInterpTo_en")
    L226.StubTech_en <- get_data(all_data, "L226.StubTech_en")
    L226.GlobalTechEff_en <- get_data(all_data, "L226.GlobalTechEff_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    L226.StubTechCoef_elecownuse <- get_data(all_data, "L226.StubTechCoef_elecownuse")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")
    L226.StubTechCoef_gaspipe <- get_data(all_data, "L226.StubTechCoef_gaspipe")
    L226.SubsectorShrwtFllt_en_cwf_H2_scenarios <- get_data(all_data, "L226.SubsectorShrwtFllt_en_cwf_H2_scenarios")
    L226.SubsectorInterp_en_cwf_H2_scenarios <- get_data(all_data, "L226.SubsectorInterp_en_cwf_H2_scenarios")

    # ===================================================
    en_distribution_H2_peaker_cwf_low_H2.xml <- en_distribution_H2_peaker_cwf_med_H2.xml <- en_distribution_H2_peaker_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs
    create_xml("en_distribution.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en, "SubsectorLogit") ->
      en_distribution.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L226.SubsectorShrwt_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorShrwt_en, "SubsectorShrwt") ->
        en_distribution.xml
    }

    if(!is.null(L226.SubsectorShrwtFllt_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") ->
        en_distribution.xml
    }

    if(!is.null(L226.SubsectorInterp_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorInterp_en, "SubsectorInterp") ->
        en_distribution.xml
    }

    if(!is.null(L226.SubsectorInterpTo_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorInterpTo_en, "SubsectorInterpTo") ->
        en_distribution.xml
    }

    en_distribution.xml %>%
      add_xml_data(L226.StubTech_en, "StubTech") %>%
      add_xml_data(L226.GlobalTechEff_en, "GlobalTechEff") %>%
      add_xml_data(L226.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L226.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L226.StubTechCoef_elecownuse, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_electd, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_gaspipe, "StubTechCoef") %>%
      add_precursors("L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.StubTech_en",
                     "L226.GlobalTechEff_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_elecownuse",
                     "L226.StubTechCoef_electd",
                     "L226.StubTechCoef_gaspipe") ->
      en_distribution.xml

    create_xml("en_distribution_H2_peaker.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en, "SubsectorLogit") ->
      en_distribution_H2_peaker.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L226.SubsectorShrwt_en)) {
      en_distribution_H2_peaker.xml %>%
        add_xml_data(L226.SubsectorShrwt_en, "SubsectorShrwt") ->
        en_distribution_H2_peaker.xml
    }

    if(!is.null(L226.SubsectorShrwtFllt_en)) {
      en_distribution_H2_peaker.xml %>%
        add_xml_data(L226.SubsectorShrwtFllt_en %>%
                       mutate(share.weight = if_else(supplysector == "backup_electricity" & subsector == "hydrogen",1,share.weight)), "SubsectorShrwtFllt") ->
        en_distribution_H2_peaker.xml
    }

    if(!is.null(L226.SubsectorInterp_en)) {
      en_distribution_H2_peaker.xml %>%
        add_xml_data(L226.SubsectorInterp_en, "SubsectorInterp") ->
        en_distribution_H2_peaker.xml
    }

    if(!is.null(L226.SubsectorInterpTo_en)) {
      en_distribution_H2_peaker.xml %>%
        add_xml_data(L226.SubsectorInterpTo_en, "SubsectorInterpTo") ->
        en_distribution_H2_peaker.xml
    }

    en_distribution_H2_peaker.xml %>%
      add_xml_data(L226.StubTech_en, "StubTech") %>%
      add_xml_data(L226.GlobalTechEff_en, "GlobalTechEff") %>%
      add_xml_data(L226.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L226.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L226.StubTechCoef_elecownuse, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_electd, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_gaspipe, "StubTechCoef") %>%
      add_precursors("L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.StubTech_en",
                     "L226.GlobalTechEff_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_elecownuse",
                     "L226.StubTechCoef_electd",
                     "L226.StubTechCoef_gaspipe") ->
      en_distribution_H2_peaker.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2", "cwf_med_H2", "cwf_high_H2")) {
      L226.SubsectorShrwtFllt_en_cwf_H2_scenarios_sel <- L226.SubsectorShrwtFllt_en_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L226.SubsectorInterp_en_cwf_H2_scenarios_sel <- L226.SubsectorInterp_en_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("en_distribution_H2_peaker_", i, ".xml")

      create_xml(xml_name) %>%
        add_logit_tables_xml(L226.Supplysector_en, "Supplysector") %>%
        add_logit_tables_xml(L226.SubsectorLogit_en, "SubsectorLogit") %>%
        add_xml_data(L226.SubsectorShrwtFllt_en_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L226.SubsectorInterp_en_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        add_xml_data(L226.StubTech_en, "StubTech") %>%
        add_xml_data(L226.GlobalTechEff_en, "GlobalTechEff") %>%
        add_xml_data(L226.GlobalTechCost_en, "GlobalTechCost") %>%
        add_xml_data(L226.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
        add_xml_data(L226.StubTechCoef_elecownuse, "StubTechCoef") %>%
        add_xml_data(L226.StubTechCoef_electd, "StubTechCoef") %>%
        add_xml_data(L226.StubTechCoef_gaspipe, "StubTechCoef") %>%
        add_precursors("L226.Supplysector_en",
                       "L226.SubsectorLogit_en",
                       "L226.SubsectorShrwt_en",
                       "L226.SubsectorShrwtFllt_en_cwf_H2_scenarios",
                       "L226.SubsectorInterp_en_cwf_H2_scenarios",
                       "L226.SubsectorInterpTo_en",
                       "L226.StubTech_en",
                       "L226.GlobalTechEff_en",
                       "L226.GlobalTechCost_en",
                       "L226.GlobalTechShrwt_en",
                       "L226.StubTechCoef_elecownuse",
                       "L226.StubTechCoef_electd",
                       "L226.StubTechCoef_gaspipe") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(en_distribution.xml,
                en_distribution_H2_peaker.xml,
                en_distribution_H2_peaker_cwf_low_H2.xml,
                en_distribution_H2_peaker_cwf_med_H2.xml,
                en_distribution_H2_peaker_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}
