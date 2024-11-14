# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_distribution_xml
#'
#' Construct XML data structure for \code{en_distribution.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_distribution.xml} \code{elect_td_mineral.xml}. The corresponding file in the
#' original data system was \code{batch_en_distribution.xml.R} (energy XML).
module_energy_en_distribution_xml <- function(command, ...) {
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
              "L226.GlobalTechTrackCapital_en",
              "L226.GlobalTechShrwt_en",
              "L226.StubTechCoef_elecownuse",
              "L226.StubTechCoef_electd",
              "L226.StubTechCoef_gaspipe",
             "L2261.StubTechCost_elect_td",
             "L2261.StubTechCoef_elect_td_mineral",
             "L2261.StubTechLifetime_elect_td",
             "L2261.StubTechSCurve_elect_td",
             "L2261.StubTechProfitShutdown_elect_td"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_distribution.xml",
             XML = "elect_td_mineral.xml"))
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
    L226.GlobalTechTrackCapital_en <- get_data(all_data, "L226.GlobalTechTrackCapital_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    L226.StubTechCoef_elecownuse <- get_data(all_data, "L226.StubTechCoef_elecownuse")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")
    L226.StubTechCoef_gaspipe <- get_data(all_data, "L226.StubTechCoef_gaspipe")

    #Vintaged T&D, with minerals
    L2261.StubTechCost_elect_td <- get_data(all_data, "L2261.StubTechCost_elect_td")
    L2261.StubTechCoef_elect_td_mineral <- get_data(all_data, "L2261.StubTechCoef_elect_td_mineral")
    L2261.StubTechLifetime_elect_td <- get_data(all_data, "L2261.StubTechLifetime_elect_td")
    L2261.StubTechSCurve_elect_td <- get_data(all_data, "L2261.StubTechSCurve_elect_td")
    L2261.StubTechProfitShutdown_elect_td <- get_data(all_data, "L2261.StubTechProfitShutdown_elect_td")

    # Separate tables into:
    #L2261 elect_td_ sectors
    #L226 all others
    #elect_td sectors will now be vintaged and have mineral information. Therefore, it is cleaner to separate into 2 xmls
    #en_distribution.xml for all sectors except elect_td_
    #elect_td_mineral.xml for elect_td sectors
    L2261.Supplysector_elect_td <- L226.Supplysector_en %>%
      filter(grepl("elect_td", supplysector))
    L226.Supplysector_en <- L226.Supplysector_en %>%
      filter(!grepl("elect_td", supplysector))

    L2261.SubsectorLogit_elect_td <- L226.SubsectorLogit_en %>%
      filter(grepl("elect_td", supplysector))
    L226.SubsectorLogit_en <- L226.SubsectorLogit_en %>%
      filter(!grepl("elect_td", supplysector))

    if(!is.null(L226.SubsectorShrwt_en)) {
      L2261.SubsectorShrwt_elect_td <- L226.SubsectorShrwt_en %>%
        filter(grepl("elect_td", supplysector))
      L226.SubsectorShrwt_en <- L226.SubsectorShrwt_en %>%
        filter(!grepl("elect_td", supplysector))
    }

    if(!is.null(L226.SubsectorShrwtFllt_en)) {
      L2261.SubsectorShrwtFllt_elect_td <- L226.SubsectorShrwtFllt_en %>%
        filter(grepl("elect_td", supplysector))
      L226.SubsectorShrwtFllt_en <- L226.SubsectorShrwtFllt_en %>%
        filter(!grepl("elect_td", supplysector))
    }

    if(!is.null(L226.SubsectorInterp_en)) {
      L2261.SubsectorInterp_elect_td <- L226.SubsectorInterp_en %>%
        filter(grepl("elect_td", supplysector))
      L226.SubsectorInterp_en <- L226.SubsectorInterp_en %>%
        filter(!grepl("elect_td", supplysector))
    }

    if(!is.null(L226.SubsectorInterpTo_en)) {
      L2261.SubsectorInterpTo_elect_td <- L226.SubsectorInterpTo_en %>%
        filter(grepl("elect_td", supplysector))
      L226.SubsectorInterpTo_en <- L226.SubsectorInterpTo_en %>%
        filter(!grepl("elect_td", supplysector))
    }

    L2261.StubTech_elect_td <- L226.StubTech_en %>%
      filter(grepl("elect_td", supplysector))
    L226.StubTech_en <- L226.StubTech_en %>%
      filter(!grepl("elect_td", supplysector))


    L2261.GlobalTechEff_elect_td <- L226.GlobalTechEff_en %>%
      filter(grepl("elect_td", sector.name))
    L226.GlobalTechEff_en <- L226.GlobalTechEff_en %>%
      filter(!grepl("elect_td", sector.name))

    L2261.GlobalTechCost_elect_td <- L226.GlobalTechCost_en %>%
      filter(grepl("elect_td", sector.name))
    L226.GlobalTechCost_en <- L226.GlobalTechCost_en %>%
      filter(!grepl("elect_td", sector.name))

    L2261.GlobalTechTrackCapital_elect_td <- L226.GlobalTechTrackCapital_en %>%
      filter(grepl("elect_td", sector.name))
    L226.GlobalTechTrackCapital_en <- L226.GlobalTechTrackCapital_en %>%
      filter(!grepl("elect_td", sector.name))

    L2261.GlobalTechShrwt_elect_td <- L226.GlobalTechShrwt_en %>%
      filter(grepl("elect_td", sector.name))
    L226.GlobalTechShrwt_en <- L226.GlobalTechShrwt_en %>%
      filter(!grepl("elect_td", sector.name))

    # ===================================================

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
      add_node_equiv_xml("input") %>%
      add_xml_data(L226.GlobalTechTrackCapital_en, "GlobalTechTrackCapital") %>%
      add_xml_data(L226.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L226.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L226.StubTechCoef_elecownuse, "StubTechCoef") %>%
      #add_xml_data(L226.StubTechCoef_electd, "StubTechCoef") %>%
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
                     "L226.GlobalTechTrackCapital_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_elecownuse",
                     #"L226.StubTechCoef_electd",
                     "L226.StubTechCoef_gaspipe") ->
      en_distribution.xml

    # Produce outputs
    create_xml("elect_td_mineral.xml") %>%
      add_logit_tables_xml(L2261.Supplysector_elect_td, "Supplysector") %>%
      add_logit_tables_xml(L2261.SubsectorLogit_elect_td, "SubsectorLogit") ->
      elect_td_mineral.xml

    #Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L226.SubsectorShrwt_en)) {
      elect_td_mineral.xml %>%
        add_xml_data(L2261.SubsectorShrwt_elect_td, "SubsectorShrwt") ->
        elect_td_mineral.xml
    }

    if(!is.null(L226.SubsectorShrwtFllt_en)) {
      elect_td_mineral.xml %>%
        add_xml_data(L2261.SubsectorShrwtFllt_elect_td, "SubsectorShrwtFllt") ->
        elect_td_mineral.xml
    }

    if(!is.null(L226.SubsectorInterp_en)) {
      elect_td_mineral.xml %>%
        add_xml_data(L2261.SubsectorInterp_elect_td, "SubsectorInterp") ->
        elect_td_mineral.xml
    }

    if(!is.null(L226.SubsectorInterpTo_en)) {
      elect_td_mineral.xml %>%
        add_xml_data(L2261.SubsectorInterpTo_elect_td, "SubsectorInterpTo") ->
        elect_td_mineral.xml
    }

    elect_td_mineral.xml %>%
      add_xml_data(L2261.StubTech_elect_td, "StubTech") %>%
      add_xml_data(L2261.GlobalTechEff_elect_td, "GlobalTechEff") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2261.GlobalTechTrackCapital_elect_td, "GlobalTechTrackCapital") %>%
      add_xml_data(L2261.GlobalTechCost_elect_td, "GlobalTechCost") %>%
      add_xml_data(L2261.GlobalTechShrwt_elect_td, "GlobalTechShrwt") %>%
      add_xml_data(L226.StubTechCoef_electd, "StubTechCoef") %>%
      add_xml_data(L2261.StubTechCost_elect_td, "StubTechCost") %>%
      add_xml_data(L2261.StubTechCoef_elect_td_mineral, "RegionalTechMineralCurCoef") %>%
      add_xml_data(L2261.StubTechLifetime_elect_td, "StubTechLifetime") %>%
      add_xml_data(L2261.StubTechSCurve_elect_td, "StubTechSCurve") %>%
      add_xml_data(L2261.StubTechProfitShutdown_elect_td, "StubTechProfitShutdown") %>%
      add_precursors("L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.StubTech_en",
                     "L226.GlobalTechEff_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechTrackCapital_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd",
                     "L2261.StubTechCost_elect_td",
                     "L2261.StubTechCoef_elect_td_mineral",
                     "L2261.StubTechLifetime_elect_td",
                     "L2261.StubTechSCurve_elect_td",
                     "L2261.StubTechProfitShutdown_elect_td") ->
        elect_td_mineral.xml

    return_data(en_distribution.xml, elect_td_mineral.xml)
  } else {
    stop("Unknown command")
  }
}
