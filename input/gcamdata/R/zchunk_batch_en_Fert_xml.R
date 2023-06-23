# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_en_Fert_xml
#'
#' Construct XML data structure for \code{en_Fert.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_Fert.xml}, \code{en_Fert_cwf.xml}, \code{en_Fert_cwf_low_H2.xml}, \code{en_Fert_cwf_med_H2.xml}, \code{en_Fert_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_en_Fert_xml.R} (energy XML).
module_energy_batch_en_Fert_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2322.Supplysector_Fert",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.StubTech_Fert",
             "L2322.GlobalTechShrwt_Fert",
             "L2322.GlobalTechCoef_Fert",
             "L2322.GlobalTechCost_Fert",
             "L2322.GlobalTechCapture_Fert",
             "L2322.GlobalTechSCurve_Fert",
             "L2322.GlobalTechProfitShutdown_Fert",
             "L2322.StubTechProd_Fert",
             "L2322.StubTechCoef_Fert",
             "L2322.StubTechFixOut_Fert_imp",
             "L2322.StubTechFixOut_Fert_exp",
             "L2322.PerCapitaBased_Fert",
             "L2322.BaseService_Fert",
             "L2322.SubsectorShrwtFllt_Fert_cwf",
             "L2322.SubsectorInterp_Fert_cwf",
             "L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios",
             "L2322.SubsectorInterp_Fert_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_Fert.xml",
             XML = "en_Fert_cwf.xml",
             XML = "en_Fert_cwf_low_H2.xml",
             XML = "en_Fert_cwf_med_H2.xml",
             XML = "en_Fert_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2322.Supplysector_Fert <- get_data(all_data, "L2322.Supplysector_Fert")
    L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert")
    L2322.SubsectorLogit_Fert <- get_data(all_data, "L2322.SubsectorLogit_Fert")
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert")
    L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert")
    L2322.StubTech_Fert <- get_data(all_data, "L2322.StubTech_Fert")
    L2322.GlobalTechShrwt_Fert <- get_data(all_data, "L2322.GlobalTechShrwt_Fert")

    L2322.GlobalTechCoef_Fert <- get_data(all_data, "L2322.GlobalTechCoef_Fert")
    L2322.GlobalTechCost_Fert <- get_data(all_data, "L2322.GlobalTechCost_Fert")
    L2322.GlobalTechCapture_Fert <- get_data(all_data, "L2322.GlobalTechCapture_Fert")
    L2322.GlobalTechSCurve_Fert <- get_data(all_data, "L2322.GlobalTechSCurve_Fert")
    L2322.GlobalTechProfitShutdown_Fert <- get_data(all_data, "L2322.GlobalTechProfitShutdown_Fert")
    L2322.StubTechProd_Fert <- get_data(all_data, "L2322.StubTechProd_Fert")
    L2322.StubTechCoef_Fert <- get_data(all_data, "L2322.StubTechCoef_Fert")
    L2322.StubTechFixOut_Fert_imp <- get_data(all_data, "L2322.StubTechFixOut_Fert_imp")
    L2322.StubTechFixOut_Fert_exp <- get_data(all_data, "L2322.StubTechFixOut_Fert_exp")
    L2322.PerCapitaBased_Fert <- get_data(all_data, "L2322.PerCapitaBased_Fert")
    L2322.BaseService_Fert <- get_data(all_data, "L2322.BaseService_Fert")
    L2322.SubsectorShrwtFllt_Fert_cwf <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_cwf")
    L2322.SubsectorInterp_Fert_cwf <- get_data(all_data, "L2322.SubsectorInterp_Fert_cwf")
    L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios")
    L2322.SubsectorInterp_Fert_cwf_H2_scenarios <- get_data(all_data, "L2322.SubsectorInterp_Fert_cwf_H2_scenarios")

    # ===================================================

    en_Fert_cwf_low_H2.xml <- en_Fert_cwf_med_H2.xml <- en_Fert_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs
    create_xml("en_Fert.xml") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert, "Supplysector") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_Fert, "SubsectorInterp") %>%
      add_xml_data(L2322.StubTech_Fert, "StubTech") %>%
      add_xml_data(L2322.GlobalTechShrwt_Fert, "GlobalTechShrwt") %>%
      add_xml_data(L2322.GlobalTechCoef_Fert, "GlobalTechCoef") %>%
      add_xml_data(L2322.GlobalTechCost_Fert, "GlobalTechCost") %>%
      add_xml_data(L2322.GlobalTechCapture_Fert, "GlobalTechCapture") %>%
      add_xml_data(L2322.GlobalTechSCurve_Fert, "GlobalTechSCurve") %>%
      add_xml_data(L2322.GlobalTechProfitShutdown_Fert, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2322.StubTechProd_Fert, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert, "StubTechCoef") %>%
      add_xml_data(L2322.StubTechFixOut_Fert_imp, "StubTechFixOut") %>%
      add_xml_data(L2322.StubTechFixOut_Fert_exp, "StubTechFixOut") %>%
      add_xml_data(L2322.PerCapitaBased_Fert, "PerCapitaBased") %>%
      add_xml_data(L2322.BaseService_Fert, "BaseService") %>%
      add_precursors("L2322.Supplysector_Fert",
                     "L2322.FinalEnergyKeyword_Fert",
                     "L2322.SubsectorLogit_Fert",
                     "L2322.SubsectorShrwtFllt_Fert",
                     "L2322.SubsectorInterp_Fert",
                     "L2322.StubTech_Fert",
                     "L2322.GlobalTechShrwt_Fert",
                     "L2322.GlobalTechCoef_Fert",
                     "L2322.GlobalTechCost_Fert",
                     "L2322.GlobalTechCapture_Fert",
                     "L2322.GlobalTechSCurve_Fert",
                     "L2322.GlobalTechProfitShutdown_Fert",
                     "L2322.StubTechProd_Fert",
                     "L2322.StubTechCoef_Fert",
                     "L2322.StubTechFixOut_Fert_imp",
                     "L2322.StubTechFixOut_Fert_exp",
                     "L2322.PerCapitaBased_Fert",
                     "L2322.BaseService_Fert") ->
      en_Fert.xml

    create_xml("en_Fert_cwf.xml") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert, "Supplysector") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert_cwf, "SubsectorShrwtFllt") %>% # CWF version
      add_xml_data(L2322.SubsectorInterp_Fert_cwf, "SubsectorInterp") %>% # CWF version
      add_xml_data(L2322.StubTech_Fert, "StubTech") %>%
      add_xml_data(L2322.GlobalTechShrwt_Fert, "GlobalTechShrwt") %>%
      add_xml_data(L2322.GlobalTechCoef_Fert, "GlobalTechCoef") %>%
      add_xml_data(L2322.GlobalTechCost_Fert, "GlobalTechCost") %>%
      add_xml_data(L2322.GlobalTechCapture_Fert, "GlobalTechCapture") %>%
      add_xml_data(L2322.GlobalTechSCurve_Fert, "GlobalTechSCurve") %>%
      add_xml_data(L2322.GlobalTechProfitShutdown_Fert, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2322.StubTechProd_Fert, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert, "StubTechCoef") %>%
      add_xml_data(L2322.StubTechFixOut_Fert_imp, "StubTechFixOut") %>%
      add_xml_data(L2322.StubTechFixOut_Fert_exp, "StubTechFixOut") %>%
      add_xml_data(L2322.PerCapitaBased_Fert, "PerCapitaBased") %>%
      add_xml_data(L2322.BaseService_Fert, "BaseService") %>%
      add_precursors("L2322.Supplysector_Fert",
                     "L2322.FinalEnergyKeyword_Fert",
                     "L2322.SubsectorLogit_Fert",
                     "L2322.SubsectorShrwtFllt_Fert_cwf",
                     "L2322.SubsectorInterp_Fert_cwf",
                     "L2322.StubTech_Fert",
                     "L2322.GlobalTechShrwt_Fert",
                     "L2322.GlobalTechCoef_Fert",
                     "L2322.GlobalTechCost_Fert",
                     "L2322.GlobalTechCapture_Fert",
                     "L2322.GlobalTechSCurve_Fert",
                     "L2322.GlobalTechProfitShutdown_Fert",
                     "L2322.StubTechProd_Fert",
                     "L2322.StubTechCoef_Fert",
                     "L2322.StubTechFixOut_Fert_imp",
                     "L2322.StubTechFixOut_Fert_exp",
                     "L2322.PerCapitaBased_Fert",
                     "L2322.BaseService_Fert") ->
      en_Fert_cwf.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2", "cwf_med_H2", "cwf_high_H2")) {
      L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios_sel <- L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2322.SubsectorInterp_Fert_cwf_H2_scenarios_sel <- L2322.SubsectorInterp_Fert_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("en_Fert_", i, ".xml")

      create_xml(xml_name) %>%
        add_logit_tables_xml(L2322.Supplysector_Fert, "Supplysector") %>%
        add_xml_data(L2322.FinalEnergyKeyword_Fert, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L2322.SubsectorLogit_Fert, "SubsectorLogit") %>%
        add_xml_data(L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L2322.SubsectorInterp_Fert_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        add_xml_data(L2322.StubTech_Fert, "StubTech") %>%
        add_xml_data(L2322.GlobalTechShrwt_Fert, "GlobalTechShrwt") %>%
        add_xml_data(L2322.GlobalTechCoef_Fert, "GlobalTechCoef") %>%
        add_xml_data(L2322.GlobalTechCost_Fert, "GlobalTechCost") %>%
        add_xml_data(L2322.GlobalTechCapture_Fert, "GlobalTechCapture") %>%
        add_xml_data(L2322.GlobalTechSCurve_Fert, "GlobalTechSCurve") %>%
        add_xml_data(L2322.GlobalTechProfitShutdown_Fert, "GlobalTechProfitShutdown") %>%
        add_xml_data(L2322.StubTechProd_Fert, "StubTechProd") %>%
        add_xml_data(L2322.StubTechCoef_Fert, "StubTechCoef") %>%
        add_xml_data(L2322.StubTechFixOut_Fert_imp, "StubTechFixOut") %>%
        add_xml_data(L2322.StubTechFixOut_Fert_exp, "StubTechFixOut") %>%
        add_xml_data(L2322.PerCapitaBased_Fert, "PerCapitaBased") %>%
        add_xml_data(L2322.BaseService_Fert, "BaseService") %>%
        add_precursors("L2322.Supplysector_Fert",
                       "L2322.FinalEnergyKeyword_Fert",
                       "L2322.SubsectorLogit_Fert",
                       "L2322.SubsectorShrwtFllt_Fert_cwf_H2_scenarios",
                       "L2322.SubsectorInterp_Fert_cwf_H2_scenarios",
                       "L2322.StubTech_Fert",
                       "L2322.GlobalTechShrwt_Fert",
                       "L2322.GlobalTechCoef_Fert",
                       "L2322.GlobalTechCost_Fert",
                       "L2322.GlobalTechCapture_Fert",
                       "L2322.GlobalTechSCurve_Fert",
                       "L2322.GlobalTechProfitShutdown_Fert",
                       "L2322.StubTechProd_Fert",
                       "L2322.StubTechCoef_Fert",
                       "L2322.StubTechFixOut_Fert_imp",
                       "L2322.StubTechFixOut_Fert_exp",
                       "L2322.PerCapitaBased_Fert",
                       "L2322.BaseService_Fert") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(en_Fert.xml, en_Fert_cwf.xml, en_Fert_cwf_low_H2.xml, en_Fert_cwf_med_H2.xml, en_Fert_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}
