# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_cement_xml
#'
#' Construct XML data structure for \code{cement.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement.xml}, \code{cement_cwf.xml}, \code{cement_cwf_low_H2.xml}, \code{cement_cwf_med_H2.xml}, \code{cement_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_cement_xml.R} (energy XML).
module_energy_batch_cement_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2321.Supplysector_cement",
             "L2321.FinalEnergyKeyword_cement",
             "L2321.SubsectorLogit_cement",
             # "L2321.SubsectorShrwt_cement",
             "L2321.SubsectorShrwtFllt_cement",
             "L2321.SubsectorInterp_cement",
             # "L2321.SubsectorInterpTo_cement",
             "L2321.StubTech_cement",
             "L2321.GlobalTechShrwt_cement",
             "L2321.GlobalTechCoef_cement",
             "L2321.GlobalTechCost_cement",
             "L2321.GlobalTechCapture_cement",
			 "L2321.GlobalTechSCurve_en",
             "L2321.GlobalTechProfitShutdown_en",
             "L2321.StubTechProd_cement",
             "L2321.StubTechCalInput_cement_heat",
             "L2321.StubTechCoef_cement",
             "L2321.PerCapitaBased_cement",
             "L2321.BaseService_cement",
             "L2321.PriceElasticity_cement",
			 "L2321.GlobalTechCoef_cement_cwf",
			 "L2321.StubTechCoef_cement_cwf",
			 "L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios",
			 "L2321.SubsectorInterp_cement_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement.xml",
             XML = "cement_cwf.xml",
             XML = "cement_cwf_low_H2.xml",
             XML = "cement_cwf_med_H2.xml",
             XML = "cement_cwf_high_H2.xml",
             XML = "cement_all_CCS_post2030.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement")
    L2321.FinalEnergyKeyword_cement <- get_data(all_data, "L2321.FinalEnergyKeyword_cement")
    L2321.SubsectorLogit_cement <- get_data(all_data, "L2321.SubsectorLogit_cement")
    #    L2321.SubsectorShrwt_cement <- get_data(all_data, "L2321.SubsectorShrwt_cement")
    L2321.SubsectorShrwtFllt_cement <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement")
    L2321.SubsectorInterp_cement <- get_data(all_data, "L2321.SubsectorInterp_cement")
    #    L2321.SubsectorInterpTo_cement <- get_data(all_data, "L2321.SubsectorInterpTo_cement")
    L2321.StubTech_cement <- get_data(all_data, "L2321.StubTech_cement")
    L2321.GlobalTechShrwt_cement <- get_data(all_data, "L2321.GlobalTechShrwt_cement")
    L2321.GlobalTechCoef_cement <- get_data(all_data, "L2321.GlobalTechCoef_cement")
    L2321.GlobalTechCost_cement <- get_data(all_data, "L2321.GlobalTechCost_cement")
    L2321.GlobalTechCapture_cement <- get_data(all_data, "L2321.GlobalTechCapture_cement")
	L2321.GlobalTechSCurve_en <- get_data(all_data, "L2321.GlobalTechSCurve_en")
    L2321.GlobalTechProfitShutdown_en <- get_data(all_data, "L2321.GlobalTechProfitShutdown_en")
    L2321.StubTechProd_cement <- get_data(all_data, "L2321.StubTechProd_cement")
    L2321.StubTechCalInput_cement_heat <- get_data(all_data, "L2321.StubTechCalInput_cement_heat")
    L2321.StubTechCoef_cement <- get_data(all_data, "L2321.StubTechCoef_cement")
    L2321.PerCapitaBased_cement <- get_data(all_data, "L2321.PerCapitaBased_cement")
    L2321.BaseService_cement <- get_data(all_data, "L2321.BaseService_cement")
    L2321.PriceElasticity_cement <- get_data(all_data, "L2321.PriceElasticity_cement")
    L2321.GlobalTechCoef_cement_cwf <- get_data(all_data, "L2321.GlobalTechCoef_cement_cwf")
    L2321.StubTechCoef_cement_cwf <- get_data(all_data, "L2321.StubTechCoef_cement_cwf")
    L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios")
    L2321.SubsectorInterp_cement_cwf_H2_scenarios <- get_data(all_data, "L2321.SubsectorInterp_cement_cwf_H2_scenarios")
    # ===================================================

    cement_cwf_low_H2.xml <- cement_cwf_med_H2.xml <- cement_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs
    create_xml("cement.xml") %>%
      add_logit_tables_xml(L2321.Supplysector_cement, "Supplysector") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement, "SubsectorLogit") %>%
      #      add_xml_data(L2321.SubsectorShrwt_cement, "SubsectorShrwt") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement, "SubsectorInterp") %>%
      #      add_xml_data(L2321.SubsectorInterpTo_cement, "SubsectorInterpTo") %>%
      add_xml_data(L2321.StubTech_cement, "StubTech") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement, "GlobalTechShrwt") %>%
      add_xml_data(L2321.GlobalTechCoef_cement, "GlobalTechCoef") %>%
	  add_xml_data(L2321.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2321.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2321.GlobalTechCost_cement, "GlobalTechCost") %>%
      add_xml_data(L2321.GlobalTechCapture_cement, "GlobalTechCapture") %>%
      add_xml_data(L2321.StubTechProd_cement, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechCoef_cement, "StubTechCoef") %>%
      add_xml_data(L2321.PerCapitaBased_cement, "PerCapitaBased") %>%
      add_xml_data(L2321.BaseService_cement, "BaseService") %>%
      add_xml_data(L2321.PriceElasticity_cement, "PriceElasticity") %>%
      add_precursors("L2321.Supplysector_cement", "L2321.FinalEnergyKeyword_cement", "L2321.SubsectorLogit_cement",
                     # "L2321.SubsectorShrwt_cement",
                     "L2321.SubsectorShrwtFllt_cement",
                     "L2321.SubsectorInterp_cement",
                     # "L2321.SubsectorInterpTo_cement",
                     "L2321.StubTech_cement","L2321.GlobalTechSCurve_en", "L2321.GlobalTechProfitShutdown_en",
                     "L2321.GlobalTechShrwt_cement", "L2321.GlobalTechCoef_cement", "L2321.GlobalTechCost_cement",
                     "L2321.GlobalTechCapture_cement", "L2321.StubTechProd_cement", "L2321.StubTechCalInput_cement_heat",
                     "L2321.StubTechCoef_cement", "L2321.PerCapitaBased_cement", "L2321.BaseService_cement",
                     "L2321.PriceElasticity_cement") ->
      cement.xml

    create_xml("cement_cwf.xml") %>%
      add_logit_tables_xml(L2321.Supplysector_cement, "Supplysector") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement, "SubsectorLogit") %>%
      #      add_xml_data(L2321.SubsectorShrwt_cement, "SubsectorShrwt") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement, "SubsectorInterp") %>%
      #      add_xml_data(L2321.SubsectorInterpTo_cement, "SubsectorInterpTo") %>%
      add_xml_data(L2321.StubTech_cement, "StubTech") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement, "GlobalTechShrwt") %>%
      add_xml_data(L2321.GlobalTechCoef_cement_cwf, "GlobalTechCoef") %>% # CWF version
      add_xml_data(L2321.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2321.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2321.GlobalTechCost_cement, "GlobalTechCost") %>%
      add_xml_data(L2321.GlobalTechCapture_cement, "GlobalTechCapture") %>%
      add_xml_data(L2321.StubTechProd_cement, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechCoef_cement_cwf, "StubTechCoef") %>% # CWF version
      add_xml_data(L2321.PerCapitaBased_cement, "PerCapitaBased") %>%
      add_xml_data(L2321.BaseService_cement, "BaseService") %>%
      add_xml_data(L2321.PriceElasticity_cement, "PriceElasticity") %>%
      add_precursors("L2321.Supplysector_cement", "L2321.FinalEnergyKeyword_cement", "L2321.SubsectorLogit_cement",
                     # "L2321.SubsectorShrwt_cement",
                     "L2321.SubsectorShrwtFllt_cement",
                     "L2321.SubsectorInterp_cement",
                     # "L2321.SubsectorInterpTo_cement",
                     "L2321.StubTech_cement","L2321.GlobalTechSCurve_en", "L2321.GlobalTechProfitShutdown_en",
                     "L2321.GlobalTechShrwt_cement", "L2321.GlobalTechCoef_cement_cwf", "L2321.GlobalTechCost_cement",
                     "L2321.GlobalTechCapture_cement", "L2321.StubTechProd_cement", "L2321.StubTechCalInput_cement_heat",
                     "L2321.StubTechCoef_cement_cwf", "L2321.PerCapitaBased_cement", "L2321.BaseService_cement",
                     "L2321.PriceElasticity_cement") ->
      cement_cwf.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2", "cwf_med_H2", "cwf_high_H2")) {
      L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios_sel <- L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2321.SubsectorInterp_cement_cwf_H2_scenarios_sel <- L2321.SubsectorInterp_cement_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("cement_", i, ".xml")

      create_xml(xml_name) %>%
        add_logit_tables_xml(L2321.Supplysector_cement, "Supplysector") %>%
        add_xml_data(L2321.FinalEnergyKeyword_cement, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L2321.SubsectorLogit_cement, "SubsectorLogit") %>%
        #      add_xml_data(L2321.SubsectorShrwt_cement, "SubsectorShrwt") %>%
        add_xml_data(L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L2321.SubsectorInterp_cement_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        #      add_xml_data(L2321.SubsectorInterpTo_cement, "SubsectorInterpTo") %>%
        add_xml_data(L2321.StubTech_cement, "StubTech") %>%
        add_xml_data(L2321.GlobalTechShrwt_cement, "GlobalTechShrwt") %>%
        add_xml_data(L2321.GlobalTechCoef_cement_cwf, "GlobalTechCoef") %>% # CWF version
        add_xml_data(L2321.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
        add_xml_data(L2321.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
        add_xml_data(L2321.GlobalTechCost_cement, "GlobalTechCost") %>%
        add_xml_data(L2321.GlobalTechCapture_cement, "GlobalTechCapture") %>%
        add_xml_data(L2321.StubTechProd_cement, "StubTechProd") %>%
        add_xml_data(L2321.StubTechCalInput_cement_heat, "StubTechCalInput") %>%
        add_xml_data(L2321.StubTechCoef_cement_cwf, "StubTechCoef") %>% # CWF version
        add_xml_data(L2321.PerCapitaBased_cement, "PerCapitaBased") %>%
        add_xml_data(L2321.BaseService_cement, "BaseService") %>%
        add_xml_data(L2321.PriceElasticity_cement, "PriceElasticity") %>%
        add_precursors("L2321.Supplysector_cement", "L2321.FinalEnergyKeyword_cement", "L2321.SubsectorLogit_cement",
                       # "L2321.SubsectorShrwt_cement",
                       "L2321.SubsectorShrwtFllt_cement_cwf_H2_scenarios",
                       "L2321.SubsectorInterp_cement_cwf_H2_scenarios",
                       # "L2321.SubsectorInterpTo_cement",
                       "L2321.StubTech_cement","L2321.GlobalTechSCurve_en", "L2321.GlobalTechProfitShutdown_en",
                       "L2321.GlobalTechShrwt_cement", "L2321.GlobalTechCoef_cement_cwf", "L2321.GlobalTechCost_cement",
                       "L2321.GlobalTechCapture_cement", "L2321.StubTechProd_cement", "L2321.StubTechCalInput_cement_heat",
                       "L2321.StubTechCoef_cement_cwf", "L2321.PerCapitaBased_cement", "L2321.BaseService_cement",
                       "L2321.PriceElasticity_cement") %>%
        assign(xml_name, ., envir = curr_env)
    }

    L2321.GlobalTechShrwt_cement_cwf <- L2321.GlobalTechShrwt_cement %>%
      mutate(share.weight = if_else(technology == 'cement' & year > 2030, 0,share.weight))

    create_xml("cement_all_CCS_post2030.xml") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement_cwf, "GlobalTechShrwt") %>%
      add_precursors('L2321.GlobalTechShrwt_cement') ->
      cement_all_CCS_post2030.xml

    return_data(cement.xml, cement_cwf.xml, cement_all_CCS_post2030.xml,
                cement_cwf_low_H2.xml, cement_cwf_med_H2.xml, cement_cwf_high_H2.xml)

  } else {
    stop("Unknown command")
  }
}
