# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_chemical_xml
#'
#' Construct XML data structure for \code{chemical.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{chemical.xml}, \code{chemical_cwf.xml}, \code{chemical_cwf_low_H2.xml}, \code{chemical_cwf_med_H2.xml}, \code{chemical_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_chemical_xml.R} (energy XML).
module_energy_batch_chemical_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2325.Supplysector_chemical",
             "L2325.FinalEnergyKeyword_chemical",
             "L2325.SubsectorLogit_chemical",
             "L2325.SubsectorShrwtFllt_chemical",
             "L2325.SubsectorInterp_chemical",
             "L2325.StubTech_chemical",
             "L2325.GlobalTechShrwt_chemical",
             "L2325.GlobalTechCoef_chemical",
             "L2325.GlobalTechCost_chemical",
			 "L2325.GlobalTechSCurve_chemical",
			 "L2325.GlobalTechCSeq_ind",
             "L2325.GlobalTechProfitShutdown_chemical",
             "L2325.StubTechProd_chemical",
             "L2325.StubTechCalInput_chemical",
             "L2325.StubTechCoef_chemical",
             "L2325.PerCapitaBased_chemical",
             "L2325.BaseService_chemical",
             "L2325.PriceElasticity_chemical",
             "L2325.GlobalTechCapture_chemical",
             "L2325.GlobalTechEff_chemical",
             "L2325.GlobalTechSecOut_chemical",
			 "L2325.GlobalTechEff_chemical_cwf",
			 "L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios",
			 "L2325.SubsectorInterp_chemical_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "chemical.xml",
             XML = "chemical_cwf.xml",
             XML = "chemical_cwf_low_H2.xml",
             XML = "chemical_cwf_med_H2.xml",
             XML = "chemical_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2325.Supplysector_chemical <- get_data(all_data, "L2325.Supplysector_chemical")
    L2325.FinalEnergyKeyword_chemical <- get_data(all_data, "L2325.FinalEnergyKeyword_chemical")
    L2325.SubsectorLogit_chemical <- get_data(all_data, "L2325.SubsectorLogit_chemical")
    L2325.SubsectorShrwtFllt_chemical <- get_data(all_data, "L2325.SubsectorShrwtFllt_chemical")
    L2325.SubsectorInterp_chemical <- get_data(all_data, "L2325.SubsectorInterp_chemical")
    L2325.StubTech_chemical <- get_data(all_data, "L2325.StubTech_chemical")
    L2325.GlobalTechShrwt_chemical <- get_data(all_data, "L2325.GlobalTechShrwt_chemical")
    L2325.GlobalTechEff_chemical <- get_data(all_data, "L2325.GlobalTechEff_chemical")
    L2325.GlobalTechCoef_chemical <- get_data(all_data, "L2325.GlobalTechCoef_chemical")
    L2325.GlobalTechCost_chemical <- get_data(all_data, "L2325.GlobalTechCost_chemical")
	  L2325.GlobalTechSCurve_chemical <- get_data(all_data, "L2325.GlobalTechSCurve_chemical")
    L2325.GlobalTechProfitShutdown_chemical <- get_data(all_data, "L2325.GlobalTechProfitShutdown_chemical")
    L2325.GlobalTechCapture_chemical <- get_data(all_data, "L2325.GlobalTechCapture_chemical")
    L2325.StubTechProd_chemical <- get_data(all_data, "L2325.StubTechProd_chemical")
    L2325.StubTechCalInput_chemical <- get_data(all_data, "L2325.StubTechCalInput_chemical")
    L2325.StubTechCoef_chemical <- get_data(all_data, "L2325.StubTechCoef_chemical")
    L2325.PerCapitaBased_chemical <- get_data(all_data, "L2325.PerCapitaBased_chemical")
    L2325.BaseService_chemical <- get_data(all_data, "L2325.BaseService_chemical")
    L2325.PriceElasticity_chemical <- get_data(all_data, "L2325.PriceElasticity_chemical")
    L2325.GlobalTechSecOut_chemical <- get_data(all_data, "L2325.GlobalTechSecOut_chemical")
    L2325.GlobalTechCSeq_ind <- get_data(all_data, "L2325.GlobalTechCSeq_ind")
    L2325.GlobalTechEff_chemical_cwf <- get_data(all_data, "L2325.GlobalTechEff_chemical_cwf")
    L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios <- get_data(all_data, "L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios")
    L2325.SubsectorInterp_chemical_cwf_H2_scenarios <- get_data(all_data, "L2325.SubsectorInterp_chemical_cwf_H2_scenarios")
    # ===================================================

    chemical_cwf_low_H2.xml <- chemical_cwf_med_H2.xml <- chemical_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs
    create_xml("chemical.xml") %>%
      add_logit_tables_xml(L2325.Supplysector_chemical, "Supplysector") %>%
      add_xml_data(L2325.FinalEnergyKeyword_chemical, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2325.SubsectorLogit_chemical, "SubsectorLogit") %>%
      add_xml_data(L2325.SubsectorShrwtFllt_chemical, "SubsectorShrwtFllt") %>%
      add_xml_data(L2325.SubsectorInterp_chemical, "SubsectorInterp") %>%
      add_xml_data(L2325.StubTech_chemical, "StubTech") %>%
      add_xml_data(L2325.GlobalTechShrwt_chemical, "GlobalTechShrwt") %>%
      add_xml_data(L2325.GlobalTechEff_chemical, "GlobalTechEff") %>%
      add_xml_data(L2325.GlobalTechCoef_chemical, "GlobalTechCoef") %>%
      add_xml_data(L2325.GlobalTechCost_chemical, "GlobalTechCost") %>%
	    add_xml_data(L2325.GlobalTechSCurve_chemical, "GlobalTechSCurve") %>%
      add_xml_data(L2325.GlobalTechProfitShutdown_chemical, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2325.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_xml_data(L2325.GlobalTechCapture_chemical, "GlobalTechCapture") %>%
      add_xml_data(L2325.StubTechProd_chemical, "StubTechProd") %>%
      add_xml_data(L2325.StubTechCalInput_chemical, "StubTechCalInput") %>%
      add_xml_data(L2325.StubTechCoef_chemical, "StubTechCoef") %>%
      add_xml_data(L2325.PerCapitaBased_chemical, "PerCapitaBased") %>%
      add_xml_data(L2325.BaseService_chemical, "BaseService") %>%
      add_xml_data(L2325.PriceElasticity_chemical, "PriceElasticity") %>%
      add_xml_data(L2325.GlobalTechSecOut_chemical, "GlobalTechSecOut") %>%
      add_precursors("L2325.Supplysector_chemical", "L2325.FinalEnergyKeyword_chemical", "L2325.SubsectorLogit_chemical",
                     "L2325.SubsectorShrwtFllt_chemical","L2325.GlobalTechEff_chemical",
                     "L2325.SubsectorInterp_chemical","L2325.StubTechProd_chemical",
                     "L2325.StubTech_chemical","L2325.StubTechCoef_chemical","L2325.GlobalTechCSeq_ind",
                     "L2325.GlobalTechShrwt_chemical", "L2325.GlobalTechCoef_chemical", "L2325.GlobalTechCost_chemical",
                     "L2325.GlobalTechProfitShutdown_chemical", "L2325.GlobalTechSCurve_chemical",
                     "L2325.StubTechCalInput_chemical","L2325.GlobalTechCapture_chemical",
                     "L2325.PerCapitaBased_chemical", "L2325.BaseService_chemical",
                     "L2325.PriceElasticity_chemical","L2325.GlobalTechSecOut_chemical") ->
      chemical.xml

    create_xml("chemical_cwf.xml") %>%
      add_logit_tables_xml(L2325.Supplysector_chemical, "Supplysector") %>%
      add_xml_data(L2325.FinalEnergyKeyword_chemical, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2325.SubsectorLogit_chemical, "SubsectorLogit") %>%
      add_xml_data(L2325.SubsectorShrwtFllt_chemical, "SubsectorShrwtFllt") %>%
      add_xml_data(L2325.SubsectorInterp_chemical, "SubsectorInterp") %>%
      add_xml_data(L2325.StubTech_chemical, "StubTech") %>%
      add_xml_data(L2325.GlobalTechShrwt_chemical, "GlobalTechShrwt") %>%
      add_xml_data(L2325.GlobalTechEff_chemical_cwf, "GlobalTechEff") %>% # CWF version
      add_xml_data(L2325.GlobalTechCoef_chemical, "GlobalTechCoef") %>%
      add_xml_data(L2325.GlobalTechCost_chemical, "GlobalTechCost") %>%
      add_xml_data(L2325.GlobalTechSCurve_chemical, "GlobalTechSCurve") %>%
      add_xml_data(L2325.GlobalTechProfitShutdown_chemical, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2325.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_xml_data(L2325.GlobalTechCapture_chemical, "GlobalTechCapture") %>%
      add_xml_data(L2325.StubTechProd_chemical, "StubTechProd") %>%
      add_xml_data(L2325.StubTechCalInput_chemical, "StubTechCalInput") %>%
      add_xml_data(L2325.StubTechCoef_chemical, "StubTechCoef") %>%
      add_xml_data(L2325.PerCapitaBased_chemical, "PerCapitaBased") %>%
      add_xml_data(L2325.BaseService_chemical, "BaseService") %>%
      add_xml_data(L2325.PriceElasticity_chemical, "PriceElasticity") %>%
      add_xml_data(L2325.GlobalTechSecOut_chemical, "GlobalTechSecOut") %>%
      add_precursors("L2325.Supplysector_chemical", "L2325.FinalEnergyKeyword_chemical", "L2325.SubsectorLogit_chemical",
                     "L2325.SubsectorShrwtFllt_chemical","L2325.GlobalTechEff_chemical_cwf",
                     "L2325.SubsectorInterp_chemical","L2325.StubTechProd_chemical",
                     "L2325.StubTech_chemical","L2325.StubTechCoef_chemical","L2325.GlobalTechCSeq_ind",
                     "L2325.GlobalTechShrwt_chemical", "L2325.GlobalTechCoef_chemical", "L2325.GlobalTechCost_chemical",
                     "L2325.GlobalTechProfitShutdown_chemical", "L2325.GlobalTechSCurve_chemical",
                     "L2325.StubTechCalInput_chemical","L2325.GlobalTechCapture_chemical",
                     "L2325.PerCapitaBased_chemical", "L2325.BaseService_chemical",
                     "L2325.PriceElasticity_chemical","L2325.GlobalTechSecOut_chemical") ->
      chemical_cwf.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2", "cwf_med_H2", "cwf_high_H2")) {
      L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios_sel <- L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L2325.SubsectorInterp_chemical_cwf_H2_scenarios_sel <- L2325.SubsectorInterp_chemical_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("chemical_", i, ".xml")

      create_xml(xml_name) %>%
        add_logit_tables_xml(L2325.Supplysector_chemical, "Supplysector") %>%
        add_xml_data(L2325.FinalEnergyKeyword_chemical, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L2325.SubsectorLogit_chemical, "SubsectorLogit") %>%
        add_xml_data(L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L2325.SubsectorInterp_chemical_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        add_xml_data(L2325.StubTech_chemical, "StubTech") %>%
        add_xml_data(L2325.GlobalTechShrwt_chemical, "GlobalTechShrwt") %>%
        add_xml_data(L2325.GlobalTechEff_chemical_cwf, "GlobalTechEff") %>% # CWF version
        add_xml_data(L2325.GlobalTechCoef_chemical, "GlobalTechCoef") %>%
        add_xml_data(L2325.GlobalTechCost_chemical, "GlobalTechCost") %>%
        add_xml_data(L2325.GlobalTechSCurve_chemical, "GlobalTechSCurve") %>%
        add_xml_data(L2325.GlobalTechProfitShutdown_chemical, "GlobalTechProfitShutdown") %>%
        add_xml_data(L2325.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
        add_xml_data(L2325.GlobalTechCapture_chemical, "GlobalTechCapture") %>%
        add_xml_data(L2325.StubTechProd_chemical, "StubTechProd") %>%
        add_xml_data(L2325.StubTechCalInput_chemical, "StubTechCalInput") %>%
        add_xml_data(L2325.StubTechCoef_chemical, "StubTechCoef") %>%
        add_xml_data(L2325.PerCapitaBased_chemical, "PerCapitaBased") %>%
        add_xml_data(L2325.BaseService_chemical, "BaseService") %>%
        add_xml_data(L2325.PriceElasticity_chemical, "PriceElasticity") %>%
        add_xml_data(L2325.GlobalTechSecOut_chemical, "GlobalTechSecOut") %>%
        add_precursors("L2325.Supplysector_chemical", "L2325.FinalEnergyKeyword_chemical", "L2325.SubsectorLogit_chemical",
                       "L2325.SubsectorShrwtFllt_chemical_cwf_H2_scenarios","L2325.GlobalTechEff_chemical_cwf",
                       "L2325.SubsectorInterp_chemical_cwf_H2_scenarios","L2325.StubTechProd_chemical",
                       "L2325.StubTech_chemical","L2325.StubTechCoef_chemical","L2325.GlobalTechCSeq_ind",
                       "L2325.GlobalTechShrwt_chemical", "L2325.GlobalTechCoef_chemical", "L2325.GlobalTechCost_chemical",
                       "L2325.GlobalTechProfitShutdown_chemical", "L2325.GlobalTechSCurve_chemical",
                       "L2325.StubTechCalInput_chemical","L2325.GlobalTechCapture_chemical",
                       "L2325.PerCapitaBased_chemical", "L2325.BaseService_chemical",
                       "L2325.PriceElasticity_chemical","L2325.GlobalTechSecOut_chemical") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(chemical.xml, chemical_cwf.xml, chemical_cwf_low_H2.xml, chemical_cwf_med_H2.xml, chemical_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}

