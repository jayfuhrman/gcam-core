# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_other_industry_xml
#'
#' Construct XML data structure for \code{other_industry.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{other_industry.xml}, \code{other_industry_cwf.xml},
#' \code{other_industry_cwf_low_H2.xml}, \code{other_industry_cwf_med_H2.xml},
#' \code{other_industry_cwf_high_H2.xml}. The corresponding file in the
#' original data system was \code{batch_industry_xml.R} (energy XML).
module_energy_batch_other_industry_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.SubsectorLogit_ind",
             "L232.FinalEnergyKeyword_ind",
             # "L232.SubsectorShrwt_ind",
             "L232.SubsectorInterp_ind",
             "L232.SubsectorInterp_ind_low_fossil",
             # "L232.SubsectorInterpTo_ind",
             "L232.StubTech_ind",
             "L232.GlobalTechShrwt_ind",
             "L232.StubTechInterp_ind",
             "L232.GlobalTechEff_ind",
             "L232.GlobalTechCoef_ind",
             "L232.GlobalTechCost_ind",
             "L232.GlobalTechSecOut_ind",
             "L232.GlobalTechCSeq_ind",
             "L232.StubTechCalInput_indenergy",
             "L232.GlobalTechSCurve_en",
             "L232.GlobalTechProfitShutdown_en",
             "L232.StubTechCalInput_indfeed",
             "L232.StubTechProd_industry",
             "L232.StubTechCoef_industry",
             "L232.FuelPrefElast_indenergy",
             "L232.PerCapitaBased_ind",
             "L232.PriceElasticity_ind",
             "L232.BaseService_ind",
             "L232.SubsectorShrwtFllt_ind",
             "L232.SubsectorShrwtFllt_ind_low_fossil",
             "L232.Supplysector_ind",
             "L232.GlobalTechEff_ind_cwf",
             "L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios",
             "L232.SubsectorInterp_ind_cwf_H2_scenarios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "other_industry.xml",
             XML = "other_industry_cwf.xml",
             XML = "other_industry_cwf_low_fossil.xml",
             XML = "other_industry_cwf_low_H2.xml",
             XML = "other_industry_cwf_med_H2.xml",
             XML = "other_industry_cwf_high_H2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.SubsectorLogit_ind <- get_data(all_data, "L232.SubsectorLogit_ind")
    L232.FinalEnergyKeyword_ind <- get_data(all_data, "L232.FinalEnergyKeyword_ind")
    # L232.SubsectorShrwt_ind <- get_data(all_data, "L232.SubsectorShrwt_ind")
    L232.SubsectorInterp_ind <- get_data(all_data, "L232.SubsectorInterp_ind")
    L232.SubsectorInterp_ind_low_fossil <- get_data(all_data, "L232.SubsectorInterp_ind_low_fossil")
    # L232.SubsectorInterpTo_ind <- get_data(all_data, "L232.SubsectorInterpTo_ind")
    L232.StubTech_ind <- get_data(all_data, "L232.StubTech_ind")
    L232.GlobalTechShrwt_ind <- get_data(all_data, "L232.GlobalTechShrwt_ind")
    L232.StubTechInterp_ind <- get_data(all_data, "L232.StubTechInterp_ind")
    L232.GlobalTechEff_ind <- get_data(all_data, "L232.GlobalTechEff_ind")
    L232.GlobalTechCoef_ind <- get_data(all_data, "L232.GlobalTechCoef_ind")
    L232.GlobalTechCost_ind <- get_data(all_data, "L232.GlobalTechCost_ind")
    L232.GlobalTechSecOut_ind <- get_data(all_data, "L232.GlobalTechSecOut_ind")
    L232.GlobalTechCSeq_ind <- get_data(all_data, "L232.GlobalTechCSeq_ind")
    L232.StubTechCalInput_indenergy <- get_data(all_data, "L232.StubTechCalInput_indenergy")
    L232.StubTechCalInput_indfeed <- get_data(all_data, "L232.StubTechCalInput_indfeed")
    L232.StubTechProd_industry <- get_data(all_data, "L232.StubTechProd_industry")
    L232.GlobalTechSCurve_en <- get_data(all_data, "L232.GlobalTechSCurve_en")
    L232.GlobalTechProfitShutdown_en <- get_data(all_data, "L232.GlobalTechProfitShutdown_en")
    L232.StubTechCoef_industry <- get_data(all_data, "L232.StubTechCoef_industry")
    L232.FuelPrefElast_indenergy <- get_data(all_data, "L232.FuelPrefElast_indenergy")
    L232.PerCapitaBased_ind <- get_data(all_data, "L232.PerCapitaBased_ind")
    L232.PriceElasticity_ind <- get_data(all_data, "L232.PriceElasticity_ind")
    L232.BaseService_ind <- get_data(all_data, "L232.BaseService_ind")
    L232.SubsectorShrwtFllt_ind <- get_data(all_data, "L232.SubsectorShrwtFllt_ind")
    L232.SubsectorShrwtFllt_ind_low_fossil <- get_data(all_data, "L232.SubsectorShrwtFllt_ind_low_fossil")
    L232.Supplysector_ind <- get_data(all_data, "L232.Supplysector_ind")
    L232.GlobalTechEff_ind_cwf <- get_data(all_data, "L232.GlobalTechEff_ind_cwf")
    L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios <- get_data(all_data, "L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios")
    L232.SubsectorInterp_ind_cwf_H2_scenarios <- get_data(all_data, "L232.SubsectorInterp_ind_cwf_H2_scenarios")

    # ===================================================
    other_industry_cwf_low_H2.xml <- other_industry_cwf_med_H2.xml <- other_industry_cwf_high_H2.xml <- NULL # silence package check notes

    curr_env <- environment()

    # Produce outputs
    create_xml("other_industry.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind, "Supplysector") %>%
      add_logit_tables_xml(L232.SubsectorLogit_ind, "SubsectorLogit") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.FinalEnergyKeyword_ind, "FinalEnergyKeyword") %>%
      # add_xml_data(L232.SubsectorShrwt_ind, "SubsectorShrwt") %>%
      add_xml_data(L232.SubsectorInterp_ind, "SubsectorInterp") %>%
      # add_xml_data(L232.SubsectorInterpTo_ind, "SubsectorInterpTo") %>%
      add_xml_data(L232.StubTech_ind, "StubTech") %>%
      add_xml_data(L232.GlobalTechShrwt_ind, "GlobalTechShrwt") %>%
      add_xml_data(L232.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L232.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L232.StubTechInterp_ind, "StubTechInterp") %>%
      add_xml_data(L232.GlobalTechEff_ind, "GlobalTechEff") %>%
      add_xml_data(L232.GlobalTechCoef_ind, "GlobalTechCoef") %>%
      add_xml_data(L232.GlobalTechCost_ind, "GlobalTechCost") %>%
      add_xml_data(L232.GlobalTechSecOut_ind, "GlobalTechSecOut") %>%
      add_xml_data(L232.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_xml_data(L232.StubTechCalInput_indenergy, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechCalInput_indfeed, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechProd_industry, "StubTechProd") %>%
      add_xml_data(L232.StubTechCoef_industry, "StubTechCoef") %>%
      add_xml_data(L232.FuelPrefElast_indenergy, "FuelPrefElast") %>%
      add_xml_data(L232.PerCapitaBased_ind, "PerCapitaBased") %>%
      add_xml_data(L232.PriceElasticity_ind, "PriceElasticity") %>%
      add_xml_data(L232.BaseService_ind, "BaseService") %>%
      add_precursors("L232.SubsectorLogit_ind", "L232.FinalEnergyKeyword_ind", #"L232.SubsectorShrwt_ind",
                     "L232.SubsectorInterp_ind", #"L232.SubsectorInterpTo_ind",
                     "L232.StubTech_ind",
                     "L232.GlobalTechShrwt_ind", "L232.StubTechInterp_ind", "L232.GlobalTechEff_ind",
                     "L232.GlobalTechCoef_ind", "L232.GlobalTechCost_ind", "L232.GlobalTechSecOut_ind",
                     "L232.GlobalTechCSeq_ind", "L232.StubTechCalInput_indenergy", "L232.StubTechCalInput_indfeed",
                     "L232.GlobalTechSCurve_en", "L232.GlobalTechProfitShutdown_en",
                     "L232.StubTechProd_industry", "L232.StubTechCoef_industry", "L232.FuelPrefElast_indenergy",
                     "L232.PerCapitaBased_ind", "L232.PriceElasticity_ind", "L232.BaseService_ind",
                     "L232.SubsectorShrwtFllt_ind", "L232.Supplysector_ind") ->
      other_industry.xml

    create_xml("other_industry_cwf.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind, "Supplysector") %>%
      add_logit_tables_xml(L232.SubsectorLogit_ind, "SubsectorLogit") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.FinalEnergyKeyword_ind, "FinalEnergyKeyword") %>%
      # add_xml_data(L232.SubsectorShrwt_ind, "SubsectorShrwt") %>%
      add_xml_data(L232.SubsectorInterp_ind, "SubsectorInterp") %>%
      # add_xml_data(L232.SubsectorInterpTo_ind, "SubsectorInterpTo") %>%
      add_xml_data(L232.StubTech_ind, "StubTech") %>%
      add_xml_data(L232.GlobalTechShrwt_ind, "GlobalTechShrwt") %>%
      add_xml_data(L232.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L232.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L232.StubTechInterp_ind, "StubTechInterp") %>%
      add_xml_data(L232.GlobalTechEff_ind_cwf, "GlobalTechEff") %>% # CWF version
      add_xml_data(L232.GlobalTechCoef_ind, "GlobalTechCoef") %>%
      add_xml_data(L232.GlobalTechCost_ind, "GlobalTechCost") %>%
      add_xml_data(L232.GlobalTechSecOut_ind, "GlobalTechSecOut") %>%
      add_xml_data(L232.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_xml_data(L232.StubTechCalInput_indenergy, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechCalInput_indfeed, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechProd_industry, "StubTechProd") %>%
      add_xml_data(L232.StubTechCoef_industry, "StubTechCoef") %>%
      add_xml_data(L232.FuelPrefElast_indenergy, "FuelPrefElast") %>%
      add_xml_data(L232.PerCapitaBased_ind, "PerCapitaBased") %>%
      add_xml_data(L232.PriceElasticity_ind, "PriceElasticity") %>%
      add_xml_data(L232.BaseService_ind, "BaseService") %>%
      add_precursors("L232.SubsectorLogit_ind", "L232.FinalEnergyKeyword_ind", #"L232.SubsectorShrwt_ind",
                     "L232.SubsectorInterp_ind", #"L232.SubsectorInterpTo_ind",
                     "L232.StubTech_ind",
                     "L232.GlobalTechShrwt_ind", "L232.StubTechInterp_ind", "L232.GlobalTechEff_ind_cwf",
                     "L232.GlobalTechCoef_ind", "L232.GlobalTechCost_ind", "L232.GlobalTechSecOut_ind",
                     "L232.GlobalTechCSeq_ind", "L232.StubTechCalInput_indenergy", "L232.StubTechCalInput_indfeed",
                     "L232.GlobalTechSCurve_en", "L232.GlobalTechProfitShutdown_en",
                     "L232.StubTechProd_industry", "L232.StubTechCoef_industry", "L232.FuelPrefElast_indenergy",
                     "L232.PerCapitaBased_ind", "L232.PriceElasticity_ind", "L232.BaseService_ind",
                     "L232.SubsectorShrwtFllt_ind", "L232.Supplysector_ind") ->
      other_industry_cwf.xml

    create_xml("other_industry_cwf_low_fossil.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind, "Supplysector") %>%
      add_logit_tables_xml(L232.SubsectorLogit_ind, "SubsectorLogit") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_low_fossil, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.FinalEnergyKeyword_ind, "FinalEnergyKeyword") %>%
      # add_xml_data(L232.SubsectorShrwt_ind, "SubsectorShrwt") %>%
      add_xml_data(L232.SubsectorInterp_ind_low_fossil, "SubsectorInterp") %>%
      # add_xml_data(L232.SubsectorInterpTo_ind, "SubsectorInterpTo") %>%
      add_xml_data(L232.StubTech_ind, "StubTech") %>%
      add_xml_data(L232.GlobalTechShrwt_ind, "GlobalTechShrwt") %>%
      add_xml_data(L232.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L232.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L232.StubTechInterp_ind, "StubTechInterp") %>%
      add_xml_data(L232.GlobalTechEff_ind_cwf, "GlobalTechEff") %>% # CWF version
      add_xml_data(L232.GlobalTechCoef_ind, "GlobalTechCoef") %>%
      add_xml_data(L232.GlobalTechCost_ind, "GlobalTechCost") %>%
      add_xml_data(L232.GlobalTechSecOut_ind, "GlobalTechSecOut") %>%
      add_xml_data(L232.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_xml_data(L232.StubTechCalInput_indenergy, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechCalInput_indfeed, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechProd_industry, "StubTechProd") %>%
      add_xml_data(L232.StubTechCoef_industry, "StubTechCoef") %>%
      add_xml_data(L232.FuelPrefElast_indenergy, "FuelPrefElast") %>%
      add_xml_data(L232.PerCapitaBased_ind, "PerCapitaBased") %>%
      add_xml_data(L232.PriceElasticity_ind, "PriceElasticity") %>%
      add_xml_data(L232.BaseService_ind, "BaseService") %>%
      add_precursors("L232.SubsectorLogit_ind", "L232.FinalEnergyKeyword_ind", #"L232.SubsectorShrwt_ind",
                     "L232.SubsectorInterp_ind_low_fossil", #"L232.SubsectorInterpTo_ind",
                     "L232.StubTech_ind",
                     "L232.GlobalTechEff_ind_cwf",
                     "L232.GlobalTechCoef_ind", "L232.GlobalTechCost_ind", "L232.GlobalTechSecOut_ind",
                     "L232.GlobalTechCSeq_ind", "L232.StubTechCalInput_indenergy", "L232.StubTechCalInput_indfeed",
                     "L232.GlobalTechSCurve_en", "L232.GlobalTechProfitShutdown_en",
                     "L232.StubTechProd_industry", "L232.StubTechCoef_industry", "L232.FuelPrefElast_indenergy",
                     "L232.PerCapitaBased_ind", "L232.PriceElasticity_ind", "L232.BaseService_ind",
                     "L232.SubsectorShrwtFllt_ind_low_fossil",
                     "L232.Supplysector_ind") ->
      other_industry_cwf_low_fossil.xml

    # create the CWF high/medium/low hydrogen XMLs
    for (i in c("cwf_low_H2", "cwf_med_H2", "cwf_high_H2")) {
      L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios_sel <- L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      L232.SubsectorInterp_ind_cwf_H2_scenarios_sel <- L232.SubsectorInterp_ind_cwf_H2_scenarios %>%
        filter(scenario == i) %>%
        select(-scenario)

      xml_name <- paste0("other_industry_", i, ".xml")

      create_xml(xml_name) %>%
        add_logit_tables_xml(L232.Supplysector_ind, "Supplysector") %>%
        add_logit_tables_xml(L232.SubsectorLogit_ind, "SubsectorLogit") %>%
        add_xml_data(L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios_sel, "SubsectorShrwtFllt") %>% # CWF version for this case
        add_xml_data(L232.FinalEnergyKeyword_ind, "FinalEnergyKeyword") %>%
        # add_xml_data(L232.SubsectorShrwt_ind, "SubsectorShrwt") %>%
        add_xml_data(L232.SubsectorInterp_ind_cwf_H2_scenarios_sel, "SubsectorInterp") %>% # CWF version for this case
        # add_xml_data(L232.SubsectorInterpTo_ind, "SubsectorInterpTo") %>%
        add_xml_data(L232.StubTech_ind, "StubTech") %>%
        add_xml_data(L232.GlobalTechShrwt_ind, "GlobalTechShrwt") %>%
        add_xml_data(L232.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
        add_xml_data(L232.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
        add_xml_data(L232.StubTechInterp_ind, "StubTechInterp") %>%
        add_xml_data(L232.GlobalTechEff_ind_cwf, "GlobalTechEff") %>% # CWF version
        add_xml_data(L232.GlobalTechCoef_ind, "GlobalTechCoef") %>%
        add_xml_data(L232.GlobalTechCost_ind, "GlobalTechCost") %>%
        add_xml_data(L232.GlobalTechSecOut_ind, "GlobalTechSecOut") %>%
        add_xml_data(L232.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
        add_xml_data(L232.StubTechCalInput_indenergy, "StubTechCalInput") %>%
        add_xml_data(L232.StubTechCalInput_indfeed, "StubTechCalInput") %>%
        add_xml_data(L232.StubTechProd_industry, "StubTechProd") %>%
        add_xml_data(L232.StubTechCoef_industry, "StubTechCoef") %>%
        add_xml_data(L232.FuelPrefElast_indenergy, "FuelPrefElast") %>%
        add_xml_data(L232.PerCapitaBased_ind, "PerCapitaBased") %>%
        add_xml_data(L232.PriceElasticity_ind, "PriceElasticity") %>%
        add_xml_data(L232.BaseService_ind, "BaseService") %>%
        add_precursors("L232.SubsectorLogit_ind", "L232.FinalEnergyKeyword_ind", #"L232.SubsectorShrwt_ind",
                       "L232.SubsectorInterp_ind_cwf_H2_scenarios_sel", #"L232.SubsectorInterpTo_ind",
                       "L232.StubTech_ind",
                       "L232.GlobalTechShrwt_ind", "L232.StubTechInterp_ind", "L232.GlobalTechEff_ind_cwf",
                       "L232.GlobalTechCoef_ind", "L232.GlobalTechCost_ind", "L232.GlobalTechSecOut_ind",
                       "L232.GlobalTechCSeq_ind", "L232.StubTechCalInput_indenergy", "L232.StubTechCalInput_indfeed",
                       "L232.GlobalTechSCurve_en", "L232.GlobalTechProfitShutdown_en",
                       "L232.StubTechProd_industry", "L232.StubTechCoef_industry", "L232.FuelPrefElast_indenergy",
                       "L232.PerCapitaBased_ind", "L232.PriceElasticity_ind", "L232.BaseService_ind",
                       "L232.SubsectorShrwtFllt_ind_cwf_H2_scenarios", "L232.Supplysector_ind") %>%
        assign(xml_name, ., envir = curr_env)
    }

    return_data(other_industry.xml, other_industry_cwf.xml, other_industry_cwf_low_fossil.xml,
                other_industry_cwf_low_H2.xml, other_industry_cwf_med_H2.xml, other_industry_cwf_high_H2.xml)
  } else {
    stop("Unknown command")
  }
}
