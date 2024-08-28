# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_transportation_UCD_CORE_cwf_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_transportation_cwf_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.GlobalTranTechInterp_cwf_low_h2",
             "L254.GlobalTranTechShrwt_cwf_low_h2",
             "L254.GlobalTranTechInterp_cwf",
             "L254.GlobalTranTechShrwt_cwf",
             "L254.tranSubsectorVOTT_cwf",

             "L254.tranSubsectorFuelPref",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCoef",
             "L254.StubTranTechCost",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt",

             #"L254.tranSubsectorSpeed",
             #"L254.StubTranTech",
             #"L254.tranSubsectorSpeed_passthru",
             #"L254.StubTech_passthru",
             #"L254.StubTech_nonmotor",
             #"L254.Supplysector_trn",
             #"L254.FinalEnergyKeyword_trn",
             #"L254.tranSubsectorLogit",
             #"L254.tranSubsectorShrwtFllt",
             #"L254.tranSubsectorInterp",
             #"L254.StubTranTechCalInput",


             "L254.StubTranTechInterpTo_ICEPhaseout"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    #xml_files<- c("transportation_UCD_CORE.xml","transportation_UCD_SSP1.xml","transportation_UCD_SSP3.xml","transportation_UCD_SSP5.xml","transportation_UCD_highEV.xml")

    return(c(XML = "transportation_UCD_cwf_low_H2.xml",
             XML = "transportation_cwf_high_en_demand.xml",
             XML = "transportation_UCD_CWF_med.xml",
             XML = "transportation_UCD_CWF_high.xml",
             XML = "transportation_UCD_CWF_low.xml",
             XML = "transportation_UCD_CWF_ref_shrwts.xml"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs

    L254.GlobalTranTechInterp_cwf_low_h2 <- get_data(all_data, "L254.GlobalTranTechInterp_cwf_low_h2")
    L254.GlobalTranTechShrwt_cwf_low_h2 <- get_data(all_data, "L254.GlobalTranTechShrwt_cwf_low_h2")
    L254.GlobalTranTechInterp_cwf <- get_data(all_data, "L254.GlobalTranTechInterp_cwf") %>% filter(sce == 'CWF_high')
    L254.GlobalTranTechShrwt_cwf <- get_data(all_data, "L254.GlobalTranTechShrwt_cwf") %>% filter(sce == 'CWF_high')
    L254.tranSubsectorVOTT_cwf <- get_data(all_data, "L254.tranSubsectorVOTT_cwf")
    L254.tranSubsectorFuelPref <- get_data(all_data,"L254.tranSubsectorFuelPref")
    L254.PerCapitaBased_trn <- get_data(all_data,"L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data,"L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data,"L254.IncomeElasticity_trn")
    L254.StubTranTechLoadFactor <- get_data(all_data,"L254.StubTranTechLoadFactor")
    L254.StubTranTechCoef <- get_data(all_data,"L254.StubTranTechCoef")
    L254.StubTranTechCost <- get_data(all_data,"L254.StubTranTechCost")
    L254.GlobalTranTechInterp <- get_data(all_data,"L254.GlobalTranTechInterp")
    L254.GlobalTranTechShrwt <- get_data(all_data,"L254.GlobalTranTechShrwt")




    L254.StubTranTechInterpTo_ICEPhaseout <- get_data(all_data,"L254.StubTranTechInterpTo_ICEPhaseout")

    # ===================================================
    curr_env <- environment()
    ret_data <- c()

    for (i in c("CWF_med", "CWF_low", "CWF_high", "CWF_ref_shrwts")){
      xml_name <- paste0("transportation_UCD_", i, ".xml")

      # for CWF scenarios, pull in CORE (or SSP1) values for some of these variables and also pull in base year data as needed
      if (grepl("CWF", i)) {
        L254.StubTranTechInterpTo <- L254.StubTranTechInterpTo_ICEPhaseout %>% filter(sce == i)
        #L254.tranSubsectorSpeed_SSP <- L254.tranSubsectorSpeed %>% filter(sce== "CORE")
        #L254.StubTranTech_SSP <- L254.StubTranTech %>% filter(sce== "CORE")
        #L254.tranSubsectorSpeed_passthru_SSP <- L254.tranSubsectorSpeed_passthru %>% filter(sce=="CORE")

        L254.tranSubsectorVOTT_SSP<- L254.tranSubsectorVOTT_cwf # CWF version (for all high/med/low)
        L254.tranSubsectorFuelPref_SSP <- L254.tranSubsectorFuelPref %>% filter(sce=="SSP1") # SSP1
        L254.PerCapitaBased_trn_SSP<- L254.PerCapitaBased_trn %>% filter(sce=="SSP1") # SSP1
        L254.PriceElasticity_trn_SSP <- L254.PriceElasticity_trn %>%  filter(sce=="SSP1") # SSP1
        L254.IncomeElasticity_trn_SSP <- L254.IncomeElasticity_trn %>% filter(sce== "CWF") # CWF version (for all high/med/low)

        # L254.StubTech_passthru_SSP <- L254.StubTech_passthru %>% filter(sce=="CORE")
        # L254.StubTech_nonmotor_SSP <- L254.StubTech_nonmotor %>% filter(sce=="CORE")
        # L254.Supplysector_trn_SSP  <- L254.Supplysector_trn %>% filter(sce=="CORE")
        # L254.FinalEnergyKeyword_trn_SSP <- L254.FinalEnergyKeyword_trn %>% filter(sce=="CORE")
        # L254.tranSubsectorLogit_SSP <- L254.tranSubsectorLogit %>% filter(sce=="CORE")
        # L254.tranSubsectorShrwtFllt_SSP <- L254.tranSubsectorShrwtFllt %>%  filter(sce =="CORE")
        # L254.tranSubsectorInterp_SSP <- L254.tranSubsectorInterp %>%  filter(sce =="CORE")
        # L254.StubTranTechCalInput_SSP <-  L254.StubTranTechCalInput %>% filter(sce =="CORE")

        # CWF versions for share weights for the respective scenarios
        L254.GlobalTranTechInterp_SSP <- L254.GlobalTranTechInterp_cwf %>% filter(sce==i)
        L254.GlobalTranTechShrwt_SSP <- L254.GlobalTranTechShrwt_cwf %>%  filter(sce==i)

        # CWF versions that need base years added
        L254.StubTranTechLoadFactor_SSP <- L254.StubTranTechLoadFactor %>%  filter(sce== "CWF") %>% filter(year>MODEL_FINAL_BASE_YEAR)
        L254.StubTranTechLoadFactor_SSP <- rbind(L254.StubTranTechLoadFactor_SSP,
                                                 L254.StubTranTechLoadFactor %>%
                                                   filter(sce == "CORE") %>%
                                                   left_join(L254.StubTranTechLoadFactor_SSP %>% dplyr::select(-sce),
                                                             by = c("region", "supplysector", "tranSubsector", "stub.technology", "year")) %>%
                                                   filter(is.na(loadFactor.y)) %>%
                                                   dplyr::select(-loadFactor.y) %>%
                                                   rename(loadFactor = loadFactor.x))
        L254.StubTranTechCost_SSP <- L254.StubTranTechCost %>%  filter(sce== "CWF") %>% filter(year>MODEL_FINAL_BASE_YEAR)
        L254.StubTranTechCost_SSP <- rbind(L254.StubTranTechCost_SSP,
                                           L254.StubTranTechCost %>%
                                             filter(sce == "CORE") %>%
                                             left_join(L254.StubTranTechCost_SSP %>% dplyr::select(-sce),
                                                       by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.non.energy.input")) %>%
                                             filter(is.na(input.cost.y)) %>%
                                             dplyr::select(-input.cost.y) %>%
                                             rename(input.cost = input.cost.x))
        L254.StubTranTechCoef_SSP <- L254.StubTranTechCoef %>%  filter(sce== "CWF") %>% filter(year>MODEL_FINAL_BASE_YEAR)
        L254.StubTranTechCoef_SSP <- rbind(L254.StubTranTechCoef_SSP,
                                           L254.StubTranTechCoef %>%
                                             filter(sce == "CORE") %>%
                                             left_join(L254.StubTranTechCoef_SSP %>% dplyr::select(-sce),
                                                       by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input", "market.name")) %>%
                                             filter(is.na(coefficient.y)) %>%
                                             dplyr::select(-coefficient.y) %>%
                                             rename(coefficient = coefficient.x))

        # if it's the CWF scenario with reference share weights, use the default share weights, but keep all other CWF values
        if(i == "CWF_ref_shrwts") {
          L254.GlobalTranTechInterp_SSP <- L254.GlobalTranTechInterp %>% filter(sce=="CORE")
          L254.GlobalTranTechShrwt_SSP <- L254.GlobalTranTechShrwt %>%  filter(sce=="CORE")
        }
      }


      #Create xmls
      create_xml(xml_name) %>%
        #add_logit_tables_xml(L254.Supplysector_trn_SSP, "Supplysector") %>%
        #add_xml_data(L254.FinalEnergyKeyword_trn_SSP, "FinalEnergyKeyword") %>%
        #add_logit_tables_xml(L254.tranSubsectorLogit_SSP, "tranSubsectorLogit", "tranSubsector") %>%
        #add_xml_data(L254.tranSubsectorShrwtFllt_SSP, "tranSubsectorShrwtFllt") %>%
        #add_xml_data(L254.tranSubsectorInterp_SSP, "tranSubsectorInterp") %>%
        #add_xml_data(L254.tranSubsectorSpeed_SSP, "tranSubsectorSpeed") %>%
        #add_xml_data(L254.tranSubsectorSpeed_passthru_SSP, "tranSubsectorSpeed") %>%
        #add_xml_data(L254.tranSubsectorSpeed_noVOTT, "tranSubsectorSpeed") %>%
        #add_xml_data(L254.tranSubsectorSpeed_nonmotor, "tranSubsectorSpeed") %>%
        #may need this later
        #add_xml_data(L254.tranSubsectorVOTT_SSP, "tranSubsectorVOTT") %>%
        add_xml_data(L254.tranSubsectorFuelPref_SSP, "tranSubsectorFuelPref") %>%
        #add_xml_data(L254.StubTranTech_SSP, "StubTranTech") %>%
        #add_xml_data(L254.StubTech_passthru_SSP, "StubTranTech") %>%
        #add_xml_data(L254.StubTech_nonmotor_SSP, "StubTranTech") %>%
        add_xml_data(L254.StubTranTechInterpTo,"StubTranTechInterpTo") %>%
        #add_xml_data(L254.GlobalTechShrwt_passthru, "GlobalTechShrwt") %>%
        #add_xml_data(L254.GlobalTechShrwt_nonmotor, "GlobalTechShrwt") %>%
        #add_xml_data(L254.GlobalTechCoef_passthru, "GlobalTechCoef") %>%
        #add_xml_data(L254.GlobalRenewTech_nonmotor, "GlobalRenewTech") %>%
        add_xml_data(L254.GlobalTranTechInterp_SSP, "GlobalTranTechInterp") %>%
        add_xml_data(L254.GlobalTranTechShrwt_SSP, "GlobalTranTechShrwt") %>%
        #add_xml_data(L254.GlobalTranTechSCurve, "GlobalTranTechSCurve") %>%
        #       8/2/2022 - disable the profit shutdown decider until its performance has been assessed/tuned
        #        add_xml_data(L254.GlobalTranTechProfitShutdown, "GlobalTranTechProfitShutdown") %>%
        add_node_equiv_xml("technology") %>%
        add_node_equiv_xml("input") %>%
        #add_xml_data(L254.StubTranTechCalInput_SSP, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTranTechLoadFactor_SSP, "StubTranTechLoadFactor") %>%
        add_node_equiv_xml("subsector") %>%
        #add_xml_data(L254.StubTechTrackCapital_SSP, "StubTechTrackCapital") %>%
        add_xml_data(L254.StubTranTechCost_SSP, "StubTranTechCost") %>%
        add_xml_data(L254.StubTranTechCoef_SSP, "StubTranTechCoef") %>%
        #add_xml_data(L254.StubTechCalInput_passthru, "StubTranTechCalInput") %>%
        #add_xml_data(L254.StubTechProd_nonmotor, "StubTranTechProd") %>%
        add_xml_data(L254.PerCapitaBased_trn_SSP, "PerCapitaBased") %>%
        add_xml_data(L254.PriceElasticity_trn_SSP, "PriceElasticity") %>%
        add_xml_data(L254.IncomeElasticity_trn_SSP, "IncomeElasticity") %>%
        #add_xml_data(L254.BaseService_trn_SSP, "BaseService") %>%
        add_precursors("L254.tranSubsectorVOTT_SSP",
                       "L254.tranSubsectorFuelPref_SSP",
                       "L254.GlobalTranTechInterp_SSP",
                       "L254.GlobalTranTechShrwt_SSP",
                       "L254.StubTranTechLoadFactor_SSP",
                       "L254.StubTranTechCost_SSP",
                       "L254.StubTranTechCoef_SSP",
                       "L254.PerCapitaBased_trn_SSP",
                       "L254.PriceElasticity_trn_SSP",
                       "L254.IncomeElasticity_trn_SSP",
                       "L254.StubTranTechInterpTo_ICEPhaseout")  %>%
        assign(xml_name, ., envir = curr_env)


      ret_data <- c(ret_data, xml_name)

    }

    # Produce outputs
    #Create xmls
    create_xml("transportation_UCD_cwf_low_H2.xml") %>%
      add_xml_data(L254.StubTranTechInterpTo,"StubTranTechInterpTo") %>%
      add_xml_data(L254.GlobalTranTechInterp_cwf_low_h2, "GlobalTranTechInterp") %>%
      add_xml_data(L254.GlobalTranTechShrwt_cwf_low_h2, "GlobalTranTechShrwt") %>%
      add_precursors("L254.GlobalTranTechInterp_cwf_low_h2",
                     "L254.GlobalTranTechShrwt_cwf_low_h2",
                     "L254.StubTranTechInterpTo_ICEPhaseout") ->
      transportation_UCD_cwf_low_H2.xml

    # has all technology assumptions re: ZEV uptake + fossil phaseout but no demand reduction
    create_xml("transportation_cwf_high_en_demand.xml") %>%
      add_xml_data(L254.StubTranTechInterpTo,"StubTranTechInterpTo") %>%
      add_precursors("L254.StubTranTechInterpTo_ICEPhaseout") ->
      transportation_cwf_high_en_demand.xml

    return_data(transportation_UCD_cwf_low_H2.xml,
                transportation_cwf_high_en_demand.xml,
                transportation_UCD_CWF_med.xml,
                transportation_UCD_CWF_high.xml,
                transportation_UCD_CWF_low.xml,
                transportation_UCD_CWF_ref_shrwts.xml)

  } else {
    stop("Unknown command")
  }
}
