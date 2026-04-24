# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_cement_xml
#'
#' Construct XML data structure for \code{cement.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement.xml}. The corresponding file in the
#' original data system was \code{batch_cement_xml.R} (energy XML).
module_energy_cement_xml <- function(command, ...) {
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
             "L2321.GlobalTechTrackCapital_cement",
             "L2321.GlobalTechCapture_cement",
             "L2321.GlobalTechSCurve_en",
             "L2321.GlobalTechProfitShutdown_en",
             "L2321.StubTechProd_cement",
			       "L2321.GlobalTechCSeq_ind",
             "L2321.StubTechCalInput_cement_heat",
             "L2321.StubTechCoef_cement",
             "L2321.PerCapitaBased_cement",
             "L2321.BaseService_cement",
             "L2321.PriceElasticity_cement",

			       "L2321.StubTechFractSecOut",
			       "L2321.StubTechFractProd",
			       "L2321.StubTechFractCalPrice",
			       "L2321.StubTechInterp_cement"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement.xml",
             XML = "cement_noLC3.xml",
             XML = "cement_noAdvChem.xml",
             XML = "cement_noCCS.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # This can be toggled based on if we want to represent a cement plant where CO2 from limestone decomposition and process heat are captured in a single stream.
    # Set to TRUE to represent such a plant (i.e., cement CCS must use process heat cement CCS).
    # Set to FALSE if we wish instead to model a plant with separate capture equipment for the lower-purity process heat exhaust stream
    # (i.e., decision to capture or vent both limestone and process heat emissions is independent and based on costs for each technology)

    SINGLE_STACK_CCS <- TRUE

    process_heat_sector_combine <- function(df, SINGLE_STACK_CCS) {

      if (isFALSE(SINGLE_STACK_CCS)) {

        # Rename in `sector` if it exists
        if ("sector.name" %in% names(df)) {
          df <- df %>%
            mutate(sector.name = ifelse(sector.name == "process heat cement ccs",
                                        "process heat cement",
                                        sector.name))
        }

        # Rename in `supplysector` if it exists
        if ("supplysector" %in% names(df)) {
          df <- df %>%
            mutate(supplysector = ifelse(supplysector == "process heat cement ccs",
                                         "process heat cement",
                                         supplysector))
        }

        # Return distinct rows across all columns
        df <- df %>% distinct()
      }

      return(df)
    }



    # Load required inputs
    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.FinalEnergyKeyword_cement <- get_data(all_data, "L2321.FinalEnergyKeyword_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.SubsectorLogit_cement <- get_data(all_data, "L2321.SubsectorLogit_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    #    L2321.SubsectorShrwt_cement <- get_data(all_data, "L2321.SubsectorShrwt_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.SubsectorShrwtFllt_cement <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.SubsectorInterp_cement <- get_data(all_data, "L2321.SubsectorInterp_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    #    L2321.SubsectorInterpTo_cement <- get_data(all_data, "L2321.SubsectorInterpTo_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.StubTech_cement <- get_data(all_data, "L2321.StubTech_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechShrwt_cement <- get_data(all_data, "L2321.GlobalTechShrwt_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechCoef_cement <- get_data(all_data, "L2321.GlobalTechCoef_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechCost_cement <- get_data(all_data, "L2321.GlobalTechCost_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechTrackCapital_cement <- get_data(all_data, "L2321.GlobalTechTrackCapital_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechCapture_cement <- get_data(all_data, "L2321.GlobalTechCapture_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechSCurve_en <- get_data(all_data, "L2321.GlobalTechSCurve_en") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechProfitShutdown_en <- get_data(all_data, "L2321.GlobalTechProfitShutdown_en") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.StubTechProd_cement <- get_data(all_data, "L2321.StubTechProd_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.StubTechCalInput_cement_heat <- get_data(all_data, "L2321.StubTechCalInput_cement_heat") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.StubTechCoef_cement <- get_data(all_data, "L2321.StubTechCoef_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.PerCapitaBased_cement <- get_data(all_data, "L2321.PerCapitaBased_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.BaseService_cement <- get_data(all_data, "L2321.BaseService_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.PriceElasticity_cement <- get_data(all_data, "L2321.PriceElasticity_cement") %>% process_heat_sector_combine(SINGLE_STACK_CCS)
    L2321.GlobalTechCSeq_ind <-  get_data(all_data, "L2321.GlobalTechCSeq_ind") %>% process_heat_sector_combine(SINGLE_STACK_CCS)

    L2321.StubTechFractSecOut <- get_data(all_data, "L2321.StubTechFractSecOut")
    L2321.StubTechFractProd <- get_data(all_data,"L2321.StubTechFractProd")
    L2321.StubTechFractCalPrice <- get_data(all_data,"L2321.StubTechFractCalPrice")
    L2321.StubTechInterp_cement <- get_data(all_data,"L2321.StubTechInterp_cement")
    # ===================================================

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
      add_node_equiv_xml("input") %>%
      add_xml_data(L2321.GlobalTechCoef_cement, "GlobalTechCoef") %>%
      add_xml_data(L2321.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2321.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2321.GlobalTechTrackCapital_cement, "GlobalTechTrackCapital") %>%
      add_xml_data(L2321.GlobalTechCost_cement, "GlobalTechCost") %>%
      add_xml_data(L2321.GlobalTechCapture_cement, "GlobalTechCapture") %>%
      add_xml_data(L2321.StubTechProd_cement, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechCoef_cement, "StubTechCoef") %>%
      add_xml_data(L2321.StubTechInterp_cement, "StubTechInterp") %>%
      add_xml_data(L2321.PerCapitaBased_cement, "PerCapitaBased") %>%
      add_xml_data(L2321.BaseService_cement, "BaseService") %>%
      add_xml_data(L2321.PriceElasticity_cement, "PriceElasticity") %>%
      add_xml_data(L2321.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%

      add_xml_data(L2321.StubTechFractSecOut, "StubTechFractSecOut") %>%
      add_xml_data(L2321.StubTechFractProd, "StubTechFractProd") %>%
      add_xml_data(L2321.StubTechFractCalPrice, "StubTechFractCalPrice") %>%

      add_precursors("L2321.Supplysector_cement", "L2321.FinalEnergyKeyword_cement", "L2321.SubsectorLogit_cement",
                     # "L2321.SubsectorShrwt_cement",
                     "L2321.SubsectorShrwtFllt_cement",
                     "L2321.SubsectorInterp_cement",
                     # "L2321.SubsectorInterpTo_cement",
                     "L2321.StubTech_cement","L2321.GlobalTechSCurve_en", "L2321.GlobalTechProfitShutdown_en",
                     "L2321.GlobalTechShrwt_cement", "L2321.GlobalTechCoef_cement", "L2321.GlobalTechCost_cement",
                     "L2321.GlobalTechCapture_cement", "L2321.StubTechProd_cement", "L2321.StubTechCalInput_cement_heat",
                     "L2321.StubTechCoef_cement", "L2321.PerCapitaBased_cement", "L2321.BaseService_cement", "L2321.GlobalTechCSeq_ind",
                     "L2321.PriceElasticity_cement", "L2321.GlobalTechTrackCapital_cement",
                     "L2321.StubTechInterp_cement",
                     "L2321.StubTechFractSecOut","L2321.StubTechFractProd","L2321.StubTechFractCalPrice") ->
      cement.xml

    create_xml("cement_noLC3.xml") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement %>% filter(str_detect(technology,"LC3")) %>%
                     mutate(share.weight = 0), "GlobalTechShrwt") ->
      cement_noLC3.xml

    create_xml("cement_noAdvChem.xml") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement %>% filter(str_detect(technology,"silicate")) %>%
                     mutate(share.weight = 0), "GlobalTechShrwt") ->
      cement_noAdvChem.xml

    create_xml("cement_noCCS.xml") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement %>% filter(str_detect(technology,"CCS")) %>%
                     mutate(share.weight = 0), "GlobalTechShrwt") ->
      cement_noCCS.xml


    return_data(cement.xml,
                cement_noLC3.xml,
                cement_noAdvChem.xml,
                cement_noCCS.xml)
  } else {
    stop("Unknown command")
  }
}
