# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_transportation_UCD_CORE_mineral_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_transportation_UCD_CORE_mineral_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L2541.trn_globaltech_mineral_curcoef_final",
             "L2541.trn_globaltech_mineral_coef_final",
             "L2541.trn_globaltech_mineral_Pmult",
             "L2541.StubTranTechCost_no_mineral_cost"))
  } else if(command == driver.DECLARE_OUTPUTS) {

    xml_files<- c("transportation_UCD_CORE_mineral.xml","transportation_UCD_SSP1_mineral.xml",
                  "transportation_UCD_SSP3_mineral.xml","transportation_UCD_SSP5_mineral.xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L2541.trn_globaltech_mineral_coef_final <- get_data(all_data, "L2541.trn_globaltech_mineral_coef_final")
    L2541.trn_globaltech_mineral_curcoef_final <- get_data(all_data, "L2541.trn_globaltech_mineral_curcoef_final")
    L2541.trn_globaltech_mineral_Pmult <- get_data(all_data, "L2541.trn_globaltech_mineral_Pmult")
    L2541.StubTranTechCost_no_mineral_cost <- get_data(all_data, "L2541.StubTranTechCost_no_mineral_cost")

    # ===================================================

    # Produce outputs
    # Because `return_data` gets the name of the object from what's actually given in the call,
    # we need to assign xml_tmp to a correctly-named variable in the current environment
    # transportation_UCD_CORE.xml <- transportation_UCD_SSP1.xml <- transportation_UCD_SSP2.xml <-
    #   transportation_UCD_SSP3.xml <- transportation_UCD_SSP5.xml <- transportation_UCD_CORE_highEV.xml <- NULL  # silence package check notes
    transportation_UCD_CORE_mineral.xml <- transportation_UCD_SSP1_mineral.xml <- transportation_UCD_SSP2_mineral.xml <-
      transportation_UCD_SSP3_mineral.xml <- transportation_UCD_SSP5_mineral.xml <- NULL  # silence package check notes

    ret_data <- c()
    curr_env <- environment()

    # #for (i in c("CORE","SSP1","SSP3","SSP5", "highEV")){
    for (i in c("CORE","SSP1","SSP3","SSP5")){

      xml_name <- paste0("transportation_UCD_", i, "_mineral.xml")

      #Read SSP specific data
      L2541.trn_globaltech_mineral_coef_final_SSP <- L2541.trn_globaltech_mineral_coef_final %>%  filter(sce== i)
      L2541.trn_globaltech_mineral_curcoef_final_SSP <- L2541.trn_globaltech_mineral_curcoef_final %>%  filter(sce== i)
      L2541.trn_globaltech_mineral_Pmult_SSP <- L2541.trn_globaltech_mineral_Pmult %>% filter(sce==i)
      L2541.StubTranTechCost_no_mineral_cost_SSP <- L2541.StubTranTechCost_no_mineral_cost %>%  filter(sce== i)
      if (i != "CORE"){
        L2541.StubTranTechCost_no_mineral_cost_SSP <-
          L2541.StubTranTechCost_no_mineral_cost %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      #Create xmls
      create_xml(xml_name) %>%
        add_xml_data(L2541.trn_globaltech_mineral_coef_final_SSP, "PassThruStubTranTechMineralCoef") %>%
        add_xml_data(L2541.trn_globaltech_mineral_curcoef_final_SSP, "PassThruStubTranTechMineralCurCoef") %>%
        add_xml_data(L2541.trn_globaltech_mineral_Pmult_SSP, "PassThruStubTranTechPriceUnitConv") %>%

        add_xml_data(L2541.StubTranTechCost_no_mineral_cost_SSP, "PassThruStubTranTechCost") %>%
        add_precursors("L2541.trn_globaltech_mineral_coef_final",
                       "L2541.trn_globaltech_mineral_curcoef_final",
                       "L2541.StubTranTechCost_no_mineral_cost")  %>%
                        assign(xml_name, ., envir = curr_env)

      ret_data <- c(ret_data, xml_name)

    }
    # #Return all xmls
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()

    # tibble() %>%
    #   add_title("First output") %>%
    #   add_units("None") %>%
    #   add_precursors("common/iso_GCAM_regID", "L200.ModelTime") %>%
    #   add_flags(FLAG_NO_TEST, FLAG_NO_OUTPUT) %>%
    #   add_legacy_name("<none>") %>%
    #   add_comments("Sample chunk output") ->
    #   first_output1
    #
    # tibble() %>%
    #   add_title("Second output") %>%
    #   add_units("None") %>%
    #   add_precursors("common/iso_GCAM_regID", "L200.ModelTime") %>%
    #   add_flags(FLAG_NO_TEST, FLAG_NO_OUTPUT) %>%
    #   add_legacy_name("<none>") %>%
    #   add_comments("Sample chunk output") ->
    #   second_output1

  } else {
    stop("Unknown command")
  }
}
