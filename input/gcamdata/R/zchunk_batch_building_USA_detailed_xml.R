#' module_gcamusa_batch_building_USA_detailed_xml
#'
#' Construct XML data structure for \code{building_USA_detailed.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_USA_detailed.xml}.
module_gcamusa_batch_building_USA_detailed_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.DeleteSupplysector_USAonly",
             "L244.DeleteGenericService_USAonly",
             "L244.PriceExp_IntGains_USAonly",
             "L244.Floorspace_USAonly",
             "L244.SatiationAdder_USAonly",
             "L244.ThermalBaseService_USAonly",
             "L244.GenericBaseService_USAonly",
             "L244.ThermalServiceSatiation_USAonly",
             "L244.GenericServiceSatiation_USAonly",
             "L244.Intgains_scalar_USAonly",
             "L244.ShellConductance_bld_USAonly",
             "L244.Supplysector_bld_USAonly",
             "L244.FinalEnergyKeyword_bld_USAonly",
             "L244.SubsectorShrwtFllt_bld_USAonly",
             "L244.SubsectorInterp_bld_USAonly",
             "L244.SubsectorInterpTo_bld_USAonly",
             "L244.SubsectorLogit_bld_USAonly",
             "L244.StubTech_bld_USAonly",
             "L244.StubTechCalInput_bld_USAonly",
             "L244.StubTechMarket_bld_USAonly",
             "L244.GlobalTechIntGainOutputRatio",
             "L244.GlobalTechInterpTo_bld",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld_gcamusa",
             "L244.GlobalTechCost_bld_gcamusa",
             "L244.GlobalTechSCurve_bld",
             # emissions
             "L244.InputEmissions_bld_pol_USAonly",
             "L244.InputEmissions_bld_ghg_USAonly",
             "L244.GDPCtrlMax_bld_ghg_USAonly",
             "L244.GDPCtrlSteep_bld_ghg_USAonly",
             "L244.StbTechOutputEmissions_bld_hfc_USAonly",
             'L244.OutputEmissCoeff_bld_hfc_future_USAonly',
             "L244.StubTechEmissUnits_bld_hfc_USAonly",
             "L244.MAC_bld_hfc_USAonly",
             "L244.MACPhaseIn_bld_hfc_USAonly",
             "L244.MACTC_bld_hfc_USAonly"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_USA_detailed.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.DeleteSupplysector_USAonly <- get_data(all_data, "L244.DeleteSupplysector_USAonly")
    L244.DeleteGenericService_USAonly <- get_data(all_data, "L244.DeleteGenericService_USAonly")
    L244.PriceExp_IntGains_USAonly <- get_data(all_data, "L244.PriceExp_IntGains_USAonly")
    L244.Floorspace_USAonly <- get_data(all_data, "L244.Floorspace_USAonly")
    L244.SatiationAdder_USAonly <- get_data(all_data, "L244.SatiationAdder_USAonly")
    L244.ThermalBaseService_USAonly <- get_data(all_data, "L244.ThermalBaseService_USAonly")
    L244.GenericBaseService_USAonly <- get_data(all_data, "L244.GenericBaseService_USAonly")
    L244.ThermalServiceSatiation_USAonly <- get_data(all_data, "L244.ThermalServiceSatiation_USAonly")
    L244.GenericServiceSatiation_USAonly <- get_data(all_data, "L244.GenericServiceSatiation_USAonly")
    L244.Intgains_scalar_USAonly <- get_data(all_data, "L244.Intgains_scalar_USAonly")
    L244.ShellConductance_bld_USAonly <- get_data(all_data, "L244.ShellConductance_bld_USAonly")
    L244.Supplysector_bld_USAonly <- get_data(all_data, "L244.Supplysector_bld_USAonly")
    L244.FinalEnergyKeyword_bld_USAonly <- get_data(all_data, "L244.FinalEnergyKeyword_bld_USAonly")
    L244.SubsectorShrwtFllt_bld_USAonly <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_USAonly")
    L244.SubsectorInterp_bld_USAonly <- get_data(all_data, "L244.SubsectorInterp_bld_USAonly")
    L244.SubsectorInterpTo_bld_USAonly <- get_data(all_data, "L244.SubsectorInterpTo_bld_USAonly")
    L244.SubsectorLogit_bld_USAonly <- get_data(all_data, "L244.SubsectorLogit_bld_USAonly")
    L244.StubTech_bld_USAonly <- get_data(all_data, "L244.StubTech_bld_USAonly")
    L244.StubTechCalInput_bld_USAonly <- get_data(all_data, "L244.StubTechCalInput_bld_USAonly")
    L244.StubTechMarket_bld_USAonly <- get_data(all_data, "L244.StubTechMarket_bld_USAonly")
    L244.GlobalTechIntGainOutputRatio <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio")
    L244.GlobalTechInterpTo_bld <- get_data(all_data, "L244.GlobalTechInterpTo_bld")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamusa")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld_gcamusa")
    L244.GlobalTechSCurve_bld <- get_data(all_data, "L244.GlobalTechSCurve_bld")
    # emissions
    L244.InputEmissions_bld_pol_USAonly <- get_data(all_data, "L244.InputEmissions_bld_pol_USAonly")
    L244.InputEmissions_bld_ghg_USAonly <- get_data(all_data, "L244.InputEmissions_bld_ghg_USAonly")
    L244.GDPCtrlMax_bld_ghg_USAonly <- get_data(all_data, "L244.GDPCtrlMax_bld_ghg_USAonly")
    L244.GDPCtrlSteep_bld_ghg_USAonly <- get_data(all_data, "L244.GDPCtrlSteep_bld_ghg_USAonly")
    L244.StbTechOutputEmissions_bld_hfc_USAonly <- get_data(all_data, "L244.StbTechOutputEmissions_bld_hfc_USAonly")
    L244.OutputEmissCoeff_bld_hfc_future_USAonly <- get_data(all_data, "L244.OutputEmissCoeff_bld_hfc_future_USAonly")
    L244.StubTechEmissUnits_bld_hfc_USAonly <- get_data(all_data, "L244.StubTechEmissUnits_bld_hfc_USAonly")
    L244.MAC_bld_hfc_USAonly <- get_data(all_data, "L244.MAC_bld_hfc_USAonly")
    L244.MACPhaseIn_bld_hfc_USAonly <- get_data(all_data, "L244.MACPhaseIn_bld_hfc_USAonly")
    L244.MACTC_bld_hfc_USAonly <- get_data(all_data, "L244.MACTC_bld_hfc_USAonly")

    # ===================================================

    # Produce outputs
    create_xml("building_USA_detailed.xml") %>%
      add_xml_data(L244.DeleteSupplysector_USAonly, "DeleteSupplysector") %>%
      add_xml_data(L244.DeleteGenericService_USAonly, "DeleteGenericService") %>%
      add_xml_data(L244.PriceExp_IntGains_USAonly, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace_USAonly, "Floorspace") %>%
      add_xml_data(L244.SatiationAdder_USAonly, "SatiationAdder") %>%
      add_xml_data(L244.ThermalBaseService_USAonly, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService_USAonly, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation_USAonly, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation_USAonly, "GenericServiceSatiation") %>%
      add_xml_data(L244.Intgains_scalar_USAonly, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_bld_USAonly, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_bld_USAonly, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld_USAonly, "FinalEnergyKeyword") %>%
      add_xml_data(L244.SubsectorShrwtFllt_bld_USAonly, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterp_bld_USAonly, "SubsectorInterp") %>%
      add_xml_data(L244.SubsectorInterpTo_bld_USAonly, "SubsectorInterpTo") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_USAonly, "SubsectorLogit") %>%
      add_xml_data(L244.StubTech_bld_USAonly, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_bld_USAonly, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechMarket_bld_USAonly, "StubTechMarket") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechInterpTo_bld, "GlobalTechInterpTo") %>%
      add_xml_data(L244.GlobalTechEff_bld, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechSCurve_bld, "GlobalTechSCurve") %>%
      # emissions
      add_xml_data(L244.InputEmissions_bld_pol_USAonly, "InputEmissions") %>%
      add_xml_data(L244.InputEmissions_bld_ghg_USAonly, "InputEmissions") %>%
      add_xml_data(L244.GDPCtrlMax_bld_ghg_USAonly, "GDPCtrlMax") %>%
      add_xml_data(L244.GDPCtrlSteep_bld_ghg_USAonly, "GDPCtrlSteep") %>%
      add_xml_data(L244.StbTechOutputEmissions_bld_hfc_USAonly, "StbTechOutputEmissions") %>%
      add_xml_data(L244.OutputEmissCoeff_bld_hfc_future_USAonly, "OutputEmissCoeff") %>%
      add_xml_data(L244.StubTechEmissUnits_bld_hfc_USAonly, "StubTechEmissUnits") %>%
      add_xml_data(L244.MAC_bld_hfc_USAonly, "MAC") %>%
      add_xml_data(L244.MACPhaseIn_bld_hfc_USAonly, "MACPhaseIn") %>%
      add_xml_data(L244.MACTC_bld_hfc_USAonly, "MACTC") %>%
      add_precursors("L244.DeleteSupplysector_USAonly",
                     "L244.DeleteGenericService_USAonly",
                     "L244.PriceExp_IntGains_USAonly",
                     "L244.Floorspace_USAonly",
                     "L244.SatiationAdder_USAonly",
                     "L244.ThermalBaseService_USAonly",
                     "L244.GenericBaseService_USAonly",
                     "L244.ThermalServiceSatiation_USAonly",
                     "L244.GenericServiceSatiation_USAonly",
                     "L244.Intgains_scalar_USAonly",
                     "L244.ShellConductance_bld_USAonly",
                     "L244.Supplysector_bld_USAonly",
                     "L244.FinalEnergyKeyword_bld_USAonly",
                     "L244.SubsectorShrwtFllt_bld_USAonly",
                     "L244.SubsectorInterp_bld_USAonly",
                     "L244.SubsectorInterpTo_bld_USAonly",
                     "L244.SubsectorLogit_bld_USAonly",
                     "L244.StubTech_bld_USAonly",
                     "L244.StubTechCalInput_bld_USAonly",
                     "L244.StubTechMarket_bld_USAonly",
                     "L244.GlobalTechIntGainOutputRatio",
                     "L244.GlobalTechInterpTo_bld",
                     "L244.GlobalTechEff_bld",
                     "L244.GlobalTechShrwt_bld_gcamusa",
                     "L244.GlobalTechCost_bld_gcamusa",
                     "L244.GlobalTechSCurve_bld",
                     # emissions
                     "L244.InputEmissions_bld_pol_USAonly",
                     "L244.InputEmissions_bld_ghg_USAonly",
                     "L244.GDPCtrlMax_bld_ghg_USAonly",
                     "L244.GDPCtrlSteep_bld_ghg_USAonly",
                     "L244.StbTechOutputEmissions_bld_hfc_USAonly",
                     'L244.OutputEmissCoeff_bld_hfc_future_USAonly',
                     "L244.StubTechEmissUnits_bld_hfc_USAonly",
                     "L244.MAC_bld_hfc_USAonly",
                     "L244.MACPhaseIn_bld_hfc_USAonly",
                     "L244.MACTC_bld_hfc_USAonly") ->
      building_USA_detailed.xml

    return_data(building_USA_detailed.xml)
  } else {
    stop("Unknown command")
  }
}
