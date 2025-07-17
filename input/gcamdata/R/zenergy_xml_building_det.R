# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_building_det_xml
#'
#' Construct XML data structure for \code{building_det.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_det.xml}. The corresponding file in the
#' original data system was \code{batch_building_det.xml} (energy XML).
module_energy_building_det_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.SubsectorInterpTo_bld",
             "L244.SubsectorInterp_bld",
             "L244.SubsectorShrwtFllt_bld",
             "L244.SubsectorShrwt_bld",
             "L244.FinalEnergyKeyword_bld",
             "L244.Supplysector_bld",
             "L244.ShellConductance_bld",
             "L244.Intgains_scalar",
             "L244.GenericServiceSatiation",
             "L244.ThermalServiceSatiation",
             "L244.GenericBaseService",
             "L244.ThermalBaseService",
             "L244.SatiationAdder",
             "L244.Satiation_flsp",
             "L244.DemandFunction_flsp",
             "L244.DemandFunction_serv",
             "L244.Floorspace",
             "L244.PriceExp_IntGains",
             "L244.SubregionalShares",
             "L244.SubsectorLogit_bld",
             "L244.FuelPrefElast_bld",
             "L244.StubTech_bld",
             "L244.StubTechEff_bld",
             "L244.StubTechCalInput_bld",
             "L244.StubTechIntGainOutputRatio",
             "L244.GlobalTechShrwt_bld",
             "L244.GlobalTechCost_bld",
             "L244.GlobalTechTrackCapital_bld",
             "L244.DeleteThermalService",
             "L244.Satiation_impedance",
             "L244.GenericServiceImpedance",
             "L244.ThermalServiceImpedance",
             "L244.GenericServiceAdder",
             "L244.ThermalServiceAdder",
             "L244.GenericServiceCoef",
             "L244.ThermalServiceCoef",
             "L244.GompFnParam",
             "L244.GenericCoalCoef",
             "L244.ThermalCoalCoef",
             "L244.GenericTradBioCoef",
             "L244.ThermalTradBioCoef",
             "L244.GenericServicePrice",
             "L244.ThermalServicePrice",
             "L244.GenericBaseDens",
             "L244.ThermalBaseDens",
             "L244.DeleteGenericService",
             "L2441.GenericBaseServiceMaterials",
             "L2441.SupplysectorMaterials",
             "L2441.SubsectorLogitMaterials",
             "L2441.SubsectorShrwtMaterials",
             "L2441.SubsectorShrwtFlltMaterials",
             "L2441.SubsectorInterpMaterials",
             "L2441.SubsectorInterpToMaterials",
             "L2441.TechCalOutputMaterials",
             "L2441.TechShrwtMaterials",
             "L2441.TechCoefMaterials",
             "L2441.TechLifetimeMaterials",
             "L2441.TechSCurveMaterials",
             "L2441.TechProfitShutdownMaterials"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_det.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld", strip_attributes = TRUE)
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld", strip_attributes = TRUE)
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld", strip_attributes = TRUE)
    L244.SubsectorShrwt_bld <- get_data(all_data, "L244.SubsectorShrwt_bld", strip_attributes = TRUE)
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld", strip_attributes = TRUE)
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld", strip_attributes = TRUE)
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld", strip_attributes = TRUE)
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar", strip_attributes = TRUE)
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation", strip_attributes = TRUE)
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation", strip_attributes = TRUE)
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService", strip_attributes = TRUE)
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService", strip_attributes = TRUE)
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder", strip_attributes = TRUE)
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp", strip_attributes = TRUE)
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp", strip_attributes = TRUE)
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv", strip_attributes = TRUE)
    L244.Floorspace <- get_data(all_data, "L244.Floorspace", strip_attributes = TRUE)
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains", strip_attributes = TRUE)
    L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares", strip_attributes = TRUE)
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld", strip_attributes = TRUE)
    L244.FuelPrefElast_bld <- get_data(all_data, "L244.FuelPrefElast_bld", strip_attributes = TRUE)
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld", strip_attributes = TRUE)
    L244.StubTechEff_bld <- get_data(all_data, "L244.StubTechEff_bld", strip_attributes = TRUE)
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld", strip_attributes = TRUE)
    L244.StubTechIntGainOutputRatio <- get_data(all_data, "L244.StubTechIntGainOutputRatio", strip_attributes = TRUE)
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld", strip_attributes = TRUE)
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld", strip_attributes = TRUE)
    L244.GlobalTechTrackCapital_bld <- get_data(all_data, "L244.GlobalTechTrackCapital_bld", strip_attributes = TRUE)
    L244.DeleteThermalService <- get_data(all_data, "L244.DeleteThermalService", strip_attributes = TRUE)
    L244.DeleteGenericService <- get_data(all_data, "L244.DeleteGenericService", strip_attributes = TRUE)
    L244.Satiation_impedance <- get_data(all_data, "L244.Satiation_impedance", strip_attributes = TRUE)
    L244.GenericServiceImpedance<-get_data(all_data, "L244.GenericServiceImpedance", strip_attributes = TRUE)
    L244.ThermalServiceImpedance<-get_data(all_data, "L244.ThermalServiceImpedance", strip_attributes = TRUE)
    L244.GenericServiceAdder<-get_data(all_data, "L244.GenericServiceAdder", strip_attributes = TRUE)
    L244.ThermalServiceAdder<-get_data(all_data, "L244.ThermalServiceAdder", strip_attributes = TRUE)
    L244.GenericServiceCoef<-get_data(all_data, "L244.GenericServiceCoef", strip_attributes = TRUE)
    L244.ThermalServiceCoef<-get_data(all_data, "L244.ThermalServiceCoef", strip_attributes = TRUE)
    L244.GompFnParam <- get_data(all_data, "L244.GompFnParam", strip_attributes = TRUE)
    L244.GenericCoalCoef <- get_data(all_data, "L244.GenericCoalCoef", strip_attributes = TRUE)
    L244.ThermalCoalCoef <- get_data(all_data, "L244.ThermalCoalCoef", strip_attributes = TRUE)
    L244.GenericTradBioCoef <- get_data(all_data, "L244.GenericTradBioCoef", strip_attributes = TRUE)
    L244.ThermalTradBioCoef <- get_data(all_data, "L244.ThermalTradBioCoef", strip_attributes = TRUE)
    L244.GenericServicePrice <- get_data(all_data, "L244.GenericServicePrice", strip_attributes = TRUE)
    L244.ThermalServicePrice <- get_data(all_data, "L244.ThermalServicePrice", strip_attributes = TRUE)
    L244.GenericBaseDens <- get_data(all_data, "L244.GenericBaseDens", strip_attributes = TRUE)
    L244.ThermalBaseDens <- get_data(all_data, "L244.ThermalBaseDens", strip_attributes = TRUE)

    L2441.GenericBaseServiceMaterials <- get_data(all_data,"L2441.GenericBaseServiceMaterials", strip_attributes = TRUE)
    L2441.SupplysectorMaterials <- get_data(all_data, "L2441.SupplysectorMaterials", strip_attributes = TRUE)
    L2441.SubsectorLogitMaterials <- get_data(all_data, "L2441.SubsectorLogitMaterials", strip_attributes = TRUE)
    L2441.SubsectorShrwtMaterials <- get_data(all_data, "L2441.SubsectorShrwtMaterials", strip_attributes = TRUE)
    L2441.SubsectorShrwtFlltMaterials <- get_data(all_data, "L2441.SubsectorShrwtFlltMaterials", strip_attributes = TRUE)
    L2441.SubsectorInterpMaterials <- get_data(all_data, "L2441.SubsectorInterpMaterials", strip_attributes = TRUE)
    L2441.SubsectorInterpToMaterials <- get_data(all_data, "L2441.SubsectorInterpToMaterials", strip_attributes = TRUE)
    L2441.TechCalOutputMaterials <- get_data(all_data, "L2441.TechCalOutputMaterials", strip_attributes = TRUE)
    L2441.TechShrwtMaterials <- get_data(all_data, "L2441.TechShrwtMaterials", strip_attributes = TRUE)
    L2441.TechCoefMaterials <- get_data(all_data, "L2441.TechCoefMaterials", strip_attributes = TRUE)

    L2441.TechLifetimeMaterials <- get_data(all_data, "L2441.TechLifetimeMaterials", strip_attributes = TRUE)
    L2441.TechSCurveMaterials <- get_data(all_data, "L2441.TechSCurveMaterials", strip_attributes = TRUE)
    L2441.TechProfitShutdownMaterials <- get_data(all_data, "L2441.TechProfitShutdownMaterials", strip_attributes = TRUE)


    # ===================================================

    # Produce outputs
    create_xml("building_det.xml") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L244.Supplysector_bld, "Supplysector") %>%
      add_xml_data(L244.ShellConductance_bld, "ShellConductance") %>%
      add_xml_data(L244.Intgains_scalar, "Intgains_scalar") %>%
      add_xml_data(L244.GenericServiceSatiation, "GenericServiceSatiation") %>%
      add_xml_data(L244.ThermalServiceSatiation, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericBaseService, "GenericBaseService") %>%
      add_xml_data(L244.ThermalBaseService, "ThermalBaseService") %>%
      add_xml_data(L244.GenericServiceImpedance, "GenericServiceImpedance") %>%
      add_xml_data(L244.ThermalServiceImpedance, "ThermalServiceImpedance") %>%
      add_xml_data(L244.GenericServiceAdder, "GenericServiceAdder") %>%
      add_xml_data(L244.ThermalServiceAdder, "ThermalServiceAdder") %>%
      add_xml_data(L244.GenericServiceCoef, "GenericServiceCoef") %>%
      add_xml_data(L244.ThermalServiceCoef, "ThermalServiceCoef") %>%
      add_xml_data(L244.SatiationAdder, "SatiationAdder") %>%
      add_xml_data(L244.Satiation_flsp, "Satiation_flsp") %>%
      add_xml_data(L244.Satiation_impedance, "SatiationImpedance") %>%
      add_xml_data(L244.GompFnParam, "GompFnParam") %>%
      add_xml_data(L244.GenericCoalCoef, "GenericCoalCoef") %>%
      add_xml_data(L244.ThermalCoalCoef, "ThermalCoalCoef") %>%
      add_xml_data(L244.GenericTradBioCoef, "GenericTradBioCoef") %>%
      add_xml_data(L244.ThermalTradBioCoef, "ThermalTradBioCoef") %>%
      add_xml_data(L244.GenericServicePrice, "GenericServicePrice") %>%
      add_xml_data(L244.ThermalServicePrice, "ThermalServicePrice") %>%
      add_xml_data(L244.GenericBaseDens, "GenericBaseDens") %>%
      add_xml_data(L244.ThermalBaseDens, "ThermalBaseDens") %>%
      add_xml_data(L244.DemandFunction_flsp, "DemandFunction_flsp") %>%
      add_xml_data(L244.DemandFunction_serv, "DemandFunction_serv") %>%
      add_xml_data(L244.Floorspace, "Floorspace") %>%
      add_xml_data(L244.PriceExp_IntGains, "PriceExp_IntGains") %>%
      add_xml_data(L244.SubregionalShares, "SubregionalShares") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld, "SubsectorLogit") %>%
      add_xml_data(L244.FuelPrefElast_bld, "FuelPrefElast") %>%
      add_xml_data(L244.StubTech_bld, "StubTech") %>%
      add_xml_data(L244.StubTechEff_bld, "StubTechEff") %>%
      add_xml_data(L244.StubTechCalInput_bld, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechIntGainOutputRatio, "StubTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L244.GlobalTechTrackCapital_bld, "GlobalTechTrackCapital") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%
      add_xml_data(L2441.GenericBaseServiceMaterials, "GenericBaseService") %>%
      add_logit_tables_xml(L2441.SupplysectorMaterials, "Supplysector") %>%
      add_logit_tables_xml(L2441.SubsectorLogitMaterials, "SubsectorLogit") %>%
      add_xml_data(L2441.TechCalOutputMaterials, "Production") %>%
      add_xml_data(L2441.TechShrwtMaterials, "TechShrwt") %>%
      add_xml_data(L2441.TechCoefMaterials, "RegionalTechMineralCurCoef") %>%
      add_xml_data(L2441.TechLifetimeMaterials, "TechLifetime") %>%
      add_xml_data(L2441.TechSCurveMaterials, "TechSCurve") %>%
      add_xml_data(L2441.TechProfitShutdownMaterials, "TechProfitShutdown") %>%
      add_precursors("L244.SubsectorInterpTo_bld", "L244.SubsectorInterp_bld" , "L244.SubsectorShrwtFllt_bld",
                     "L244.SubsectorShrwt_bld", "L244.FinalEnergyKeyword_bld", "L244.Supplysector_bld",
                     "L244.ShellConductance_bld", "L244.Intgains_scalar", "L244.GenericServiceSatiation",
                     "L244.ThermalServiceSatiation", "L244.GenericBaseService", "L244.ThermalBaseService", "L244.SatiationAdder",
                     "L244.Satiation_flsp",
                     "L244.GompFnParam",
                     "L244.Satiation_impedance",
                     "L244.DemandFunction_flsp", "L244.DemandFunction_serv",
                     "L244.Floorspace", "L244.SubregionalShares", "L244.SubsectorLogit_bld",
                     "L244.FuelPrefElast_bld", "L244.StubTech_bld", "L244.StubTechEff_bld",
                     "L244.StubTechCalInput_bld", "L244.StubTechIntGainOutputRatio", "L244.GlobalTechShrwt_bld",
                     "L244.GlobalTechCost_bld", "L244.DeleteThermalService", "L244.DeleteGenericService",
                     "L244.PriceExp_IntGains","L244.GenericServiceImpedance","L244.ThermalServiceImpedance",
                     "L244.GenericServiceAdder","L244.ThermalServiceAdder",
                     "L244.GenericTradBioCoef","L244.ThermalTradBioCoef",
                     "L244.GenericCoalCoef","L244.ThermalCoalCoef",
                     "L244.GenericServicePrice","L244.ThermalServicePrice",
                     "L244.GenericBaseDens", "L244.ThermalBaseDens",
                     "L244.GenericServiceCoef","L244.ThermalServiceCoef",
                     "L244.GlobalTechTrackCapital_bld",
                     "L2441.GenericBaseServiceMaterials",
                     "L2441.SupplysectorMaterials",
                     "L2441.SubsectorLogitMaterials",
                     "L2441.SubsectorShrwtMaterials",
                     "L2441.SubsectorShrwtFlltMaterials",
                     "L2441.SubsectorInterpMaterials",
                     "L2441.SubsectorInterpToMaterials",
                     "L2441.TechCalOutputMaterials",
                     "L2441.TechCoefMaterials",
                     "L2441.TechShrwtMaterials",
                     "L2441.TechLifetimeMaterials",
                     "L2441.TechSCurveMaterials",
                     "L2441.TechProfitShutdownMaterials") ->   building_det.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(nrow(L244.DeleteThermalService) > 0) {
      building_det.xml %>%
        add_xml_data(L244.DeleteThermalService, "DeleteThermalService") ->
        building_det.xml
    }

    if(!is.null(L244.DeleteGenericService)) {
      building_det.xml %>%
        add_xml_data(L244.DeleteGenericService, "DeleteGenericService") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorShrwt_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorShrwt_bld, "SubsectorShrwt") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorShrwtFllt_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorInterp_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorInterp_bld, "SubsectorInterp") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorInterpTo_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld, "SubsectorInterp") ->
        building_det.xml
    }
    if(!is.null(L2441.SubsectorShrwtMaterials)) {
      building_det.xml %>%
        add_xml_data(L2441.SubsectorShrwtMaterials, "SubsectorShrwt") ->
        building_det.xml
    }
    if(!is.null(L2441.SubsectorShrwtFlltMaterials)) {
      building_det.xml %>%
        add_xml_data(L2441.SubsectorShrwtFlltMaterials, "SubsectorShrwtFllt") ->
        building_det.xml
    }
    if(!is.null(L2441.SubsectorInterpMaterials)) {
      building_det.xml %>%
        add_xml_data(L2441.SubsectorInterpMaterials, "SubsectorInterp") ->
        building_det.xml
    }
    if(!is.null(L2441.SubsectorInterpToMaterials)) {
      building_det.xml %>%
        add_xml_data(L244.1SubsectorInterpToMaterials, "SubsectorInterp") ->
        building_det.xml
    }

    return_data(building_det.xml)
  } else {
    stop("Unknown command")
  }
}
