# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_building_det_cwf_xml
#'
#' Construct XML data structure for \code{building_det.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_det.xml} and \code{building_det_cwf.xml},
#' The corresponding file in the
#' original data system was \code{batch_building_det.xml} (energy XML).
module_energy_building_det_cwf_xml <- function(command, ...) {
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

      "L244.SubsectorInterpTo_bld_low_fossil",
      "L244.SubsectorInterp_bld_low_fossil",
      "L244.SubsectorShrwtFllt_bld_low_fossil",
      "L244.SubsectorShrwt_bld_low_fossil",

      "L244.ShellConductance_bld_cwf",
      "L244.StubTechEff_bld_cwf",
      "L244.StubTechIntGainOutputRatio_cwf",
      "L244.Satiation_flsp_cwf",
      "L244.GompFnParam_cwf",
      "L244.globaltech_shrwt_cwf_no_H2_building"
      ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_det_cwf.xml",
             XML = "building_det_cwf_high_en.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld")
    L244.SubsectorShrwt_bld <- get_data(all_data, "L244.SubsectorShrwt_bld")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar")
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation")
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp")
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp")
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains")
    L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld")
    L244.FuelPrefElast_bld <- get_data(all_data, "L244.FuelPrefElast_bld")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld")
    L244.StubTechEff_bld <- get_data(all_data, "L244.StubTechEff_bld")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld")
    L244.StubTechIntGainOutputRatio <- get_data(all_data, "L244.StubTechIntGainOutputRatio")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld")
    L244.GlobalTechTrackCapital_bld <- get_data(all_data, "L244.GlobalTechTrackCapital_bld")
    L244.DeleteThermalService <- get_data(all_data, "L244.DeleteThermalService")
    L244.DeleteGenericService <- get_data(all_data, "L244.DeleteGenericService")
    L244.Satiation_impedance <- get_data(all_data, "L244.Satiation_impedance")
    L244.GenericServiceImpedance<-get_data(all_data, "L244.GenericServiceImpedance")
    L244.ThermalServiceImpedance<-get_data(all_data, "L244.ThermalServiceImpedance")
    L244.GenericServiceAdder<-get_data(all_data, "L244.GenericServiceAdder")
    L244.ThermalServiceAdder<-get_data(all_data, "L244.ThermalServiceAdder")
    L244.GenericServiceCoef<-get_data(all_data, "L244.GenericServiceCoef")
    L244.ThermalServiceCoef<-get_data(all_data, "L244.ThermalServiceCoef")
    L244.GompFnParam <- get_data(all_data, "L244.GompFnParam")
    L244.GenericCoalCoef <- get_data(all_data, "L244.GenericCoalCoef")
    L244.ThermalCoalCoef <- get_data(all_data, "L244.ThermalCoalCoef")
    L244.GenericTradBioCoef <- get_data(all_data, "L244.GenericTradBioCoef")
    L244.ThermalTradBioCoef <- get_data(all_data, "L244.ThermalTradBioCoef")
    L244.GenericServicePrice <- get_data(all_data, "L244.GenericServicePrice")
    L244.ThermalServicePrice <- get_data(all_data, "L244.ThermalServicePrice")
    L244.GenericBaseDens <- get_data(all_data, "L244.GenericBaseDens")
    L244.ThermalBaseDens <- get_data(all_data, "L244.ThermalBaseDens")

    L244.SubsectorInterpTo_bld_low_fossil <- get_data(all_data, "L244.SubsectorInterpTo_bld_low_fossil")
    L244.SubsectorInterp_bld_low_fossil <- get_data(all_data, "L244.SubsectorInterp_bld_low_fossil")
    L244.SubsectorShrwtFllt_bld_low_fossil <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_low_fossil")
    L244.SubsectorShrwt_bld_low_fossil <- get_data(all_data, "L244.SubsectorShrwt_bld_low_fossil")

    L244.ShellConductance_bld_cwf <- get_data(all_data, "L244.ShellConductance_bld_cwf")
    L244.StubTechEff_bld_cwf <- get_data(all_data, "L244.StubTechEff_bld_cwf")
    L244.StubTechIntGainOutputRatio_cwf <- get_data(all_data, "L244.StubTechIntGainOutputRatio_cwf")
    L244.Satiation_flsp_cwf <- get_data(all_data, "L244.Satiation_flsp_cwf")
    L244.GompFnParam_cwf <- get_data(all_data, "L244.GompFnParam_cwf")
    L244.globaltech_shrwt_cwf_no_H2_building <- get_data(all_data, "L244.globaltech_shrwt_cwf_no_H2_building")

    # ===================================================

    curr_env <- environment()

    # Produce outputs
    create_xml("building_det_cwf.xml") %>%
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
      add_xml_data(L244.ShellConductance_bld_cwf, "ShellConductance") %>% # CWF version
      add_xml_data(L244.SatiationAdder, "SatiationAdder") %>%
      add_xml_data(L244.Satiation_flsp_cwf, "Satiation_flsp") %>% # CWF version
      add_xml_data(L244.GompFnParam_cwf, "GompFnParam") %>% # CWF version
      add_xml_data(L244.StubTechEff_bld_cwf, "StubTechEff") %>% # CWF version
      add_xml_data(L244.StubTechIntGainOutputRatio_cwf, "StubTechIntGainOutputRatio") %>% # CWF version
      add_node_equiv_xml("input") %>%
      add_xml_data(L244.GlobalTechTrackCapital_bld, "GlobalTechTrackCapital") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%

      add_xml_data(L244.globaltech_shrwt_cwf_no_H2_building, "GlobalTechShrwt") %>% # CWF version
      add_precursors("L244.FinalEnergyKeyword_bld", "L244.Supplysector_bld", "L244.SubsectorLogit_bld",
					 "L244.SubsectorInterpTo_bld", "L244.SubsectorInterp_bld" , "L244.SubsectorShrwtFllt_bld",
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
                     "L244.ShellConductance_bld_cwf", "L244.SatiationAdder", "L244.Satiation_flsp_cwf",
                     "L244.GompFnParam_cwf", "L244.StubTechEff_bld_cwf", "L244.StubTechIntGainOutputRatio_cwf",
                     "L244.globaltech_shrwt_cwf_no_H2_building"
                     ) ->
      building_det_cwf.xml


    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(nrow(L244.DeleteThermalService) > 0) {
      building_det_cwf.xml %>%
        add_xml_data(L244.DeleteThermalService, "DeleteThermalService") ->
        building_det_cwf.xml
	}

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L244.SubsectorShrwt_bld_low_fossil)) {
      building_det_cwf.xml %>%
        add_xml_data(L244.SubsectorShrwt_bld_low_fossil, "SubsectorShrwt") %>%
        add_precursors("L244.SubsectorShrwt_bld_low_fossil") ->
        building_det_cwf.xml
    }

    if(!is.null(L244.DeleteGenericService)) {
      building_det_cwf.xml %>%
        add_xml_data(L244.DeleteGenericService, "DeleteGenericService") ->
        building_det_cwf.xml
}

    if(!is.null(L244.SubsectorShrwtFllt_bld_low_fossil)) {
      building_det_cwf.xml %>%
        add_xml_data(L244.SubsectorShrwtFllt_bld_low_fossil, "SubsectorShrwtFllt") %>%
        add_precursors("L244.SubsectorShrwtFllt_bld_low_fossil") ->
        building_det_cwf.xml
    }

    if(!is.null(L244.SubsectorInterp_bld_low_fossil)) {

      L244.SubsectorInterp_bld_low_fossil %>%
        group_by(region, supplysector, subsector) %>%
        mutate(is_first = row_number()==1) %>%
        ungroup() ->
        L244.SubsectorInterp_bld_low_fossil
      # For interpolation rules the delete doesn't apply to the tag that it is on, but rather prior ones.
      # And the one that it is attached to is parsed and set like the delete wasn't there


      building_det_cwf.xml %>%
        # delete existing subsector interp rule so that it doesn't interfere with the fossil fuel phaseout we want
        add_xml_data(L244.SubsectorInterp_bld_low_fossil %>% filter(is_first),"DeleteSubsectorInterp") %>%
        add_xml_data(L244.SubsectorInterp_bld_low_fossil %>% filter(!is_first), "SubsectorInterp") %>%
        add_precursors("L244.SubsectorInterp_bld_low_fossil") ->
        building_det_cwf.xml
    }

    if(!is.null(L244.SubsectorInterpTo_bld_low_fossil)) {
      building_det_cwf.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld_low_fossil, "SubsectorInterpTo") %>%
        add_precursors("L244.SubsectorInterpTo_bld_low_fossil") ->
        building_det_cwf.xml
    }

    # create another add-on to be used en lieu of building_det_cwf with CWF efficiency + technology improvements, but not floorspace satiation changes
    create_xml("building_det_cwf_high_en.xml") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L244.Supplysector_bld, "Supplysector") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld, "SubsectorLogit") %>%
      add_xml_data(L244.ShellConductance_bld_cwf, "ShellConductance") %>% # CWF version
      add_xml_data(L244.StubTechEff_bld_cwf, "StubTechEff") %>% # CWF version
      add_xml_data(L244.StubTechIntGainOutputRatio_cwf, "StubTechIntGainOutputRatio") %>% # CWF version
      add_xml_data(L244.globaltech_shrwt_cwf_no_H2_building, "GlobalTechShrwt") %>% # CWF version
      add_precursors("L244.FinalEnergyKeyword_bld", "L244.Supplysector_bld", "L244.SubsectorLogit_bld",
                     "L244.ShellConductance_bld_cwf",
                     "L244.StubTechEff_bld_cwf", "L244.StubTechIntGainOutputRatio_cwf",
                     "L244.globaltech_shrwt_cwf_no_H2_building"
      ) ->
      building_det_cwf_high_en.xml


    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L244.SubsectorShrwt_bld_low_fossil)) {
      building_det_cwf_high_en.xml %>%
        add_xml_data(L244.SubsectorShrwt_bld_low_fossil, "SubsectorShrwt") %>%
        add_precursors("L244.SubsectorShrwt_bld_low_fossil") ->
        building_det_cwf_high_en.xml
    }

    if(!is.null(L244.SubsectorShrwtFllt_bld_low_fossil)) {
      building_det_cwf_high_en.xml %>%
        add_xml_data(L244.SubsectorShrwtFllt_bld_low_fossil, "SubsectorShrwtFllt") %>%
        add_precursors("L244.SubsectorShrwtFllt_bld_low_fossil") ->
        building_det_cwf_high_en.xml
    }

    if(!is.null(L244.SubsectorInterp_bld_low_fossil)) {

      L244.SubsectorInterp_bld_low_fossil %>%
        group_by(region, supplysector, subsector) %>%
        mutate(is_first = row_number()==1) %>%
        ungroup() ->
        L244.SubsectorInterp_bld_low_fossil
      # For interpolation rules the delete doesn't apply to the tag that it is on, but rather prior ones.
      # And the one that it is attached to is parsed and set like the delete wasn't there


      building_det_cwf_high_en.xml %>%
        # delete existing subsector interp rule so that it doesn't interfere with the fossil fuel phaseout we want
        add_xml_data(L244.SubsectorInterp_bld_low_fossil %>% filter(is_first),"DeleteSubsectorInterp") %>%
        add_xml_data(L244.SubsectorInterp_bld_low_fossil %>% filter(!is_first), "SubsectorInterp") %>%
        add_xml_data(L244.SubsectorInterpTo_bld_low_fossil, "SubsectorInterpTo") %>%
        add_precursors("L244.SubsectorInterp_bld_low_fossil",
                       "L244.SubsectorInterpTo_bld_low_fossil") ->
        building_det_cwf_high_en.xml
    }

    if(!is.null(L244.SubsectorInterpTo_bld_low_fossil)) {
      building_det_cwf_high_en.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld_low_fossil, "SubsectorInterpTo") %>%
        add_precursors("L244.SubsectorInterpTo_bld_low_fossil") ->
        building_det_cwf_high_en.xml
    }

    return_data(building_det_cwf.xml,
                building_det_cwf_high_en.xml)
  } else {
    stop("Unknown command")
  }
}
