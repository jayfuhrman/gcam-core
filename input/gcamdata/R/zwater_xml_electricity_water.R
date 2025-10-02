# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_electricity_water_xml
#'
#' Construct XML data structure for \code{electricity_water.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water.xml.R} (water XML).
module_water_electricity_water_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L223.Supplysector_elec",
      "L223.SubsectorShrwtFllt_elec",
      "L223.ElecReserve",
      "L223.SectorUseTrialMarket_elec",
      "L223.StubTechCapFactor_elec",
      "L223.SubsectorInterp_elec",
      "L223.SubsectorInterpTo_elec",
      "L223.SubsectorLogit_elec",
      "L223.SubsectorShrwt_coal",
      "L223.SubsectorShrwt_nuc",
      "L223.SubsectorShrwt_renew",
      "L2233.AvgFossilEffKeyword_elec_cool",
      # "L2233.GlobalIntTechValueFactor_elec_cool",
      "L2233.GlobalReSubtypeTechValueFactor_elec_cool",
      "L2233.GlobalIntTechValueFactor_elec_cool_CSP",
      # "L2233.GlobalIntTechBackup_elec_cool",
      "L2233.GlobalReSubtypeTechBackup_elec_cool",
      "L2233.GlobalIntTechBackup_elec_cool_CSP",
      # "L2233.GlobalIntTechEff_elec_cool",
      "L2233.GlobalReSubtypeTechEff_elec_cool",
      "L2233.GlobalIntTechEff_elec_cool_CSP",
      # "L2233.GlobalIntTechShrwt_elec_cool",
      "L2233.GlobalIntPassThruTechShrwt_elec_cool",
      "L2233.GlobalIntTechShrwt_elec_cool_CSP",
      "L2233.GlobalIntTechCapFac_elec_cool",
      # "L2233.GlobalTechCapFac_elec_cool",
      "L2233.GlobalPassThruTechCapFac_elec_cool",
      "L2233.GlobalTechCapFac_elec_cool_non_RE",
      "L2233.GlobalTechCapture_elec_cool",
      # "L2233.GlobalTechEff_elec_cool",
      "L2233.GlobalPassThruTechEff_elec_cool",
      "L2233.GlobalTechEff_elec_cool_non_RE",
      "L2233.GlobalTechProfitShutdown_elec_cool",
      "L2233.GlobalTechSCurve_elec_cool",
      # "L2233.GlobalTechShrwt_elec_cool",
      "L2233.GlobalPassThruTechShrwt_elec_cool",
      "L2233.GlobalTechShrwt_elec_cool_non_RE",
      "L2233.PrimaryRenewKeyword_elec_cool",
      "L2233.PrimaryRenewKeywordInt_elec_cool",
      "L2233.StubTech_elecPassthru",
      "L2233.StubTechProd_elecPassthru",
      "L2233.GlobalPassThroughTech",
      "L2233.GlobalTechEff_elecPassthru",
      "L2233.GlobalTechShrwt_elecPassthru",
      # "L2233.GlobalIntTechCapital_elec",
      # "L2233.GlobalTechCapital_elecPassthru",
      "L2233.GlobalIntTechOMfixed_elec",
      "L2233.GlobalTechOMfixed_elecPassthru",
      "L2233.GlobalIntTechOMvar_elec",
      "L2233.GlobalTechOMvar_elecPassthru",
      "L2233.GlobalTechInterp_elecPassthru",
      "L2233.PassThroughSector_elec_cool",
      "L2233.Supplysector_elec_cool",
      "L2233.ElecReserve_elec_cool",
      "L2233.SubsectorShrwtFllt_elec_cool",
      "L2233.SubsectorLogit_elec_cool",
      "L2233.StubTechTrackCapital_elec",
      "L2233.StubTech_elec_cool",
      "L2233.StubTechEff_elec_cool",
      "L2233.StubTechProd_elec_cool",
      "L2233.StubTechCapFactor_elec_cool",
      "L2233.StubTechSecOut_desal_elec_cool",
      "L2233.StubTechFixOut_hydro",
      "L2233.StubTechShrwt_elec_cool",
      "L2233.GlobalTechCapital_elec_cool",
      "L2233.GlobalIntTechCapital_elec_cool",
      "L223.GlobalTechCapFac_elec",

      "L2233.Sector_elec_mineral",
      "L2233.PassThruSector_elec_mineral",
      "L2233.SubsectorLogit_elec_mineral",
      "L2233.SubsectorShrwtFllt_elec_mineral",
      "L2233.SubsectorInterp_elec_mineral",
      "L2233.SubsectorInterpTo_elec_mineral",
      "L2233.SubsectorShrwt_elec_mineral",
      "L2233.SubsecShrwt_mineral_other_pv_wind",
      "L2233.SubsecShrwt_mineral_pv_wind",
      "L2233.StubTechShrwt_mineral_pv_wind",
      "L2233.StubTechProd_mineral_pv_wind",
      "L2233.StubTechShrwt_mineral_other_pv_wind",
      "L2233.StubTechShrwt_mineral_pv_wind_future",
      "L2233.StubTechInterpTo_mineral_pv_wind_tech",
      "L2233.StubTechCapFac_mineral_pv_wind",
      # "L2233.Regionaltech_mineral_coef_constance_final",
      # "L2233.Globaltech_mineral_coef_constance_final",
      # "L2233.Regional_Globaltech_mineral_coef_constance_Yb",
      # "L2233.Regionaltech_mineral_PMult",
      # "L2233.Globaltech_mineral_PMult",
      # "L2233.Regional_Globaltech_mineral_Yb_PMult",
      "L2233.GlobalTechCapital_elec_subtype_pv_wind",
      "L2233.GlobalTechCapital_elec_subtype_pv_wind_storage",
      "L2233.GlobalTechCapital_elecPassthru_no_pv_wind",
      "L2233.GlobalIntTechMineral_elecSupplySector",
      "L2233.GlobalTechMineral_elecSupplySector",
      "L2233.GlobalTechLifetimeMineral_elec",
      "L2233.GlobalIntTechLifetimeMineral_elec",
      "L2233.GlobalIntTechLifetime_CSP",
      "L2233.GlobalTechLifetime_elec_cool_no_pv_wind")

  MODULE_OUTPUTS <-
    c(XML = "electricity_water.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Silence package checks
    technology <- NULL

    # ===================================================

    # Rename columns to match add_xml_data header expectations.
    L2233.GlobalIntTechEff_elec_cool_CSP      <- rename(L2233.GlobalIntTechEff_elec_cool_CSP, `intermittent.technology` = technology)
    L2233.GlobalIntTechShrwt_elec_cool_CSP    <- rename(L2233.GlobalIntTechShrwt_elec_cool_CSP,  `intermittent.technology` = technology)
    L2233.GlobalIntPassThruTechShrwt_elec_cool    <- rename(L2233.GlobalIntPassThruTechShrwt_elec_cool,  `pass.through.technology` = technology)
    L2233.GlobalPassThruTechShrwt_elec_cool <- rename(L2233.GlobalPassThruTechShrwt_elec_cool,  `pass.through.technology` = technology)
    L2233.GlobalIntTechCapFac_elec_cool   <- rename(L2233.GlobalIntTechCapFac_elec_cool,  `intermittent.technology` = technology)
    L2233.GlobalIntTechLifetime_CSP       <- rename(L2233.GlobalIntTechLifetime_CSP, `intermittent.technology` = technology)
    L2233.GlobalIntTechValueFactor_elec_cool_CSP   <- rename(L2233.GlobalIntTechValueFactor_elec_cool_CSP,  `intermittent.technology` = technology)
    L2233.GlobalIntTechBackup_elec_cool_CSP   <- rename(L2233.GlobalIntTechBackup_elec_cool_CSP,  `backup.intermittent.technology` = technology)

    # Produce outputs
    create_xml("electricity_water.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_logit_tables_xml(L223.Supplysector_elec, "Supplysector") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.ElecReserve, "ElecReserve") %>%
      add_xml_data(L223.SectorUseTrialMarket_elec, "SectorUseTrialMarket") %>%
      add_xml_data(L223.StubTechCapFactor_elec, "StubTechCapFactor") %>%
      add_xml_data(L223.SubsectorInterp_elec, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec, "SubsectorInterpTo") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwt_coal, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_nuc, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew, "SubsectorShrwt") %>%
      add_xml_data(L2233.AvgFossilEffKeyword_elec_cool, "AvgFossilEffKeyword") ->
      electricity_water.xml

    if(energy.ELEC_USE_BACKUP) {
      electricity_water.xml %>%
        add_xml_data(L2233.GlobalReSubtypeTechBackup_elec_cool, "GlobalIntTechBackup") %>%
        add_xml_data(L2233.GlobalIntTechBackup_elec_cool_CSP, "GlobalIntTechBackup") %>%
        add_precursors("L2233.GlobalReSubtypeTechBackup_elec_cool",
                       "L2233.GlobalIntTechBackup_elec_cool_CSP") ->
        electricity_water.xml
    } else {
      electricity_water.xml %>%
        add_xml_data(L2233.GlobalReSubtypeTechValueFactor_elec_cool, "GlobalIntTechValueFactor") %>%
        add_xml_data(L2233.GlobalIntTechValueFactor_elec_cool_CSP, "GlobalIntTechValueFactor") %>%
        add_precursors("L2233.GlobalReSubtypeTechValueFactor_elec_cool",
                       "L2233.GlobalIntTechValueFactor_elec_cool_CSP") ->
        electricity_water.xml
    }

    electricity_water.xml %>%
      add_xml_data(L2233.GlobalIntTechCapFac_elec_cool, "GlobalIntTechCapFac") %>%
      add_xml_data(L2233.GlobalReSubtypeTechEff_elec_cool, "GlobalIntTechEff") %>%
      add_xml_data(L2233.GlobalIntTechEff_elec_cool_CSP, "GlobalIntTechEff") %>%
      add_xml_data(L2233.GlobalIntTechLifetime_CSP, "GlobalIntTechLifetime") %>%
      add_xml_data(L2233.GlobalIntPassThruTechShrwt_elec_cool, "GlobalIntPassThruTechShrwt") %>%
      add_xml_data(L2233.GlobalIntTechShrwt_elec_cool_CSP, "GlobalIntTechShrwt") %>%
      add_xml_data(L2233.GlobalPassThruTechCapFac_elec_cool, "GlobalTechCapFac") %>%
      add_xml_data(L2233.GlobalTechCapFac_elec_cool_non_RE, "GlobalTechCapFac") %>%
      add_xml_data(L2233.GlobalTechCapture_elec_cool, "GlobalTechCapture") %>%
      add_xml_data(L2233.GlobalPassThruTechEff_elec_cool, "GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechEff_elec_cool_non_RE, "GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechLifetime_elec_cool_no_pv_wind, "GlobalTechLifetime") %>%
      add_xml_data(L2233.GlobalTechProfitShutdown_elec_cool, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2233.GlobalTechSCurve_elec_cool, "GlobalTechSCurve") %>%
      add_xml_data(L2233.GlobalPassThruTechShrwt_elec_cool, "GlobalIntPassThruTechShrwt") %>%
      add_xml_data(L2233.GlobalTechShrwt_elec_cool_non_RE, "GlobalTechShrwt") %>%
      add_xml_data(L2233.PrimaryRenewKeyword_elec_cool, "PrimaryRenewKeyword") %>%
      add_xml_data(L2233.PrimaryRenewKeywordInt_elec_cool, "PrimaryRenewKeywordInt") %>%
      add_xml_data(L2233.StubTech_elecPassthru, "StubTech") %>%
      add_xml_data(L2233.StubTechProd_elecPassthru, "StubTechProd") %>%
      add_xml_data(L2233.GlobalPassThroughTech, "GlobalPassThroughTech") %>%
      add_xml_data(L2233.GlobalTechEff_elecPassthru, "GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechShrwt_elecPassthru, "GlobalTechShrwt") %>%
      add_xml_data(L2233.GlobalTechCapital_elecPassthru_no_pv_wind, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechOMfixed_elec, "GlobalIntTechOMfixed", "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecPassthru, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalIntTechOMvar_elec, "GlobalIntTechOMvar", "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecPassthru, "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalTechInterp_elecPassthru, "GlobalTechInterp") %>%
      add_xml_data(L2233.PassThroughSector_elec_cool, "PassThroughSector") %>%
      add_logit_tables_xml(L2233.Supplysector_elec_cool, "Supplysector") %>%
      add_xml_data(L2233.ElecReserve_elec_cool, "PassThruSectorElecReserve") %>%
      add_xml_data(L2233.SubsectorShrwtFllt_elec_cool, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L2233.SubsectorLogit_elec_cool, "SubsectorLogit") %>%
      add_xml_data(L2233.StubTech_elec_cool, "StubTech") %>%
      add_xml_data(L2233.StubTechTrackCapital_elec, "StubTechTrackCapital") %>%
      add_xml_data(L2233.StubTechTrackCapital_elec, "StubTechCost") %>%
      add_xml_data(L2233.StubTechEff_elec_cool, "StubTechEff") %>%
      add_xml_data(L2233.StubTechSecOut_desal_elec_cool, "StubTechSecOut") %>%
      add_xml_data(L2233.StubTechProd_elec_cool, "StubTechProd") %>%
      add_xml_data(L2233.StubTechCapFactor_elec_cool, "StubTechCapFactor") %>%
      add_xml_data(L2233.StubTechFixOut_hydro, "StubTechFixOut") %>%
      add_xml_data(L2233.StubTechShrwt_elec_cool, "StubTechShrwt") %>%
      add_xml_data(L2233.GlobalTechCapital_elec_cool, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elec_cool, "GlobalIntTechCapital", "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalTechCapFac_elec, "GlobalTechCapFac") %>%

      add_xml_data(L2233.PassThruSector_elec_mineral, "PassThroughSector") %>%
      add_logit_tables_xml(L2233.Sector_elec_mineral, "Supplysector") %>%
      add_logit_tables_xml(L2233.SubsectorLogit_elec_mineral, "SubsectorLogit") %>%
      add_xml_data(L2233.SubsectorShrwtFllt_elec_mineral, "SubsectorShrwtFllt") %>%
      add_xml_data(L2233.SubsectorInterp_elec_mineral, "SubsectorInterp") %>%
      add_xml_data(L2233.SubsectorInterpTo_elec_mineral, "SubsectorInterpTo") %>%
      add_xml_data(L2233.SubsectorShrwt_elec_mineral, "SubsectorShrwt") %>%
      add_xml_data(L2233.SubsecShrwt_mineral_other_pv_wind, "SubsectorShrwt") %>%
      add_xml_data(L2233.SubsecShrwt_mineral_pv_wind, "SubsectorShrwt") %>%
      add_xml_data(L2233.StubTechShrwt_mineral_pv_wind, "StubTechShrwt") %>%
      add_xml_data(L2233.StubTechProd_mineral_pv_wind, "StubTechCal") %>%
      add_xml_data(L2233.StubTechShrwt_mineral_other_pv_wind, "StubTechShrwt") %>%
      add_xml_data(L2233.StubTechShrwt_mineral_pv_wind_future, "StubTechShrwt") %>%
      add_xml_data(L2233.StubTechInterpTo_mineral_pv_wind_tech, "StubTechInterpTo") %>%
      add_xml_data(L2233.StubTechCapFac_mineral_pv_wind, "StubTechCapFactor") %>%
      # add_xml_data(L2233.Regionaltech_mineral_coef_constance_final, "RegionalStubTechMineralCurCoefAllYr") %>%
      # add_xml_data(L2233.Globaltech_mineral_coef_constance_final, "GlobalTechMineralCurCoefAllYr") %>%
      # add_xml_data(L2233.Regional_Globaltech_mineral_coef_constance_Yb, "RegionalStubTechMineralCurCoefAllYr") %>%
      # add_xml_data(L2233.Regionaltech_mineral_PMult, "StubCaloriePriceConv") %>%
      # add_xml_data(L2233.Globaltech_mineral_PMult, "GlobalTechInputPMult") %>%
      # add_xml_data(L2233.Regional_Globaltech_mineral_Yb_PMult, "StubCaloriePriceConv") %>%
      add_xml_data(L2233.GlobalTechCapital_elec_subtype_pv_wind, "GlobalIntTechCapital") %>%
      add_xml_data(L2233.GlobalTechCapital_elec_subtype_pv_wind_storage, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechMineral_elecSupplySector, "GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechMineral_elecSupplySector, "GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechLifetimeMineral_elec, "GlobalTechLifetime") %>%
      add_xml_data(L2233.GlobalIntTechLifetimeMineral_elec, "GlobalIntTechLifetime") %>%

      add_precursors(MODULE_INPUTS) ->
      electricity_water.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
