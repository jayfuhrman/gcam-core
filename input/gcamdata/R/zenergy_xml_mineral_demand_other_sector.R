# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_mineral_demand_other_sector_xml
#'
#' Construct XML data structure for \code{water_demand_municipal.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_municipal.xml}. The corresponding file in the
#' original data system was \code{batch_water_demand_municipal.xml.R} (water XML).
module_energy_mineral_demand_other_sector_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L271.Supplysector_mineral_other_sector",
             "L271.SubsectorLogit_mineral_other_sector",
             "L271.SubsectorShrwtFllt_mineral_other_sector",
             "L271.TechShrwt_mineral_other_sector",
             "L271.TechCoef_mineral_other_sector",
             "L271.TechCost_mineral_other_sector",
             "L271.PerCapitaBased_mineral_other_sector",
             "L271.IncomeElasticity_mineral_other_sector",
             "L271.PriceElasticity_mineral_other_sector",
             "L271.aeei_mineral_other_sector",
             "L271.regional_cmm_historical_demand_other_sector"
             # "L271.GlobalTechSCurve_en",
             # "L271.GlobalTechProfitShutdown_en"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "mineral_demand_other_sector.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L271.Supplysector_mineral_other_sector <- get_data(all_data, "L271.Supplysector_mineral_other_sector")
    L271.SubsectorLogit_mineral_other_sector <- get_data(all_data, "L271.SubsectorLogit_mineral_other_sector")
    L271.SubsectorShrwtFllt_mineral_other_sector <- get_data(all_data, "L271.SubsectorShrwtFllt_mineral_other_sector")
    L271.TechShrwt_mineral_other_sector <- get_data(all_data, "L271.TechShrwt_mineral_other_sector")
    L271.TechCoef_mineral_other_sector <- get_data(all_data, "L271.TechCoef_mineral_other_sector")
    L271.TechCost_mineral_other_sector <- get_data(all_data, "L271.TechCost_mineral_other_sector")
    L271.PerCapitaBased_mineral_other_sector <- get_data(all_data, "L271.PerCapitaBased_mineral_other_sector")
    L271.IncomeElasticity_mineral_other_sector <- get_data(all_data, "L271.IncomeElasticity_mineral_other_sector")
    L271.PriceElasticity_mineral_other_sector <- get_data(all_data, "L271.PriceElasticity_mineral_other_sector")
    L271.aeei_mineral_other_sector <- get_data(all_data, "L271.aeei_mineral_other_sector")
    L271.regional_cmm_historical_demand_other_sector <- get_data(all_data, "L271.regional_cmm_historical_demand_other_sector")
    # L271.GlobalTechSCurve_en <- get_data(all_data, "L271.GlobalTechSCurve_en")
    # L271.GlobalTechProfitShutdown_en <- get_data(all_data, "L271.GlobalTechProfitShutdown_en")

    # ===================================================

    # Produce outputs
    create_xml("mineral_demand_other_sector.xml") %>%
      add_logit_tables_xml(L271.Supplysector_mineral_other_sector, "Supplysector") %>%
      add_logit_tables_xml(L271.SubsectorLogit_mineral_other_sector, "SubsectorLogit") %>%
      add_xml_data(L271.SubsectorShrwtFllt_mineral_other_sector, "SubsectorShrwtFllt") %>%
      add_xml_data(L271.TechShrwt_mineral_other_sector, "TechShrwt") %>%
      add_xml_data(L271.TechCoef_mineral_other_sector, "TechCoef") %>%
      add_xml_data(L271.TechCost_mineral_other_sector, "TechCost") %>%
      add_xml_data(L271.PerCapitaBased_mineral_other_sector, "PerCapitaBased") %>%
      add_xml_data(L271.regional_cmm_historical_demand_other_sector, "BaseService") %>%
      add_xml_data(L271.IncomeElasticity_mineral_other_sector, "IncomeElasticity") %>%
      add_xml_data(L271.PriceElasticity_mineral_other_sector, "PriceElasticity") %>%
      add_xml_data(L271.aeei_mineral_other_sector, "aeei") %>%
      # add_xml_data(L271.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      # add_xml_data(L271.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_precursors("L271.Supplysector_mineral_other_sector",
                     "L271.SubsectorLogit_mineral_other_sector",
                     "L271.SubsectorShrwtFllt_mineral_other_sector",
                     "L271.TechShrwt_mineral_other_sector",
                     "L271.TechCoef_mineral_other_sector",
                     "L271.TechCost_mineral_other_sector",
                     "L271.PerCapitaBased_mineral_other_sector",
                     "L271.IncomeElasticity_mineral_other_sector",
                     "L271.PriceElasticity_mineral_other_sector",
                     "L271.aeei_mineral_other_sector",
                     "L271.regional_cmm_historical_demand_other_sector"
                     # "L271.GlobalTechSCurve_en",
                     # "L271.GlobalTechProfitShutdown_en"
                     ) ->
      mineral_demand_other_sector.xml

    return_data(mineral_demand_other_sector.xml)
  } else {
    stop("Unknown command")
  }
}
