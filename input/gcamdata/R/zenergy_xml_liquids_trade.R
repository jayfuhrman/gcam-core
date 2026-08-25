# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_liquids_trade_xml
#'
#' Construct XML data structure for \code{liquids_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquids_trade.xml}.
module_energy_liquids_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L240A.Supplysector_tra",
             "L240A.SectorUseTrialMarket_tra",
             "L240A.SubsectorAll_tra",
             "L240A.TechShrwt_tra",
             "L240A.TechCost_tra",
             "L240A.TechCoef_tra",
             "L240A.Production_tra",
             "L240A.Supplysector_reg",
             "L240A.SubsectorAll_reg",
             "L240A.TechShrwt_reg",
             "L240A.TechCoef_reg",
             "L240A.Production_reg_imp",
             "L240A.Production_reg_dom"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_trade.xml",
             XML = "Refined_Products_HormuzPriceShock.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L240A.Supplysector_tra <- get_data(all_data, "L240A.Supplysector_tra")
    L240A.SectorUseTrialMarket_tra <- get_data(all_data, "L240A.SectorUseTrialMarket_tra")
    L240A.SubsectorAll_tra <- get_data(all_data, "L240A.SubsectorAll_tra")
    L240A.TechShrwt_tra <- get_data(all_data, "L240A.TechShrwt_tra")
    L240A.TechCost_tra <- get_data(all_data, "L240A.TechCost_tra")
    L240A.TechCoef_tra <- get_data(all_data, "L240A.TechCoef_tra")
    L240A.Production_tra <- get_data(all_data, "L240A.Production_tra")
    L240A.Supplysector_reg <- get_data(all_data, "L240A.Supplysector_reg")
    L240A.SubsectorAll_reg <- get_data(all_data, "L240A.SubsectorAll_reg")
    L240A.TechShrwt_reg <- get_data(all_data, "L240A.TechShrwt_reg")
    L240A.TechCoef_reg <- get_data(all_data, "L240A.TechCoef_reg")
    L240A.Production_reg_imp <- get_data(all_data, "L240A.Production_reg_imp")
    L240A.Production_reg_dom <- get_data(all_data, "L240A.Production_reg_dom")
    # ===================================================

    # Produce outputs
    create_xml("liquids_trade.xml") %>%
      add_logit_tables_xml(L240A.Supplysector_tra, "Supplysector") %>%
      add_xml_data(L240A.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L240A.SubsectorAll_tra, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L240A.TechShrwt_tra, "TechShrwt") %>%
      add_xml_data(L240A.TechCost_tra, "TechCost") %>%
      add_xml_data(L240A.TechCoef_tra, "TechCoef") %>%
      add_xml_data(L240A.Production_tra, "Production") %>%
      add_logit_tables_xml(L240A.Supplysector_reg, "Supplysector") %>%
      add_logit_tables_xml(L240A.SubsectorAll_reg, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L240A.TechShrwt_reg, "TechShrwt") %>%
      add_xml_data(L240A.TechCoef_reg, "TechCoef") %>%
      add_xml_data(L240A.Production_reg_imp, "Production") %>%
      add_xml_data(L240A.Production_reg_dom, "Production") %>%
      add_precursors("L240A.Supplysector_tra",
                     "L240A.SectorUseTrialMarket_tra",
                     "L240A.SubsectorAll_tra",
                     "L240A.TechShrwt_tra",
                     "L240A.TechCost_tra",
                     "L240A.TechCoef_tra",
                     "L240A.Production_tra",
                     "L240A.Supplysector_reg",
                     "L240A.SubsectorAll_reg",
                     "L240A.TechShrwt_reg",
                     "L240A.TechCoef_reg",
                     "L240A.Production_reg_imp",
                     "L240A.Production_reg_dom") ->
      liquids_trade.xml


    refined_products <- c('traded Distillate_FuelOil','traded Residual_FuelOil','traded Gasoline',
                          'traded PetCoke','traded Jet_Kerosene','traded LPG')

    MidEastPriceShock <- bind_rows(L240A.TechCost_tra) %>%
      filter(str_detect(subsector, "Middle East"),
                        year >= 2030) %>%
      mutate(input.cost = if_else(supplysector == "traded oil", 1,
                     if_else(supplysector %in% refined_products,1.5,
                             if_else(supplysector == 'traded natural gas', 3, input.cost))))

    create_xml("Refined_Products_HormuzPriceShock.xml") %>%
      add_logit_tables_xml(L240A.Supplysector_tra, "Supplysector") %>%
      add_xml_data(L240A.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L240A.SubsectorAll_tra, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L240A.TechShrwt_tra, "TechShrwt") %>%
      add_xml_data(MidEastPriceShock, "TechCost") %>% #price shock
      add_xml_data(L240A.TechCoef_tra, "TechCoef") %>%
      add_xml_data(L240A.Production_tra, "Production") %>%
      add_logit_tables_xml(L240A.Supplysector_reg, "Supplysector") %>%
      add_logit_tables_xml(L240A.SubsectorAll_reg, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L240A.TechShrwt_reg, "TechShrwt") %>%
      add_xml_data(L240A.TechCoef_reg, "TechCoef") %>%
      add_xml_data(L240A.Production_reg_imp, "Production") %>%
      add_xml_data(L240A.Production_reg_dom, "Production") ->
      Refined_Products_HormuzPriceShock.xml

    return_data(liquids_trade.xml,
                Refined_Products_HormuzPriceShock.xml)
  } else {
    stop("Unknown command")
  }
}
