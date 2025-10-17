# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_minerals_trade_xml
#'
#' Construct XML data structure for \code{minerals_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{minerals_trade.xml}, \code{minerals_annual_prod_constraint.xml}. (minerals XML).

module_minerals_trade_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2112.Supplysector_tra",
      "L2112.SectorUseTrialMarket_tra",
      "L2112.SubsectorAll_tra",
      "L2112.TechShrwt_tra",
      "L2112.TechCoef_tra",
      "L2112.Production_tra",
      "L2112.Supplysector_reg",
      "L2112.SubsectorAll_reg",
      "L2112.TechShrwt_reg",
      "L2112.TechCoef_reg",
      "L2111.AnnProdConstraint_InputTax",
      "L2111.AnnProdConstraint_PortfolioStdConstraint"
    )

  MODULE_OUTPUTS <-
    c(XML = "minerals_trade.xml",
      XML = "minerals_annual_prod_constraint.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # ===================================================

    # Produce outputs
    create_xml("minerals_trade.xml") %>%
      add_logit_tables_xml(L2112.Supplysector_tra, "Supplysector") %>%
      add_xml_data(L2112.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L2112.SubsectorAll_tra, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2112.TechShrwt_tra, "TechShrwt") %>%
      add_xml_data(L2112.TechCoef_tra, "TechCoef") %>%
      add_xml_data(L2112.Production_tra, "Production") %>%
      add_logit_tables_xml(L2112.Supplysector_reg, "Supplysector") %>%
      add_logit_tables_xml(L2112.SubsectorAll_reg, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2112.TechShrwt_reg, "TechShrwt") %>%
      add_xml_data(L2112.TechCoef_reg, "TechCoef") %>%
      add_node_equiv_xml("input") %>%
      add_precursors("L2112.Supplysector_tra",
                     "L2112.SectorUseTrialMarket_tra",
                     "L2112.SubsectorAll_tra",
                     "L2112.TechShrwt_tra",
                     "L2112.TechCoef_tra",
                     "L2112.Supplysector_reg",
                     "L2112.SubsectorAll_reg",
                     "L2112.TechShrwt_reg",
                     "L2112.TechCoef_reg") ->
      minerals_trade.xml

    create_xml("minerals_annual_prod_constraint.xml") %>%
      add_xml_data(L2111.AnnProdConstraint_InputTax, "InputTax") %>%
      add_xml_data(L2111.AnnProdConstraint_PortfolioStdConstraint, "PortfolioStdConstraint") %>%
      add_precursors("L2111.AnnProdConstraint_InputTax",
                     "L2111.AnnProdConstraint_PortfolioStdConstraint") ->
      minerals_annual_prod_constraint.xml

  return_data(minerals_trade.xml, minerals_annual_prod_constraint.xml)
} else {
  stop("Unknown command")
}
}
