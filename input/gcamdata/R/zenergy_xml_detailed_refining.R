# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_xml_detailed_refining
#'
#' Construct XML data structure for \code{detailed_refining.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{detailed_refining.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation.xml.R} (energy XML).
#' @importFrom dplyr filter mutate select rename if_else
module_energy_detailed_refining_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2221.Supplysector_en",
             "L2221.ProfitRateSector",
             "L2221.ProfitRateSubsector",
             "L2221.SubsectorLogit_en",
             "L2221.SubsectorShrwtFllt_en",
             "L2221.SubsectorInterpTo_en",
             "L2221.GlobalTechCoef_en",
             "L2221.GlobalTechCost_en",
             "L2221.GlobalTechFractSecOut_en",
             "L2221.GlobalTechResSecOut_en",
             "L2221.GlobalTechZeroProfitOut_en",
             "L2221.GlobalTechShrwt",
             "L2221.GlobalTechShutdown",
             "L2221.Rsrc",
             "L2221.RsrcCal",
             "L2221.RsrcPrice",
             "L2221.StubTechProd",
             "L2221.PortfolioStdConstraint",
             "L2221.PortfolioStdFixedTax",
             "L2221.GlobalTechInterp",
             "L2221.GlobalTechSCurve",
             "L2221.GlobalTechLifetime_en",
             "L2221.GlobalTechProfitShutdown",
             "L2221.SectorZeroProfitMarketName",
             "L2221.StubTechSecondaryOutput",
             "L2221.StubTech_en",
             #"L2221.StubTechShrwt",
             "L2221.StubTechCost",
             "L2221.StubTechTrackCapital_en",
             "L2221.StubTechCoef_refining",

             "L226.TechResSecOutCredit",
             "L226.StubTechCoefInputCredit",
             "L226.PortfolioStdConstraint"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "detailed_refining.xml",
             XML = "USA_ethanol_RFS.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    minicam.energy.input <- price.unit.conversion <- sector.name <-
      profit.rate.technology <- technology <- NULL # silence package checks

    # Load required inputs
    L2221.Supplysector_en <- get_data(all_data, "L2221.Supplysector_en")
    L2221.RsrcCal <- get_data(all_data,"L2221.RsrcCal") %>% filter(resource != "Gasoline_crude oil")
    L2221.ProfitRateSector <- get_data(all_data,"L2221.ProfitRateSector")
    L2221.ProfitRateSubsector <- get_data(all_data,"L2221.ProfitRateSubsector")
    L2221.SubsectorLogit_en <- get_data(all_data, "L2221.SubsectorLogit_en")
    L2221.SubsectorShrwtFllt_en <- get_data(all_data, "L2221.SubsectorShrwtFllt_en")
    L2221.SubsectorInterpTo_en <- get_data(all_data, "L2221.SubsectorInterpTo_en")
    L2221.GlobalTechCoef_en <- get_data(all_data, "L2221.GlobalTechCoef_en")
    L2221.GlobalTechCost_en <- get_data(all_data, "L2221.GlobalTechCost_en")
    L2221.GlobalTechFractSecOut_en <- get_data(all_data, "L2221.GlobalTechFractSecOut_en")
    L2221.GlobalTechResSecOut_en <- get_data(all_data, "L2221.GlobalTechResSecOut_en")
    L2221.GlobalTechZeroProfitOut_en <- get_data(all_data, "L2221.GlobalTechZeroProfitOut_en")
    L2221.GlobalTechShrwt <- get_data(all_data, "L2221.GlobalTechShrwt")
    L2221.Rsrc <- get_data(all_data, "L2221.Rsrc")
    L2221.RsrcPrice <- get_data(all_data, "L2221.RsrcPrice")
    L2221.StubTechProd <- get_data(all_data, "L2221.StubTechProd")
    L2221.PortfolioStdConstraint <- get_data(all_data, "L2221.PortfolioStdConstraint")
    L2221.PortfolioStdFixedTax <- get_data(all_data,'L2221.PortfolioStdFixedTax')
    L2221.GlobalTechInterp <- get_data(all_data,"L2221.GlobalTechInterp")
    L2221.GlobalTechSCurve <- get_data(all_data,"L2221.GlobalTechSCurve")
    L2221.GlobalTechProfitShutdown <- get_data(all_data,"L2221.GlobalTechProfitShutdown")
    L2221.SectorZeroProfitMarketName <- get_data(all_data,"L2221.SectorZeroProfitMarketName")
    L2221.GlobalTechShutdown <- get_data(all_data,"L2221.GlobalTechShutdown")
    L2221.GlobalTechLifetime_en <- get_data(all_data, "L2221.GlobalTechLifetime_en")
    L2221.StubTech_en <- get_data(all_data, "L2221.StubTech_en")
    #L2221.StubTechShrwt <- get_data(all_data, "L2221.StubTechShrwt")
    L2221.StubTechCoef_refining <- get_data(all_data, "L2221.StubTechCoef_refining")
    L2221.StubTechTrackCapital_en <- get_data(all_data, "L2221.StubTechTrackCapital_en")

    L226.TechResSecOutCredit <- get_data(all_data, "L226.TechResSecOutCredit")
    L226.StubTechCoefInputCredit <- get_data(all_data, "L226.StubTechCoefInputCredit")
    L226.PortfolioStdConstraint <- get_data(all_data, "L226.PortfolioStdConstraint")

    L2221.GlobalTechInputPmult <- L2221.GlobalTechCoef_en %>%
      filter(minicam.energy.input == 'refining') %>%
      mutate(price.unit.conversion = 0) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInputPMult"]])

    L2221.StubTechSecondaryOutput <- get_data(all_data,"L2221.StubTechSecondaryOutput")
    L2221.StubTechCost <- get_data(all_data, "L2221.StubTechCost") %>%
      mutate(input.cost = if_else(region == "Ukraine" & year %in% MODEL_FUTURE_YEARS, input.cost + 1.75, input.cost))

    L2221.SubsectorInterp <- L2221.SubsectorInterpTo_en %>%
      filter(subsector %in% c("ctl","gtl","crude oil refining","biorefining 1st gen")) %>%
      mutate(interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]])


    # ===================================================

    # Produce outputs
    create_xml("detailed_refining.xml") %>%
      add_xml_data(L2221.Rsrc, "Rsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2221.RsrcPrice, "RsrcPrice") %>%
      # set up profit rate sector and zero profit market name
      add_xml_data(L2221.ProfitRateSector, "ProfitRateSector") %>%
      add_xml_data(L2221.SectorZeroProfitMarketName, "SectorZeroProfitMarketName") %>%
      add_node_equiv_xml("sector") %>%
      add_logit_tables_xml(L2221.Supplysector_en, "Supplysector") %>%
      add_xml_data(L2221.ProfitRateSubsector, "ProfitRateSubsector") %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L2221.SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L2221.SubsectorInterpTo_en %>% filter(!subsector %in% c("ctl","gtl","crude oil refining")), "SubsectorInterpTo") %>%
      add_xml_data(L2221.SubsectorInterp, "SubsectorInterp") %>%
      add_xml_data(L2221.GlobalTechCoef_en %>%
                     filter(sector.name == 'refining') %>%
                     rename(profit.rate.technology = technology) %>%
                     select(LEVEL2_DATA_NAMES[['GlobalTechProfitRate']]),"GlobalTechProfitRate") %>%
      add_xml_data(L2221.GlobalTechInputPmult, "GlobalTechInputPMult") %>%
      add_xml_data(L2221.GlobalTechFractSecOut_en, "GlobalTechSecOut") %>%
      add_xml_data(L2221.GlobalTechResSecOut_en, "GlobalTechRESSecOut") %>%
      add_xml_data(L2221.StubTech_en, "StubTech") %>%
      #add_xml_data(L2221.StubTechShrwt, "StubTechProd") %>%
      add_xml_data(L2221.StubTechCost, "TechCost") %>%
      add_xml_data(L2221.RsrcCal, "RsrcCal") %>%
      add_xml_data(L2221.StubTechSecondaryOutput, "StubTechSecOut") %>%
      add_xml_data(L2221.GlobalTechZeroProfitOut_en, "GlobalTechZeroProfitOut") %>%
      add_xml_data(L2221.GlobalTechInterp, "GlobalTechInterpTo") %>%
      add_xml_data(L2221.GlobalTechShrwt, "GlobalTechShrwt") %>%
      add_xml_data(L2221.GlobalTechCoef_en, "GlobalTechCoef") %>%
      add_xml_data(L2221.StubTechCoef_refining, "StubTechCoef") %>%
      add_xml_data(L2221.GlobalTechCost_en, "GlobalTechCost") %>%
      #add_xml_data(L2221.StubTechTrackCapital_en, "StubTechTrackCapital") %>%
      add_xml_data(L2221.GlobalTechSCurve, "GlobalTechSCurve") %>%
      add_xml_data(L2221.GlobalTechProfitShutdown, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2221.GlobalTechShutdown, "GlobalTechShutdown") %>%
      add_xml_data(L2221.GlobalTechLifetime_en, "GlobalTechLifetime") %>%
      add_xml_data(L2221.StubTechProd, "StubTechProd") %>%
      add_xml_data(L2221.PortfolioStdFixedTax, "PortfolioStdFixedTax") %>%
      add_xml_data(L2221.PortfolioStdConstraint, "PortfolioStdConstraint") %>%
      add_precursors("L2221.Supplysector_en",
                     "L2221.ProfitRateSubsector",
                     "L2221.SubsectorLogit_en",
                     #"L2221.SubsectorShrwtFllt_en",
                     "L2221.SubsectorInterpTo_en",
                     "L2221.GlobalTechCoef_en",
                     "L2221.GlobalTechCost_en",
                     "L2221.GlobalTechFractSecOut_en",
                     "L2221.GlobalTechResSecOut_en",
                     "L2221.GlobalTechZeroProfitOut_en",
                     "L2221.GlobalTechInterp",
                     "L2221.Rsrc",
                     "L2221.RsrcPrice",
                     "L2221.RsrcCal",
                     "L2221.PortfolioStdConstraint",
                     "L2221.PortfolioStdFixedTax",
                     "L2221.GlobalTechSCurve",
                     "L2221.GlobalTechProfitShutdown",
                     "L2221.GlobalTechLifetime_en",
                     "L2221.SectorZeroProfitMarketName",
                     "L2221.GlobalTechShutdown",
                     "L2221.StubTechCoef_refining",
                     "L2221.StubTechCost",
                     #"L2221.StubTechTrackCapital_en",
                     "L2221.StubTechSecondaryOutput") ->
      detailed_refining.xml

    create_xml("USA_ethanol_RFS.xml") %>%
      add_xml_data(L226.TechResSecOutCredit, "TechRESSecOut") %>%
      add_xml_data(L226.StubTechCoefInputCredit, "StubTechCoef") %>%
      add_xml_data(L226.PortfolioStdConstraint, "PortfolioStdConstraint") %>%
      add_precursors("L226.TechResSecOutCredit",
                     "L226.StubTechCoefInputCredit",
                     "L226.PortfolioStdConstraint") -> USA_ethanol_RFS.xml

    return_data(detailed_refining.xml,
                USA_ethanol_RFS.xml)
  } else {
    stop("Unknown command")
  }
}
