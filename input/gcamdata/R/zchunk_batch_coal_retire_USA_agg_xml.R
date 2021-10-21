# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_coal_retire_USA_agg_xml
#'
#' Construct XML data structure for \code{coal_retire_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_retire_vintage_USAagg.xml}.
module_gcamusa_batch_coal_retire_USA_agg_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2241.StubTechCalInput_elec_coalret_USAagg",
             "L2241.StubTechEff_elec_coalret_USAagg",
             "L2241.StubTechSCurve_elec_coalret_USAagg",
             "L2241.GlobalTechShrwt_elec_coalret_USAagg",
             "L2241.GlobalTechCapFac_elec_coalret_USAagg",
             "L2241.GlobalTechCapital_elec_coalret_USAagg",
             "L2241.GlobalTechOMfixed_elec_coalret_USAagg",
             "L2241.GlobalTechOMvar_elec_coalret_USAagg",
             "L2241.GlobalTechEff_elec_coalret_USAagg",
             "L2241.GlobalTechProfitShutdown_elec_coalret_USAagg",
             # coal vintage
             "L2241.StubTechCalInput_coal_vintage_USAagg",
             "L2241.StubTechEff_coal_vintage_USAagg",
             "L2241.StubTechSCurve_coal_vintage_USAagg",
             "L2241.StubTechProfitShutdown_coal_vintage_USAagg",
             "L2241.GlobalTechShrwt_coal_vintage_USAagg",
             "L2241.GlobalTechEff_coal_vintage_USAagg",
             "L2241.GlobalTechCapFac_coal_vintage_USAagg",
             "L2241.GlobalTechCapital_coal_vintage_USAagg",
             "L2241.GlobalTechOMfixed_coal_vintage_USAagg",
             "L2241.GlobalTechOMvar_coal_vintage_USAagg",
             "L2241.OutputEmissions_elec_coalret_USAagg",
             "L2241.OutputEmissions_coal_vintage_USAagg"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_retire_USAagg.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    tech.share.weight <- share.weight <- sector.name <- supplysector <- subsector.name <- subsector <- NULL

    # Load required inputs

    L2241.StubTechCalInput_elec_coalret_USAagg <- get_data(all_data, "L2241.StubTechCalInput_elec_coalret_USAagg")
    L2241.StubTechEff_elec_coalret_USAagg <- get_data(all_data, "L2241.StubTechEff_elec_coalret_USAagg")
    L2241.StubTechSCurve_elec_coalret_USAagg <- get_data(all_data, "L2241.StubTechSCurve_elec_coalret_USAagg")
    L2241.GlobalTechShrwt_elec_coalret_USAagg <- get_data(all_data, "L2241.GlobalTechShrwt_elec_coalret_USAagg")
    L2241.GlobalTechCapFac_elec_coalret_USAagg <- get_data(all_data, "L2241.GlobalTechCapFac_elec_coalret_USAagg")
    L2241.GlobalTechCapital_elec_coalret_USAagg <- get_data(all_data, "L2241.GlobalTechCapital_elec_coalret_USAagg")
    L2241.GlobalTechOMfixed_elec_coalret_USAagg <- get_data(all_data, "L2241.GlobalTechOMfixed_elec_coalret_USAagg")
    L2241.GlobalTechOMvar_elec_coalret_USAagg <- get_data(all_data, "L2241.GlobalTechOMvar_elec_coalret_USAagg")
    L2241.GlobalTechEff_elec_coalret_USAagg <- get_data(all_data, "L2241.GlobalTechEff_elec_coalret_USAagg")
    L2241.GlobalTechProfitShutdown_elec_coalret_USAagg <- get_data(all_data, "L2241.GlobalTechProfitShutdown_elec_coalret_USAagg")
    # coal vintage
    L2241.StubTechCalInput_coal_vintage_USAagg <- get_data(all_data, "L2241.StubTechCalInput_coal_vintage_USAagg")
    L2241.StubTechEff_coal_vintage_USAagg <- get_data(all_data, "L2241.StubTechEff_coal_vintage_USAagg")
    L2241.StubTechSCurve_coal_vintage_USAagg <- get_data(all_data, "L2241.StubTechSCurve_coal_vintage_USAagg")
    L2241.StubTechProfitShutdown_coal_vintage_USAagg <- get_data(all_data, "L2241.StubTechProfitShutdown_coal_vintage_USAagg")
    L2241.GlobalTechShrwt_coal_vintage_USAagg <- get_data(all_data, "L2241.GlobalTechShrwt_coal_vintage_USAagg")
    L2241.GlobalTechEff_coal_vintage_USAagg <- get_data(all_data, "L2241.GlobalTechEff_coal_vintage_USAagg")
    L2241.GlobalTechCapFac_coal_vintage_USAagg <- get_data(all_data, "L2241.GlobalTechCapFac_coal_vintage_USAagg")
    L2241.GlobalTechCapital_coal_vintage_USAagg <- get_data(all_data, "L2241.GlobalTechCapital_coal_vintage_USAagg")
    L2241.GlobalTechOMfixed_coal_vintage_USAagg <- get_data(all_data, "L2241.GlobalTechOMfixed_coal_vintage_USAagg")
    L2241.GlobalTechOMvar_coal_vintage_USAagg <- get_data(all_data, "L2241.GlobalTechOMvar_coal_vintage_USAagg")
    # emissions
    L2241.OutputEmissions_elec_coalret_USAagg <- get_data(all_data, "L2241.OutputEmissions_elec_coalret_USAagg")
    L2241.OutputEmissions_coal_vintage_USAagg <- get_data(all_data, "L2241.OutputEmissions_coal_vintage_USAagg")

    # Produce outputs
    create_xml("coal_retire_USAagg.xml") %>%
      add_xml_data(L2241.StubTechCalInput_elec_coalret_USAagg, "StubTechCalInput") %>%
      add_xml_data(L2241.StubTechEff_elec_coalret_USAagg, "StubTechEff") %>%
      add_xml_data(L2241.StubTechSCurve_elec_coalret_USAagg, "StubTechSCurve") %>%
      add_xml_data(L2241.GlobalTechShrwt_elec_coalret_USAagg, "GlobalTechShrwt") %>%
      add_xml_data(L2241.GlobalTechCapFac_elec_coalret_USAagg, "GlobalTechCapFac") %>%
      add_xml_data(L2241.GlobalTechCapital_elec_coalret_USAagg, "GlobalTechCapital") %>%
      add_xml_data(L2241.GlobalTechOMfixed_elec_coalret_USAagg, "GlobalTechOMfixed") %>%
      add_xml_data(L2241.GlobalTechOMvar_elec_coalret_USAagg, "GlobalTechOMvar") %>%
      add_xml_data(L2241.GlobalTechEff_elec_coalret_USAagg, "GlobalTechEff") %>%
      add_xml_data(L2241.GlobalTechProfitShutdown_elec_coalret_USAagg, "GlobalTechProfitShutdown") %>%
      # coal vintage
      # add_xml_data(L2241.StubTechCalInput_coal_vintage_USAagg, "StubTechCalInput") %>%
      # add_xml_data(L2241.StubTechEff_coal_vintage_USAagg, "StubTechEff") %>%
      # add_xml_data(L2241.StubTechSCurve_coal_vintage_USAagg, "StubTechSCurve") %>%
      # add_xml_data(L2241.StubTechProfitShutdown_coal_vintage_USAagg, "StubTechProfitShutdown") %>%
      # add_xml_data(L2241.GlobalTechShrwt_coal_vintage_USAagg, "GlobalTechShrwt") %>%
      # add_xml_data(L2241.GlobalTechEff_coal_vintage_USAagg, "GlobalTechEff") %>%
      # add_xml_data(L2241.GlobalTechCapFac_coal_vintage_USAagg, "GlobalTechCapFac") %>%
      # add_xml_data(L2241.GlobalTechCapital_coal_vintage_USAagg, "GlobalTechCapital") %>%
      # add_xml_data(L2241.GlobalTechOMfixed_coal_vintage_USAagg, "GlobalTechOMfixed") %>%
      # add_xml_data(L2241.GlobalTechOMvar_coal_vintage_USAagg, "GlobalTechOMvar") %>%
      add_xml_data(L2241.OutputEmissions_elec_coalret_USAagg, "OutputEmissions") %>%
      # add_xml_data(L2241.OutputEmissions_coal_vintage_USAagg, "OutputEmissions") %>%
      add_precursors("L2241.StubTechCalInput_elec_coalret_USAagg",
                     "L2241.StubTechEff_elec_coalret_USAagg",
                     "L2241.StubTechSCurve_elec_coalret_USAagg",
                     "L2241.GlobalTechShrwt_elec_coalret_USAagg",
                     "L2241.GlobalTechCapFac_elec_coalret_USAagg",
                     "L2241.GlobalTechCapital_elec_coalret_USAagg",
                     "L2241.GlobalTechOMfixed_elec_coalret_USAagg",
                     "L2241.GlobalTechOMvar_elec_coalret_USAagg",
                     "L2241.GlobalTechEff_elec_coalret_USAagg",
                     "L2241.GlobalTechProfitShutdown_elec_coalret_USAagg",
                     # coal vintage
                     # "L2241.StubTechCalInput_coal_vintage_USAagg",
                     # "L2241.StubTechEff_coal_vintage_USAagg",
                     # "L2241.StubTechSCurve_coal_vintage_USAagg",
                     # "L2241.StubTechProfitShutdown_coal_vintage_USAagg",
                     # "L2241.GlobalTechShrwt_coal_vintage_USAagg",
                     # "L2241.GlobalTechEff_coal_vintage_USAagg",
                     # "L2241.GlobalTechCapFac_coal_vintage_USAagg",
                     # "L2241.GlobalTechCapital_coal_vintage_USAagg",
                     # "L2241.GlobalTechOMfixed_coal_vintage_USAagg",
                     # "L2241.GlobalTechOMvar_coal_vintage_USAagg",
                     "L2241.OutputEmissions_elec_coalret_USAagg") ->
                     #"L2241.OutputEmissions_coal_vintage_USAagg") ->
      coal_retire_USAagg.xml

    return_data(coal_retire_USAagg.xml)
  } else {
    stop("Unknown command")
  }
}
