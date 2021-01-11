#' module_gcamusa_batch_RPS_USA_xml
#'
#' Construct XML data structure for \code{RPS_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{RPS_USA.xml}. The corresponding file in the
#' original data system was \code{batch_RPS_USA.xml.R} (gcamusa XML batch).
module_gcamusa_batch_RPS_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2245.StubTechRESSecondaryOutput_RPS_USA",
             "L2245.StubTechAdjCoef_RPS_USA",
             "L2245.RESPolicy_RPS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "RPS_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2245.StubTechRESSecondaryOutput_RPS_USA <- get_data(all_data, "L2245.StubTechRESSecondaryOutput_RPS_USA")
    L2245.StubTechAdjCoef_RPS_USA <- get_data(all_data, "L2245.StubTechAdjCoef_RPS_USA")
    L2245.RESPolicy_RPS_USA <- get_data(all_data, "L2245.RESPolicy_RPS_USA")

    # Produce outputs
    create_xml("RPS_USA.xml") %>%
      add_xml_data(L2245.StubTechRESSecondaryOutput_RPS_USA, "StubTechRESSecondaryOutput", column_order_lookup = NULL) %>%
      add_xml_data(L2245.StubTechAdjCoef_RPS_USA, "StubTechAdjCoef", column_order_lookup = NULL) %>%
      add_xml_data(L2245.RESPolicy_RPS_USA, "RESPolicy", column_order_lookup = NULL) %>%
      add_precursors("L2245.StubTechRESSecondaryOutput_RPS_USA",
                     "L2245.StubTechAdjCoef_RPS_USA",
                     "L2245.RESPolicy_RPS_USA") ->
      RPS_USA.xml

    return_data(RPS_USA.xml)
  } else {
    stop("Unknown command")
  }
}
