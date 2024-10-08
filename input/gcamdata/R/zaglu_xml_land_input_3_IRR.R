# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_land_input_3_IRR_xml
#'
#' Construct XML data structure for \code{land_input_3_IRR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_3_IRR.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_3_IRR.xml.R} (aglu XML).
module_aglu_land_input_3_IRR_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2231.LN3_Logit",
      "L2231.LN3_HistUnmgdAllocation",
      "L2231.LN3_UnmgdAllocation",
      "L2231.LN3_NoEmissCarbon",
      "L2231.LN3_NodeCarbon",
      "L2231.LN3_HistMgdAllocation_noncrop",
      "L2231.LN3_MgdAllocation_noncrop",
      "L2231.LN3_UnmgdCarbon",
      "L2231.LN3_MgdCarbon_noncrop")

  MODULE_OUTPUTS <-
    c(XML = "land_input_3_IRR.xml")

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
    create_xml("land_input_3_IRR.xml") %>%
      add_logit_tables_xml(L2231.LN3_Logit, "LN3_Logit") %>%
      add_xml_data(L2231.LN3_HistUnmgdAllocation, "LN3_HistUnmgdAllocation") %>%
      add_xml_data(L2231.LN3_UnmgdAllocation, "LN3_UnmgdAllocation") %>%
      add_node_equiv_xml("LandLeaf") %>%
      add_node_equiv_xml("carbon-calc") %>%
      add_xml_data(L2231.LN3_NoEmissCarbon, "LN3_NoEmissCarbon") %>%
      add_xml_data(L2231.LN3_NodeCarbon, "LN3_NodeCarbon") %>%
      add_xml_data(L2231.LN3_HistMgdAllocation_noncrop, "LN3_HistMgdAllocation") %>%
      add_xml_data(L2231.LN3_MgdAllocation_noncrop, "LN3_MgdAllocation") %>%
      add_xml_data(L2231.LN3_UnmgdCarbon, "LN3_UnmgdCarbon") %>%
      add_xml_data(L2231.LN3_MgdCarbon_noncrop, "LN3_MgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L2231.LN3_Logit",
                     "L2231.LN3_HistUnmgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_NoEmissCarbon",
                     "L2231.LN3_NodeCarbon",
                     "L2231.LN3_HistMgdAllocation_noncrop",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2231.LN3_UnmgdCarbon",
                     "L2231.LN3_MgdCarbon_noncrop") ->
      land_input_3_IRR.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
