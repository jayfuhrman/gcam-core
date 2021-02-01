# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_building_USA_EE_xml
#'
#' Construct XML data structure for \code{building_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_USA.xml}. The corresponding file in the
#' original data system was \code{batch_building_USA.xml} (gcamusa XML).
module_gcamusa_batch_building_USA_EE_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A44.bld_ee_flsp"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_USA_EE.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.bld_ee_flsp <- get_data(all_data, "gcam-usa/A44.bld_ee_flsp")

    L244.Floorspace <- A44.bld_ee_flsp %>%
      gather(year, base.building.size, -region, -gcam.consumer, -nodeInput, -building.node.input) %>%
      mutate(year = substr(year, 2, 5)) %>%
      mutate(year = as.integer(year)) %>%
      select(region, gcam.consumer, nodeInput, building.node.input, year, base.building.size)


    # ===================================================

    # Produce outputs
    create_xml("building_USA_EE.xml") %>%
      add_xml_data(L244.Floorspace, "Floorspace") %>%
      add_precursors("gcam-usa/A44.bld_ee_flsp") ->
      building_USA_EE.xml

    return_data(building_USA_EE.xml)
  } else {
    stop("Unknown command")
  }
}
