# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_electricity_water_xml
#'
#' Construct XML data structure for \code{electricity_water.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water.xml} and \code{electricity_water_cwf.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water.xml.R} (water XML).
module_water_electricity_water_cwf_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.SubsectorShrwt_renew_cwf",
             "L223.SubsectorInterp_elec_cwf",
             "L223.SubsectorShrwt_nuc_cwf",
             "L223.SubsectorInterpTo_elec_cwf",
             "L2233.GlobalIntTechBackup_elec_cool",
             "L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil",
             "L223.GlobalTechInterp_elec_no_new_unabated_fossil",
             "L2233.GlobalTechSCurve_elec_cool",
             "L2233.GlobalTechLifetime_elec_cool"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_water_cwf.xml",
             XML = "grid_management_cwf.xml",
             XML = "electricity_water_cwf_no_new_unabated_fossil.xml",
             XML = "accelerated_fossil_retirement.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    L2233.GlobalTechLifetime_elec_cool <- get_data(all_data, "L2233.GlobalTechLifetime_elec_cool") #

    L2233.GlobalTechSCurve_elec_cool <- get_data(all_data, "L2233.GlobalTechSCurve_elec_cool") #

    L223.SubsectorShrwt_renew_cwf <- get_data(all_data, "L223.SubsectorShrwt_renew_cwf")
    L223.SubsectorInterp_elec_cwf <- get_data(all_data, "L223.SubsectorInterp_elec_cwf")
    L223.SubsectorShrwt_nuc_cwf <- get_data(all_data, "L223.SubsectorShrwt_nuc_cwf")
    L223.SubsectorInterpTo_elec_cwf <- get_data(all_data, "L223.SubsectorInterpTo_elec_cwf")
    L2233.GlobalIntTechBackup_elec_cool <- get_data(all_data, "L2233.GlobalIntTechBackup_elec_cool")
    L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil <- get_data(all_data, "L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil")
    L223.GlobalTechInterp_elec_no_new_unabated_fossil <- get_data(all_data, "L223.GlobalTechInterp_elec_no_new_unabated_fossil")

    # Silence package checks
    technology <- NULL


    # ===================================================


    # Produce outputs
    create_xml("electricity_water_cwf.xml") %>%
      add_xml_data(L223.SubsectorInterp_elec_cwf, "SubsectorInterp") %>% # CWF version
      add_xml_data(L223.SubsectorInterpTo_elec_cwf, "SubsectorInterpTo") %>% # CWF version
      add_xml_data(L223.SubsectorShrwt_nuc_cwf, "SubsectorShrwt") %>% # CWF version
      add_xml_data(L223.SubsectorShrwt_renew_cwf, "SubsectorShrwt") %>% # CWF version
      add_precursors("L223.SubsectorInterp_elec_cwf",
                     "L223.SubsectorInterpTo_elec_cwf",
                     "L223.SubsectorShrwt_nuc_cwf",
                     "L223.SubsectorShrwt_renew_cwf") ->
      electricity_water_cwf.xml

    create_xml("electricity_water_cwf_no_new_unabated_fossil.xml") %>%
      add_xml_data(L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil, "GlobalTechShrwt") %>% #disable new unabated fossil generation
      add_xml_data(L223.GlobalTechInterp_elec_no_new_unabated_fossil, "GlobalTechInterp") %>%
      add_precursors("L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil",
                     "L223.GlobalTechInterp_elec_no_new_unabated_fossil") ->
      electricity_water_cwf_no_new_unabated_fossil.xml

    L2233.GlobalIntTechBackup_elec_cool <- L2233.GlobalIntTechBackup_elec_cool %>%
      mutate(backup.capacity.factor = energy.BACKUP_CAPACITY_FACTOR_LOW,
             capacity.limit = energy.CAPACITY_LIMIT_HI)

    create_xml("grid_management_cwf.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2233.GlobalIntTechBackup_elec_cool, "GlobalIntTechBackup") %>%
      add_precursors("L2233.GlobalIntTechBackup_elec_cool") -> grid_management_cwf.xml

    L2233.GlobalTechSCurve_elec_cool <- L2233.GlobalTechSCurve_elec_cool %>%
      filter(sector.name %in% c('elec_coal (conv pul)',
                                'elec_gas (steam/CT)',
                                'elec_gas (CC)',
                                'elec_refined liquids (steam/CT)',
                                'elec_coal (IGCC)',
                                'elec_refined liquids (CC)')) %>%
      mutate(lifetime = round(as.numeric(lifetime / 2),0), as.numeric(lifetime),
             half.life = as.numeric(half.life / 2), as.numeric(half.life))

    L2233.GlobalTechSCurve_elec_cool %>%
      bind_rows(L2233.GlobalTechSCurve_elec_cool %>%
                  mutate(year = 2020)) -> L2233.GlobalTechSCurve_elec_cool

    L2233.GlobalTechLifetime_elec_cool <- L2233.GlobalTechLifetime_elec_cool %>%
      filter(sector.name %in% c('elec_coal (conv pul)',
                                'elec_gas (steam/CT)',
                                'elec_gas (CC)',
                                'elec_refined liquids (steam/CT)',
                                'elec_coal (IGCC)',
                                'elec_refined liquids (CC)')) %>%
      mutate(lifetime = round(as.numeric(lifetime / 2),0), as.numeric(lifetime))

    create_xml("accelerated_fossil_retirement.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2233.GlobalTechSCurve_elec_cool, "GlobalTechSCurve") %>%
      add_xml_data(L2233.GlobalTechLifetime_elec_cool, "GlobalTechLifetime") %>%
      add_precursors("L2233.GlobalTechSCurve_elec_cool") %>%
      add_precursors("L2233.GlobalTechLifetime_elec_cool") -> accelerated_fossil_retirement.xml

    return_data(electricity_water_cwf.xml,grid_management_cwf.xml,electricity_water_cwf_no_new_unabated_fossil.xml,accelerated_fossil_retirement.xml)
    } else {
    stop("Unknown command")
  }
}
