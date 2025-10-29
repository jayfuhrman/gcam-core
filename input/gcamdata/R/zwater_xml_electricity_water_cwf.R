# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_electricity_water_cwf_xml
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
    return(c("L2233.GlobalIntTechBackup_elec_cool",
             "L2233.GlobalTechSCurve_elec_cool",
             "L2233.GlobalTechLifetime_elec_cool",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil",
             FILE = "energy/A23.globalinttech",
             FILE = "cwf/A23.subsector_shrwt_renew_R_cwf_adj",
             FILE = "cwf/A23.subsector_interp_cwf_adj",
             FILE = "cwf/A23.subsector_shrwt_nuc_R_cwf",
             FILE = "cwf/A23.globaltech_shrwt_no_new_unabated_fossil",
             FILE = "cwf/A23.globaltech_interp_no_new_unabated_fossil"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_water_cwf.xml",
             XML = "electricity_water_cwf_no_new_unabated_fossil.xml",
             XML = "accelerated_fossil_retirement.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    L2233.GlobalTechLifetime_elec_cool <- get_data(all_data, "L2233.GlobalTechLifetime_elec_cool") #
    L2233.GlobalTechSCurve_elec_cool <- get_data(all_data, "L2233.GlobalTechSCurve_elec_cool") #
    L2233.GlobalIntTechBackup_elec_cool <- get_data(all_data, "L2233.GlobalIntTechBackup_elec_cool")
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech", strip_attributes = TRUE)
    L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil <- get_data(all_data, "L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil")
    #cwf addons
    A23.subsector_shrwt_renew_R_cwf_adj <- get_data(all_data, "cwf/A23.subsector_shrwt_renew_R_cwf_adj")
    A23.subsector_interp_cwf_adj <- get_data(all_data, "cwf/A23.subsector_interp_cwf_adj", strip_attributes = TRUE)
    A23.subsector_shrwt_nuc_R_cwf <- get_data(all_data, "cwf/A23.subsector_shrwt_nuc_R_cwf", strip_attributes = TRUE)
    A23.globaltech_shrwt_no_new_unabated_fossil <- get_data(all_data, "cwf/A23.globaltech_shrwt_no_new_unabated_fossil", strip_attributes = TRUE)
    A23.globaltech_interp_no_new_unabated_fossil <- get_data(all_data, "cwf/A23.globaltech_interp_no_new_unabated_fossil", strip_attributes = TRUE)

    # Silence package checks
    technology <- NULL


    # ===================================================
    # CWF adjustments

    # L223.DeleteSubsectorInterp_elec_cwf
    # make adjustments to default SubsectorInter_elec - only keep values from the original
    # that correspond to subsectors not specified in the CWF adjustment file
    L223.DeleteSubsectorInterp_elec_cwf <- L223.SubsectorInterp_elec %>%
      mutate(to.value = 1) %>%
      bind_rows(L223.SubsectorInterpTo_elec %>% select(LEVEL2_DATA_NAMES[["DeleteSubsectorInterpTo"]]) %>%
                  mutate(to.year = as.numeric(to.year))) %>%
      filter(subsector %in% unique(A23.subsector_interp_cwf_adj$subsector))

    # L223.SubsectorInterpTo_elec_cwf
    # similarly only keep values for subsectors not adjusted by CWF changes, so these will not overwrite desired CWF changes
    L223.SubsectorInterpTo_elec_cwf <- A23.subsector_interp_cwf_adj

    # L223.SubsectorShrwt_renew_cwf
    # make adjustments to the default values for subsectors specified in A23.subsector_shrwt_renew_R_cwf_adj
    L223.SubsectorShrwt_renew_cwf <- A23.subsector_shrwt_renew_R_cwf_adj %>%
      mutate(`2025` = to.value) %>%
      gather_years(value_col = "share.weight")

    # L223.SubsectorShrwt_nuc_cwf
    # this just needs to be expanded, these are the final values for all regions
    L223.SubsectorShrwt_nuc_cwf <- A23.subsector_shrwt_nuc_R_cwf %>%
      gather_years(value_col = "share.weight")

    L223.SubsectorShrwtInterp_nuc_cwf <- L223.SubsectorShrwt_nuc_cwf %>%
      mutate(from.year = min(year),
             to.year = max(year),
             apply.to = "share.weight",
             interpolation.function = "linear") %>%
      distinct(region,supplysector,subsector,from.year,to.year,apply.to,interpolation.function)

    L223.SubsectorInterp_elec_cwf <- L223.SubsectorInterp_elec %>%
      filter(subsector %in% unique(A23.subsector_interp_cwf_adj$subsector)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]])

    L223.SubsectorShrwt_renew_cwf  <- L223.SubsectorShrwt_renew_cwf %>%
      #filter(from.year == MODEL_FINAL_BASE_YEAR) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])


    A23.globaltech_shrwt_no_new_unabated_fossil %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.GlobalTechShrwt_elec_all
    # reorders columns to match expected model interface input
    L223.GlobalTechShrwt_elec_all <- L223.GlobalTechShrwt_elec_all[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight")]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechShrwt_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology" = "intermittent.technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechShrwt_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechShrwt_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology" = "intermittent.technology")) ->
      L223.GlobalTechShrwt_elec_no_new_unabated_fossil

    #repeat for fossil phaseout version
    A23.globaltech_interp_no_new_unabated_fossil %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # strips attributes from assumptions file
      mutate(sector.name = sector.name) ->
      L223.GlobalTechInterp_elec_no_new_unabated_fossil


    # Produce xml outputs
    create_xml("electricity_water_cwf.xml") %>%
      add_xml_data(L223.DeleteSubsectorInterp_elec_cwf, "DeleteSubsectorInterpTo") %>% #First delete interp rules for the changes we want to change for CWF
      add_xml_data(L223.SubsectorInterpTo_elec_cwf, "SubsectorInterpTo")  -> # ... then add CWF version
      electricity_water_cwf.xml

    create_xml("electricity_water_cwf_no_new_unabated_fossil.xml") %>%
      add_xml_data(L2233.GlobalTechShrwt_elecPassthru_no_new_unabated_fossil, "GlobalTechShrwt") %>% #disable new unabated fossil generation
      add_xml_data(L223.GlobalTechInterp_elec_no_new_unabated_fossil, "GlobalTechInterp") ->
      electricity_water_cwf_no_new_unabated_fossil.xml

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
                  mutate(year = MODEL_FUTURE_YEARS[1])) -> L2233.GlobalTechSCurve_elec_cool

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

    return_data(electricity_water_cwf.xml,electricity_water_cwf_no_new_unabated_fossil.xml,accelerated_fossil_retirement.xml)
    } else {
    stop("Unknown command")
  }
}
