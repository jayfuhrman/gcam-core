# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2441.building_det_mineral
#'
#' Creates level2 data for the building sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:  \code{L2441.GenericBaseServiceMaterials}, \code{L2441.SupplysectorMaterials},
#' \code{L2441.SubsectorLogitMaterials}, \code{L2441.SubsectorShrwtMaterials}, \code{L2441.SubsectorShrwtFlltMaterials},
#' \code{L2441.SubsectorInterpMaterials}, \code{L2441.SubsectorInterpToMaterials}, \code{L2441.TechCalOutputMaterials}, \code{L2441.TechShrwtMaterials},
#' \code{L2441.TechCoefMaterials_final}, \code{L2441.TechLifetimeMaterials}, \code{L2441.TechSCurveMaterials}, \code{L2441.TechProfitShutdownMaterials}
#' @details Creates level2 material services and coefficient data for the building sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete gather nesting unite
#' @author BY December 2024

module_energy_L2441.building_det_mineral <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "minerals/buildings/A44.bld_materials_subsector_shares_reg",
             FILE = "minerals/buildings/A44.bld_materials_intensity_reg",
             FILE = "minerals/buildings/A44.bld_materials_mean_lifetime_vintage_reg",
             FILE = "minerals/buildings/A44.bld_materials_sector",
             FILE = "minerals/buildings/A44.bld_materials_subsector_interp",
             FILE = "minerals/buildings/A44.bld_materials_subsector_logit",
             FILE = "minerals/buildings/A44.bld_materials_subsector_shrwt",
             "L244.gcam_consumer",
             "L244.Floorspace"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2441.GenericBaseServiceMaterials",
             "L2441.SupplysectorMaterials",
             "L2441.SubsectorLogitMaterials",
             "L2441.SubsectorShrwtMaterials",
             "L2441.SubsectorShrwtFlltMaterials",
             "L2441.SubsectorInterpMaterials",
             "L2441.SubsectorInterpToMaterials",
             "L2441.TechCalOutputMaterials",
             "L2441.TechShrwtMaterials",
             "L2441.TechCoefMaterials_final",
             "L2441.TechLifetimeMaterials",
             "L2441.TechSCurveMaterials",
             "L2441.TechPMultMaterials",
             "L2441.TechProfitShutdownMaterials"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")

    L244.Floorspace <- get_data(all_data, "L244.Floorspace", strip_attributes = TRUE)
    L244.gcam_consumer <- get_data(all_data, "L244.gcam_consumer", strip_attributes = TRUE)

    #materials inputs
    A44.bld_materials_subsector_shares_reg <- get_data(all_data, "minerals/buildings/A44.bld_materials_subsector_shares_reg")
    A44.bld_materials_intensity_reg <- get_data(all_data, "minerals/buildings/A44.bld_materials_intensity_reg")
    A44.bld_materials_mean_lifetime_vintage_reg <- get_data(all_data, "minerals/buildings/A44.bld_materials_mean_lifetime_vintage_reg")
    A44.bld_materials_sector <- get_data(all_data, "minerals/buildings/A44.bld_materials_sector")
    A44.bld_materials_subsector_interp <- get_data(all_data, "minerals/buildings/A44.bld_materials_subsector_interp")
    A44.bld_materials_subsector_logit <- get_data(all_data, "minerals/buildings/A44.bld_materials_subsector_logit")
    A44.bld_materials_subsector_shrwt <- get_data(all_data, "minerals/buildings/A44.bld_materials_subsector_shrwt")

    #------------------------------------------------------
    # MATERIALS SERVICE: for tracking material inputs -------------

    # 1. GCAM-CONSUMER: adding a base-service
    # base service will just be 1 (1 materials floorspace service per unit of floorspace)
    L2441.GenericBaseServiceMaterials_resid <- L244.Floorspace %>%
      filter(nodeInput == "resid") %>%
      mutate(decile = gsub("resid_", "", gcam.consumer)) %>%
      mutate(building.service.input = paste0(nodeInput, " materials_", decile),
             base.service = base.building.size) %>%
      select(-decile)

    L2441.GenericBaseServiceMaterials_comm <- L244.Floorspace %>%
      filter(nodeInput == "comm") %>%
      mutate(building.service.input = paste0(nodeInput, " materials"),
             base.service = base.building.size)

    L2441.GenericBaseServiceMaterials <- bind_rows(L2441.GenericBaseServiceMaterials_resid,
                                                   L2441.GenericBaseServiceMaterials_comm) %>%
      select(-base.building.size)

    #2.1: TECHNOLOGY information
    # We create the "add.cg" ("add consumer groups") function to make this process automatic for the different files
    # Filter residential gcam.consumer table
    L244.gcam_consumer_resid <- L244.gcam_consumer %>%
      filter(grepl("resid", gcam.consumer))

    cons.groups<-unique(L244.gcam_consumer_resid$gcam.consumer)
    n.cons.groups<-as.numeric(length(unique(L244.gcam_consumer_resid$gcam.consumer)))

    add.cg<-function(df){
      df.res<-df %>% filter(grepl("resid",supplysector))
      df.comm<-df %>% filter(grepl("comm",supplysector))

      df<- df.res %>%
        repeat_add_columns(tibble::tibble(cons.groups)) %>%
        separate(cons.groups,c("sector","cons.groups"),sep="_") %>%
        unite(supplysector,c(supplysector,cons.groups), sep="_") %>%
        select(-sector) %>%
        bind_rows(df.comm)
      return(df)
    }


    # We need to calculate the floorspace for each building sub-type, and set it as a calOutputValue for each materials supplysector
    # total floorspace by gcam.consumer (10 deciles in residential + 1 commercial) is set in L244.Floorspace
    # multiply total floorspace by sub-type shares from A44.bld_materials_subsector_shares_reg
    # Note we assume the same sub-type floorspace shares in each decile for now

    L2441.TechCalOutputMaterials <- A44.bld_materials_subsector_shares_reg %>%
      rename(supplysector = sector) %>%
      add.cg() %>%
      mutate(technology = subsector) %>%
      left_join_error_no_match(L2441.GenericBaseServiceMaterials, by = c("region", "year", "supplysector" = "building.service.input")) %>%
      full_join(L244.Floorspace, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(calOutputValue = round(base.building.size * flsp_share, energy.DIGITS_CALOUTPUT),
           share.weight.year = year,
           subs.share.weight = if_else(calOutputValue > 0, 1, 0),
           tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) %>%
      # remove technologies that are 0
      filter(calOutputValue != 0)


    #add shareweights for future years
    L2441.TechShrwtMaterials <- L2441.TechCalOutputMaterials %>%
      select(region, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    # #check
    # L2441.TechCalOutputMaterials_total <- L2441.TechCalOutputMaterials %>%
    #   group_by(region, supplysector, year) %>%
    #   dplyr::summarise(value = sum(calOutputValue))


    # Set the material intensity for each technology
    L2441.TechCoefMaterials_hist <- L2441.TechCalOutputMaterials %>%
      select(region, supplysector, subsector, technology, year) %>%
      full_join(A44.bld_materials_intensity_reg, by = c("region", "subsector", "year")) %>%
      select(-sector, -Units) %>%
      # Note that the building material intensity units were specified in kg/m2 which is the same as Mt/bm2.
      # Showing the conversion here for transparency.
      mutate(value = value * CONV_KG_T * CONV_T_MT * CONV_BM2_M2,
             model.year = year,
             coefficient = 0) %>%
      rename(minicam.energy.input = material,
             current.coef = value) %>%
      select(LEVEL2_DATA_NAMES[["RegionalTechMineralCurCoef"]])

    #copy mineral intensity from last base year forward
    L2441.TechCoefMaterials_fut <- L2441.TechCoefMaterials_hist %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year, -model.year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(model.year = year)


    L2441.TechCoefMaterials <- bind_rows(L2441.TechCoefMaterials_hist,
                                        L2441.TechCoefMaterials_fut)


    # Set vintage assumptions
    L2441.TechLifetimeMaterials <- A44.bld_materials_mean_lifetime_vintage_reg %>%
      rename(supplysector = sector) %>%
      add.cg() %>%
      right_join(L2441.TechCalOutputMaterials %>% select(region, supplysector, subsector, technology, year), by = c("region", "supplysector")) %>%
      select(-year) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(year >= max(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechLifetime"]])

    L2441.TechSCurveMaterials <- A44.bld_materials_mean_lifetime_vintage_reg %>%
      rename(supplysector = sector) %>%
      add.cg() %>%
      right_join(L2441.TechCalOutputMaterials %>% select(region, supplysector, subsector, technology, year), by = c("region", "supplysector")) %>%
      select(-year) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(year >= max(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechSCurve"]]) %>%
      #BY 1-24-2025: for 2015 vintages, halve the half life --
      #YQ 04-21-2025: updated half.life and steepness in 2015 to smooth out the retirement of 2015 vintage, so that mineral demand in 2020 has not sudden spike.
      mutate(half.life = if_else(year == 2015, half.life*0.667, half.life),
             steepness = if_else(year == 2015, 0.1, steepness))

    L2441.TechProfitShutdownMaterials <- A44.bld_materials_mean_lifetime_vintage_reg %>%
      rename(supplysector = sector) %>%
      add.cg() %>%
      right_join(L2441.TechCalOutputMaterials %>% select(region, supplysector, subsector, technology, year), by = c("region", "supplysector")) %>%
      select(-year) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechProfitShutdown"]])

    #2.2. SUPPLYSECTOR: adding new materials supplysectors/subsectors/technologies

    #add consumer groups to input assumption files
    A44.bld_materials_sector <- add.cg(A44.bld_materials_sector)
    A44.bld_materials_subsector_interp <- add.cg(A44.bld_materials_subsector_interp)
    A44.bld_materials_subsector_logit <- add.cg(A44.bld_materials_subsector_logit)
    A44.bld_materials_subsector_shrwt <- add.cg(A44.bld_materials_subsector_shrwt)


    # L2441.SupplysectorMaterials: Supplysector info for materials
    L2441.SupplysectorMaterials <- write_to_all_regions(A44.bld_materials_sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                                                        GCAM_region_names = GCAM_region_names)

    #2.3 SUBSECTOR information

    # L2441.SubsectorLogitMaterials: Subsector logit exponents of materials
    L2441.SubsectorLogitMaterials <- write_to_all_regions(A44.bld_materials_subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                                                          GCAM_region_names = GCAM_region_names) %>%
      # only keep the info for the subsectors that exist in TechCalOutput
      semi_join(L2441.TechCalOutputMaterials, by = c("region", "supplysector", "subsector"))


    # L2441.SubsectorShrwtMaterials and L2441.SubsectorShrwtFlltMaterials: Subsector shareweights of materials
    if(any(!is.na(A44.bld_materials_subsector_shrwt$year))) {
      L2441.SubsectorShrwtMaterials <- A44.bld_materials_subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names = GCAM_region_names) %>%
        # only keep the info for the subsectors that exist in TechCalOutput
        semi_join(L2441.TechCalOutputMaterials, by = c("region", "supplysector", "subsector"))
    }
    if(any(!is.na(A44.bld_materials_subsector_shrwt$year.fillout))) {
      L2441.SubsectorShrwtFlltMaterials <- A44.bld_materials_subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names = GCAM_region_names) %>%
        # only keep the info for the subsectors that exist in TechCalOutput
        semi_join(L2441.TechCalOutputMaterials, by = c("region", "supplysector", "subsector"))
    }

    # L2441.SubsectorInterpMaterials and L2441.SubsectorInterpToMaterials: Subsector shareweight interpolation of materials
    if(any(is.na(A44.bld_materials_subsector_interp$to.value))) {
      L2441.SubsectorInterpMaterials <- A44.bld_materials_subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names = GCAM_region_names) %>%
        # only keep the info for the subsectors that exist in TechCalOutput
        semi_join(L2441.TechCalOutputMaterials, by = c("region", "supplysector", "subsector"))
    }
    if(any(!is.na(A44.bld_materials_subsector_interp$to.value))) {
      L2441.SubsectorInterpToMaterials <- A44.bld_materials_subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) %>%
        # only keep the info for the subsectors that exist in TechCalOutput
        semi_join(L2441.TechCalOutputMaterials, by = c("region", "supplysector", "subsector"))
    }

    #------------------------------------------------------------------------------------------------------------------

    ## BY 7-7-2025: Regionalize demands
    ## For minerals that are now traded, we need to differentiate mineral supply and demand
    # Mineral supplies are named as: copper, lithium, nickel
    # Mineral demands are named as: regional copper, regional lithium, regional nickel
    L2441.TechCoefMaterials_regMineralInputs <- regionalize_mineral_inputs(L2441.TechCoefMaterials)


    ## BY 7-28-2025: Modify mineral intensities in the base years such that we would have the equivalent mineral demands if we
    # had the service demand representing solely the new investment (i.e. if base years were vintaged)

    # First, calculate the "new investment" in each base year.
    # We assume a depreciation rate of 5% annually.
    L2441.NewInvestment_Materials <- L2441.TechCalOutputMaterials %>%
      rename(output = calOutputValue) %>%
      group_by(region, supplysector, subsector, technology) %>%
      arrange(year) %>%
      mutate(lag_output = lag(output),
             new_investment = output - lag_output,
             new_investment = pmax(new_investment, 0),
             new_investment = if_else((is.na(new_investment) & !is.na(output)), output, new_investment)) %>%
      ungroup()


    L2441.TechCoefMaterials_modified <- L2441.NewInvestment_Materials %>%
      # Join in the mineral intensity coefficient
      # Using LJ as it is not a 1-to-1 mapping
      left_join(filter(L2441.TechCoefMaterials_regMineralInputs, year %in% MODEL_BASE_YEARS),
                by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      # adjust the mineral intensities by the ratio between the incremental service demand and the original service demand
      mutate(current.coef_new = if_else(output == 0, 0, current.coef * (new_investment / output))) %>%
      # replace the current coef with the incremental current coef
      mutate(current.coef = current.coef_new) %>%
      select(LEVEL2_DATA_NAMES[["RegionalTechMineralCurCoef"]])

    L2441.TechCoefMaterials_modMI <- bind_rows(L2441.TechCoefMaterials_modified,
                                               filter(L2441.TechCoefMaterials_regMineralInputs, !(year %in% MODEL_BASE_YEARS)))

    ##BY 8-19-2025 Annualize mineral intensities
    # By default GCAM output reports the mineral demand associated with new investment for each full period (e.g. 5-years)
    # We want to view annual mineral demand, and therefore we have previously divided output by 5
    # However, to balance calibration, we now need to do this step internally

    L2441.TechCoefMaterials_final <- L2441.TechCoefMaterials_modMI  %>%
      group_by(region, supplysector, subsector, technology, minicam.energy.input) %>%
      arrange(year) %>%
      mutate(years_elapsed = if_else(is.na(lag(year)), 1, year - lag(year)),
             current.coef  = current.coef / years_elapsed) %>%
      ungroup() %>%
      select(-years_elapsed)

    #BY 9-8-2025: Add price multipliers for the mineral component of cost
    # price multiplier is equivalent to 0.13 * the number of years elapsed because new additions are tracked on a timestep basis
    # 0.13 is the fixed-charge-rate. The mineral cost is considered part of the capital cost,
    # so the mineral cost are multiplied by the fixed-charge-rate to get the annuity, which will later be used for calculating technology levelized
    # cost.
    L2441.TechPMultMaterials  <- L2441.TechCoefMaterials_final %>%
      group_by(region, supplysector, subsector, technology, minicam.energy.input) %>%
      arrange(year) %>%
      mutate(price.unit.conversion = 0.13*if_else(is.na(lag(year)), 1, year - lag(year))) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["TechPriceUnitConv"]]) %>%
      distinct()

    #===================================================

    L2441.GenericBaseServiceMaterials %>%
      add_title("Base service for materials service") %>%
      add_units("none") %>%
      add_comments("Base service scales directly with floorspace") %>%
      add_precursors("L244.Floorspace") ->
      L2441.GenericBaseServiceMaterials

    L2441.SupplysectorMaterials %>%
      add_title("Materials supplysector") %>%
      add_units("none") %>%
      add_comments("Materials supplysector for tracking material use") %>%
      add_precursors("minerals/buildings/A44.bld_materials_sector", "L244.gcam_consumer") ->
      L2441.SupplysectorMaterials

    L2441.SubsectorLogitMaterials %>%
      add_title("Materials subsector logit") %>%
      add_units("none") %>%
      add_comments("Materials subsector for tracking material use") %>%
      add_precursors("minerals/buildings/A44.bld_materials_subsector_logit", "common/GCAM_region_names", "L244.gcam_consumer") ->
      L2441.SubsectorLogitMaterials

    if(exists("L2441.SubsectorShrwtMaterials")) {
      L2441.SubsectorShrwtMaterials %>%
        add_title("Subsector shareweights for materials") %>%
        add_units("Unitless") %>%
        add_comments("A44.bld_materials_subsector_shrwt written to all regions") %>%
        add_precursors("minerals/buildings/A44.bld_materials_subsector_shrwt", "common/GCAM_region_names", "L244.gcam_consumer")  ->
        L2441.SubsectorShrwtMaterials
    } else {
      missing_data()  ->
        L2441.SubsectorShrwtMaterials
    }

    if(exists("L2441.SubsectorShrwtFlltMaterials")) {
      L2441.SubsectorShrwtFlltMaterials %>%
        add_title("Subsector shareweights for materials") %>%
        add_units("Unitless") %>%
        add_comments("A44.bld_materials_subsector_shrwt written to all regions") %>%
        add_precursors("minerals/buildings/A44.bld_materials_subsector_shrwt", "common/GCAM_region_names", "L244.gcam_consumer")  ->
        L2441.SubsectorShrwtFlltMaterials
    } else {
      missing_data() ->
        L2441.SubsectorShrwtFlltMaterials
    }

    if(exists("L2441.SubsectorInterpMaterials")) {
      L2441.SubsectorInterpMaterials %>%
        add_title("Subsector shareweight interpolation for materials") %>%
        add_units("NA") %>%
        add_comments("A44.bld_materials_subsector_interp written to all regions") %>%
        add_precursors("minerals/buildings/A44.bld_materials_subsector_interp", "common/GCAM_region_names", "L244.gcam_consumer")  ->
        L2441.SubsectorInterpMaterials
    } else {
      missing_data() ->
        L2441.SubsectorInterpMaterials
    }

    if(exists("L2441.SubsectorInterpToMaterials")) {
      L2441.SubsectorInterpToMaterials %>%
        add_title("Subsector shareweight interpolation for materials") %>%
        add_units("NA") %>%
        add_comments("A44.bld_materials_subsector_interp written to all regions") %>%
        add_precursors("minerals/buildings/A44.bld_materials_subsector_interp", "common/GCAM_region_names", "L244.gcam_consumer")  ->
        L2441.SubsectorInterpToMaterials
    } else {
      missing_data() ->
        L2441.SubsectorInterpToMaterials
    }

    L2441.TechCalOutputMaterials %>%
      add_title("Materials technologies calibrated output") %>%
      add_units("none") %>%
      add_comments("Materials calibrated output (by building sub-type subsectors)") %>%
      add_precursors("L244.Floorspace", "minerals/buildings/A44.bld_materials_subsector_shares_reg") ->
      L2441.TechCalOutputMaterials

    L2441.TechShrwtMaterials %>%
      add_title("Materials technologies calibrated output") %>%
      add_units("none") %>%
      add_comments("Materials calibrated output (by building sub-type subsectors)") %>%
      same_precursors_as(L2441.TechCalOutputMaterials)  ->
      L2441.TechShrwtMaterials

    L2441.TechCoefMaterials_final %>%
      add_title("Materials technologies material coefficients") %>%
      add_units("none") %>%
      add_comments("Material coefficients (by building sub-type)") %>%
      same_precursors_as(L2441.TechCalOutputMaterials) %>%
      add_precursors("minerals/buildings/A44.bld_materials_intensity_reg") ->
      L2441.TechCoefMaterials_final

    L2441.TechPMultMaterials %>%
      add_title("Materials price unit conversion") %>%
      add_units("none") %>%
      add_comments("Materials price unit conversion") %>%
      same_precursors_as(L2441.TechCoefMaterials_final)  ->
      L2441.TechPMultMaterials

    L2441.TechLifetimeMaterials %>%
      add_title("Materials technologies lifetime") %>%
      add_units("none") %>%
      add_comments("Materials technologies lifetime") %>%
      same_precursors_as(L2441.TechCalOutputMaterials) %>%
      add_precursors("minerals/buildings/A44.bld_materials_mean_lifetime_vintage_reg") ->
      L2441.TechLifetimeMaterials

    L2441.TechSCurveMaterials %>%
      add_title("Materialstechnologies s-curve parameters") %>%
      add_units("none") %>%
      add_comments("Materials technologies s-curve parameters") %>%
      same_precursors_as(L2441.TechCalOutputMaterials) %>%
      add_precursors("minerals/buildings/A44.bld_materials_mean_lifetime_vintage_reg") ->
      L2441.TechSCurveMaterials

    L2441.TechProfitShutdownMaterials %>%
      add_title("Materials technologies profit shutdown parameters") %>%
      add_units("none") %>%
      add_comments("Materials technologies profit shutdown parameters") %>%
      same_precursors_as(L2441.TechCalOutputMaterials) %>%
      add_precursors("minerals/buildings/A44.bld_materials_mean_lifetime_vintage_reg") ->
      L2441.TechProfitShutdownMaterials

    return_data(L2441.GenericBaseServiceMaterials, L2441.SupplysectorMaterials,
    L2441.SubsectorLogitMaterials, L2441.SubsectorShrwtMaterials, L2441.SubsectorShrwtFlltMaterials,
    L2441.SubsectorInterpMaterials, L2441.SubsectorInterpToMaterials, L2441.TechCalOutputMaterials, L2441.TechShrwtMaterials,
    L2441.TechCoefMaterials_final, L2441.TechPMultMaterials, L2441.TechLifetimeMaterials, L2441.TechSCurveMaterials, L2441.TechProfitShutdownMaterials
    )

  } else {
    stop("Unknown command")
  }
}
