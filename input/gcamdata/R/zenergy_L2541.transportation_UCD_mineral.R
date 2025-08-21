# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2541.transportation_UCD_mineral
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author Author name
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
module_energy_L2541.transportation_UCD_mineral <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "minerals/supply/A10.mineral_rsrc_info",
             FILE = "minerals/transport/A54.trn_globaltech_mineral_coef",
             FILE = "minerals/transport/A54.trn_annual_travel_data",
             "L254.StubTranTechTravel",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechOutput",
             "L254.StubTechProd_nonmotor_PassThrusector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2541.trn_globaltech_mineral_curcoef_final",
             "L2541.trn_globaltech_mineral_coef_final",
             "L2541.StubTranTechCost_no_mineral_cost"))  # input produced by another chunk
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # printlog("Historical GDP and per-capita GDP by state")

    # Load data
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A10.mineral_rsrc_info <- get_data(all_data, "minerals/supply/A10.mineral_rsrc_info", strip_attributes = TRUE)
    A54.trn_globaltech_mineral_coef <- get_data(all_data, "minerals/transport/A54.trn_globaltech_mineral_coef", strip_attributes = TRUE)
    A54.trn_annual_travel_data <- get_data(all_data, "minerals/transport/A54.trn_annual_travel_data", strip_attributes = TRUE)
    L254.StubTranTechTravel <- get_data(all_data, "L254.StubTranTechTravel", strip_attributes = TRUE)
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor", strip_attributes = TRUE)
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost", strip_attributes = TRUE)
    L254.StubTranTechOutput <- get_data(all_data, "L254.StubTranTechOutput", strip_attributes = TRUE)
    L254.StubTechProd_nonmotor_PassThrusector <- get_data(all_data,"L254.StubTechProd_nonmotor_PassThrusector", strip_attributes = TRUE)

    # "person/vehicle and tonnes/vehicle"

    # Process...

    # unique(A54.trn_globaltech_mineral_coef$unit)

    # 1. Pre-process the mineral intensity data,
    # 1.1 make the intensity data unit consistent to be kg/veh-km travel.
    A2541.trn_globaltech_mineral_coef_kg_veh <-
      A54.trn_globaltech_mineral_coef %>%
      # select(-unit) %>%
      gather(key = minicam.energy.input, value = value, 5:last_col()) %>%
      filter(value != 0) %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "technology", "year", "unit", "minicam.energy.input", "value"),
                           GCAM_region_names = GCAM_region_names)

    unique(A2541.trn_globaltech_mineral_coef_kg_veh$unit)

    # unique(A2541.trn_globaltech_mineral_coef_kg_per_travel_part1$sce)

    A2541.trn_globaltech_mineral_coef_kg_per_travel_part1 <-
      A2541.trn_globaltech_mineral_coef_kg_veh %>%
      filter(unit %in% c("kg/seat-km travel", "kg/ton-km", "kg/pass-km")) %>%
      rename(tranSubsector = subsector, stub.technology = technology) %>%
      right_join(L254.StubTranTechLoadFactor,
                by = c("region", "supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(value = value * loadFactor,
             unit = "kg/vkt") %>%
      select(-loadFactor) %>%
      na.omit()

    A2541.trn_globaltech_mineral_coef_kg_per_travel_part2 <-
      A2541.trn_globaltech_mineral_coef_kg_veh %>%
      filter(unit %in% c("kg/vehicle")) %>%
      # rename(tranSubsector = subsector, stub.technology = technology) %>%
      left_join(A54.trn_annual_travel_data %>%
                  gather(key = year, value = value, 6:last_col()) %>%
                  mutate(year = as.numeric(year)) %>%
                  rename(annual_travel = value),
                by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(value = value / annual_travel,
             unit.x = "kg/vkt") %>%
      select(region, supplysector, tranSubsector = subsector, stub.technology = technology, year, unit = unit.x, minicam.energy.input, value) %>%
      # Ideally, we will have travel distance data at different scenarios, for now, we just use the same data for all scenarios.
      repeat_add_columns(tibble(sce = c("CORE", "SSP1", "SSP3", "SSP5")))

    A2541.trn_globaltech_mineral_coef_kg_vtk <-
      A2541.trn_globaltech_mineral_coef_kg_per_travel_part1 %>%
      rbind(A2541.trn_globaltech_mineral_coef_kg_per_travel_part2) %>%
      mutate(model.year = year) %>%
      semi_join(L254.StubTranTechCost %>%
          ungroup() %>%
          select(-minicam.non.energy.input, -input.cost, -sce) %>%
          unique(), by = c("region", "supplysector", "tranSubsector", "stub.technology", "year")) %>%
      ## add back the cycle material intensity (cycle does not have cost information)
      rbind(A2541.trn_globaltech_mineral_coef_kg_per_travel_part2 %>%
              filter(stub.technology == "Cycle") %>%
              mutate(model.year = year))


    # 1.2 Convert the mineral coefficient to current-coefficient--only apply input to the new vintage.

    L2541.trn_globaltech_mineral_curcoef <-
      A2541.trn_globaltech_mineral_coef_kg_vtk %>%
      select(region, supplysector, tranSubsector, stub.technology, minicam.energy.input, year, value, sce) %>%
      distinct() %>%
      repeat_add_columns(tibble(model.year = MODEL_YEARS)) %>%
      left_join(A2541.trn_globaltech_mineral_coef_kg_vtk,
                by = c("region", "supplysector", "tranSubsector", "stub.technology", "minicam.energy.input", "year", "model.year", "sce")) %>%
      replace_na(list(value.y = 0)) %>%
      # mutate(value.y = if_else(is.na(value.y), 0, value.y)) %>%
      select(region, pass.through.sector = supplysector, tranSubsector, stub.technology, year, minicam.energy.input, model.year, current.coef = value.y, sce) %>%
      filter(current.coef != 0) %>%
      distinct()
    # --OUTPUT-- unit based on kg/vkm

    # Produce outputs, add appropriate flags and comments
    # 1 BTU = 0.00105506 MJ
    # this part is just to reverse the unit conversion for energy in C++ code, if that is updated, we need to remove this
    # 1055 is for BTU to J conversion, 1e12 is for MJ to EJ conversion (multiplying service output (Million tkm), that is why
    # J to MJ conversion is considered by default), 1e-3 is for kt to Mt material conversion.
    L2541.trn_globaltech_mineral_curcoef %>%
      mutate(current.coef = current.coef * (1e12/1055) * 1e-3) ->
      L2541.trn_globaltech_mineral_curcoef_Units


    # 1.3 create a coefficent input and assign the value to 0,
    # this allows the model to input mineral coefficient to be 0 for all years, unless we input a non-zero current-coef.
    # This approach avoid unnecessary zero current-coef input.
    L2541.trn_globaltech_mineral_coef_0 <-
      A2541.trn_globaltech_mineral_coef_kg_vtk %>%
      select(region, pass.through.sector = supplysector, tranSubsector, stub.technology, minicam.energy.input, year, coefficient = value, sce) %>%
      mutate(coefficient = 0) %>%
      distinct()
      # --OUTPUT--



    #  2. Mineral cost calcuation, subtract mineral cost from tech non-energy cost
    #  2.1. Calculate the mineral costs based on the mineral price information
    #  collect mineral price data and expand it to all model years (assuming mineral price remain constant for now) The price input is already based on 1990 USD, so no need to do the year conversion for USD
    A10.mineral_price <-
      A10.mineral_rsrc_info %>%
      gather_years %>%
      select(resource, price_unit = `price-unit`, year, value) %>%
      complete(nesting(resource,  price_unit), year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(resource,  price_unit, year) %>%
      group_by(resource, price_unit) %>%
      mutate(value = approx_fun(year, value, rule = 2),
             mineral_price = round(value, energy.DIGITS_COST),
             mineral_price = mineral_price * gdp_deflator(1990, 1975),
             price_unit = "1990$/kg") %>%
      select(resource, price_unit, year, mineral_price)

    #  Calculate the mineral cost for transport technologies (here I am using the mineral intensity data based on "kg/vkt")
    fcr_veh <- energy.DISCOUNT_RATE_VEH +
      energy.DISCOUNT_RATE_VEH / (((1 + energy.DISCOUNT_RATE_VEH) ^ energy.NPER_AMORT_VEH) - 1)


    A2541.trn_globaltech_mineral_cost <-
      A2541.trn_globaltech_mineral_coef_kg_vtk %>%
      # get rid of cycle for calculating the mineral cost, because cycle does not have cost input
      filter(stub.technology != "Cycle") %>%
      mutate(value = value * (1e12/1055) * 1e-3) %>%
      left_join(A10.mineral_price, by = c("minicam.energy.input" = "resource", "year")) %>%
      # convert mineral price to 1990 USD$ to be consistent with tech non-energy cost
      mutate(mineral_cost = value * mineral_price * 0.5 * 1055/10000000000) %>%
      select(region, supplysector, tranSubsector, stub.technology, minicam.energy.input, year, mineral_cost, sce) %>%
      distinct() %>%
      group_by(region, supplysector,  tranSubsector,  stub.technology, year, sce) %>%
      summarise(mineral_cost = sum(mineral_cost, na.rm = TRUE))

    #  2.2 Subtract the mineral cost from the capital cost to calculate the non-mineral capital cost, this data will be used to replace the original
    #  capital cost in the global database. The unit remains as 1990$US/vkm
    L2541.StubTranTechCost_no_mineral_cost <-
      L254.StubTranTechCost %>%
      left_join(A2541.trn_globaltech_mineral_cost,
                by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "sce")) %>%
      # only substract mineral cost for the early year, and the capital cost (no mineral cost) component will follow the cost reduction projection.
      # This approach is to ensure that future non-mineral capital cost will not go negative as the capital cost decrease over time (while mineral
      # cost remain unchanged.)
      mutate(input.cost = input.cost - mineral_cost) %>%
      select(- mineral_cost) %>%
      rename(pass.through.sector = supplysector) %>%
      # spread(key = "year", value = "capital.overnight") %>%
      # fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      # rename(capital.overnight = value) %>%
      # mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) %>%
      select(LEVEL2_DATA_NAMES[["PassThruStubTranTechCost"]], sce)
    # --OUTPUT--

    # options(scipen = 999)
    # L2541.StubTranTechCost_no_mineral_cost_check <-
    #   L254.StubTranTechCost %>%
    #   left_join(A2541.trn_globaltech_mineral_cost,
    #             by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "sce")) %>%
    #   mutate(input.cost.final = input.cost - mineral_cost,
    #          share = mineral_cost/input.cost)


    #------------------------------------------------------------------------------------------------------------------

    ## BY 7-7-2025: Regionalize demands
    ## For minerals that are now traded, we need to differentiate mineral supply and demand
    # Mineral supplies are named as: copper, lithium, nickel
    # Mineral demands are named as: regional copper, regional lithium, regional nickel
    L2541.trn_globaltech_mineral_curcoef_regMineralInputs <- regionalize_mineral_inputs(L2541.trn_globaltech_mineral_curcoef_Units)
    L2541.trn_globaltech_mineral_coef_regMineralInputs <- regionalize_mineral_inputs(L2541.trn_globaltech_mineral_coef_0)

    # First, bind together the calibration values associated with all technologies
    # This includes:
    # Many technologies from L254.StubTranTechCalInput
    # Cycle from L254.StubTranTechOutput
    # get in units of vkm by dividing by load factor
    L2541.StubTranTechOutput_vkm <- L254.StubTranTechOutput %>%
      filter(sce == "CORE") %>%
      mutate(output = output/loadFactor) %>%
      rename(pass.through.sector = supplysector,
             calOutputValue = output) %>%
      select(-loadFactor, -calibrated.value, -coefficient, -minicam.energy.input, -sce) %>%
      bind_rows(L254.StubTechProd_nonmotor_PassThrusector)


    L2541.NewInvestment_Trn <- L2541.StubTranTechOutput_vkm %>%
      rename(output = calOutputValue) %>%
      group_by(region, pass.through.sector, tranSubsector, stub.technology) %>%
      arrange(year) %>%
      mutate(lag_output = lag(output),
             new_investment = output - lag_output,
             new_investment = pmax(new_investment, 0),
             new_investment = if_else((is.na(new_investment) & !is.na(output)), output, new_investment)) %>%
      ungroup() %>%
      select(-sce)

    L2541.trn_globaltech_mineral_curcoef_modified <- L2541.NewInvestment_Trn  %>%
      # Join in the mineral intensity coefficient
      # Using LJ as it is not a 1-to-1 mapping
      left_join(filter(L2541.trn_globaltech_mineral_curcoef_regMineralInputs, year %in% MODEL_BASE_YEARS, sce == "CORE"),
                by = c("region", "pass.through.sector", "tranSubsector", "stub.technology", "year")) %>%
      # adjust the mineral intensities by the ratio between the incremental service demand and the original service demand
      mutate(current.coef_new = if_else(output == 0, 0, current.coef * (new_investment / output))) %>%
      # replace the current coef with the incremental current coef
      mutate(current.coef = current.coef_new) %>%
      select(LEVEL2_DATA_NAMES[["PassThruStubTranTechMineralCurCoef"]])

    # Use modified coefficients where they exist, else default to the original coefficients.
    # modified coefficients only exist for technologies with StubTechProd calibrated values in base years.
    L2541.trn_globaltech_mineral_curcoef_modMI <- L2541.trn_globaltech_mineral_curcoef_regMineralInputs %>%
      left_join(L2541.trn_globaltech_mineral_curcoef_modified, by = c("region", "pass.through.sector", "tranSubsector", "stub.technology", "year",
                                                                           "minicam.energy.input", "model.year"),
                suffix = c(".original", ".new")) %>%
      mutate(current.coef = if_else(is.na(current.coef.new), current.coef.original, current.coef.new)) %>%
      select(LEVEL2_DATA_NAMES[["PassThruStubTranTechMineralCurCoef"]], sce)

    ##BY 8-19-2025 Annualize mineral intensities
    # By default GCAM output reports the mineral demand associated with new investment for each full period (e.g. 5-years)
    # We want to view annual mineral demand, and therefore we have previously divided output by 5
    # However, to balance calibration, we now need to do this step internally

    L2541.trn_globaltech_mineral_curcoef_final <- L2541.trn_globaltech_mineral_curcoef_modMI  %>%
      group_by(region, pass.through.sector, tranSubsector, stub.technology, minicam.energy.input, sce) %>%
      arrange(year) %>%
      mutate(years_elapsed = if_else(is.na(lag(year)), 1, year - lag(year)),
             current.coef  = current.coef / years_elapsed) %>%
      ungroup() %>%
      select(-years_elapsed)

    L2541.trn_globaltech_mineral_coef_final <-  L2541.trn_globaltech_mineral_coef_regMineralInputs


    #------------------------------------------------------------------------------------------------------------------

    L2541.trn_globaltech_mineral_curcoef_final %>%
      add_title("transport sector technology mineral intensity") %>%
      add_units("kg/vkm") %>%
      add_precursors("common/GCAM_region_names", "L254.StubTranTechLoadFactor", "minerals/transport/A54.trn_annual_travel_data",
                     "minerals/transport/A54.trn_globaltech_mineral_coef") %>%
      add_legacy_name("L2541.trn_globaltech_mineral_curcoef_final") %>%
      add_comments("This dataset includes mineral intensity data for transport sector technologies") ->
      L2541.trn_globaltech_mineral_curcoef_final

    L2541.trn_globaltech_mineral_coef_final %>%
      add_title("transport sector technology coefficient") %>%
      add_units("kg/vkm") %>%
      add_precursors("common/GCAM_region_names", "L254.StubTranTechLoadFactor", "minerals/transport/A54.trn_annual_travel_data",
                     "minerals/transport/A54.trn_globaltech_mineral_coef") %>%
      add_legacy_name("L2541.trn_globaltech_mineral_coef_final") %>%
      add_comments("This dataset are just all zero value, which is used to set all current coef as 0 by default. This avoid the unnecessary 0 current coef input") ->
      L2541.trn_globaltech_mineral_coef_final

    L2541.StubTranTechCost_no_mineral_cost %>%
      add_title("TranTechnology costs of transport sector (excluding material cost)") %>%
      add_units("$1990USD/vkm") %>%
      add_precursors("common/GCAM_region_names", "L254.StubTranTechTravel", "L254.StubTranTechLoadFactor",
                     "minerals/transport/A54.trn_globaltech_mineral_coef", "minerals/supply/A10.mineral_rsrc_info",
                     "L254.StubTranTechCost") %>%
      add_legacy_name("L2541.StubTranTechCost_no_mineral_cost") %>%
      add_comments("This dataset includes TranTechnology costs of transport sector (excluding material cost)") ->
      L2541.StubTranTechCost_no_mineral_cost

    return_data(L2541.trn_globaltech_mineral_curcoef_final,
                L2541.trn_globaltech_mineral_coef_final,
                L2541.StubTranTechCost_no_mineral_cost)
  } else {
    stop("Unknown command")
  }
}
