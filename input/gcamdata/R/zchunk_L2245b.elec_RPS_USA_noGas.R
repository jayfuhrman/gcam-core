#' module_gcamusa_L2245b.elec_RPS_USA_noGas
#'
#' Generates the vintage coefficient-based RPS structure for GCAM-USA created for UMD/CGS Projects.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2245.StubTechRESSecondaryOutput_RPS_USA_noGas},
#' \code{L2245.StubTechAdjCoef_RPS_USA_noGas}, \code{L2245.RESPolicy_RPS_USA_noGas}.
#' The corresponding file in the original data system was \code{L2245.RPS_USA.R} (gcam-usa level2).
#' @details This chunk generates input files to create an electricity generation sector with multiple load segments
#' for each state and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS Oct 2019 / YO Jan 2020
module_gcamusa_L2245b.elec_RPS_USA_noGas <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/RPS_tech_MASTER",
             FILE = "gcam-usa/states_rps_region",
             # FILE = "gcam-usa/rps_states_existing",
             FILE = "gcam-usa/rps_states_hundred",
             # FILE = "gcam-usa/rps_states_fedb",
             # FILE = "gcam-usa/rps_states_intv",
             # FILE = "gcam-usa/rps_states_pldg",
             FILE = "gcam-usa/gcam_usa_elec_gen",
             FILE = "gcam-usa/A23.include_in_rps_CES_noGas",
             FILE = "gcam-usa/gcam_usa_elec_cons",
             "L2234.GlobalTechCapture_elecS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2245.StubTechRESSecondaryOutput_RPS_USA_noGas",
             "L2245.StubTechAdjCoef_RPS_USA_noGas",
             "L2245.RESPolicy_RPS_USA_noGas"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- grid_region <- state <- year <- supplysector <- sector <- technology <- units <- rate.notes <-
      Units <- stub.technology <- type <- rps_region <- state_name <- DIVISON <- LRGSTATE <- REGION <-
      subregion13 <- DIVISION2009 <- subregion9 <- subregion4 <- REPORTABLE_DOMAIN <- subregion27 <-
      NEMS <- res.secondary.output <- output.ratio <- minicam.energy.input <- market.name <-
      model.year <- adjusted.coefficient <- CO2 <- policy.portfolio.standard <- policyType <- start.year <-
      constraint <- NULL # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    RPS_tech_MASTER <- get_data(all_data, "gcam-usa/RPS_tech_MASTER", strip_attributes = TRUE)
    states_rps_region <- get_data(all_data, "gcam-usa/states_rps_region", strip_attributes = TRUE)

    # rps_states <- get_data(all_data, "gcam-usa/rps_states_existing") %>%
    #   select(-supplysector, -subsector, -units, -rate.notes) %>%
    #   gather_years(value_col = "rps_share")
    rps_states_exst <- get_data(all_data, "gcam-usa/rps_states_hundred", strip_attributes = TRUE)
    # rps_states_fedb <- get_data(all_data, "gcam-usa/rps_states_fedb")
    # rps_states_intv <- get_data(all_data, "gcam-usa/rps_states_intv")
    # rps_states_pldg <- get_data(all_data, "gcam-usa/rps_states_pldg")

    gcam_usa_elec_gen <- get_data(all_data, "gcam-usa/gcam_usa_elec_gen", strip_attributes = TRUE)
    include_in_rps <- get_data(all_data, "gcam-usa/A23.include_in_rps_CES_noGas", strip_attributes = TRUE)
    gcam_usa_elec_cons <- get_data(all_data, "gcam-usa/gcam_usa_elec_cons", strip_attributes = TRUE)
    L2234.GlobalTechCapture_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapture_elecS_USA", strip_attributes = TRUE)


    # ===================================================
    # Process data

    # Pre-process a couple of tables
    # rps_states <- rps_states_exst %>%
    # rps_states <- rps_states_fedb %>%
    # rps_states <- rps_states_intv %>%
    rps_states <- rps_states_exst %>%
      select(-supplysector, -subsector, -units, -rate.notes) %>%
      gather_years(value_col = "rps_share")

    RPS_tech_MASTER %>%
      repeat_add_columns(tibble::tibble(state = gcamusa.STATES)) %>%
      repeat_add_columns(tibble::tibble(year = c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS))) -> state_tech_table

    # # List of states with RPS policy
    # rps_states_list <- unique(rps_states$region)
    #
    # # Table of states & years with RPS policy in effect
    # rps_states %>%
    #   filter(!is.na(rps_share)) %>%
    #   distinct(region, year) -> rps_states_years

    # List of states & years without RPS policy in effect
    rps_states %>%
      complete(region = gcamusa.STATES,
               year = MODEL_YEARS) %>%
      filter(is.na(rps_share)) %>%
      distinct(region, year) -> non_rps_states_years


    # Create table specifying the supply of RPS credits. We name the market as "ELEC_RPS". The supply is set
    # as RES secondary output with output ratio 1 for renewable technologies. The list of technologies to
    # supply the credits is exogenously read in through a list read in through an assumptions file. This list of technologies
    # is kept uniform across states. In the future, it would be nice to read in the technologies that
    # supply credits by state.

    # # Creating secondary output table
    include_in_rps %>%
      filter(credit_fraction != 0) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      # LJENM throws error because of NAs in new data colums
      # intention is to join CCS tech capture rates to reduce credit (secondary output) accordingly
      # table includes other techs besides CCS; NAs will be replaced subsequently; left_join() is used
      left_join(L2234.GlobalTechCapture_elecS_USA %>%
                  select(-storage.market) %>%
                  rename(credit_adj = remove.fraction),
                by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      # NAs are non-CCS technologies, which generate full credits (rather than partial credits for partial capture)
      # assign 1 "remove fraction" so these techs generate a full credit
      replace_na(list(credit_adj = 1)) %>%
      # the exception to reducing CCS tech RPS credits is biomass, which receives a full credit even without CCS
      mutate(credit_adj = if_else(subsector == "biomass", 1, credit_adj)) %>%
      mutate(res.secondary.output = ELEC_RPS) %>%
      mutate(output.ratio = credit_fraction * credit_adj) %>%
      select(-credit_fraction, -credit_adj) -> L2245.RPS_credit_fraction_USA

    # state_tech_table %>%
    #   semi_join(include_in_rps %>%
    #               filter(type == "Renewable"),
    #             by = "subsector") %>%
    #   mutate(res.secondary.output = ELEC_RPS,
    #          output.ratio = 1) -> L2245.RPS_credit_fraction_USA

    L2245.StubTechRESSecondaryOutput_RPS_USA <-
      state_tech_table %>%
      semi_join(L2245.RPS_credit_fraction_USA,
                by = c("supplysector", "subsector", "stub.technology", "year")) %>%
      left_join_error_no_match(L2245.RPS_credit_fraction_USA,
                               by = c("supplysector", "subsector", "stub.technology", "year"))

    # Create table specifying demand of credits. The demand for credits is set up as minicam-energy-input
    # of ELEC_RPS into all techs in the power sector.
    # The coeffieicnets are read in as "adjusted-coefficient" which can be read in by period and
    # technology vintage year. In a given period, the coefficients are set equal for all vintages at a
    # value equal to the share of RPS technologies expected in that period.

    # First, state-level RPS shares data in long format
    rps_states %>%
      filter(!is.na(rps_share)) %>%
      arrange(region) -> rps_share_state

    # Calculate Grid-level shares based on state-level shares.

    # Convert GCAM Reference generation and consumption data into long format

    cons <- gcam_usa_elec_cons %>%
      select(-scenario, -Units) %>%
      # Convert to long format
      gather(year, cons, -region) %>%
      mutate(year = as.integer(year))

    gen <- gcam_usa_elec_gen %>%
      select(-scenario, -Units) %>%
      # Convert to long format
      gather(year, gen, -region, - subsector, -technology) %>%
      mutate(year = as.integer(year),
             gen = as.numeric(gen)) %>%
      group_by(region, subsector, technology, year) %>%
      summarise(gen = sum(gen))

    # Calculate RPS for non-RPS states based on reference data
    non_rps <- gen %>%
      # Filter to non-RPS states
      semi_join(non_rps_states_years, by = c("region", "year")) %>%
      # LJENM throws error because techs that don't generate credits aren't included in
      # L2245.StubTechRESSecondaryOutput_RPS_USA.  NAs are dealt with below. left_join() is used.
      left_join(L2245.StubTechRESSecondaryOutput_RPS_USA %>%
                  distinct(subsector, technology = stub.technology, year, output.ratio),
                by = c("subsector", "technology", "year")) %>%
      replace_na(list(gen = 0, output.ratio = 0)) %>%
      mutate(type = if_else(output.ratio > 0, "eligible", "non-eligible")) %>%
      # Calculate the total renewable/non-renewable generation in a given year/state
      group_by(region, year, type) %>%
      summarise(gen = sum(gen)) %>%
      # Calculate the renewable share
      mutate(rps_share = gen / sum(gen)) %>%
      ungroup() %>%
      # Filter to Renewable only
      filter(type == "eligible") %>%
      select(region, year, rps_share) %>%
      filter(!is.na(rps_share))

    # Calculate share of rps grid region consumption
    grid_share <- cons %>%
      # Add in grid region, simple left join because more observations on LHS
      left_join(states_rps_region %>%
                  select(state, rps_region),
                by = c("region" = "state")) %>%
      # Calculate proportion of grid consumption by state
      group_by(rps_region, year) %>%
      mutate(grid.share = cons / sum(cons)) %>%
      ungroup() %>%
      select(region, rps_region, year, grid.share) %>%
      filter(!is.na(grid.share))

    # Calculate RPS share of grid consumption.

    # First, combine RPS states with non-RPS states
    rps <- rps_share_state %>%
      bind_rows(non_rps)

    # Next, multiply RPS by share of grid consumption. Simple left join because more observations on LHS
    rps_x_share <- rps %>%
      left_join(grid_share, by = c("region", "year")) %>%
      mutate(grid_rps_share = rps_share * grid.share) %>%
      select(region, rps_region, year, grid_rps_share) %>%
      filter(!is.na(grid_rps_share))

    # Now, sum by grid
    grid_rps <- rps_x_share %>%
      group_by(rps_region, year) %>%
      summarise(value = sum(grid_rps_share)) %>%
      ungroup() %>%
      # Renaming "year" in this table which refers to model year. This is to differentiate from vintage year which we refer to
      # simply as "year" later.
      rename (model.year = year)

    # Finally, Copy the coefficients of the first constriant period (2020) to previous model periods and that of the last constriant year (2050) to remaining
    # model periods.

    grid_rps_years_before_first_const_year <-
      grid_rps %>%
      filter(model.year == FIRST_YEAR_OF_SHARE_CONSTRAINT) %>%
      select(-model.year) %>%
      repeat_add_columns(tibble(model.year = MODEL_YEARS)) %>%
      filter(model.year < FIRST_YEAR_OF_SHARE_CONSTRAINT) %>%
      select(rps_region, model.year, value)

    grid_rps_years_before_final_const_year <-
      grid_rps %>%
      filter(model.year == FINAL_YEAR_OF_SHARE_CONSTRAINT) %>%
      select(-model.year) %>%
      repeat_add_columns(tibble(model.year = MODEL_YEARS)) %>%
      filter(model.year > FINAL_YEAR_OF_SHARE_CONSTRAINT) %>%
      select(rps_region, model.year, value)

    # Bind the tables together.
    grid_rps <-
      grid_rps_years_before_first_const_year %>%
      bind_rows(grid_rps) %>%
      bind_rows(grid_rps_years_before_final_const_year) %>%
      arrange(rps_region,model.year)

    # Create table of adjusted-coefficients by state and technologies. Note that we want to read in
    # the same adjusted-coeffieicnets in a model year for all vintages equal to grid rps share
    # obtained above. Left join instead of error no match, because LHS is more extensive.

    L2245.StubTechAdjCoef_RPS_USA <-
      state_tech_table %>%
      mutate(minicam.energy.input = ELEC_RPS) %>%
      # left_join(states_rps_region, by = c("region" = "state")) %>%
      left_join(states_rps_region, by = c("state")) %>%
      rename(region = state) %>%
      mutate(market.name = region)

    # Populate coefficients for all model years.  Simple left join because LHS has more values.
    Temp <-
      L2245.StubTechAdjCoef_RPS_USA  %>%
      repeat_add_columns(tibble(model.year = MODEL_YEARS))

    Temp %>%
      left_join(grid_rps, by = c("rps_region", "model.year")) %>%
      filter(!is.na(value)) %>%
      # replace_na(list(value = 0)) %>%
      # group_by(region, stub.technology, year) %>%
      # mutate(value = approx_fun(model.year, value, rule = 2)) %>%
      # ungroup() %>%
	  mutate(value = round(value, energy.DIGITS_COEFFICIENT)) %>%
      rename(adjusted.coefficient = value) %>%
      select(region, supplysector, subsector, stub.technology, year,
             minicam.energy.input, market.name, model.year, adjusted.coefficient) -> L2245.StubTechAdjCoef_RPS_USA

    # Create table specifying constraint years and RPS market. Constraint is assumed to be active in all years beyond
    # first constriant year
    rps_share_state %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, grid_region),
                               by = c("region" = "state")) %>%
      # assuming that policy for every state in a grid region goes into effect once a single state's policy begins
      group_by(grid_region) %>%
      summarise(start.year = min(year)) %>%
      ungroup() %>%
      select(grid_region, start.year) -> L2245.RPS_start_year

    L2245.RPS_start_year_min <- min(L2245.RPS_start_year$start.year)

    L2245.RESPolicy_RPS_USA <-
      states_rps_region %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, grid_region),
                               by = "state") %>%
      # not all grids will necessarily have an RPS policy
      # using left_join() to avoid LJENM errors from NAs
      # NAs are dealt with below
      left_join(L2245.RPS_start_year, by = "grid_region") %>%
      mutate(start.year = if_else(is.na(start.year), L2245.RPS_start_year_min, start.year)) %>%
      mutate(policy.portfolio.standard = ELEC_RPS,
             market = rps_region,
             policyType = RES,
             constraint = CONSTRAINT) %>%
      select(region = state, policy.portfolio.standard, market.name = market, policyType, start.year, constraint)


    #Adding CO2 column and moving it to the appropriate place for header purposes
    L2245.StubTechAdjCoef_RPS_USA %>%
      mutate(CO2 = "CO2") -> L2245.StubTechAdjCoef_RPS_USA


    # ===================================================
    # Produce outputs

    # Ensure that columns are in correct order since we're not using L2 data names
    # level2_data_names[["RESPolicy"]] <- c("region","policy.portfolio.standard","market.name","policyType","start.year","constraint")
    # level2_data_names[["StubTechAdjCoef"]] <- c("region","supplysector","subsector","stub.technology","year","minicam.energy.input","market.name","model.year","adjusted.coefficient","CO2")
    # level2_data_names[["StubTechRESSecondaryOutput"]] <- c("region","supplysector","subsector","stub.technology","year","res.secondary.output","output.ratio")

    L2245.StubTechRESSecondaryOutput_RPS_USA %>%
      select(region = state, supplysector, subsector, stub.technology,
             year, res.secondary.output, output.ratio) ->
      L2245.StubTechRESSecondaryOutput_RPS_USA

    L2245.StubTechAdjCoef_RPS_USA %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input,
             market.name, model.year, adjusted.coefficient, CO2) ->
      L2245.StubTechAdjCoef_RPS_USA

    L2245.RESPolicy_RPS_USA %>%
      select(region, policy.portfolio.standard, market.name,
             policyType, start.year, constraint) ->
      L2245.RESPolicy_RPS_USA

    L2245.StubTechRESSecondaryOutput_RPS_USA %>%
      add_title("Secondary output designation for RPS replacement for every state/technology/vintage") %>%
      add_units("unitless") %>%
      add_comments("Defines ELEC_RPS for RES Secondary Output with ratio 1") %>%
      add_legacy_name("L2245.StubTechRESSecondaryOutput_RPS_USA") %>%
      add_precursors("gcam-usa/RPS_tech_MASTER",
                     "gcam-usa/A23.include_in_rps_CES_noGas",
                     "L2234.GlobalTechCapture_elecS_USA") ->
      L2245.StubTechRESSecondaryOutput_RPS_USA_noGas

    L2245.StubTechAdjCoef_RPS_USA %>%
      add_title("Adjusted coefficient w.r.t RPS for every state/technology/vintage combination") %>%
      add_units("unitless") %>%
      add_comments("Coefficients determine how much is getting replaced by RPS") %>%
      add_legacy_name("L2245.StubTechAdjCoef_RPS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/RPS_tech_MASTER",
                     "gcam-usa/states_rps_region",
                     # "gcam-usa/rps_states_existing",
                     "gcam-usa/rps_states_hundred",
                     # "gcam-usa/rps_states_fedb",
                     # "gcam-usa/rps_states_intv",
                     # "gcam-usa/rps_states_pldg",
                     "gcam-usa/gcam_usa_elec_gen",
                     "gcam-usa/gcam_usa_elec_cons",
                     "gcam-usa/A23.include_in_rps_CES_noGas",
                     "L2234.GlobalTechCapture_elecS_USA") ->
      L2245.StubTechAdjCoef_RPS_USA_noGas

    L2245.RESPolicy_RPS_USA %>%
      add_title("Policy start year and portfolio standard defined for all states") %>%
      add_units("unitless") %>%
      add_comments("Acts as a link file for RPS policies") %>%
      add_legacy_name("L2245.RESPolicy_RPS_USA") %>%
      add_precursors("gcam-usa/states_rps_region") ->
      L2245.RESPolicy_RPS_USA_noGas

    return_data(L2245.StubTechRESSecondaryOutput_RPS_USA_noGas,
                L2245.StubTechAdjCoef_RPS_USA_noGas,
                L2245.RESPolicy_RPS_USA_noGas)

  } else {
    stop("Unknown command")
  }
}
