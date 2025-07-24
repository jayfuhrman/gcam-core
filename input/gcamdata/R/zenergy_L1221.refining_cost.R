# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1221.refining_cost
#'
#' Generates refining sector cost input files based on EIA data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1221.globaltech_capital},
#' \code{L1221.globaltech_OMfixed}, \code{L1221.globaltech_OMvar},
#' \code{L1221.globaltech_margin}.
#' @details Includes EIA FRS and NEMS LMMM data as starting point.
#' @importFrom dplyr filter mutate select group_by summarize slice left_join bind_rows arrange rename coalesce
#' @author MEL Jan 2025

module_energy_L1221.refining_cost <- function(command, ...) {
  if (command == driver.DECLARE_INPUTS) {
    return(c(
      FILE = "energy/A221.globaltech_capital",
      FILE = "energy/A221.globaltech_OMfixed",
      FILE = "energy/A221.globaltech_OMvar",
      FILE = "energy/A221.globaltech_margin",
      FILE = "energy/EIA_FRS_Stats",
      FILE = "energy/EIA_FRS_Opex",
      FILE = "energy/EIA_FRS_PPE",
      FILE = "energy/EIA_FRS_Sales",
      FILE = "energy/EIA_LFMM_Table10",
      FILE = "energy/A221.globaltech_HHV"
    ))
  } else if (command == driver.DECLARE_OUTPUTS) {
    return(c(
      "L1221.globaltech_capital",
      "L1221.globaltech_OMfixed",
      "L1221.globaltech_OMvar",
      "L1221.globaltech_margin"
    ))
  } else if (command == driver.MAKE) {
    all_data <- list(...)[[1]]

    # Silence global package checks
    descriptor <- `schedule/element` <- kbpd <- value <- year <- name <-
      NECC <- ECC <- Sales <- CAPEX <- GJ <- margin <- om_var <- om_fixed <-
      capital <- subsector <- base_year <- cal <- technology <- wacc <- crf <-
      `Cost of Capital (WACC) %` <- `Utilization %` <-
      `Overnight capital cost $/b/sd` <- `Fixed O&M cost $/d/b/sd` <-
      `Non-feedstock variable O&M cost $/b` <- product <- supplysector <- NULL

    # Load required inputs ----------------------------------------------------

    # A221 shell files for tibble structure
    A221.globaltech_capital <- get_data(all_data, "energy/A221.globaltech_capital") %>% gather_years()
    A221.globaltech_OMvar <- get_data(all_data, "energy/A221.globaltech_OMvar") %>% gather_years()
    A221.globaltech_OMfixed <- get_data(all_data, "energy/A221.globaltech_OMfixed") %>% gather_years()
    A221.globaltech_margin <- get_data(all_data, "energy/A221.globaltech_margin") %>% gather_years()

    # EIA FRS files
    EIA_FRS_Opex <- get_data(all_data, "energy/EIA_FRS_Opex") %>% gather_years()
    EIA_FRS_PPE <- get_data(all_data, "energy/EIA_FRS_PPE") %>% gather_years()
    EIA_FRS_Sales <- get_data(all_data, "energy/EIA_FRS_Sales") %>% gather_years()
    EIA_FRS_Stats <- get_data(all_data, "energy/EIA_FRS_Stats") %>% gather_years()

    # EIA Liquid Fuels Module assumptions
    EIA_LFMM_Table10 <- get_data(all_data, "energy/EIA_LFMM_Table10")

    # Mapping and Conversions
    heating_vals <- get_data(all_data, "energy/A221.globaltech_HHV")
    frs_base_years <- c(1977, 1990, 2005, 2009)
    altfuels <- c("Corn ethanol", "Advanced grain ethanol", "Cellulosic ethanol",
                  "Methyl ester biodiesel (FAME)", "Pyrolysis", "FT GTL",
                  "FT CTL", "Biomass-to-liquids (BTL)")

    # Calculate crude refining costs ------------------------------------------
    ## Normalize cost components to $1975/GJ
    ## If costs treated as equivalent across technologies then they can be
    ## calculated on a crude throughput basis.
    ## Assume fixed costs make up 3/4ths of operational expenditures, and that
    ## demonstrated profit margins can be assigned to variable costs. This is
    ## done to allow for the model's zero investment profit constraint. Source:
    ## The Palgrave Handbook of International Energy Economics, Ch 6.1 and 6.2

    frs_throughput <- EIA_FRS_Stats %>% # thousand bbls feed per calendar day
      filter(`schedule/element` == "5242/0500A", year %in% frs_base_years) %>%
      select(year, kbpd = value)

    frs_opex <- EIA_FRS_Opex %>% # nominal annual opex in MMUSD
      select(-c(descriptor, `schedule/element`)) %>%
      filter(year %in% frs_base_years) %>%
      group_by(year) %>%
      summarize(
        NECC = sum(value[name %in% c("Other_Op", "Other_Supply")]),
        ECC = sum(value[name %in% c("Feeds", "Ref_Fuel")]) -
          value[name == "Feed_to_Fuel"],
        .groups = "drop"
      )

    frs_sales <- EIA_FRS_Sales %>% # nominal annual product sales in MMUSD
      filter(`schedule/element` == "5212/1800A", year %in% frs_base_years) %>%
      select(year, Sales = value)

    # nominal annual MMUSD additions to property, plant, equipment
    frs_capex <- EIA_FRS_PPE %>%
      filter(
        `schedule/element` %in% c("5120/4000D", "5120/4000I"),
        year %in% frs_base_years
      ) %>%
      group_by(year) %>%
      summarize(CAPEX = sum(value, na.rm = TRUE), .groups = "drop") %>%
      select(year, CAPEX)

    # Calculate costs from FRS and convert to 1975$/GJ
    crude_bbl_costs <- frs_opex %>%
      left_join(frs_capex, by = "year") %>%
      left_join(frs_sales, by = "year") %>%
      left_join(frs_throughput, by = "year") %>%
      mutate(
        GJ       = kbpd * 1000 * CONV_BBL_GJ * 365,
        margin   = Sales - CAPEX - NECC - ECC,
        om_var   = NECC * .25,
        om_fixed = NECC * .75,
        across(
          -c(year, kbpd, GJ),
          ~ . * 1000000 / GJ * gdp_deflator(1975, base_year = year)
        ),
        subsector = "crude oil refining"
      ) %>%
      select(year, capital = CAPEX, om_var, om_fixed, margin, subsector)

    # Assume 1977 and 2009 data can be proxies for 1971 and 2010 base years
    crude_bbl_costs$year <- c(1971, 1990, 2005, 2010)

    # Normalize Rule of Thumb costs to 1975$ from 2022$ and combine historical
    # and future costs
    A1221.crude_refining_capital <- A221.globaltech_capital %>%
      left_join(crude_bbl_costs, by = c("year", "subsector")) %>%
      filter(subsector == "crude oil refining") %>%
      mutate(
        value = value * gdp_deflator(1975, base_year = 2022) / CONV_BBL_GJ,
        value = dplyr::coalesce(value, capital)
        ) %>%
      select(-c(om_var, om_fixed, capital, margin))

    A1221.crude_refining_OMfixed <- A221.globaltech_OMfixed %>%
      left_join(crude_bbl_costs, by = c("year", "subsector")) %>%
      filter(subsector == "crude oil refining") %>%
      mutate(
        value = value * gdp_deflator(1975, base_year = 2022) / CONV_BBL_GJ,
        value = dplyr::coalesce(value, om_fixed)
        ) %>%
      select(-c(om_var, capital, om_fixed, margin))

    A1221.crude_refining_OMvar <- A221.globaltech_OMvar %>%
      left_join(crude_bbl_costs, by = c("year", "subsector")) %>%
      filter(subsector == "crude oil refining") %>%
      mutate(
        value = value * gdp_deflator(1975, base_year = 2022) / CONV_BBL_GJ,
        value = dplyr::coalesce(value, om_var)
        ) %>%
      select(-c(om_fixed, capital, om_var, margin))


    # Calculate biorefining costs ---------------------------------------------
    ## Biofuels, CTL, GTL cost data from the EIA LFMM assumptions Table 10
    ## CAPEX = CRF * overnight capital cost / utilization
    ## CRF = {i(1 + i)^n} / {[(1 + i)^n]-1}
    ## Variable costs assume a US Gulf Coast facility
    ## Estimates using 20-year plant lifetime and 2022 USD and are converted
    ## to 1975$/bbl. Then 1975$/GJ per product.

    LFMM_cost_bbl <- EIA_LFMM_Table10 %>%
      filter(description %in% altfuels) %>%
      mutate(
        wacc = `Cost of Capital (WACC) %` / 100,
        crf = (wacc * (1 + wacc)^20) / (((1 + wacc)^20) - 1),
        capital = `Overnight capital cost $/b/sd` * crf /
          (365 * `Utilization %` / 100),
        om_fixed = `Fixed O&M cost $/d/b/sd` * `Utilization %` / 100,
        om_var = `Non-feedstock variable O&M cost $/b`,
        across(
          c(capital, om_var, om_fixed),
          ~ . * gdp_deflator(1975, base_year = 2022)
        )
      ) %>%
      select(subsector, technology, product, capital, om_var, om_fixed) %>%
      na.omit

    # Fill out products that aren't currently made with CTL/GTL separately.
    # Capital and variable costs increased to reflect these technologies
    # can't/don't make these these products at scale yet
    # TODO: add expectation of profit to om_var? or subsidy/discount
    # TODO: if using multiplier find one from lit
    MULTIPLIER = 1.25
    placeholder_gtl <- LFMM_cost_bbl %>%
      filter(subsector == "gtl") %>%
      slice(rep(1, 3)) %>%
      mutate(product = c("Gasoline", "Heavy_Residual", "Residual_FuelOil"),
             technology = product,
             capital = capital * MULTIPLIER,
             om_var = om_var * MULTIPLIER)

    placeholder_ctl <- LFMM_cost_bbl %>%
      filter(subsector == "ctl") %>%
      slice(rep(1, 4)) %>%
      mutate(product = c("LPG", "Gasoline", "Heavy_Residual", "Residual_FuelOil"),
             technology = product,
             capital = capital * MULTIPLIER,
             om_var = om_var * MULTIPLIER)

    LFMM_cost_bbl <- bind_rows(LFMM_cost_bbl, placeholder_gtl, placeholder_ctl)

    ## Convert fuel costs to $1975/GJ using EIA HHV converted to LHV to match
    ## IEA data convention. Assume HHV * 0.95 = LHV per API; the conversion to
    ## GJ from MMBTU is the same as BTU to kJ.
    heating_vals$GJ_LHV <- heating_vals$value * 0.95 * CONV_BTU_KJ
    DISCOUNT <- .75 # TODO: apply as proxy for subsidies?
    altcost <- LFMM_cost_bbl %>%
      left_join(heating_vals, by = c("subsector", "product")) %>%
      mutate(
        capital  = capital / GJ_LHV,
        om_var   = om_var / GJ_LHV,
        om_fixed = om_fixed / GJ_LHV
      ) %>%
      select(
        supplysector, subsector, technology, capital, om_var, om_fixed
      )

    A1221.alt_refining_capital <- A221.globaltech_capital %>%
      filter(subsector %in% c("biorefining", "ctl", "gtl")) %>%
      left_join(altcost, by = c("supplysector", "subsector", "technology")) %>%
      select(-c(om_var, om_fixed, value), value = capital)

    A1221.alt_refining_OMfixed <- A221.globaltech_OMfixed %>%
      filter(subsector %in% c("biorefining", "ctl", "gtl")) %>%
      left_join(altcost, by = c("supplysector", "subsector", "technology")) %>%
      select(-c(om_var, capital, value)) %>%
      rename(value = om_fixed)

    A1221.alt_refining_OMvar <- A221.globaltech_OMvar %>%
      filter(subsector %in% c("biorefining", "ctl", "gtl")) %>%
      left_join(altcost, by = c("supplysector", "subsector", "technology")) %>%
      select(-c(om_fixed, capital, value)) %>%
      rename(value = om_var)

    # Regroup subsectors ------------------------------------------------------
    L1221.globaltech_capital <- bind_rows(A1221.alt_refining_capital, A1221.crude_refining_capital)
    L1221.globaltech_OMfixed <- bind_rows(A1221.alt_refining_OMfixed, A1221.crude_refining_OMfixed)
    L1221.globaltech_OMvar <- bind_rows(A1221.alt_refining_OMvar, A1221.crude_refining_OMvar)

    # FRS data stops in 2009. Assume margin term for crude oil refining afterwards
    L1221.globaltech_margin <- A221.globaltech_margin %>%
      left_join(crude_bbl_costs %>% select(-capital, -om_fixed, -om_var),
                by = c("year", "subsector")) %>%
      select(-value) %>%
      left_join(L1221.globaltech_capital %>% select(-minicam.non.energy.input),
                by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(margin = replace_na(margin, -1),
             # Assume alternative fuels get a 15% discount/subsidy
             margin = if_else(margin <= 0 & !grepl("crude", subsector), value * -.05, margin),
             value = if_else(margin == -1, 1, margin)) %>%
      select(-margin)

    # TODO: can do stubtech costs (same adder all technologies in a different region)
    # TODO: calculate the historical discounts

    # Produce outputs ---------------------------------------------------------

    L1221.globaltech_capital %>%
      add_title("Refining sector capital cost structure for GCAM") %>%
      add_units("1975$/GJ") %>%
      add_comments(
        "Will be used in place of A221.globaltech_cost by relevant chunks"
      ) %>%
      add_precursors(
        "energy/A221.globaltech_capital",
        "energy/EIA_FRS_Stats",
        "energy/EIA_FRS_PPE",
        "energy/EIA_LFMM_Table10",
        "energy/A221.globaltech_HHV"
      ) -> L1221.globaltech_capital

    L1221.globaltech_OMfixed %>%
      add_title("Refining sector fixed OM cost structure for GCAM") %>%
      add_units("1975$/GJ") %>%
      add_comments(
        "Will be used in place of A221.globaltech_cost by relevant chunks"
      ) %>%
      add_precursors(
        "energy/A221.globaltech_OMfixed",
        "energy/EIA_FRS_Stats",
        "energy/EIA_FRS_Opex",
        "energy/EIA_FRS_PPE",
        "energy/EIA_FRS_Sales",
        "energy/EIA_LFMM_Table10",
        "energy/A221.globaltech_HHV"
      ) -> L1221.globaltech_OMfixed

    L1221.globaltech_OMvar %>%
      add_title("Refining sector variable OM cost structure for GCAM") %>%
      add_units("1975$/GJ") %>%
      add_comments(
        "Will be used in place of A221.globaltech_cost by relevant chunks"
      ) %>%
      add_precursors(
        "energy/A221.globaltech_OMvar",
        "energy/EIA_FRS_Stats",
        "energy/EIA_FRS_Opex",
        "energy/EIA_FRS_PPE",
        "energy/EIA_FRS_Sales",
        "energy/EIA_LFMM_Table10",
        "energy/A221.globaltech_HHV"
      ) -> L1221.globaltech_OMvar

    L1221.globaltech_margin %>%
      add_title("Refining technology financial profit estimates") %>%
      add_units("1975$/GJ") %>%
      add_comments(
        "Will be used in place of A221.globaltech_cost by relevant chunks"
      ) %>%
      add_precursors(
        "energy/A221.globaltech_margin",
        "energy/EIA_FRS_Stats",
        "energy/EIA_FRS_Opex",
        "energy/EIA_FRS_PPE",
        "energy/EIA_FRS_Sales",
        "energy/calibrated_techs_refining"
      ) -> L1221.globaltech_margin

    return_data(
      L1221.globaltech_capital,
      L1221.globaltech_OMfixed,
      L1221.globaltech_OMvar,
      L1221.globaltech_margin
    )
  } else {
    stop("Unknown command")
  }
}
