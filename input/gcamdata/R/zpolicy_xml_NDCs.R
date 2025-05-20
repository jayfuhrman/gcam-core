# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# module_policy_NDCs_xml
#' @author JF May 2025

module_policy_NDCs_xml <- function(command, ...) {


  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "policy/ghg_constraint_regionalized",
             FILE = "policy/ghg_link",
             FILE = "policy/price_adjust",
             FILE = "policy/demand_adjust"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "NDCs_regionalized.xml",
             XML = "NDCs_global.xml",
             XML = "NDCs_regionalized_link.xml"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]


    # Load required inputs
    ghg_constraint_regionalized <- get_data(all_data, "policy/ghg_constraint_regionalized", strip_attributes = TRUE)
    ghg_link <- get_data(all_data, "policy/ghg_link", strip_attributes = TRUE)
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    price_adjust <- get_data(all_data, "policy/price_adjust", strip_attributes = TRUE)
    demand_adjust <- get_data(all_data, "policy/demand_adjust", strip_attributes = TRUE)


    NDCs_regionalized <- ghg_constraint_regionalized %>%
      tidyr::pivot_longer(cols = -c(year),names_to='market') %>%
      rename(constraint = value) %>%
      right_join(ghg_link, by = 'market') %>%
      complete(year = c(year), nesting(region,market)) %>%
      mutate(policyType = 'tax')


    #aggregate into global constraint file
    NDCs_global <- ghg_constraint_regionalized %>%
      tidyr::pivot_longer(cols = -c(year),names_to='market') %>%
      group_by(year) %>%
      summarize(constraint = sum(value)) %>%
      ungroup() %>%
      mutate(policy.portfolio.standard = "GHG",
             market = "global",
             policyType = "tax") %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]]),
                           GCAM_region_names = GCAM_region_names)

    NDCs_regionalized_link <- price_adjust %>%
      gather_years(value_col = 'price.adjust') %>%
      left_join(demand_adjust, by = 'linked.ghg.policy') %>%
      merge(GCAM_region_names) %>%
      right_join(ghg_link, by = 'region') %>%
      select(-GCAM_region_ID, -policy.portfolio.standard) %>%
      mutate('linked.policy' = 'GHG', year.demand.adjust = year, year.price.adjust = year) %>%
      mutate(demand.adjust = case_when(year == 1975 ~ 0,
                                       (market == "ROW") & !(linked.ghg.policy %in% c("CO2", "CO2_LUC", "CO2_FUG")) & (year == 2040) ~ 0,
                                       TRUE ~ demand.adjust))

    create_xml("NDCs_regionalized.xml") %>%
      add_xml_data(NDCs_regionalized, "PortfolioStdConstraint") %>%
      add_precursors("policy/ghg_constraint_regionalized",
                     "policy/ghg_link") ->
      NDCs_regionalized.xml

    create_xml("NDCs_global.xml") %>%
      add_xml_data(NDCs_global, "PortfolioStdConstraint") %>%
      add_precursors("policy/ghg_constraint_regionalized",
                     "policy/ghg_link") ->
      NDCs_global.xml

    create_xml("NDCs_regionalized_link.xml") %>%
      add_xml_data(NDCs_regionalized_link, "GHGConstrLinkNDC") %>%
      add_precursors("policy/price_adjust",
                     "policy/demand_adjust") ->
      NDCs_regionalized_link.xml



  return_data(NDCs_regionalized.xml,
              NDCs_global.xml,
              NDCs_regionalized_link.xml)

  } else {
    stop("Unknown command")
  }
}
