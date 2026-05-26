# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2221.refining
#'
#' Writes all energy supply sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2221.Supplysector_en}, \code{L2221.GlobalTechShutdown}, \code{L2221.GlobalTechLifetime_en}, \code{L2221.ProfitRateSector}, \code{L2221.StubTechCoef_refining}, \code{L2221.ProfitRateSubsector}, \code{L2221.SubsectorLogit_en}, \code{L2221.SubsectorShrwtFllt_en}, \code{L2221.SubsectorInterpTo_en}, \code{L2221.GlobalTechCoef_en}, \code{L2221.GlobalTechCoef_en}, \code{L2221.GlobalTechCost_en}, \code{L2221.GlobalTechFractSecOut_en}, \code{L2221.GlobalTechResSecOut_en}, \code{L2221.GlobalTechZeroProfitOut_en}, \code{L2221.GlobalTechShrwt}, \code{L2221.GlobalTechInterp}, \code{L2221.Rsrc}, \code{L2221.UnlimitRsrc}, \code{L2221.UnlimitRsrcPrice}, \code{L2221.RsrcPrice}, \code{L2221.PortfolioStdConstraint}, \code{L2221.PortfolioStdFixedTax}, \code{L2221.StubTechCalInput}, \code{L2221.BaseService}, \code{L2221.GlobalTechSCurve}, \code{L2221.GlobalTechProfitShutdown}, \code{L2221.SectorZeroProfitMarketName}, \code{L2221.StubTechSecondaryOutput}, \code{L2221.StubTech_en}, \code{L2221.RenewRsrc}, \code{L2221.RsrcCalProd}, \code{L2221.RsrcCurves_fos}, \code{L2221.SmthRenewRsrcCurves_MSW}, \code{L2221.ResTechShrwt}, \code{L2221.PriceElasticity_fuel}, \code{L2221.IncomeElasticity_fuel}. The corresponding file in the
#' original data system was \code{L2221.refining.R} (energy level2).
#' @details This chunk creates level 2 output files for refined liquids supply. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter full_join if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr gather complete crossing
#' @author JF July 2024 SD Jan 2025 ML 2025
module_energy_L2221.refining <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/A221.sector",
      FILE = "energy/A221.subsector_logit",
      FILE = "energy/A221.subsector_shrwt",
      FILE = "energy/A221.subsector_interp",
      FILE = "energy/A221.globaltech_coef",
      FILE = "energy/A221.globaltech_shrwt",
      FILE = "energy/A221.globaltech_interp",
      FILE = "energy/A221.globaltech_retirement",
      FILE = "energy/A221.globaltech_secout",
      FILE = "energy/A221.rsrc_info",
      FILE = "energy/A221.stubtech_regional_output",
      FILE = "energy/calibrated_techs_refining",
      FILE = "energy/refining_mapping",
      FILE = "energy/refining_feed_prices_hist",
      "LB1092.GCAM_REG_LIQUIDS_PROD_agg",
      "LB1092.GCAM_BIO_LIQUIDS_PROD_agg",
      "LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg",
      "L1093.IO_R_oilrefining_F_Yh",
      "L1221.globaltech_capital",
      "L1221.globaltech_OMfixed",
      "L1221.globaltech_OMvar",
      "L1221.globaltech_margin")

  MODULE_OUTPUTS <-
    c("L2221.Supplysector_en",
      "L2221.ProfitRateSector",
      "L2221.ProfitRateSubsector",
      "L2221.SubsectorLogit_en",
      "L2221.SubsectorShrwtFllt_en",
      "L2221.SubsectorInterpTo_en",
      "L2221.StubTechCoef_refining",
      "L2221.GlobalTechCoef_en",
      "L2221.GlobalTechCost_en",
      "L2221.GlobalTechFractSecOut_en",
      "L2221.GlobalTechResSecOut_en",
      "L2221.GlobalTechZeroProfitOut_en",
      "L2221.GlobalTechShrwt",
      "L2221.GlobalTechInterp",
      "L2221.Rsrc",
      "L2221.RsrcPrice",
      "L2221.PortfolioStdConstraint",
      "L2221.PortfolioStdFixedTax",
      "L2221.StubTechProd",
      "L2221.GlobalTechSCurve",
      "L2221.GlobalTechLifetime_en",
      #"L2221.StubTechShrwt",
      "L2221.GlobalTechProfitShutdown",
      "L2221.GlobalTechShutdown",
      "L2221.SectorZeroProfitMarketName",
      "L2221.StubTechSecondaryOutput",
      "L2221.RsrcCal",
      "L2221.StubTechCost",
      "L2221.StubTechTrackCapital_en",
      "L2221.StubTech_en")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    biodiesel <- calibrated.value <- calOutputValue <- input <- coef <-
      coefficient <- resource <- ethanol <- feed_price <- fuel <- input.cost <-
      fractional.secondary.output <- market <- minicam.energy.input <- price <-
      minicam.non.energy.input <- output <- output.ratio <- output.unit <-
      price.unit <- primary.consumption <- region <- sector <- sector.name <-
      supplysector <- share.weight <- stub.technology <- subsector <- unit <-
      subsector.name <- subsector.share.weight <- technology <- to.value <-
      traded <- value <- variable <- year <- year.fillout <- year.share.weight <-
      GCAM_commodity <- GCAM_region_ID <- tech.share.weight <- market.name <-
      passthru_tech_input <- SecOutRatio <- IOcoef <- NULL

    # Load required inputs
    all_data <- list(...)[[1]]
    #all_data <- load_from_cache(MODULE_INPUTS)
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Process crude-based liquids production
    L1221.refineryFuelsOutputsEJ <- LB1092.GCAM_REG_LIQUIDS_PROD_agg %>%
      mutate(input = "crude oil") %>%
      left_join_error_no_match(calibrated_techs_refining %>%
                                 filter(subsector == "crude oil refining") %>%
                                 select(fuel, subsector, input),
                               by = c("fuel", "input")) %>%
      select(region, year, subsector, output = fuel, value, input)

   # Process bio-based liquids production
    L1221.biofuelOutputsEJ <- LB1092.GCAM_BIO_LIQUIDS_PROD_agg %>%
      rename(technology = sector, output = fuel) %>%
      left_join_error_no_match(
        calibrated_techs_refining %>%
          filter(sector == "biorefining") %>%
          select(output = subsector, subsector = sector, input, technology),
        by = c("output", "technology")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, year, subsector, output, value, input)

    # Process ctl/gtl-based liquids production
    L1221.ctl_gtl_OutputsEJ <- LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg %>%
      rename(output = fuel) %>%
      left_join_error_no_match(
        calibrated_techs_refining %>%
          filter(subsector %in% c("ctl", "gtl")) %>%
          select(sector = subsector, subsector = technology, fuel, input),
        by = c("sector", "output" = "fuel")) %>%
      left_join_error_no_match(GCAM_region_names,by = "GCAM_region_ID") %>%
      select(region, year, subsector = sector, output, value, input)

    # Aggregate crude oil, biomass, and ctl/gtl liquids production
    # rbind because it's more strict with columns in case mapping changes
    L1221.refiningFuelsOutputsEJCombined <- L1221.refineryFuelsOutputsEJ %>%
      rbind(L1221.biofuelOutputsEJ, L1221.ctl_gtl_OutputsEJ)

    # A. Output unit, price unit, market
    # TODO: modify product prices here for development convenience, possibly
    # switch to full calculation in here later; oil product prices from SEDS
    L2221.rsrc_info <- A221.rsrc_info %>%
      gather_years() %>%
      # Repeat and add region to resource assumptions table
      repeat_add_columns(select(GCAM_region_names, region)) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = if_else(market == "regional", region, market))

    # L2221.Rsrc: output unit, price unit, and market for depletable resources
    L2221.Rsrc <- L2221.rsrc_info %>%
      filter(resource.type == "resource") %>%
      select(region, resource = resource, output.unit = `output-unit`,
             price.unit = `price-unit`, market) %>%
      distinct()


# Share Weights and Interpolation -----------------------------------------

    L2221.Supplysector_en <- A221.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]],
                             LOGIT_TYPE_COLNAME),
                           has_traded = TRUE,
                           GCAM_region_names = GCAM_region_names)

    L2221.SubsectorLogit_en <- A221.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]],
                             LOGIT_TYPE_COLNAME),
                           has_traded = TRUE,
                           GCAM_region_names = GCAM_region_names)

    if(any(!is.na(A221.subsector_shrwt$year))) {
      L2221.SubsectorShrwt_en <- A221.subsector_shrwt %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]),
                             has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(!is.na(year))
    }

    if(any(!is.na(A221.subsector_shrwt$year.fillout))) {
      L2221.SubsectorShrwtFllt_en <- A221.subsector_shrwt %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]),
                             has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(!is.na(year.fillout))
    }

    if(any(is.na(A221.subsector_interp$to.value))) {
      L2221.SubsectorInterp_en <- A221.subsector_interp %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]], "to.value"),
                             has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(is.na(to.value)) %>%
        select(-to.value)
    }

    if(any(!is.na(A221.subsector_interp$to.value))) {
      L2221.SubsectorInterpTo_en <- A221.subsector_interp %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]]),
                             has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(!is.na(to.value))
    }

    L2221.StubTech_en <- A221.globaltech_shrwt %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTech"]]),
                           has_traded = FALSE,
                           GCAM_region_names = GCAM_region_names)

    L2221.GlobalTechShrwt <- A221.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology),
               year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight")

    L2221.GlobalTechInterp <- A221.globaltech_interp %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      set_years() %>%
      mutate(to.value = 1) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]])


# IO Coefficients ---------------------------------------------------------

    # L222.StubTechCoef_refining: calibrated input-output coefficients of oil
    # refining by region and input. Interpolates values of IO coefficients for
    # base years from historical values
    A222.IO_R_oilrefining_F_Yh <- L1093.IO_R_oilrefining_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(value = IO_coeff) %>%
      complete(nesting(region, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      group_by(region, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      left_join_error_no_match(
        calibrated_techs_refining %>%
          filter(sector == "oil refining") %>%
          select(fuel, subsector, supplysector, minicam.energy.input),
        by = "fuel") %>%
      select(-c(fuel, sector))

    # Use global coef template to fill out historical oil refining coefs into
    # the future.
    # TODO: combine above with list of techs and crossing
    L2221.StubTechCoef_refining <- A221.globaltech_coef %>%
      select(supplysector, subsector, stub.technology = technology,
             minicam.energy.input, `1971`) %>%
      gather_years() %>%
      select(-value) %>%
      filter(subsector == "crude oil refining") %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechYr"]],
                             "minicam.energy.input"),
                           has_traded = FALSE,
                           GCAM_region_names = GCAM_region_names) %>%
      complete(nesting(region, supplysector, subsector,
                       stub.technology, minicam.energy.input),
               year = MODEL_YEARS) %>%
      # plain left join here because we expect NA coefs in future years
      left_join(A222.IO_R_oilrefining_F_Yh,
                by = c("region", "supplysector", "subsector",
                       "minicam.energy.input", "year")) %>%
      group_by(region,supplysector, subsector,
               stub.technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 2),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT),
             market.name = region) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # remove 1971
      select(LEVEL2_DATA_NAMES[["StubTechYr"]],
             "minicam.energy.input", "coefficient", "market.name")

    # Use static global coefs for all other technologies
    # TODO: calibrate these too where possible
    L2221.GlobalTechCoef_en <- A221.globaltech_coef %>%
      gather_years() %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # remove 1971
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])


# Costs -------------------------------------------------------------------
    # TODO: refactor into CAPEX and OPEX for macro capital tracking
    A221.globaltech_cost <-
      bind_rows(L1221.globaltech_capital, L1221.globaltech_OMvar,
                L1221.globaltech_OMfixed, L1221.globaltech_margin,
                L1221.globaltech_margin %>%
                  mutate(value = 0,
                         minicam.non.energy.input = "cost.adjustment")) %>%
      #select(supplysector, subsector, technology, minicam.non.energy.input, year, value) #%>%
      group_by(supplysector, subsector, technology, year) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(minicam.non.energy.input = "non-energy-cost")

    L2221.GlobalTechCost_en <- A221.globaltech_cost %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input),
               year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # remove 1971
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    L2221.ProdPrice <- L2221.rsrc_info %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(region, resource, resource.type, year, price = value)

    # Calculate historic energy input flows for cost calc. The mapping here
    # cheats a little by dropping the 'high' from the crude technology names.
    # Will need to update this if crude technologies are split in history
    product_EJ <- L1221.refiningFuelsOutputsEJCombined %>%
      # only use techs that produced in history
      filter(value > 0) %>%
      left_join_error_no_match(calibrated_techs_refining %>%
                  filter(sector == "refining") %>%
                  select(subsector, fuel, technology, input,
                         resource = secondary.output),
                by = c("subsector", "input", "output" = "fuel")) %>%
      select(region, year, subsector, technology, input, resource, product.EJ = value)

    # Back calculate energy input flows using IO coefficients. Start with crude
    # oil refining, which has regionally-calibrated coefs. All other subsectors
    # currently use the generic global IO coef assumptions.
    crude_inputs_EJ <- product_EJ %>%
      filter(subsector == "crude oil refining") %>%
      # full join to keep the elec and gas coefs and product EJ in the same rows
      full_join(L2221.StubTechCoef_refining %>%
                  filter(stub.technology == "high Gasoline",
                         year %in% MODEL_BASE_YEARS,
                         coefficient != 0) %>%
                  select(region, year, subsector, minicam.energy.input, coefficient),
                by = c("region", "year", "subsector"),
                # repeating rows on purpose for the non-feedstock energy inputs
                relationship = 'many-to-many')

    noncrude_inputs_EJ <- product_EJ %>%
      filter(subsector != "crude oil refining") %>%
      # another many-to-many relationship so regular left_join
      left_join(L2221.GlobalTechCoef_en %>%
                filter(subsector.name != "crude oil refining") %>%
                select(year, subsector = subsector.name, technology,
                       minicam.energy.input, coefficient),
              by = c("subsector", "technology", "year"),
              relationship = 'many-to-many')

    # Now can actually calc the input EJ. Note this is not total input EJ, but
    # EJ per individual feedstock to each technology
    feed_EJ <- bind_rows(crude_inputs_EJ, noncrude_inputs_EJ) %>%
      mutate(feed.EJ = product.EJ * coefficient)

    # TODO: unit costs for each energy input from output db, better way?
    feed_prices <- refining_feed_prices_hist %>%
      as_tibble() %>%
      mutate(`1975` = `1990`) %>%
      select(region, sector, `1975`, `1990`, `2005`, `2010`, `2015`, `2021`) %>%
      gather_years() %>%
      rename(price = value)

    # Current method finds the total inputGJ and input cost per subsector in
    # each region and year. It also calculates the unit energy cost (en.cost).
    en_cost <- feed_EJ %>%
      left_join_error_no_match(
        feed_prices,
        by = c("region", "year", "minicam.energy.input" = "sector")) %>%
      # some technologies have multiple energy costs (e.g. crude/gas/elec)
      group_by(region, year, resource) %>%
      #summarise(feed.cost = sum(feed.EJ * 1E9 * price), .groups = "drop")
      mutate(feed_GJ = sum(feed.EJ * 1E9),
             feed.cost = sum(feed.EJ * 1E9 * price),
             # Call energy cost the $$ spent per unit product made
             en.cost = feed.cost / (product.EJ * 1E9)) %>%
      ungroup() %>%
      distinct() %>%
      select(region, year, subsector, technology, resource, feed.cost, en.cost)

    # All crude refining technology costs are assumed to be equal right now, so
    # we can calculate non-energy costs on a product EJ basis rather than the EJ
    # attributed to each hypothetical crude refining technology. If that changes
    # in the future will need to calculate product costs attributed to each tech
    non_en_cost <- product_EJ %>%
      left_join_error_no_match(
        L2221.GlobalTechCost_en %>%
          filter(year %in% MODEL_BASE_YEARS) %>%
          mutate(technology = gsub("high ", "", technology)) %>%
          select(subsector = subsector.name, technology, year, input.cost),
        by = c("subsector", "year", "technology")) %>%
      mutate(non.en.cost = input.cost * product.EJ * 1E9) %>%
      select(region, year, subsector, technology, resource, input.cost, non.en.cost)

    # Add regional cost variation based on what was required to meet a normal
    # profit rate in history (i.e. costs = revenue)
    product_revs <- product_EJ %>%
      left_join_error_no_match(L2221.ProdPrice, by = c("region", "year", "resource")) %>%
      group_by(region, subsector, year) %>%
      mutate(rev = price * product.EJ * 1E9,
             wt_price = sum(rev) / sum(product.EJ * 1E9)) %>%
      ungroup() %>%
      select(region, year, subsector, technology, resource, product.EJ, price, wt_price, rev)

    profit_rate_calcs <- en_cost %>%
      left_join_error_no_match(
        non_en_cost,
        by = c("region", "year", "subsector", "technology", "resource")) %>%
      left_join_error_no_match(
        product_revs,
        by = c("region", "year", "subsector", "technology", "resource")) %>%
      mutate(tot_cost = non.en.cost + feed.cost) %>%
      group_by(region, subsector, year) %>%
      mutate(subsector_discrepancy = sum(rev) - sum(tot_cost),
             pr_init = sum(tot_cost) / sum(rev),
             cost.adj = (subsector_discrepancy / sum(product.EJ) / 1E9)) %>%
      ungroup() %>%
      # this is janky, but without regional price / cost data it's to get us in
      # the ballpark. either global estimate could be off.
      mutate(
        # apply the adjustment in fractions to the cost and price to make the
        # historical profit rate exactly 1. This calc stops neither costs nor
        # prices from going negative.
        costsum = input.cost + en.cost,
        off = wt_price - costsum,

        adj.price = round(price - off * .5, energy.DIGITS_COST),
        old.input.cost = input.cost,
        input.cost = round(input.cost + off * .5, energy.DIGITS_COST))

    # # # TODO: temporary troubleshooting # #
    # pr_vars <- profit_rate_calcs %>%
    # select(region, year, subsector, resource, product.EJ,
    #          cost.adj, old.input.cost, input.cost, price, adj.price)
    # check_pr <- profit_rate_calcs %>%
    #   group_by(region, year) %>%
    #   summarise(pr = ((sum(feed.cost) + sum(input.cost * product.EJ * 1E9)) /
    #               sum(adj.price * product.EJ * 1E9)), .groups = "drop")
    # summary(check_pr)
    # summary(pr_vars)
    # #                               # #

    L2221.StubTechCost <- profit_rate_calcs %>%
      mutate(supplysector = "refining",
             minicam.non.energy.input = "non-energy-cost") %>%
      # TODO: left_join_error_no_match?
      left_join(
        L2221.GlobalTechCost_en,
        by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
               "technology", "minicam.non.energy.input", "year")) %>%
      mutate(input.cost = round(input.cost.x, energy.DIGITS_COST),
             technology = if_else(grepl("crude", subsector),
                                  paste0("high ", technology),
                                  technology)) %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])
    L2221.StubTechCost <- L2221.StubTechCost %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      complete(nesting(region, supplysector, subsector, technology,
                       minicam.non.energy.input, input.cost),
               year = MODEL_YEARS) %>%
      bind_rows(L2221.StubTechCost) %>%
      arrange(region, supplysector, subsector, year) %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]]) %>%
      distinct()

    # TODO: from the old en_transformation. incorporate capital tracking into detailed refining
    # # L222.GlobalTechTrackCapital_en: We want track capital investments for these technologies thus
    # # we will change the object type accordingly and add the market name which will track investments
    # # and the fraction of the total non-energy cost we should assume is annual investment in capital
    FCR <- (socioeconomics.DEFAULT_INTEREST_RATE * (1 + socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.REFINING_CAP_PAYMENTS) /
      ((1 + socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.REFINING_CAP_PAYMENTS - 1)
    L2221.StubTechTrackCapital_en <- L2221.StubTechCost %>%
      rename(stub.technology = technology) %>%
      mutate(capital.coef = socioeconomics.REFINING_CAPITAL_RATIO / FCR,
             tracking.market = socioeconomics.EN_CAPITAL_MARKET_NAME,
             # refining has vintaging so no need to for depreciation rate
             # (although will get ignored)
             depreciation.rate = 0) %>%   # 1/30 with no vintaging
      select(LEVEL2_DATA_NAMES[['StubTechTrackCapital']])

    # L2221.RsrcPrice: historical prices for depletable resources
    L2221.RsrcPrice <- L2221.rsrc_info %>%
      filter(resource.type == "resource", year %in% MODEL_BASE_YEARS) %>%
      left_join(profit_rate_calcs %>% select(region, year, resource, adj.price),
                by = c("region", "year", "resource"))  %>%
      mutate(adj.price = replace_na(adj.price, 0),
             # let prices for things that didn't exist in history be the default
             # TODO: or set it to some very high price
             price = if_else(adj.price == 0, value, adj.price),
             # don't let product prices go negative
             #price = if_else(price < 0, .15, price),
             price = round(price, 2)) %>%
      select(region, resource, year, price)


# Calibrated Production ---------------------------------------------------

    # Complete region, year, subsector, output, input combinations
    L2221.StubTechProd_fuels <- calibrated_techs_refining %>%
      select(supplysector, subsector, technology, input) %>%
      filter(supplysector %in% energy.REFINED_LIQUIDS) %>%
      distinct() %>%
      # TODO: change this when mapping fixed upstream
      mutate(fuel = if_else(input == "crude oil", "oil", input)) %>%
      write_to_all_regions(c("region", colnames(.)), has_traded = FALSE,
                           GCAM_region_names = GCAM_region_names) %>%
      tidyr::crossing(year = MODEL_BASE_YEARS)  %>%
      # now have a complete template of years, regions, technologies, products
      # and can now add back in the calibrated historical production (using a
      # regular left join as techs with zero historical production will be NA)
      left_join(product_EJ %>%
                  # grab the fuel out of the resource name via regex
                  mutate(supplysector = sub("_([^_]*)$", "", resource)) %>%
                  select(region, year, supplysector, input, value = product.EJ),
                by = c("region", "year", "supplysector", "input")) %>%
      # explicitly zero out combos not in history to ensure those periods solve
      replace(., is.na(.), 0) %>%
      rename(stub.technology = input) %>%
      # TODO: does the below cause symmetry problems for solution?
      #filter(!(technology == "PetCoke" & stub.technology %in% c("coal", "natural gas"))) %>%
      select(-c(technology, fuel)) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = share.weight) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]], "share.weight")

    # Add stub.tech share weights for refining technologies
    L2221.StubTechProd_refining <- L2221.StubTechProd_fuels %>%
      left_join_error_no_match(
        refining_mapping,
        by = c("supplysector", "subsector", "stub.technology")) %>%
      select(-c(supplysector, subsector, stub.technology)) %>%
      rename(supplysector = supplysector_1,
             subsector = subsector_2,
             stub.technology = stub.technology_1) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = share.weight) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]], "share.weight")

    L2221.StubTechProd <- bind_rows(L2221.StubTechProd_fuels,
                                    L2221.StubTechProd_refining)

    zL2221.StubTechCoef_refining <- L2221.GlobalTechCoef_en %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTechCoef"]],
                           GCAM_region_names = GCAM_region_names) %>%
      bind_rows(L2221.StubTechCoef_refining %>% filter(year %in% MODEL_BASE_YEARS))

    zL2221.StubTechProd <- L2221.StubTechProd

    zL2221.StubTechProdCalInput <- zL2221.StubTechProd %>%
      left_join(zL2221.StubTechCoef_refining, by = c("region","supplysector","subsector","stub.technology","year")) %>%
      mutate(CalInputValue = calOutputValue * coefficient) %>%
      group_by(region,year,minicam.energy.input) %>%
      summarize(CalInputValue = sum(CalInputValue)) %>%
      ungroup()

    zL2221.StubTechProdCalOutput <- zL2221.StubTechProd %>%
      group_by(region,year,supplysector) %>%
      summarize(calOutputValue = sum(calOutputValue)) %>%
      ungroup()

    # Set resources to 'fully-calibrated' to reduce solution issues in history
    # TODO: rename this output to something more obvious
    L2221.RsrcCal <- L2221.rsrc_info %>%
      distinct(region, resource) %>%
      mutate(fully.calibrated = 1) %>%
      select(LEVEL2_DATA_NAMES[["RsrcCal"]])

    # Secondary output ratios
    SecondaryOutputs <- A221.globaltech_secout %>%
      gather_years() %>%
      complete(nesting(sec.output.type,supplysector, subsector,
                       technology, fractional.secondary.output),
               year = MODEL_YEARS) %>%
      group_by(sec.output.type,supplysector, subsector,
               technology, fractional.secondary.output) %>%
      mutate(output.ratio = round(approx_fun(year, value, rule = 2),
                                  energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sec.output.type, sector.name = supplysector,
             subsector.name = subsector, technology,
             secondary.output = fractional.secondary.output, output.ratio, year)

    L2221.GlobalTechFractSecOut_en <- SecondaryOutputs %>%
      filter(sec.output.type == 'secondary-output') %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechSecOut']])

    L2221.GlobalTechResSecOut_en <- SecondaryOutputs %>%
      filter(sec.output.type == 'res-secondary-output') %>%
      rename(res.secondary.output = secondary.output) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechRESSecOut']])

    L2221.GlobalTechZeroProfitOut_en <- SecondaryOutputs %>%
      filter(sec.output.type == 'zero-profit-output') %>%
      rename(zero.profit.output = secondary.output) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechZeroProfitOut']])

    # Use region-specific High Residual_FuelOil ratios for outlier regions
    L2221.StubTechSecondaryOutput <- A221.stubtech_regional_output %>%
      select(-crude.type) %>%
      gather(key = "secondary.output", value = output.ratio, -region) %>%
      mutate(supplysector = "refining",
             subsector = "crude oil refining",
             stub.technology = 'high Residual_FuelOil',
             secondary.output = paste0(secondary.output,"_crude oil"),
             # Only apply technology co-product yields to the future
             year = min(MODEL_FUTURE_YEARS)) %>%
      complete(nesting(region, supplysector, subsector, stub.technology,
                       secondary.output),
               year = MODEL_FUTURE_YEARS) %>%
      group_by(region) %>%  # only fill regions with ratios specified for 2025
      fill(output.ratio, .direction = "down") %>%
      ungroup() %>%
      filter(!is.na(output.ratio)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechSecOut"]])

    zL2221.GlobalTechSecondaryOutputCombined <-
      bind_rows(L2221.GlobalTechFractSecOut_en %>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name,
                         stub.technology = technology),
                L2221.GlobalTechResSecOut_en %>%
                         rename(supplysector = sector.name,
                         subsector = subsector.name,
                         stub.technology = technology,
                        secondary.output = res.secondary.output)) %>%
      filter(year %in% MODEL_BASE_YEARS)

    zL221.CalOutputRefining <- L2221.StubTechProd %>% left_join(zL2221.GlobalTechSecondaryOutputCombined) %>%
      mutate(CalSecOutputValue = calOutputValue * output.ratio) %>%
      group_by(region,year,secondary.output) %>%
      summarize(calOutputValue = sum(CalSecOutputValue))

    zIO_sumcheck <- zL221.CalOutputRefining %>%
      rename(supplysector = secondary.output) %>%
      bind_rows(zL2221.StubTechProdCalOutput) %>%
      left_join(zL2221.StubTechProdCalInput, by = c("region","year","supplysector" = "minicam.energy.input")) %>%
      mutate(diff = calOutputValue - CalInputValue)


# Retirement Functions ----------------------------------------------------

    L2221.globaltech_retirement_base <- A221.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector)

    # Copies first future year retirement information into all future years
    L2221.globaltech_retirement_future <- L2221.globaltech_retirement_base %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    # Appends future years onto final base year
    L2221.globaltech_retirement <- L2221.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      bind_rows(L2221.globaltech_retirement_future)

    # Retirement may consist of any of three types of retirement function
    # (phased, s-curve, or none). This section checks for each of these
    # functions and creates a separate level 2 output for each. All of these
    # options have different headers, and all are allowed

    # PHASED RETIREMENT
    if(any(!is.na(L2221.globaltech_retirement$shutdown.rate))) {
      L2221.GlobalTechShutdown <- L2221.globaltech_retirement %>%
        filter(!is.na(L2221.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechShutdown"]])
    }

    # S-CURVE RETIREMENT
    # Subsets the S-Curve retirement function
    if(any(!is.na(L2221.globaltech_retirement$half.life))) {
      L2221.GlobalTechSCurve <- L2221.globaltech_retirement %>%
        filter(!is.na(L2221.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life")
    }

    # PROFIT-BASED SHUTDOWN (includes profit-based idling)
    # Subsets any technologies with a shutdown parameter based on profitability
    if(any(!is.na(L2221.globaltech_retirement$median.shutdown.point))) {
      L2221.GlobalTechProfitShutdown <- L2221.globaltech_retirement %>%
        filter(!is.na(L2221.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness")
    }

    # TOTAL LIFETIME SHUTDOWN
    if(any(is.na(L2221.globaltech_retirement$shutdown.rate) & is.na(L2221.globaltech_retirement$half.life))) {
      L2221.GlobalTechLifetime_en <- L2221.globaltech_retirement %>%
        filter(is.na(L2221.globaltech_retirement$shutdown.rate) & is.na(L2221.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime")
    }

# Constraints -------------------------------------------------------------
    L2221.PortfolioStdConstraint <- L2221.rsrc_info %>%
      filter(resource.type == "policy-portfolio-standard") %>%
      select(-year)%>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # this is the normal profit constraint, i.e.
      # sectoral revenue = sectoral cost for new vintages
      # TODO: set market to global instead of regional?
      mutate(policyType = 'tax', constraint = 1) %>%
      rename(policy.portfolio.standard = resource) %>%
      select(LEVEL2_DATA_NAMES[['PortfolioStdConstraint']]) %>%
      filter(!is.na(region))

    # TODO: this is where the ethanol and biodiesel cost guesses come from
    # incorporate in script somewhere? in gasoline gallon equivalent so conv to GJ
    bio_price_for_conv <- data.frame(year = c(2005, 2010, 2015, 2021),
                                     E85 = c(2.75, 3.37, 2.95, 3.18),
                                     B99 = c(3.3, 3.63, 3.62, 3.47))
    bio_price_usa <- bio_price_for_conv %>%
      mutate(across(c("E85", "B99"), ~ . * 42 / (.95 * 5.052) * CONV_MMBTU_GJ),
             E85conv = E85 * gdp_deflator(1975, base_year = year),
             B99conv = B99 * gdp_deflator(1975, base_year = year))

    # SEDS gasoline: avg USA mogas consumer price
    seds_gas_price <- data.frame(year = c(1975, 1990, 2005, 2010, 2015, 2021),
                                 price = c(4.64, 3.62, 4.68, 5.19, 4.20, 4.71))
    # TODO: GCAM crude price is $2.02 vs $3ish in 2005; $4.67 vs $2.5ish in 2015

    # Regional adjustment based on fitting history to normal profit condition
    adjusted_gas_price <- profit_rate_calcs %>%
      filter(resource == "Gasoline_crude oil") %>%
      select(region, year, price = adj.price) %>%
      distinct() %>%
      # check if adjusted prices exist for every region in every base year
      complete(nesting(region), year = MODEL_BASE_YEARS) %>%
      arrange(region, year, price) %>%
      # missing Ukraine in 2021, fill it in with SEDS
      left_join(seds_gas_price, by = "year") %>%
      mutate(price = round(if_else(is.na(price.x), price.y, price.x), 2)) %>%
      select(region, year, price)

    # Fixed tax constraint for history - regional crude oil gasoline price
    L2221.PortfolioStdFixedTax <- L2221.rsrc_info %>%
      filter(resource.type == "policy-portfolio-standard") %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>% # TODO: why did we expand to all years before
      filter(year %in% MODEL_BASE_YEARS) %>%
      #left_join_error_no_match(seds_gas_price, by = "year") %>%
      left_join_error_no_match(adjusted_gas_price, by = c("region", "year")) %>%
      rename(policy.portfolio.standard = resource) %>%
      select(LEVEL2_DATA_NAMES[['PortfolioStdFixedTax']]) %>%
      distinct()

# Profit Markets ----------------------------------------------------------

    L2221.SectorZeroProfitMarketName <- L2221.GlobalTechShrwt %>%
      filter(str_detect(sector.name,'refining')) %>%
      mutate(zero.profit.market.name = 'Gasoline_crude oil') %>%
      rename(profit.rate.sector = sector.name) %>%
      distinct(profit.rate.sector,zero.profit.market.name) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SectorZeroProfitMarketName"]]),
                           has_traded = FALSE,
                           GCAM_region_names = GCAM_region_names)

    L2221.ProfitRateSector <- L2221.SectorZeroProfitMarketName %>%
      select(LEVEL2_DATA_NAMES[['ProfitRateSector']])

    L2221.ProfitRateSubsector <- L2221.SubsectorLogit_en %>%
      filter(supplysector %in% L2221.ProfitRateSector$profit.rate.sector) %>%
      rename(profit.rate.sector = supplysector,
             profit.rate.subsector = subsector) %>%
      select(LEVEL2_DATA_NAMES[['ProfitRateSubsector']])

# Produce Outputs ---------------------------------------------------------

    L2221.Supplysector_en %>%
      add_title("Supply sector information for refining sector") %>%
      add_units("NA") %>%
      add_comments("For refined liquids commodities, the supply sector information is expanded into all GCAM regions") %>%
      add_legacy_name("L2221.Supplysector_en") %>%
      add_precursors("energy/A221.sector") ->
      L2221.Supplysector_en

    L2221.ProfitRateSector %>%
      add_title("Profit rate sector information for refining sector") %>%
      add_units("NA") %>%
      add_comments("For refining sector, the profit rate sector information is expanded into all GCAM regions") %>%
      add_legacy_name("L2221.ProfitRateSector") %>%
      add_precursors("energy/A221.globaltech_shrwt") ->
      L2221.ProfitRateSector

    L2221.ProfitRateSubsector %>%
      add_title("Profit rate subsector information for refining sector") %>%
      add_units("NA") %>%
      add_comments("For refining sector, the profit rate subsector information is expanded into all GCAM regions") %>%
      add_legacy_name("L2221.ProfitRateSubsector") %>%
      add_precursors("energy/A221.globaltech_shrwt") ->
      L2221.ProfitRateSubsector

    L2221.SubsectorLogit_en %>%
      add_title("Subsector logit exponents of refining sector") %>%
      add_units("Unitless") %>%
      add_comments("For refining sector, the subsector logit exponents from A221.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2221.SubsectorLogit_en") %>%
      add_precursors("energy/A221.subsector_logit", "common/GCAM_region_names") ->
      L2221.SubsectorLogit_en

    if(exists("L2221.SubsectorShrwtFllt_en")) {
      L2221.SubsectorShrwtFllt_en %>%
        add_title("Subsector shareweights of refining sector") %>%
        add_units("unitless") %>%
        add_comments("For refining sector, the subsector shareweights from A221.subsector_shrwt, are expanded into all GCAM regions") %>%
        add_legacy_name("L2221.SubsectorShrwtFllt_en") %>%
        add_precursors("energy/A221.subsector_shrwt", "common/GCAM_region_names") ->
        L2221.SubsectorShrwtFllt_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.SubsectorShrwtFllt_en") ->
        L2221.SubsectorShrwtFllt_en
    }

    if(exists("L2221.SubsectorInterpTo_en")) {
      L2221.SubsectorInterpTo_en %>%
        add_title("Subsector shareweight interpolation of refining sector") %>%
        add_units("NA") %>%
        add_comments("For refining sector, the subsector shareweight interpolation function information from A221.subsector_interp is expanded into all GCAM regions") %>%
        add_legacy_name("L2221.SubsectorInterpTo_en") %>%
        add_precursors("energy/A221.subsector_interp") ->
        L2221.SubsectorInterpTo_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.SubsectorInterpTo_en") ->
        L2221.SubsectorInterpTo_en
    }

    L2221.GlobalTechCoef_en %>%
      add_title("Energy coefficients of refining technologies") %>%
      add_units("NA") %>%
      add_comments("For refining sector, the energy use coefficients from A221.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2221.GlobalTechCoef_en") %>%
      add_precursors("energy/A221.globaltech_coef") ->
      L2221.GlobalTechCoef_en

    L2221.StubTechCoef_refining%>%
      add_title("Energy coefficients of crude refining technologies") %>%
      add_units("NA") %>%
      add_comments("For refining sector, the energy use coefficients from A221.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2221.StubTechCoef_refining") %>%
      add_precursors("energy/A221.globaltech_coef","L1093.IO_R_oilrefining_F_Yh") ->
      L2221.StubTechCoef_refining

    L2221.GlobalTechCost_en %>%
      add_title("Non-energy costs of global refined liquids technologies") %>%
      add_units("1975$/GJ for supplysector refining technologies") %>%
      add_comments("For refining sector, the non-energy costs of global refined liquids manufacturing technologies") %>%
      add_legacy_name("L2221.GlobalTechCost_en") %>%
      add_precursors("L1221.globaltech_capital", "L1221.globaltech_OMvar", "L1221.globaltech_OMfixed", "L1221.globaltech_margin") ->
      L2221.GlobalTechCost_en

    L2221.GlobalTechFractSecOut_en %>%
      add_title("Gasoline yields of different refined liquids manufacturing technologies") %>%
      add_units("%") %>%
      add_comments("For refining sector, the gasoline yields of global refined liquids manufacturing technologies") %>%
      add_legacy_name("L2221.GlobalTechFractSecOut_en") %>%
      add_precursors("energy/A221.globaltech_secout") ->
      L2221.GlobalTechFractSecOut_en

    L2221.GlobalTechResSecOut_en %>%
      add_title("co-product yields of different refined liquids manufacturing technologies") %>%
      add_units("%") %>%
      add_comments("For refining sector, the co-product yields of global refined liquids manufacturing technologies") %>%
      add_legacy_name("L2221.GlobalTechResSecOut_en") %>%
      add_precursors("energy/A221.globaltech_secout") ->
      L2221.GlobalTechResSecOut_en

    if(exists("L2221.GlobalTechSCurve")) {
      L2221.GlobalTechSCurve %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2221.GlobalTechSCurve") %>%
        add_precursors("energy/A221.globaltech_retirement") ->
        L2221.GlobalTechSCurve
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.GlobalTechSCurve") ->
        L2221.GlobalTechSCurve
    }

    if(exists("L2221.GlobalTechProfitShutdown")) {
      L2221.GlobalTechProfitShutdown %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2221.GlobalTechProfitShutdown") %>%
        add_precursors("energy/A221.globaltech_retirement") ->
        L2221.GlobalTechProfitShutdown
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.GlobalTechProfitShutdown") ->
        L2221.GlobalTechProfitShutdown
    }

    if(exists("L2221.GlobalTechShutdown")) {
      L2221.GlobalTechShutdown %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2221.GlobalTechShutdown") %>%
        add_precursors("energy/A221.globaltech_retirement") ->
        L2221.GlobalTechShutdown
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.GlobalTechShutdown") ->
        L2221.GlobalTechShutdown
    }

    if(exists("L2221.GlobalTechLifetime_en")) {
      L2221.GlobalTechLifetime_en %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2221.GlobalTechLifetime_en") %>%
        add_precursors("energy/A221.globaltech_retirement") ->
        L2221.GlobalTechLifetime_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.GlobalTechLifetime_en") ->
        L2221.GlobalTechLifetime_en
    }

    L2221.StubTechProd %>%
      add_title("Calibrated refining sector production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1221.refiningFuelsOutputsEJCombined then given GCAM region, supplysector, subsector, and technology information") %>%
      add_legacy_name("L2221.StubTechProd") %>%
      add_precursors("LB1092.GCAM_REG_LIQUIDS_PROD_agg", "LB1092.GCAM_BIO_LIQUIDS_PROD_agg",
                     "LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg", "energy/calibrated_techs_refining",
                     "energy/refining_mapping") ->
      L2221.StubTechProd

    L2221.GlobalTechShrwt %>%
      add_title("Shareweights of global refined liquids production technologies") %>%
      add_units("Unitless") %>%
      add_comments("For refined liquids sector, the share weights from A221.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2221.GlobalTechShrwt") %>%
      add_precursors("energy/A221.globaltech_shrwt") ->
      L2221.GlobalTechShrwt

    L2221.Rsrc %>%
      add_title("Market information for depletable resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L2221.Rsrc") %>%
      add_precursors("energy/A221.rsrc_info") ->
      L2221.Rsrc

    L2221.RsrcPrice %>%
      add_title("Historical prices for depletable resources") %>%
      add_units("1975$/GJ") %>%
      add_comments("A221.rsrc_info written to all regions") %>%
      add_legacy_name("L2221.RsrcPrice") %>%
      add_precursors("energy/A221.rsrc_info") ->
      L2221.RsrcPrice

    L2221.StubTech_en %>%
      add_title("Stub technology information for refining sector") %>%
      add_units("NA") %>%
      add_comments("For refined liquids commodities, the stub technology information is expanded into all GCAM regions") %>%
      add_legacy_name("L2221.StubTech_en") %>%
      add_precursors("energy/A221.globaltech_shrwt") ->
      L2221.StubTech_en

    # L2221.StubTechShrwt %>%
    #   add_title("Stub technology information for refining sector") %>%
    #   add_units("NA") %>%
    #   add_comments("For refined liquids commodities, the stub technology information is expanded into all GCAM regions") %>%
    #   add_legacy_name("L2221.StubTechShrwt") %>%
    #   add_precursors("energy/refining_mapping","energy/A221.globaltech_shrwt") ->
    #   L2221.StubTechShrwt

    L2221.RsrcCal %>%
      add_title("Subsector information for refining sector") %>%
      add_units("NA") %>%
      add_comments("For refined liquids commodities, the stub technology information is expanded into all GCAM regions") %>%
      add_legacy_name("L2221.RsrcCal") %>%
      add_precursors("energy/refining_mapping","energy/A221.globaltech_shrwt") ->
      L2221.RsrcCal

    L2221.GlobalTechInterp %>%
      add_title("Global technology interpolation information for refining sector") %>%
      add_units("NA") %>%
      add_comments("Interpolation rules for global refined liquids technologies") %>%
      add_legacy_name("L2221.GlobalTechInterp") %>%
      add_precursors("energy/A221.globaltech_interp") ->
      L2221.GlobalTechInterp

    if(exists("L2221.GlobalTechZeroProfitOut_en")) {
      L2221.GlobalTechZeroProfitOut_en %>%
        add_title("Global zero profit technology information for refining sector") %>%
        add_units("NA") %>%
        add_comments("Global zero profit technology information for refining sector") %>%
        add_legacy_name("L2221.GlobalTechZeroProfitOut_en") %>%
        add_precursors("energy/A221.globaltech_secout") ->
        L2221.GlobalTechZeroProfitOut_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.SubsectorInterp_en") ->
        L2221.GlobalTechZeroProfitOut_en
    }

    L2221.StubTechSecondaryOutput %>%
      add_title("Stub technology secondary outputs for the refining sector") %>%
      add_units("NA") %>%
      add_comments("Regional crude oil quality adjustments for technologies in the refining sector") %>%
      add_legacy_name("L2221.StubTech_en") %>%
      add_precursors("energy/A221.stubtech_regional_output") ->
      L2221.StubTechSecondaryOutput

    L2221.StubTechCost %>%
      add_title("Stub technology secondary outputs for refining sector") %>%
      add_units("1975$/GJ") %>%
      add_comments("Regional cost adjustments for technologies in the refining sector") %>%
      add_legacy_name("L2221.StubTechCost") %>%
      add_precursors("energy/calibrated_techs_refining", "energy/A221.rsrc_info",
                     "LB1092.GCAM_REG_LIQUIDS_PROD_agg",
                     "LB1092.GCAM_BIO_LIQUIDS_PROD_agg",
                     "LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg",
                     "L1093.IO_R_oilrefining_F_Yh",
                     "L1221.globaltech_capital",
                     "L1221.globaltech_OMfixed",
                     "L1221.globaltech_OMvar",
                     "L1221.globaltech_margin") ->
    L2221.StubTechCost

    L2221.PortfolioStdConstraint %>%
      add_title("Policy portfolio standard constraint for refining sector") %>%
      add_units("NA") %>%
      add_comments("Policy portfolio standard constraint (zero profit constraint) information for refining sector") %>%
      add_legacy_name("L2221.PortfolioStdConstraint") %>%
      add_precursors("energy/A221.rsrc_info") ->
      L2221.PortfolioStdConstraint

    L2221.PortfolioStdFixedTax %>%
      add_title("Fixed tax information for refining sector") %>%
      add_units("1975$/GJ") %>%
      add_comments("Fixed tax information for refining sector in the historical years") %>%
      add_legacy_name("L2221.PortfolioStdFixedTax") %>%
      add_precursors("energy/A221.rsrc_info") ->
      L2221.PortfolioStdFixedTax

    L2221.SectorZeroProfitMarketName %>%
      add_title("Zero profit sector market name information for refining sector") %>%
      add_units("NA") %>%
      add_comments("Zero profit sector market name information for refining sector") %>%
      add_legacy_name("L2221.SectorZeroProfitMarketName") %>%
      add_precursors("energy/A221.globaltech_shrwt") ->
      L2221.SectorZeroProfitMarketName

    return_data(L2221.Supplysector_en,
                L2221.ProfitRateSector,
                L2221.ProfitRateSubsector,
                #L2221.StubTechShrwt,
                L2221.SubsectorLogit_en,
                L2221.SubsectorShrwtFllt_en,
                L2221.SubsectorInterpTo_en,
                L2221.StubTechCoef_refining,
                L2221.GlobalTechCoef_en,
                L2221.GlobalTechCost_en,
                L2221.GlobalTechFractSecOut_en,
                L2221.GlobalTechResSecOut_en,
                L2221.GlobalTechZeroProfitOut_en,
                L2221.GlobalTechShrwt,
                L2221.GlobalTechInterp,
                L2221.Rsrc,
                L2221.RsrcPrice,
                L2221.PortfolioStdConstraint,
                L2221.PortfolioStdFixedTax,
                L2221.StubTechProd,
                L2221.RsrcCal,
                L2221.GlobalTechSCurve,
                L2221.GlobalTechProfitShutdown,
                L2221.GlobalTechShutdown,
                L2221.GlobalTechLifetime_en,
                L2221.SectorZeroProfitMarketName,
                L2221.StubTechSecondaryOutput,
                L2221.StubTechCost,
                L2221.StubTechTrackCapital_en,
                L2221.StubTech_en)
  } else {
    stop("Unknown command")
  }
}
