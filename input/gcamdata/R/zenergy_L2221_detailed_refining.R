# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2221.refining
#'
#' Writes all energy supply sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2221.Supplysector_en}, \code{L2221.GlobalTechShutdown}, \code{L2221.ProfitRateSector}, \code{L2221.StubTechCoef_refining}, \code{L2221.ProfitRateSubsector}, \code{L2221.SubsectorLogit_en}, \code{L2221.SubsectorShrwtFllt_en}, \code{L2221.SubsectorInterp_en}, \code{L2221.GlobalTechCoef_en}, \code{L2221.GlobalTechCoef_en}, \code{L2221.GlobalTechCost_en}, \code{L2221.GlobalTechFractSecOut_en}, \code{L2221.GlobalTechResSecOut_en}, \code{L2221.GlobalTechZeroProfitOut_en}, \code{L2221.GlobalTechShrwt}, \code{L2221.GlobalTechInterp}, \code{L2221.Rsrc}, \code{L2221.UnlimitRsrc}, \code{L2221.UnlimitRsrcPrice}, \code{L2221.RsrcPrice}, \code{L2221.PortfolioStdConstraint}, \code{L2221.PortfolioStdFixedTax}, \code{L2221.StubTechCalInput}, \code{L2221.BaseService}, \code{L2221.GlobalTechSCurve}, \code{L2221.GlobalTechProfitShutdown}, \code{L2221.SectorZeroProfitMarketName}, \code{L2221.StubTechSecondaryOutput}, \code{L2221.StubTech_en}, \code{L2221.RenewRsrc}, \code{L2221.RsrcCalProd}, \code{L2221.RsrcCurves_fos}, \code{L2221.SmthRenewRsrcCurves_MSW}, \code{L2221.ResTechShrwt}, \code{L2221.PriceElasticity_fuel}, \code{L2221.IncomeElasticity_fuel}. The corresponding file in the
#' original data system was \code{L2221.refining.R} (energy level2).
#' @details This chunk creates level 2 output files for refined liquids supply. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter full_join if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr gather
#' @author JF July 2024 SD Jan 2025
module_energy_L2221.refining <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
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
             "LB1092.GCAM_REG_LIQUIDS_PROD_agg",
             "LB1092.GCAM_BIO_LIQUIDS_PROD_agg",
             "LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg",
             "L1093.IO_R_oilrefining_F_Yh",
             "L1221.globaltech_capital",
             "L1221.globaltech_OMfixed",
             "L1221.globaltech_OMvar",
             "LB1092.Tradebalance_refined_liquids_EJ_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2221.Supplysector_en",
             "L2221.ProfitRateSector",
             "L2221.ProfitRateSubsector",
             "L2221.SubsectorLogit_en",
                "L2221.SubsectorShrwtFllt_en",
                "L2221.SubsectorInterp_en",
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
                #"L2221.StubTechShrwt",
                "L2221.GlobalTechProfitShutdown",
                 "L2221.GlobalTechShutdown",
                "L2221.SectorZeroProfitMarketName",
                "L2221.StubTechSecondaryOutput",
                "L2221.StubTech_en"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    P1 <- biodiesel <- biomassOil_tech <- calOutputValue <- calPrice <- coef <- coefficient <-
    resource <- ethanol <- feed_price <- fractional.secondary.output <- fuel <-
    input.cost <- market <- minicam.energy.input <- minicam.non.energy.input <-
    object <- output.ratio <- output.unit <- price <- price.unit <- primary.consumption <-
    region <- sector <- sector.name <- share.weight <- stub.technology <- subsector <-
    subsector.name <- subsector.share.weight <- supplysector <- technology <-
    to.value <- tradbio_region <- traded <- unit <- value <- value_fby <- variable <- year <-
    year.fillout <- year.share.weight <- GCAM_commodity <- GCAM_region_ID <-
    GCAM_region_ID.x <- GCAM_region_ID.y <- P0 <- calibrated.value <- tech.share.weight <-
      market.name <- passthru_tech_input <- SecOutRatio <- IOcoef <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A221.sector <- get_data(all_data, "energy/A221.sector", strip_attributes = TRUE)
    A221.subsector_logit <- get_data(all_data, "energy/A221.subsector_logit", strip_attributes = TRUE)
    A221.subsector_shrwt <- get_data(all_data, "energy/A221.subsector_shrwt", strip_attributes = TRUE)
    A221.subsector_interp <- get_data(all_data, "energy/A221.subsector_interp", strip_attributes = TRUE)
    A221.globaltech_coef <- get_data(all_data, "energy/A221.globaltech_coef", strip_attributes = TRUE)
    A221.globaltech_shrwt <- get_data(all_data, "energy/A221.globaltech_shrwt", strip_attributes = TRUE)
    A221.globaltech_interp <- get_data(all_data, "energy/A221.globaltech_interp", strip_attributes = TRUE)
    A221.globaltech_retirement <- get_data(all_data,"energy/A221.globaltech_retirement", strip_attributes = TRUE)
    A221.globaltech_secout <- get_data(all_data, "energy/A221.globaltech_secout", strip_attributes = TRUE)
    A221.rsrc_info <- get_data(all_data, "energy/A221.rsrc_info", strip_attributes = TRUE)
    LB1092.Tradebalance_refined_liquids_EJ_R_Y <- get_data(all_data,"LB1092.Tradebalance_refined_liquids_EJ_R_Y", strip_attributes = TRUE)
    A221.globaltech_capital <- get_data(all_data, "L1221.globaltech_capital", strip_attributes = TRUE)
    A221.globaltech_OMvar <- get_data(all_data, "L1221.globaltech_OMvar", strip_attributes = TRUE)
    A221.globaltech_OMfixed <- get_data(all_data, "L1221.globaltech_OMfixed", strip_attributes = TRUE)
    A221.stubtech_regional_output <- get_data(all_data, "energy/A221.stubtech_regional_output", strip_attributes = TRUE)
    LB1092.GCAM_REG_LIQUIDS_PROD_agg <- get_data(all_data,"LB1092.GCAM_REG_LIQUIDS_PROD_agg", strip_attributes = TRUE)
    LB1092.GCAM_BIO_LIQUIDS_PROD_agg <- get_data(all_data,"LB1092.GCAM_BIO_LIQUIDS_PROD_agg", strip_attributes = TRUE)
    LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg <- get_data(all_data,"LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg", strip_attributes = TRUE)
    L1093.IO_R_oilrefining_F_Yh <- get_data(all_data,"L1093.IO_R_oilrefining_F_Yh", strip_attributes = TRUE)
    calibrated_techs_refining <- get_data(all_data,"energy/calibrated_techs_refining",strip_attributes = TRUE)

    #Process crude-based liquids production
    L1221.refineryFuelsOutputsEJ <- LB1092.GCAM_REG_LIQUIDS_PROD_agg %>%
      mutate(value = replace_na(value, 0), input = "oil", GCAM_mapping = fuel, subsector = fuel)

    #Remap bio fuels to products and expected GCAM naming
    bio_mapping <- data.frame(GCAM_mapping = c("corn ethanol", "sugar cane ethanol", "biodiesel"),
      input = c("corn", "sugar", "biomassOil"))

   #Process bio-based liquids production
    L1221.biofuelOutputsEJ <- LB1092.GCAM_BIO_LIQUIDS_PROD_agg %>%
      filter(year %in% MODEL_BASE_YEARS)%>%
      mutate(sector = "biorefining",
             input = if_else(fuel == "biomass oil", "biomassOil", fuel),
             fuel = if_else(fuel == "biomass oil", "Distillate_FuelOil", "Gasoline")) %>%
      left_join(bio_mapping, by = "input") %>%
      mutate(value = replace_na(value, 0), subsector = fuel) %>%
      left_join(GCAM_region_names,by=c("GCAM_region_ID"))%>%
      select(-fuel,-GCAM_region_ID)

    #Process ctl/gtl-based liquids production
    L1221.ctl_gtl_OutputsEJ <- LB1092.GCAM_CTL_GTL_LIQUIDS_PROD_agg %>%
      filter(year %in% MODEL_BASE_YEARS)%>%
      mutate(value = replace_na(value, 0),
             input = if_else(sector == "ctl", "coal", "natural gas"),
             subsector = fuel) %>%
      rename(GCAM_mapping = fuel)%>%
      left_join(GCAM_region_names,by=c("GCAM_region_ID"))%>%
      select(-GCAM_region_ID)

    #Aggregate liquids production from crude oil, biomass, and ctl/gtl processes
    L1221.refiningFuelsOutputsEJCombined <-
      bind_rows(L1221.refineryFuelsOutputsEJ,L1221.biofuelOutputsEJ,L1221.ctl_gtl_OutputsEJ) %>%
      select(region, year, subsector, output = GCAM_mapping, value, input)

    # A. Output unit, price unit, market
    L2221.rsrc_info <- A221.rsrc_info %>%
      gather_years() %>%
       # Repeat and add region to resource assumptions table
      repeat_add_columns(select(GCAM_region_names, region)) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = if_else(market == "regional", region, market))

    # L2221.Rsrc: output unit, price unit, and market for depletable resources
    L2221.Rsrc <- L2221.rsrc_info %>%
      filter(resource.type == "resource") %>%
      select(region, resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    L2221.PortfolioStdConstraint <- L2221.rsrc_info %>%
      filter(resource.type == "policy-portfolio-standard") %>%
      select(-year)%>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      filter(year %in% c(MODEL_FUTURE_YEARS)) %>%
      mutate(policyType = 'tax',
             constraint = 1) %>%
      rename(policy.portfolio.standard = resource) %>%
      select(LEVEL2_DATA_NAMES[['PortfolioStdConstraint']]) %>%
      filter(!is.na(region))

    L2221.PortfolioStdFixedTax <- L2221.rsrc_info %>%
      filter(resource.type == "policy-portfolio-standard") %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      filter(year %in% c(MODEL_BASE_YEARS)) %>%
      mutate(policyType = 'tax',
             price = 4.2) %>% #TODO: in the future we will need to calibrate this
      rename(policy.portfolio.standard = resource) %>%
      select(LEVEL2_DATA_NAMES[['PortfolioStdFixedTax']])

    # L2221.RsrcPrice: historical prices for depletable resources
    L2221.RsrcPrice <- L2221.rsrc_info %>%
      filter(resource.type == "resource",
             year %in% MODEL_BASE_YEARS) %>%
      select(region, resource = resource, year, price = value)

    L2221.Supplysector_en <- A221.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                           has_traded = TRUE, GCAM_region_names = GCAM_region_names)

    L2221.SubsectorLogit_en <- A221.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                           has_traded = TRUE, GCAM_region_names = GCAM_region_names)

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


    A221.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L2221.GlobalTechShrwt

    A221.globaltech_interp %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      set_years() %>%
      mutate(to.value = 1) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]])-> L2221.GlobalTechInterp

    # L222.StubTechCoef_refining: calibrated input-output coefficients of oil refining by region and input
    # interpolates values of IO coefficients for base years from historical values
    L1093.IO_R_oilrefining_F_Yh %>%
      rename(value=IO_coeff)%>%
      complete(nesting(region, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(region, year) %>%
      group_by(region, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS)->A222.IO_R_oilrefining_F_Yh

    #crude oil refining coefficients
    A221.globaltech_coef %>%
      select(supplysector,subsector,technology,minicam.energy.input,`1971`)%>%
      gather_years%>%
      filter(subsector=="crude oil refining")%>%
      left_join(GCAM_region_names %>%
                  rename(supplysector=GCAM_region_ID)%>%
                  mutate(supplysector="refining"),by=c("supplysector"))%>%
      complete(nesting(region,supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))->L2221.StubTechCoef_refining

    #left_join the coefficients A222.IO_R_oilrefining_F_Yh
    L2221.StubTechCoef_refining %>%
      select(-value)%>%
      left_join(A222.IO_R_oilrefining_F_Yh%>%
                  select(-sector)%>%
                  mutate(supplysector="refining")%>%
                  mutate(fuel=ifelse(fuel=="oil","regional oil",fuel))%>%
                  mutate(fuel=ifelse(fuel=="gas","wholesale gas",fuel))%>%
                  mutate(fuel=ifelse(fuel=="electricity","elect_td_ind",fuel))%>%
                  rename(minicam.energy.input=fuel),
                by=c("region","supplysector","minicam.energy.input","year"))%>%
      arrange(region,supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(region,supplysector, subsector, technology, minicam.energy.input)%>%
      mutate(coefficient = approx_fun(year, value, rule = 2),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(stub.technology = technology)%>%
      mutate(market.name=region)%>%
      #mutate(minicam.energy.input=ifelse(minicam.energy.input=="crude oil","regional oil",minicam.energy.input))%>%
      mutate(minicam.energy.input=ifelse(minicam.energy.input=="natural gas","wholesale gas",minicam.energy.input))%>%
      mutate(minicam.energy.input=ifelse(minicam.energy.input=="electricity","elect_td_ind",minicam.energy.input))-> L2221.StubTechCoef_refining

    # reorders columns to match expected model interface input
    L2221.StubTechCoef_refining <- L2221.StubTechCoef_refining[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input", "coefficient", "market.name")]

    A221.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) -> L2221.GlobalTechCoef_en


    A221.globaltech_cost <-
      bind_rows(A221.globaltech_capital,
                A221.globaltech_OMvar,
                A221.globaltech_OMfixed) %>%
      group_by(supplysector, subsector, technology, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(minicam.non.energy.input = "non-energy-cost")

    L2221.GlobalTechCost_en <- A221.globaltech_cost %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    #biomass technology combinations
    biomass_combinations <- data.frame(
      subsector = c("Residual_FuelOil","Gasoline",
                    "LPG","Heavy_Residual","Jet_Kerosene","Other"),
      output = c("Residual_FuelOil","Gasoline",
                 "LPG","Heavy_Residual","Jet_Kerosene","Other"),
      input = "biomass")

    #Complete region, year, subsector, output, input combinations
    combinations_all <- L1221.refiningFuelsOutputsEJCombined %>%
      distinct(subsector,output,input)%>%
      rbind(biomass_combinations)%>%
      tidyr::crossing(expand.grid(
        region = unique(L1221.refiningFuelsOutputsEJCombined$region),
        year = unique(L1221.refiningFuelsOutputsEJCombined$year)))

    # # left_join to get technology and regions in history that are both in historical data
    # # and also not present in the historical data. These are calibrated to zero output explicitly to
    # # ensure we can solve the historical periods
    L1221.refiningFuelsOutputsEJCombined_all <- combinations_all %>%
      left_join(L1221.refiningFuelsOutputsEJCombined,by=c("subsector","output","input","region","year"))%>%
      mutate(value=ifelse(is.na(value),0,value))


    # Calibrated historical production
    L2221.StubTechProd <- L1221.refiningFuelsOutputsEJCombined_all %>%
      rename(sector = subsector) %>%
      left_join_error_no_match(calibrated_techs_refining,
                               by = c("input" = "fuel", "sector")) %>%
      select(-c(sector, calibration, output)) %>%
      rename(stub.technology = technology) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = share.weight) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]], "share.weight")

    # Secondary output ratios
    SecondaryOutputs  <- A221.globaltech_secout %>%
      gather_years() %>%
      complete(nesting(sec.output.type,supplysector, subsector, technology, fractional.secondary.output),
               year = sort(unique(c(year, MODEL_YEARS)))) %>%
      group_by(sec.output.type,supplysector, subsector, technology, fractional.secondary.output) %>%
      mutate(output.ratio = round(approx_fun(year, value, rule = 2), energy.DIGITS_COEFFICIENT)) %>%
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
      tidyr::gather(key = "secondary.output", value = output.ratio, -region) %>%
      mutate(supplysector = "refining",
             subsector = "crude oil refining",
             stub.technology = 'high Residual_FuelOil',
             secondary.output = paste0(secondary.output,"_crude oil"),
             year = 1975) %>%
      arrange(region) %>%
      complete(nesting(region,
                       supplysector,
                       subsector,
                       stub.technology,
                       secondary.output),
               year = sort(unique(c(year, MODEL_YEARS)))) %>%
      ungroup() %>%
      filter(!is.na(output.ratio)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechSecOut"]])

    # develop a table of supply side technologies to zero out in the historical
    # years to avoid having to come up with a solution, particularly when
    # multiple options need to be zero simultaneously
    # limiting to just biomass for now which is going to be one tech per output product
    # L2221.StubTechShrwt <- L2221.StubTechCalInput %>%
    #   filter(calibrated.value == 0,
    #          stub.technology %in% c("biomass", "coal", "natural gas"),
    #          minicam.energy.input != "oil refining") %>%
    #   select(region, minicam.energy.input, year) %>%
    #   rename(res.secondary.output = minicam.energy.input) %>%
    #   left_join(L2221.GlobalTechResSecOut_en, by = c("res.secondary.output", "year")) %>%
    #   mutate(share.weight = 0) %>%
    #   rename(supplysector = sector.name, subsector = subsector.name,
    #          stub.technology = technology) %>%
    #   select(LEVEL2_DATA_NAMES[['StubTechShrwt']])

    L2221.globaltech_retirement_base <- A221.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector)

    # Copies base year retirement information into all future years and appends back onto itself
    L2221.globaltech_retirement <- L2221.globaltech_retirement_base %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    # S-CURVE RETIREMENT
    # Subsets the S-Curve retirement function
    L2221.GlobalTechSCurve <- L2221.globaltech_retirement %>%
      filter(!is.na(L2221.globaltech_retirement$half.life),
             !year %in% MODEL_BASE_YEARS) %>%
      bind_rows(filter(L2221.globaltech_retirement_base,
                       year == max(MODEL_BASE_YEARS))) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life")

    # PROFIT-BASED SHUTDOWN PARAMETERS
    # Subsets any technologies with a shutdown parameter based on profitability
    L2221.GlobalTechProfitShutdown <- L2221.globaltech_retirement %>%
      filter(!is.na(L2221.globaltech_retirement$median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness")

    L2221.GlobalTechShutdown <- L2221.globaltech_retirement %>%
      filter(year %in% MODEL_BASE_YEARS,
             year != MODEL_FINAL_BASE_YEAR) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShutdown"]])

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

    #======================================================
    # Produce outputs
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

    if(exists("L2221.SubsectorInterp_en")) {
      L2221.SubsectorInterp_en %>%
        add_title("Subsector shareweight interpolation of refining sector") %>%
        add_units("NA") %>%
        add_comments("For refining sector, the subsector shareweight interpolation function infromation from A323.subsector_interp is expanded into all GCAM regions") %>%
        add_legacy_name("L2221.SubsectorInterp_en") %>%
        add_precursors("energy/A221.subsector_interp") ->
        L2221.SubsectorInterp_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.SubsectorInterp_en") ->
        L2221.SubsectorInterp_en
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
      add_precursors("energy/A221.globaltech_cost") ->
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

    L2221.GlobalTechProfitShutdown %>%
      add_title("Global tech profit shutdown decider and parameters") %>%
      add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
      add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
      add_legacy_name("L2221.GlobalTechProfitShutdown") %>%
      add_precursors("energy/A221.globaltech_retirement") ->
      L2221.GlobalTechProfitShutdown

    L2221.StubTechProd %>%
      add_title("Calibrated refining sector production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1221.refiningFuelsOutputsEJCombined then given GCAM region, supplysector, subsector, and technology information") %>%
      add_legacy_name("L2221.StubTechProd") %>%
      add_precursors("L1221.refiningFuelsOutputsEJCombined","energy/calibrated_techs_refining") ->
      L2221.StubTechProd

    L2221.GlobalTechShrwt %>%
      add_title("Shareweights of global refined liquids production technologies") %>%
      add_units("Unitless") %>%
      add_comments("For refined liquids sector, the share weights from A221.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2221.GlobalTechShrwt") %>%
      add_precursors("energy/A221.globaltech_shrwt") ->
      L2221.GlobalTechShrwt

    L2221.GlobalTechSCurve %>%
      add_title("Global tech lifetime for techs with s-curve retirement function") %>%
      add_units("Lifetime in years, half-life in years") %>%
      add_comments("Filters for any technology that uses an S-curve retirement function") %>%
      add_legacy_name("L2221.GlobalTechSCurve") %>%
      add_precursors("energy/A221.globaltech_retirement") ->
      L2221.GlobalTechSCurve

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
      add_title("Stub technology secondary outputs for refining sector") %>%
      add_units("NA") %>%
      add_comments("Regional crude oil quality adjustments for technologies in the refining sector") %>%
      add_legacy_name("L2221.StubTech_en") %>%
      add_precursors("energy/A221.stubtech_regional_output") ->
      L2221.StubTechSecondaryOutput

    L2221.PortfolioStdConstraint %>%
      add_title("Policy portfolio standard constraint for refining sector") %>%
      add_units("NA") %>%
      add_comments("Policy portfolio standard constraint (zero profit constraint) information for refining sector") %>%
      add_legacy_name("L2221.PortfolioStdConstraint") %>%
      add_precursors("energy/A221.rsrc_info") ->
      L2221.PortfolioStdConstraint

    L2221.PortfolioStdFixedTax %>%
      add_title("Fixed tax information for refining sector") %>%
      add_units("NA") %>%
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
                L2221.SubsectorInterp_en,
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
                #L2221.StubTechCalInput,
                L2221.GlobalTechSCurve,
                L2221.GlobalTechProfitShutdown,
                L2221.GlobalTechShutdown,
                L2221.SectorZeroProfitMarketName,
                L2221.StubTechSecondaryOutput,
                L2221.StubTech_en)
  } else {
    stop("Unknown command")
  }
}
