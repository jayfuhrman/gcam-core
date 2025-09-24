# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2327.paper
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for paper-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2327.SectorLogitTables[[ curr_table ]]$data}, \code{L2327.Supplysector_paper}, \code{L2327.FinalEnergyKeyword_paper},
#' \code{L2327.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2327.SubsectorLogit_paper}, \code{L2327.SubsectorShrwtFllt_paper},
#' \code{L2327.SubsectorInterp_paper}, \code{L2327.StubTech_paper}, \code{L2327.GlobalTechShrwt_paper}, \code{L2327.GlobalTechCoef_paper},
#' \code{L2327.GlobalTechCost_paper}, \code{L2327.GlobalTechCapture_paper}, \code{L2327.StubTechProd_paper}, \code{L2327.StubTechCalInput_paper_heat},
#' \code{L2327.StubTechCoef_paper}, \code{L2327.PerCapitaBased_paper}, \code{L2327.BaseService_paper}, \code{L2327.PriceElasticity_paper},
#' \code{L2327.GlobalTechSecOut_paper},
#' \code{object}. The corresponding file in the
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for paper sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author JGF July 2024

module_energy_L2327.paper.cwf <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "cwf/A327.globaltech_coef_cwf_adj",
             "L2327.GlobalTechCoef_paper",
             "L2327.StubTechCoef_paper"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2327.GlobalTechCoef_paper_cwf",
             "L2327.StubTechCoef_paper_cwf"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A327.globaltech_coef_cwf_adj <- get_data(all_data, "cwf/A327.globaltech_coef_cwf_adj", strip_attributes = TRUE)
    L2327.GlobalTechCoef_paper <- get_data(all_data, "L2327.GlobalTechCoef_paper", strip_attributes = TRUE)
    L2327.StubTechCoef_paper <- get_data(all_data,"L2327.StubTechCoef_paper",strip_attributes = TRUE)

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <- output.ratio <-
      shutdown.rate <- half.life <- median.shutdown.point <- share.weight.year <- tech.share.weight <-
      value.x <- value.y <- parameter <- secondary.output <- elec_ratio <- year.x <- year.y <- output.ratio.x <-
      output.ratio.y <- sector.name <- subsector.name <- stub.technology <- market.name <- terminal_coef <- NULL

    # ===================================================

    A327.globaltech_coef_cwf_adj %>%
      gather_years(value_col = "coefficient_adj") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, scenario), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, scenario, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, scenario) %>%
      mutate(coefficient_adj = approx_fun(year, coefficient_adj, rule = 2)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2327.globaltech_coef_cwf_adj # intermediate tibble

    # apply to the original global tech coefficients
    L2327.GlobalTechCoef_paper %>%
      left_join(L2327.globaltech_coef_cwf_adj) %>%
      mutate(coefficient = round(coefficient * coefficient_adj, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]], secondary.output, scenario) ->
      L2327.GlobalTechCoef_paper_cwf


    L2327.StubTechCoef_paper %>%
      # apply CWF adjustments
      left_join(L2327.globaltech_coef_cwf_adj %>%
                  rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
                  dplyr::select(-secondary.output),
                by = c("supplysector","subsector","stub.technology","minicam.energy.input","year")) %>%
      mutate(coefficient = round(coefficient * coefficient_adj, energy.DIGITS_COEFFICIENT)) %>%
      filter(year %in% MODEL_YEARS) %>% # drop the terminal coef year if it's outside of the model years
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]], scenario) ->
      L2327.StubTechCoef_paper_cwf


    # =======================================================
    # Produce outputs

    L2327.GlobalTechCoef_paper_cwf %>%
      add_title("Energy inputs and coefficients of paper technologies") %>%
      add_units("Unitless") %>%
      add_comments("For paper sector, the energy use coefficients from A327.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2327.GlobalTechCoef_paper_cwf") %>%
      add_precursors("cwf/A327.globaltech_coef_cwf", "L2327.GlobalTechCoef_paper") ->
      L2327.GlobalTechCoef_paper_cwf


    L2327.StubTechCoef_paper_cwf %>%
      add_title("region-specific coefficients of aluminum production technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients from literature wirh CWF adjustments") %>%
      add_legacy_name("L2327.StubTechCoef_paper_cwf") %>%
      add_precursors("cwf/A327.globaltech_coef_cwf", "L2327.StubTechCoef_paper") ->
      L2327.StubTechCoef_paper_cwf

    return_data(L2327.GlobalTechCoef_paper_cwf, L2327.StubTechCoef_paper_cwf)

  } else {
    stop("Unknown command")
  }
}
