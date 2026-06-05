# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_cstorage_variations_xml
#'
#' Construct XML data structures for \code{cstorage_cost_*xml}
#' and \code{cstorage_volume_*xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' #'  @author JF, CB and MG April 2026
module_energy_cstorage_variations_xml <- function(command, ...) {

  # --- combinations ---
  storage_locales <- c("Onshore", "Offshore")
  storage_categories <- c("technical_potential", "prudent_potential", "current_oil_and_gas_potential")
  vol_combos <- expand.grid(locale = storage_locales, category = storage_categories)
  cost_multipliers <- c("high", "default", "low", "lowest")
  cost_combos <- expand.grid(locale = storage_locales, kind = cost_multipliers)

  # --- inputs ---
  vol_df_names  <- paste0("L263.cstorage_volume_", vol_combos$category, "_", vol_combos$locale)
  cost_df_names <- paste0("L263.cstorage_cost_", cost_combos$kind, "_", cost_combos$locale)
  MODULE_INPUTS <- c(vol_df_names, cost_df_names,
                     "L261.Rsrc",
                     "L261.RsrcOffshore",

                     "L261.DeleteUnlimitRsrc",
                     "L261.DeleteRsrc",

                     "L261.ResTechShrwt_C",

                     "L261.ResSubresourceProdLifetime",
                     "L261.ResSubresourceProdLifetimeOffshore",

                     "L261.ResReserveTechLifetime",
                     "L261.ResReserveTechLifetimeOffshore",

                     "L261.ResReserveTechDeclinePhase",
                     "L261.ResReserveTechDeclinePhaseOffshore",

                     "L261.ResReserveTechProfitShutdown",
                     "L261.ResReserveTechProfitShutdownOffshore",

                     "L261.ResReserveTechInvestmentInput",
                     "L261.ResReserveTechInvestmentInputOffshore",

                     "L261.Supplysector_C",
                      FILE = "common/GCAM_region_names")

  # --- outputs ---
  vol_fnames  <- paste0("cstorage_volume_", vol_combos$category, "_", vol_combos$locale, ".xml")
  cost_fnames <- paste0("cstorage_cost_", cost_combos$kind, "_", cost_combos$locale, ".xml")
  MODULE_OUTPUTS <- c(vol_fnames, cost_fnames)

  # --- logic ---
  if(command == driver.DECLARE_INPUTS) {

    return(MODULE_INPUTS)

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cstorage_volume_technical_potential_Onshore.xml",
             XML = "cstorage_volume_technical_potential_Offshore.xml",
             XML = "cstorage_volume_prudent_potential_Onshore.xml",
             XML = "cstorage_volume_prudent_potential_Offshore.xml",
             XML = "cstorage_volume_current_oil_and_gas_potential_Onshore.xml",
             XML = "cstorage_volume_current_oil_and_gas_potential_Offshore.xml",
             XML = "cstorage_cost_high_Onshore.xml",
             XML = "cstorage_cost_high_Offshore.xml",
             XML = "cstorage_cost_default_Onshore.xml",
             XML = "cstorage_cost_default_Offshore.xml",
             XML = "cstorage_cost_low_Onshore.xml",
             XML = "cstorage_cost_low_Offshore.xml",
             XML = "cstorage_cost_lowest_Onshore.xml",
             XML = "cstorage_cost_lowest_Offshore.xml"))

  } else if(command == driver.MAKE) {

    # Silence package checks (predeclare objects)
    cstorage_volume_technical_potential_Onshore.xml <-
      cstorage_volume_technical_potential_Offshore.xml <-
      cstorage_volume_prudent_potential_Onshore.xml <-
      cstorage_volume_prudent_potential_Offshore.xml <-
      cstorage_volume_current_oil_and_gas_potential_Onshore.xml <-
      cstorage_volume_current_oil_and_gas_potential_Offshore.xml <-
      cstorage_cost_high_Onshore.xml <-
      cstorage_cost_high_Offshore.xml <-
      cstorage_cost_default_Onshore.xml <-
      cstorage_cost_default_Offshore.xml <-
      cstorage_cost_low_Onshore.xml <-
      cstorage_cost_low_Offshore.xml <-
      cstorage_cost_lowest_Onshore.xml <-
      cstorage_cost_lowest_Offshore.xml <- NULL

    all_data <- list(...)[[1]]

    L261.Rsrc <- get_data(all_data,"L261.Rsrc")
    L261.RsrcOffshore <- get_data(all_data, "L261.RsrcOffshore")

    L261.ResTechShrwt_C <- get_data(all_data,"L261.ResTechShrwt_C")

    L261.ResSubresourceProdLifetime <- get_data(all_data, "L261.ResSubresourceProdLifetime")
    L261.ResSubresourceProdLifetimeOffshore <- get_data(all_data, "L261.ResSubresourceProdLifetimeOffshore")

    L261.ResReserveTechLifetime <- get_data(all_data, "L261.ResReserveTechLifetime")
    L261.ResReserveTechLifetimeOffshore <- get_data(all_data, "L261.ResReserveTechLifetimeOffshore")

    L261.ResReserveTechDeclinePhase <- get_data(all_data, "L261.ResReserveTechDeclinePhase")
    L261.ResReserveTechDeclinePhaseOffshore <- get_data(all_data, "L261.ResReserveTechDeclinePhaseOffshore")

    L261.ResReserveTechProfitShutdown <- get_data(all_data, "L261.ResReserveTechProfitShutdown")
    L261.ResReserveTechProfitShutdownOffshore <- get_data(all_data, "L261.ResReserveTechProfitShutdownOffshore")

    L261.ResReserveTechInvestmentInput <- get_data(all_data, "L261.ResReserveTechInvestmentInput")
    L261.ResReserveTechInvestmentInputOffshore <- get_data(all_data, "L261.ResReserveTechInvestmentInputOffshore")

    L261.DeleteUnlimitRsrc <- get_data(all_data,"L261.DeleteUnlimitRsrc")
    L261.DeleteRsrc <- get_data(all_data,"L261.DeleteRsrc")

    base.value = 0.001

    L261.Supplysector_C <- get_data(all_data, "L261.Supplysector_C") %>%
      mutate(logit.exponent = logit.exponent * base.value)

    # --- build volume XMLs ---
    for(i in seq_along(vol_fnames)) {
      if(str_detect(vol_fnames[[i]],"Onshore")){

        #Shareweight zero for regions with zero potential and therefore infinite slope

        RsrcCurvesAvail_i <- get_data(all_data, vol_df_names[[i]]) %>%
          group_by(region,resource,subresource) %>%
          mutate(value = sum(available)) %>%
          ungroup() %>%
          mutate(available = if_else(value == 0 & !(grade %in% c("grade a1")), (10 ^ -energy.DIGITS_RESOURCE), available),
                 available = round(available,energy.DIGITS_RESOURCE))

        ResTechShrwt_i <- RsrcCurvesAvail_i %>%
          group_by(region,resource,subresource) %>%
          summarize(value = sum(available)) %>%
          ungroup() %>%
          mutate(share.weight = 1,
                 technology = subresource) %>%
          repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
          select(LEVEL2_DATA_NAMES[["ResTechShrwt"]])

        SubsectorShrwtFllt_i <- RsrcCurvesAvail_i %>%
          group_by(region,resource,subresource) %>%
          summarize(share.weight = sum(available) / 1000) %>%
          ungroup() %>%
          rename(subsector = subresource) %>%
          mutate(supplysector = "carbon-storage",
                 year.fillout = min(MODEL_BASE_YEARS)) %>%
          select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])

        create_xml(vol_fnames[[i]]) %>%
          add_xml_data(L261.DeleteRsrc, "DeleteRsrc") %>%
          add_xml_data(L261.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
          add_xml_data(L261.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
          add_xml_data(L261.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
          add_xml_data(L261.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
          add_xml_data(L261.ResReserveTechInvestmentInput, "ResReserveTechInvestmentInput") %>%
          add_node_equiv_xml("resource") %>%
          add_node_equiv_xml("subresource") %>%
          add_node_equiv_xml("technology") %>%
          add_xml_data(L261.Rsrc, "Rsrc") %>%
          add_xml_data(ResTechShrwt_i, "ResTechShrwt") %>%
          add_xml_data(RsrcCurvesAvail_i, "RsrcCurvesAvail") %>%
          add_logit_tables_xml(L261.Supplysector_C,"Supplysector") %>%
          add_xml_data(SubsectorShrwtFllt_i, "SubsectorShrwtFllt") %>%
          add_precursors(vol_df_names[[i]],
                       "common/GCAM_region_names",
                       "L261.ResSubresourceProdLifetime",
                       "L261.ResReserveTechDeclinePhase",
                       "L261.ResReserveTechProfitShutdown",
                       "L261.ResReserveTechLifetime",
                       "L261.ResReserveTechInvestmentInput",
                       "L261.Rsrc",
                       "L261.ResTechShrwt_C",
                       "L261.Supplysector_C") ->
        x

        assign(vol_fnames[[i]], x)
      }

      else if(str_detect(vol_fnames[[i]],"Offshore")){

        RsrcCurvesAvail_i <- get_data(all_data, vol_df_names[[i]]) %>%
          mutate(available = round(available,energy.DIGITS_RESOURCE)) %>%
          mutate(value = sum(available)) %>%
          ungroup() %>%
          mutate(available = if_else(value == 0 & !(grade %in% c("grade a1")), (10 ^ -energy.DIGITS_RESOURCE), available),
                 available = round(available,energy.DIGITS_RESOURCE))


        ResTechShrwt_i <- RsrcCurvesAvail_i %>%
          group_by(region,resource,subresource) %>%
          summarize(value = sum(available)) %>%
          ungroup() %>%
          mutate(share.weight = 1,
                 technology = subresource) %>%
          repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
          select(LEVEL2_DATA_NAMES[["ResTechShrwt"]])

        SubsectorShrwtFllt_i <- RsrcCurvesAvail_i %>%
          group_by(region,resource,subresource) %>%
          summarize(share.weight = sum(available) / 1000) %>%
          ungroup() %>%
          rename(subsector = subresource) %>%
          mutate(supplysector = "carbon-storage",
                 year.fillout = min(MODEL_BASE_YEARS)) %>%
          select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])


        create_xml(vol_fnames[[i]]) %>%
          add_xml_data(L261.DeleteUnlimitRsrc,"DeleteUnlimitRsrc") %>%
          add_xml_data(L261.ResSubresourceProdLifetimeOffshore, "ResSubresourceProdLifetime") %>%
          add_xml_data(L261.ResReserveTechDeclinePhaseOffshore, "ResReserveTechDeclinePhase") %>%
          add_xml_data(L261.ResReserveTechProfitShutdownOffshore, "ResReserveTechProfitShutdown") %>%
          add_xml_data(L261.ResReserveTechLifetimeOffshore, "ResReserveTechLifetime") %>%
          add_xml_data(L261.ResReserveTechInvestmentInputOffshore, "ResReserveTechInvestmentInput") %>%
          add_node_equiv_xml("resource") %>%
          add_node_equiv_xml("subresource") %>%
          add_node_equiv_xml("technology") %>%
          add_xml_data(L261.RsrcOffshore, "Rsrc") %>%
          add_xml_data(ResTechShrwt_i, "ResTechShrwt") %>%
          add_xml_data(RsrcCurvesAvail_i, "RsrcCurvesAvail") %>%
          add_logit_tables_xml(L261.Supplysector_C,"Supplysector") %>%
          add_xml_data(SubsectorShrwtFllt_i, "SubsectorShrwtFllt") %>%
          add_precursors(vol_df_names[[i]],
                         "common/GCAM_region_names",
                         "L261.ResSubresourceProdLifetimeOffshore",
                         "L261.ResReserveTechDeclinePhaseOffshore",
                         "L261.ResReserveTechProfitShutdownOffshore",
                         "L261.ResReserveTechLifetimeOffshore",
                         "L261.ResReserveTechInvestmentInputOffshore",
                         "L261.DeleteRsrc",
                         "L261.ResTechShrwt_C",
                         "L261.Supplysector_C",
                         "L261.RsrcOffshore",
                         "L261.DeleteUnlimitRsrc") ->
          x

        assign(vol_fnames[[i]], x)
      }


    }

    # --- build cost XMLs ---
    for(i in seq_along(cost_fnames)) {

      RsrcCurvesExtCost_i <- get_data(all_data, cost_df_names[[i]])

      create_xml(cost_fnames[[i]]) %>%
        add_node_equiv_xml("subresource") %>%
        add_node_equiv_xml("technology") %>%
        add_xml_data(RsrcCurvesExtCost_i, "RsrcCurvesExtCost") %>%
        add_precursors(cost_df_names[[i]]) ->
        y
      assign(cost_fnames[[i]], y)

    }

    # return all outputs explicitly
    return_data(
      cstorage_volume_technical_potential_Onshore.xml,
      cstorage_volume_technical_potential_Offshore.xml,
      cstorage_volume_prudent_potential_Onshore.xml,
      cstorage_volume_prudent_potential_Offshore.xml,
      cstorage_volume_current_oil_and_gas_potential_Onshore.xml,
      cstorage_volume_current_oil_and_gas_potential_Offshore.xml,
      cstorage_cost_high_Onshore.xml,
      cstorage_cost_high_Offshore.xml,
      cstorage_cost_default_Onshore.xml,
      cstorage_cost_default_Offshore.xml,
      cstorage_cost_low_Onshore.xml,
      cstorage_cost_low_Offshore.xml,
      cstorage_cost_lowest_Onshore.xml,
      cstorage_cost_lowest_Offshore.xml
    )

  } else {
    stop("Unknown command")
  }
}
