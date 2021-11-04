#' module_energy_L227.CCSretro_USA
#'
#' Construct XML data structure for \code{eor.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{eor.xml}.
#' #' @importFrom tidyr expand
module_energy_L227.CCSretro_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A232.sector",
             FILE = "energy/A232.subsector_logit",
             FILE = "energy/A232.subsector_shrwt",
             FILE = "energy/A232.globaltech_shrwt",
             FILE = "energy/A232.globaltech_capital",
             FILE = "energy/A232.globaltech_OMfixed",
             FILE = "energy/A232.tech_associations",
             FILE = "energy/NEMS_CCUS_Historical_Retrofit",
             "L202.CarbonCoef",
             "L223.GlobalTechCapFac_elec",
             "L223.GlobalTechCapital_elec",
             "L223.GlobalTechCapture_elec",
             "L223.GlobalTechEff_elec",
             "L223.GlobalTechLifetime_elec",
             "L223.GlobalTechProfitShutdown_elec",
             "L223.GlobalTechOMfixed_elec",
             "L223.GlobalTechOMvar_elec",
             "L223.GlobalTechSCurve_elec",
             "L223.StubTechEff_elec", #Multiply by this efficiency
             "L223.StubTechCalInput_elec",
             "L2241.StubTechCalInput_elec_coalret_USAagg" #added: cal inputs from coal retire chunk
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "CCS_retrofit.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A232.sector <- get_data(all_data, "energy/A232.sector")
    A232.subsector_logit <- get_data(all_data, "energy/A232.subsector_logit")
    A232.subsector_shrwt <- get_data(all_data, "energy/A232.subsector_shrwt")
    A232.globaltech_shrwt <- get_data(all_data, "energy/A232.globaltech_shrwt")
    A232.globaltech_capital <- get_data(all_data, "energy/A232.globaltech_capital")
    A232.globaltech_OMfixed <- get_data(all_data, "energy/A232.globaltech_OMfixed")
    A232.tech_associations <- get_data(all_data, "energy/A232.tech_associations")
    NEMS_CCUS_Historical_Retrofit <- get_data(all_data, "energy/NEMS_CCUS_Historical_Retrofit")
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")
    L223.GlobalTechCapFac_elec <- get_data(all_data, "L223.GlobalTechCapFac_elec")
    L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec")
    L223.GlobalTechCapture_elec <- get_data(all_data, "L223.GlobalTechCapture_elec")
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec")
    L223.GlobalTechLifetime_elec <- get_data(all_data, "L223.GlobalTechLifetime_elec")
    L223.GlobalTechProfitShutdown_elec <- get_data(all_data, "L223.GlobalTechProfitShutdown_elec")
    L223.GlobalTechOMfixed_elec <- get_data(all_data, "L223.GlobalTechOMfixed_elec")
    L223.GlobalTechOMvar_elec <- get_data(all_data, "L223.GlobalTechOMvar_elec")
    L223.GlobalTechSCurve_elec <- get_data(all_data, "L223.GlobalTechSCurve_elec")
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec")
    #L223.StubTechCalInput_elec <- get_data(all_data, "L223.StubTechCalInput_elec")
    L2241.StubTechCalInput_elec_coalret_USAagg <- get_data(all_data, "L2241.StubTechCalInput_elec_coalret_USAagg") %>%
      filter(stub.technology == "coal (conv pul)")

    #establish necessary constants/values
    elec_coal_tech <- "coal (conv pul)"
    sup_sub_tech <- c("supplysector", "subsector", "technology")
    conv_2012_1975_USD <- 0.2982
    CCS_retro_year <- 2015
    elec_CCSretro_coal_tech <- "CCS retrofit"

    # ===================================================

    # 2. Build tables for CSVs
    # 2a. Supplysector information
    A232.sector %>%
      mutate(region = "USA") %>%
      mutate(logit.type = NA) %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1]) ->
      L227.Supplysector_CCSretro

    # 2b. Subsector information
    A232.subsector_logit %>%
      mutate(region = "USA") %>%
      mutate(logit.type = NA) %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1]) ->
      L227.SubsectorLogit_CCSretro

    A232.subsector_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      filter(region == "USA") ->
      L227.SubsectorShrwtFllt_CCSretro

    # 2c. Technology information
    # Shareweights of global technologies
    A232.globaltech_shrwt %>%
      gather("year", "share.weight", matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      filter(year %in% c(MODEL_YEARS)) %>%
      rename(sector.name = "supplysector", subsector.name = "subsector")->
      L227.GlobalTechShrwt_CCSretro

    # Capture information for CCS retrofit technology
    filter( L223.GlobalTechCapture_elec, subsector.name == "coal") %>%
      mutate(sector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$supplysector)%>%
      mutate(subsector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$subsector)%>%
      mutate(technology = filter(A232.globaltech_shrwt, grepl("add",technology))$technology)->
      L227.GlobalTechCapture_CCSretro

    # Efficiency and energy input of CCS retrofit technology in the electricity sector
    filter(L223.GlobalTechEff_elec, technology == elec_coal_tech & year == CCS_retro_year) %>%
      mutate(technology = elec_CCSretro_coal_tech) %>%
      mutate(minicam.energy.input = "coal CCS retrofit") %>%
      mutate(efficiency = 1) ->
      L227.GlobalTechEff_elec_CCSretro

    #Lifetime and profit shutdown decider for CCS retrofit technology
    filter(L223.GlobalTechLifetime_elec, technology == elec_coal_tech) %>%
      mutate(sector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$supplysector)%>%
      mutate(subsector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$subsector)%>%
      mutate(technology = filter(A232.globaltech_shrwt, grepl("add",technology))$technology) ->
      L227.GlobalTechLifetime_CCSretro

    filter(L223.GlobalTechProfitShutdown_elec, technology == elec_coal_tech) %>%
      mutate(sector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$supplysector)%>%
      mutate(subsector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$subsector)%>%
      mutate(technology = filter(A232.globaltech_shrwt, grepl("add",technology))$technology) ->
      L227.GlobalTechProfitShutdown_CCSretro

    #Incremental fixed operating cost for CCS retrofit technology
    A232.globaltech_OMfixed %>%
      gather("year", "OM.fixed", matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(supplysector, subsector, technology, input.OM.fixed, capacity.factor), year = c(year, HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, input.OM.fixed, capacity.factor, year) %>%
      group_by(supplysector, subsector, technology, input.OM.fixed, capacity.factor) %>%
      mutate(OM.fixed = approx_fun(year, OM.fixed)) %>%
      filter(year %in% c(MODEL_YEARS)) %>%
      mutate(OM.fixed = NEMS_CCUS_Historical_Retrofit$`CCS Retrofit Fixed O&M Cost`) %>%
      rename(sector.name = "supplysector", subsector.name = "subsector")->
      L227.GlobalTechOMfixed_CCSretro

    A232.globaltech_OMfixed %>%
      gather("year", "OM.fixed", matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(supplysector, subsector, technology, input.OM.fixed, capacity.factor), year = c(year, HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, input.OM.fixed, capacity.factor, year) %>%
      mutate(OM.fixed = NEMS_CCUS_Historical_Retrofit$`CCS Retrofit Variable O&M Cost`) %>%
      mutate(input.OM.fixed = "OM-variable") %>%
      group_by(supplysector, subsector, technology, input.OM.fixed, capacity.factor) %>%
      filter(year %in% c(MODEL_YEARS)) %>%
      rename(input.OM.var = input.OM.fixed, OM.var = OM.fixed, sector.name = supplysector, subsector.name = subsector)->
      L227.GlobalTechOMvar_CCSretro

    filter(L223.GlobalTechCapFac_elec, technology == elec_coal_tech) %>%
      mutate(sector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$supplysector)%>%
      mutate(subsector.name = filter(A232.globaltech_shrwt, grepl("add",technology))$subsector)%>%
      mutate(technology = filter(A232.globaltech_shrwt, grepl("add",technology))$technology) ->
      L227.GlobalTechCapFac_CCSretro

    #Efficiencies of stub technologies
    filter(L223.StubTechEff_elec, region == "USA" &
          supplysector == A232.tech_associations$elec_supplysector &
          subsector ==  A232.tech_associations$elec_subsector &
          stub.technology == A232.tech_associations$elec_technology) %>%
      left_join(A232.tech_associations, .,
              by = c("elec_supplysector" = "supplysector",
                     "elec_subsector" = "subsector",
                     "elec_technology"= "stub.technology")) %>%
      select(-elec_supplysector, -elec_subsector, -elec_technology) ->
      L227.StubTechEff_CCSretro

    # Because the pass through techs are not assigned a lifetime, the efficiencies from the final base year are
    # copied forward to all future years
    filter(L227.StubTechEff_CCSretro, year == CCS_retro_year ) %>%
      repeat_add_columns(tibble::tibble("year" = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      bind_rows( L227.StubTechEff_CCSretro, . ) %>%
      select(-year,-year.y) %>%
      bind_cols(year = c(MODEL_YEARS))%>%
      mutate(subsector = "coal CCS retrofit") %>%
      mutate(technology = "add CCS retrofit") %>%
      mutate(efficiency = NEMS_CCUS_Historical_Retrofit$retrofit_efficiency) %>%
      # bind_rows(L227.StubTechEff_CCSretro) %>%
      rename(stub.technology = "technology")->
      L227.StubTechEff_CCSretro

    # #The efficiency of the CCS retrofit technology depends on the CO2 capture rate which first requires CO2 emission rate
    # #Get the coal CO2 coefficient in USA in the base years from the energy data table
    # L202.CarbonCoef %>%
    #   filter(region == "USA" & PrimaryFuelCO2Coef.name == "coal") ->
    #   coal_CO2_coef
    # "MTC/EJ" ->
    #   coal_CO2_coef$Units
    #
    # #Coal electricity CO2 capture rate is emission rate times capture fraction divided by efficiency of coal plant
    # L227.GlobalTechCapture_CCSretro ->
    #   coal_elec_CO2_capture
    # coal_elec_CO2_capture$remove.fraction * coal_CO2_coef$PrimaryFuelCO2Coef ->
    #   coal_elec_CO2_capture$remove.rate
    #
    # coal_elec_CO2_capture %>%
    #   filter(year == 2015) ->
    #   capture_rows_Yb
    # capture_rows_Yb$year <- 1975
    # coal_elec_CO2_capture <- bind_rows(coal_elec_CO2_capture,capture_rows_Yb)
    # capture_rows_Yb$year <- 1990
    # coal_elec_CO2_capture <- bind_rows(coal_elec_CO2_capture,capture_rows_Yb)
    # capture_rows_Yb$year <- 2005
    # coal_elec_CO2_capture <- bind_rows(coal_elec_CO2_capture,capture_rows_Yb)
    # capture_rows_Yb$year <- 2015
    # coal_elec_CO2_capture <- bind_rows(coal_elec_CO2_capture,capture_rows_Yb)
    #
    # # for(i in MODEL_BASE_YEARS){
    # #   capture_rows_Yb$year <- i
    # #   coal_elec_CO2_capture <- bind_rows(coal_elec_CO2_capture,capture_rows_Yb)
    # # }
    # L223.StubTechEff_elec %>%
    #   filter(region == "USA" & stub.technology == elec_coal_tech) ->
    #   USA_coal_eff
    #
    # USA_coal_eff %>%
    #   filter(year == 2015) %>%
    #   repeat_add_columns(tibble::tibble(year = MODEL_FUTURE_YEARS)) %>%
    #   select(-year.x) %>%
    #   rename("year" = "year.y") ->
    #   USA_coal_eff_fy
    #
    #
    # bind_rows(USA_coal_eff, USA_coal_eff_fy) ->
    #   USA_coal_eff
    # coal_elec_CO2_capture$remove.rate / left_join(USA_coal_eff,coal_elec_CO2_capture, by = "year")$efficiency ->
    #   coal_elec_CO2_capture$remove.rate
    #
    # # #Same data for all provinces
    # coal_elec_CO2_capture %>%
    #   write_to_all_regions(c("region",colnames(coal_elec_CO2_capture)), GCAM_region_names) %>%
    #   filter(region == "USA") ->
    #   coal_elec_CO2_capt_R
    #
    #
    # # #Coal CCS retrofit energy penalty is remove rate times the energy penalty with proper unit conversions
    # #   CCS_energy_penalty <- L227.StubTechEff_CCSretro
    # #   left_join(CCS_energy_penalty, select(coal_elec_CO2_capt_R, "region", "year", "remove.rate"), by = c("region", "year")) ->
    # #     CCS_energy_penalty
    # #
    # #
    # #
    # # #Pound to tonne conversion
    # # CCS_energy_penalty$remove.rate * NETL_CCS_retrofit_data$energy.penalty *
    # #   conv_kwh_GJ * 2.204623 * conv_C_CO2 ->
    # #   CCS_energy_penalty$energy.penalty
    # #
    # # CCS_energy_penalty$efficiency * (1 - CCS_energy_penalty$energy.penalty) ->
    # #   CCS_energy_penalty$efficiency
    # #
    # # CCS_energy_penalty[ names_StubTechEff ] ->
    # #   CCS_energy_penalty
    # #
    # # "add CCS retrofit" ->
    # #    CCS_energy_penalty$stub.technology
    #
    #
    # #Bind the CCS retrofit efficiency table onto the existing efficiency table
    # bind_rows(L227.StubTechEff_CCSretro, CCS_energy_penalty) ->
    #   L227.StubTechEff_CCSretro

    #Now adjust the efficiency of the existing coal technology to 1 since the energy transformation is happening elsewhere
    # TODO: already set in global tech, do we still need this?
    L223.StubTechEff_elec %>%
      filter(stub.technology == elec_coal_tech & year == CCS_retro_year & region == "USA") %>%
      mutate(minicam.energy.input = "coal CCS retrofit")%>%
      mutate(stub.technology = "CCS retrofit")%>%
      mutate(efficiency = 1) ->
    L227.StubTechEff_coal_elec

    #Other global technology and stub technology information
    #Costs of global technologies
    A232.globaltech_capital %>%
      gather("year", "capital.overnight", matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(supplysector, subsector, technology, `input-capital`, fixed.charge.rate, capacity.factor), year = c(year, HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, `input-capital`, fixed.charge.rate, capacity.factor, year) %>%
      group_by(supplysector, subsector, technology, `input-capital`, fixed.charge.rate, capacity.factor) %>%
      mutate(capital.overnight = approx_fun(year, capital.overnight)) %>%
      filter(year %in% c(MODEL_YEARS)) %>%
      mutate(capital.overnight = if_else(technology == "add CCS retrofit", NEMS_CCUS_Historical_Retrofit$`CCS Retrofit Overnight Cost`, capital.overnight)) %>%
      rename( "sector.name" = "supplysector", "subsector.name" = "subsector" , "input.capital" = "input-capital") ->
      L227.GlobalTechCapital_CCSretro

    # L227.GlobalTechCapital_CCSretro <- L227.globaltech_capital.melt[ names_GlobalTechCapital ]

    # #Read in capital cost of CCS retrofits with a lengthy unit conversion
    # right_join(filter(L227.GlobalTechCapital_CCSretro, technology == "add CCS retrofit"),
    #            coal_elec_CO2_capture, by = "year")$remove.rate *
    #   NETL_CCS_retrofit_data$cost * conv_2012_1975_USD * 24 * 10^6 * conv_C_CO2 * conv_kwh_GJ * 10^-9 ->
    # L227.GlobalTechCapital_CCSretro[L227.GlobalTechCapital_CCSretro$technology == "add CCS retrofit",
    #                                 "capital.overnight"]

    CCS_retrofit.xml <- create_xml("CCS_retrofit.xml")


    L227.GlobalTech.list <- list(  L223.GlobalTechCapFac_elec = L223.GlobalTechCapFac_elec,
                                   L223.GlobalTechCapital_elec = L223.GlobalTechCapital_elec,
                                   L223.GlobalTechLifetime_elec = L223.GlobalTechLifetime_elec,
                                   L223.GlobalTechProfitShutdown_elec = L223.GlobalTechProfitShutdown_elec,
                                   L223.GlobalTechOMfixed_elec = L223.GlobalTechOMfixed_elec,
                                   L223.GlobalTechOMvar_elec = L223.GlobalTechOMvar_elec )


    for( i in 1:length( L227.GlobalTech.list ) ){
      objectname <- sub( "L223.", "L227.", names( L227.GlobalTech.list[i] ) )
      objectname <- sub( "_elec", "_elec_CCSretro", objectname )
      tech_name <- names( L227.GlobalTech.list[[i]] )[ grepl( "technology", names( L227.GlobalTech.list[[i]] ) ) ]
      object <- L227.GlobalTech.list[[i]][
        L227.GlobalTech.list[[i]][[tech_name]] == elec_coal_tech &
          L227.GlobalTech.list[[i]][["year"]] == CCS_retro_year, ]
      if( nrow( object ) != 0 ){
        object[[tech_name]] <- elec_CCSretro_coal_tech
        #assign( objectname, object )
        IDstring <- substr( objectname, 6, regexpr( "_elec_CCSretro", objectname, fixed = T ) - 1 )
        add_xml_data(CCS_retrofit.xml, object, IDstring) ->
          CCS_retrofit.xml
        #print(object)
      }
    }

    #Establish a lifetime for CCS Retrofit
    L223.GlobalTechSCurve_elec %>%
      select(!!!LEVEL2_DATA_NAMES[["GlobalTechLifetime"]]) %>%
      filter(year == CCS_retro_year & technology == elec_coal_tech) %>%
      mutate(technology = elec_CCSretro_coal_tech) %>%
      add_xml_data(CCS_retrofit.xml, . , "GlobalTechLifetime") ->
      CCS_retrofit.xml

    #Redirect the electricity being produced by coal in CHINA to be a cal input to the CCS retrofit technology
    L2241.StubTechCalInput_elec_coalret_USAagg %>%
      left_join(L223.StubTechEff_elec, by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(calOutputValue = calibrated.value * efficiency) ->
      L223.StubTechProd_elec

    # Added 8/31
    # ALL coal that's not retired in 2020 is retrofitable
    L223.StubTechProd_elec %>%
      mutate(tech.share.weight = subs.share.weight) %>%
      filter(year == CCS_retro_year & stub.technology == elec_coal_tech & region == "USA") -> retrofitable_coal

    #Replace the energy input in the stub technology with the energy from the correct supplysector
    # filter(L223.StubTechProd_elec, year == CCS_retro_year & stub.technology == elec_coal_tech & region == "USA") %>%
    #   mutate(tech.share.weight = subs.share.weight) %>%
    #   mutate(calOutputValue = calOutputValue*NEMS_CCUS_Historical_Retrofit$en_out_fraction) %>%
    #   mutate(calibrated.value = calibrated.value*NEMS_CCUS_Historical_Retrofit$en_in_fraction) %>%
    #   mutate(efficiency = calOutputValue/calibrated.value) ->
    #   retrofitable_coal
    retrofitable_coal %>%
      select(!!!LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
      mutate(stub.technology = elec_CCSretro_coal_tech) ->
      # mutate(initial.available.year = CCS_retro_year,
      #        final.available.year = CCS_retro_year ) %>%
      # select(!!!LEVEL2_DATA_NAMES[["StubTechProd"]]) %>% #StubTechProdCCSRet
      # mutate(stub.technology = elec_CCSretro_coal_tech) %>%
      # select(region, supplysector, subsector, stub.technology, initial.available.year, final.available.year, year, calOutputValue, share.weight.year,
      #        subs.share.weight, tech.share.weight) ->
      L227.StubTechProd_CCSretro

    #The generation from the coal electricity technology in the final base year must be zero
    filter(L223.StubTechProd_elec, year == CCS_retro_year &
          stub.technology == elec_coal_tech &
          region == "USA") %>%
      # mutate(calOutputValue = calOutputValue*(1-NEMS_CCUS_Historical_Retrofit$en_out_fraction)) %>%
      mutate(calOutputValue = 0) %>%
      mutate(calibrated.value = calibrated.value*(1-NEMS_CCUS_Historical_Retrofit$en_in_fraction)) %>% #does cal input or eff matter?
      mutate(efficiency = calOutputValue/calibrated.value,
             efficiency = if_else(efficiency == 0, 1, efficiency)) -> # this only will happen when cal output is 0 so setting efficiency to 1 won't matter
      nonretrofitable_coal
    nonretrofitable_coal %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = 1) %>%
      mutate(tech.share.weight = ifelse(calOutputValue == 0, 0, 1 )) %>%
      select(!!!LEVEL2_DATA_NAMES[["StubTechProd"]])->
      L227.StubTechProd_coal_elec

    retrofitable_coal %>%
      mutate(supplysector = "coal CCS retrofit") %>%
      mutate(subsector = "coal CCS retrofit") %>%
      mutate(stub.technology = "existing coal") %>%
      select(-year) %>%
      tidyr::expand(., ., year=MODEL_YEARS) %>%
      select(!!!LEVEL2_DATA_NAMES[["StubTechEff"]]) %>%
      bind_rows(L227.StubTechEff_CCSretro) ->
      L227.StubTechEff_CCSretro

    nonretrofitable_coal %>%
      select(!!!LEVEL2_DATA_NAMES[["StubTechEff"]]) %>%
      bind_rows(L227.StubTechEff_coal_elec) ->
    L227.StubTechEff_coal_elec

    #Organize table
    A232.tech_associations %>%
      mutate(initial.available.year = CCS_retro_year,
      final.available.year = CCS_retro_year ) %>%
      rename( "sector.name" = "elec_supplysector", "subsector.name" = "elec_subsector") %>%
      mutate(technology = elec_CCSretro_coal_tech) %>%
      select(!!!LEVEL2_DATA_NAMES[["GlobalTechAvail"]]) ->
      L227.GlobalTechAvail_CCSretro

    L227.StubTechProd_CCSretro %>%
      mutate(initial.available.year = CCS_retro_year,
             final.available.year = CCS_retro_year ) %>%
      select(!!!LEVEL2_DATA_NAMES[["StubTechAvail"]]) ->
      L227.StubTechAvail_CCSretro

    CCS_retrofit.xml  %>%
      add_logit_tables_xml(L227.Supplysector_CCSretro, "Supplysector") %>%
      add_logit_tables_xml(L227.SubsectorLogit_CCSretro, "SubsectorLogit") %>%
      add_xml_data(L227.SubsectorShrwtFllt_CCSretro, "SubsectorShrwtFllt") %>%
      add_xml_data(L227.GlobalTechAvail_CCSretro, "GlobalTechAvail") %>%
      add_xml_data(L227.GlobalTechCapFac_CCSretro, "GlobalTechCapFac") %>%
      add_xml_data(L227.GlobalTechCapture_CCSretro, "GlobalTechCapture")%>%
      add_xml_data(L227.GlobalTechCapital_CCSretro, "GlobalTechCapital")%>%
      add_xml_data(L227.GlobalTechEff_elec_CCSretro, "GlobalTechEff")%>%
      add_xml_data(L227.GlobalTechLifetime_CCSretro, "GlobalTechLifetime")%>%
      add_xml_data(L227.GlobalTechOMfixed_CCSretro, "GlobalTechOMfixed") %>%
      add_xml_data(L227.GlobalTechOMvar_CCSretro, "GlobalTechOMvar") %>%
      add_xml_data(L227.GlobalTechProfitShutdown_CCSretro, "GlobalTechProfitShutdown")%>%
      add_xml_data(L227.GlobalTechShrwt_CCSretro, "GlobalTechShrwt") %>%
      add_xml_data(L227.StubTechEff_CCSretro, "StubTechEff") %>%
      add_xml_data(L227.StubTechEff_coal_elec, "StubTechEff")%>%
      add_xml_data(L227.StubTechProd_CCSretro, "StubTechProd")%>% #StubTechProdCCSRet
      add_xml_data(L227.StubTechProd_coal_elec, "StubTechProd")%>%
      add_xml_data(L227.StubTechAvail_CCSretro, "StubTechAvail") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A232.sector",
                     "energy/A232.subsector_logit",
                     "energy/A232.subsector_shrwt",
                     "energy/A232.globaltech_shrwt",
                     "energy/A232.globaltech_capital",
                     "energy/A232.globaltech_OMfixed",
                     "energy/A232.tech_associations",
                     "energy/NEMS_CCUS_Historical_Retrofit",
                     "L202.CarbonCoef",
                     "L223.GlobalTechCapFac_elec",
                     "L223.GlobalTechCapital_elec",
                     "L223.GlobalTechCapture_elec",
                     "L223.GlobalTechEff_elec",
                     "L223.GlobalTechLifetime_elec",
                     "L223.GlobalTechProfitShutdown_elec",
                     "L223.GlobalTechOMfixed_elec",
                     "L223.GlobalTechOMvar_elec",
                     "L223.GlobalTechSCurve_elec",
                     "L223.StubTechEff_elec",
                     "L223.StubTechCalInput_elec")->
      CCS_retrofit.xml

    # Reorder
    CCS_retrofit.xml[["data_tables"]][[28]]$data <- L227.StubTechProd_CCSretro

    return_data(CCS_retrofit.xml)
  } else {
    stop("Unknown command")
  }
}
