# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_minerals_L1111.rsrc_minerals_supply
#'
#' Calculate mineral resource historical production, supply curves and annual production constraints.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1111.mineral_production_R_Y_hist}, \code{L1111.mineral_AnnProdLimit_R_Y}, \code{L1111.mineral_AnnResourceLimit_R_Y},
#' \code{L1111.ResSupplyCurves_PricePoints}, \code{L1111.mineral_AvgProdLifetime}
#' @details Using mine-level data to generate mineral historical production, resource supply curves and annual production constraints at the GCAM-region level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na pivot_longer pivot_wider expand_grid
#' @author BY February 2025
#'
module_minerals_L1111.rsrc_minerals_supply <- function(command, ...) {
if(command == driver.DECLARE_INPUTS) {
  return(c(FILE = "common/iso_GCAM_regID",
           FILE = "common/GCAM_region_names",
           FILE = "minerals/supply/Mineral_supply_curve_data",
           FILE = "minerals/supply/All_minerals_GCAM_reg_supply_data",
           FILE = "minerals/supply/historical_copper_production",
           FILE = "minerals/supply/historical_lithium_production",
           FILE = "minerals/supply/historical_nickel_production"))
} else if(command == driver.DECLARE_OUTPUTS) {
  return(c("L1111.mineral_production_R_Yb",
           "L1111.mineral_AnnProdLimit_R_Y",
           "L1111.mineral_AnnResourceLimit_R_Y",
           "L1111.ResSupplyCurves_PricePoints",
           "L1111.mineral_AvgProdLifetime"))
} else if(command == driver.MAKE) {

  all_data <- list(...)[[1]]

  # Load required inputs
  iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  GCAM_region_names <-get_data(all_data, "common/GCAM_region_names")
  All_minerals_GCAM_reg_supply_data <- get_data(all_data, "minerals/supply/All_minerals_GCAM_reg_supply_data")
  Mineral_supply_curve_data <- get_data(all_data, "minerals/supply/Mineral_supply_curve_data")

  #convert historical production data to kt to align with the supply curve data
  historical_copper_production <- get_data(all_data, "minerals/supply/historical_copper_production") %>%
    mutate(Mineral = "Cu",
           production = production/1000,
           Units = "kt")
  historical_lithium_production <- get_data(all_data, "minerals/supply/historical_lithium_production") %>%
    mutate(Mineral = "Li",
           production = production/1000,
           Units = "kt")
  historical_nickel_production <- get_data(all_data, "minerals/supply/historical_nickel_production") %>%
    mutate(Mineral = "Ni",
           production = production/1000,
           Units = "kt")




# PROCESS HISTORICAL MINERAL PRODUCTION BY REGION -------------------------

# We need a consistent data set for mineral production by GCAM region over the historical period (1975-2020) or (1975-2023)
  # Copper data includes country level back to 1970 - ALL SET
  # Lithium data includes country level back to 2020, Global level back to 2000 - Need to extrapolate data back to 1975 and partition by country from 1975-2020
  # Nickel data includes country level back to 2020, Global level back to 1900! - Need to partition by country from 1975-2020
    # also need to partition "Other" for all years to country-level.

  # For lithium, extrapolate back the global production to 1975
  # Use the linear growth rate from 2000-2010
  # 2000: 14.1 kt
  # 2010: 24.4 kt
  # linear growth rate = 0.173 kt/year
    # Define the known start point
    start_year <- 2000
    start_production <- 14.1 # kt

    # Define the linear growth rate (given)
    growth_rate <- 0.173 # kt/year

    # Create a sequence of years from 1975 to 1999
    extrapolated_years <- tibble(Year = seq(1975, 1999, by = 1))

  # Compute production using linear extrapolation
  historical_lithium_production_1975_1999_GLO <- extrapolated_years %>%
    mutate(production = start_production - (start_year - Year) * growth_rate) %>%
    mutate(Entity = "World",
           Mineral = "Li") # Keep consistent with the original dataset

  # For lithium and nickel, we need to partition the global data into country level.
  # Do this on the basis of the earliest available year (2020) with all countries

  # Nickel has a category "Other", we should partition this based on Karan's data
  Ni_production_shares_Other <- Mineral_supply_curve_data %>%
    filter(Mineral == "Ni", Stage == "Production") %>%
    select(Mineral, Country, Capacity) %>%
    mutate(Country = if_else(Country == "USA", "United States", Country)) %>%
    filter(!Country %in% c(unique(historical_nickel_production$Entity))) %>%
    mutate(Share = Capacity/sum(Capacity)) %>%
    rename(Entity = Country)

  Ni_production_Other_2020 <- Ni_production_shares_Other %>%
    # 2020 "Other" is 373000 t = 373 kt, multiply this by share
    mutate(production = 373 * Share,
           Year = 2020,
           Mineral = "Ni") %>%
    select(Entity, Year, production, Mineral)

  historical_production_country_shares_Li_Ni <- bind_rows(historical_lithium_production,
                                                          historical_nickel_production) %>%
    filter(Entity != "World", Entity != "Other")  %>%
    group_by(Mineral) %>%
    filter(Year == min(Year)) %>%
    bind_rows(Ni_production_Other_2020) %>%
    group_by(Mineral) %>%
    mutate(Share = production/sum(production)) %>%
    ungroup() %>%
    select(Entity, Share, Mineral)

  historical_production_GLO_Li_Ni <- bind_rows(historical_lithium_production_1975_1999_GLO,
                                               historical_lithium_production,
                                               historical_nickel_production) %>%
    filter(Entity == "World", Year >= 1975) %>%
    arrange(Mineral, Year)

  # Now we have a country level data for Li and Ni from 1975-2019
  historical_production_country_Li_Ni_1975_2019 <- historical_production_GLO_Li_Ni %>%
    left_join(historical_production_country_shares_Li_Ni, by = c("Mineral")) %>%
    mutate(production = production * Share,
           Entity = Entity.y) %>%
    select(Mineral, Year, Entity, production) %>%
    filter(Year < 2020)

  #partition "Other" Ni production from 2020 to 2023
  historical_production_Ni_Other_2020_2023 <- historical_nickel_production %>%
    filter(Entity == "Other") %>%
    select(-Entity) %>%
    left_join(Ni_production_shares_Other) %>%
    mutate(production = production * Share) %>%
    select(Mineral, Year, Entity, production)

 # combine all data together
  historical_mineral_production_C_Y <- bind_rows(historical_lithium_production,
                                                 historical_nickel_production) %>%
    filter(Entity != "World", Entity != "Other", Year >= 2020) %>%
    bind_rows(historical_production_country_Li_Ni_1975_2019,
              historical_production_Ni_Other_2020_2023) %>%
    bind_rows(historical_copper_production) %>%
    filter(Year >= 1975, Entity != "World") %>%
    arrange(Year, Mineral, Entity, production) %>%
    mutate(production = production,
           Units = "kt")
    # check data
   # spread(key = "Year", value = "production")


  # aggregate to GCAM region level
  L1111.mineral_production_R_Y_hist <- historical_mineral_production_C_Y %>%
    # For regions that no longer exist, just map to the nearest existing region
    mutate(Entity = gsub("Czechoslovakia", "Czech Republic", Entity),
           Entity = gsub("Democratic Republic of Congo", "Congo, the Democratic Republic of the", Entity),
           Entity = gsub("East Germany", "Germany", Entity),
           Entity = gsub("West Germany", "Germany", Entity),
           Entity = gsub("Iran", "Iran, Islamic Republic of", Entity),
           Entity = gsub("North Korea", "Korea, Democratic Peoples Republic of", Entity),
           Entity = gsub("South Korea", "Korea, Republic of", Entity),
           Entity = gsub("Russia", "Russian Federation", Entity),
           Entity = gsub("USSR", "Russian Federation", Entity),
           Entity = gsub("United States", "United States of America", Entity),
           Entity = gsub("Yugoslavia", "Yugoslavia, Federal Republic of", Entity),
           Entity = gsub("North Macedonia", "Macedonia, the former Yugoslav Republic of", Entity),
           Entity = gsub("Tanzania", "Tanzania, United Republic of", Entity),
           Entity = gsub("Vietnam", "Viet Nam", Entity),
           Entity = gsub("Laos", "Lao Peoples Democratic Republic", Entity)) %>%
    left_join(iso_GCAM_regID, by = c("Entity" = "country_name")) %>%
    na.omit() %>%
    group_by(Mineral, GCAM_region_ID, Year, Units) %>%
    dplyr::summarise(value = sum(production)) %>%
    ungroup() %>%
    left_join_error_no_match(GCAM_region_names) %>%
    select(-GCAM_region_ID) %>%
    # complete the set of data for all historical years (interpolation/extrapolation)
    group_by(Mineral, region, Units) %>%
    arrange(Year, .by_group = TRUE) %>%
    complete(Year = c(seq(1975,2022,by=1))) %>%
    mutate(value = approx_fun(Year, value, rule = 2)) %>%
    ungroup()

  L1111.mineral_production_R_Yb <- L1111.mineral_production_R_Y_hist %>%
    # filter to GCAM model base years and the first model future year
    filter(Year %in% c(MODEL_BASE_YEARS, 2020)) %>%
    complete(Mineral, region = GCAM_region_names$region, Units, Year = c(MODEL_BASE_YEARS, 2020)) %>%
    mutate(value = if_else(is.na(value), 0, value)) ##final-output


# PROCESS SUPPLY CURVE DATA -----------------------------------------------


  # UPDATED SUPPLY CURVE DATA
  L1111.All_data_reg <- All_minerals_GCAM_reg_supply_data %>%
    mutate(Mineral = case_when(Resource == "Copper" ~ "Cu",
                               Resource == "Lithium" ~ "Li",
                               Resource == "Nickel" ~ "Ni")) %>%
    select(Mineral, region, Stage, Capacity = Production, Resource = Reserves, P10 = cost_10pct, P50 = cost_50pct, P90 = cost_90pct) %>%
    # calculate "lifetime" (years) if reserve were to be produced at capacity until exhausted
    mutate(Lifetime = Resource / Capacity) %>%
    # For now, omitting rows in which there is production capacity but zero resources
    filter(Resource != 0)

  #OLD SUPPLY CURVE DATA
  # First, aggregate data (all variables to the GCAM 32 region level)
  L1111.All_data_reg_OLD <- Mineral_supply_curve_data %>%
    mutate(Country = gsub("Dem. Rep. Congo", "Congo, the Democratic Republic of the", Country),
           Country = gsub("Iran", "Iran, Islamic Republic of", Country),
           Country = gsub("Laos", "Lao Peoples Democratic Republic", Country),
           Country = gsub("Rep. Of the Congo", "Congo", Country),
           Country = gsub("Russia", "Russian Federation", Country),
           Country = gsub("Tanzania", "Tanzania, United Republic of", Country),
           Country = gsub("USA", "United States of America", Country),
           Country = gsub("Vietnam", "Viet Nam", Country),
           Country = gsub("Bosnia & Herzegovina", "Bosnia and Herzegovina", Country),
           Country = gsub("Czechia", "Czech Republic", Country),
           Country = gsub("South Korea", "Korea, Republic of", Country),
           Country = gsub("Cote d'Ivoire", "Cote dIvoire", Country)) %>%
    left_join(iso_GCAM_regID, by = c("Country" = "country_name")) %>%
    group_by(GCAM_region_ID, Mineral, Stage) %>%
    summarise(Capacity = sum(Capacity),
              Resource = sum(Resource),
              # Should cost be the weighted average cost based on resource or capacity?
              # I think resource, because this will be used for the cumulative supply curve, based on resources
              # Let's revisit this assumption later
              P10 = sum(P10 * Resource)/sum(Resource),
              P50 = sum(P50 * Resource)/sum(Resource),
              P90 = sum(P90 * Resource)/sum(Resource)) %>%
    ungroup() %>%
    left_join(GCAM_region_names, by = c("GCAM_region_ID")) %>%
    select(Mineral, region, Stage, Capacity, Resource, P10, P50, P90) %>%
    # calculate "lifetime" (years) if reserve were to be produced at capacity until exhausted
    mutate(Lifetime = Resource / Capacity) %>%
    ##NOTE: For lithium, some resources are "NA" while there is still capacity for production in that region. For now, omit these rows.
    na.omit()

  # Calculate price points based on Karan's data.
  # Assumptions:
  # 1: price does not change from initial levels (earliest stage available)
  # 2. drop 50th percentile cost if it is higher than the 90th percentile cost
  # 3. assume P100 to be 3x cost of P90

  L1111.ResSupplyCurves_PricePoints <- L1111.All_data_reg %>%
    # assign each stage a number.
    mutate(StageNum = case_when(Stage == "Production" ~ 1,
                                Stage == "Pre-Production" ~ 2,
                                Stage == "Incentive" ~ 3,
                                Stage == "Late Stage" ~ 4,
                                Stage == "Early Stage" ~ 5)) %>%
    group_by(Mineral, region) %>%
    filter(StageNum == min(StageNum)) %>%
    select(Mineral, region, P10, P50, P90) %>%
    # assume a very high cost for the resource limit (e.g. 3x the 90th percentile cost)
    mutate(P100 = P90*3) %>%
    mutate(P50 = ifelse(P50 > P90, NA, P50)) %>%  # Set P_50 to NA if P_50 > P_90
    tidyr::pivot_longer(cols = c(`P10`, `P50`, `P90`, `P100`), names_to = "percentile", values_to = "P", values_drop_na = TRUE) %>%
    mutate(percentile = gsub("P", "", percentile)) %>%
    ungroup()


# ANNUAL PRODUCTION LIMIT  (NEW METHOD) --------------------------------------------------

    # Calculate how much production comes online using a binomial distribution by stage

  All_capacity_data_stages <- L1111.All_data_reg %>%
    select(Mineral, region, Stage, Capacity) %>%
    #StageNum sets the Stages in the correct order.
    mutate(StageNum = case_when(Stage == "Production" ~ 1,
                                Stage == "Pre-Production" ~ 2,
                                Stage == "Incentive" ~ 3,
                                Stage == "Late Stage" ~ 4,
                                Stage == "Early Stage" ~ 5)) %>%
    select(-Stage) %>%
    spread(key = StageNum, value = Capacity) %>%
    # Fill in data tables with zero values if a region has no capacity in that stage
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    gather(key = "StageNum", value = "Capacity", `1`, `2`, `3`, `4`, `5`)


  #First, Set average number of years to move through each stage based on lead times for that mineral (S&P data)

    Li_capacity_data_AvgYears <- All_capacity_data_stages %>%
      filter(Mineral == "Li") %>%
      # assign the average number of years for each stage to be available
      mutate(AvgYears = case_when(StageNum == 1 ~ 0,
                                  StageNum == 2 ~ 2,
                                  StageNum == 3 ~ 1.7,
                                  StageNum == 4 ~ 5, #0.38 * 13 # REVISIT THIS LATER
                                  StageNum == 5 ~ 8)) #0.62 * 13

    Ni_capacity_data_AvgYears  <- All_capacity_data_stages %>%
      filter(Mineral == "Ni") %>%
      # assign the average number of years for each stage to be available
      mutate(AvgYears = case_when(StageNum == 1 ~ 0,
                                  StageNum == 2 ~ 4.1,
                                  StageNum == 3 ~ 2.5,
                                  StageNum == 4 ~ 4.4,
                                  StageNum == 5 ~ 7.2))

    Cu_capacity_data_AvgYears <- All_capacity_data_stages %>%
      filter(Mineral == "Cu") %>%
      # assign the average number of years for each stage to be available
      mutate(AvgYears = case_when(StageNum == 1 ~ 0,
                                  StageNum == 2 ~ 2.4,
                                  StageNum == 3 ~ 1.6,
                                  StageNum == 4 ~ 4.9,
                                  StageNum == 5 ~ 7.9))

    All_capacity_data_AvgYears <- bind_rows(Cu_capacity_data_AvgYears,
                               Ni_capacity_data_AvgYears,
                               Li_capacity_data_AvgYears) %>%

             # Calculate the transition rate = fraction moving to the next stage in a given year
             mutate(TransitionRate = 0.5/AvgYears,
             # In the case of what is already in production, it does not move to another stage,
             # so reset the TransitionRate to 0
             TransitionRate = if_else(StageNum == 1, 0, TransitionRate)) %>%
      arrange(StageNum, Mineral, region) %>%
      select(Mineral, region, StageNum, AvgYears, Capacity, TransitionRate) %>%
      # Supply curve data is based on a 2023 snapshot. But for now, we will treat it as 2020.
      mutate(Year = 2020)

  # Recursive function to update the Capacity that is in each stage
    UpdateCapacity <- function(data, data_all) {
      data <- data %>%
        # iterate the year
        mutate(Year = Year + 1) %>%
        arrange(Mineral, region, StageNum) %>%
        group_by(Mineral, region) %>%
        # Calculate the capacity in each stage in the given year, based on previous stage capacity and transition rate
        # For the last Stage (i.e. earliest stage) for each Mineral/Region, the Capacity remains constant (assume a steady-state of Capacity in this stage)
        mutate(PrevStageCapacity = lead(Capacity, default = 0),
               PrevTransitionRate = lead(TransitionRate, default = 0),
               Capacity = if_else(StageNum == max(StageNum), Capacity, Capacity*(1-TransitionRate) + PrevStageCapacity*(PrevTransitionRate))) %>%
        ungroup()
      yr <- data$Year[1]
      data_all <- bind_rows(data_all, data)
      if(yr < 2100){
        UpdateCapacity(data, data_all)
      } else{

        return(data_all)

      }
    }

  All_capacity_data_AllYr  <- UpdateCapacity(All_capacity_data_AvgYears, All_capacity_data_AvgYears)


  # Adjust production constraint.

  #Get only "Production", which is the first stage
  AnnProdLimit_AllYr <- All_capacity_data_AllYr %>%
    filter(StageNum == 1) %>%
    select(Mineral, region, Year, Capacity)

   # Calculate the production growth rate relative to 2020
   AnnProdLimit_GrowthRate <- AnnProdLimit_AllYr %>%
     mutate(R = Capacity/Capacity[Year == 2020])

   # In cases where actual historical production in 2020 is larger than the production constraint,
   # we want to set the constraint based on 2020 actual production
   AnnProdLimit_check2020 <- L1111.mineral_production_R_Y_hist %>%
     filter(Year == 2020) %>%
     select(-Year) %>%
     rename(Prod2020 = value) %>%
     left_join(AnnProdLimit_GrowthRate, by = c("Mineral", "region")) %>%
     filter(Year == 2020) %>%
     mutate(Reset_Capacity = if_else(Prod2020 > Capacity, 1, 0)) %>%
     select(Mineral, region, Prod2020, Reset_Capacity)

   AnnProdLimit_adj <- AnnProdLimit_check2020 %>%
     right_join(AnnProdLimit_GrowthRate, by = c("Mineral", "region")) %>%
     mutate(Prod2020 = if_else(is.na(Prod2020), 0, Prod2020)) %>%
     group_by(Mineral, region, Year) %>%
     mutate(Capacity_adj = if_else(Reset_Capacity == 1, R*Prod2020, Capacity),
            Capacity_adj = if_else(is.na(Capacity_adj), max(Prod2020, Capacity), Capacity_adj),
            Capacity_adj = if_else(is.nan(Capacity_adj), max(Prod2020, Capacity), Capacity_adj),
            Capacity_adj = if_else(is.infinite(Capacity_adj),  max(Prod2020, Capacity), Capacity_adj)) %>%
     select(Mineral, region, Year, Capacity_adj) %>%
     ungroup()

   # Filter to model years
   L1111.mineral_AnnProdLimit_R_Y <- AnnProdLimit_adj %>%
     filter(Year %in% MODEL_YEARS)  ##final-output


# ANNUAL TOTAL RESOURCES LIMIT (NEW METHOD) ---------------------------------------

 # Similar to the evolution of Capacity available for Production in each year,
 # the total underlying Resources accessible will depend on the Capacity that comes on-line (e.g. what mines have been opened).

   # Calculate how much Resource comes online using a binomial distribution by stage

   All_resource_data_stages <- L1111.All_data_reg %>%
     select(Mineral, region, Stage, Resource) %>%
     #StageNum sets the Stages in the correct order.
     mutate(StageNum = case_when(Stage == "Production" ~ 1,
                                 Stage == "Pre-Production" ~ 2,
                                 Stage == "Incentive" ~ 3,
                                 Stage == "Late Stage" ~ 4,
                                 Stage == "Early Stage" ~ 5)) %>%
     select(-Stage) %>%
     spread(key = StageNum, value = Resource) %>%
     # Fill in data tables with zero values if a region has no resource in that stage
     mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
     gather(key = "StageNum", value = "Resource", `1`, `2`, `3`, `4`, `5`)

   #First, Set average number of years to move through each stage based on lead times for that mineral (S&P data)

   Li_resource_data_AvgYears <- All_resource_data_stages %>%
     filter(Mineral == "Li") %>%
     # assign the average number of years for each stage to be available
     mutate(AvgYears = case_when(StageNum == 1 ~ 0,
                                 StageNum == 2 ~ 2,
                                 StageNum == 3 ~ 1.7,
                                 StageNum == 4 ~ 5, #0.38 * 13 # REVISIT THIS LATER
                                 StageNum == 5 ~ 8)) #0.62 * 13

   Ni_resource_data_AvgYears  <- All_resource_data_stages %>%
     filter(Mineral == "Ni") %>%
     # assign the average number of years for each stage to be available
     mutate(AvgYears = case_when(StageNum == 1 ~ 0,
                                 StageNum == 2 ~ 4.1,
                                 StageNum == 3 ~ 2.5,
                                 StageNum == 4 ~ 4.4,
                                 StageNum == 5 ~ 7.2))

   Cu_resource_data_AvgYears <- All_resource_data_stages %>%
     filter(Mineral == "Cu") %>%
     # assign the average number of years for each stage to be available
     mutate(AvgYears = case_when(StageNum == 1 ~ 0,
                                 StageNum == 2 ~ 2.4,
                                 StageNum == 3 ~ 1.6,
                                 StageNum == 4 ~ 4.9,
                                 StageNum == 5 ~ 7.9))

   All_resource_data_AvgYears <- bind_rows(Cu_resource_data_AvgYears,
                                           Ni_resource_data_AvgYears,
                                           Li_resource_data_AvgYears) %>%

     # Calculate the transition rate = fraction moving to the next stage in a given year
     mutate(TransitionRate = 0.5/AvgYears,
            # In the case of what is already in production, it does not move to another stage,
            # so reset the TransitionRate to 0
            TransitionRate = if_else(StageNum == 1, 0, TransitionRate)) %>%
     arrange(StageNum, Mineral, region) %>%
     select(Mineral, region, StageNum, AvgYears, Resource, TransitionRate) %>%
     # set Initial Year as 2020
     mutate(Year = 2020)

   # Recursive function to update the Resource that is in each stage
   UpdateResource <- function(data, data_all) {
     data <- data %>%
       # iterate the year
       mutate(Year = Year + 1) %>%
       arrange(Mineral, region, StageNum) %>%
       group_by(Mineral, region) %>%
       # Calculate the resource in each stage in the given year, based on previous stage resource and transition rate
       mutate(PrevStageResource = lead(Resource, default = 0),
              PrevTransitionRate = lead(TransitionRate, default = 0),
              Resource = Resource*(1-TransitionRate) + PrevStageResource*(PrevTransitionRate)) %>%
       ungroup()
     yr <- data$Year[1]
     data_all <- bind_rows(data_all, data)
     if(yr < 2100){
       UpdateResource(data, data_all)
     } else{

       return(data_all)

     }
   }

   All_resource_data_AllYr  <- UpdateResource(All_resource_data_AvgYears, All_resource_data_AvgYears)

   #Get only Resources associated with "Production", which is the first stage
   AnnResourceLimit_AllYr <- All_resource_data_AllYr %>%
     filter(StageNum == 1) %>%
     select(Mineral, region, Year, Resource)

   # Filter to model years
   L1111.mineral_AnnResourceLimit_R_Y <- AnnResourceLimit_AllYr %>%
     filter(Year %in% MODEL_YEARS) %>%
     group_by(Mineral, region) %>%
     complete(Year = MODEL_YEARS) %>%
     # Fill in 0 Resources for years before the first year in which resources become available
     mutate(Resource = ifelse(Year < min(Year[!is.na(Resource)]), 0, Resource),
            Units = "kt") %>%
     ungroup() ##final-output


# AVERAGE PRODUCTION LIFETIME ----------------------------------------------------------------


   # For now, taking a simple approach for representing lifetime.

   # Lifetime calculated as sum of Resources across stages / Capacity across stages
  Lifetime_sumStage <- L1111.All_data_reg %>%
    group_by(Mineral, region) %>%
    dplyr::summarise(Capacity = sum(Capacity),
                   Resource = sum(Resource)) %>%
    mutate(Lifetime = Resource/Capacity) %>%
    ungroup()

  # Find the average and median lifetime across regions
  Lifetime_sumStage_stats <- Lifetime_sumStage %>%
    group_by(Mineral) %>%
    dplyr::summarise(average = mean(Lifetime),
                      median = median(Lifetime)) %>%
    ungroup()

  # For regions with lifetimes above the median, just set it to the median lifetime
  L1111.mineral_AvgProdLifetime <- Lifetime_sumStage %>%
    select(-Capacity, -Resource) %>%
    left_join(Lifetime_sumStage_stats) %>%
    mutate(Lifetime = if_else(Lifetime > median, median, Lifetime)) %>%
    select(Mineral, region, Lifetime) ##final-output

  ## COMPARE WITH OLD DATA
  # Lifetime calculated as sum of Resources across stages / Capacity across stages
  # Lifetime_sumStage_OLD <- L1111.All_data_reg_OLD %>%
  #   group_by(Mineral, region) %>%
  #   dplyr::summarise(Capacity = sum(Capacity),
  #                    Resource = sum(Resource)) %>%
  #   mutate(Lifetime = Resource/Capacity) %>%
  #   ungroup()
  #
  # # Find the average and median lifetime across regions
  # Lifetime_sumStage_stats_OLD <- Lifetime_sumStage_OLD %>%
  #   group_by(Mineral) %>%
  #   dplyr::summarise(average = mean(Lifetime),
  #                    median = median(Lifetime)) %>%
  #   ungroup()
  #
  # # For regions with lifetimes above the median, just set it to the median lifetime
  # L1111.mineral_AvgProdLifetime_OLD <- Lifetime_sumStage_OLD %>%
  #   select(-Capacity, -Resource) %>%
  #   left_join(Lifetime_sumStage_stats) %>%
  #   mutate(Lifetime = if_else(Lifetime > median, median, Lifetime)) %>%
  #   select(Mineral, region, Lifetime) ##final-output
  #
  # #Compare
  # L1111.mineral_AvgProdLifetime_compare <- L1111.mineral_AvgProdLifetime %>%
  #   left_join(L1111.mineral_AvgProdLifetime_OLD, by = c("Mineral", "region"), suffix = c(".NEW", ".OLD"))


# WRITE OUTPUTS -----------------------------------------------------------

  L1111.mineral_production_R_Yb %>%
    add_title("Mineral resource historical production", overwrite = TRUE) %>%
    add_units("kt/yr") %>%
    add_comments("Using historical (USGS/Our World in Data) data to get historical production at the GCAM region for model historical base years") %>%
    add_precursors("common/iso_GCAM_regID",
                   "common/GCAM_region_names",
                   "minerals/supply/historical_copper_production",
                   "minerals/supply/historical_lithium_production",
                   "minerals/supply/historical_nickel_production") ->
    L1111.mineral_production_R_Yb

  L1111.mineral_AnnProdLimit_R_Y %>%
    add_title("Mineral resource annual production limits", overwrite = TRUE) %>%
    add_units("kt/yr") %>%
    add_comments("Using mine-level data to generate mineral resource annual production limits at the GCAM-region level. Linearly extrapolate for 2045 onward.") %>%
    add_precursors("common/iso_GCAM_regID",
                   "common/GCAM_region_names",
                   "minerals/supply/Mineral_supply_curve_data") ->
    L1111.mineral_AnnProdLimit_R_Y

  L1111.mineral_AnnResourceLimit_R_Y %>%
    add_title("Mineral resource annual resource limit", overwrite = TRUE) %>%
    add_units("kt") %>%
    add_comments("Using mine-level data to generate mineral resource annual resource limits at the GCAM-region level.") %>%
    add_precursors("common/iso_GCAM_regID",
                   "common/GCAM_region_names",
                   "minerals/supply/Mineral_supply_curve_data") ->
    L1111.mineral_AnnResourceLimit_R_Y

  L1111.ResSupplyCurves_PricePoints %>%
    add_title("Mineral resource supply curve price points", overwrite = TRUE) %>%
    add_units("Quantity: kt and Price: 2020$/t") %>%
    add_comments("Price points are time-invariant. These will be combined with resources (quantity) in level2 to form supply curves.") %>%
    add_precursors("common/iso_GCAM_regID",
                   "common/GCAM_region_names",
                   "minerals/supply/Mineral_supply_curve_data") ->
    L1111.ResSupplyCurves_PricePoints

  L1111.mineral_AvgProdLifetime %>%
    add_title("Mineral average production lifetime", overwrite = TRUE) %>%
    add_units("years") %>%
    add_comments("Calculated based on ratio of resources to capacity across all stages in the initial year. Use values only up to the median lifetime, any values above are reset to median.") %>%
    add_precursors("common/iso_GCAM_regID",
                   "common/GCAM_region_names",
                   "minerals/supply/Mineral_supply_curve_data") ->
    L1111.mineral_AvgProdLifetime



  return_data(L1111.mineral_production_R_Yb,
              L1111.mineral_AnnProdLimit_R_Y,
              L1111.mineral_AnnResourceLimit_R_Y,
              L1111.ResSupplyCurves_PricePoints,
              L1111.mineral_AvgProdLifetime)
  } else {
    stop("Unknown command")
  }

}

