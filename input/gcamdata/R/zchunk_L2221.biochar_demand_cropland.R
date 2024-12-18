# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2221.biochar_demand_cropland
#'
#' Prepare the assumptions for the demand of biochar in cropland; similar to fertilizer demand
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") alll
#' the generated outputs: \code{L2221.LN5_HistMgdAllocation_crop_biochar},
#' \code{L2221.LN5_MgdAllocation_crop_biochar},
#' \code{L2221.LN5_MgdCarbon_crop_biochar},
#' \code{L2221.LN5_LeafGhostShare},
#' \code{L2221.AgProduction_ag_irr_mgmt}, \code{L2221.AgHAtoCL_irr_mgmt},
#' \code{L2221.AgYield_biochar_ref},
#' \code{L2221.AgCost_ag_irr_mgmt},
#' \code{L2221.AgProdChange_ag_irr_ref},
#' \code{L2221.AgCoef_Fert_ag_irr_mgmt},
#' \code{L2221.AgCost_ag_irr_mgmt_adj}, \code{L2221.AgResBio_ag_irr_mgmt},
#' \code{L2221.AgResBioCurve_ag_irr_mgmt}, \code{L2221.AgCoef_Biochar_ag_irr_mgmt}, \code{L2221.AgCost_Biochar_irr_mgmt_adj},
#' \code{L2221.CarbonCoef}, \code{L2221.AgCoef_IrrBphysWater_ag_mgmt},
#' \code{L2221.AgCoef_IrrWaterWdraw_ag_mgmt},
#' \code{L2221.AgCoef_IrrWaterCons_ag_mgmt},
#' \code{L2221.AgCoef_RfdBphysWater_ag_mgmt}.
#' @details This chunk sets up the biochar demand in agriculture. We assume an application rate of 20 tons of
#' biochar per hectare. We follow similar approach to fertilizer to calculate IO coefficients.
#' Applies only to future years
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author MCB March 2021
module_energy_L2221.biochar_demand_cropland <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(# DayCent Data
      FILE = "aglu/A221.biochar_yield_ratio",
      FILE = "aglu/A221.biochar_soc",
      # PART 1: biochar_land_input_5_IRR_MGMT.xml
      "L2252.LN5_HistMgdAllocation_crop",
      "L2252.LN5_MgdAllocation_crop",
      "L2252.LN5_MgdCarbon_crop",
      FILE = "energy/A221.ghost_shrwt",
      # PART 2: biochar_ag_For_Past_bio_base_IRR_MGMT.xml
      "L2012.AgProduction_ag_irr_mgmt",
      "L2012.AgHAtoCL_irr_mgmt",
      "L2012.AgYield_bio_ref",
      # PART 3: biochar_ag_cost_IRR_MGMT.xml
      "L2052.AgCost_ag_irr_mgmt",
      # PART 4: ag_prodchange_ref_IRR_MGMT.xml
      "L2052.AgProdChange_ag_irr_ref",
      FILE = "common/GCAM_region_names_climate_zone",
      FILE = "aglu/A221.biochar_yield_increase",
      FILE = "aglu/A221.biochar_land_alloc",
      # PART 5: biochar_ag_Fert_IRR_MGMT.xml
      "L2062.AgCoef_Fert_ag_irr_mgmt",
      "L2062.AgCost_ag_irr_mgmt_adj",
      # PART 6: biochar_resbio_input_IRR_MGMT.xml
      "L2042.AgResBio_ag_irr_mgmt",
      "L2042.AgResBioCurve_ag_irr_mgmt",
      # PART 7: biochar_ag_demand.xml
      FILE = "energy/A221.A_PrimaryFuelCCoef",
      FILE = "common/GCAM_region_names",
      FILE = "aglu/A221.bio_tech_prod",
      FILE = "aglu/A221.bio_tech_land_alloc",
      # PART 8: biochar_ag_water_input_IRR_MGMT.xml
      "L2072.AgCoef_IrrBphysWater_ag_mgmt",
      "L2072.AgCoef_IrrWaterWdraw_ag_mgmt",
      "L2072.AgCoef_IrrWaterCons_ag_mgmt",
      "L2072.AgCoef_RfdBphysWater_ag_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(#1. biochar_land_input_5_IRR_MGMT.xml
      "L2221.LN5_HistMgdAllocation_crop_biochar",
      "L2221.LN5_MgdAllocation_crop_biochar",
      "L2221.LN5_MgdCarbon_crop_biochar",
      "L2221.LN5_LeafGhostShare",
      #2. biochar_ag_For_Past_bio_base_IRR_MGMT.xml
      "L2221.AgProduction_ag_irr_mgmt",
      "L2221.AgHAtoCL_irr_mgmt",
      "L2221.AgYield_biochar_ref",
      #3. biochar_ag_cost_IRR_MGMT.xml
      "L2221.AgCost_ag_irr_mgmt",
      #4. ag_prodchange_ref_IRR_MGMT.xml
      "L2221.AgProdChange_ag_irr_ref",
      #5. biochar_ag_Fert_IRR_MGMT.xml
      "L2221.AgCoef_Fert_ag_irr_mgmt",
      "L2221.AgCost_ag_irr_mgmt_adj",
      #6. biochar_resbio_input_IRR_MGMT.xml
      "L2221.AgResBio_ag_irr_mgmt",
      "L2221.AgResBioCurve_ag_irr_mgmt",
      #7. biochar_ag_demand.xml
      "L2221.AgCoef_Biochar_ag_irr_mgmt",
      "L2221.AgCost_Biochar_irr_mgmt_adj",
      "L2221.CarbonCoef",
      #8. biochar_ag_water_input_IRR_MGMT.xml
      "L2221.AgCoef_IrrBphysWater_ag_mgmt",
      "L2221.AgCoef_IrrWaterWdraw_ag_mgmt",
      "L2221.AgCoef_IrrWaterCons_ag_mgmt",
      "L2221.AgCoef_RfdBphysWater_ag_mgmt"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silencing global variable package checks
    GCAM_region_ID <- calOutputValue <- coefficient <- fuel <- half.life <- input.cost <- lifetime <-
      median.shutdown.point <- minicam.energy.input <- minicam.non.energy.input <- object <-
      profit.shutdown.steepness <- region <- remove.fraction <- sector <- sector.name <- share.weight <-
      shutdown.rate <- steepness <- stub.technology <- subsector <- subsector.name <- supplysector <-
      technology <- to.value <- value <- year <- year.fillout <- year.share.weight <- year.x <- year.y <-
      primary.consumption <- NULL

    # Load required inputs
    # DayCent Data
    A221.biochar_yield_ratio <- get_data(all_data, "aglu/A221.biochar_yield_ratio")
    A221.biochar_soc <- get_data(all_data, "aglu/A221.biochar_soc")

    #PART 1: biochar_land_input_5_IRR_MGMT.xml
    L2252.LN5_HistMgdAllocation_crop <- get_data(all_data, "L2252.LN5_HistMgdAllocation_crop")
    L2252.LN5_MgdAllocation_crop <- get_data(all_data, "L2252.LN5_MgdAllocation_crop")
    L2252.LN5_MgdCarbon_crop <- get_data(all_data, "L2252.LN5_MgdCarbon_crop")
    A221.ghost_shrwt <- get_data(all_data, "energy/A221.ghost_shrwt")

    #PART 2: biochar_ag_For_Past_bio_base_IRR_MGMT.xml
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
    L2012.AgHAtoCL_irr_mgmt <- get_data(all_data, "L2012.AgHAtoCL_irr_mgmt")
    L2012.AgYield_bio_ref <- get_data(all_data, "L2012.AgYield_bio_ref")

    #PART 3: biochar_ag_cost_IRR_MGMT.xml
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "L2052.AgCost_ag_irr_mgmt")

    #PART 4: ag_prodchange_ref_IRR_MGMT.xml
    L2052.AgProdChange_ag_irr_ref <- get_data(all_data, "L2052.AgProdChange_ag_irr_ref")
    GCAM_region_names_climate_zone <- get_data(all_data, "common/GCAM_region_names_climate_zone")
    # Use NREL
    A221.biochar_yield_increase <- get_data(all_data, "aglu/A221.biochar_yield_increase")
    A221.biochar_land_alloc <- get_data(all_data, "aglu/A221.biochar_land_alloc")

    #PART 5: biochar_ag_Fert_IRR_MGMT.xml
    L2062.AgCoef_Fert_ag_irr_mgmt <- get_data(all_data, "L2062.AgCoef_Fert_ag_irr_mgmt")
    L2062.AgCost_ag_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_ag_irr_mgmt_adj")

    #PART 6: biochar_resbio_input_IRR_MGMT.xml
    L2042.AgResBio_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBio_ag_irr_mgmt")
    L2042.AgResBioCurve_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBioCurve_ag_irr_mgmt")

    #PART 7: biochar_ag_demand.xml
    A221.A_PrimaryFuelCCoef <- get_data(all_data, "energy/A221.A_PrimaryFuelCCoef")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    #Update these files
    A221.bio_tech_prod <- get_data(all_data, "aglu/A221.bio_tech_prod")
    A221.bio_tech_land_alloc <- get_data(all_data, "aglu/A221.bio_tech_land_alloc")
    # And we also use L2252.LN5_MgdAllocation_crop, L2012.AgProduction_ag_irr_mgmt, L2062.AgCost_ag_irr_mgmt_adj

    #PART 8: biochar_ag_water_input_IRR_MGMT.xml
    L2072.AgCoef_IrrBphysWater_ag_mgmt <- get_data(all_data, "L2072.AgCoef_IrrBphysWater_ag_mgmt")
    L2072.AgCoef_IrrWaterWdraw_ag_mgmt <- get_data(all_data, "L2072.AgCoef_IrrWaterWdraw_ag_mgmt")
    L2072.AgCoef_IrrWaterCons_ag_mgmt <- get_data(all_data, "L2072.AgCoef_IrrWaterCons_ag_mgmt")
    L2072.AgCoef_RfdBphysWater_ag_mgmt <- get_data(all_data, "L2072.AgCoef_RfdBphysWater_ag_mgmt")


    # ===================================================
    test.region <- c("USA")

    #make a list of the technologies that can demand biochar
    biochar.techs <- A221.biochar_yield_ratio %>%
      mutate(treatment = gsub("t", "", treatment))

    biochar.techs <- unique(biochar.techs$technology)

    #make a table of tech options to be used for joining purposes
    biochar_techs <- A221.biochar_yield_ratio %>%
      mutate(treatment = gsub("t", "", treatment),
             add = "biochar") %>%
      unite("treatment", c(add, treatment)) %>%
      select(-c(year, yield_ratio)) %>%
      distinct()

    # We create the biochar demand in cropland.
    # Since we are creating new technologies that demand the biochar in model years, we need to create the structure in different
    # parts of the land system.

    #### PART 1: Historical land allocation (biochar_land_input_5_IRR_MGMT.xml) ####
    # Here we want to add to xml land_input_5 the new "technologies" that demand biochar
    # which includes all high techs (both rainfed and irrigated)
    # Even though they have no historical values, we need to include them in the structure, as it is the case
    # with purpose grown-biomass.

    ## L2221.LN5_HistMgdAllocation_crop_biochar
    L2252.LN5_HistMgdAllocation_crop %>%
      filter(region %in% test.region,
             LandLeaf %in% biochar.techs) %>%
      # Set historical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("LandLeaf" = "technology"), relationship = "many-to-many") %>%
      unite("LandLeaf", c(LandLeaf, treatment)) -> L2221.LN5_HistMgdAllocation_crop_biochar # OUTPUT


    ## L2221.LN5_MgdAllocation_crop_biochar
    L2252.LN5_MgdAllocation_crop %>%
      filter(region %in% test.region,
             LandLeaf %in% biochar.techs) %>%
      # Set historical values to 0, since there is no land allocated to techs that demand biochar (same as)
      mutate(allocation = 0) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("LandLeaf" = "technology"), relationship = "many-to-many") %>%
      unite("LandLeaf", c(LandLeaf, treatment)) -> L2221.LN5_MgdAllocation_crop_biochar # OUTPUT


    ## L2221.LN5_MgdCarbon_crop_biochar
    # For SOC, we want to relate the values from DayCent to those in GCAM.
    # From DayCent, we have SOC from all biochar application options
    # We want to process this to find the at which point/value each technology reached equilibrium
    # And change the value in GCAM by that percent
    # Some data manipulation must happen first to get this in the right processing format
    soc_biochar <- A221.biochar_soc %>%
      separate(technology, c("crop", "basin", "water", "fertilizer", "treatment"), sep = "_") %>%
      mutate(add = "biochar") %>%
      unite("treatment", c(add, treatment)) %>%
      # "B" will stand for biochar application
      rename(B = soc) %>%
      unite("LandLeaf", "crop", "basin", "water", "fertilizer", sep = "_") %>%
      filter(treatment != "biochar_0")

    soc_no_biochar <- A221.biochar_soc %>%
      separate(technology, c("crop", "basin", "water", "fertilizer", "treatment"), sep = "_") %>%
      mutate(add = "biochar") %>%
      unite("treatment", c(add, treatment)) %>%
      # "NB" will stand for no biochar application
      rename(NB = soc) %>%
      unite("LandLeaf", "crop", "basin", "water", "fertilizer", sep = "_") %>%
      filter(treatment == "biochar_0")

    biochar_soc <- soc_biochar %>%
      full_join(soc_no_biochar, by = c("LandLeaf", "year")) %>%
      select(-treatment.y) %>%
      unite(technology, "LandLeaf", "treatment.x", sep = "_")


    for (z in unique(biochar_soc$technology)) {
      biochar_soc %>%
        filter(technology == z) -> data

      model <- lm(B ~ year + I(year^2) + I(year^3), data = data)

      smooth_data <- as.data.frame(predict(model)) %>%
        rename( smooth_B = "predict(model)") %>%
        cbind( data )

      assign( paste0( "smooth_", z ), smooth_data )
    }

    smooth_dataframes <- lapply(ls(pattern="^smooth"), function(x) get(x))
    all_smooth_data <- rbindlist(smooth_dataframes) %>%
      unique()

    peak_value_year_all <- all_smooth_data %>%
      group_by(technology) %>%
      # find rate of change for cover crop based on smooth values
      # TODO: some techs always have a negative rate of change. In this case .... ?
      mutate(roc = (smooth_B - lag(smooth_B)) / (year - lag(year))) %>%
      filter(!is.na(roc), roc > 0) %>%
      # eq = the point at which rate of change is the closest to 0, without being negative
      # peak = the year at which SOC is the highest
      # If the peak is in 2100, we will use eq instead.
      mutate(eq = min(roc),
             peak = max(smooth_B)) %>%
      filter(eq == roc | peak == smooth_B)

    # Now we want to filter for entries where the year is 2100 and peak is equal to smooth_B
    # unless the year is 2100, peak == smooth_B, AND eq == roc. In this case, 2100 truly is the peak.
    # Anti-join them with the previous table- these are the entries where the peak is in 2100,
    # but roc is in a different year, in which case we want to use the roc year instead
    entries_to_remove <- peak_value_year_all %>%
      filter(year == 2100 & peak == smooth_B & eq != roc)

    # Make a table that does not include entries where the peak was in 2100
    peak_value_year <- peak_value_year_all %>%
      anti_join(entries_to_remove, by = c("smooth_B", "technology", "year", "B", "NB", "roc", "eq", "peak"))

    # Make a table that has counts for each technology. Those with two entries will be the techs we want to filter.
    # Join with our table that has all entries, aside from those with a peak in 2100
    # This output table has the peak / equilibrium year for each technology, and the SOC content at that point.
    soc_peak_kg_m2_df <- as.data.frame(table(peak_value_year$technology)) %>%
      rename(technology = Var1) %>%
      full_join(peak_value_year, by = "technology") %>%
      # from the entries with a frequency of 2, filter for the one where peak == smooth_B
      filter(Freq == 1 | Freq == 2 & peak == smooth_B) %>%
      select(c("year", "technology", "B", "NB")) %>%
      # Now we need to adjust the DayCent conventional SOC to reflect the values currently in GCAM, so our baselines match.
      # Convert DayCent units to GCAM
      # DayCent SOC is in MgC / Ha
      # GCAM SOC is in kgC / m2
      # 1 hectare = 10,000 m2
      # 1 Megagram = 1,000 kg
      # DayCent * 0.1 = GCAM
      mutate(NB = NB * 0.1,
             B = B * 0.1)

    # In some cases, the SOC rate of change is negative out to 2100.
    # Here, we will set the soil timescale to be 1000 (never reach equilibrium)
    # First, make a table of techs that have negative ROC
    all_techs_full <- biochar_techs %>%
      unite(tech, technology, treatment)

    negative_roc <- all_techs_full %>%
      # techs with a negative roc in every period will not be in soc_peak_kg_m2_df
      anti_join(soc_peak_kg_m2_df, by = c("tech" = "technology"))

    all_smooth_data_2100 <- all_smooth_data %>%
      filter(year == 2100)

    # Using the table of smooth data, pull these techs out for 2100
    techs_to_add <- negative_roc %>%
      left_join_error_no_match(all_smooth_data_2100, by = c("tech" = "technology")) %>%
      select(-smooth_B) %>%
      rename(technology = tech) %>%
      # Now we need to adjust the DayCent conventional SOC to reflect the values currently in GCAM, so our baselines match.
      # Convert DayCent units to GCAM
      # DayCent SOC is in MgC / Ha
      # GCAM SOC is in kgC / m2
      # 1 hectare = 10,000 m2
      # 1 Megagram = 1,000 kg
      # DayCent * 0.1 = GCAM
      mutate(NB = NB * 0.1,
             B = B * 0.1) %>%
      separate(technology, c("crop", "basin", "water", "fertilizer", "biochar", "treatment"), sep = "_") %>%
      unite("LandLeaf", "crop", "basin", "water", "fertilizer", sep = "_") %>%
      unite("treatment", "biochar", "treatment", sep = "_")

    # Convert from dataframe to tibble
    soc_peak_kg_m2 <- as_tibble(soc_peak_kg_m2_df) %>%
      separate(technology, c("crop", "basin", "water", "fertilizer", "biochar", "treatment"), sep = "_") %>%
      unite("LandLeaf", "crop", "basin", "water", "fertilizer", sep = "_") %>%
      unite("treatment", "biochar", "treatment", sep = "_")

    # Join with the table that has values from GCAM
    soc_adjusted_missing <- L2252.LN5_MgdCarbon_crop %>%
      filter(region %in% test.region,
             LandLeaf %in% biochar.techs) %>%
      full_join(soc_peak_kg_m2, by = "LandLeaf") %>%
      # remove NAs
      filter(!is.na(year)) %>%
      select(c("LandLeaf", "treatment", "soil.carbon.density", "year", "NB", "B")) %>%
      # calculate scaling factors for each technology to scale DayCent values to GCAM
      mutate(scaling_factor = soil.carbon.density / NB,
             NB = NB * scaling_factor,
             B = B * scaling_factor,
             # add a soil timescale column
             # since the model we get the SOC values from has a LUC in 2015, we deduct that year from the peak
             # year to get the amount of time it takes for each technology to reach equilibrium again
             soilTimeScale = (year - 2015)) %>%
      # These N values will become the biochar values
      select(c("LandLeaf", "treatment", "B", "soilTimeScale")) %>%
      # Here we create the biochar suffix to add to the technologies
      unite("LandLeaf", c("LandLeaf", "treatment"), sep="_")

    # Do this again for the other table, but manually set soil timescale to be 1000
    soc_adjusted <- L2252.LN5_MgdCarbon_crop %>%
      filter(region %in% test.region,
             LandLeaf %in% biochar.techs) %>%
      full_join(techs_to_add, by = "LandLeaf") %>%
      # remove NAs
      filter(!is.na(year)) %>%
      select(c("LandLeaf", "treatment", "soil.carbon.density", "year", "NB", "B")) %>%
      # calculate scaling factors for each technology to scale DayCent values to GCAM
      mutate(scaling_factor = soil.carbon.density / NB,
             NB = NB * scaling_factor,
             B = B * scaling_factor,
             # add a soil timescale column
             # since the model we get th SOC values from has a LUC in 2015, we deduct that year from the peak
             # year to get the amount of time it takes for each technology to reach equilibrium again
             soilTimeScale = 1000) %>%
      # These N values will become the biochar values
      select(c("LandLeaf", "treatment", "B", "soilTimeScale")) %>%
      # Here we create the biochar suffix to add to the technologies
      unite("LandLeaf", c("LandLeaf", "treatment"), sep="_") %>%
      # bind in the table with the techs that have a soil timescale
      bind_rows(soc_adjusted_missing)


    L2252.LN5_MgdCarbon_crop %>%
      filter(region %in% test.region,
             LandLeaf %in% biochar.techs) %>%
      # add in the table with no till parameters for SOC change
      #full_join(A221.biochar_soc, by=c("LandLeaf"="technology")) %>%
      # Here we create the biochar suffix to add to the technologies
      full_join(biochar_techs, by = c("LandLeaf" = "technology")) %>%
      unite("LandLeaf", c("LandLeaf", "treatment"), sep="_") %>%
      # Set historical values to 0, since there is no land allocated to techs that demand biochar (same as)
      #mutate(allocation = 0) %>%
      # attached the biochar values to replace the convention base values
      # TODO: change this back to LJENM when the rate of change issue is addressed
      left_join(soc_adjusted, by = "LandLeaf") %>%
      select(-soil.carbon.density) %>%
      # rename the cover crop values to be soil.carbon.density
      rename(soil.carbon.density = B) -> L2221.LN5_MgdCarbon_crop_biochar # OUTPUT

    ## L2221.LN5_LeafGhostShare
    # Since there is no historical land allocation for biochar technologies, we need to include this "ghost share" so that it exists in the future
    # otherwise the model does not know there should be any land for this
    A221.ghost_shrwt %>%
      write_to_all_regions(names = c("region", "year", "ghost.unnormalized.share"), GCAM_region_names) %>%
      filter(region %in% test.region) -> ghost_shrwt_region

    # We want to use the land allocation table for the crops and basins
    L2221.LN5_MgdAllocation_crop_biochar %>%
      # But we do not need neither the year nor the allocaiton amount
      select(-year, -allocation) %>%
      # Now we want to keep only 1 version of each combination, since we ar enot using year there are duplicates
      distinct() %>%
      # Now we join the table with the ghost share weights for different years
      left_join_error_no_match(ghost_shrwt_region, by = "region") %>%
      select(LEVEL2_DATA_NAMES$LN5_LeafGhostShare)-> L2221.LN5_LeafGhostShare # OUTPUT


    #=============================================================================================================================

    #### PART 2: Agricultural Production (biochar_ag_For_Past_bio_base_IRR_MGMT.xml) ####
    ## L2221.AgProduction_ag_irr_mgmt
    L2012.AgProduction_ag_irr_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Set historical production to 0, and tech shareweight to 0. Subsector should not change
      mutate(calOutputValue = 0,
             tech.share.weight = 0) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgProduction_ag_irr_mgmt # OUTPUT

    ## L2221.AgHAtoCL_irr_mgmt
    L2012.AgHAtoCL_irr_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgHAtoCL_irr_mgmt # OUTPUT

    ## L2221.AgYield_biochar_ref
    # We need to create this table with yields for biochar technologies, since there is no production nor land allocation,
    # therefore the model calculates the yield as 0. Biomass has a determined yield in the xml.
    # since new biochar technologies are the same as their equivalent high tech technologies,
    # we need to calculate the yields based on agricultural production and land allocation
    # and then manually include them
    L2012.AgProduction_ag_irr_mgmt %>%
      # filter for high techs only
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      left_join(L2252.LN5_MgdAllocation_crop, by = c("AgSupplySubsector" = "LandNode4", "AgProductionTechnology" = "LandLeaf","region", "year"))%>%
      # Here we  calculate the yields and we replace NAs with 0 yields
      mutate(yield = calOutputValue / allocation,
             yield = if_else(is.na(yield), 0, yield)) %>%
      select(LEVEL2_DATA_NAMES$AgYield) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgYield_biochar_ref # OUTPUT

    #=============================================================================================================================

    #### PART 3: Agricultural production costs (biochar_ag_cost_IRR_MGMT.xml) ####
    ## L2221.AgCost_ag_irr_mgmt
    L2052.AgCost_ag_irr_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgCost_ag_irr_mgmt # OUTPUT

    #=============================================================================================================================


    #### PART 4: Agricultural production Change (biochar_ag_prodchange_ref_IRR_MGMT.xml) ####
    # Here we want to calculate the yield changes from biochar application
    # This yield change is based on crop, water basin, irrigation, and fertilizer
    # These are the steps we follow to calculate the yields, considering existing ag prod change
    # 1. Pre-processed data from DayCent model to get 5 year average ratios from no biochar to biochar
    # 2. Create a yield index for biochar using existing AgProdChange values, assuming biochar and no biochar have the same yield in 2025 (first year biochar is available)
    # 3. Calculate new yield change values that combine existing yield change from GCAM + biochar yield changes

    # 1. Exogenous
    # 2. Create a yield index from GCAM values.
    # First, create a table that has all target technologies, with a yield index of 1 for 2025.
    #TODO: help here. Cover crop method or old biochar method?
    L2052.AgProdChange_ag_irr_ref %>%
      #Filter for regions we are analyzing (either all or some specific one), and for high technologies
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs,
             year == 2020) %>%
      mutate(year = 2015,
        # AgProdChange here is actually the yield index. Naming it this for binding purposes
        AgProdChange = 1) %>%
      bind_rows(L2052.AgProdChange_ag_irr_ref) %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      arrange(AgProductionTechnology) %>%
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>%
      # yield equation: next period yield = ( next period AgProdChange + 1 )^5 * previous period yield
      mutate(yield_index = 1) -> yield_index

    for (z in unique(yield_index$year)) {
      yield_index %>%
        mutate(yield_index = (AgProdChange + 1)^5 * lag(yield_index)) %>%
        replace_na(list(yield_index = 1)) -> yield_index
    }

    # Add the full suite of treatments to the yield index dataframe
    yield_index_full <- yield_index %>%
      full_join(biochar_techs, by = c("AgProductionTechnology" = "technology")) %>%
      unite(AgProductionTechnology, AgProductionTechnology, treatment, sep = "_")

    # 3. Calculate new yield changes
    # combine the no biochar yield index with the table containing no biochar to biochar ratios from Daycent
    A221.biochar_yield_ratio %>%
      mutate(treatment = gsub("t", "", treatment),
             add = "biochar") %>%
      unite("treatment", c(add, treatment)) %>%
      distinct() %>%
      # Comment out the following chunk if you want yield change from DayCent with 5 year variations
      # Otherwise, we want a smooth yield change over time, so that no matter when No Till is implemented
      # it receives the same yield change. Eventually we want yield change associated with technology age.
      filter(year == 2100) %>%
      unite("technology", "technology", "treatment", sep = "_") %>%
      complete(nesting(technology), year = c(MODEL_YEARS)) %>%
      filter(year > max(MODEL_BASE_YEARS) | year == max(MODEL_BASE_YEARS)) %>%
      mutate(yield_ratio = if_else(year == 2015, 1, yield_ratio)) %>%
      group_by(technology) %>%
      mutate(yield_ratio = approx_fun(year, yield_ratio, rule = 2)) %>%
      ungroup() %>%
      filter(year > max(MODEL_BASE_YEARS)) %>%
      separate(technology, c("crop", "basin", "water", "fertilizer", "treatment", "treatment1"), sep = "_") %>%
      unite("LandLeaf", "crop", "basin", "water", "fertilizer", sep = "_") %>%
      unite("treatment", "treatment", "treatment1", sep = "_") %>%
      # this is where the "comment out" chunk ends
      left_join_error_no_match(yield_index, by = c("LandLeaf" = "AgProductionTechnology", "year")) %>%
      unite(AgProductionTechnology, LandLeaf, treatment, sep = "_") %>%
      select(-AgProdChange) %>%
      # now we want to change the yield index to reflect changes from no biochar to biochar
      mutate(yield_ratio = yield_ratio - 1,
             yield_index = yield_index + yield_ratio) %>%
      # add 2020 yield index back in
      bind_rows(yield_index_full %>% filter(year == 2015) %>% select(-AgProdChange)) %>%
      arrange(AgProductionTechnology, year) %>%
      select(-yield_ratio) %>%
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>%
      # calculate new AgProdChange values
      mutate(AgProdChange = ((yield_index / lag(yield_index))^0.2) - 1) %>%
      ungroup() %>%
      filter(year > 2015) %>%
      select(LEVEL2_DATA_NAMES$AgProdChange) -> L2221.AgProdChange_ag_irr_ref # OUTPUT


    # =============================================================================================================


    #### PART 4: Agricultural production Change (biochar_ag_prodchange_ref_IRR_MGMT.xml) ####
    # Here we want to clauclate the yield increases from biochar application
    # This yield improvement is based on tropical/temeprate regions, and between irrigated and rainfed
    # These are the steps we follow to calculate the increase yields, considering existing ag prod change
    # 1. Share of total biochar land  over total land:
    #We first assume that biochar is applied linearly from 2020-2100, in a period of 75 years
    #(1.25% per year, or 6.25% every 5 years)
    # 2. Average yield on biochar land:
    #Then we apply to that percentage of land the yield increase of applying the biochar
    #(we multiply both; add 1; multiply by 1)
    # 3. Cumulative base yield improvement:
    #Then we calculate the cumulative base yield improvement
    #(add 1 to the base annual yield improvement, multiply by previous cumulative base yield improvement
    #and then raise to 5)
    # 4. Biochar yield cumulative improvement:
    #Then we multiply the average yield increase (step 2) times the cumulative base yield improvement (step 3)
    # 5. Annual biochar yield improvement:
    #Finally, we turn this parameter into an annual yield improvement.


    ## L2221.AgProdChange_ag_irr_ref
    L2052.AgProdChange_ag_irr_ref %>%
      #Filter for regions we are analyzing (either all or some specific one), and for high technologies
      filter(region %in% test.region,
             grepl("_hi", AgProductionTechnology)) %>%
      # Here we create the biochar suffix to add to the technologies
      mutate(suffix = "biochar") %>%
      unite("AgProductionTechnology", c("AgProductionTechnology", "suffix"), sep="_") %>%
      # Now get column to distinguish irrigated vs rainfed
      mutate(water = if_else(grepl("IRR", AgProductionTechnology), "irrigated", "rainfed")) %>%
      # Add climate zones tables
      left_join_error_no_match(GCAM_region_names_climate_zone, by = "region") %>%
      # Now add the table with the yield improvements as percentages, distinguishing from irrigated-rainfed, and tropical/temperate
      left_join_error_no_match(A221.biochar_yield_increase, by = c("water", "zone")) %>%
      # (Step 1): Finally, add the table with percentage of biochar land per period, which has the information for step 1
      left_join_error_no_match(A221.biochar_land_alloc, by = "year") %>%
      # There are 3 specifc GLUs that have an AgPordChange parameter of -1, which would affect our following calculaitons
      # So we just change those three from -1 to -0.9999
      # The three GLUs are: Wheat_ShebJubR_IRR_hi_biochar; OtherGrain_AfrCstNW_IRR_hi_biochar; OtherGrain_LChad_IRR_hi_biochar
      mutate(AgProdChange = if_else(AgProdChange == -1, -0.9999, AgProdChange)) %>%
      # Now everything is ready, perform calculations
      # Step 2: Get the average yield on biochar land
      # considering the percentage of land with biochar, and the yield increase of appliying the biochar
      mutate(av_yield_biochar = 1*(1 + percentage * yield_increase)) %>%
      # Step 3: Get cumulative base yield improvements (without biochar)
      # Since this calculation is recursive, we need to set a for loop
      # Each computaiton is dependent on the computaiton from teh previous year (within each basin and technology and crop)
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, water, GCAM_region_ID, zone) %>%
      mutate(cum_base_yield_improv = 1) -> temp

    for (y in unique(temp$year)) {
      temp %>%
        mutate(cum_base_yield_improv = lag(cum_base_yield_improv)*(1 + AgProdChange)^5) %>%
        replace_na(list(cum_base_yield_improv = 1))->temp
    }

    # Now continue with computations
    temp %>%
      # Step 4: Get cumulative biochar yield improvement
      mutate(cum_biochar_yield_improv = cum_base_yield_improv * av_yield_biochar) %>%
      # Step 5: Get annual biochar yield improvement
      #We need to group first so that the divisin happens wihtin the same country, basin, crop, tech, and compared to previous year
      #and not across different regions.
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, water, GCAM_region_ID, zone) %>%
      mutate(annual_biochar_yield_improv = ((cum_biochar_yield_improv / lag(cum_biochar_yield_improv))^(1/5))-1) %>%
      ungroup() %>%
      # Now replace NA in 2020 with the same ag prod change from the default technology, since the yield increase comes after the application
      mutate(annual_biochar_yield_improv = if_else(year == 2020 & is.na(annual_biochar_yield_improv), AgProdChange, annual_biochar_yield_improv)) %>%
      # Now reanme old ag prod change, and keep the new one
      rename(old_AgProdChange = AgProdChange,
             AgProdChange = annual_biochar_yield_improv) %>%
      select(LEVEL2_DATA_NAMES$AgProdChange)-> L2221.AgProdChange_ag_irr_ref_BEFORE # OUTPUT

    #=============================================================================================================================

    #### PART 5: Fertilizer information (biochar_ag_Fert_IRR_MGMT.xml) ####
    ## L2221.AgCoef_Fert_ag_irr_mgmt
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgCoef_Fert_ag_irr_mgmt # OUTPUT

    ## L2221.AgCost_ag_irr_mgmt_adj
    L2062.AgCost_ag_irr_mgmt_adj %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgCost_ag_irr_mgmt_adj # OUTPUT

    #=============================================================================================================================

    #### PART 6: Agriculture residues from cropland (biochar_resbio_input_IRR_MGMT.xml) ####
    ## L2221.AgResBio_ag_irr_mgmt
    L2042.AgResBio_ag_irr_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) %>%
      mutate(residue.biomass.production = "biomassHerbaceous") -> L2221.AgResBio_ag_irr_mgmt # OUTPUT

    ## L2221.AgResBioCurve_ag_irr_mgmt
    L2042.AgResBioCurve_ag_irr_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) %>%
      mutate(residue.biomass.production = "biomassHerbaceous") -> L2221.AgResBioCurve_ag_irr_mgmt # OUTPUT

    #=============================================================================================================================

    #### PART 7: Create Agriculture Demand for Biochar (biochar_ag_demand.xml) ####
    # This part is used to create the demand for biochar in agricultural crops
    # We follow a similar approach to fertilizer input to agriculture
    # Based on output table L142.ag_Fert_IO_R_C_Y_GLU from LB142.ag_Fert_IO_R_C_Y_GLU fertilizer is calculated as:
    # Input-output coefficients for each crop are first calculated as fertilizer demands divided by agriculture production in the base year

    # We use similar approach with biochar demand (assumed) divided by agriculture production in the base year
    # Steps:
    # 1. First filter for base year, and for high tech crops
    # 2. from total land allocation get total biochar application (assuming certain biochar application rate from literature)
    # 3. We assume that the total from step 2 is absolute maximum biochar application. To get the rate per year we divide total by 80
    #(which are the years modeled, since biochar application starts in 2020)
    # 4. Then that biochar per year application (in Mt) is divided by the agricultural production (in Mt) to get the coefficient

    ## L2221.AgCoef_Biochar_ag_irr_mgmt
    # Estimate coefficients for biochar demand
    # Get the total amount of land per crop, basin and tech for 2015 only
    # But we create a new ag sector that competes with the other ones.
    L2252.LN5_MgdAllocation_crop %>%
      filter(region %in% test.region,
             year == MODEL_FINAL_BASE_YEAR,
             LandLeaf %in% biochar.techs) %>%
      # TODO: Need help with this for different rates
      # Units are in thousand km2 and we need them in hectares
      # Then we assume certain biochar application rate named at the constant "biochar.rate"
      # This is expected to be the absolute maximum (total amount of land times "X" tons per hectare)
      # Then divide by the remaining modeled years to get a rate (from constant "assumed.biochar.years")
      # This will give us the demand in a given year for biochar
      mutate(allocation_ha = allocation * 1000 * km2.to.ha,
             # 10^6 since we need units in megatons, and rate is in tons
             max.bicohar_MT = (allocation_ha * biochar.rate) / 10^6,
             biochar.year_MT = max.bicohar_MT / assumed.biochar.years,
             #Get allocation in kg per hectare, since units are in kg of biochar per kg of crop
             biochar.year_kg = biochar.year_MT * 10^9) %>%
      rename("AgSupplySubsector" = "LandNode4", "AgProductionTechnology" =  "LandLeaf") %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2252.LN5_MgdAllocation_crop_biochar

    # Now prepare the table with the agricultural production
    L2012.AgProduction_ag_irr_mgmt %>%
      filter(region %in% test.region,
             year == MODEL_FINAL_BASE_YEAR,
             AgProductionTechnology %in% biochar.techs) %>%
      # Since units are in Mt we convert them to kg
      mutate(prod_kg = calOutputValue * 10^9) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2012.AgProduction_ag_irr_mgmt_biochar

    # Merge tables
    L2252.LN5_MgdAllocation_crop_biochar %>%
      # Join table with agriculture production by crops
      left_join_error_no_match(L2012.AgProduction_ag_irr_mgmt_biochar, by = c("region", "year", "AgSupplySubsector", "AgProductionTechnology")) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, biochar.year_MT, calOutputValue,
             biochar.year_kg, prod_kg) %>%
      # Now compute coefficient as biochar.year_MT divided total crop production (as with fertilzier)
      # This is Megatons of biochar / Mt of crop produced in that land; or kf of biochar / kg of crop production
      mutate(minicam.energy.input = "biochar",
             coefficient = biochar.year_MT / calOutputValue,
             coefficient_kg = biochar.year_kg / prod_kg) %>%
      # Complete other years
      complete(nesting(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology,
                       minicam.energy.input, coefficient), year = c(year, MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$AgCoef)-> L2221.AgCoef_Biochar_ag_irr_mgmt # OUTPUT

    ## L2221.AgCost_Biochar_irr_mgmt_adj
    L2062.AgCost_ag_irr_mgmt_adj %>% # We use the adjsuted costs of applying fertilizer here
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) %>%
      # filter out non existent crops
      semi_join(L2221.AgCoef_Biochar_ag_irr_mgmt, by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      select(LEVEL2_DATA_NAMES$AgCost) -> L2221.AgCost_Biochar_irr_mgmt_adj # OUTPUT


    ## L2221.CarbonCoef
    A221.A_PrimaryFuelCCoef %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["CarbonCoef"]]), GCAM_region_names = GCAM_region_names, has_traded = TRUE) %>%
      filter(region %in% test.region) -> L2221.CarbonCoef # OUTPUT

    #=============================================================================================================================

    #### PART 8: Create Water demand for newly created technologies (biochar_ag_water_input_IRR_MGMT.xml)####
    # Here we create the water demands for the new technologies, so that they do not have any missing information
    ## L2221.AgCoef_IrrBphysWater_ag_mgmt
    L2072.AgCoef_IrrBphysWater_ag_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      # Irrigated techs only (RFD have NA here because they do not exist in the original dataframe)
      filter(grepl("IRR", AgProductionTechnology)) %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgCoef_IrrBphysWater_ag_mgmt # OUTPUT

    ## L2221.AgCoef_IrrWaterWdraw_ag_mgmt
    L2072.AgCoef_IrrWaterWdraw_ag_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      # Irrigated techs only (RFD have NA here because they do not exist in the original dataframe)
      filter(grepl("IRR", AgProductionTechnology)) %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgCoef_IrrWaterWdraw_ag_mgmt # OUTPUT

    ## L2221.AgCoef_IrrWaterCons_ag_mgmt
    L2072.AgCoef_IrrWaterCons_ag_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      # Irrigated techs only (RFD have NA here because they do not exist in the original dataframe)
      filter(grepl("IRR", AgProductionTechnology)) %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgCoef_IrrWaterCons_ag_mgmt # OUTPUT

    ## L2221.AgCoef_RfdBphysWater_ag_mgmt
    L2072.AgCoef_RfdBphysWater_ag_mgmt %>%
      filter(region %in% test.region,
             AgProductionTechnology %in% biochar.techs) %>%
      # Add in technology suffixes
      full_join(biochar_techs, by=c("AgProductionTechnology" = "technology"), relationship = "many-to-many") %>%
      # Rainfed techs only (IRR have NA here because they do not exist in the original dataframe)
      filter(grepl("RFD", AgProductionTechnology)) %>%
      unite("AgProductionTechnology", c(AgProductionTechnology, treatment)) -> L2221.AgCoef_RfdBphysWater_ag_mgmt # OUTPUT

    # ====================================================================================================================================

    # Produce outputs
    #1. biochar_land_input_5_IRR_MGMT.xml
    L2221.LN5_HistMgdAllocation_crop_biochar %>%
      add_title("Historical land allocaiton biochar techs (prior 1975)") %>%
      add_units("Mt") %>%
      add_comments("Written to all high tech regions, historical values are 0") %>%
      add_legacy_name("L2221.LN5_HistMgdAllocation_crop_biochar") %>%
      add_precursors("L2252.LN5_HistMgdAllocation_crop") ->
      L2221.LN5_HistMgdAllocation_crop_biochar

    L2221.LN5_MgdAllocation_crop_biochar %>%
      add_title("Historical land allocaiton biochar techs (1975 - 2015)") %>%
      add_units("Mt") %>%
      add_comments("Written to all high tech regions, historical values are 0") %>%
      add_legacy_name("L2221.LN5_MgdAllocation_crop_biochar") %>%
      add_precursors("L2252.LN5_MgdAllocation_crop") ->
      L2221.LN5_MgdAllocation_crop_biochar

    L2221.LN5_MgdCarbon_crop_biochar %>%
      add_title("Carbon informaiton for biochar techs") %>%
      add_units("various") %>%
      add_comments("Written to all high tech regions with exact info from L2252.LN5_MgdCarbon_crop") %>%
      add_legacy_name("L2221.LN5_MgdCarbon_crop_biochar") %>%
      add_precursors("L2252.LN5_MgdCarbon_crop") ->
      L2221.LN5_MgdCarbon_crop_biochar

    L2221.LN5_LeafGhostShare %>%
      add_title("Ghost share for new biochar land") %>%
      add_units("various") %>%
      add_comments("Ghost shares for biochar land types; otherwise there is not deployment") %>%
      add_legacy_name("L2221.LN5_LeafGhostShare") %>%
      add_precursors("energy/A221.ghost_shrwt", "L2252.LN5_MgdAllocation_crop") ->
      L2221.LN5_LeafGhostShare

    #2. biochar_ag_For_Past_bio_base_IRR_MGMT.xml
    L2221.AgProduction_ag_irr_mgmt %>%
      add_title("Biochar techs calibrated production (0)") %>%
      add_units("Mt") %>%
      add_comments("Calibrated production for biochar crop techs (0)") %>%
      add_legacy_name("L2221.AgProduction_ag_irr_mgmt") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt") ->
      L2221.AgProduction_ag_irr_mgmt

    L2221.AgHAtoCL_irr_mgmt %>%
      add_title("Harvests per yer for biochar techs") %>%
      add_units("Unitless") %>%
      add_comments("Number of harvests per year for new biochar technologies, same as L2221.AgHAtoCL_irr_mgmt") %>%
      add_legacy_name("L2221.AgHAtoCL_irr_mgmt") %>%
      add_precursors("L2012.AgHAtoCL_irr_mgmt") ->
      L2221.AgHAtoCL_irr_mgmt

    L2221.AgYield_biochar_ref %>%
      add_title("Biochar Technology Yields") %>%
      add_units("t/m2???") %>%
      add_comments("Biochar technology yields, since there is no historical production we need to determine them, as with biomass") %>%
      add_legacy_name("L2221.AgYield_biochar_ref") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt", "L2252.LN5_MgdAllocation_crop") ->
      L2221.AgYield_biochar_ref

    #3. biochar_ag_cost_IRR_MGMT.xml
    L2221.AgCost_ag_irr_mgmt %>%
      add_title("Costs for agriculture techs for biochar") %>%
      add_units("1975$ per kg") %>%
      add_comments("Number of harvests per year for new biochar technologies, same as L2221.AgCost_ag_irr_mgmt") %>%
      add_legacy_name("L2221.AgCost_ag_irr_mgmt") %>%
      add_precursors("L2052.AgCost_ag_irr_mgmt") ->
      L2221.AgCost_ag_irr_mgmt

    #4. ag_prodchange_ref_IRR_MGMT.xml
    L2221.AgProdChange_ag_irr_ref %>%
      add_title("Agricultural Production Change for biochar") %>%
      add_units("Unitless") %>%
      add_comments("Agricultural production change for new biochar technologies, same as L2221.AgProdChange_ag_irr_ref") %>%
      add_legacy_name("L2221.AgProdChange_ag_irr_ref") %>%
      add_precursors("L2052.AgProdChange_ag_irr_ref",
                     "common/GCAM_region_names_climate_zone",
                     "aglu/A221.biochar_yield_increase",
                     "aglu/A221.biochar_land_alloc") ->
      L2221.AgProdChange_ag_irr_ref

    #5. biochar_ag_Fert_IRR_MGMT.xml
    L2221.AgCoef_Fert_ag_irr_mgmt %>%
      add_title("Fertilizer inputs for biochar technologies") %>%
      add_units("Unitless") %>%
      add_comments("Fertilizer inputs for new biochar techs, same as L2062.AgCoef_Fert_ag_irr_mgmt, but could change to reflect improvements from biochar") %>%
      add_legacy_name("L2221.AgCoef_Fert_ag_irr_mgmt") %>%
      add_precursors("L2062.AgCoef_Fert_ag_irr_mgmt") ->
      L2221.AgCoef_Fert_ag_irr_mgmt

    L2221.AgCost_ag_irr_mgmt_adj %>%
      add_title("Fertilizer costs for biochar land technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Fertilizer inputs for new biochar techs, same as L2062.AgCost_ag_irr_mgmt_adj") %>%
      add_legacy_name("L2221.AgCost_ag_irr_mgmt_adj") %>%
      add_precursors("L2062.AgCost_ag_irr_mgmt_adj") ->
      L2221.AgCost_ag_irr_mgmt_adj

    #6. biochar_resbio_input_IRR_MGMT.xml
    L2221.AgResBio_ag_irr_mgmt %>%
      add_title("Residues from crops for biochar land technologies") %>%
      add_units("Varies") %>%
      add_comments("Crop residues for new biochar techs same as L2042.AgResBio_ag_irr_mgmt") %>%
      add_legacy_name("L2221.AgResBio_ag_irr_mgmt") %>%
      add_precursors("L2042.AgResBio_ag_irr_mgmt") ->
      L2221.AgResBio_ag_irr_mgmt

    L2221.AgResBioCurve_ag_irr_mgmt %>%
      add_title("Residues resource curves for biochar land technologies") %>%
      add_units("Fraction harvested") %>%
      add_comments("Crop residues curves for new biochar techs same as L2042.AgResBioCurve_ag_irr_mgmt") %>%
      add_legacy_name("L2221.AgResBioCurve_ag_irr_mgmt") %>%
      add_precursors("L2042.AgResBioCurve_ag_irr_mgmt") ->
      L2221.AgResBioCurve_ag_irr_mgmt

    #7. biochar_ag_demand.xml
    L2221.AgCoef_Biochar_ag_irr_mgmt %>%
      add_title("Biochar inputs coefficients") %>%
      add_units("Unitless (Mt of biochar per Mt of crop, or kg per kg)") %>%
      add_comments("Written to all high tech regions from L2012.AgProduction_ag_irr_mgmt; assumed applicaiton rate for biochar") %>%
      add_legacy_name("L2221.AgCoef_Biochar_ag_irr_mgmt") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt",
                     "L2252.LN5_MgdAllocation_crop") ->
      L2221.AgCoef_Biochar_ag_irr_mgmt

    L2221.AgCost_Biochar_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for biochar in high tech crops") %>%
      add_units("1975$ per kg") %>%
      add_comments("We use the same cost of pplying fertilizer") %>%
      add_legacy_name("L2221.AgCost_Biochar_irr_mgmt_adj") %>%
      add_precursors("L2062.AgCost_ag_irr_mgmt_adj") ->
      L2221.AgCost_Biochar_irr_mgmt_adj

    L2221.CarbonCoef %>%
      #add_title("Biochar carbon coefficient") %>%
      add_units("kgC/GJ") %>%
      add_comments("Biochar coefficient is 0 to avoid emissions from cropland.") %>%
      add_legacy_name("L2221.CarbonCoef") %>%
      add_precursors("common/GCAM_region_names", "energy/A221.A_PrimaryFuelCCoef") ->
      L2221.CarbonCoef

    #8. biochar_ag_water_input_IRR_MGMT.xml
    L2221.AgCoef_IrrBphysWater_ag_mgmt%>%
      add_title("Biophysical water consumption IO coefficients by region / irrigated crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_IrrBphysWater_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrBphysWater_ag_mgmt") ->
      L2221.AgCoef_IrrBphysWater_ag_mgmt

    L2221.AgCoef_IrrWaterWdraw_ag_mgmt %>%
      add_title("Irrigation water withdrawals IO coefficients by region / irrigated crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("Withdrawals coefs are calculated as consumption coefs divided by irrigation efficiency") %>%
      add_comments("Set a floor on profit and adjust the coefs to ensure the profit floor is met") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_IrrWaterWdraw_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrWaterWdraw_ag_mgmt") ->
      L2221.AgCoef_IrrWaterWdraw_ag_mgmt

    L2221.AgCoef_IrrWaterCons_ag_mgmt %>%
      add_title("Irrigation water consumption IO coefficients by region / dedicated bioenergy crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("Multiply biophysical water coefs of bioenergy crops with the average % of blue water for primary crops by region / GLU") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_IrrWaterCons_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_IrrWaterCons_ag_mgmt") ->
      L2221.AgCoef_IrrWaterCons_ag_mgmt

    L2221.AgCoef_RfdBphysWater_ag_mgmt %>%
      add_title("Biophysical water consumption IO coefficients by region / rainfed crop / year / GLU / management level for new biochar techs") %>%
      add_units("km3/Mt") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_comments("We carry values from previous original table for new biochar technologies") %>%
      add_legacy_name("L2221.AgCoef_RfdBphysWater_ag_mgmt") %>%
      add_precursors("L2072.AgCoef_RfdBphysWater_ag_mgmt") ->
      L2221.AgCoef_RfdBphysWater_ag_mgmt


    return_data(#1. biochar_land_input_5_IRR_MGMT.xml
      L2221.LN5_HistMgdAllocation_crop_biochar,
      L2221.LN5_MgdAllocation_crop_biochar,
      L2221.LN5_MgdCarbon_crop_biochar,
      L2221.LN5_LeafGhostShare,
      #2. biochar_ag_For_Past_bio_base_IRR_MGMT.xml
      L2221.AgProduction_ag_irr_mgmt, L2221.AgHAtoCL_irr_mgmt,
      L2221.AgYield_biochar_ref,
      #3. biochar_ag_cost_IRR_MGMT.xml
      L2221.AgCost_ag_irr_mgmt,
      #4. ag_prodchange_ref_IRR_MGMT.xml
      L2221.AgProdChange_ag_irr_ref,
      #5. biochar_ag_Fert_IRR_MGMT.xml
      L2221.AgCoef_Fert_ag_irr_mgmt,
      L2221.AgCost_ag_irr_mgmt_adj,
      #6. biochar_resbio_input_IRR_MGMT.xml
      L2221.AgResBio_ag_irr_mgmt, L2221.AgResBioCurve_ag_irr_mgmt,
      #7. biochar_ag_demand.xml
      L2221.AgCoef_Biochar_ag_irr_mgmt, L2221.AgCost_Biochar_irr_mgmt_adj,
      L2221.CarbonCoef,
      #8. biochar_ag_water_input_IRR_MGMT.xml
      L2221.AgCoef_IrrBphysWater_ag_mgmt,
      L2221.AgCoef_IrrWaterWdraw_ag_mgmt,
      L2221.AgCoef_IrrWaterCons_ag_mgmt,
      L2221.AgCoef_RfdBphysWater_ag_mgmt)
  } else {
    stop("Unknown command")
  }
}
