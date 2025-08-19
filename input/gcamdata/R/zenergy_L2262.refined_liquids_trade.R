#' module_energy_L2262.refined_liquids_trade
#'
#' Model input for regional and (globally) traded refined liquids
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L2262.Supplysector_refinedLiquids_tra},
#'   \code{L2262.SectorUseTrialMarket_refinedLiquids_tra}, \code{L2262.SubsectorAll_refinedLiquids_tra}, \code{L2262.TechShrwt_refinedLiquids_tra},
#'   \code{L2262.TechCost_refinedLiquids_tra}, \code{L2262.TechCoef_refinedLiquids_tra}, \code{L2262.Production_refinedLiquids_tra}, \code{L2262.Supplysector_refinedLiquids_reg},
#'   \code{L2262.SubsectorAll_refinedLiquids_reg}, \code{L2262.TechShrwt_refinedLiquids_reg}, \code{L2262.TechCoef_refinedLiquids_reg}, \code{L2262.Production_refinedLiquids_reg_imp},
#'   \code{L2262.Production_refinedLiquids_reg_dom}.
#' @details Build datasets for ssp4 agricultural trade: food and nonfood trade coefficients, feed trade
#' coefficients, restricted agricultural trade, and trade regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tibble tibble
#' @author GPK/JEH March 2019
module_energy_L2262.refined_liquids_trade <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/A_regions",
      FILE = "energy/A262.refinedLiquidsRegionalSector",
      FILE = "energy/A262.refinedLiquidsRegionalSubsector",
      FILE = "energy/A262.refinedLiquidsRegionalTechnology",
      FILE = "energy/A262.refinedLiquidsTradedSector",
      FILE = "energy/A262.refinedLiquidsTradedSubsector",
      FILE = "energy/A262.refinedLiquidsTradedTechnology",
      FILE = "energy/bio_feed_mapping",
      "L121.share_R_TPES_biofuel_tech",
      "L122.out_EJ_R_refining_F_Yh",
      "L1262.refinedOil_GrossTrade_EJ_R_C_Y",
      "L1262.Biofuel_GrossTrade_EJ_R_C_Y",
      "L222.biofuel_type_filter_R")

  MODULE_OUTPUTS <-
    c("L2262.Supplysector_refinedLiquids_tra",
      "L2262.SectorUseTrialMarket_refinedLiquids_tra",
      "L2262.SubsectorAll_refinedLiquids_tra",
      "L2262.TechShrwt_refinedLiquids_tra",
      "L2262.TechCost_refinedLiquids_tra",
      "L2262.TechCoef_refinedLiquids_tra",
      "L2262.Production_refinedLiquids_tra",
      "L2262.Supplysector_refinedLiquids_reg",
      "L2262.SubsectorAll_refinedLiquids_reg",
      "L2262.TechShrwt_refinedLiquids_reg",
      "L2262.TechCost_refinedLiquids_reg",
      "L2262.TechCoef_refinedLiquids_reg",
      "L2262.Production_refinedLiquids_reg_imp",
      "L2262.Production_refinedLiquids_reg_dom")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {


   all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_commodity <- export <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      import <- Prod_EJ <- gross_exports <- gross_imports <- net_trade <- NULL # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Downscale the biofuel export volumes into the specific fuel types using L121.share_R_TPES_biofuel_tech
    # and A_regions

    L2262.share_R_TPES_biofuel_tech <- A_regions %>%
      select(GCAM_region_ID, ethanol, biodiesel) %>%
      gather(key = "Biofuel", value = "technology", -GCAM_region_ID) %>%
      full_join(select(L121.share_R_TPES_biofuel_tech, -GCAM_commodity),
                by = c("GCAM_region_ID", "Biofuel", "technology")) %>%
      replace_na(list(share = 1))

    # Use an expanding join to disaggregate the fuel commodities (ethanol, biodieseel)
    # to their feedstocks (e.g., corn ethanol, soybean biodiesel, etc), and multiply exports by
    # the shares in order to compute exports by technology ("sector")
    L2262.Biofuel_Exports_EJ_R_C_Y <- L1262.Biofuel_GrossTrade_EJ_R_C_Y %>%
      left_join(L2262.share_R_TPES_biofuel_tech, by = c("GCAM_region_ID", GCAM_commodity = "Biofuel")) %>%
      mutate(gross_exports = gross_exports * share) %>%
      select(GCAM_region_ID, GCAM_commodity, sector = technology, year, gross_exports)

    # Aggregate through regions to compute the feedstock shares of imports in each region
    L2262.Biofuel_Imports_FeedstockShares <- L2262.Biofuel_Exports_EJ_R_C_Y %>%
      group_by(GCAM_commodity, sector, year) %>%
      summarise(gross_exports = sum(gross_exports)) %>%
      ungroup() %>%
      group_by(GCAM_commodity, year) %>%
      mutate(import_share = gross_exports / sum(gross_exports)) %>%
      ungroup() %>%
      replace_na(list(import_share = 0)) %>%
      select(GCAM_commodity, sector, year, import_share)

    L2262.Biofuel_Imports_EJ_R_C_Y <- L1262.Biofuel_GrossTrade_EJ_R_C_Y %>%
      left_join(L2262.Biofuel_Imports_FeedstockShares, by = c("GCAM_commodity", "year")) %>%
      mutate(gross_imports = gross_imports * import_share) %>%
      select(GCAM_region_ID, GCAM_commodity, sector, year, gross_imports)

    L2262.Biofuel_GrossTrade_EJ_R_C_Y_0 <-
      full_join(L2262.Biofuel_Exports_EJ_R_C_Y,
                L2262.Biofuel_Imports_EJ_R_C_Y,
                by = c("GCAM_region_ID", "GCAM_commodity", "sector", "year")) %>%
      replace_na(list(gross_exports = 0, gross_imports = 0))

    # GPK 8/19/2025 use production as a cap on exports, and adjust trade accordingly
    L2262.Biofuel_GrossTrade_EJ_R_C_Y_0 %>%
      left_join(L122.out_EJ_R_refining_F_Yh %>%
                  rename(prod = value),
                by = c("GCAM_region_ID", "sector", "year")) %>%
      replace_na(list(prod = 0)) %>%
      mutate(gross_exp_adj = if_else(gross_exports > prod, prod, gross_exports)) ->
      L2262.Biofuel_GrossTrade_EJ_R_C_Y_1

    L2262.Biofuel_GrossTrade_EJ_R_C_Y_1 %>%
      mutate(export_revision = gross_exp_adj - gross_exports) %>%
      group_by(GCAM_commodity, sector, year) %>%
      summarize(export_revision = sum(export_revision),
                import_total = sum(gross_imports)) %>%
      ungroup() %>%
      mutate(import_scaler = (import_total + export_revision) / import_total) %>%
      select(GCAM_commodity, sector, year, import_scaler) %>%
      drop_na() ->
      L2262.Biofuel_ImportScalers_C_Y

    L2262.Biofuel_GrossTrade_EJ_R_C_Y_1 %>%
      left_join(L2262.Biofuel_ImportScalers_C_Y, by = c("GCAM_commodity", "sector", "year")) %>%
      mutate(gross_exports = gross_exp_adj,
             gross_imports = if_else(is.na(import_scaler), gross_imports, gross_imports * import_scaler),
             net_trade = gross_exports - gross_imports) %>%
      select(GCAM_region_ID, GCAM_commodity, sector, year, gross_exports, gross_imports, net_trade) ->
      L2262.Biofuel_GrossTrade_EJ_R_C_Y

    # Modify the oil refining gross trade table to have similar format as the biofuels, and combine
    L2262.refinedLiquids_GrossTrade_EJ_R_C_Y <- L1262.refinedOil_GrossTrade_EJ_R_C_Y %>%
      mutate(sector = "oil refining") %>%
      select(GCAM_region_ID, GCAM_commodity, sector, year, gross_exports, gross_imports, net_trade) %>%
      bind_rows(L2262.Biofuel_GrossTrade_EJ_R_C_Y)

    # 7/21/2021 adjustment - gpk
    # The refined liquids trade data can include regions where the gross exports are greater than the total refinery output (production)
    # This is because the L1262 data on gross trade were assembled without reference to the energy balances.
    # At present this is seen in the Africa_Southern region for oil refining.
    L2262.refinedLiquids_GrossTrade_EJ_R_C_Y %>%
      left_join(L122.out_EJ_R_refining_F_Yh,
                by = c("GCAM_region_ID", "sector", "year")) %>%
      replace_na(list(value = 0)) %>%
      mutate(imbalance = pmax(0, gross_exports - value),
             gross_exports = gross_exports - imbalance,
             gross_imports = gross_imports - imbalance) %>%
      group_by(GCAM_region_ID, GCAM_commodity, sector, year) %>%
      summarise(gross_exports = sum(gross_exports),
                gross_imports = sum(gross_imports),
                net_trade = sum(net_trade)) %>%
      ungroup() ->
      L2262.refinedLiquids_GrossTrade_EJ_R_C_Y


    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
    # L2262.Supplysector_refinedLiquids_tra: generic supplysector info for traded regional liquids commodity
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A262.refinedLiquidsTradedSector$region <- gcam.USA_REGION

    # L2262.Supplysector_refinedLiquids_tra: generic supplysector info for traded refinedOil commodities
    L2262.Supplysector_refinedLiquids_tra <- mutate(A262.refinedLiquidsTradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L2262.SectorUseTrialMarket_refinedLiquids_tra: Create solved markets for the traded sectors
    L2262.SectorUseTrialMarket_refinedLiquids_tra <- select(A262.refinedLiquidsTradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L2262.SubsectorAll_refinedLiquids_tra: generic subsector info for traded refinedOil commodity
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L2262.SubsectorAll_refinedLiquids_tra <- write_to_all_regions(A262.refinedLiquidsTradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  GCAM_region_names,
                                                  has_traded = TRUE)

    # Base technology-level table for several tables to be written out")
    A262.refinedLiquidsTradedTechnology_R_Y <- repeat_add_columns(A262.refinedLiquidsTradedTechnology,
                                                   tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

    # L2262.TechShrwt_refinedLiquids_tra: Share-weights of traded technologies
    L2262.TechShrwt_refinedLiquids_tra <- select(A262.refinedLiquidsTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L2262.TechCost_refinedLiquids_tra: Costs of traded technologies
    L2262.TechCost_refinedLiquids_tra <- A262.refinedLiquidsTradedTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L2262.TechCoef_refinedLiquids_tra: Coefficient and market name of traded technologies
    L2262.TechCoef_refinedLiquids_tra <- select(A262.refinedLiquidsTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L2262.Production_refinedLiquids_tra: Output (gross exports) of traded technologies
    L2262.refinedLiquidsGrossExports_EJ_R_C_Y <- L2262.refinedLiquids_GrossTrade_EJ_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      ### HACK - the fuel commodity names (biodiesel, ethanol, refining) are replaced by feedstock-specific fuel commodities for biodiesel and ethanol
      mutate(minicam.energy.input = if_else(GCAM_commodity == "refining", GCAM_commodity, sector)) %>%
      select(region, GCAM_commodity, sector, minicam.energy.input, year, gross_exports)

    L2262.Production_refinedLiquids_tra <- filter(A262.refinedLiquidsTradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2262.refinedLiquidsGrossExports_EJ_R_C_Y,
                               by = c(market.name = "region", "minicam.energy.input", "year")) %>%
      mutate(calOutputValue = round(gross_exports, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # PART 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
    # L2262.Supplysector_refinedLiquids_reg: generic supplysector info for regional refinedOil commodity
    L2262.Supplysector_refinedLiquids_reg <- mutate(A262.refinedLiquidsRegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names)

    # L2262.SubsectorAll_refinedLiquids_reg: generic subsector info for regional refinedOil commodity (competing domestic prod vs intl imports)
    L2262.SubsectorAll_refinedLiquids_reg <- write_to_all_regions(A262.refinedLiquidsRegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  GCAM_region_names)

    # Base technology-level table for several tables to be written out")
    A262.refinedLiquidsRegionalTechnology_R_Y <- repeat_add_columns(A262.refinedLiquidsRegionalTechnology,
                                                     tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names["region"]) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name)) %>%
      filter(is.na(memo) | region == "USA" & memo == "USA only") %>%
      select(-memo)

    # L2262.TechShrwt_refinedLiquids_tra: Share-weights of traded technologies
    L2262.TechShrwt_refinedLiquids_reg <- select(A262.refinedLiquidsRegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    L2262.TechCost_refinedLiquids_reg <- A262.refinedLiquidsRegionalTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      filter(input.cost != 0) %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L2262.TechCoef_refinedLiquids_reg: Coefficient and market name of traded technologies
    L2262.TechCoef_refinedLiquids_reg <- select(A262.refinedLiquidsRegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L2262.Production_refinedLiquids_reg_imp: Output (flow) of gross imports
    # Imports are equal to the gross imports calculated in LA1262
    L2262.refinedLiquidsGrossImports_EJ_R_C_Y <- L2262.refinedLiquids_GrossTrade_EJ_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      ### HACK - the fuel commodity names (biodiesel, ethanol, refining) are replaced by feedstock-specific fuel commodities for biodiesel and ethanol
      mutate(minicam.energy.input = if_else(GCAM_commodity == "refining", GCAM_commodity, sector)) %>%
      left_join(select(A262.refinedLiquidsTradedTechnology, supplysector, minicam.energy.input),
                by = "minicam.energy.input") %>%
      select(region, supplysector, year, gross_imports)

    L2262.Production_refinedLiquids_reg_imp <- A262.refinedLiquidsRegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L2262.refinedLiquidsGrossImports_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      mutate(calOutputValue = round(gross_imports, energy.DIGITS_CALOUTPUT),
             calOutputValue = if_else(technology != subsector, 0, calOutputValue),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]])


    # L2262.Production_refinedLiquids_reg_dom: Output (flow) of domestic
    # Domestic "output" is equal to production (value in L122) minus gross exports (calculated in LA1262)

    #### DOMESTIC TECHNOLOGY OUTPUT = refinedOil PRODUCTION - GROSS EXPORTS
    # Production
    L2262.refinedLiquidsProd_EJ_R_C_Y <- L122.out_EJ_R_refining_F_Yh %>%
      mutate(minicam.energy.input = if_else(sector %in% c("oil refining", "ctl", "gtl"), "refining", sector)) %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID")) %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(Prod_EJ = sum(value)) %>%
      ungroup()

    # Consumption of domestic production: equal to production minus gross exports
    # The production table doesn't have all zeroes written out (ignore_columns -> replace_na
    # 3/31/2025 gpk revision - the "exceed blend wall" techs have names that differ from their parent subsector, and have 0 calibration
    L2262.Production_refinedLiquids_reg_dom <- A262.refinedLiquidsRegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L2262.refinedLiquidsGrossExports_EJ_R_C_Y,
                               by = c("region", "minicam.energy.input", "year")) %>%
      left_join_error_no_match(L2262.refinedLiquidsProd_EJ_R_C_Y,
                               by = c("region", "minicam.energy.input", "year"),
                               ignore_columns = "Prod_EJ") %>%
      replace_na(list(Prod_EJ = 0)) %>%
      mutate(calOutputValue = round(Prod_EJ - gross_exports, energy.DIGITS_CALOUTPUT),
             calOutputValue = if_else(technology != subsector, 0, calOutputValue),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # FINAL STEP - filter out the region x biofuel markets that do not exist.
    # Traded subsectors and technologies
    L2262.biofuel_type_filter_tra <- L222.biofuel_type_filter_R %>%
      mutate(subsector = paste(region, "traded", supplysector),
             region = "USA",
             supplysector = paste("traded", supplysector))

    L2262.SubsectorAll_refinedLiquids_tra <- anti_join(L2262.SubsectorAll_refinedLiquids_tra, L2262.biofuel_type_filter_tra,
                                                       by = c("region", "supplysector", "subsector"))
    L2262.TechShrwt_refinedLiquids_tra <- anti_join(L2262.TechShrwt_refinedLiquids_tra, L2262.biofuel_type_filter_tra,
                                                    by = c("region", "supplysector", "subsector"))
    L2262.TechCost_refinedLiquids_tra <- anti_join(L2262.TechCost_refinedLiquids_tra, L2262.biofuel_type_filter_tra,
                                                   by = c("region", "supplysector", "subsector"))
    L2262.TechCoef_refinedLiquids_tra <- anti_join(L2262.TechCoef_refinedLiquids_tra, L2262.biofuel_type_filter_tra,
                                                   by = c("region", "supplysector", "subsector"))
    L2262.Production_refinedLiquids_tra <- anti_join(L2262.Production_refinedLiquids_tra, L2262.biofuel_type_filter_tra,
                                                     by = c("region", "supplysector", "subsector"))

    # Regional subsectors and technologies
    L2262.biofuel_type_filter_reg <- L222.biofuel_type_filter_R %>%
      mutate(subsector = paste("domestic", supplysector),
             supplysector = paste("regional", supplysector))

    L2262.SubsectorAll_refinedLiquids_reg <- anti_join(L2262.SubsectorAll_refinedLiquids_reg, L2262.biofuel_type_filter_reg,
                                                       by = c("region", "supplysector", "subsector"))
    L2262.TechShrwt_refinedLiquids_reg <- anti_join(L2262.TechShrwt_refinedLiquids_reg, L2262.biofuel_type_filter_reg,
                                                    by = c("region", "supplysector", "subsector"))
    L2262.TechCost_refinedLiquids_reg <- anti_join(L2262.TechCost_refinedLiquids_reg, L2262.biofuel_type_filter_reg,
                                                   by = c("region", "supplysector", "subsector"))
    L2262.TechCoef_refinedLiquids_reg <- anti_join(L2262.TechCoef_refinedLiquids_reg, L2262.biofuel_type_filter_reg,
                                                   by = c("region", "supplysector", "subsector"))
    L2262.Production_refinedLiquids_reg_dom <- anti_join(L2262.Production_refinedLiquids_reg_dom, L2262.biofuel_type_filter_reg,
                                                         by = c("region", "supplysector", "subsector"))

    # 12/2021: We adjust biodiesel shareweights to correct the behaviour in some regions
    # We use a hybrid approach:
    #  - Self-producers (domestic prod > imports) -> Long-term share-weights of imports to 0.2
    #  - Importers (domestic prod <= imports) -> Long-term share-weights of imports to 1 (current assumption)

    # First we classify the regions as self-producers or importers based on their regional biodiesel in final calibration year:
    self_prod<-bind_rows(L2262.Production_refinedLiquids_reg_dom, L2262.Production_refinedLiquids_reg_imp) %>%
      filter(year == max(MODEL_BASE_YEARS),
             grepl("biodiesel",supplysector) | grepl("ethanol",supplysector),
             subsector == technology) %>%
      select(region,supplysector,subsector,year,calOutputValue) %>%
      mutate(subsector = if_else(grepl("domestic",subsector),"domestic","imported")) %>%
      spread(subsector,calOutputValue) %>%
      replace_na(list(domestic = 0)) %>%
      mutate(is.self.prod = if_else(imported > domestic,0,1)) %>%
      gather(subsector_adj,value,-region,-supplysector,-is.self.prod,-year) %>%
      mutate(supplysector_adj = gsub("regional ","",supplysector),
             subsector = paste(subsector_adj,supplysector_adj)) %>%
      select(region,supplysector,subsector,is.self.prod)

    # Then, we adjust the long-term shareweights in L2262.SubsectorAll_refinedLiquids_reg
    L2262.SubsectorAll_refinedLiquids_reg<-L2262.SubsectorAll_refinedLiquids_reg %>%
      # use left_join because the full list includes traded refined oil and ethanol
      left_join(self_prod, by = c("region", "supplysector", "subsector")) %>%
      # make an adjustment to make sure that the non-biodiesel categories are not adjusted
      replace_na(list(is.self.prod = 0)) %>%
      # also adjust the domestic subsector because we don't want to change the sw there
      mutate(is.self.prod = if_else(grepl("domestic",subsector),0,is.self.prod)) %>%
      # adjust the sw
      mutate(share.weight = if_else(is.self.prod == 1,0.2,share.weight)) %>%
      select(-is.self.prod)


    # Produce outputs
    L2262.Supplysector_refinedLiquids_tra %>%
      add_title("Supplysector info for traded refinedOil commodity") %>%
      add_units("None") %>%
      add_comments("None") %>%
      add_precursors("common/GCAM_region_names", "energy/A262.refinedLiquidsTradedSector") ->
      L2262.Supplysector_refinedLiquids_tra

    L2262.SectorUseTrialMarket_refinedLiquids_tra %>%
      add_title("Supplysector flag indicating to make trial markets") %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with refinedOil trade") %>%
      add_precursors("common/GCAM_region_names", "energy/A262.refinedLiquidsTradedSector") ->
      L2262.SectorUseTrialMarket_refinedLiquids_tra

    L2262.SubsectorAll_refinedLiquids_tra %>%
      add_title("Subsector info for traded refinedOil commodities") %>%
      add_units("None") %>%
      add_comments("None") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsTradedSubsector") ->
      L2262.SubsectorAll_refinedLiquids_tra

    L2262.TechShrwt_refinedLiquids_tra %>%
      add_title("Technology share-weights for traded refinedOil commodities") %>%
      add_units("None") %>%
      add_comments("None") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsTradedTechnology") ->
      L2262.TechShrwt_refinedLiquids_tra

    L2262.TechCost_refinedLiquids_tra %>%
      add_title("Technology costs for traded refinedOil commodities") %>%
      add_units("1975$/EJ") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsTradedTechnology") ->
      L2262.TechCost_refinedLiquids_tra

    L2262.TechCoef_refinedLiquids_tra %>%
      add_title("Technology input-output coefficients for traded refinedOil commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsTradedTechnology") ->
      L2262.TechCoef_refinedLiquids_tra

    L2262.Production_refinedLiquids_tra %>%
      add_title("Technology calibration for traded refinedOil commodities") %>%
      add_units("EJ") %>%
      add_comments("Regional exports of commodities that are traded between GCAM regions. Note: the sum of traded should equal sum of imports") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_regions",
                     "energy/A262.refinedLiquidsTradedTechnology",
                     "L1262.refinedOil_GrossTrade_EJ_R_C_Y") ->
      L2262.Production_refinedLiquids_tra

    L2262.Supplysector_refinedLiquids_reg %>%
      add_title("Supplysector info for regional refinedOil commodities") %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced refined liquids versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsRegionalSector") ->
      L2262.Supplysector_refinedLiquids_reg

    L2262.SubsectorAll_refinedLiquids_reg %>%
      add_title("Subsector info for traded refinedOil commodities") %>%
      add_units("None") %>%
      add_comments("None") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsRegionalSubsector",
                     "L222.biofuel_type_filter_R") ->
      L2262.SubsectorAll_refinedLiquids_reg

    L2262.TechShrwt_refinedLiquids_reg %>%
      add_title("Technology share-weights for traded refinedOil commodities") %>%
      add_units("None") %>%
      add_comments("None") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsRegionalTechnology",
                     "L222.biofuel_type_filter_R") ->
      L2262.TechShrwt_refinedLiquids_reg

    L2262.TechCost_refinedLiquids_reg %>%
      add_title("Technology costs for regional refinedOil commodities") %>%
      add_units("None") %>%
      add_comments("only non-zero costs written") %>%
      same_precursors_as(L2262.TechShrwt_refinedLiquids_reg) ->
      L2262.TechCost_refinedLiquids_reg

    L2262.TechCoef_refinedLiquids_reg %>%
      add_title("Technology input-output coefficients for regional refinedOil commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsRegionalTechnology",
                     "L222.biofuel_type_filter_R") ->
      L2262.TechCoef_refinedLiquids_reg

    L2262.Production_refinedLiquids_reg_imp %>%
      add_title("Technology calibration for regional refinedOil commodities: imports") %>%
      add_units("EJ") %>%
      add_comments("Consumption of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_regions",
                     "energy/A262.refinedLiquidsRegionalTechnology",
                     "L121.share_R_TPES_biofuel_tech",
                     "L1262.refinedOil_GrossTrade_EJ_R_C_Y",
                     "L1262.Biofuel_GrossTrade_EJ_R_C_Y") ->
      L2262.Production_refinedLiquids_reg_imp

    L2262.Production_refinedLiquids_reg_dom %>%
      add_title("Technology calibration for regional refinedOil commodities: consumption of domestic production") %>%
      add_units("EJ") %>%
      add_comments("Consumption of commodities produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A262.refinedLiquidsRegionalTechnology",
                     "L121.share_R_TPES_biofuel_tech",
                     "L122.out_EJ_R_refining_F_Yh",
                     "L1262.refinedOil_GrossTrade_EJ_R_C_Y",
                     "L1262.Biofuel_GrossTrade_EJ_R_C_Y",
                     "L222.biofuel_type_filter_R",
                     FILE = "energy/bio_feed_mapping") ->
      L2262.Production_refinedLiquids_reg_dom

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
