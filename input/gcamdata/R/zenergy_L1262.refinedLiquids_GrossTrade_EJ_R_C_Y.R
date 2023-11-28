#' module_energy_L1262.refinedOil_GrossTrade
#'
#' Calculate primary regional liquids product mass balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1262.refinedOil_GrossTrade_EJ_R_C_Y} (energy level1)
#' @details This chunk processes the bi-lateral trade flow data matrix from UN Comtrade, in order to differentiate trade of
#'   regional liquids between GCAM regions.
#' @importFrom dplyr anti_join bind_rows distinct filter group_by inner_join left_join mutate rename select ungroup
#' @importFrom tidyr complete drop_na replace_na spread
#' @author GPK/JEH April 2019
module_energy_L1262.refinedOil_GrossTrade <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "energy/mappings/comtrade_ISO",
      FILE = "energy/mappings/comtrade_commodity_GCAM",
      FILE = "energy/mappings/comtrade_trade_flow_code_mapping",
      FILE = "energy/comtrade_biofuels",
      FILE = "energy/comtrade_refinedLiquids",
      FILE="energy/transport_data",
      "L1011.en_bal_EJ_R_Si_Fi_Yh")

  MODULE_OUTPUTS <-
    c("L1262.refinedOil_GrossTrade_EJ_R_C_Y",
      "L1262.Biofuel_GrossTrade_EJ_R_C_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Element <- Reporter.Countries <- Partner.Countries <-
      Item.Code <- Item <- item.code <- bitrade_commod <- GCAM_commodity <-
      FAO_country <- iso <- iso.partner <- iso.reporter <- GCAM_region_ID <-
      GCAMreg.partner <- var <- gross_exports <- gross_imports <- NetExp_Mt <- net_trade <-
      GrossExp_Mt <- Prod_Mt <- GrossImp_Mt <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    #April 29th 2019: Note that as Taiwan is not recognized by the UN there is no TWN ISO specified in the comtrade_ISO mappings
    #According to COMTRADE code 490 is (in practice) only Taiwan, but when/if better data is available for Taiwan we may want to update
   L154.in_EJ_R_trn_m_sz_tech_F_Yh <- transport_data
   
    non_GCAM_ISO <- c("wld", "n/a", "ata", "atf", "sgs", "umi", "iot", "hmd", "sxm", "blm", "bes")
    # 1: Filter and prepare the bi-lateral trade flow volume data by country and comtrade commodity

    # Select columns, filter to the import and export quantity variables, and filter to traded GCAM commodities
    # Note this data set was particularly selected for refined liquids trade. Alterations will be necessary if other commodities are added

      L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item <- bind_rows(comtrade_refinedLiquids, comtrade_biofuels) %>%
        select(Year, `Reporter Code`, `Partner Code`, `Trade Flow Code`, `Commodity Code`, `Netweight (kg)`) %>%
        left_join(comtrade_trade_flow_code_mapping,
                  by = c(`Trade Flow Code` = "Trade_Flow_Code")) %>%
      # Join the reporter and partner countries.
      # Note also - this uses left_join_keep_first_only for countries like the USSR with multiple associated present-day
      # iso codes. We wouldn't want to repeat the trade data by each post-dissolution country, and since none of these
      # actually exist during the time frame for which gross trade is being assessed, there's no benefit to downscaling.
      # Comtrade has several ISOs that GCAM doesn't use/need (e.g. wld to represent net trade by country). These are filtered
      # out to keep only relevent countries. All filtered regions are small (minor outlying islands) or unneccessary (wld)
      # GPK 8/15/25: assign the year as the final calibration year. This is the only base year gross trade is represented.
        left_join_keep_first_only(comtrade_ISO%>%select(Country_Code, ISO3),
                                  by=c(`Reporter Code` = "Country_Code")) %>%
        rename(iso.reporter = ISO3) %>%
        left_join_keep_first_only(comtrade_ISO%>%select(Country_Code, ISO3),
                                  by=c(`Partner Code` = "Country_Code")) %>%
        rename(iso.partner = ISO3,
               value = `Netweight (kg)`,
               Element = Trade) %>%
        mutate(year = max(MODEL_BASE_YEARS),
               iso.reporter = tolower(iso.reporter),
               iso.partner = tolower(iso.partner)) %>%
        select(year, iso.reporter, iso.partner, Element, `Commodity Code`, value) %>%
        drop_na(iso.partner) %>%
        drop_na(value) %>%
        filter(!iso.reporter %in% non_GCAM_ISO, !iso.partner %in% non_GCAM_ISO)


      #2. Re-balancing bilateral trade data
      # The bilateral trade data may not be symmetrical - some countries could be partner countries but not reporter countries
      # (e.g. Cuba does not report trading with others, but appears as a partner countries for a number of others). In
      # those missing cases, we can use what's already available in the bilateral trade data, and simply flip reporter/partner
      # countries, as well as export/import to get a symmetrical dataset.
      # Note any pair of non-reporting partner/reporter countries will necessarily be unaccounted for

      # First, we find those missing cases by comparing the export and import lists
      # The export list - reporter countries, partner countries, commodities, and year
      exp.list <- L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item %>%
        filter(Element == "Export Quantity") %>%
        select(iso.reporter, iso.partner, `Commodity Code`, year) %>%
        distinct()

      # The import list - reporter countries, partner countries, commodities, and year
      imp.list <- L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item %>%
        filter(Element == "Import Quantity") %>%
        select(iso.reporter, iso.partner, `Commodity Code`, year) %>%
        distinct()

      # The two lists should have the same number of observations -
      # Partner countries in the export list should be reporter countries in the import list, and vice versa

      # Find the partner countries that are missing to report import
      L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item_missing_exp <- exp.list %>%
        anti_join(imp.list, by = c("Commodity Code", "year",
                                   "iso.reporter" = "iso.partner",
                                   "iso.partner" = "iso.reporter")) %>%
        mutate(Element = "Export Quantity")
      # Find the partner countries that are missing to report export
      L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item_missing_imp <- imp.list %>%
        anti_join(exp.list, by = c("Commodity Code", "year",
                                   "iso.reporter" = "iso.partner",
                                   "iso.partner" = "iso.reporter")) %>%
        mutate(Element = "Import Quantity")

      # Filter those asymetric obersvations in the original data, and flip reporter/partner, export/import
      L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item_full <- L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item_missing_exp %>%
        bind_rows(L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item_missing_imp) %>%
        inner_join(L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item,
                   by = c("iso.reporter", "iso.partner", "Element", "Commodity Code", "year")) %>%
        mutate(Element = if_else(Element == "Import Quantity", "Export Quantity", "Import Quantity")) %>%
        select(iso.reporter = iso.partner, iso.partner = iso.reporter,
               `Commodity Code`, Element, year, value) %>%
        bind_rows(L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item)

      # 3. Deriving extra-regional trade flows by GCAM region and traded commodity, filtering out within-region trade

      # Method: join in the reporting region and partner region (from reporting and partner
      # country), filter out where reporting and partner region are the same, convert units, aggregate by GCAM region and
      # commodity, and calculate the net trade, and take an unweighted average of the years.
      # Explanation: In multi-country GCAM regions, within-region trade is excluded.

      #Prepartion: There is no mapping between Comtrade country codes and GCAM regions, so first join ISO_GCAM and Comtrade_ISO mappings

      iso_GCAM_regID %>%
        select(iso, GCAM_region_ID) %>%
        rename(iso.partner = iso,
               GCAMreg.partner = GCAM_region_ID) ->
        iso_mapping_partner

      L1262.comtrade_refinedLiquidsBiTrade_y_ctry_item_full %>%
        left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                                 by = c("iso.reporter" = "iso")) %>%
        left_join_error_no_match(iso_mapping_partner,
                                 by = "iso.partner") %>%
        filter(GCAM_region_ID != GCAMreg.partner) %>%
        left_join(comtrade_commodity_GCAM%>%select(Commodity_Code, GCAM_commodity, GJ_per_tonne),
                  by = c(`Commodity Code` = "Commodity_Code")) %>%
        mutate(value = value * CONV_KG_T * GJ_per_tonne * CONV_GJ_EJ,
               var = tolower(sub(" Quantity", "", Element))) %>%
        filter(GCAM_commodity %in% c("refining", "ethanol", "biodiesel")) %>%
        group_by(GCAM_region_ID, GCAM_commodity, var, year) %>%
        summarise(value = sum(value)) %>%
        # group_by(GCAM_region_ID, GCAM_commodity, var, .add = FALSE) %>%  #Add in when more than one year is averaged
        # summarise(value = mean(value)) %>%
        ungroup() %>%
        complete(GCAM_region_ID = unique(GCAM_region_ID),
                 GCAM_commodity = unique(GCAM_commodity),
                 var = unique(var),
                 year = unique(year)) %>%
        replace_na(list(value = 0)) %>%
        spread(var, value) %>%
        mutate(net_trade = export - import) %>%
        select(GCAM_region_ID, GCAM_commodity, year, gross_exports = export, gross_imports = import, net_trade) ->
        L1262.XregTrade_EJ_R_C

      # Modifications to trade volumes for consistency with actual use of the fuels in each region
      L1011.en_bal_EJ_R_Si_Fi_Yh %>%
        filter(sector == energy.TPES_FLOW,
               fuel %in% c("refined biofuels_ethanol", "refined biofuels_FT", "refined liquids"),
               year %in% unique(L1262.XregTrade_EJ_R_C$year)) %>%
        mutate(GCAM_commodity = if_else(fuel == "refined liquids", "refining",
                                        if_else(fuel == "refined biofuels_FT", "biodiesel", "ethanol"))) %>%
        select(GCAM_region_ID, GCAM_commodity, year, TPES = value) ->
        L1262.in_EJ_R_TPES_liq


      L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
        filter(mode %in% c("Air Domestic","Air International")) %>%
        group_by(GCAM_region_ID,year) %>%
        summarize(air_value=sum(value))->L1262_aviation_adj

      # Do not allow imports to exceed total primary energy supply. Reduce trade by this amount.
      L1262.XregTrade_EJ_R_C <- L1262.XregTrade_EJ_R_C %>%
        left_join(L1262.in_EJ_R_TPES_liq,
                                 by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
        mutate(trade_reduction = if_else(gross_imports > (TPES), gross_imports - TPES, 0),
               gross_exports = pmax(0, gross_exports - trade_reduction),
               gross_imports = pmax(0, gross_imports - trade_reduction),
               net_trade = gross_exports - gross_imports) %>%
        select(-TPES, -trade_reduction)

      #Alter Gross Exports to maintain internal consistency as the net_trade does not sum to 0
      # NOTE: give precedence to imports (rather than exports) of each commodity. This is arbitrary but of little consequence, and generally reduces amount of trade.
      L1262.XregTrade_EJ_R_C %>%
        group_by(GCAM_commodity) %>%
        mutate(gross_exports = gross_exports * sum(gross_imports)/sum(gross_exports),
               net_trade = gross_exports - gross_imports) %>%
        ungroup() ->
        L1262.refinedLiquids_GrossTrade_EJ_R_C_Y

      #Fill in 0s for all years where data isn't available (Note this defaults to previous practice)
      L1262.refinedLiquids_GrossTrade_EJ_R_C_Y %>%
        group_by(GCAM_region_ID, GCAM_commodity) %>%
        complete(year = HISTORICAL_YEARS) %>%
        replace_na(list("gross_exports" = 0, "gross_imports" = 0, "net_trade" = 0)) %>%
        ungroup() ->
        L1262.refinedLiquids_GrossTrade_EJ_R_C_Y

      L1262.refinedOil_GrossTrade_EJ_R_C_Y <- filter(L1262.refinedLiquids_GrossTrade_EJ_R_C_Y, GCAM_commodity == "refining")
      L1262.Biofuel_GrossTrade_EJ_R_C_Y <- filter(L1262.refinedLiquids_GrossTrade_EJ_R_C_Y, GCAM_commodity %in% c("ethanol", "biodiesel"))

    # Produce outputs
    L1262.refinedOil_GrossTrade_EJ_R_C_Y %>%
      add_title("L1262.refinedOil_GrossTrade_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Determined from bi-lateral trade flows; only includes trade between countries in different GCAM regions") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "energy/mappings/comtrade_ISO",
                     "energy/mappings/comtrade_commodity_GCAM",
                     "energy/mappings/comtrade_trade_flow_code_mapping",
                     "energy/comtrade_biofuels",
                     "energy/comtrade_refinedLiquids",
                     "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/transport_data") ->
      L1262.refinedOil_GrossTrade_EJ_R_C_Y

    L1262.Biofuel_GrossTrade_EJ_R_C_Y %>%
      add_title("L1262.Biofuel_GrossTrade_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Determined from bi-lateral trade flows; only includes trade between countries in different GCAM regions") %>%
      same_precursors_as(L1262.refinedOil_GrossTrade_EJ_R_C_Y) ->
      L1262.Biofuel_GrossTrade_EJ_R_C_Y

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
