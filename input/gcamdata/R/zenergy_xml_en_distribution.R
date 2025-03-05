# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_distribution_xml
#'
#' Construct XML data structure for \code{en_distribution.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_distribution.xml}. The corresponding file in the
#' original data system was \code{batch_en_distribution.xml.R} (energy XML).
module_energy_en_distribution_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      "L226.Supplysector_en",
      "L226.SubsectorLogit_en",
      "L226.SubsectorShrwt_en",
      "L226.SubsectorShrwtFllt_en",
      "L226.SubsectorInterp_en",
      "L226.SubsectorInterpTo_en",
      "L226.StubTech_en",
      "L226.GlobalTechEff_en",
      "L226.GlobalTechCost_en",
      "L226.GlobalTechTrackCapital_en",
      "L226.GlobalTechShrwt_en",
      "L226.StubTechCoef_elecownuse",
      "L226.StubTechCoef_electd",
      "L226.StubTechCoef_gaspipe",
      "L254.StubTranTechCalInput",
      "L244.StubTechCalInput_bld",

      "L2261.GlobalTechEff_en",

      # temp include the residual energy inputs into other_ind
      "L1328.in_EJ_R_indenergy_F_Yh",
      #"L131.in_EJ_R_Senduse_F_Yh",
      "L2262.Production_refinedLiquids_tra",
      "L2262.Production_refinedLiquids_reg_imp",
      "L2262.Production_refinedLiquids_reg_dom")

  MODULE_OUTPUTS <-
    c(XML = "en_distribution.xml",
      "L1329.in_EJ_R_indenergy_F_Yh")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Calculate net trade again based on the final values
    L2262.Production_refinedLiquids_tra %>%
      group_by(year, subsector) %>%
      mutate(region = gsub(supplysector, "", subsector),
             region = gsub(" $", "", region),
             sector = gsub("traded ", "", supplysector)) %>%
      ungroup() %>%
      select(region, year, sector, export = calOutputValue) %>%
      full_join(
        L2262.Production_refinedLiquids_reg_imp %>%
          mutate(sector = gsub("regional ", "", supplysector)) %>%
          select(region, year, sector, import = calOutputValue),
        by = c("region", "year", "sector")
      ) %>%
      replace_na(list(export = 0, import = 0)) %>%
      mutate(netexport = export - import) %>%
      filter(netexport != 0) ->
      refinedLiquids_netexport

    # remove trade data for refined oil
    refinedLiquids_netexport %>% filter(sector != "refined oil") ->
      refinedLiquids_netexport

    L1328.in_EJ_R_indenergy_F_Yh %>%
      filter(fuel == "refined liquids") %>%
      group_by(GCAM_region_ID, fuel, sector, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(
        refinedLiquids_netexport %>%
          group_by(region, year) %>%
          summarize(netexport = sum(netexport), .groups = "drop"),
        by = c("year", "region")) %>%
      replace_na(list(netexport = 0)) %>%
      mutate(value = value - netexport) %>%
      select(-netexport, -region) ->
        L1328.in_EJ_R_indenergy_F_Yh_refinedliquids_adj


    L1328.in_EJ_R_indenergy_F_Yh_refinedliquids_adj %>%
      mutate(value = pmax(0, value)) ->
      L1328.in_EJ_R_indenergy_F_Yh_refinedliquids_adj


    L1328.in_EJ_R_indenergy_F_Yh %>%
      filter(fuel != "refined liquids") %>%
      bind_rows(
        L1328.in_EJ_R_indenergy_F_Yh_refinedliquids_adj
      ) ->
      L1329.in_EJ_R_indenergy_F_Yh


    # calculate regional biofuels
    L2262.Production_refinedLiquids_reg_imp %>%
      bind_rows(L2262.Production_refinedLiquids_reg_dom) %>%
      group_by(region, year, minicam.energy.input = supplysector) %>%
      summarize(calOutputValue = sum(calOutputValue), .groups = "drop") %>%
      filter(minicam.energy.input != "regional refined oil") %>%
      left_join_error_no_match(
        L226.GlobalTechEff_en %>%
          bind_rows(L2261.GlobalTechEff_en %>% filter(subsector.name == "ethanol")) %>%
          filter(sector.name != "refined liquids industrial") %>%
          transmute(supplysector = sector.name,
                    subsector = subsector.name,
                    stub.technology = technology,
                    minicam.energy.input, year),
        by = c("year", "minicam.energy.input")) %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0),
             subs.share.weight = tech.share.weight) %>%
      select(-minicam.energy.input) ->
      L2262.StubTechConsumption_refining_biofuels


    # right balance of refined liquids enduse demand
    # As we move 1G biofuels up in the nesting to be directly compete with
    # "refined oil" in an upper level. We want to be sure data is balanced.
    # Now refined oil, biodiesel, and other biofuels are aggregated into
    # "refined liquids enduse".

    # For some reason, we need the data for initial calibration
    # likely because "refined liquids enduse" is consumed in multiple placed
    # and the subsector input "refined oil" is also consumed in multiple places
    # ("refined liquids industrial" and "refined liquids enduse")
    # so the existing framework didn't work directly


    # "refined liquids enduse" is used in 2 places:

    # WRONG METHODS
    # L131.in_EJ_R_Senduse_F_Yh %>%
    #   filter(grepl("bld|trn", sector)) %>%
    #   left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
    #   filter(fuel == "refined liquids", year %in% MODEL_BASE_YEARS) %>%
    #   group_by(region, year) %>%
    #   summarize(value = sum(value), .groups = "drop") ->
    #   L226.StubTechProd_RefinedLiquidsEnduse1

    # Alternatively:
    # transportation (L254.StubTranTechCalInput)
    # and building (L244.StubTechCalInput_bld)
    # we need to calculate the values used there.

    L254.StubTranTechCalInput %>%
      # there are multiple sce (scenarios), any one would work
      filter(sce == "CORE",
             minicam.energy.input == "refined liquids enduse") %>%
      group_by(region, year) %>%
      summarize(value = sum(calibrated.value), .groups = "drop") %>%
      bind_rows(
        L244.StubTechCalInput_bld %>%
          filter(minicam.energy.input == "refined liquids enduse") %>%
          group_by(region, year) %>%
          summarize(value = sum(calibrated.value), .groups = "drop")
      ) %>%
      group_by(region, year) %>%
      summarize(calOutputValue = sum(value), .groups = "drop") ->
      L226.StubTechProd_RefinedLiquidsEnduse

    L254.StubTranTechCalInput %>%
      # there are multiple sce (scenarios), any one would work
      filter(sce == "CORE",
             minicam.energy.input == "refined liquids gasoline pool") %>%
      group_by(region, year) %>%
      summarize(value = sum(calibrated.value), .groups = "drop") %>%
      group_by(region, year) %>%
      summarize(calOutputValue = sum(value), .groups = "drop") ->
      L226.StubTechProd_RefinedLiquidsGasPool


    # Then we need to split "refined liquids enduse" by "biodiesel", "ethanol" and "refined oil"

    #  This is biodiesel data
    L2262.StubTechConsumption_refining_biofuels %>%
      filter(subsector == "biodiesel")%>%
      group_by(region, year, subsector) %>%
      mutate(subs.share.weight = if_else(sum(calOutputValue)>0, 1, 0)) %>%
      ungroup() ->
      L226.StubTechProd_biodiesel

    #  This is ethanol data
    L2262.StubTechConsumption_refining_biofuels %>%
      filter(subsector == "ethanol") ->
      L226.StubTechProd_ethanol

    # add cellulosic ethanol
    L226.StubTechProd_ethanol %>%
      distinct(region, supplysector, subsector, year) %>%
      mutate(stub.technology = "cellulosic ethanol refining",
             share.weight.year = year, tech.share.weight = 0, subs.share.weight = tech.share.weight,
             calOutputValue = 0) %>%
      bind_rows(L226.StubTechProd_ethanol) %>%
      group_by(region, year, subsector) %>%
      mutate(subs.share.weight = if_else(sum(calOutputValue)>0, 1, 0)) %>%
      ungroup() ->
      L226.StubTechProd_ethanol

    # subtract biodiesel and ethanol  from the total to get refined oil consumption

    # ethanol is now in gasoline pool
    L226.StubTechProd_biodiesel %>%
      #bind_rows(L226.StubTechProd_ethanol) %>%
      select(names(L226.StubTechProd_RefinedLiquidsEnduse)) %>%
      mutate(calOutputValue = -calOutputValue) %>%
      bind_rows(L226.StubTechProd_RefinedLiquidsEnduse) %>%
      group_by_at(vars(-calOutputValue)) %>%
      summarize(calOutputValue = sum(calOutputValue), .groups = "drop") %>%
      mutate(supplysector = "refined liquids enduse",
             subsector = "refined oil",
             stub.technology = "refined oil",
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0),
             subs.share.weight = tech.share.weight) ->
      L226.StubTechProd_refinedoil_in_RefinedLiquidsEnduse


    # do the same for refined oil in gasoline pool
    L226.StubTechProd_ethanol %>%
      select(names(L226.StubTechProd_RefinedLiquidsGasPool)) %>%
      mutate(calOutputValue = -calOutputValue) %>%
      bind_rows(L226.StubTechProd_RefinedLiquidsGasPool) %>%
      group_by_at(vars(-calOutputValue)) %>%
      summarize(calOutputValue = sum(calOutputValue), .groups = "drop") %>%
      mutate(supplysector = "refined liquids gasoline pool",
             subsector = "refined oil",
             stub.technology = "refined oil",
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0),
             subs.share.weight = tech.share.weight) ->
      L226.StubTechProd_refinedoil_in_RefinedLiquidsGasPool


    # Clean up techs in refined liquids enduse

    L226.StubTech_en %>% filter(subsector == "biodiesel") %>%
      distinct(region, subsector, stub.technology) %>%
      anti_join(
        L226.StubTechProd_biodiesel %>%
          distinct(region, subsector, stub.technology),
        by = c("region", "subsector", "stub.technology")
      ) ->
      EmptyBiodieselTech

    L226.StubTech_en %>% filter(subsector == "ethanol") %>%
      distinct(region, subsector, stub.technology) %>%
      anti_join(
        L226.StubTechProd_ethanol %>%
          distinct(region, subsector, stub.technology),
        by = c("region", "subsector", "stub.technology")
      ) ->
      EmptyEthanolTech

    L226.StubTech_en %>%
      anti_join(EmptyBiodieselTech,
                by = c("region", "subsector", "stub.technology")) %>%
      anti_join(EmptyEthanolTech,
                by = c("region", "subsector", "stub.technology")) ->
      L226.StubTech_en



    # ===================================================

    # Produce outputs
    create_xml("en_distribution.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en, "SubsectorLogit") ->
      en_distribution.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L226.SubsectorShrwt_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorShrwt_en, "SubsectorShrwt") ->
        en_distribution.xml
    }

    if(!is.null(L226.SubsectorShrwtFllt_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") ->
        en_distribution.xml
    }

    if(!is.null(L226.SubsectorInterp_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorInterp_en, "SubsectorInterp") ->
        en_distribution.xml
    }

    if(!is.null(L226.SubsectorInterpTo_en)) {
      en_distribution.xml %>%
        add_xml_data(L226.SubsectorInterpTo_en, "SubsectorInterpTo") ->
        en_distribution.xml
    }

    en_distribution.xml %>%
      add_xml_data(L226.StubTech_en, "StubTech") %>%
      add_xml_data(L226.GlobalTechEff_en, "GlobalTechEff") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L226.GlobalTechTrackCapital_en, "GlobalTechTrackCapital") %>%
      add_xml_data(L226.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L226.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L226.StubTechCoef_elecownuse, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_electd, "StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_gaspipe, "StubTechCoef") %>%
      add_xml_data(L226.StubTechProd_biodiesel, "StubTechProd") %>%
      add_xml_data(L226.StubTechProd_ethanol, "StubTechProd") %>%
      add_xml_data(L226.StubTechProd_refinedoil_in_RefinedLiquidsEnduse, "StubTechProd") %>%
      add_xml_data(L226.StubTechProd_refinedoil_in_RefinedLiquidsGasPool, "StubTechProd") %>%
      add_precursors(MODULE_INPUTS) ->
      en_distribution.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

