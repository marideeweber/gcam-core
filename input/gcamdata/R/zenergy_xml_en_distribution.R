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
    c("L226.Supplysector_en",
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
      "L222.StubTechProd_refining",
      "L254.StubTranTechCalInput",
      "L244.StubTechCalInput_bld")

  MODULE_OUTPUTS <-
    c(XML = "en_distribution.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # right balance of refined liquids enduse demand
    # As we move 1G biofuels up in the nesting to be directly compete with
    # "refined oil" in an upper level. We want to be sure data is balanced.
    # Now refined oil, biodiesel, and other biofuels are aggregated into
    # "refined liquids enduse".

    # For some reason, we need the data for iniital calibration
    # likely because "refined liquids enduse" is consumed in multiple placed
    # and the subsector input "refined oil" is also consumed in multiple places
    # ("refined liquids industrial" and "refined liquids enduse")
    # so the existing framework didn't work directly


    # "refined liquids enduse" is used in 2 places:
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

    # The we need to split "refined liquids enduse" by "biodiesel and "refined oil"

    #  This is biodiesel data
    L222.StubTechProd_refining %>%
      filter(grepl("biodiesel", supplysector)) %>%
      mutate(supplysector = "refined liquids enduse", subsector = "biodiesel",
             stub.technology = paste0(stub.technology, " refining")) %>%
      rename(share.weight.year = year.share.weight, tech.share.weight = share.weight) ->
      L226.StubTechProd_biodiesel

    L226.StubTechProd_biodiesel %>%
      mutate(coefficient = 1, market.name = region,
             minicam.energy.input = gsub(" refining", "", stub.technology)) %>%
      select(names(L226.StubTechCoef_gaspipe)) ->
      L226.StubTechCoef_biodiesel

    #  This is ethanol data
    L222.StubTechProd_refining %>%
      filter(grepl("ethanol", supplysector)) %>%
      mutate(supplysector = "refined liquids enduse", subsector = "ethanol",
             stub.technology = paste0(stub.technology, " refining")) %>%
      rename(share.weight.year = year.share.weight, tech.share.weight = share.weight) ->
      L226.StubTechProd_ethanol

    L226.StubTechProd_ethanol %>%
      mutate(coefficient = 1, market.name = region,
             minicam.energy.input = gsub(" refining", "", stub.technology)) %>%
      select(names(L226.StubTechCoef_gaspipe)) ->
      L226.StubTechCoef_ethanol

    # add cellulosic ethanol
    L226.StubTechProd_ethanol %>%
      distinct(region, supplysector, subsector, year) %>%
      mutate(stub.technology = "cellulosic ethanol refining",
             share.weight.year = 0, tech.share.weight = 0, subs.share.weight = tech.share.weight,
             calOutputValue = 0) %>%
      bind_rows(L226.StubTechProd_ethanol) ->
      L226.StubTechProd_ethanol

    # subtract biodiesel and ethanol  from the total to get refined oil consumption

    L226.StubTechProd_biodiesel %>%
      bind_rows(L226.StubTechProd_ethanol) %>%
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


    # More importantly, clean up techs in refined liquids enduse

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

    # remove interpolation when tech is zero
    # L226.SubsectorInterp_en %>%
    #   anti_join(
    #     L226.StubTechProd_biodiesel %>%
    #       filter(year == MODEL_FINAL_BASE_YEAR) %>%
    #       group_by(region, supplysector, subsector) %>%
    #       summarize(calOutputValue = sum(calOutputValue)) %>%
    #       filter(calOutputValue == 0),
    #     by = c("region", "supplysector", "subsector")
    #   ) ->
    #   L226.SubsectorInterp_en


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
      #add_xml_data(L226.StubTechCoef_biodiesel, "StubTechCoef") %>%
      add_xml_data(L226.StubTechProd_biodiesel, "StubTechProd") %>%
      #add_xml_data(L226.StubTechCoef_ethanol, "StubTechCoef") %>%
      add_xml_data(L226.StubTechProd_ethanol, "StubTechProd") %>%
      add_xml_data(L226.StubTechProd_refinedoil_in_RefinedLiquidsEnduse, "StubTechProd") %>%
      add_precursors(MODULE_INPUTS) ->
      en_distribution.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

