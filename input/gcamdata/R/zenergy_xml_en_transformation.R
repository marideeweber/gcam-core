# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_transformation_xml
#'
#' Construct XML data structure for \code{en_transformation.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation.xml.R} (energy XML).
module_energy_en_transformation_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L222.Supplysector_en",
      "L222.SectorUseTrialMarket_en",
      "L222.SubsectorLogit_en",
      "L222.SubsectorShrwtFllt_en",
      "L222.SubsectorInterp_en",
      "L222.StubTech_en",
      "L222.GlobalTechInterp_en",
      "L222.GlobalTechCoef_en",
      "L222.GlobalTechCost_en",
      "L222.GlobalTechTrackCapital_en",
      "L222.GlobalTechShrwt_en",
      "L222.GlobalTechCapture_en",
      "L222.GlobalTechSCurve_en",
      "L222.GlobalTechProfitShutdown_en",
      "L222.StubTechLifetime_en",
      "L222.GlobalTechKeyword_en",
      "L222.StubTechProd_gasproc",
      "L222.StubTechProd_refining",
      "L222.StubTechCoef_refining")

  MODULE_OUTPUTS <-
    c(XML = "en_transformation.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    year.share.weight <- share.weight <- NULL # silence package checks
    # ===================================================
    # Rename the tibble columns to match the header information.
    L222.StubTechProd_gasproc <- rename(L222.StubTechProd_gasproc, share.weight.year = year.share.weight, tech.share.weight = share.weight)
    L222.StubTechProd_refining <- rename(L222.StubTechProd_refining, share.weight.year = year.share.weight, tech.share.weight = share.weight)

    # L222.StubTech_en %>%
    #   filter(!grepl("biodiesel", stub.technology)) %>%
    #   # bind cleaned biodiesel tech
    #   bind_rows(
    #     L222.StubTech_en %>%
    #       filter(grepl("biodiesel", stub.technology)) %>%
    #       inner_join(
    #         L222.StubTechProd_refining %>%
    #           distinct(region, supplysector, subsector, stub.technology) %>%
    #           filter(grepl("biodiesel", stub.technology)),
    #         by = c("region", "supplysector", "subsector", "stub.technology")
    #       )
    #   ) ->
    #   L222.StubTech_en

    # Produce outputs
    create_xml("en_transformation.xml") %>%
      add_logit_tables_xml(L222.Supplysector_en, "Supplysector") %>%
      add_xml_data(L222.SectorUseTrialMarket_en, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L222.SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L222.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") %>%
      add_xml_data(L222.SubsectorInterp_en, "SubsectorInterp") %>%
      add_xml_data(L222.StubTech_en, "StubTech") %>%
      add_xml_data(L222.GlobalTechInterp_en, "GlobalTechInterp") %>%
      add_xml_data(L222.GlobalTechCoef_en, "GlobalTechCoef") %>%
      # set non-energy inputs to be read as tracking
      add_node_equiv_xml("input") %>%
      add_xml_data(L222.GlobalTechTrackCapital_en, "GlobalTechTrackCapital") %>%
      add_xml_data(L222.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L222.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L222.GlobalTechCapture_en, "GlobalTechCapture") %>%
      add_xml_data(L222.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L222.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L222.StubTechLifetime_en, "StubTechLifetime") %>%
      add_xml_data(L222.GlobalTechKeyword_en, "PrimaryConsKeyword") %>%
      add_xml_data(L222.StubTechProd_gasproc, "StubTechProd") %>%
      add_xml_data(L222.StubTechProd_refining, "StubTechProd") %>%
      add_xml_data(L222.StubTechCoef_refining, "StubTechCoef") %>%
      add_precursors(MODULE_INPUTS) ->
      en_transformation.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
