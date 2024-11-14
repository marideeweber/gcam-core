# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_en_transformation_biochar_xml
#'
#' Construct XML data structure for \code{en_transformation_biochar.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation_biochar.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation_biochar.xml.R} (energy XML).
module_energy_batch_en_transformation_biochar_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2221.Supplysector_en",
             "L2221.SubsectorLogit_en",
             "L2221.SubsectorShrwtFllt_en",
             "L2221.SubsectorInterp_en",
             "L2221.StubTech_en",
             "L2221.GlobalTechInterp_en",
             "L2221.GlobalTechCoef_en",
             "L2221.GlobalTechCost_en",
             "L2221.GlobalTechShrwt_en",
             "L2221.GlobalTechCapture_en",
             "L2221.GlobalTechSCurve_en",
             "L2221.GlobalTechProfitShutdown_en"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_biochar.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2221.Supplysector_en <- get_data(all_data, "L2221.Supplysector_en") %>%
      filter(region == "USA")
    L2221.SubsectorLogit_en <- get_data(all_data, "L2221.SubsectorLogit_en") %>%
      filter(region == "USA")
    L2221.SubsectorShrwtFllt_en <- get_data(all_data, "L2221.SubsectorShrwtFllt_en") %>%
      filter(region == "USA")
    L2221.SubsectorInterp_en <- get_data(all_data, "L2221.SubsectorInterp_en") %>%
      filter(region == "USA")
    L2221.StubTech_en <- get_data(all_data, "L2221.StubTech_en") %>%
      filter(region == "USA")
    L2221.GlobalTechInterp_en <- get_data(all_data, "L2221.GlobalTechInterp_en")
    L2221.GlobalTechCoef_en <- get_data(all_data, "L2221.GlobalTechCoef_en")
    L2221.GlobalTechCost_en <- get_data(all_data, "L2221.GlobalTechCost_en")
    L2221.GlobalTechShrwt_en <- get_data(all_data, "L2221.GlobalTechShrwt_en")
    L2221.GlobalTechCapture_en <- get_data(all_data, "L2221.GlobalTechCapture_en")
    L2221.GlobalTechSCurve_en <- get_data(all_data, "L2221.GlobalTechSCurve_en")
    L2221.GlobalTechProfitShutdown_en <- get_data(all_data, "L2221.GlobalTechProfitShutdown_en")


    year.share.weight <- share.weight <- NULL # silence package checks
    # ===================================================

    # Produce outputs
    create_xml("en_transformation_biochar.xml") %>%
      add_logit_tables_xml(L2221.Supplysector_en, "Supplysector") %>%
      add_logit_tables_xml(L2221.SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L2221.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") %>%
      add_xml_data(L2221.SubsectorInterp_en, "SubsectorInterp") %>%
      add_xml_data(L2221.StubTech_en, "StubTech") %>%
      add_xml_data(L2221.GlobalTechInterp_en, "GlobalTechInterp") %>%
      add_xml_data(L2221.GlobalTechCoef_en, "GlobalTechCoef") %>%
      add_xml_data(L2221.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L2221.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L2221.GlobalTechCapture_en, "GlobalTechCapture") %>%
      add_xml_data(L2221.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2221.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_precursors("L2221.Supplysector_en",
                     "L2221.SubsectorLogit_en",
                     "L2221.SubsectorShrwtFllt_en",
                     "L2221.SubsectorInterp_en",
                     "L2221.StubTech_en",
                     "L2221.GlobalTechInterp_en",
                     "L2221.GlobalTechCoef_en",
                     "L2221.GlobalTechCost_en",
                     "L2221.GlobalTechShrwt_en",
                     "L2221.GlobalTechCapture_en",
                     "L2221.GlobalTechSCurve_en",
                     "L2221.GlobalTechProfitShutdown_en") ->
      en_transformation_biochar.xml

    return_data(en_transformation_biochar.xml)
  } else {
    stop("Unknown command")
  }
}
