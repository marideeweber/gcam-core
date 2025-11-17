# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_Weathering_xml
#'
#' Construct XML data structure for \code{Weathering.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Weathering.xml}.
module_energy_batch_Weathering_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L263.Rsrc",
              "L263.RsrcCurves_C",
              "L263.ResTechShrwt_C",
              "L263.Supplysector_C",
              "L263.SubsectorLogit_C",
              "L263.SubsectorShrwtFllt_C",
              "L263.StubTech_C",
              "L263.GlobalTechCoef_C",
              "L263.GlobalTechCost_C",
              "L263.GlobalTechShrwt_C",
              "L263.RsrcPrice",
              "L263.WeatheringRsrcMax",
              "L263.GlobalTechCSeq",
              "L263.SubsectorInterp",
              "L263.GlobalTechInputPMult",
             "L263.GlobalTechSCurve",
             "L263.GlobalTechProfitShutdown"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Weathering.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L263.Rsrc <- get_data(all_data, "L263.Rsrc")
    L263.RsrcCurves_C <- get_data(all_data, "L263.RsrcCurves_C")
    L263.ResTechShrwt_C <- get_data(all_data, "L263.ResTechShrwt_C")
    L263.Supplysector_C <- get_data(all_data, "L263.Supplysector_C")
    L263.SubsectorLogit_C <- get_data(all_data, "L263.SubsectorLogit_C")
    L263.SubsectorShrwtFllt_C <- get_data(all_data, "L263.SubsectorShrwtFllt_C")
    L263.StubTech_C <- get_data(all_data, "L263.StubTech_C")
    L263.GlobalTechCoef_C <- get_data(all_data, "L263.GlobalTechCoef_C")
    L263.GlobalTechCost_C <- get_data(all_data, "L263.GlobalTechCost_C")
    L263.GlobalTechShrwt_C <- get_data(all_data, "L263.GlobalTechShrwt_C")
    L263.RsrcPrice <- get_data(all_data, "L263.RsrcPrice")
    L263.WeatheringRsrcMax <- get_data(all_data, "L263.WeatheringRsrcMax")
    L263.GlobalTechCSeq <- get_data(all_data, "L263.GlobalTechCSeq")
    L263.SubsectorInterp <- get_data(all_data, "L263.SubsectorInterp")
    L263.GlobalTechInputPMult <- get_data(all_data, "L263.GlobalTechInputPMult")
    L263.GlobalTechSCurve <- get_data(all_data, "L263.GlobalTechSCurve")
    L263.GlobalTechProfitShutdown <- get_data(all_data, "L263.GlobalTechProfitShutdown")
    # ===================================================

    # Produce outputs
    create_xml("Weathering.xml") %>%
      add_xml_data(L263.Rsrc, "RenewRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L263.RsrcCurves_C, "GrdRenewRsrcCurves") %>%
      add_xml_data(L263.ResTechShrwt_C, "ResTechShrwt") %>%
      add_logit_tables_xml(L263.Supplysector_C, "Supplysector") %>%
      add_logit_tables_xml(L263.SubsectorLogit_C, "SubsectorLogit") %>%
      add_xml_data(L263.SubsectorShrwtFllt_C, "SubsectorShrwtFllt") %>%
      add_xml_data(L263.StubTech_C, "StubTech") %>%
      add_xml_data(L263.GlobalTechCoef_C, "GlobalTechCoef") %>%
      add_xml_data(L263.GlobalTechCost_C, "GlobalTechCost") %>%
      add_xml_data(L263.GlobalTechShrwt_C, "GlobalTechShrwt") %>%
      add_xml_data(L263.RsrcPrice, "RenewRsrcPrice") %>%
      add_xml_data(L263.WeatheringRsrcMax, "GrdRenewRsrcMax") %>%
      add_xml_data(L263.SubsectorInterp, "SubsectorInterp") %>%
      add_xml_data(L263.GlobalTechCSeq, "GlobalTechCSeq") %>%
      add_xml_data(L263.GlobalTechInputPMult, "GlobalTechInputPMult") %>%
      add_xml_data(L263.GlobalTechSCurve, "GlobalTechSCurve") %>%
      add_xml_data(L263.GlobalTechProfitShutdown, "GlobalTechProfitShutdown") %>%
      add_precursors("L263.Rsrc", "L263.RsrcCurves_C", "L263.ResTechShrwt_C", "L263.Supplysector_C", "L263.SubsectorLogit_C", "L263.SubsectorShrwtFllt_C", "L263.StubTech_C", "L263.GlobalTechCoef_C","L263.GlobalTechCost_C", "L263.GlobalTechShrwt_C","L263.RsrcPrice","L263.WeatheringRsrcMax","L263.GlobalTechCSeq","L263.SubsectorInterp",
                     "L263.GlobalTechInputPMult","L263.GlobalTechProfitShutdown","L263.GlobalTechSCurve") ->
      Weathering.xml

    return_data(Weathering.xml)
  } else {
    stop("Unknown command")
  }
}
