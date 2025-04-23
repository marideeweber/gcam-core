# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_carbon_burial_xml
#'
#' Construct XML data structure for \code{carbon_burial.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{carbon_burial.xml}.
module_energy_batch_carbon_burial_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L263.SubsectorLogit_C",
              "L263.SubsectorShrwtFllt_C",
              "L263.StubTech_C",
              "L263.GlobalTechCoef_C",
              "L263.GlobalTechCost_C",
              "L263.GlobalTechShrwt_C",
              "L263.GlobalTechCSeq",
              "L263.SubsectorInterp",
              "L263.GlobalTechInputPMult"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "carbon_burial.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L263.SubsectorLogit_C <- get_data(all_data, "L263.SubsectorLogit_C")
    L263.SubsectorShrwtFllt_C <- get_data(all_data, "L263.SubsectorShrwtFllt_C")
    L263.StubTech_C <- get_data(all_data, "L263.StubTech_C")
    L263.GlobalTechCoef_C <- get_data(all_data, "L263.GlobalTechCoef_C")
    L263.GlobalTechCost_C <- get_data(all_data, "L263.GlobalTechCost_C")
    L263.GlobalTechShrwt_C <- get_data(all_data, "L263.GlobalTechShrwt_C")
    L263.GlobalTechCSeq <- get_data(all_data, "L263.GlobalTechCSeq")
    L263.SubsectorInterp <- get_data(all_data, "L263.SubsectorInterp") %>%
      mutate(interpolation.function = if_else(subsector == 'biomass burial' &
                                                from.year == 2025,'s-curve',interpolation.function))
    L263.GlobalTechInputPMult <- get_data(all_data, "L263.GlobalTechInputPMult")
    # ===================================================

    # Produce outputs
    create_xml("carbon_burial.xml") %>%
      add_logit_tables_xml(L263.SubsectorLogit_C, "SubsectorLogit") %>%
      add_xml_data(L263.SubsectorShrwtFllt_C, "SubsectorShrwtFllt") %>%
      add_xml_data(L263.StubTech_C, "StubTech") %>%
      add_xml_data(L263.GlobalTechCoef_C, "GlobalTechCoef") %>%
      add_xml_data(L263.GlobalTechCost_C, "GlobalTechCost") %>%
      add_xml_data(L263.GlobalTechShrwt_C, "GlobalTechShrwt") %>%
      add_xml_data(L263.SubsectorInterp, "SubsectorInterp") %>%
      add_xml_data(L263.GlobalTechCSeq, "GlobalTechCSeq") %>%
      add_xml_data(L263.GlobalTechInputPMult, "GlobalTechInputPMult") %>%
      add_precursors("L263.SubsectorLogit_C", "L263.SubsectorShrwtFllt_C", "L263.StubTech_C", "L263.GlobalTechCoef_C","L263.GlobalTechCost_C", "L263.GlobalTechShrwt_C", "L263.GlobalTechCSeq","L263.SubsectorInterp",
                     "L263.GlobalTechInputPMult") ->
      carbon_burial.xml

    return_data(carbon_burial.xml)
  } else {
    stop("Unknown command")
  }
}
