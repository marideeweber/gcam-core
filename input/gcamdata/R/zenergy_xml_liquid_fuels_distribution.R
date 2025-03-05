#' module_energy_liquid_fuels_distribution_xml
#'
#' Construct XML data structure for \code{liquid_fuels_distribution.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquid_fuels_distribution.xml}.
module_energy_liquid_fuels_distribution_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2261.Supplysector_en",
      "L2261.SubsectorLogit_en",
      "L2261.SubsectorShrwtFllt_en",
      "L2261.SubsectorInterp_en",
      "L2261.StubTech_en",
      "L2261.GlobalTechEff_en",
      "L2261.GlobalTechCost_en",
      "L2261.GlobalTechShrwt_en",
      "L2261.StubTechInterp_en",
      "L2261.StubTechProd_gsln",
      "L2261.StubTechProd_refliqend",
      #"L2261.StubTechFixOut_otherBiod"
      "L254.StubTranTechCalInput"
      )

  MODULE_OUTPUTS <-
    c(XML = "liquid_fuels_distribution.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # #=======#=======#=======#=======#=======#=======#=========


    # Produce outputs
    create_xml("liquid_fuels_distribution.xml") %>%
      add_logit_tables_xml(L2261.Supplysector_en, "Supplysector") %>%
      add_logit_tables_xml(L2261.SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L2261.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") %>%
      add_xml_data(L2261.SubsectorInterp_en, "SubsectorInterp") %>%
      add_xml_data(L2261.StubTech_en, "StubTech") %>%
      add_xml_data(L2261.GlobalTechEff_en, "GlobalTechEff") %>%
      add_xml_data(L2261.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L2261.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L2261.StubTechInterp_en, "StubTechInterp") %>%
      #add_xml_data(L2261.StubTechProd_gsln, "StubTechProd") %>%
      #add_xml_data(L2261.StubTechProd_refliqend, "StubTechProd") %>%
      #add_xml_data(L2261.StubTechFixOut_otherBiod, "StubTechFixOut_otherBiod") %>%
      add_precursors(MODULE_INPUTS) ->
      liquid_fuels_distribution.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
