#' module_energy_refined_liquids_trade_xml
#'
#' Construct XML data structure for \code{refined_liquids_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{refined_liquids_trade.xml}.
module_energy_refined_liquids_trade_xml <- function(command, ...) {

  MODULE_INPUTS <-
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
      "L2262.TechCoef_refinedLiquids_reg",
      "L2262.Production_refinedLiquids_reg_imp",
      "L2262.Production_refinedLiquids_reg_dom")

  MODULE_OUTPUTS <-
    c(XML = "refined_liquids_trade.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # #=======#=======#=======#=======#=======#=======#=========

    Current_Envir <- environment()

    # remove trade data for refined oil
    lapply(MODULE_INPUTS, function(.df){
      .df %>% get %>%
        filter(!grepl("refined oil", supplysector)) %>%
        assign(x = .df, value = .,
               envir = Current_Envir)
    })


    # Produce outputs
    create_xml("refined_liquids_trade.xml") %>%
      add_logit_tables_xml(L2262.Supplysector_refinedLiquids_tra, "Supplysector") %>%
      add_xml_data(L2262.SectorUseTrialMarket_refinedLiquids_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L2262.SubsectorAll_refinedLiquids_tra, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2262.TechShrwt_refinedLiquids_tra, "TechShrwt") %>%
      add_xml_data(L2262.TechCost_refinedLiquids_tra, "TechCost") %>%
      add_xml_data(L2262.TechCoef_refinedLiquids_tra, "TechCoef") %>%
      add_xml_data(L2262.Production_refinedLiquids_tra, "Production") %>%
      add_logit_tables_xml(L2262.Supplysector_refinedLiquids_reg, "Supplysector") %>%
      add_logit_tables_xml(L2262.SubsectorAll_refinedLiquids_reg, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2262.TechShrwt_refinedLiquids_reg, "TechShrwt") %>%
      add_xml_data(L2262.TechCoef_refinedLiquids_reg, "TechCoef") %>%
      add_xml_data(L2262.Production_refinedLiquids_reg_imp, "Production") %>%
      add_xml_data(L2262.Production_refinedLiquids_reg_dom, "Production") %>%
      add_precursors(MODULE_INPUTS) ->
      refined_liquids_trade.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
