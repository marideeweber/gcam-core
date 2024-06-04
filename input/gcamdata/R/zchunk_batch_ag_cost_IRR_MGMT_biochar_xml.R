# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_cost_IRR_MGMT_biochar_xml
#'
#' Construct XML data structure for \code{biochar_ag_cost_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{biochar_ag_cost_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_biochar_ag_demand.xml.R} (energy XML).
module_aglu_batch_ag_cost_IRR_MGMT_biochar_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2221.AgCost_ag_irr_mgmt"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "biochar_ag_cost_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2221.AgCost_ag_irr_mgmt <- get_data(all_data, "L2221.AgCost_ag_irr_mgmt")

    # ===================================================

    # Produce outputs
    create_xml("biochar_ag_cost_IRR_MGMT.xml") %>%
      add_xml_data(L2221.AgCost_ag_irr_mgmt, "AgCost") %>%
      add_precursors("L2221.AgCost_ag_irr_mgmt") ->
      biochar_ag_cost_IRR_MGMT.xml

    return_data(biochar_ag_cost_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
