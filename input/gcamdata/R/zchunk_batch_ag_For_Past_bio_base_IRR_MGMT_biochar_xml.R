# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_For_Past_bio_base_IRR_MGMT_biochar_xml
#'
#' Construct XML data structure for \code{biochar_ag_For_Past_bio_base_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{biochar_ag_For_Past_bio_base_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_biochar_ag_demand.xml.R} (energy XML).
module_aglu_batch_ag_For_Past_bio_base_IRR_MGMT_biochar_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2012.AgSupplySector", # No need to modify, just filter out what we do not use
             "L2012.AgSupplySubsector", # No need to modify, just filter out what we do not use
             "L2221.AgProduction_ag_irr_mgmt",
             "L2221.AgHAtoCL_irr_mgmt",
             "L2221.AgYield_biochar_ref"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "biochar_ag_For_Past_bio_base_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2012.AgSupplySector <- get_data(all_data, "L2012.AgSupplySector")
    L2012.AgSupplySubsector <- get_data(all_data, "L2012.AgSupplySubsector")
    L2221.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2221.AgProduction_ag_irr_mgmt")
    L2221.AgHAtoCL_irr_mgmt <- get_data(all_data, "L2221.AgHAtoCL_irr_mgmt")
    L2221.AgYield_biochar_ref <- get_data(all_data, "L2221.AgYield_biochar_ref")

    # Filter out supply sector tables (delete forest and pasture)
    # TODO: filter again
    ## L2221.AgSupplySector
    L2012.AgSupplySector %>%
      # Filter out forest and pasture, since these do not demand biochar for now
      filter(AgSupplySector %in% L2221.AgProduction_ag_irr_mgmt$AgSupplySector,
             region %in% L2221.AgProduction_ag_irr_mgmt$region) -> L2221.AgSupplySector # OUTPUT

    ## L2221.AgSupplySubsector
    L2012.AgSupplySubsector %>%
      # Filter out forest and pasture, since these do not demand biochar for now
      filter(AgSupplySubsector %in% L2221.AgProduction_ag_irr_mgmt$AgSupplySubsector,
             region %in% L2221.AgProduction_ag_irr_mgmt$region) -> L2221.AgSupplySubsector # OUTPUT


    # ===================================================

    # Produce outputs
    create_xml("biochar_ag_For_Past_bio_base_IRR_MGMT.xml") %>%
      add_logit_tables_xml(L2221.AgSupplySector, "AgSupplySector") %>%
      add_logit_tables_xml(L2221.AgSupplySubsector, "AgSupplySubsector") %>%
      add_xml_data(L2221.AgProduction_ag_irr_mgmt, "AgProduction") %>%
      add_xml_data(L2221.AgHAtoCL_irr_mgmt, "AgHAtoCL") %>%
      add_xml_data(L2221.AgYield_biochar_ref, "AgYield") %>%
      add_precursors("L2012.AgSupplySector",
                     "L2012.AgSupplySubsector",
                     "L2221.AgProduction_ag_irr_mgmt",
                     "L2221.AgHAtoCL_irr_mgmt",
                     "L2221.AgYield_biochar_ref") ->
      biochar_ag_For_Past_bio_base_IRR_MGMT.xml

    return_data(biochar_ag_For_Past_bio_base_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
