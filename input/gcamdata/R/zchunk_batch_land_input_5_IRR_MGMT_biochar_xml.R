# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_land_input_5_IRR_MGMT_biochar_xml
#'
#' Construct XML data structure for \code{biochar_land_input_5_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{biochar_land_input_5_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_biochar_land_input_5_IRR_MGMT.xml.R} (energy XML).
module_aglu_batch_land_input_5_IRR_MGMT_biochar_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2252.LN5_Logit", # Logit is at node level, so no change
             "L2221.LN5_HistMgdAllocation_crop_biochar",
             "L2221.LN5_MgdAllocation_crop_biochar",
             "L2221.LN5_MgdCarbon_crop_biochar",
             "L2221.LN5_LeafGhostShare"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "biochar_land_input_5_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2252.LN5_Logit <- get_data(all_data, "L2252.LN5_Logit")
    L2221.LN5_HistMgdAllocation_crop_biochar <- get_data(all_data, "L2221.LN5_HistMgdAllocation_crop_biochar")
    L2221.LN5_MgdAllocation_crop_biochar <- get_data(all_data, "L2221.LN5_MgdAllocation_crop_biochar")
    L2221.LN5_MgdCarbon_crop_biochar <- get_data(all_data, "L2221.LN5_MgdCarbon_crop_biochar")
    L2221.LN5_LeafGhostShare <- get_data(all_data, "L2221.LN5_LeafGhostShare")

    # Filter logit table (delete entries that are not relevant to biochar)
    ## L2221.AgSupplySector
    L2252.LN5_Logit %>%
      # Filter out forest and pasture, since these do not demand biochar for now
      filter(LandNode5 %in% L2221.LN5_MgdAllocation_crop_biochar$LandNode5,
             region %in% L2221.LN5_MgdAllocation_crop_biochar$region) -> L2221.LN5_Logit # OUTPUT


    # ===================================================

    # Produce outputs
    create_xml("biochar_land_input_5_IRR_MGMT.xml") %>%
      add_logit_tables_xml(L2221.LN5_Logit, "LN5_Logit") %>%
      add_xml_data(L2221.LN5_HistMgdAllocation_crop_biochar, "LN5_HistMgdAllocation") %>%
      add_xml_data(L2221.LN5_MgdAllocation_crop_biochar, "LN5_MgdAllocation") %>%
      add_xml_data(L2221.LN5_MgdCarbon_crop_biochar, "LN5_SoilTimeScale") %>%
      add_xml_data(L2221.LN5_LeafGhostShare, "LN5_LeafGhostShare") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L2252.LN5_Logit",
                     "L2221.LN5_HistMgdAllocation_crop_biochar",
                     "L2221.LN5_MgdAllocation_crop_biochar",
                     "L2221.LN5_MgdCarbon_crop_biochar",
                     "L2221.LN5_LeafGhostShare") ->
      biochar_land_input_5_IRR_MGMT.xml

    return_data(biochar_land_input_5_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
