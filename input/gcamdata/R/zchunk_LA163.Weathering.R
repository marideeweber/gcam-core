# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA161.Cstorage
#'
#' Build carbon storage supply curves by region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.RsrcCurves_MtC_R}. The corresponding file in the
#' original data system was \code{LA161.Cstorage.R} (energy level1).
#' @details Build enhanced weathering supply curves by region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by mutate select summarise
#' @importFrom tidyr gather
#' @author JF Sept 2021
module_energy_LA163.Weathering <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(#FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             #FILE = "aglu/LDS/Land_type_area_ha",
             #FILE = "energy/AlkalineMaterial_Mt",
             #FILE = "energy/A63.AlkalineMaterial_curves",
             FILE = "energy/A63.rsrc_curves"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L163.RsrcCurves_Mt"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    GCAM_region_ID <- available <- cost_2005USDtCO2 <- fraction <- grade <- iso <-
      region_GCAM3 <- share <- value <- variable <- year <- NULL

    # Load required inputs
    #iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    #Land_type_area_ha <- get_data(all_data, "aglu/LDS/Land_type_area_ha")
    #A63.AlkalineMaterial_curves <- get_data(all_data, "energy/A63.AlkalineMaterial_curves")
    A63.rsrc_curves <- get_data(all_data, "energy/A63.rsrc_curves")


    A63.rsrc_curves %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      select(-region) %>%
      rename(extractioncost = cost) -> L163.RsrcCurves_Mt




#    AlkalineMaterial_Mt <- get_data(all_data, "energy/AlkalineMaterial_Mt") %>%
#      gather(variable, value, -region_GCAM3)

#    AlkalineMaterial_Mt %>%
#      group_by(region_GCAM3) %>%
#      summarise(value = sum(value)) %>%
#      ungroup() %>%
      # Repeat by number of grades, and multiply by the fraction of the total resource assigned to each grade
#      repeat_add_columns(tibble(grade = unique(A63.AlkalineMaterial_curves$grade))) %>%
#      left_join_error_no_match(A63.AlkalineMaterial_curves, by = "grade") %>%
#      mutate(available = value * fraction) -> L163.RsrcCurves_Mt

    # Downscaling GCAM 3.0 carbon storage supply curves to countries on the basis of land area
    # Calculate land cover shares of GCAM regions within region_GCAM3
#    L163.LC_bm2_ctry <- Land_type_area_ha %>%
#      filter(year == max(year)) %>%
#      group_by(iso) %>%
      # Total land per country
#      summarise(value = sum(value * CONV_HA_BM2)) %>%
#      ungroup() %>%
      # Add in GCAM3 region and calculate shares of countries within region
#      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
#      group_by(region_GCAM3) %>%
#      mutate(share = value / sum(value)) %>%
#      ungroup()

    # Repeat by number of grades, and match in the available quantities
#    L163.LC_bm2_ctry %>%
#      repeat_add_columns(tibble(grade = unique(A63.AlkalineMaterial_curves$grade))) %>%
#      left_join_error_no_match(L163.RsrcCurves_Mt, by = c("region_GCAM3","grade")) %>%
      # The carbon storage quantities from the literature are in CO2; for GCAM we convert to C.
      # Aggregate to GCAM 4 regions
#      group_by(GCAM_region_ID, grade, cost_2005USDtCO2) %>%
#      mutate(available = available * share / emissions.CONV_C_CO2) %>%
#      summarise(available = sum(available)) %>%
#      mutate(resource = unique(A63.AlkalineMaterial_curves$resource),
#             subresource = unique(A63.AlkalineMaterial_curves$subresource),
#             extractioncost = round(cost_2005USDtCO2 * emissions.CONV_C_CO2 / gdp_deflator(2005, 1975), energy.DIGITS_COST))%>%
#      ungroup() -> L163.RsrcCurves_Mt



    # Building carbon storage supply curves

    # Produce output
    L163.RsrcCurves_Mt  %>%
      #add_title("Silicate resource supply curves by GCAM region") %>%
      add_units("Mt") %>%
      add_comments("Supply curves for alkaline resource for use in enhanced weathering") %>%
      add_comments("Country supply curves aggregated to GCAM4 region") %>%
      add_precursors("common/GCAM_region_names","energy/A63.rsrc_curves") %>%
      add_legacy_name("L163.RsrcCurves_Mt")  ->
      L163.RsrcCurves_Mt

    return_data(L163.RsrcCurves_Mt)
  } else {
    stop("Unknown command")
  }
}
