# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_biochar_co_products_xml
#'
#' Write district heat sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2221.TechSecOut_biochar}. The corresponding file in the
#' original data system was \code{L224.heat.R} (energy level2).
#' @details This chunk creates level 2 output files for district heat sector. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions that have district heat. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions. From the level 1 heat data, this chunk computes stub tech calibrated inputs, secondary
#' outputs from elec and modified costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select
#' @author JDH August 2017
module_energy_biochar_co_products_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A221.tech_secout"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "biochar_co_products.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    has_district_heat <- region <- year.fillout <- to.value <- efficiency <-
      technology <- coef <- subsector <- supplysector <- minicam.energy.input <-
      input.cost <- minicam.non.energy.input <- share.weight <- sector <- fuel <-
      value <- subs.share.weight <- calibrated.value <- secondary.output.name <-
      secondary.output <- stub.technology <- cost_modifier <- year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names") %>%
      filter(region == "USA")
    A221.tech_secout <- get_data(all_data, "energy/A221.tech_secout")

    # ===================================================
    # Secondary output of biochar production (slow pyrolisis): oil and natural gas
    # We use the ratios from Roberts et al 2010:
    # 35% of output is biochar (this mas 2.857142857 tons of switchgrass are needed for 1 ton of biochar)
    # 30% bio-oil
    # 35% syngas
    A221.tech_secout %>%
      repeat_add_columns(tibble(region = GCAM_region_names$region)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$TechSecOut)-> L2221.TechSecOut_biochar # OUTPUT

    # Add the cost for refined liquids enduse, since it is not reflected in the secondary output, and it is included in the en_supply file
    # when refining goes into refined liquids enduse, there is a non-enegry cost of 1.1. We add it here.

    # ===================================================

    # Produce outputs
    create_xml("biochar_co_products.xml") %>%
      add_xml_data(L2221.TechSecOut_biochar, "TechSecOut") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A221.tech_secout") ->
      biochar_co_products.xml


    return_data(biochar_co_products.xml)
  } else {
    stop("Unknown command")
  }
}
