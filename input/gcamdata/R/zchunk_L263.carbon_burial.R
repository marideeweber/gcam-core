# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L263.carbon_burial
#'
#' Calculate carbon storage resource supply curves, shareweights, technology coefficients and costs, and other carbon storage information.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L263.Rsrc}, \code{L263.UnlimitRsrc}, \code{L263.RsrcCurves_C}, \code{L263.SectorLogitTables[[ curr_table ]]$data}, \code{L263.Supplysector_C}, \code{L263.SubsectorLogitTables[[ curr_table ]]$data}, \code{L263.SubsectorLogit_C}, \code{L263.SubsectorShrwtFllt_C}, \code{L263.StubTech_C}, \code{L263.GlobalTechCoef_C}, \code{L263.GlobalTechCost_C}, \code{L263.GlobalTechShrwt_C}, \code{L263.GlobalTechCost_C_High}, \code{L263.GlobalTechShrwt_C_nooffshore}, \code{L263.RsrcCurves_C_high}, \code{L263.RsrcCurves_C_low}, \code{L263.RsrcCurves_C_lowest}. The corresponding file in the
#' original data system was \code{L263.Cstorage.R} (energy level2).
#' @details The following tables pertaining to carbon storage properties are generated:
#' \itemize{
#'  \item{Carbon storage information}
#'  \item{Unlimited carbon storage information}
#'  \item{Supply curve of carbon storage resources}
#'  \item{High supply curve of onshore carbon storage resources}
#'  \item{Low supply curve of onshore carbon storage resources}
#'  \item{Lowest supply curve of onshore carbon storage resources}
#'  \item{Carbon storage sector information}
#'  \item{Subsector logit exponents of carbon storage sector}
#'  \item{Subsector shareweights of carbon storage sectors}
#'  \item{Identification of stub technologies of carbon storage}
#'  \item{Carbon storage global technology coefficients across base model years}
#'  \item{Carbon storage global technology costs across base model years}
#'  \item{Carbon storage global technology costs across base model years, high price scenario}
#'  \item{Shareweights of carbon storage technologies across base model years}
#'  \item{Shareweights of offshore carbon storage technologies}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter mutate select
#' @importFrom tidyr complete nesting
#' @author AJS August 2017
module_energy_L263.carbon_burial <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A63.rsrc_info",
             FILE = "energy/A63.sector",
             FILE = "energy/A63.subsector_logit",
             FILE = "energy/A63.subsector_shrwt",
             FILE = "energy/A63.globaltech_coef",
             FILE = "energy/A63.globaltech_cost",
             FILE = "energy/A63.globaltech_shrwt",
             FILE = "energy/A63.nonenergy_Cseq",
             FILE = "energy/A63.subsector_interp",
             FILE = "energy/A63.globaltech_retirement"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L263.SubsectorLogit_C",
             "L263.SubsectorShrwtFllt_C",
             "L263.StubTech_C",
             "L263.GlobalTechCoef_C",
             "L263.GlobalTechCost_C",
             "L263.GlobalTechShrwt_C",
             "L263.GlobalTechCSeq",
             "L263.SubsectorInterp",
             "L263.GlobalTechInputPMult"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names") %>% filter(region == "USA")
    A63.subsector_logit <- get_data(all_data, "energy/A63.subsector_logit", strip_attributes = TRUE)
    A63.subsector_shrwt <- get_data(all_data, "energy/A63.subsector_shrwt", strip_attributes = TRUE)
    A63.globaltech_coef <- get_data(all_data, "energy/A63.globaltech_coef",strip_attributes = TRUE)
    A63.globaltech_cost <- get_data(all_data, "energy/A63.globaltech_cost",strip_attributes = TRUE)
    A63.globaltech_shrwt <- get_data(all_data, "energy/A63.globaltech_shrwt", strip_attributes = TRUE)
    A63.nonenergy_Cseq <- get_data(all_data, "energy/A63.nonenergy_Cseq", strip_attributes = TRUE)
    A63.subsector_interp <- get_data(all_data, "energy/A63.subsector_interp", strip_attributes = TRUE)

    # ===================================================

    # Silence package notes
    . <- available <- capacity.factor <- curr_table <- extractioncost <-
      grade <- logit.type <- minicam.energy.input <- minicam.non.energy.input <-
      `output-unit` <- `price-unit` <- resource <- resource_type <- share.weight <-
      subresource <- subsector <- subsector.name <- supplysector <- technology <-
      value <- year <- region <- resource <- output.unit <- price.unit <-
      market <- logit.exponent <- coefficient <- input.cost <- NULL


    # D
    # Subsector information

    # Subsector logit exponents of carbon storage sector
    A63.subsector_logit %>%
      mutate(logit.exponent = as.numeric(logit.exponent)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L263.SubsectorLogit_C # This is a final output table.


    # Subsector shareweights of carbon storage sectors
    A63.subsector_shrwt %>%
      mutate(share.weight = as.numeric(share.weight)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L263.SubsectorShrwtFllt_C # This is a final output table.

    A63.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      L263.SubsectorInterp


    # E
    # Technology information
    # Identification of stub technologies of carbon storage
    # A63.globaltech_shrwt reports carbon storage technology shareweights
    # Note: assuming that the technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A63.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]],
                           GCAM_region_names = GCAM_region_names) %>%
      select(region, supplysector, subsector, stub.technology = technology) ->
      L263.StubTech_C # This is a final output table.

    # Energy inputs and coefficients of global technologies for carbon storage
    # A63.globaltech_coef reports carbon storage global technology coefficients
    A63.globaltech_coef %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.energy.input, coefficient) ->
      L263.GlobalTechCoef_C # This is a final output table.

    A63.globaltech_coef %>%
      filter(!is.na(price.unit.conversion)) %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(price.unit.conversion = approx_fun(year, price.unit.conversion, rule = 2)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInputPMult"]]) %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) -> L263.GlobalTechInputPMult

    # Costs of global technologies
    # A61.globaltech_cost reports carbon storage offshore storage cost (1975$/tCO2)
    A63.globaltech_cost %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.non.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology) %>%
      mutate(input.cost = approx_fun(year, value, rule = 2)) %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      ungroup() %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.non.energy.input, input.cost) ->
      L263.GlobalTechCost_C # This is a final output table.

    # Shareweights of global technologies for energy transformation
    A63.globaltech_shrwt %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight) ->
      L263.GlobalTechShrwt_C # This is a final output table.

    # Use zero shareweights for offshore storage
    L263.GlobalTechShrwt_C %>%
      filter(subsector.name == "offshore carbon-storage") %>%
      mutate(share.weight = 0) ->
      L263.GlobalTechShrwt_C_nooffshore # This is a final output table.

    A63.nonenergy_Cseq %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCSeq"]]) ->
      L263.GlobalTechCSeq

    # ===================================================


    L263.SubsectorLogit_C %>%
      add_title("Subsector logit exponents of carbon storage sector") %>%
      add_units("Unitless") %>%
      add_comments("Table on subsector logit exponents was expanded to include GCAM region names") %>%
      add_legacy_name("L263.SubsectorLogit_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A63.subsector_logit") ->
      L263.SubsectorLogit_C

    L263.SubsectorShrwtFllt_C %>%
      add_title("Subsector shareweights of carbon storage sectors") %>%
      add_units("Unitless") %>%
      add_comments("Table on subsector shareweights was expanded to include GCAM region names") %>%
      add_legacy_name("L263.SubsectorShrwtFllt_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A63.subsector_shrwt") ->
      L263.SubsectorShrwtFllt_C

    L263.StubTech_C %>%
      add_title("Identification of stub technologies of carbon storage") %>%
      add_units("Not Applicable") %>%
      add_comments("Technology list in the global shareweight table for carbon storage was expanded to include GCAM regions") %>%
      add_legacy_name("L263.StubTech_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A63.globaltech_shrwt") ->
      L263.StubTech_C

    L263.GlobalTechCoef_C %>%
      add_title("Carbon storage global technology coefficients across base model years") %>%
      add_units("Unitless") %>%
      add_comments("Global technology coefficients were interpolated across all base model years") %>%
      add_legacy_name("L263.GlobalTechCoef_C") %>%
      add_precursors("energy/A63.globaltech_coef") ->
      L263.GlobalTechCoef_C

    L263.GlobalTechCost_C %>%
      add_title("Carbon storage global technology costs across base model years") %>%
      add_units("1975$/kg") %>%
      add_comments("Global technology coefficients were interpolated across all base model years") %>%
      add_legacy_name("L263.GlobalTechCost_C") %>%
      add_precursors("energy/A63.globaltech_cost") ->
      L263.GlobalTechCost_C

    L263.GlobalTechShrwt_C %>%
      add_title("Shareweights of carbon storage technologies across base model years") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights of global technologies for energy transformation were interpolated across all base model years") %>%
      add_legacy_name("L263.GlobalTechShrwt_C") %>%
      add_precursors("energy/A63.globaltech_shrwt") ->
      L263.GlobalTechShrwt_C


    L263.GlobalTechCSeq %>%
      add_title("CO2 capture fractions for rock carbon_burial") %>%
      add_units("Unitless") %>%
      add_comments("Remove fractions from A63.nonenergy_Cseq are expanded into all model years") %>%
      add_precursors("energy/A63.nonenergy_Cseq") ->
      L263.GlobalTechCSeq

    L263.SubsectorInterp %>%
      add_title("Subsector shareweight interpolation of carbon_burial subsector") %>%
      add_units("NA") %>%
      add_comments("For enhanced carbon_burial sector, the subsector shareweight interpolation function infromation from A63.subsector_interp is expanded into all GCAM regions") %>%
      add_precursors("energy/A63.subsector_interp", "common/GCAM_region_names") ->
      L263.SubsectorInterp

    L263.GlobalTechInputPMult %>%
      add_title("Price conversion from transportation technologies") %>%
      add_comments("converts from $1990/tkm to $1975$/EJ") %>%
      add_units("Unitless") ->
      L263.GlobalTechInputPMult



    return_data(L263.SubsectorLogit_C,
                L263.SubsectorShrwtFllt_C,
                L263.StubTech_C,
                L263.GlobalTechCoef_C,
                L263.GlobalTechCost_C,
                L263.GlobalTechShrwt_C,
                L263.GlobalTechCSeq,
                L263.SubsectorInterp,
                L263.GlobalTechInputPMult)
  } else {
    stop("Unknown command")
  }
}
