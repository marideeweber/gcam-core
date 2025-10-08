# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2221.en_transformation_biochar
#'
#' Prepare the assumptions and calibrated outputs for energy transformation supplysectors, subsectors, and technologies.
#' @param command API command to execute
#' @return Depends on \code{command}: either a vector of required inputs
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2221.SectorLogitTables[[ curr_table ]]$data}, \code{L2221.Supplysector_en},
#' \code{L2221.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2221.SubsectorLogit_en}, },
#'  \code{L2221.SubsectorShrwtFllt_en}, \code{L2221.SubsectorInterp_en},
#'  \code{L2221.StubTech_en}, \code{L2221.GlobalTechInterp_en}, \code{L2221.GlobalTechCoef_en},
#'  \code{L2221.GlobalTechCost_en}, \code{L2221.GlobalTechShrwt_en}, \code{L2221.GlobalTechCapture_en},
#'  \code{L2221.GlobalTechSCurve_en},
#'  \code{L2221.GlobalTechProfitShutdown_en}. The corresponding file in the
#' original data system was \code{L222.en_transformation.R} (energy level2).
#' @details This chunk sets up the biochar sector as well as writing out assumptions to all regions for shareweights and logits.
#' Applies only to future years
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author MCB March 2021
module_energy_L2221.en_transformation_biochar <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_regions",
             FILE = "energy/A221.sector",
             FILE = "energy/A221.subsector_logit",
             FILE = "energy/A221.subsector_shrwt",
             FILE = "energy/A221.subsector_interp",
             FILE = "energy/A221.globaltech_coef",
             FILE = "energy/A221.globaltech_cost",
             FILE = "energy/A221.globaltech_shrwt",
             FILE = "energy/A221.globaltech_interp",
             FILE = "energy/A221.globaltech_co2capture",
             FILE = "energy/A221.globaltech_retirement"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2221.Supplysector_en",
             "L2221.SubsectorLogit_en",
             "L2221.SubsectorShrwtFllt_en",
             "L2221.SubsectorInterp_en",
             "L2221.StubTech_en",
             "L2221.GlobalTechInterp_en",
             "L2221.GlobalTechCoef_en",
             "L2221.GlobalTechCost_en",
             "L2221.GlobalTechShrwt_en",
             "L2221.GlobalTechCapture_en",
             "L2221.GlobalTechSCurve_en",
             "L2221.GlobalTechProfitShutdown_en"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silencing global variable package checks
    GCAM_region_ID <- calOutputValue <- coefficient <- fuel <- half.life <- input.cost <- lifetime <-
      median.shutdown.point <- minicam.energy.input <- minicam.non.energy.input <- object <-
      profit.shutdown.steepness <- region <- remove.fraction <- sector <- sector.name <- share.weight <-
      shutdown.rate <- steepness <- stub.technology <- subsector <- subsector.name <- supplysector <-
      technology <- to.value <- value <- year <- year.fillout <- year.share.weight <- year.x <- year.y <-
      primary.consumption <- NULL

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    A221.sector <- get_data(all_data, "energy/A221.sector", strip_attributes = TRUE)
    A221.subsector_logit <- get_data(all_data, "energy/A221.subsector_logit", strip_attributes = TRUE)
    A221.subsector_shrwt <- get_data(all_data, "energy/A221.subsector_shrwt", strip_attributes = TRUE)
    A221.subsector_interp <- get_data(all_data, "energy/A221.subsector_interp", strip_attributes = TRUE)
    A221.globaltech_coef <- get_data(all_data, "energy/A221.globaltech_coef", strip_attributes = TRUE)
    A221.globaltech_cost <- get_data(all_data, "energy/A221.globaltech_cost")
    A221.globaltech_shrwt <- get_data(all_data, "energy/A221.globaltech_shrwt", strip_attributes = TRUE)
    A221.globaltech_interp <- get_data(all_data, "energy/A221.globaltech_interp", strip_attributes = TRUE)
    A221.globaltech_co2capture <- get_data(all_data, "energy/A221.globaltech_co2capture")
    A221.globaltech_retirement <- get_data(all_data, "energy/A221.globaltech_retirement", strip_attributes = TRUE)
    # ===================================================

    # 2. Build tables for CSVs
    # 2a. Supplysector information
    # L2221.Supplysector_en: Supply sector information for energy transformation sectors

    A221.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2221.Supplysector_en # OUTPUT
    # 2b. Subsector information
    # L2221.SubsectorLogit_en: Subsector logit exponents of energy transformation sectors

    A221.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2221.SubsectorLogit_en # OUTPUT


    if(any(!is.na(A221.subsector_shrwt$year.fillout))) {
      A221.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]), GCAM_region_names) ->
        L2221.SubsectorShrwtFllt_en
    }

    # L2221.SubsectorInterp_en: Subsector shareweight interpolation of energy transformation sectors

    if(any(is.na(A221.subsector_interp$to.value))) {
      A221.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]]), GCAM_region_names) ->
        L2221.SubsectorInterp_en
    }

    # 2c. Technology information
    # L2221.StubTech_en: Identification of stub technologies of energy transformation

    # create list of regional stub.technologies
    A221.globaltech_shrwt %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]]), GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2221.StubTech_en # OUTPUT

    # L2221.GlobalTechInterp_en: Technology shareweight interpolation of energy transformation sectors
    A221.globaltech_interp %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # included to strip attributes from assumption file
      mutate(sector.name = sector.name) ->
      L2221.GlobalTechInterp_en # OUTPUT

    # L2221.GlobalTechCoef_en: Energy inputs and coefficients of global technologies for energy transformation
    A221.globaltech_coef %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) ->
      L2221.GlobalTechCoef_en
    # reorders columns to match expected model interface input
    L2221.GlobalTechCoef_en <- L2221.GlobalTechCoef_en[LEVEL2_DATA_NAMES[["GlobalTechCoef"]]] # OUTPUT

    # L2221.GlobalTechCost_en: Costs of global technologies for energy transformation
    A221.globaltech_cost %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, input.cost = value) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
      L2221.GlobalTechCost_en
    # reorders columns to match expected model interface input
    L2221.GlobalTechCost_en <- L2221.GlobalTechCost_en[LEVEL2_DATA_NAMES[["GlobalTechCost"]]] # OUTPUT

    # L2221.GlobalTechShrwt_en: Shareweights of global technologies for energy transformation
    A221.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2221.GlobalTechShrwt_en
    # reorders columns to match expected model interface input
    L2221.GlobalTechShrwt_en <- L2221.GlobalTechShrwt_en[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight")]

    # L2221.GlobalTechCapture_en: CO2 capture fractions from global technologies for energy transformation
    # No need to consider historical periods here
    A221.globaltech_co2capture %>%
      gather_years(value_col = "remove.fraction") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # Rounds the fraction to two digits and adds the name of the carbon storage market
      mutate(remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION), storage.market = energy.CO2.STORAGE.MARKET.biochar) ->
      L2221.GlobalTechCapture_en
    # reorders columns to match expected model interface input
    L2221.GlobalTechCapture_en <- L2221.GlobalTechCapture_en[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction", "storage.market")]

    # Retirement information
    A221.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2221.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L2221.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2221.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2221.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L2221.globaltech_retirement_future) ->
      L2221.globaltech_retirement # output

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2221.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed

    if(any(!is.na(L2221.globaltech_retirement$half.life))) {
      L2221.globaltech_retirement %>%
        filter(!is.na(L2221.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2221.GlobalTechSCurve_en
    }

    # L2221.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2221.globaltech_retirement$median.shutdown.point))) {
      L2221.globaltech_retirement %>%
        filter(!is.na(L2221.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2221.GlobalTechProfitShutdown_en
    }

    # ===================================================

    # Produce outputs

    L2221.Supplysector_en %>%
      add_title("Supply sector information for energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Written to all regions from A221.sector") %>%
      add_legacy_name("L2221.Supplysector_en") %>%
      add_precursors("energy/A221.sector", "common/GCAM_region_names") ->
      L2221.Supplysector_en

    L2221.SubsectorLogit_en %>%
      add_title("Subsector logit exponents of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Written to all regions from A221.subsector_logit") %>%
      add_legacy_name("L2221.SubsectorLogit_en") %>%
      add_precursors("energy/A221.subsector_logit", "common/GCAM_region_names") ->
      L2221.SubsectorLogit_en

    if(exists("L2221.SubsectorShrwtFllt_en")) {
      L2221.SubsectorShrwtFllt_en %>%
        add_title("Subsector shareweights of energy transformation sectors") %>%
        add_units("Unitless") %>%
        add_comments("Conditionally created from the subset of A221.subsector_shrwt with values in column 'year.fillout'.") %>%
        add_comments("by default contains all values from A221.subsector_shrwt") %>%
        add_legacy_name("L2221.SubsectorShrwtFllt_en") %>%
        add_precursors("energy/A221.subsector_shrwt", "common/GCAM_region_names") ->
        L2221.SubsectorShrwtFllt_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.SubsectorShrwtFllt_en") ->
        L2221.SubsectorShrwtFllt_en
    }

    if(exists("L2221.SubsectorInterp_en")) {
      L2221.SubsectorInterp_en %>%
        add_title("Subsector shareweight interpolation rules of energy transformation sectors") %>%
        add_units("Unitless") %>%
        add_comments("Conditionally created from the subset of A221.subsector_interp used to define regional shareweights interpolated to a year") %>%
        add_comments("by default contains all of A221.subsector_interp.") %>%
        add_legacy_name("L2221.SubsectorInterp_en") %>%
        add_precursors("energy/A221.subsector_interp", "common/GCAM_region_names") ->
        L2221.SubsectorInterp_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.SubsectorInterp_en") ->
        L2221.SubsectorInterp_en
    }

    L2221.StubTech_en %>%
      add_title("Identification of stub technologies of energy transformation") %>%
      add_units("Unitless") %>%
      add_comments("Writes out subset of stub technologies to all regions where those technologies exist") %>%
      add_comments("removes some first gen bio techs from regions where they do not exist") %>%
      add_legacy_name("L2221.StubTech_en") %>%
      add_precursors("energy/A221.globaltech_shrwt", "energy/A_regions") ->
      L2221.StubTech_en

    L2221.GlobalTechInterp_en %>%
      add_title("Technology shareweight interpolation of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("fills out model years in A221.globaltech_interp") %>%
      add_legacy_name("L2221.GlobalTechInterp_en") %>%
      add_precursors("energy/A221.globaltech_interp") ->
      L2221.GlobalTechInterp_en

    L2221.GlobalTechCoef_en %>%
      add_title("Energy inputs and coefficients of global technologies for energy transformation") %>%
      add_units("Coefficients") %>%
      add_comments("Historical and future values interpolated from coefficients in A221.globaltech_coef") %>%
      add_legacy_name("L2221.GlobalTechCoef_en") %>%
      add_precursors("energy/A221.globaltech_coef") ->
      L2221.GlobalTechCoef_en

    L2221.GlobalTechCost_en %>%
      add_title("Costs of global technologies for energy transformation") %>%
      add_units("1975USD/kg of bicoahr") %>%
      add_comments("Values interpolated to model years from assumptions in A221.globaltech_cost") %>%
      add_legacy_name("L2221.GlobalTechCost_en") %>%
      add_precursors("energy/A221.globaltech_cost") ->
      L2221.GlobalTechCost_en

    L2221.GlobalTechShrwt_en %>%
      add_title("Shareweights of global technologies for energy transformation") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights interpolated to model years from assumptions in A221.globaltech_shrwt") %>%
      add_legacy_name("L2221.GlobalTechShrwt_en") %>%
      add_precursors("energy/A221.globaltech_shrwt") ->
      L2221.GlobalTechShrwt_en

    L2221.GlobalTechCapture_en %>%
      add_title("CO2 capture fractions from global technologies for energy transformation") %>%
      add_units("Unitless") %>%
      add_comments("Fraction of CO2 captured by global CCS tech in energy transformation interpolated from assumptions in A221.globaltech_co2capture") %>%
      add_legacy_name("L2221.GlobalTechCapture_en") %>%
      add_precursors("energy/A221.globaltech_co2capture") ->
      L2221.GlobalTechCapture_en

    if(exists("L2221.GlobalTechSCurve_en")) {
      L2221.GlobalTechSCurve_en %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2221.GlobalTechSCurve_en") %>%
        add_precursors("energy/A221.globaltech_retirement") ->
        L2221.GlobalTechSCurve_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.GlobalTechSCurve_en") ->
        L2221.GlobalTechSCurve_en
    }

    if(exists("L2221.GlobalTechProfitShutdown_en")) {
      L2221.GlobalTechProfitShutdown_en %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2221.GlobalTechProfitShutdown_en") %>%
        add_precursors("energy/A221.globaltech_retirement") ->
        L2221.GlobalTechProfitShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2221.GlobalTechProfitShutdown_en") ->
        L2221.GlobalTechProfitShutdown_en
    }

    return_data(L2221.Supplysector_en, L2221.SubsectorLogit_en,
                L2221.SubsectorShrwtFllt_en, L2221.SubsectorInterp_en,
                L2221.StubTech_en, L2221.GlobalTechInterp_en, L2221.GlobalTechCoef_en, L2221.GlobalTechCost_en,
                L2221.GlobalTechShrwt_en, L2221.GlobalTechCapture_en,
                L2221.GlobalTechSCurve_en, L2221.GlobalTechProfitShutdown_en)
  } else {
    stop("Unknown command")
  }
}
