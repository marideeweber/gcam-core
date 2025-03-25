#' module_energy_L2261.liquid_fuels_distribution
#'
#' Prepare the assumptions and calibrated outputs for motor gasoline ("refined liquids gasoline pool")
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2261.Supplysector_en},
#' \code{L2261.SubsectorLogit_en}, \code{L2261.SubsectorShrwtFllt_en}, \code{L2261.SubsectorInterp_en},
#'  \code{L2261.StubTech_en}, \code{L2261.GlobalTechEff_en}, \code{L2261.GlobalTechCost_en},
#'  \code{L2261.GlobalTechShrwt_en}, \code{L2261.StubTechProd_gsln}.
#' @details This chunk modifies the parameterization of subsectors and technologies in the USA refining sector as per
#'   data provided by OTAQ
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author GPK March 2019
module_energy_L2261.liquid_fuels_distribution <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/A_regions",
      FILE = "energy/A26.globaltech_eff",
      FILE = "energy/A261.sector",
      FILE = "energy/A261.subsector_logit",
      FILE = "energy/A261.subsector_shrwt",
      FILE = "energy/A261.subsector_interp",
      FILE = "energy/A261.globaltech_eff",
      FILE = "energy/A261.globaltech_cost",
      FILE = "energy/A261.globaltech_shrwt",
      FILE = "energy/A261.globaltech_interp",
      #"L122.StubTechFixOut_otherBiod",
      "L244.StubTechCalInput_bld",
      "L254.StubTranTechCalInput",
      "L2262.Production_refinedLiquids_reg_imp",
      "L2262.Production_refinedLiquids_reg_dom",
      FILE = "energy/bio_feed_mapping",
      "L121.share_R_TPES_biofuel_tech")

  MODULE_OUTPUTS <-
    c("L2261.Supplysector_en",
      "L2261.SubsectorLogit_en",
      "L2261.SubsectorShrwtFllt_en",
      "L2261.SubsectorInterp_en",
      "L2261.StubTech_en",
      "L2261.GlobalTechEff_en",
      "L2261.GlobalTechCost_en",
      "L2261.GlobalTechShrwt_en",
      "L2261.StubTechInterp_en",
      #"L2261.StubTechFixOut_otherBiod",
      "L2261.StubTechProd_gsln",
      "L2261.StubTechProd_refliqend")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silencing global variable package checks
    year <- year.fillout <- technology <- supplysector <- subsector <- minicam.energy.input <-
      efficiency <- minicam.non.energy.input <- input.cost <- share.weight <- region <-
      calOutputValue <- calibrated.value <- total_gsln <- ethanol <- subs.share.weight <- NULL

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # #=======#=======#=======#=======#=======#=======#=========

    # 2. Build tables for CSVs

    L2261.Supplysector_en <- write_to_all_regions(A261.sector,
                                                  c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                                                  GCAM_region_names)

    L2261.SubsectorLogit_en <- write_to_all_regions(A261.subsector_logit,
                                                    c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                                                    GCAM_region_names)

    if(any(!is.na(A261.subsector_shrwt$year))){
      L2261.SubsectorShrwt_en <- write_to_all_regions(filter(A261.subsector_shrwt, !is.na(year)),
                                                      LEVEL2_DATA_NAMES[["SubsectorShrwt"]],
                                                      GCAM_region_names)
    }

    if(any(!is.na(A261.subsector_shrwt$year.fillout))) {
      L2261.SubsectorShrwtFllt_en <- write_to_all_regions(filter(A261.subsector_shrwt, !is.na(year.fillout)),
                                                      LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],
                                                      GCAM_region_names)
    }

    if(any(is.na(A261.subsector_interp$to.value))){
      L2261.SubsectorInterp_en <- write_to_all_regions(filter(A261.subsector_interp, is.na(A261.subsector_interp$to.value)),
                                                       LEVEL2_DATA_NAMES[["SubsectorInterp"]],
                                                       GCAM_region_names)
    }
    if(any(!is.na(A261.subsector_interp$to.value))){
      L2261.SubsectorInterpTo_en <- write_to_all_regions(filter(A261.subsector_interp, !is.na(A261.subsector_interp$to.value)),
                                                         LEVEL2_DATA_NAMES[["names_SubsectorInterpTo"]],
                                                         GCAM_region_names)
    }

    # 2c. Technology information

    L2261.StubTech_en <- write_to_all_regions(A261.globaltech_shrwt,
                                              LEVEL2_DATA_NAMES[["Tech"]],
                                              GCAM_region_names) %>%
      rename(stub.technology = technology)


    # L2261.GlobalTechEff_en: Energy inputs and efficiencies of ref liq gasoline pool technologies
    A261.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(efficiency = round(approx_fun(year, efficiency, rule = 1), energy.DIGITS_EFFICIENCY)) %>%
      ungroup() %>%
      drop_na() %>% # any model years outside the envelope of years with provided values are dropped
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]])->
      L2261.GlobalTechEff_en

    # L2261.GlobalTechCost_en: Costs of global technologies for ref liq gasoline pool
    A261.globaltech_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input),
               year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = round(approx_fun(year, input.cost, rule = 1), energy.DIGITS_COST)) %>%
      ungroup() %>%
      drop_na() %>% # any model years outside the envelope of years with provided values are dropped
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])->
      L2261.GlobalTechCost_en

    # L2261.GlobalTechShrwt_en: Shareweights of refining technologies
    A261.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology),
               year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = round(approx_fun(year, share.weight, rule = 1), energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      drop_na() %>% # any model years outside the envelope of years with provided values are dropped
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L2261.GlobalTechShrwt_en

    A261.globaltech_interp %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2261.StubTechInterp_en


    # Calibration information
    # First, compute the total "refined liquids gasoline pool" (ethanol + refining) from the inputs to the
    # transportation technologies. The ethanol volume is equal to the "regional * ethanol" outputs, calibrated elsewhere. The refining
    # "output" is equal to the total minus the sum of the ethanol.

    L2261.StubTechProd_ethanol <- bind_rows(
      filter(L2262.Production_refinedLiquids_reg_imp, grepl("ethanol", supplysector)),
      filter(L2262.Production_refinedLiquids_reg_dom, grepl("ethanol", supplysector))) %>%
      group_by(region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      rename(minicam.energy.input = supplysector)

    L2261.TotalEthanol <- L2261.StubTechProd_ethanol %>%
      group_by(region, year) %>%
      summarise(ethanol = sum(calOutputValue)) %>%
      ungroup()

    L2261.StubTechProd_gsln <- filter(L254.StubTranTechCalInput, minicam.energy.input %in% unique(A261.sector$supplysector),
                                      sce == "CORE") %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(total_gsln = sum(calibrated.value)) %>%
      ungroup() %>%
      left_join_error_no_match(L2261.TotalEthanol, by = c("region", "year")) %>%
      mutate(calOutputValue = round(total_gsln - ethanol, digits = energy.DIGITS_CALOUTPUT),
             minicam.energy.input = "refining") %>%
             #minicam.energy.input = "regional refined oil") %>%
      select(region, minicam.energy.input, year, calOutputValue) %>%
      bind_rows(L2261.StubTechProd_ethanol) %>%
      left_join_error_no_match(select(A261.globaltech_eff, supplysector, subsector, technology, minicam.energy.input),
                               by = "minicam.energy.input") %>%
      rename(stub.technology = technology) %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]])

    # Calibration of biodiesel/refining sharing in refined liquids enduse
    # Total ref liq end: sum of ref liq end inputs in L244 (buildings) and L254 (transport)
    # Biodiesel tech: equal to biodiesel production (biodiesel tech + fixed output "other biodiesel")
    # Ref oil tech: total minus biodiesel

    L2261.ref_liq_inputs <- bind_rows(L244.StubTechCalInput_bld,
                                      filter(L254.StubTranTechCalInput, sce == "OTAQ")) %>%
      filter(minicam.energy.input == "refined liquids enduse")

    L2261.ref_liq_cal_techs <- A26.globaltech_eff %>%
      filter(supplysector %in% L2261.ref_liq_inputs$minicam.energy.input) %>%
      select(supplysector, subsector, stub.technology = technology, minicam.energy.input)

    # # Calibration of biodiesel in refined liquids enduse
    # # Update the prefix of the fixed output biodiesel table for tracking provenance of data tables
    # L2261.StubTechFixOut_otherBiod <- mutate(L122.StubTechFixOut_otherBiod,
    #                                          fixedOutput = round(fixedOutput, digits = energy.DIGITS_CALOUTPUT))

    L2261.StubTechProd_biodiesel <- bind_rows(
      filter(L2262.Production_refinedLiquids_reg_imp, grepl("biodiesel", supplysector)),
      filter(L2262.Production_refinedLiquids_reg_dom, grepl("biodiesel", supplysector))) %>%
      group_by(region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      rename(minicam.energy.input = supplysector)

    L2261.TotalBiodiesel <- L2261.StubTechProd_biodiesel %>%
      #bind_rows(select(L122.StubTechFixOut_otherBiod, region, year, calOutputValue = fixedOutput)) %>%
      group_by(region, year) %>%
      summarise(biodiesel = sum(calOutputValue)) %>%
      ungroup()

    L2261.StubTechProd_refliqend <- L2261.ref_liq_inputs %>%
      group_by(region, year, minicam.energy.input) %>%
      summarise(total = sum(calibrated.value)) %>%
      ungroup() %>%
      left_join_error_no_match(L2261.TotalBiodiesel, by = c("region", "year")) %>%
      mutate(calOutputValue = total - biodiesel,
             minicam.energy.input = "regional refined oil") %>%
      select(region, minicam.energy.input, year, calOutputValue) %>%
      bind_rows(L2261.StubTechProd_biodiesel) %>%
      left_join_error_no_match(L2261.ref_liq_cal_techs %>%
                                 mutate(minicam.energy.input = if_else(minicam.energy.input == "refining",
                                                                       "regional refined oil", minicam.energy.input)),
                               by = "minicam.energy.input") %>%
      #rename(stub.technology = technology) %>%
      mutate(calOutputValue = round(calOutputValue, digits = energy.DIGITS_CALOUTPUT),
                                    share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]])

    # #=======#=======#=======#=======#=======#=======#=========

    # Produce outputs

    L2261.Supplysector_en %>%
      add_title("Motor gasoline (ref liq gas pool) supplysector info") %>%
      add_units("Unitless") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("common/GCAM_region_names", "energy/A261.sector") ->
      L2261.Supplysector_en

    L2261.SubsectorLogit_en %>%
      add_title("Motor gasoline subsector logit exponents") %>%
      add_units("Unitless") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("common/GCAM_region_names", "energy/A261.subsector_logit") ->
      L2261.SubsectorLogit_en

    L2261.SubsectorShrwtFllt_en %>%
      add_title("Motor gasoline subsector share-weight default fillout values") %>%
      add_units("Unitless") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("common/GCAM_region_names", "energy/A261.subsector_shrwt") ->
      L2261.SubsectorShrwtFllt_en

    L2261.SubsectorInterp_en %>%
      add_title("Motor gasoline subsector share-weight interpolation") %>%
      add_units("Unitless") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("common/GCAM_region_names", "energy/A261.subsector_interp") ->
      L2261.SubsectorInterp_en

    L2261.StubTech_en %>%
      add_title("Motor gasoline stub technologies") %>%
      add_units("unitless") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("common/GCAM_region_names", "energy/A261.globaltech_shrwt") ->
      L2261.StubTech_en

    L2261.GlobalTechEff_en %>%
      add_title("Motor gasoline technology efficiencies") %>%
      add_units("Unitless output/input") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("energy/A261.globaltech_eff") ->
      L2261.GlobalTechEff_en

    L2261.GlobalTechCost_en %>%
      add_title("Motor gasoline technology costs") %>%
      add_units("1975$/GJ") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("energy/A261.globaltech_cost") ->
      L2261.GlobalTechCost_en

    L2261.GlobalTechShrwt_en %>%
      add_title("Motor gasoline technology share-weights") %>%
      add_units("unitless") %>%
      add_comments("Copied from exogenous inputs") %>%
      add_precursors("energy/A261.globaltech_shrwt") ->
      L2261.GlobalTechShrwt_en

    L2261.StubTechInterp_en %>%
      add_title("gasoline technology shareweight interpolation") %>%
      add_units("unitless") %>%
      add_comments("interpolated to all model years based on assumptions in A261.globaltech_interp") %>%
      add_precursors("energy/A261.globaltech_interp") ->
      L2261.StubTechInterp_en


    # L2261.StubTechFixOut_otherBiod %>%
    #   add_title("FixedOutput for other bidiesel") %>%
    #   add_units("EJ") %>%
    #   add_comments("This corrects the mismatch between otaq and IEA data") %>%
    #   add_precursors("L122.StubTechFixOut_otherBiod") ->
    #   L2261.StubTechFixOut_otherBiod

    L2261.StubTechProd_gsln %>%
      add_title("Motor gasoline technology calibration") %>%
      add_units("EJ/yr") %>%
      add_comments("Ethanol tech: equal to ethanol output") %>%
      add_comments("Refining tech: total ref liq gasoline pool consumption in transportation minus ethanol output") %>%
      add_precursors("L254.StubTranTechCalInput",
                     "L2262.Production_refinedLiquids_reg_dom",
                     "L2262.Production_refinedLiquids_reg_imp") ->
      L2261.StubTechProd_gsln

    L2261.StubTechProd_refliqend %>%
      add_title("Biodiesel/refining technology calibration in ref liq enduse") %>%
      add_units("EJ/yr") %>%
      add_comments("Biodiesel tech: equal to biodiesel output") %>%
      add_comments("Refining tech: total ref liq enduse minus biodiesel") %>%
      add_precursors("energy/A_regions",
                     "energy/A26.globaltech_eff",
                     "L244.StubTechCalInput_bld",
                     "L254.StubTranTechCalInput",
                     #"L122.StubTechFixOut_otherBiod",
                     "L2262.Production_refinedLiquids_reg_dom",
                     "L2262.Production_refinedLiquids_reg_imp",
                     "energy/bio_feed_mapping",
                     "L121.share_R_TPES_biofuel_tech") ->
      L2261.StubTechProd_refliqend

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
