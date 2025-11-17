# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_dac_xml
#'
#' Construct XML data structure for \code{dac.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{dac.xml}. The corresponding file in the
#' original data system was \code{batch_dac_xml.R} (energy XML).
#' @author JF March 2021
module_energy_dac_xml <- function(command, ...) {


  TECH_PARAMETRIZATION_INPUTS <- append(c('EMF_ref','EMF_adv'),paste0("ssp", 1:5))
  if(command == driver.DECLARE_INPUTS) {
    return(c("L262.CarbonCoef_dac",
             "L262.Supplysector_dac",
             "L262.FinalEnergyKeyword_dac",
             "L262.SubsectorLogit_dac",
             "L262.SubsectorShrwtFllt_dac",
             "L262.SubsectorInterp_dac",
             "L262.StubTech_dac",
             "L262.GlobalTechShrwt_dac",
             "L262.GlobalTechShrwt_dac_EMF",
             c(paste("L262.GlobalTechCoef_dac", TECH_PARAMETRIZATION_INPUTS, sep = "_")),
             c(paste("L262.GlobalTechCost_dac", TECH_PARAMETRIZATION_INPUTS, sep = "_")),
             "L262.GlobalTechCapture_dac",
             "L262.GlobalTechCapture_dac_EMF",
             "L262.StubTechProd_dac",
             "L262.PerCapitaBased_dac",
             "L262.BaseService_dac",
             "L262.PriceElasticity_dac",
             "L262.GlobalTechSCurve_dac",
             "L262.GlobalTechProfitShutdown_dac",
             "L262.GlobalTechSCurve_dac_EMF",
             "L262.GlobalTechProfitShutdown_dac_EMF",
             "L262.GlobalTechCapture_dac_EMF",
             "L262.StubTech_dac_EMF",
             "L262.StubTechCost_dac"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "dac_ssp1.xml",
             XML = "dac_ssp2.xml",
             XML = "dac_ssp3.xml",
             XML = "dac_ssp4.xml",
             XML = "dac_ssp5.xml",
             XML = "dac_EMF_ref.xml",
             XML = "dac_EMF_adv.xml"))
  } else if(command == driver.MAKE) {

    # Silence package check notes
    dac_ssp1.xml <- dac_ssp2.xml <- dac_ssp3.xml <- dac_ssp4.xml <- dac_ssp5.xml <- NULL

    all_data <- list(...)[[1]]

    for(sce in TECH_PARAMETRIZATION_INPUTS){
    # Load required inputs
    L262.CarbonCoef_dac <- get_data(all_data, "L262.CarbonCoef_dac")
    L262.Supplysector_dac <- get_data(all_data, "L262.Supplysector_dac")
    L262.FinalEnergyKeyword_dac <- get_data(all_data, "L262.FinalEnergyKeyword_dac")
    L262.SubsectorLogit_dac <- get_data(all_data, "L262.SubsectorLogit_dac")
    L262.SubsectorShrwtFllt_dac <- get_data(all_data, "L262.SubsectorShrwtFllt_dac")
    L262.SubsectorInterp_dac <- get_data(all_data, "L262.SubsectorInterp_dac")
    L262.StubTech_dac <- get_data(all_data, "L262.StubTech_dac")
    L262.GlobalTechShrwt_dac <- get_data(all_data, "L262.GlobalTechShrwt_dac")

    coef_name <- paste0("L262.GlobalTechCoef_dac_",sce)
    cost_name <- paste0("L262.GlobalTechCost_dac_",sce)

    L262.GlobalTechCoef_dac <- get_data(all_data, coef_name)
    L262.GlobalTechCost_dac <- get_data(all_data, cost_name)

    L262.StubTechCost_dac <- get_data(all_data, "L262.StubTechCost_dac")
    L262.GlobalTechCapture_dac <- get_data(all_data, "L262.GlobalTechCapture_dac")
    L262.StubTechProd_dac <- get_data(all_data, "L262.StubTechProd_dac")
    L262.PerCapitaBased_dac <- get_data(all_data, "L262.PerCapitaBased_dac")
    L262.BaseService_dac <- get_data(all_data, "L262.BaseService_dac")
    L262.PriceElasticity_dac <- get_data(all_data, "L262.PriceElasticity_dac")
    L262.GlobalTechSCurve_dac <- get_data(all_data, "L262.GlobalTechSCurve_dac")
    L262.GlobalTechProfitShutdown_dac <- get_data(all_data, "L262.GlobalTechProfitShutdown_dac")

    if(grepl("EMF",sce, fixed=TRUE)) {
        L262.GlobalTechSCurve_dac <- get_data(all_data, "L262.GlobalTechSCurve_dac_EMF")
        L262.GlobalTechProfitShutdown_dac <- get_data(all_data, "L262.GlobalTechProfitShutdown_dac_EMF")
        L262.GlobalTechCapture_dac <- get_data(all_data, "L262.GlobalTechCapture_dac_EMF")
        L262.StubTech_dac <- get_data(all_data, "L262.StubTech_dac_EMF")
        L262.GlobalTechShrwt_dac <- get_data(all_data, "L262.GlobalTechShrwt_dac_EMF")

        dac_sectors <- c('CO2 removal','process heat dac','airCO2')

        L262.SubsectorLogit_dac <- L262.SubsectorLogit_dac %>%
          filter(supplysector %in% dac_sectors)

        L262.SubsectorShrwtFllt_dac <- L262.SubsectorShrwtFllt_dac %>%
          filter(supplysector %in% dac_sectors)

        L262.SubsectorInterp_dac <- L262.SubsectorInterp_dac %>%
          filter(supplysector %in% dac_sectors)



        }
    #} else if (!grepl("EMF",sce, fixed=TRUE)){
    #  L262.GlobalTechSCurve_dac <- get_data(all_data, "L262.GlobalTechSCurve_dac")
    #  L262.GlobalTechProfitShutdown_dac <- get_data(all_data, "L262.GlobalTechProfitShutdown_dac")
    #  L262.GlobalTechCapture_dac <- get_data(all_data, "L262.GlobalTechCapture_dac")
    #  L262.StubTech_dac <- get_data(all_data, "L262.StubTech_dac")
    #  L262.GlobalTechShrwt_dac <- get_data(all_data, "L262.GlobalTechShrwt_dac")
    #  }

    xmlfn <- paste0("dac_",sce, '.xml')

    # ===================================================
    # Produce outputs
    create_xml(xmlfn) %>%
      add_xml_data(L262.CarbonCoef_dac, "CarbonCoef") %>%
      add_logit_tables_xml(L262.Supplysector_dac, "Supplysector") %>%
      add_xml_data(L262.FinalEnergyKeyword_dac, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L262.SubsectorLogit_dac, "SubsectorLogit") %>%
      add_xml_data(L262.SubsectorShrwtFllt_dac, "SubsectorShrwtFllt") %>%
      add_xml_data(L262.SubsectorInterp_dac, "SubsectorInterp") %>%
      add_xml_data(L262.StubTech_dac, "StubTech") %>%
      add_xml_data(L262.GlobalTechShrwt_dac, "GlobalTechShrwt") %>%
      add_xml_data(L262.GlobalTechCoef_dac, "GlobalTechCoef") %>%
      add_xml_data(L262.GlobalTechCost_dac, "GlobalTechCost") %>%
      add_xml_data(L262.GlobalTechCapture_dac, "GlobalTechCapture") %>%
      add_xml_data(L262.StubTechProd_dac, "StubTechProd") %>%
      add_xml_data(L262.PerCapitaBased_dac, "PerCapitaBased") %>%
      add_xml_data(L262.BaseService_dac, "BaseService") %>%
      add_xml_data(L262.PriceElasticity_dac, "PriceElasticity") %>%
      add_xml_data(L262.GlobalTechSCurve_dac, "GlobalTechSCurve") %>%
      add_xml_data(L262.GlobalTechProfitShutdown_dac, "GlobalTechProfitShutdown") %>%
      add_precursors("L262.CarbonCoef_dac",
                     "L262.Supplysector_dac",
                     "L262.FinalEnergyKeyword_dac",
                     "L262.SubsectorLogit_dac",
                     "L262.SubsectorShrwtFllt_dac",
                     "L262.SubsectorInterp_dac",
                     "L262.StubTechProd_dac",
                     "L262.StubTech_dac",
                     "L262.GlobalTechShrwt_dac",
                     paste0("L262.GlobalTechCoef_dac_",sce),
                     paste0("L262.GlobalTechCost_dac_",sce),
                     "L262.GlobalTechCapture_dac",
                     "L262.PerCapitaBased_dac",
                     "L262.BaseService_dac",
                     "L262.PriceElasticity_dac",
                     "L262.GlobalTechSCurve_dac",
                     "L262.GlobalTechProfitShutdown_dac") ->
      xmlobj

    #Don't add dac to liquids stubtech costs to EMF input xmls for now
    if(!grepl("EMF",sce, fixed=TRUE)) {
      xmlobj <- xmlobj %>%
        add_xml_data(L262.StubTechCost_dac %>%
                       filter(scenario == sce) %>%
                       select(-scenario), "StubTechCost")
    }


    assign(xmlfn, xmlobj)


  }
  return_data(dac_ssp1.xml,
              dac_ssp2.xml,
              dac_ssp3.xml,
              dac_ssp4.xml,
              dac_ssp5.xml,
              dac_EMF_ref.xml,
              dac_EMF_adv.xml)}
  else {
    stop("Unknown command")
  }
}
