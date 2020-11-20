# Read in the initial sets of parameter estimates from fitting to data
# these reside in xlx files. Exported as rdata files for lazy loading

create_darwinData <- function(){
  filePath <- "data-raw"
  # reads in survey biomass and and commercial catch(landings + discard) data.
  # Also read in Kiersten curtis biomass estimates
  # tons per km2
  startingRules <- readxl::read_excel(here::here(filePath,"surv_land_disc_Beet.xlsx"),sheet="Results",col_names=TRUE)

  # these are used to validate hydra parameters.

  surveyconversionToMetricTonnes <- 49147 # number of kilometers in GB (survey strata - 10 min square EPU)
  catchconversionToMetricTonnes <- 66293 # number of kilometers in GB (statistical areas)

  options(warn=-1)
  startingRules$CurtiThesis_maxBio <- as.numeric(startingRules$CurtiThesis_maxBio)
  startingRules$CurtiThesis_minBio <- as.numeric(startingRules$CurtiThesis_minBio)
  options(warn=0)
  # min and max survey values (over entire time period)
  startingRules$min_survey <- startingRules$min_survey*surveyconversionToMetricTonnes
  startingRules$max_survey <- startingRules$max_survey*surveyconversionToMetricTonnes
  # min and max landings (over entire time period)
  startingRules$minLandings <- startingRules$minLandings*catchconversionToMetricTonnes
  startingRules$maxLandings <- startingRules$maxLandings*catchconversionToMetricTonnes

  # read in parameter estimates from fitting Shepherd and Segmented model to data.
  # This data is different from the Gaichas et al data

  hockey <- readxl::read_excel(here::here(here::here,"MasterSRFits.xlsx"),sheet="Hockey",col_names=TRUE)
  segmented <- readxl::read_excel(paste0(here::here,"MasterSRFits.xlsx"),sheet="Segmented",col_names=TRUE)
  shepherdBH <- readxl::read_excel(paste0(here::here,"MasterSRFits.xlsx"),sheet="ShepherdBH",col_names=TRUE)
  shepherdRicker <- readxl::read_excel(here::here(filePath,"MasterSRFits.xlsx"),sheet="ShepherdRicker", col_names=TRUE)
  observedSSB <- readxl::read_excel(here::here(filePath,"MasterSRFits.xlsx"),sheet="SSB", col_names=TRUE)

  darwinData <- list()
  darwinData$hockey <- hockey
  darwinData$segmented <- hockey
  darwinData$BH <- shepherdBH
  darwinData$Ricker <- shepherdRicker
  darwinData$SSBBounds <- observedSSB
  darwinData$historicBounds <- startingRules

  # export darwin data
  usethis::use_data(darwinData,overwrite = TRUE)

  # export time series of survey and landings
  survey <- readxl::read_xlsx(here::here(filePath,"surv_land_disc_Beet.xlsx"),sheet="survey",range="B1:L50",col_names=TRUE) %>%
    tidyr::pivot_longer(.,cols=-YEAR,names_to = "SPECIES",values_to = "VALUE") %>%
    dplyr::mutate(value=value*surveyconversionToMetricTonnes,SPECIES=as.factor(SPECIES)) %>%
    dplyr::mutate(TYPE = "survey")

  landings <- readxl::read_xlsx(here::here(filePath,"surv_land_disc_Beet.xlsx"),sheet="landings",range="B1:L56",col_names=TRUE) %>%
    tidyr::pivot_longer(.,cols=-YEAR,names_to = "SPECIES",values_to = "VALUE") %>%
    dplyr::mutate(value=value*catchconversionToMetricTonnes,SPECIES=as.factor(SPECIES)) %>%
    dplyr::mutate(TYPE = "landings")

  discards <- readxl::read_xlsx(here::here(filePath,"surv_land_disc_Beet.xlsx"),sheet="landings",range="B1:L56",col_names=TRUE) %>%
    tidyr::pivot_longer(.,cols=-YEAR,names_to = "SPECIES",values_to = "VALUE") %>%
    dplyr::mutate(value=value*catchconversionToMetricTonnes,SPECIES=as.factor(SPECIES)) %>%
    dplyr::mutate(TYPE = "discards")

  yrs <- unique(c(unique(discards$YEAR),unique(survey$YEAR),unique(landings$YEAR)))
  species <- unique(c(unique(levels(discards$SPECIES)),unique(levels(survey$SPECIES)),unique(levels(landings$SPECIES))))

  mainGrid <- expand.grid(YEAR=yrs,SPECIES=species)

  timeSeries <- rbind(landings,survey,discards)

  #
  usethis::use_data(timeSeries,overwrite = TRUE)

}

