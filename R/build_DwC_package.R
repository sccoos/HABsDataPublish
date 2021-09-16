library(googledrive)
library(here)
library(readxl)
library(rerddap)
library(worrms)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(reshape2)
library(EML)
library(taxonomyCleanr)
library(EDIutils)


#'
#' Read in spreadsheets that give info about event, measurement and occurrence parameters for this dataset from the downloaded file
#'
oldw <- getOption("warn")
options(warn = -1)
meta_event <- read_xlsx(here("DwC/HABsDwC.xlsx"), na = "", sheet = "event")
meta_measurement <-
  read_xlsx(here("DwC/HABsDwC.xlsx"), na = "", sheet = "measurement")
meta_occurrence <-
  read_xlsx(here("DwC/HABsDwC.xlsx"), na = "", sheet = "occurrence")
meta_site <- read_xlsx(here("DwC/HABsDwC.xlsx"), na = "", sheet = "site")
options(warn = oldw)

#'
#' Read in HABs data from ERDDAP
#'

# DEBUG mode will run script on only a limited set of the data
DEBUG <- "FALSE"

if (DEBUG == "TRUE") {
  datasetIds = c('HABs-ScrippsPier')
  siteIds = c('SIO')
}  else {
  datasetIds = c(
    'HABs-CalPoly',
    'HABs-MontereyWharf',
    'HABs-NewportPier',
    'HABs-SantaCruzWharf',
    'HABs-SantaMonicaPier',
    'HABs-ScrippsPier',
    'HABs-StearnsWharf'
  )
  siteIds = c('CPP', 'HAB_MWII', 'NP', 'HAB_SCW', 'SMP', 'SIO', 'SW')
}

# Create and clear lists to hold all sites in one file
oneEvent <- vector()
oneOccur <- vector()
oneMoF <- vector()
dwc_o_records <- NULL

for (i in 1:length(datasetIds)) {
  siteMeta = info(datasetIds[i], url = "http://erddap.sccoos.org/erddap/")
  siteVars = siteMeta$variables$variable_name
  coordinateUncertaintyInMeters = filter(meta_site, Site == datasetIds[i])$coordinateUncertaintyInMeters
  if (DEBUG == "TRUE") {
    siteData = tabledap(
      siteMeta,
      fields = siteVars,
      'time>=2019-05-06T16:53:00Z',
      'time<=2019-09-09T16:15:00Z',
      url = "http://erddap.sccoos.org/erddap/"
    )
  }  else {
    print(paste("Querying ERRDAP for: ", datasetIds[i]))
    siteData = tabledap(siteMeta, fields = siteVars, url = "http://erddap.sccoos.org/erddap/")
  }

  #'
  #' Cleanup siteData, correcting types, removing unused columns and replacing NaNs
  #'
  siteData <-
    cbind(
      mutate_all(select(
        siteData,-one_of("Location_Code", "time", "SampleID", "depth")
      ), as.numeric),
      locationID = datasetIds[i],
      time = siteData$time,
      SampleID = siteData$SampleID,
      minimumDepthInMeters = siteData$depth,
      maximumDepthInMeters = siteData$depth,
      stringsAsFactors = FALSE
    )
  siteData <- na_if(siteData, "NaN")

  #' Create the eventID and occurrenceID in the original file so that information
  #' can be reused for all necessary files down the line.
  siteData$eventID <- NULL
  siteData$eventID <-
    paste(datasetIds[i], siteData$time, sep = "_")
  siteData$id <- siteData$eventID

  #' We will also have to add any missing required fields
  siteData$basisOfRecord <- "HumanObservation"
  siteData$geodeticDatum <- "EPSG:4326 WGS84"
  siteData$countryCode <- "US"
  siteData$coordinateUncertaintyInMeters <-
    filter(meta_site, Location_Code == siteIds[i])$coordinateUncertaintyInMeters

  #' We will need to create three separate files to comply with the sampling event format.
  #'   ADD EVENT COLUMNS TO EVENT
  #'   ADD MoF COLUMNS to MoF
  dwc_e <- vector(mode = "list")
  dwc_m <- vector(mode = "list")
  dwc_o <- vector(mode = "list")
  dwc_worms <- vector(mode = "list")

  dwc_e <-
    c(dwc_e,
      "id",
      "eventID",
      "time",
      "geodeticDatum",
      "countryCode",
      as.vector(meta_event$measurementType))
  dwc_m <-
    c(dwc_m,
      "eventID",
      as.vector(meta_measurement$measurementType))
  dwc_o <-
    c(
      dwc_o,
      "eventID",
      "basisOfRecord",
      as.vector(meta_occurrence$measurementType)
    )

  #'
  #' Create a lookup table of scientificNames from Worms
  #'
  dwc_worms$scientificName <- meta_occurrence$scientificName
  #' Taxonomic lookup using the worrms library

  for (j in 1:length(dwc_worms$scientificName)) {
    aphia_id <- worrms::wm_name2id(name = dwc_worms$scientificName[j])
    # Do taxon lookup only if not already done for this id and add to records
    if (!aphia_id %in% dwc_o_records$AphiaID) {
      print(paste("Doing taxon lookup for: ", dwc_worms$scientificName[j]))
      new_record <-
        worrms::wm_record(aphia_id)[c(1, 2, 3, 8, 10, 13, 14, 15, 16, 17, 18, 20)]
      dwc_o_records <- rbind(dwc_o_records, new_record)
    }
  }

  event <- siteData[(as.character(dwc_e))]
  event$id <- event$eventID

  event <- rename(
    event,
    decimalLatitude = latitude,
    decimalLongitude = longitude,
    eventRemarks = SampleID,
    eventDate = time
  )

  mof <- siteData[(as.character(dwc_m))]
  mof_long <-
    reshape2::melt(
      data = mof,
      na.rm = TRUE,
      value.factor = FALSE,
      variable.factor = FALSE
    )
  mof_long <- mutate(mof_long, variable = as.character(variable))
  mof_long <- rename(mof_long,
                     id = eventID,
                     measurementType = variable,
                     measurementValue = value)

  mof_long$measurementID <- NULL
  mof_long$measurementTypeID <- NULL

  for (k in 1:length(meta_measurement$measurementType)) {
    for (j in 1:length(mof_long$measurementType)) {
      if (mof_long$measurementType[j] == meta_measurement[k, 1]) {
        mof_long$measurementID[j] = paste(mof_long$id[j], meta_measurement[k, 1], sep = "_")
        mof_long$measurementTypeID[j] = as.character(meta_measurement[k, 2])
        mof_long$measurementUnit[j] = as.character(meta_measurement[k, 3])
        mof_long$measurementUnitID[j] = as.character(meta_measurement[k, 4])
      }
    }
  }

  occur <- siteData[(as.character(dwc_o))]
  occur_long <-
    reshape2::melt(
      data = occur,
      na.rm = TRUE,
      value.factor = FALSE,
      variable.factor = FALSE
    )
  occur_long <-
    mutate(occur_long, variable = as.character(variable))

  #'  Put critter counts in as a occurrence:organismquantity and occurrence:organismquantitytype
  #'

  occur_long <- rename(occur_long,
                       id = eventID,
                       organismName = variable,
                       organismQuantity = value)

  occur_long$organismQuantityType <- "cells/L"
  occur_long$occurrenceID <- NULL
  occur_long$scientificName <- NULL

  for (k in 1:length(meta_occurrence$measurementType)) {
    for (j in 1:length(occur_long$organismName)) {
      if (occur_long$organismName[j] == meta_occurrence$measurementType[k]) {
        occur_long$occurrenceID[j] = paste(occur_long$id[j], meta_occurrence[k, 1], sep = "_")
        if (occur_long$organismQuantity[j] > 0) {
          occur_long$occurrenceStatus[j] = "present"
        }
        else {
          occur_long$occurrenceStatus[j] = "absent"
        }
        occur_long$scientificName[j] = as.character(str_replace_all(meta_occurrence[k, 2], "_", " "))
        occur_long$scientificNameID[j] = as.character(meta_occurrence[k, 3])
        occur_long$taxonID[j] = as.character(meta_occurrence[k, 5])
        occur_long$kingdom[j] = as.character(meta_occurrence[k, 6])
      }
    }
  }

  occur_long$scientificName <- as.vector(occur_long$scientificName)

  #rowbind events, occurences and MoFs together to make a single event file
  oneEvent <- rbind(oneEvent, event)
  oneOccur <- rbind(oneOccur, occur_long)
  oneMoF <- rbind(oneMoF, mof_long )
}

write.csv(
  oneEvent,
  file = here("DwC", "datapackage", "event.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)
write.csv(
  oneOccur,
  here("DwC", "datapackage", "occurrence.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)
write.csv(
  oneMoF,
  here("DwC", "datapackage", "extendedmeasurementorfact.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)

print("DwC transform completed.")

## Read in base EML template for HABs data
eml_doc <- EML::read_eml(here("EML", "HABs_base_EML.xml"))

## Create updated list for creators/contacts
creators <- list()
contacts <- list()

personnel <- read_csv(here("EML", "personnel.csv"))
for (i in 1:nrow(personnel)) {
  person <- list()

  person$individualName <- list(
    givenName = personnel$givenName[[i]],
    surName = personnel$surName[[i]]
  )

  person$organizationName <- personnel$organizationName[[i]]
  person$positionName <- personnel$positionName[[i]]
  person$electronicMailAddress <- personnel$electronicMailAddress[[i]]

  if (personnel$role[[i]] == "creator") {
    creators[[length(creators)+1]] <- person
  } else if (personnel$role[[i]] == "contact") {
    contacts[[length(contacts)+1]] <- person
  }  else {
    # other roles not supported
  }
}

# Add creators/contacts to EML
eml_doc$dataset$creator <- creators
eml_doc$dataset$contact <- contacts

## Create updated list of keywords
keywords <- list()

keyword_set <- read_csv(here("EML", "keywords.csv"))
for (i in 1:nrow(keyword_set)) {
  keyword <- list(
    keyword = keyword_set$keyword[[i]],
    keywordThesaurus = keyword_set$keywordThesaurus[[i]]
  )

  keywords[[length(keywords)+1]] <- keyword
}

# Add keywordSet to eml
eml_doc$dataset$keywordSet <- keywords

## Create updated geographic_coverage
geo_coverage <- read_csv(here("EML", "geographic_coverage.csv"))
gc <- list(
  geographicDescription = geo_coverage$geographicDescription[[1]],
  boundingCoordinates = list(
    northBoundingCoordinate = geo_coverage$northBoundingCoordinate[[1]],
    southBoundingCoordinate = geo_coverage$southBoundingCoordinate[[1]],
    eastBoundingCoordinate = geo_coverage$eastBoundingCoordinate[[1]],
    westBoundingCoordinate = geo_coverage$westBoundingCoordinate[[1]]
  )
)

# Add geographic coverage to EML
eml_doc$dataset$coverage$geographicCoverage <- gc

## Update title/abstract
package <- read_csv(here("EML", "general.csv"))
eml_doc$dataset$title <- package$title[[1]]

paragraphs = list()
for (i in 1:nrow(package)) {
  para <- package$abstract[[i]]
  paragraphs[[length(paragraphs)+1]] <- para
}

eml_doc$dataset$abstract$para <- paragraphs

## Update temporal coverage and pubDate

# get min and max of data timespan
min <- min(oneEvent$eventDate)
max <- max(oneEvent$eventDate)

# Add temporal range to EML
eml_doc$dataset$coverage$temporalCoverage$rangeOfDates <- list(
  beginDate = list(calendarDate = date(min)),
  endDate = list(calendarDate = date(max))
)

# Add pubdate to EML
eml_doc$dataset$pubDate <- lubridate::today()


## Update numberofRecords for each dataTable (event, occurrence, extendedmeasurementorfact)
# Add nrow for event
eml_doc$dataset$dataTable[[1]]$numberOfRecords <- nrow(oneEvent)
# Add nrow for occur
eml_doc$dataset$dataTable[[2]]$numberOfRecords <- nrow(oneOccur)
# Add nrow for mof
eml_doc$dataset$dataTable[[3]]$numberOfRecords <- nrow(oneMoF)

## Update taxonomic coverage
# build emld list of taxonomic coverage
dwc_o_records$authority <- "worms"
taxon_coverage <- taxonomyCleanr::make_taxonomicCoverage(
  taxa.clean = dwc_o_records$scientificname,
  authority = dwc_o_records$authority,
  authority.id = dwc_o_records$AphiaID,
  write.file = FALSE)

eml_doc$dataset$coverage$taxonomicCoverage <- taxon_coverage

# Add physical distribution URLs for data files
build_physical <- function(file_name, file_format) {
  physical <- list()

  physical$objectName <- file_name
  physical$size <- list(
    size = file.info(here("DwC", "datapackage", file_name))$size,
    unit = "bytes"
  )
  physical$dataFormat$externallyDefinedFormat$formatName <- file_format
  physical$distribution$online$url <- list(
    `function` = "download",
    url = paste("https://raw.githubusercontent.com/sccoos/HABsDataPublish/master/DwC/datapackage/", file_name, sep="")
  )

  return(physical)
}

eml_doc$dataset$dataTable[[1]]$physical <- build_physical("event.csv", "text/csv")
eml_doc$dataset$dataTable[[2]]$physical <- build_physical("occurrence.csv", "text/csv")
eml_doc$dataset$dataTable[[3]]$physical <- build_physical("extendedmeasurementorfact.csv", "text/csv")
eml_doc$dataset$otherEntity$physical <- build_physical("meta.xml", "text/xml")

## Validate eml
isValid <- eml_validate(eml_doc)

## Write eml to file for inclusion in data package
if (isValid) {
  eml <- EML::write_eml(eml_doc, here("DwC", "datapackage", "eml.xml"))
  print("Successfully built EML. Written")
  print(here("DwC", "datapackage", "eml.xml"))
} else {
  print("EML construction failed with errors:")
  print(isValid)

  throw("EML construction did not produce a valid result.")
}
