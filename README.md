# HABs data remote repository publishing

This repository pulls [HABs data from ERDDAP](https://erddap.sccoos.org/erddap/tabledap/HABs-CalPoly.html) and transforms it to Darwin Core Archive format for it to be published to remote repositories like EDI, GBIF, and OBIS. There is a scheduled task using GitHub Actions to pull the updated datasets and then publish them- currently into EDI, with plans to integrate GBIF into the process in the future.

Currently, this script supports the data from the following sites:
Cal Poly Pier, Monterey Wharf, Newport Beach Pier, Santa Cruz Wharf, Santa Monica Pier, Scripps Pier, and Stearns Wharf


## Overview

A scheduled job controlled by GitHub Actions will run the following process on the *1st of each month at midnight*.
  - Build R environment with dependencies.
  - Execute `build_DwC_package.R`, which pulls the most up-to-data HABs data from ERDDAP for the included sites, reformats the data into DwC-A event core, and [generates EML](#generating-eml) metadata to describe the dataset. The generated files (DwC-A reformat and EML files), are written to file.
  - Commit generated files from previous step back into repository.
  - Execute `publish_to_EDI.R`, which will query the EDI repository's PASTA+ API (via R package EDIutils) to [update the HABs dataset](#publishing-to-edi) currently published in EDI with the up-to-date data files and metadata.
  
### Generating EML

EML is built by reading a base template, `HABS_base_EML.xml`, which describes the more static properties like the dataTables and their attributes, and then reads in a series of .csv files for properties that are more configurable/subject to change.

The following files can be updated to change properties of the EML metadata:
   - */EML/general.csv*: Make changes to title, abstract
   - */EML/geographic_coverage.csv*: Make changes to geographic coverage (geographic description, bounding coordinates)
   - */EML/keywords.csv*: Add/change keywords
   - */EML/personnel.csv*: Add/change creators/contacts
   
### Publishing to EDI

The publishing step into EDI requires a GitHub environment to be set up called **habspublish**.

The environment should contain the following secrets:
   - `EDI_USERNAME`: the username to authenticate to EDI for publishing with
   - `EDI_PASSWORD`: the password for the EDI account for publishing
   
The csv *package_identifiers.csv* needs to contain the static package identifier granted by EDI (or elsewise) for the targeted environment.
   
### About

Organization: [Southern California Coastal Ocean Observing System](https://sccoos.org/harmful-algal-bloom/)

Author: Ian Brunjes, ianbrunjes@ucsb.edu

