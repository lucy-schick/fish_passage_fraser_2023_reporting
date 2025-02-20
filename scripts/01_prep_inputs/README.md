Workflows for pre-processing of field data so it aligns with our reporting and fits the requirements for data submission.

These are designed to be self sufficient scripts with everything in them necessary to do there specific task with a clean working environment.

<br>

# Pre-Processing Inputs

Workflows for pre-processing of field data so it aligns with our reporting and fits the requirements for PSCIS uploads.

These are often two-way workflows where we use output CSV spreadsheets to update our raw input datasheets through a manual copy-paste-special. 

These are designed to be self-sufficient scripts with everything in them necessary to do their specific task with a clean working environment.


### 0110-photos.Rmd

- Resizes and renames all Mergin photos
- Resizes and renames all OneDrive photos
- Moves and organizes all photos into site-specific directories and ensures compliance with PSCIS size requirements
- Removes duplicate photos

### 0120_pscis_backup.Rmd

- Creates a backup of raw PSCIS data before the data is QAed. 

### 0130_pscis_wrangle.Rmd

- Imports the QAed cleaned data and further cleans it, then burns to a new geopackage
- After the data has been submitted to the province, it adds PSCIS ids to the form and burns back to PSCIS geopackage
- This script is still a WIP and there is an issue about what still needs to be added here https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/56 

### 0140_pscis_export_to_template.Rmd

- Prepares PSCIS data for copy and paste into PSCIS submission spreadsheet
- Exports PSCIS data to a csv for cut and paste into PSCIS submission spreadsheet
- Determines the replacement structure, size, and type and burns it to a csv for copy and paste into PSCIS submission spreadsheet


### 0150_pscis_export_submission.R

- Moves photos and files to OneDrive for PSCIS submission
- QAs photos before submission

### 0200_fiss_backup.Rmd

- Creates a backup of raw FISS site data before the data is QAed. 

### 0205_fiss_wrangle.Rmd

- Imports the QAed FISS site data and further cleans it:
    - Adds the watershed codes
    - Calculates the average of the numeric columns
    - General cleaning
- Burns it back to a new geopackage

### 0210_fiss_export_to_template.Rmd

- Prepares FISS Site data for copy and paste into fish data submission spreadsheet (NewGraph calls this spreadsheet `habitat_confirmations.xls`)
- Exports FISS data to csv for copy and paste into the fish data submission spreadsheet 

### 0220_fish_data_tidy.R

- Joins PIT tag data to the raw fish data
- Prepares fish data for copy and paste into fish data submission spreadsheet (NewGraph calls this spreadsheet `habitat_confirmations.xls`)
- Exports fish data to csv for copy and paste into the fish data submission spreadsheet

