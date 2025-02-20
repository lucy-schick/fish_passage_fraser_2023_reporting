Workflows for processing and structuring data to align with reporting requirements.

<br>

## 0100-load-bcfishpass-data.R
- Loads bcfishpass and PSCIS data for the project watershed groups and burns to sqlite
- Load the bcfishpass modelling parameters and burns to sqlite
- Build a cross reference table for the `stream_crossing_id` and the `external_crossing_reference` (the crossing id we assigned it in the field) and burns to sqlite


## 0110-load-wshd_stats.R
- Loads watershed statistics for the project watershed groups and burns to sqlite


## 0120-read-sqlite.R
- Reads data from an SQLite database.


## 0130-tables.R
- Build tables for used in all sections of the report.
- Generated dynamic table captions


## 0140-extract-inputs.R
- These are worklows that only need to happen once really so they are separate from those in `0130-tables.R`. They often need to be re-run when data is updated.
- Code run in this script:
  - build priority spreadsheet and burn to csv called `habitat_confirmations_priorities.csv`
  - extract the road surface and type from bcfishpass
- There is still a decent amount of old code here that I am leaving in case we need it later for some reason, the old code includes:
  -   Find road tenure information from bcfishpass.
  -   Summarize fish sizes. When inputting data from the field, only one row needs to be inputted for each site in "Step 2 (Fish Coll. Data)". Once the script is run it processes the fish data and burns to a csv ready to copy-paste-special into Step 2.   
  -   Determine replacement structure type and size based on field measured metrics.
  -   Find the PSCIS reference ID for crossings that underwent Phase 1 assessments within the same program and cross-reference to Phase 2 data.
  

## 0150-build-photo-amalg.R
- Build photo amalgamation for each site and burns to OneDrive


## 0160-photos-import.Rmd
-  Bulk import all photos tagged with `_k_` or `crossings_all`  from OneDrive to the local repo directory.


## 0180-photos-extract-metadata.R
- Extracts and processes metadata from project photos.
- Reads EXIF data to capture timestamps, GPS locations, and device details.
- Prepares metadata tables for integration into reports and maps.


## 0190-build-html-map-tables.R
- Generates HTML-based map tables for visualizing project data in the Results section interactive map


## extract-fiss-species-table.R
- This script is used to overcome the issue documented here https://github.com/NewGraphEnvironment/fish_passage_fraser_2023_reporting/issues/75

