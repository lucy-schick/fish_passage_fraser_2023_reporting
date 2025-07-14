# Scripts to transfer photos and excel file to the PSCIS submission folder

##QA to be sure that you have all 6 required photos for submission to PSCIS
##convert jpg (or other formats - need to code) to JPG for consistency and to avoid issues with submission/reporting
##move the photos and spreadsheet ready for submission to pscis

source('scripts/packages.R')
source('scripts/functions.R')

library(fpr)
library(tidyverse)
library(fs)



# Functions -----------------------------------------------------------------
tfpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

tfpr_photo_change_name <- function(filenames_to_change = filestocopy_list){
  gsub(filenames_to_change, pattern = path_photos, replacement = targetdir)
}

# Params -----------------------------------------------------------------

# extract the project region we are working on, so we can use it to build the target directory path
project_region <- stringr::str_match(params$repo_name, "fish_passage_([^_]+)_")[, 2]



# Phase 1 -----------------------------------------------------------------

name_submission <- 'pscis_phase1.xlsm'

## Create folders and copy over photos -------------

# need to add photos to local machine to upload to PSCIS
targetdir = fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/", params$project_year, project_region,
                                     paste0("/PSCIS_", project_region,"_", params$project_year, "_", "phase1")))

# create the directory. Take note about which phase
fs::dir_create(targetdir)


# use the pscis spreadsheet to make the folders to copy the photos to.

path_photos <- fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects/", params$job_name, "/data/photos/"))

d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase1.xlsm')


folderstocopy<- d$my_crossing_reference |> as.character()

path_to_photos <- fs::path(path_photos, folderstocopy)

# here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface

folderstocreate<- fs::path(targetdir, folderstocopy)

##create the folders
fs::dir_create(folderstocreate)


# Identify photos that should be copied over into file
filestocopy_list <- path_to_photos |>
  purrr::map(fpr::fpr_photo_paths_to_copy) |>
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))
empty_files <- empty_idx |> tfpr_filter_list()

##rename long names if necessary

photo_sort_tracking <- path_to_photos |>
  purrr::map(fpr::fpr_photo_document_all) |>
  purrr::set_names(folderstocopy) |>
  bind_rows(.id = 'folder') |>
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))

##here we back up a csv that gives us the new location and name of the original JPG photos.

##burn to csv
fs::dir_create("data/photos")
photo_sort_tracking |>
  readr::write_csv(file = 'data/backup/photo_sort_tracking_phase1.csv')

## change path name so we can paste to folders
filestopaste_list <- filestocopy_list |>
  map(tfpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fs::file_copy,
       path =  filestocopy_list,
       new_path = filestopaste_list)


## QA photos -------------

# do a little QA to be sure all the photos are there.

t <- fpr::fpr_photo_qa_df(dat = d, dir_photos = paste0(targetdir, "/"))

## Move Pscis file -------------

##also move over the pscis file
fs::file_copy(path = fs::path('data', name_submission),
              new_path = fs::path(targetdir, name_submission),
              overwrite = T)

#make a little readme for the pdf for upload to ecocat and other details
writeLines(
  paste(
    "Online interactive report is located at: ",
    params$report_url,
    "",
    "A versioned pdf of the report can be downloaded from: ",
    paste0(params$repo_url, "/blob/main/docs/", params$pdf_name),
    "",
    "Raw data is available here: ",
    paste0(params$repo_url, "/tree/main/data"),
    "",
    "All scripts to produce online interactive reporting and pdf are located at: ",
    params$repo_url,
    sep = "\n"
  ),
  fs::path(targetdir, 'readme.txt')
)

## in the future we will need to add the copy calls to move the directory off of onedrive to
#  the windows machine used for project submission.  Don't want to set up the machine right now
# so will do it by hand. What an insane pain




# Phase 2 -----------------------------------------------------------------

name_submission <- 'pscis_phase2.xlsm'

## Create folders and copy over photos -------------

# need to add photos to local machine to upload to PSCIS
targetdir = fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/", params$project_year, project_region,
                                     paste0("/PSCIS_", project_region,"_", params$project_year, "_", "phase2")))

# create the directory. Take note about which phase
fs::dir_create(targetdir)


# use the pscis spreadsheet to make the folders to copy the photos to. For skeena 2024, the photos are stored in Onedrive.
path_photos <- fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects/", params$job_name, "/data/photos/sorted/"))


d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')


folderstocopy<- d$pscis_crossing_id |> as.character()

path_to_photos <- fs::path(path_photos, folderstocopy)


# here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface

folderstocreate<- fs::path(targetdir, folderstocopy)

##create the folders
fs::dir_create(folderstocreate)


# Identify photos that should be copied over into file
filestocopy_list <- path_to_photos |>
  purrr::map(fpr::fpr_photo_paths_to_copy) |>
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))
empty_files <- empty_idx |> tfpr_filter_list()

##rename long names if necessary

photo_sort_tracking <- path_to_photos |>
  purrr::map(fpr::fpr_photo_document_all) |>
  purrr::set_names(folderstocopy) |>
  dplyr::bind_rows(.id = 'folder') |>
  dplyr::mutate(photo_name = stringr::str_squish(stringr::str_extract(value, "[^/]*$")),
                photo_name_length = stringr::str_length(photo_name))

##here we back up a csv that gives us the new location and name of the original JPG photos.

##burn to csv
photo_sort_tracking |>
  readr::write_csv(file = 'data/photos/photo_sort_tracking_phase2.csv')

## change path name so we can paste to folders
filestopaste_list <- filestocopy_list |>
  purrr::map(tfpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fs::file_copy,
       path =  filestocopy_list,
       new_path = filestopaste_list)


## QA photos -------------

# do a little QA to be sure all the photos are there.Table shows photos that are missing. If all NA you are good to go.

t <- fpr::fpr_photo_qa_df(dat = d, dir_photos = paste0(targetdir, "/"))

## Move Pscis file -------------

##also move over the pscis file
fs::file_copy(path = fs::path('data', name_submission),
              new_path = fs::path(targetdir, name_submission),
              overwrite = T)

#make a little readme for the pdf for upload to ecocat and other details
writeLines(
  paste(
    "Online interactive report is located at: ",
    params$report_url,
    "",
    "A versioned pdf of the report can be downloaded from: ",
    paste0(params$repo_url, "/blob/main/docs/", params$pdf_name),
    "",
    "Raw data is available here: ",
    paste0(params$repo_url, "/tree/main/data"),
    "",
    "All scripts to produce online interactive reporting and pdf are located at: ",
    params$repo_url,
    sep = "\n"
  ),
  fs::path(targetdir, 'readme.txt')
)




# Reassessments -----------------------------------------------------------------

name_submission <- 'pscis_reassessments.xlsm'

## Create folders and copy over photos -------------

# need to add photos to local machine to upload to PSCIS
targetdir = fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/", params$project_year, project_region,
                                     paste0("/PSCIS_", project_region,"_", params$project_year, "_", "reassessments")))

# create the directory. Take note about which phase
fs::dir_create(targetdir)


# use the pscis spreadsheet to make the folders to copy the photos to. For skeena 2024, the photos are stored in Onedrive.

path_photos <- fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects/", params$job_name, "/data/photos/sorted/"))

d <- fpr::fpr_import_pscis(workbook_name = 'pscis_reassessments.xlsm')


folderstocopy<- d$pscis_crossing_id |> as.character()

path_to_photos <- fs::path(path_photos, folderstocopy)


# here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface
folderstocreate<- fs::path(targetdir, folderstocopy)

##create the folders
fs::dir_create(folderstocreate)


# Identify photos that should be copied over into file
filestocopy_list <- path_to_photos |>
  purrr::map(fpr::fpr_photo_paths_to_copy) |>
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))
empty_files <- empty_idx |> tfpr_filter_list()

##rename long names if necessary
photo_sort_tracking <- path_to_photos |>
  purrr::map(fpr::fpr_photo_document_all) |>
  purrr::set_names(folderstocopy) |>
  dplyr::bind_rows(.id = 'folder') |>
  dplyr::mutate(photo_name = stringr::str_squish(stringr::str_extract(value, "[^/]*$")),
                photo_name_length = stringr::str_length(photo_name))

##here we back up a csv that gives us the new location and name of the original JPG photos.

##burn to csv
photo_sort_tracking |>
  readr::write_csv(file = 'data/photos/photo_sort_tracking_reassessments.csv')

## change path name so we can paste to folders
filestopaste_list <- filestocopy_list |>
  purrr::map(tfpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fs::file_copy,
       path =  filestocopy_list,
       new_path = filestopaste_list)


## QA photos -------------

# do a little QA to be sure all the photos are there.Table shows photos that are missing. If all NA you are good to go.
t <- fpr::fpr_photo_qa_df(dat = d, dir_photos = paste0(targetdir, "/"))


## Move Pscis file -------------

##also move over the pscis file
fs::file_copy(path = fs::path('data', name_submission),
              new_path = fs::path(targetdir, name_submission),
              overwrite = T)

#make a little readme for the pdf for upload to ecocat and other details
writeLines(
  paste(
    "Online interactive report is located at: ",
    params$report_url,
    "",
    "A versioned pdf of the report can be downloaded from: ",
    paste0(params$repo_url, "/blob/main/docs/", params$pdf_name),
    "",
    "Raw data is available here: ",
    paste0(params$repo_url, "/tree/main/data"),
    "",
    "All scripts to produce online interactive reporting and pdf are located at: ",
    params$repo_url,
    sep = "\n"
  ),
  fs::path(targetdir, 'readme.txt')
)

