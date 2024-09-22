# Scripts to transfer photos and excel file to the PSCIS submission folder

##QA to be sure that you have all 6 required photos for submission to PSCIS
##convert jpg (or other formats - need to code) to JPG for consistency and to avoid issues with submission/reporting
##move the photos and spreadsheet ready for submission to pscis

## this script is special because the data was split between 2 different mergin projects, so the photos had to be
## extracted from each separately

source('scripts/packages.R')
source('scripts/functions.R')

## Phase 1-----------------------------------------------------------------
library(fpr)
library(tidyverse)
library(fs)


# PSCIS Submissions -------------

tfpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

tfpr_photo_change_name <- function(filenames_to_change = filestocopy_list){
  gsub(filenames_to_change, pattern = path, replacement = targetdir)
}


name_repo <- 'fish_passage_fraser_2023_reporting'
name_pdf <- 'fish_passage_fraser_2023_reporting.pdf' #see the output.yml
url_github <- 'https://github.com/NewGraphEnvironment/'
url_gitpages <- 'https://newgraphenvironment.github.io/'
name_submission <- 'pscis_phase1.xlsm'


# need to add photos to local machine to upload to PSCIS
targetdir = fs::path('~/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/2023/fraser')

# folders must be created and photos extracted for each project separately (or else we will run into errors bc it will
# look for all photos in one project when they are actually spit between two project)


### First project: sern_lchl_necr_fran_2023

path <- fs::path('~/Projects/gis/sern_lchl_necr_fran_2023/ignore_mobile/photos')

d <- read.csv(fs::path_wd('data/backup/sern_lchl_necr_fran_2023/form_pscis_2023.csv'))




folderstocopy<- d$my_crossing_reference %>% as.character()

path_to_photos <- fs::path(path, folderstocopy)

# here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface

fs::dir_create(targetdir)

folderstocreate<- fs::path(targetdir, folderstocopy)

##create the folders
fs::dir_create(folderstocreate)


# Identify photos that should be copied over into file
filestocopy_list <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))
empty_files <- empty_idx %>% tfpr_filter_list()

##rename long names if necessary

photo_sort_tracking <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_document_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))

##here we back up a csv that gives us the new location and name of the original JPG photos.

##burn to csv
photo_sort_tracking %>%
  readr::write_csv(file = 'data/photos/photo_sort_tracking_phase1.csv')

## change path name so we can paste to folders
filestopaste_list <- filestocopy_list %>%
  map(tfpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fs::file_copy,
       path =  filestocopy_list,
       new_path = filestopaste_list)



# Now we repeat this process with the second project

### Second project: sern_simpcw_2023

path <- fs::path('~/Projects/gis/sern_simpcw_2023/ignore_mobile/photos')

d <- read.csv(fs::path_wd('data/backup/sern_simpcw_2023/form_pscis_2023.csv'))

folderstocopy<- d$my_crossing_reference %>% as.character()

path_to_photos <- fs::path(path, folderstocopy)

# here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface

fs::dir_create(targetdir)

folderstocreate<- fs::path(targetdir, folderstocopy)

##create the folders
fs::dir_create(folderstocreate)


# Identify photos that should be copied over into file
filestocopy_list <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))
empty_files <- empty_idx %>% tfpr_filter_list()


##rename long names if necessary

photo_sort_tracking <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_document_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))

##here we back up a csv that gives us the new location and name of the original JPG photos.

##burn to csv
photo_sort_tracking %>%
  readr::write_csv(file = 'data/photos/photo_sort_tracking_phase1_simpc.csv')

## change path name so we can paste to folders
filestopaste_list <- filestocopy_list %>%
  map(tfpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fs::file_copy,
       path =  filestocopy_list,
       new_path = filestopaste_list)




##also move over the pscis file
fs::file_copy(path = fs::path('data', name_submission),
              new_path = fs::path(targetdir, name_submission),
              overwrite = T)

#make a little readme for the pdf for upload to ecocat and other details
writeLines(
  paste(
    "Online interactive report is located at: ",
    paste0(url_gitpages, name_repo),
    "",
    "A versioned pdf of the report can be downloaded from: ",
    paste0(url_github, name_repo, "/raw/main/docs/", name_pdf),
    "",
    "Raw data is available here: ",
    paste0(url_github, name_repo, "/tree/main/data"),
    "",
    "All scripts to produce online interactive reporting and pdf are located at: ",
    paste0(url_github, name_repo),
    sep = "\n"
  ),
  fs::path(targetdir, 'readme.txt')
)

##in the future we will need to add the copy calls to move the directory off of onedrive to
#  the windows machine used for project submission.  Don't want to set up the machine right now
# so will do it by hand. What an insane pain

################################################################################################################
#--------------here is a qa step I forgot about---------------------------------
################################################################################################################
d1  <- read.csv(fs::path_wd('data/backup/sern_lchl_necr_fran_2023/form_pscis_2023.csv'))
d2 <- read.csv(fs::path_wd('data/backup/sern_simpcw_2023/form_pscis_2023.csv'))
d_all <- dplyr::bind_rows(d1, d2)

# do a little qa to be sure all the photos are there
# we will cheat and jump right to onedrive since they are all together there
# its importnat (for now - till we go to fs) to

t <- fpr::fpr_photo_qa_df(dat = d_all, dir_photos = "/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/2023/fraser/")
