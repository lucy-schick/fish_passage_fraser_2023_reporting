# grab the files from mergin and move to project using linux cmd
# mv -v ~/Projects/gis/mergin/bcfishpass_elkr_20220904/photos/* ~/Projects/current/2022-056-nupqu-elk-cwf/data/photos/mergin/originals
# mv -v ~/Projects/gis/mergin/bcfishpass_skeena_20220823-v225/photos/* ~/Projects/current/2022-049-sern-skeena-fish-passage/data/photos/mergin/

# Build Local Directory  ------------------------------------------------------------
dir_project <- 'sern_lchl_necr_fran_2023'
dir_job_id <- '2023-065-sern-capacity'
dir_photos_mergin_raw <- paste0("~/Projects/gis/", dir_project, "/ignore_mobile/photos/")
dir_photos_mergin_resized <- paste0("~/Projects/gis/", dir_project, "/ignore_mobile/photos_resized/")
# dir_photos_processed_final <- paste0("~/Projects/data/", dir_project, "/photos/")
dir_photos_originals <- paste0("~/Projects/current/", dir_job_id, "/fraser/data/photos/")
dir_photos_onedrive <- paste0("~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/fraser/photos/")

# we are going to resize the original photos and move them to onedrive just in case we need to do homework
dir.create(paste0(dir_photos_onedrive, "ai"), recursive = TRUE)
dir.create(paste0(dir_photos_onedrive, "mw"), recursive = TRUE)

fpr_photo_resize_batch(
  dir_source = paste0(dir_photos_originals,"ai/originals"),
  dir_target = paste0(dir_photos_onedrive, "ai")
)

fpr_photo_resize_batch(
  dir_source = paste0(dir_photos_originals,"mw/originals"),
  dir_target = paste0(dir_photos_onedrive, "mw")
)



# QGIS mergin photos-----------------------------------------------------------------------------------------------------
## Clean up the mergin file----------------------------------------------------------------------------------------------------
# remove photos.txt file included in project when created (was to allow mergin git to see the photos dir) but needs
# to be removed or ignored to not break fpr_photo_resize_batch
file.remove(
  paste0(dir_photos_mergin_raw, "/photos.txt")
)

## Resize----------------------------------------------------------------------------------------------------
# resize the photos and change the extension to JPG for consistency and to avoid issues with fpr_photo calls in reporting
# sync to mergin after copying to new dir (resized) and removing originals
# record version number of mergin project in issue for now to track

# get a list of the photos
p <- list.files(dir_photos_mergin_raw, full.names = T, recursive = T)


# see how large the photos are in MB rounded to 1 decimal using purrr
s <- p %>%
  purrr::map(file.info) %>%
  purrr::map_dbl("size")/1024/1024

# identify the range of sizes
range(s)
# [1] 0.06309891 0.57638550 Not bad.  Lets resize anyway so that we know they fit the reporting

# create the target directory
dir.create(dir_photos_mergin_resized, recursive = TRUE)

fpr_photo_resize_batch(
  dir_source = dir_photos_mergin_raw,
  dir_target = paste0(dir_photos_mergin_resized)
)

# quick check to see if the photos are all accounted for
identical(
  length(
    list.files(dir_photos_mergin_raw, full.names = T, recursive = T)),
  length(
    list.files(dir_photos_mergin_resized, full.names = T, recursive = T))
)

# erase all the photos in the original directory
file.remove(
  list.files(dir_photos_mergin_raw, full.names = T, recursive = T)
)

# recreate the photos.txt file so the form still works
file.create(
  paste0(dir_photos_mergin_raw, "photos.txt")
)




########################################################################################################################
# Rename the photos and sort into directories -----------------------------------------------------------------------------------------------------


form_pscis_photos_raw <- sf::st_read(
  dsn = paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg')) %>%
  arrange(site_id)

# check for duplicate sites
form_pscis_photos_raw %>%
  dplyr::filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  dplyr::filter(n()>1) %>%
  nrow()

# check for empty sites
form_pscis_photos_raw %>%
  dplyr::filter(is.na(site_id)) %>%
  nrow()

# create site photo directories right on mergin to make them easy to share...
form_pscis_photos_raw %>%
  pull(site_id) %>%
  as.character() %>%
  purrr::map(
    fpr::fpr_photo_folders, path = dir_photos_mergin_raw
  )

# NOTE - accidentally didn't save the actual workflow for this so this may not be exactly what I did
fpr::fpr_photo_rename(
  dat = form_pscis_photos_raw,
  dir_from_stub = dir_photos_mergin_resized,
  dir_to_stub = dir_photos_mergin_raw
)

# looks like 9900446 has no photos at all with no note of it in the comments... Not sure what to do about that
qa_missing <- fpr_photo_qa_missing_all(
  dat = form_pscis_photos_raw,
  dir_photos = dir_photos_mergin_raw
) %>%
  pull(site)

qa_all <- fpr::fpr_photo_qa(
  dat = form_pscis_photos_raw,
  dir_photos = dir_photos_mergin_raw
) %>%
  data.table::rbindlist(fill = TRUE)

qa <- fpr_photo_qa_df(
  dat = form_pscis_photos_raw,
  dir_photos = dir_photos_mergin_raw
)

# here is the test for missing individual photos
test <- fpr::fpr_photo_qa() %>%
  bind_rows() %>%
  dplyr::filter(if_any(everything(), is.na))



################################################################################################################
#--------------------------------------------------Simpc---------------------------------------------------
################################################################################################################


# Build Local Directory  ------------------------------------------------------------
dir_project <- 'sern_simpcw_2023'
dir_job_id <- '2023-065-sern-capacity'
dir_photos_mergin_raw <- paste0("~/Projects/gis/", dir_project, "/ignore_mobile/photos/")
dir_photos_mergin_resized <- paste0("~/Projects/gis/", dir_project, "/ignore_mobile/photos_resized/")
# dir_photos_processed_final <- paste0("~/Projects/data/", dir_project, "/photos/")
dir_photos_originals <- paste0("~/Projects/current/", dir_job_id, "/fraser/data/photos/")
dir_photos_onedrive <- paste0("~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/fraser/photos/")


# QGIS mergin photos-----------------------------------------------------------------------------------------------------
## Clean up the mergin file----------------------------------------------------------------------------------------------------
# remove photos.txt file included in project when created (was to allow mergin git to see the photos dir) but needs
# to be removed or ignored to not break fpr_photo_resize_batch
file.remove(
  paste0(dir_photos_mergin_raw, "/photos.txt")
)

## Resize----------------------------------------------------------------------------------------------------
# resize the photos and change the extension to JPG for consistency and to avoid issues with fpr_photo calls in reporting
# sync to mergin after copying to new dir (resized) and removing originals
# record version number of mergin project in issue for now to track

# get a list of the photos
p <- list.files(dir_photos_mergin_raw, full.names = T, recursive = T)


# see how large the photos are in MB rounded to 1 decimal using purrr
s <- p |>
  purrr::map(file.info) |>
  purrr::map_dbl("size")/1024/1024

# identify the range of sizes
range(s)
# [1] 0.2178898 7.3512239 HUUUUGGGGEEE.  Lets resize

# create the target directory
dir.create(dir_photos_mergin_resized, recursive = TRUE)

fpr_photo_resize_batch(
  dir_source = dir_photos_mergin_raw,
  dir_target = paste0(dir_photos_mergin_resized)
)

# quick check to see if the photos are all accounted for
identical(
  length(
    list.files(dir_photos_mergin_raw, full.names = T, recursive = T)),
  length(
    list.files(dir_photos_mergin_resized, full.names = T, recursive = T))
)

# erase all the photos in the original directory
file.remove(
  list.files(dir_photos_mergin_raw, full.names = T, recursive = T)
)

# recreate the photos.txt file so the form still works
file.create(
  paste0(dir_photos_mergin_raw, "photos.txt")
)




########################################################################################################################
# Rename the photos and sort into directories -----------------------------------------------------------------------------------------------------
path <- "~/Projects/gis/sern_simpcw_2023/data_field/2023/form_pscis_2023.gpkg"
# here we assume all sites were comleted digitally. Should confirm...
form_pscis_photos_raw <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  return_object = TRUE)


# # here we assume all sites were completed digitally. Should confirm...
# form_pscis_photos_raw <- sf::st_read(
#   dsn = paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg')) %>%
#   arrange(site_id)

# check for duplicate sites
form_pscis_photos_raw %>%
  dplyr::filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  dplyr::filter(n()>1) |>
  nrow()

# check for empty sites
form_pscis_photos_raw %>%
  dplyr::filter(is.na(site_id)) %>%
  nrow()

# create site photo directories right on mergin to make them easy to share...
form_pscis_photos_raw %>%
  pull(site_id) %>%
  as.character() %>%
  purrr::map(
    fpr::fpr_photo_folders, path = dir_photos_mergin_raw
  )

# rename the photos
fpr::fpr_photo_rename(
  dat = form_pscis_photos_raw,
  dir_from_stub = dir_photos_mergin_resized,
  dir_to_stub = dir_photos_mergin_raw
)

# QA
fpr::fpr_photo_qa_df(dat = form_pscis_photos_raw, dir_photos = dir_photos_mergin_raw)


fpr_photo_qa_missing_all(
  dat = form_pscis_photos_raw,
  dir_photos = dir_photos_mergin_raw
)

# before we nuke the resized folder we should compare that all the photos in the resized directory are accounted for
# this was not done but should be.  setdiff with a list of file names starting with the resized directory should work
# to identify any missing photos
setdiff(
  list.files(dir_photos_mergin_resized, full.names = T, recursive = T),
  list.files(dir_photos_mergin_raw, full.names = T, recursive = T)
)

fs::dir_delete(dir_photos_mergin_resized)

################################################################################################################
#--------------------------------------------------qa amalgamated photos---------------------------------------------------
################################################################################################################

# read in both raw forms and join
path_form1 <- "~/Projects/gis/sern_lchl_necr_fran_2023/data_field/2023/form_pscis_2023.gpkg"
path_form2 <- "~/Projects/gis/sern_simpcw_2023/data_field/2023/form_pscis_2023.gpkg"
dir_photos <- "~/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/2023/fraser/phase1/"

form_pscis_photos_raw1 <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path_form1,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  return_object = TRUE)

form_pscis_photos_raw2 <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path_form2,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  return_object = TRUE)

form_both <- bind_rows(
  form_pscis_photos_raw1,
  form_pscis_photos_raw2
)


fpr_photo_qa_missing_all(
  dat = form_both,
  dir_photos = dir_photos
) %>%
  pull(site)

qa_all <- fpr::fpr_photo_qa2(
  dat = form_both,
  dir_photos = dir_photos
) %>%
  data.table::rbindlist(fill = TRUE)


# here is the test for missing individual photos
fpr::fpr_photo_qa2(
  dat = form_both,
  dir_photos = dir_photos
) |>
  bind_rows() |>
  dplyr::filter(if_any(everything(), is.na))


################################################################################################################
#--------------------------------------------------strip metadata---------------------------------------------------
################################################################################################################

# copy over the entire submission directory to a new location
dir_og <- fs::path_expand("~/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/2023/fraser/phase1")
dir_new <- fs::path_expand("~/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/2023/fraser/photo_meta_rm")

fs::dir_create(dir_new)
fs::dir_copy(
  dir_og,
  dir_new
)

# list the jpg files in the new location

paths_ls <- fs::dir_ls(
  dir_new, type = "file", glob = "*.JPG", recurse = TRUE
)

# have a quick look at some
head(paths_ls)

# test with an example photo
# test <- "/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/submissions/PSCIS/2023/fraser/phase1_photos_metadata_stripped/phase1/20231003_145000_road.JPG"
# t <- exifr::read_exif(test)
# rfp_photo_metadata_rm(test)
# t2 <- exifr::read_exif(test)
# fs::file_delete(test)


# remove the metadata from all the photos listed from the new directory. Be careful here. Make sure you have the correct list!!
log_df <- paths_ls |>
  purrr::map_dfr(rfp_photo_metadata_rm)

# burn out a record of what happened in case it is helpful later
log_df |>
  readr::write_csv(
    "data/backup/photo_strip_meta.csv"
  )

# summarize the results of the log
t <- log_df |>
  dplyr::mutate(
    warning = dplyr::case_when(
      stringr::str_detect(status_message, "Warning") ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::group_by(warning) |>
  summarise(n())

#############################################################################################
####################  NOT RUN YET  ##############################################################
##############################################################################################
# build photo amalgamation for each site ------------------------------------------------

