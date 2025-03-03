# Build photo amalgamation for each site

###### START OF CODE TO ADDRESS issue https://github.com/NewGraphEnvironment/fish_passage_fraser_2023_reporting/issues/129 ######

# This is a bit funky for fraser 2024 - see issue above

#2024 data

## Params
dir_photos_2024 <- fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects", params$job_name, "data/photos/sorted"))
# form_pscis_2024 is stored in the sqlite
source("scripts/02_reporting/0120-read-sqlite.R")


# get a list of sites to burn
sites_l <- form_pscis_2024 |>
  dplyr::distinct(site_id) |>
  dplyr::arrange(site_id) |>
  dplyr::pull(site_id)


# burn the amalgamated photos to onedrive
sites_l |>
  purrr::map(fpr::fpr_photo_amalg_cv, dir_photos = paste0(dir_photos_2024, "/"))


#2023 data

# Params
# All photos from `sern_simpcw_2023` and `sern_lchl_necr_fran_2023` are on OneDrive now so we can use `dir_photos_2024`
# form_pscis_2023 is in the sqlite

sites_l <- form_pscis_2023 |>
  dplyr::distinct(site_id) |>
  dplyr::arrange(site_id) |>
  dplyr::pull(site_id)


# burn the amalgamated photos to mergin
sites_l |>
  purrr::map(fpr::fpr_photo_amalg_cv, dir_photos = paste0(dir_photos_2024, "/"))


###### END OF CODE TO ADDRESS issue https://github.com/NewGraphEnvironment/fish_passage_fraser_2023_reporting/issues/129 ######








## This is the normal code, used when we don't have the issues mentioned above, commenting out for now so we can use it in the next projects

# ## Params
# dir_photos_onedrive <- fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects", params$job_name, "data/photos/"))
#
#
# # get a list of sites to burn
# sites_l <- fpr::fpr_sp_gpkg_backup(
#   form_pscis,
#   update_site_id = FALSE,
#   write_to_rdata = FALSE,
#   write_to_csv = FALSE,
#   write_back_to_path = FALSE,
#   return_object = TRUE
# ) |>
#   dplyr::distinct(site_id) |>
#   dplyr::arrange(site_id) |>
#   dplyr::pull(site_id)
#
#
# # burn the amalgamated photos to onedrive
# sites_l |>
#   purrr::map(fpr::fpr_photo_amalg_cv, dir_photos = paste0(dir_photos_onedrive, "/"))
