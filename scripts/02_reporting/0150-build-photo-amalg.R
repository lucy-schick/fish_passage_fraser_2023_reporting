# Build photo amalgamation for each site

## Params
dir_photos_onedrive <- fs::path_expand(fs::path("~/Library/CloudStorage/OneDrive-Personal/Projects", params$job_name, "data/photos/"))


# get a list of sites to burn
sites_l <- fpr::fpr_sp_gpkg_backup(
  form_pscis,
  update_site_id = FALSE,
  write_to_rdata = FALSE,
  write_to_csv = FALSE,
  write_back_to_path = FALSE,
  return_object = TRUE
) |>
  dplyr::distinct(site_id) |>
  dplyr::arrange(site_id) |>
  dplyr::pull(site_id)


# burn the amalgamated photos to onedrive
sites_l |>
  purrr::map(fpr::fpr_photo_amalg_cv, dir_photos = paste0(dir_photos_onedrive, "/"))
