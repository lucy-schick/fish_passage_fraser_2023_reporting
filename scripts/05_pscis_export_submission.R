# Export pscis data to csv for cut and paste into PSCIS submission spreadsheet

# read in cleaned form from Q after review and finalization
# first we back up the gpkg in the repo and update the coordinate columns in the gpkg in QGIS
pscis_export_raw <- fpr_sp_gpkg_backup(
  path_gpkg = "~/Projects/gis/sern_lchl_necr_fran_2023/data_field/2023/form_pscis_2023.gpkg",
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = TRUE,
  write_to_csv = TRUE,
  # this versions on git everytime due to metadata and can't be tracked visually. Should only be committed when
  # csv is versioned
  write_to_rdata = TRUE,
  return_object = TRUE)


# prep for csvs for cut and paste by subsetting columns to those in spreadsheet
# this project is simplified greatly in that it has only phase 1 sites
pscis_export <- pscis_export_raw %>%
  # only select columns from template object site_id and date_time_start
  dplyr::select(
    any_of(names(fpr_xref_template_pscis())),
    site_id,
    date_time_start
    ) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  dplyr::select(-stream_width_ratio:-barrier_result) %>%
  sf::st_drop_geometry() %>%
  # arrange so easy to copy/paste
  arrange(crossing_type,
          continuous_embeddedment_yes_no,
          backwatered_yes_no,
          crew_members,
          date_time_start)

# write to the imports_extracted dir. This is data we import to the project but they are extracted from other places.
dir.create("data/imports_extracted")
pscis_export %>%
  readr::write_csv(paste0('data/imports_extracted/pscis_export_submission.csv'), na='')
