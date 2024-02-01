# this should be run anytime data is updated in QGIS.
# Some times we move points manually and add details about things that we find after.  This updates the site_id and utm
# columns in the gpkg and versions those changes with git through a csv and rdata file.
# we probably don't need the rdata file but will version too till that can be confirmed (see ?fpr_sp_gpkg_backup for d's)

# can be done in 04_pscis_export.R if creating export spreadsheet as well

path <- "~/Projects/gis/sern_lchl_necr_fran_2023/data_field/2023/form_pscis_2023.gpkg"

fpr_sp_gpkg_backup(
  path_gpkg = path,
  dir_backup = "data/test/",
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = TRUE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = FALSE)
