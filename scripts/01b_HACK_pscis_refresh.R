# Re-tidy data from the field forms that was once cleaned with 01_pscis_tidy.R.
# THIS IS AN OUT OF STEP WORKFLOW - WE SHOULD HAVE DONE MUSCH OF THIS IN 01_pscis_tidy.R
# cache the current comments in `assessment_comment2`

path <- "~/Projects/gis/sern_lchl_necr_fran_2023/data_field/2023/form_pscis_2023.gpkg"

form_pscis <- fpr_sp_gpkg_backup(
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # b/c we see no changes to the csv we know the rdata change is just metadata
  # so we git checkout data/backup/form_pscis_2023.RData (revert changes to last commit) and turn this false and rerun
  write_to_rdata = FALSE,
  return_object = TRUE)


# refresh the form so that we can amalgamate all the commments and moti/time info after QGIS review
form_pscis_refreshed <- form_pscis %>%
  dplyr::mutate(
    # back up the updated assessment comments so we can redo this amalgamation of text if we need to
         assessment_comment2 = assessment_comment,
         # trim the appended info off the comments so we get any changes made by Mateo already but are able to
         # append the time and Moti chris_culvert_id stuff later.  t <- as_tibble(form_pscis$assessment_comment)
         # after reviewing the assessment_comment column it looks like we have either a time as the last input or the text beginning with
         # "Ministry of Transportation" so we can use this to split the text and keep everything to the left as
         # the new `assessment_comment`
         assessment_comment = str_split(assessment_comment,
                                        pattern = "Ministry of Transportation",
                                        simplify = TRUE)[, 1],
         assessment_comment = str_split(assessment_comment,
                                        pattern = "\\s+\\d{2}:\\d{2}",
                                        simplify = TRUE)[, 1]
         )



# burn cleaned copy to QGIS project gpkg, the pscis clean section can be repeated again when changes are made in Q
# if we don't name the layer - the layer becomes the filename sans extension
form_pscis_refreshed %>%
  sf::st_write(path, append = FALSE,
               delete_dsn = TRUE)

# we will also save the refreshed form as an RData file so we can easily reload it in the future
# use fpr to be consistent as far as row order issue #57
# now that it is in Q we back up all the changes by reading it back in again
fpr_sp_gpkg_backup(
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # b/c we see no changes to the csv we know the rdata change is just metadata
  # so we git checkout data/backup/form_pscis_2023.RData (revert changes to last commit) and turn this false and rerun
  write_to_rdata = TRUE,
  return_object = FALSE)
