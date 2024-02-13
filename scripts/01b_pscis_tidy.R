# Re-tidy data from the field forms that was once cleaned with 01_pscis_tidy.R.
# cache the current comments in `assessment_comment2`, concatenate all `discuss` and `notes` columns and add the moti
# crossing IDs

# name the project directory we are pulling from
# dir_project <- 'sern_lchl_necr_fran_2023'
#
# # back up the current copy of the form
#
# form_pscis <- sf::st_read(
#   paste0(
#     '../../gis/',
#     dir_project,
#     '/data_field/2023/form_pscis_2023.gpkg')
# )

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
                                        simplify = TRUE)[, 1],
         # we need more MoTi sturcture columns so that we can add multiple structure IDs in Q)
         moti_chris_culvert_id2 = NA_integer_,
         moti_chris_culvert_id3 = NA_integer_,
         # we may as well have a place for priority follow up ranking and citation keys too.
         # no fix, low, medium and high.  Justify why in the assessment comments
         my_priority = NA_character_,
         my_citation_key1 = NA_character_,
         my_citation_key2 = NA_character_,
         my_citation_key3 = NA_character_) %>%
  # we want these new columns to land at a logical place in the table so we will reorder them
  dplyr::select(
    site_id,
    crew_members,
    date_time_start,
    my_priority,
    assessment_comment,
    contains("_notes"),
    contains("moti_chris_culvert_id"),
    contains("my_citation_key"),
    everything()) %>%
  arrange(crew_members, date_time_start)



# burn cleaned copy to QGIS project gpkg, the pscis clean section can be repeated again when changes are made in Q
# if we don't name the layer - the layer becomes the filename sans extension
form_pscis_refreshed %>%
  sf::st_write(path, append = FALSE,
               delete_dsn = TRUE)

# burn to version controlled csv, so changes can be viewed on git
# this is actually pretty ugly because even one minor change in a row and git sees the whole row change
# that is b/c git is for code and not designed for data.  We should use a different diff tool for this but we are not there yet

form_pscis_refreshed %>%
  readr::write_csv('data/backup/form_pscis_2023.csv', na='')

# we will also save the refreshed form as an RData file so we can easily reload it in the future
# use fpr to be consistent as far as row order issue #57

# we actually grab it back from Q
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
