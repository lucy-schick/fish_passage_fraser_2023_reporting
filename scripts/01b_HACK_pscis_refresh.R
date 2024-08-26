library(fpr)
library(tidyverse)

# Re-tidy data from the field forms that was once cleaned with 01_pscis_tidy.R.
# THIS IS AN OUT OF STEP WORKFLOW - WE SHOULD HAVE DONE MUSCH OF THIS IN 01_pscis_tidy.R
# cache the current comments in `assessment_comment2`

path <- "~/Projects/gis/sern_lchl_necr_fran_2023/data_field/2023/form_pscis_2023.gpkg"
dir_backup = "data/backup/sern_lchl_necr_fran_2023/"

# path <- "~/Projects/gis/sern_simpcw_2023/data_field/2023/form_pscis_2023.gpkg"
# dir_backup = "data/backup/simpcw/"


# running through this again for the simpcw project
form_pscis <- fpr::fpr_sp_gpkg_backup(
  # added this to backup and distinquish projects
  dir_backup = dir_backup,
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
form_pscis_refreshed <- form_pscis |>
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
form_pscis_refreshed |>
  sf::st_write(path, append = FALSE,
               delete_dsn = TRUE)

# we will also save the refreshed form as an RData file so we can easily reload it in the future
# use fpr to be consistent as far as row order issue #57
# now that it is in Q we back up all the changes by reading it back in again
fpr::fpr_sp_gpkg_backup(
  # added this to backup and distinquish projects
  dir_backup = dir_backup,
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # b/c we see no changes to the csv we know the rdata change is just metadata
  # so we git checkout data/backup/form_pscis_2023.RData (revert changes to last commit) and turn this false and rerun
  write_to_rdata = TRUE,
  return_object = FALSE)




################################################################################################################
#--------------------------------------------------Add some columns to help sort---------------------------------------------------
################################################################################################################

path <- "~/Projects/gis/sern_lchl_necr_fran_2023/data_field/2023/form_pscis_2023.gpkg"
dir_backup = "data/backup/sern_lchl_necr_fran_2023/"

# path <- "~/Projects/gis/sern_simpcw_2023/data_field/2023/form_pscis_2023.gpkg"
# dir_backup = "data/backup/simpcw/"


# running through this again for the simpcw project
form_pscis <- fpr::fpr_sp_gpkg_backup(
  # added this to backup and distinquish projects
  dir_backup = dir_backup,
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # b/c we see no changes to the csv we know the rdata change is just metadata
  # so we git checkout data/backup/form_pscis_2023.RData (revert changes to last commit) and turn this false and rerun
  write_to_rdata = FALSE,
  return_object = TRUE)

# see the available columns
fpr_db_query(fpr_dbq_lscols())

cols_pull <- c('modelled_crossing_id',
               'channel_width', 'mad_m3s',
               'observedspp_upstr',
               'bt_rearing_km',
               'bt_spawning_km',
               'ch_rearing_km',
               'ch_spawning_km')

# Convert the column names to a comma-separated string
cols_pull_str <- glue::glue_sql("{`cols_pull`*}", .con = conn)


ids <- form_pscis |> pull(my_crossing_reference)

conn <- fpr::fpr_db_conn()


form_pscis_updated <- dplyr::left_join(form_pscis,

                                       fpr::fpr_db_query(
                                         glue::glue_sql("SELECT {cols_pull_str} FROM bcfishpass.crossings_vw WHERE modelled_crossing_id IN ({ids*})", .con = conn)
                                       ),
                                       by = c("my_crossing_reference" = "modelled_crossing_id")
)

DBI::dbDisconnect(conn)

form_pscis_updated |>
  # add a column to tag if we want a permit here
  dplyr::mutate(permit_2024 = NA_character_) |>
  sf::st_write(path, append = FALSE,
               delete_dsn = TRUE)

# we will also save the refreshed form as an RData file so we can easily reload it in the future
# use fpr to be consistent as far as row order issue #57
# now that it is in Q we back up all the changes by reading it back in again
fpr::fpr_sp_gpkg_backup(
  # added this to backup and distinquish projects
  dir_backup = dir_backup,
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # b/c we see no changes to the csv we know the rdata change is just metadata
  # so we git checkout data/backup/form_pscis_2023.RData (revert changes to last commit) and turn this false and rerun
  write_to_rdata = TRUE,
  return_object = FALSE)

t <- fpr::fpr_db_query("select * from bcfishpass.crossings_vw where modelled_crossing_id = 5400450")
