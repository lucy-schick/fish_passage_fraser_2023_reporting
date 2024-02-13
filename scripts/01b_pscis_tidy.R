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
  readr::write_csv(paste0('data/backup/form_pscis_2023.csv'), na='')

#---------------------pscis export only--------------------------

# in this section we will read in cleaned form from Q after review and finalization,
# and then get the names of the input template so we can copy and past special directly into the spreadsheet

# read in form from Q
form_pscis <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg')) %>%
  st_drop_geometry()

# this is a table that cross references column names for pscis table and has the columns in the same order as the spreadsheet
xref_names_pscis <- fpr::fpr_xref_pscis

# get order of columns as per the excel template spreadsheet
# this can be used as a select(all_of(name_pscis_sprd_ordered)) later
# to order columns for the field form and/or put the field entered table in order
name_pscis_sprd_ordered <- fpr::fpr_xref_pscis %>%
  dplyr::filter(!is.na(spdsht)) %>%
  select(spdsht) %>%
  pull(spdsht)

# see names that coincide between the xref table and what we have
intersect(name_pscis_sprd_ordered, names(form_pscis))

# see which are different
setdiff(name_pscis_sprd_ordered, names(form_pscis))
# order matters
setdiff(names(form_pscis), name_pscis_sprd_ordered)

# to use all the columns from the template first we make an empty dataframe from a template
template <- fpr::fpr_import_pscis() %>%
  slice(0)

# then we join it to our populated spreadsheet
# we may as well keep all the columns that are not in the spreadsheet and append to the end
form <- bind_rows(
  template,
  form_pscis
) %>%
  # only select columns from template object
  select(any_of(names(template))) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  select(-stream_width_ratio:-barrier_result) %>%
  # then arrange it by pscis id to separate phase 1s from reassessments
  arrange(pscis_crossing_id, date)

# burn to a csv ready for copy and paste to template
form %>% readr::write_csv(paste0(
  'data/dff/pscis_simp_export.csv'), na='')

# --------------------moti climate change ---------------------------
#
# moti_names <- setdiff(names(form_pscis), name_pscis_sprd_ordered) %>%
#   enframe(name = NULL, value = 'spdsht') %>%
#   mutate(report = str_to_title(spdsht),
#          report = stringr::str_replace_all(report, '_', ' '),
#          report = stringr::str_replace_all(report, 'event affecting culvert', ''),
#          report = stringr::str_replace_all(report, 'id', 'ID'),
#          report = stringr::str_replace_all(report, 'Gps', 'GPS'),
#          report_include = case_when(
#            str_detect(spdsht,
#                       'photo|long|lat|mergin|surveyor|gps|width|utm|time|source|camera|aggregated|rowid|geometry') ~ F,
#            T ~ T
#          ),
#          id_join = NA_integer_,
#          id_side = NA_integer_) %>%
#   dplyr::filter(spdsht != 'stream_width_ratio_score')
# # dplyr::filter(report_include == T)
#
# # burn out to csv so we can manually do the descriptions
# moti_names %>%
#   write_csv('data/inputs_raw/moti_climate.csv')
#
# # we want to get our climate change risk information summarized for each site.
# # if we were to add a tag to the names or a xref tie that tells us if each column is part of this or not we would get ahead...
#
# ##-------------------- moti correct chris_culvert_id-----------------------
# # one thing we definitely need to do it get the chris_culvert_id for each site as we used the wrong one on our forms. We should be able to cross ref the ids from bcdata
# # so let's try that first
#
#
# # get_this <- bcdata::bcdc_tidy_resources('ministry-of-transportation-mot-culverts') %>%
# #   dplyr::filter(bcdata_available == T)  %>%
# #   pull(package_id)
# #
# # dat <- bcdata::bcdc_get_data(get_this)
# #
# # moti_raw <- dat %>%
# #   purrr::set_names(nm = janitor::make_clean_names(names(dat)))
# #
# # # match our sites to ids
# # moti <- left_join(
# #   form_prep,
# #
# #   moti_raw %>% select(culvert_id, chris_culvert_id) %>% sf::st_drop_geometry(),
# #
# #   by = c('mot_culvert_id' = 'culvert_id')
# # )
#
# # the names must have changed so lets use the file in the mergin project as we know that one is the same
# moti_raw <- sf::st_read('../../gis/mergin/bcfishpass_skeena_20220823/clipped_moti_culverts_sp.gpkg') %>%
#   sf::st_drop_geometry()
#
# moti <- left_join(
#   form_prep,
#
#   moti_raw %>% select(culvert_id, chris_culvert_id),
#
#   by = c('mot_culvert_id' = 'culvert_id')
# )
#
# # burn to a csv
# moti %>%
#
#
#   #sort data by date
#   arrange(date) %>%
#
#   readr::write_csv(paste0(
#     'data/dff/form_pscis_moti_',
#     format(lubridate::now(), '%Y%m%d'),
#     '.csv'))

