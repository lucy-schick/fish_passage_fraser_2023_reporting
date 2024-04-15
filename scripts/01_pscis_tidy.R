# Tidy raw data from the field forms
# NOTE THAT A BUNCH OF CHANGES WERE MADE TO THIS AFTER IT WAS ACTUALLY DONE AS WE MADE NEW FUNCTIONS DURING
# THE PROCESSING OF THIS REPO.  CHANGED TO TRY TO REDUCE CONFUSION IN THE FUTURE BUT IT MAY NOT RUN AS EXPECTED

# name the project directory we are pulling from
path <- "~/Projects/gis/sern_lchl_necr_fran_2023"

# fpr_sp_gpkg_backup should be used here to import, assign site id, and assign utms.  Can write_to_csv = TRUE,
# write_to_rdata = TRUE and commit to get original record before any changes are made.
form_pscis <- fpr_sp_gpkg_backup(
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = TRUE)


# check for duplicates
form_pscis %>%
  filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  filter(n()>1)

# check for sites that have a culvert length over 99.9 or a fill depth over 9.9 asanything over this will cause error
# in submission sheet
# NOTE: THIS SHOULD BE TURNED INTO CASE_WHEN STATEMENTS AND INCLUDED AS PART OF THE TIDY. PROCESS IS TO CHANGE TO THESE
# MAXIMUMS AND APPEND NOTE TO ASSESSMENT COMMENTS - NEXT TIME - WE SHOULD SCRIPT THIS
form_pscis %>%
  filter(length_or_width_meters > 99.9 | fill_depth_meters > 9.9)

# consider records without a date if there is no into.  Had weird ones that needed removal before...
# look for any sites that have a date_time_start that is NA
form_pscis %>%
  filter(is.na(date_time_start))


# clean the form
form_pscis_cleaned <- form_pscis %>%
  # remove the site used to make the form
  filter(site_id != '12345' | !is.na(date_time_start)) %>%

  #split date time column into date and time
  dplyr::mutate(
    date_time_start = lubridate::ymd_hms(date_time_start),
    date = lubridate::date(date_time_start),
    time = hms::as_hms(date_time_start),

    # This should be done in the form as per https://github.com/NewGraphEnvironment/dff-2022/issues/119#issuecomment-1781242709
    across(contains('yes_no'), ~replace_na(.,'No')),

    # some numeric fields for CBS have NA values when a user input 0 https://github.com/NewGraphEnvironment/dff-2022/issues/119#issuecomment-1781242709
    # this could lead to errors as is. Should be fixed in form if possible
    across(c(outlet_drop_meters, outlet_pool_depth_0_01m, culvert_slope_percent, stream_slope),
           ~case_when(
             crossing_type == 'Closed Bottom Structure' ~
               replace_na(.,0),
             TRUE ~ .
           )),
    stream_name = str_replace_all(stream_name, 'Trib ', 'Tributary '),
    road_name = str_replace_all(road_name, 'Hwy ', 'Highway '),
    # back up the original assessment comments so we can redo this amalgamation of text if we need to
    assessment_comment_og = assessment_comment,
  ) %>%
  select(-time) %>%
  ################################################################################################################
  #---------------------------------------------START HACK---------------------------------------------------
  ################################################################################################################
  # THIS STEP WAS ADDED AFTER THE FACT AND CAME FROM O1b_pscis_tidy.R.  In the future we should have
  # all of these columns already in the form as per https://github.com/NewGraphEnvironment/dff-2022/issues/119#issuecomment-1781242709
  dplyr::mutate(
    # we need more MoTi sturcture columns so that we can add multiple structure IDs in Q)
    moti_chris_culvert_id2 = NA_integer_,
    moti_chris_culvert_id3 = NA_integer_,
    # we may as well have a place for priority follow up ranking and citation keys too.
    # no fix, low, medium and high.  Justify why in the assessment comments
    my_priority = NA_character_,
    my_citation_key1 = NA_character_,
    my_citation_key2 = NA_character_,
    my_citation_key3 = NA_character_) %>%
  ################################################################################################################
  #---------------------------------------------END HACK---------------------------------------------------
  ################################################################################################################

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

# burn cleaned copy to QGIS project as new file in the `/data_field/2023/form_pscis_2023.gpkg' format, the pscis clean section can be repeated again when changes are made in Q
form_pscis_cleaned %>%
  sf::st_write(paste0(path, '/data_field/2023/form_pscis_2023.gpkg'), append=F, delete_dsn=T)

# burn to version controlled csv, and commit again with informative commit
fpr_sp_gpkg_backup(
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = FALSE)

