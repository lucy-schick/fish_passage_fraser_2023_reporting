# Export pscis data to csv for cut and paste into PSCIS submission spreadsheet

# this is a weird workflow b/c we have two different projects and want to combine them for one submission.

path1 <- "~/Projects/gis/sern_lchl_necr_fran_2023/data_field/2023/form_pscis_2023.gpkg"
dir_backup1 = "data/backup/sern_lchl_necr_fran_2023/"
path2 <- "~/Projects/gis/sern_simpcw_2023/data_field/2023/form_pscis_2023.gpkg"
dir_backup2 = "data/backup/sern_simpcw_2023/"

# read in cleaned form from Q after review and finalization
# first we back up the gpkg in the repo and update the coordinate columns in the gpkg in QGIS
pscis_export_raw1 <- fpr::fpr_sp_gpkg_backup(
  dir_backup = dir_backup1,
  path_gpkg = path1,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # this versions on git everytime due to metadata and can't be tracked visually. Should only be committed when
  # csv is versioned
  write_to_rdata = TRUE,
  return_object = TRUE)


pscis_export_raw2 <- fpr::fpr_sp_gpkg_backup(
  dir_backup = dir_backup2,
  path_gpkg = path2,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # this versions on git everytime due to metadata and can't be tracked visually. Should only be committed when
  # csv is versioned
  write_to_rdata = FALSE,
  return_object = TRUE)

setdiff(names(pscis_export_raw1), names(pscis_export_raw2))
setdiff(names(pscis_export_raw2), names(pscis_export_raw1))

pscis_export_raw <- dplyr::bind_rows(
  pscis_export_raw1,
  pscis_export_raw2
)

## Check for duplicates
dups <- pscis_export_raw %>%
  group_by(site_id) %>%
  filter(n() > 1)

# Site 15600468 was duplicated, issue here https://github.com/NewGraphEnvironment/fish_passage_fraser_2023_reporting/issues/110

## Adding in spell-checked comments
clean_comments <-  read.csv('data/inputs_extracted/clean_comments.csv')

pscis_export_raw_clean <-  dplyr::left_join(pscis_export_raw |>
                                              select(-assessment_comment),
                                            clean_comments,
                                            join_by(site_id == my_crossing_reference)) |>
  relocate(assessment_comment, .after = my_priority)


# prep for csvs for cut and paste by subsetting columns to those in spreadsheet
# this project is simplified greatly in that it has only phase 1 sites
pscis_export <- pscis_export_raw_clean %>%
  # Fix time zone, issue here https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/18
  dplyr::mutate(date_time_start_raw = date_time_start,
         date_time_start = lubridate::force_tz(date_time_start_raw, tzone = "America/Vancouver"),
         date_time_start = lubridate::with_tz(date_time_start, tzone = "UTC"),
         # Get time to append to comments
         date_time_start = lubridate::ymd_hms(date_time_start),
         date = lubridate::date(date_time_start),
         time = hms::as_hms(date_time_start)) |>
  # append moti ids to comments, differentiate between highway major structure, and add time to end
  dplyr::mutate(assessment_comment = dplyr::case_when(
    moti_chris_culvert_id > 1000000 ~ paste0(assessment_comment, ' MoTi chris_culvert_id: ', moti_chris_culvert_id),
    moti_chris_culvert_id < 1000000 ~ paste0(assessment_comment, ' MoTi chris_hwy_structure_road_id: ', moti_chris_culvert_id),
    TRUE ~ assessment_comment)) |>
  dplyr::mutate(assessment_comment = dplyr::case_when(moti_chris_culvert_id2 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id2), TRUE ~ assessment_comment)) |>
  dplyr::mutate(assessment_comment = dplyr::case_when(moti_chris_culvert_id3 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id3), TRUE ~ assessment_comment)) |>
  # add time to end
  dplyr::mutate(assessment_comment = paste0(assessment_comment, '. ', time)) |>
  # ditch time column
  dplyr::select(-time) |>
  # only select columns from template object site_id and date_time_start
  dplyr::select(
    dplyr::any_of(names(fpr::fpr_xref_template_pscis())),
    site_id,
    date_time_start
  ) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  dplyr::select(-stream_width_ratio:-barrier_result) %>%
  sf::st_drop_geometry() %>%
  # arrange so easy to copy/paste
  dplyr::arrange(crossing_type,
                 continuous_embeddedment_yes_no,
                 backwatered_yes_no,
                 crew_members,
                 date_time_start)

# write to the imports_extracted dir. This is data we import to the project but they are extracted from other places.
dir.create("data/inputs_extracted")
pscis_export %>%
  readr::write_csv(paste0('data/inputs_extracted/pscis_export_submission.csv'), na='')


## Add Structure, type, and size

# Only phase 1 assessments in this project
pscis_list <- fpr_import_pscis_all()
pscis_phase1 <- pscis_list %>% pluck('pscis_phase1')

# Get structure, type, and size
lfpr_structure_size_type(pscis_phase1)
