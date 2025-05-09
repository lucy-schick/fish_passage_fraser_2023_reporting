# These are workflows that only need to happen once really so they are separate from those in tables.R. They often need to be re-run when data is updated.
# There is still a decent amount of old code here that I am leaving in case we need it later for some reason.
# The only code currently used comes before the OLD CODE HEADING in the outline.


# build priority spreadsheet ----------------------------------------------

# spreadsheet to build for input includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())


# Function to replace empty character and numeric values with NA
replace_empty_with_na <- function(x) {
  if(is.character(x) && length(x) == 0) return(NA_character_)
  if(is.numeric(x) && length(x) == 0) return(NA_real_)
  return(x)
}

# specify in index.Rmd YAML which species you want to use for the modelling
# For Skeena we use steelhead
# For Peace we use bull trout
# For Fraser we are using Chinook, maybe we should use bt?

# Convert the species-specific rearing column to a symbol upfront
model_species_rearing_km <- rlang::sym(paste0(params$model_species, "_rearing_km"))

hab_priority_prep <- form_fiss_site |>
  dplyr::select(
    stream_name = gazetted_names,
    local_name,
    site,
    location,
    ef,
    date_time_start
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    crew_members = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = crew_members)),
    length_surveyed = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = site_length)),
    hab_value = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = habitat_value_rating)),

    # Priority pulled from form_pscis
    priority = list(fpr::fpr_my_bcfishpass(dat = form_pscis_2024, site = site, col_filter = pscis_crossing_id, col_pull = my_priority)),

    # Comments field
    comments = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = comments)),

    # Unquoting only for the dynamic species-specific column
    upstream_habitat_length_m = list(
      fpr::fpr_my_bcfishpass(site = site, col_pull = !!model_species_rearing_km, round_dig = 4)
    ),
    upstream_habitat_length_m = list(round((upstream_habitat_length_m * 1000), digits = 0)),

    # Static column, no unquoting needed
    species_codes = list(fpr::fpr_my_bcfishpass(site = site, col_pull = observedspp_upstr)),

    # Replace empty values with NA
    dplyr::across(everything(), ~replace_empty_with_na(.))
  ) |>
  dplyr::ungroup() |>
  # if we have ef sites then we also have normal hab con sites, so then filterout the ef sites. If no ef sites then comments out this code
  # dplyr::filter(is.na(ef)) |>
  dplyr::mutate(priority = dplyr::case_when(priority == "mod" ~ "moderate",
                                            priority == "no_fix" ~ "no fix",
                                            TRUE ~ priority)) |>
  dplyr::mutate(priority = stringr::str_to_title(priority)) |>
  dplyr::mutate(hab_value = stringr::str_to_title(hab_value)) |>
  dplyr::arrange(local_name, crew_members, date_time_start) |>
  sf::st_drop_geometry()


# burn to csv
hab_priority_prep|>
  readr::write_csv("data/habitat_confirmations_priorities.csv", na = '')




# extract rd cost multiplier ----------------------------------------------

# extract the road surface and type from bcfishpass
rd_class_surface <- bcfishpass |>
  dplyr::select(stream_crossing_id, transport_line_structured_name_1:dam_operating_status) |>
  dplyr::filter(
    stringr::str_detect(
      stream_crossing_id,
      paste0(
        pscis_all |>
          dplyr::filter(!is.na(pscis_crossing_id)) |>
          dplyr::pull(pscis_crossing_id),
        collapse = "|"
      )
    )
  )|>
  dplyr::mutate(my_road_class = ften_file_type_description)|>
  dplyr::mutate(my_road_class = case_when(is.na(my_road_class) & !is.na(transport_line_type_description) ~
                                            transport_line_type_description,
                                          T ~ my_road_class))|>

  dplyr::mutate(my_road_class = case_when(is.na(my_road_class) & !is.na(rail_owner_name) ~
                                            'rail',
                                          T ~ my_road_class))|>
  dplyr::mutate(my_road_surface = case_when(is.na(transport_line_surface_description) & !is.na(ften_file_type_description) ~
                                              'loose',
                                            T ~ transport_line_surface_description))|>
  dplyr::mutate(my_road_surface = case_when(is.na(my_road_surface) & !is.na(rail_owner_name) ~
                                              'rail',
                                            T ~ my_road_surface))|>
  dplyr::mutate(my_road_class = stringr::str_replace_all(my_road_class, 'Forest Service Road', 'fsr'),
         my_road_class = stringr::str_replace_all(my_road_class, 'Road ', ''),
         my_road_class = stringr::str_replace_all(my_road_class, 'Special Use Permit, ', 'Permit-Special-'),
         my_road_class = dplyr::case_when(
           stringr::str_detect(my_road_class, '%driveway%') ~ 'driveway',
           T ~ my_road_class),
         my_road_class = stringr::word(my_road_class, 1),
         my_road_class = stringr::str_to_lower(my_road_class))

## Unique to fraser 2024
# - bcfishpass says Stella road is loose but it is paved. Need to change by hand so that the cost estimate works.
# - bcfishpass says PSCIS crossing 199232 is on a private drive but its on Highway 16, so change my_road_surface = paved and my_road_class = highway

rd_class_surface <- rd_class_surface |>
  dplyr::mutate(my_road_surface = dplyr::case_when(stream_crossing_id == "199172" ~ "paved",
                                                   stream_crossing_id == "7622" ~ "paved",
                                                   stream_crossing_id == "199232" ~ "paved",
                                                   TRUE ~ my_road_surface),
                my_road_class = dplyr::case_when(stream_crossing_id == "199232" ~ "highway",
                                                 TRUE ~ my_road_class))



conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_drop_table("rd_class_surface", conn = conn)
readwritesqlite::rws_write(rd_class_surface, exists = F, delete = T,
          conn = conn, x_name = "rd_class_surface")
readwritesqlite::rws_disconnect(conn)













# OLD CODE ----------------------
#### THE FOLLOWING CODE IS OLD AND IM NOT SURE IF WE STILL NEED IT SO LEAVING FOR NOW


# # xref_hab_site_corrected----------------------
# habitat_confirmations <- fpr_import_hab_con()
#
# hab_loc <- habitat_confirmations|>
#   purrr::pluck("step_1_ref_and_loc_info")|>
#   dplyr::filter(!is.na(site_number))%>%
#   mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))|>
#   tidyr::separate(alias_local_name, into = c('site', 'location', 'fish'), remove = F)|>
#   select(site:fish)|>
#   mutate(site = as.numeric(site))
#
# xref_hab_site_corrected <- left_join(
#   hab_loc,
#   xref_pscis_my_crossing_modelled,
#   by = c('site' = 'external_crossing_reference')
# )|>
#   mutate(stream_crossing_id = as.numeric(stream_crossing_id),
#          stream_crossing_id = case_when(
#            is.na(stream_crossing_id) ~ site,
#            T ~ stream_crossing_id
#          ))|>
#   mutate(site_corrected = paste(stream_crossing_id, location, fish, sep = '_'))|>
#   mutate(site_corrected = stringr::str_replace_all(site_corrected, '_NA', ''))|>
#   tibble::rownames_to_column()|>
#   readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/xref_hab_site_corrected.csv'), na = '')
#
#
# # rws_list_tables(conn)
# # rws_drop_table("xref_hab_site_corrected", conn = conn) ##now drop the table so you can replace it
# # rws_write(hab_site_corrected, exists = F, delete = TRUE,
# #           conn = conn, x_name = "xref_hab_site_corrected")
#
# # xref_phase2_corrected------------------------------------
# # once we have our data loaded this gives us a xref dataframe to pull in pscis ids and join to our  spreadsheet imports
# pscis_all <- bind_rows(pscis_list)
#
# xref_phase2_corrected <- left_join(
#   pscis_all,
#   xref_pscis_my_crossing_modelled,
#   by = c('my_crossing_reference' = 'external_crossing_reference')
# ) |>
#   mutate(pscis_crossing_id = case_when(
#     is.na(pscis_crossing_id) ~ stream_crossing_id,
#     T ~ as.integer(pscis_crossing_id)
#   ))|>
#   dplyr::filter(str_detect(source, 'phase2'))  |>
#   readr::write_csv(file = '/data/inputs_extracted/xref_phase2_corrected.csv', na = '')
#
# # rws_list_tables(conn)
# # rws_drop_table("xref_phase2_corrected", conn = conn) ##now drop the table so you can replace it
# # rws_write(xref_pscis_my_crossing_phase2, exists = F, delete = TRUE,
# #           conn = conn, x_name = "xref_phase2_corrected")
# # rws_list_tables(conn)
# # rws_disconnect(conn)
#
#
#
#
# # Fish species and hab gain estimates for phase 2 sites ------------------------
#
# habitat_con_pri <- read_csv('data/habitat_confirmations_priorities.csv')
#
# hab_priority_fish_hg <- left_join(
#   habitat_con_pri |> select(reference_number, alias_local_name, site, location, ef),
#   bcfishpass |> select(stream_crossing_id, observedspp_upstr, st_rearing_km),
#   by = c('site' = 'stream_crossing_id')
# ) |>
#   mutate(observedspp_upstr = gsub("[{}]", "", observedspp_upstr)) |>
#   mutate(observedspp_upstr = case_when(
#     alias_local_name %like% '_ds' |
#       # ends in a number
#       alias_local_name %like% '\\d$' ~ NA_character_,
#     T ~ observedspp_upstr),
#     st_rearing_km = case_when(
#       alias_local_name %like% 'ds' |
#         # ends in a number
#         alias_local_name %like% '\\d$' ~ NA_real_,
#       T ~ st_rearing_km)) |>
#   rename(species_codes = observedspp_upstr) |>
#   mutate(
#     upstream_habitat_length_m = st_rearing_km * 1000,
#     species_codes = stringr::str_replace_all(species_codes, c('CCT,|SST,|SP,'), ''),
#     species_codes = case_when(
#       site == 198090 ~ NA_character_,
#       T ~ species_codes
#     )
#   ) |>
#   readr::write_csv('data/inputs_extracted/hab_priority_fish_hg.csv', na = '')
#
#
#

# fish summary ------------------------------------------------------------

## Leaving this code here for now but lots of this code has been updated and moved to `fish_data_tidy.R` and now does not
## rely (as much) on the spreadsheet. Double check it is no longer needed and then delete.


# # we need to summarize all our fish sizes
#
# ## fish collection data ----------------------------------------------------
# habitat_confirmations <- fpr::fpr_import_hab_con(row_empty_remove = T)
#
#
# hab_fish_indiv_prep <- habitat_confirmations |>
#   purrr::pluck("step_3_individual_fish_data") |>
#   dplyr::filter(!is.na(site_number)) |>
#   select(-gazetted_names:-site_number)
#
# hab_loc <- habitat_confirmations |>
#   purrr::pluck("step_1_ref_and_loc_info") |>
#   dplyr::filter(!is.na(site_number))|>
#   mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))
#
#
# ##add the species code
# hab_fish_codes <- fishbc::freshwaterfish |>
#   select(species_code = Code, common_name = CommonName) |>
#   tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') |>
#   mutate(common_name = case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)', T ~ common_name))
#
# hab_fish_indiv_prep2 <- left_join(
#   hab_fish_indiv_prep,
#   hab_loc,
#   by = 'reference_number'
# ) |> mutate(
#   species = case_when(species == 'Fish Unidentified Species' ~ 'Unidentified Species',
#            T ~ species))
#
#
# hab_fish_indiv_prep3 <- left_join(
#   hab_fish_indiv_prep2,
#   select(hab_fish_codes, common_name:species_code),
#   by = c('species' = 'common_name')
# ) |>
#   dplyr::select(reference_number,
#                 alias_local_name,
#                 site_number,
#                 sampling_method,
#                 method_number,
#                 haul_number_pass_number,
#                 species_code,
#                 length_mm,
#                 weight_g) ##added method #
#
#
# ##we need the size of the sites too
#
# ####workflow is a bit weird because we need to input NFC sites and the size of the sites
# ##or else we don't know about them in the summary.
# hab_fish_collect_prep <- habitat_confirmations |>
#   purrr::pluck("step_2_fish_coll_data") |>
#   dplyr::filter(!is.na(site_number)) |>
#   # select(-gazetted_name:-site_number) |>
#   dplyr::distinct(reference_number, method_number, haul_number_pass_number, .keep_all = T) |>
#   # distinct(reference_number, .keep_all = T) |>
#   arrange(reference_number) |>
#   mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) |>
#   mutate(across(c(time_in,time_out), chron::times))
# # hab_fish_collect_prep_mt <- habitat_confirmations |>
# #   purrr::pluck("step_2_fish_coll_data") |>
# #   dplyr::filter(!is.na(site_number)) |>
# #   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
# #   mutate(site_id = paste0(site, location)) |>
# #   distinct(local_name, sampling_method, method_number, .keep_all = T) |> ##changed this to make it work as a feed for the extract-fish.R file
# #   mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) |>
# #   mutate(across(c(time_in,time_out), chron::times))
#
# ##we use this to test things out
# # hab_fish_indiv <- left_join(
# #   select(hab_fish_collect_prep_mt |> filter(reference_number == 36),
# #          reference_number,
# #          local_name,
# #          site_number:model, date_in:time_out ##added date_in:time_out
# #   ),
# #   select(hab_fish_indiv_prep3 |> filter(reference_number == 36),
# #          reference_number,
# #          sampling_method,
# #          method_number, ##added method #
# #          # alias_local_name,
# #          species_code, length_mm),
# #   by = c('reference_number', 'sampling_method', 'method_number') #added method # and haul
# # )
#
# # test to see if there are any missing lengths
# hab_fish_indiv_prep3 |>
#   filter(is.na(length_mm))
#
# # join the indiv fish data to existing site info
# hab_fish_indiv <- full_join(
#   select(hab_fish_indiv_prep3,
#          reference_number,
#          sampling_method,
#          method_number,
#          haul_number_pass_number,
#          species_code,
#          length_mm,
#          weight_g),
#   select(hab_fish_collect_prep,
#          reference_number,
#          local_name,
#          temperature_c:model,
#          date_in:time_out, ##added date_in:time_out because we did minnow traps
#          comments
#   ),
#   by = c(
#     "reference_number",
#     # 'alias_local_name' = 'local_name',
#     "sampling_method",
#     "method_number",
#     "haul_number_pass_number")
# ) |>
#   mutate(species_code = as.character(species_code)) |>
#   mutate(species_code = case_when(
#     is.na(species_code) ~ 'NFC',
#     T ~ species_code)
#   ) |>
#   mutate(species_code = as.factor(species_code)) |>
#   mutate(life_stage = case_when(  ##this section comes from the histogram below - we include here so we don't need to remake the df
#     length_mm <= 65 ~ 'fry',
#     length_mm > 65 & length_mm <= 110 ~ 'parr',
#     length_mm > 110 & length_mm <= 140 ~ 'juvenile',
#     length_mm > 140 ~ 'adult',
#     T ~ NA_character_
#     )
#   # life_stage = case_when(
#   #   species_code %in% c('L', 'SU', 'LSU') ~ NA_character_,
#   #   T ~ life_stage),
#   # comments = case_when(
#   #   species_code %in% c('L', 'SU', 'LSU') & !is.na(comments) ~
#   #     paste0(comments, 'Not salmonids so no life stage specified.'),
#   #   species_code %in% c('L', 'SU', 'LSU') & is.na(comments) ~
#   #     'Not salmonids so no life stage specified.',
#   #   T ~ comments)
#   )|>
#   mutate(life_stage = fct_relevel(life_stage,
#                                   'fry',
#                                   'parr',
#                                   'juvenile',
#                                   'adult')) |>
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
#   mutate(site_id = paste0(site, '_', location))



# ###------from duncan_fish_plots_20200210
#
# ####----------fish length-----------
# # filter(species_code == "CO")
# # fish_eb <-  hab_fish_indiv |> filter(species_code != "EB")
#
# bin_1 <- floor(min(hab_fish_indiv$length_mm, na.rm = TRUE)/5)*5
# bin_n <- ceiling(max(hab_fish_indiv$length_mm, na.rm = TRUE)/5)*5
# bins <- seq(bin_1,bin_n, by = 5)
#
# plot_fish_hist <- ggplot(hab_fish_indiv |> filter(!species_code %in% c('LSU','SU','NFC')), #!species_code %in% c('LSU','SU','NFC')
#                          aes(x=length_mm
#                              # fill=alias_local_name
#                              # color = alias_local_name
#                          )) +
#   geom_histogram(breaks = bins, alpha=0.75,
#                  position="identity", size = 0.75)+
#   labs(x = "Fork Length (mm)", y = "Count (#)") +
#   facet_wrap(~species_code)+
#   # scale_color_grey() +
#   # scale_fill_grey() +
#   ggdark::dark_theme_bw(base_size = 8)+
#   # theme_bw(base_size = 8)+
#   scale_x_continuous(breaks = bins[seq(1, length(bins), by = 2)])+
#   # scale_color_manual(values=c("grey90", "grey60", "grey30", "grey0"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # geom_histogram(aes(y=..density..), breaks = bins, alpha=1,
# #                position="identity", size = 0.75)
# plot_fish_hist
#
# # ggsave(plot = plot_fish_hist, file="./fig/fish_histogram.png",
# #        h=3.4, w=5.11, units="in", dpi=300)
#
#
# # this will be joined to the abundance estimates and the confidence intervals
# tab_fish_summary <- hab_fish_indiv |>
#   group_by(site_id,
#            ef,
#            sampling_method,
#            # haul_number_pass_number,
#            species_code) |> ##added sampling method!
#   summarise(count_fish = n()) |>
#   arrange(site_id, species_code, ef)



######----------------density plots--------------------------
# # needs to be modified to have remve the haul number and just use the pop estimate
#
# hab_fish_dens <- hab_fish_indiv |>
#   filter(sampling_method == 'electrofishing') |> ##added this since we now have mt data as well!!
#   mutate(area = round(ef_length_m * ef_width_m),0) |>
#   group_by(local_name, method_number, haul_number_pass_number, ef_length_m, ef_width_m, ef_seconds, area, species_code, life_stage) |>
#   summarise(fish_total = length(life_stage)) |>
#   ungroup() |>
#   mutate(density_100m2 = round(fish_total/area * 100, 1)) |>
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
#   mutate(site_id = paste0(site, location),
#          location = case_when(location == 'us' ~ 'Upstream',
#                               T ~ 'Downstream'),
#          life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult')))
#
# # hab_fish_dens |>
# #   readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/hab_fish_dens.csv'))
#
# ##paths to write to will need to change now
# # ggsave(plot = plot_fish_box, filename = "./fig/plot_fish_box.png",
# #        h=9.66, w=14.5, units="cm", dpi=300)
#
#
# ##clean up the objects
# rm(hab_site_prep,
#    # hab_fish_indiv_prep,
#    # hab_fish_indiv_prep2,
#    hab_fish_collect_prep2,
#    hab_loc2)
#
# # gps get coordinates for waypoints -----------------------------------------------------
#
# gpx <- 'C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/GPS/kylegps_sept22backup_bulkley2021.GPX'
#
#
# wp_kyle <- sf::st_read(gpx,
#                        layer = 'waypoints',
#   quiet = T) |>
#   janitor::clean_names() |>
#   # this is a work around so that we get the original name of the renamed wp if there were duplicate names in basecamp
#   mutate(name = as.numeric(name),
#          name = case_when(name > 1000 ~ round(name/10, 0),
#                           T ~ name)) |>
#   dplyr::select(name_old = name, everything())  |>
#   mutate(source = 'KP',
#          name = paste0(name_old, '_', source, '_', lubridate::year(time))) |>
#   sf::st_transform(crs = 26909) |>
#   poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') |>
#   select(name, name_old, source, ele, time, easting, northing)
#
# gpx <- "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/GPS/bulkley_2021_field_al.gpx"
#
# wp_al <- sf::st_read(gpx,
#                      layer = 'waypoints',
#                      quiet = T) |>
#   janitor::clean_names() |>
#   # this is a work around so that we get the original name of the renamed wp if there were duplicate names in basecamp
#   mutate(name = as.numeric(name),
#          name = case_when(name > 1000 ~ round(name/10, 0),
#                           T ~ name)) |>
#   dplyr::select(name_old = name, everything())  |>
#   mutate(source = 'AI',
#          name = paste0(name_old, '_', source, '_', lubridate::year(time))) |>
#   sf::st_transform(crs = 26909) |>
#   poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') |>
#   select(name, name_old, source, ele, time, easting, northing)
#
# wp <- bind_rows(
#   wp_kyle,
#   wp_al
# )
#
# rm(wp_kyle, wp_al)
#
# # join with the priorities spreadsheet to get those coords
# hab_con <- readr::read_csv(file = "./data/habitat_confirmations_priorities.csv")
#
#
# wp_joined <- left_join(
#   hab_con|> separate(crew_members, into = c('source', 'others')),
#   wp|> select(name_old, source, easting, northing),
#   by = c('waypoint' = 'name_old', 'source')
# )
#
# # bring in the locations and insert utms where we don't have them already
# hab_loc_utm <- left_join(
#   fpr_import_hab_con(backup = F, col_filter_na = T)|>
#   purrr::pluck("step_1_ref_and_loc_info"),
#
#   wp_joined|> select(alias_local_name, easting, northing),
#
#   by = 'alias_local_name'
# )|>
#   mutate(
#     utm_easting = case_when(
#     is.na(utm_easting) ~ easting,
#     T ~ utm_easting),
#     utm_northing = case_when(
#       is.na(utm_northing) ~ northing,
#       T ~ utm_northing
#       )
#     )
#
#
