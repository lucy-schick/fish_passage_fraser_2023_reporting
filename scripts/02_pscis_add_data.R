# Field data can be updated by the field team after tidying of the data and updates in QGIS have occurred
# this script is used to add new data from the field to the annual cleaned (or partially cleaned) data set
# in the QGIS project `data_field/2023`.  THE NEW DATA SHOULD BE CLEANED WITH 01_pscis_tidy.R BEFORE THIS SCRIPT IS RUN

# in this case, Andy and Alicia from GWA added new data in the skeena

# NOT USED IN THIS PROJECT

# import form_pscis.gpkg direct from mergin and then import form_pscis_2023 and bind the forms together
# form_pscis_field <- sf::st_read("~/Projects/gis/sern_skeena_2023/form_pscis.gpkg") %>%
#   dplyr::filter(mergin_user == "andyr")
#
# form_pscis_working <- sf::st_read("~/Projects/gis/sern_skeena_2023/data_field/2023/form_pscis_2023.gpkg")
#
# # # check to see that column names are equiv (must be if number is the same but still)
# identical(names(form_pscis_field), names(form_pscis_working))
#
# # Combine the two data forms
# form_prep <- bind_rows(
#   form_pscis_working,
#   form_pscis_field
# ) %>%
#   # update the coordinates
#   fpr_sp_assign_utm()
#
# # burn to backup folder as csv
# form_prep %>%
#   readr::write_csv('data/backup/form_pscis_og.csv')
#
# # re burn to working geopackage in Q project
# form_prep %>%
#   sf::st_write("~/Projects/gis/sern_skeena_2023/form_pscis.gpkg",
#                append=F,
#                delete_dsn=T)
