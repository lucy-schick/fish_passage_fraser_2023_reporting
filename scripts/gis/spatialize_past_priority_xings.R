# Search through all the pscis crossings in our new watersheds, grab the unique ecocat_urls, look throught associated reports, grab any priorioty crossing tables, burn those tables to spatial objects so we can flag those crossings on the map, as part of the planning process documented here https://github.com/NewGraphEnvironment/fish_passage_fraser_2023_reporting/issues/156

# name the watershed groups in our study area
wsg <- c('TABR','WILL', 'LSAL')



pscis_assessment_svw <- fpr::fpr_db_query(
  glue::glue(
    "SELECT p.*, wsg.watershed_group_code
   FROM whse_fish.pscis_assessment_svw p
   INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
   ON ST_Intersects(wsg.geom,p.geom)
  WHERE wsg.watershed_group_code IN (
  {glue::glue_collapse(glue::single_quote(wsg), sep = ', ')}
  );"
  )
)

#Check out the unique urls. Not many of them. Only 2 link to report, and only the Hooft 2015 report has sites prioritized.
unique_urls <- pscis_assessment_svw |>
  dplyr::distinct(ecocat_url)



# read in the priority sites, clean up, then join the pscis info
hooft_sites <- readr::read_csv("data/inputs_extracted/hooft_2015_priority_crossings.csv") |>
  janitor::clean_names() |>
  dplyr::filter(watershed != "Bowron") |>
  dplyr::left_join(pscis_assessment_svw,
                   by = c('my_crossing_reference' = 'external_crossing_reference'))


# Burn to geopackage so we can see these sites on the map
hooft_sites |> sf::st_write("data/gis/hooft_2015_priority_crossings.gpkg")


