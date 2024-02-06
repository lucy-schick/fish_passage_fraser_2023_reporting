# read in the data, add the columns we want and burn to mergin for processing


# 1. change this to the correct project name in our GIS folder and delete this comment
dir_project <- 'sern_fraser_2023'


# read in the crossings data for the study area.
# 2. use the newly updated extract-pg.R template script to see how to read in the crossings data from the database.
# use fpr to list the names of the colunns in the bcfishpass.crossings_vw table.  Search up (or use copilot/chattr) or
# look in past report scripts see how to list the unique values for a column in a table in SQL and write that query
# to the PG database.  Use the fpr_db_query function to do this. Print it to the console but also assign it to a variable
# and look at it that way too....


# 3.  Read in the pscis assessments layer from the database.  Use our naming convention of calling the object pscis
# modify this query (and use the fpr_db_query function to do this using `ST_Intersects`
# https://github.com/NewGraphEnvironment/fish_passage_peace_2022_reporting/blob/7ec363e88d8034ffeaa577092ec0731438ffaee0/scripts/02_reporting/0160-load-bcfishpass-data.R#L133C1-L140C39



# 3. make a sqlite database named bcfishpass.sqlite and burn in the table naming it bcfishpass
# https://github.com/NewGraphEnvironment/fish_passage_peace_2022_reporting/blob/7ec363e88d8034ffeaa577092ec0731438ffaee0/scripts/02_reporting/0160-load-bcfishpass-data.R#L180

# Making a sqlite database named bcfishpass.sqlite only need to do this once?
# mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/bcfishpass.sqlite")

# Connecing to the bcfishpass.sqlite database
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")

# Burning the bcfishpass table to the bcfishpass.sqlite database, only need to do this once
# readwritesqlite::rws_write(bcfishpass, exists = FALSE, conn = conn)

# Burning the pscis table to the bcfishpass.sqlite database, only need to do this once
# readwritesqlite::rws_write(pscis, exists = FALSE, conn = conn)



# 4 This stuff below is the old code from planning from the peace last year.  I will put ### hashmarks on new comments
# about how to customize it now that we are in the future in a different region.
### fyi - we don't really need to put things in a sqlite for this but it is a good example of how we use a local
### portable database to get a snapshot of the data at a point in time.

# Listing all the tables in the bcfishpass.sqlite database, should be empty at the moment
readwritesqlite::rws_list_tables(conn)

# Reading the bcfishpass table from the bcfishpass.sqlite database
planning_raw <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)

# Reading the pscis table from the bcfishpass.sqlite database
pscis_raw <- readwritesqlite::rws_read_table("pscis", conn = conn) %>%
  sf::st_drop_geometry()


### this ise that the data is in the right projectionevant below but may not be necesary anymore. you will see why
unique(planning_raw$utm_zone)
# unsure why this is neccesary but will re-visit later

### If you can - and its helpful perhaps break out litle bits of this big MULTIPLE join
### join and run them a move at a time to see what is going on
planning <- left_join(

  ### have a look at the new function fpr_sp_assign_sf_from_utm to see another way to do this because if the data
  ### is in more than one utm zone the way this is written will not work. Give it a try
  planning_raw %>%
    fpr_sp_assign_sf_from_utm(col_utm_zone = "utm_zone", col_northing = "utm_northing",
                              col_easting = "utm_easting"),
    # st_as_sf(coords = c('utm_easting', 'utm_northing'), crs = 26910, remove = F) %>%
    # st_transform(crs = 3005),

  ### joining pcsis_raw to planning_raw when the aggregated_crossings_id is the same as the stream_crossing_id
  planning_raw2 <- left_join(

    #arranging the planning_raw table by aggregated_crossings_id
    planning_raw %>%
      arrange(aggregated_crossings_id),

    # selecting certain columns from the pscis table
    pscis_raw %>%
      mutate(stream_crossing_id = as.character(stream_crossing_id)) %>%
      dplyr::select(
        stream_crossing_id,
        stream_name,
        road_name,
        outlet_drop,
        downstream_channel_width,
        habitat_value_code,
        image_view_url),

    by = c('aggregated_crossings_id' = 'stream_crossing_id')) %>%

    # filtering where pscis_status is NA or not equal to 'HABITAT CONFIRMATION' and barrier_status is not equal to
    # 'PASSABLE' or 'UNKNOWN'
    filter(is.na(pscis_status) | (pscis_status != 'HABITAT CONFIRMATION' &
                                    barrier_status != 'PASSABLE' &
                                    barrier_status != 'UNKNOWN')) %>%
    ### since we are deep in salmon country here lets use a coho metric.  Lets change this to look at everything with
    ### over 1km of rearing habitat to start. Don't forget about fpr_dbq_lscols .  Also - if not familiar have a look at
    ###  our tables in methods of past reports (Skeena has salmon) which explain the thresholds in general. Look at the
    ### csv in bcfishpass that decided what they are too though because they are new!
    filter(co_rearing_km > 1) %>%
    filter(crossing_type_code != 'OBS') %>%
    filter(is.na(barriers_anthropogenic_dnstr)) %>%

    # remove the geometry column so not class sf
    st_drop_geometry(planning_raw2) %>%

    ### make a note that this is the column that you will use to in the mergin project to query in the "Query Builder"
    ### so that you filter to only see the ones that you tagged as "my_review" = TRUE. Do a bit of homework to see how
    ### to use the `Query Builder`.  Note also that you can add a query that will make it so that you only
    ### see the ones that you have not yet reviewed. I will leave it to you to try to do that. Can help of course if need be
    mutate(my_review = TRUE) %>%
    dplyr::select(aggregated_crossings_id,
                  my_review,
                  stream_name,
                  road_name,
                  outlet_drop,
                  downstream_channel_width,
                  habitat_value_code,
                  image_view_url),

  by = 'aggregated_crossings_id'

) %>%
  mutate(
    my_priority = NA_character_,
    my_priority_comments = NA_character_,
    my_citation_key1 = NA_character_,
    my_citation_key2 = NA_character_,
    my_citation_key3 = NA_character_
  )

### this is going to write it into the mergin project.
### open it in QGIS and view the file.  Have a look at the Peace project to see where we put it.
### check out this function with the help (?st_write) and see if you can figure out how to write it to a geopackage
### called "planning.gpkg" with the layer name "planning_20210527" (or whatever the date is today)
planning %>%
  sf::st_write(paste0('../../gis/',
                      dir_project,
                      '/',



                      paste0('planning_', format(lubridate::now(), "%Y%m%d")),



                      '.gpkg'),
               # turned this T now that we have time in name
               delete_layer = T)



dbDisconnect(conn)


