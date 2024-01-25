source('scripts/packages.R')


# connect to the database
conn <- fpr_db_conn()

# grab all the crossings from our study area
# I will use the same nameing convention as our repository
bcfishpass <- fpr_db_query(
  query = "SELECT * from bcfishpass.crossings_vw WHERE watershed_group_code IN ('LCHL', 'NECR', 'FRAN');"
)


# Getting all the pscis assessments from our study area ('LCHL', 'NECR', 'FRAN'). Must spatially join with
# "whse_basemapping.fwa_watershed_groups_poly" to filter based on "watershed_group_code" column.
query <- "SELECT p.*, wsg.watershed_group_code
   FROM whse_fish.pscis_assessment_svw p
   INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
   ON ST_Intersects(wsg.geom,p.geom)
   WHERE wsg.watershed_group_code IN ('LCHL', 'NECR', 'FRAN');"

pscis <- st_read(conn, query =  query)

# always disconnect from the database
dbDisconnect(conn = conn)


# Making a sqlite database named bcfishpass.sqlite
mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/bcfishpass.sqlite")

# Connecing to the bcfishpass.sqlite database
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")

# Burning the bcfishpass table to the bcfishpass.sqlite database
dbWriteTable(conn, "bcfishpass", bcfishpass)

# Listing all the tables in the bcfishpass.sqlite database, should be empty at the moment
readwritesqlite::rws_list_tables(conn)

# Writing the bcfishpass table to the bcfishpass.sqlite database
planning_raw <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)


