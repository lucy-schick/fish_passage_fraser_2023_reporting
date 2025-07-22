# we need to get the coordinates for the eDNA

# read in the excel file
path <- "~/Projects/repo/fish_passage_fraser_2023_reporting/data/inputs_raw/Cumulative eDNA sapling sites-file 2020-2023_Jan28_2025.xlsx"
months <- paste0("(?i)\\b(", paste(month.abb, collapse = "|"), ")\\b")
edna_raw <- readxl::read_excel(path) |>
  janitor::clean_names() |>
  dplyr::select(collection_type:s_code) |>
  fpr::fpr_sp_assign_sf_from_utm(col_utm_zone = "zone") |>
  dplyr::rename(date_raw = date) |>
  dplyr::mutate(
    date_mdy = suppressWarnings(lubridate::mdy(date_raw)),
    date = dplyr::case_when(
      stringr::str_to_lower(date_raw) %in% c("no sample", "na", "") ~ NA_real_,
      !is.na(date_mdy) ~ as.numeric(date_mdy - as.Date("1899-12-30")),
      TRUE ~ as.numeric(date_raw)
    ),
    date = janitor::excel_numeric_to_date(date)
  ) |>
  dplyr::select(-date_mdy)


# just keep the sites with a sample date or which are distinct for combos of site_e_dna_data_file and new_site
edna <- dplyr::bind_rows(
  edna_raw |> dplyr::filter(!is.na(date)),
  edna_raw |>
    dplyr::filter(is.na(date)) |>
    dplyr::distinct(site_e_dna_data_file, new_site, .keep_all = TRUE)
) |>
  dplyr::arrange(site_e_dna_data_file)


# burn to repo and gis project
path_out_repo <- "~/Projects/repo/fish_passage_fraser_2023_reporting/data/gis/edna_unbc_2020-2023_20250128.geojson"
path_out_gis <- "~/Projects/gis/sern_fraser_2024/edna_unbc_2020-2023_20250128.geojson"

# burn out the clean csv


if(fs::file_exists(path_out_repo)){
  fs::file_delete(path_out_repo)
}

edna |>
  readr::write_csv(
    "~/Projects/repo/fish_passage_fraser_2023_reporting/data/inputs_extracted/edna_unbc_2020-2023_20250128.csv"
  )

edna |>
  sf::st_transform(crs = 4326) |>
  sf::st_write(
    path_out_repo
  )

if(fs::file_exists(path_out_gis)){
  fs::file_delete(path_out_gis)
}

edna |>
  sf::st_write(
    path_out_gis
  )
