source('scripts/packages.R')

# Grab data from bcfishpass ---------------------
wsg <- c('LCHL', 'NECR', 'FRAN', "MORK", "UFRA")
species_of_interest <- c('BT', 'CH', 'CM', 'CO', 'CT', 'DV', 'PK', 'RB','SK', 'ST')

fiss_sum <- fpr::fpr_db_query(
  glue::glue(
    "SELECT
      fish_observation_point_id,
      s.gradient,
      s.stream_order,
      s.upstream_area_ha,
      s.channel_width,
      e.species_code,
      e.watershed_group_code,
      round((ST_Z((ST_Dump(ST_LocateAlong(s.geom, e.downstream_route_measure))).geom))::numeric) AS elevation
    FROM bcfishobs.fiss_fish_obsrvtn_events_vw e
    INNER JOIN bcfishpass.streams_vw s
    ON e.linear_feature_id = s.linear_feature_id
    WHERE e.watershed_group_code IN (
      {glue::glue_collapse(glue::single_quote(wsg), sep = ', ')}
    )
    AND e.species_code IN (
      {glue::glue_collapse(glue::single_quote(species_of_interest), sep = ', ')}
    );"
  )
) |>
  sf::st_drop_geometry()



##burn it all to a file we can use later
fiss_sum |>
  readr::write_csv(file = paste0('data/inputs_extracted/fiss_sum.csv'))


##lets put it in the sqlite for safekeeping
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_drop_table("fiss_sum", conn = conn) ##if it exists get rid of it - might be able to just change exists to T in next line
readwritesqlite::rws_write(fiss_sum, exists = F, delete = TRUE,
          conn = conn, x_name = "fiss_sum")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_disconnect(conn)



# Calculate the fish observations vs. stream gradient ---------------------
fiss_sum_grad_prep1 <- fiss_sum |>
  dplyr::mutate(Gradient = dplyr::case_when(
    gradient < 0.03 ~ '0 - 3 %',
    gradient >= 0.03 & gradient < 0.05 ~ '03 - 5 %',
    gradient >= 0.05 & gradient < 0.08 ~ '05 - 8 %',
    gradient >= 0.08 & gradient < 0.15 ~ '08 - 15 %',
    gradient >= 0.15 & gradient < 0.22 ~ '15 - 22 %',
    gradient >= 0.22 ~ '22+ %'
  )) |>
  dplyr::mutate(gradient_id = dplyr::case_when(
    gradient < 0.03 ~ 3,
    gradient >= 0.03 & gradient < 0.05 ~ 5,
    gradient >= 0.05 & gradient < 0.08 ~ 8,
    gradient >= 0.08 & gradient < 0.15 ~ 15,
    gradient >= 0.15 & gradient < 0.22 ~ 22,
    gradient >= 0.22 ~ 99
  ))

fiss_sum_grad_prep2 <- fiss_sum_grad_prep1 |>
  dplyr::group_by(species_code) |>
  dplyr::summarise(total_spp = dplyr::n(), .groups = "drop")

fiss_sum_grad_prep3 <- fiss_sum_grad_prep1 |>
  dplyr::group_by(species_code, Gradient, gradient_id) |>
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")

fiss_sum_grad <- dplyr::left_join(
  fiss_sum_grad_prep3,
  fiss_sum_grad_prep2,
  by = "species_code"
) |>
  dplyr::mutate(Percent = round(Count / total_spp * 100, 0))

##save this for the report
##burn it all to a file we can use later
fiss_sum_grad |> readr::write_csv(file = 'data/inputs_extracted/fiss_sum_grad.csv')

# test the plot
plot_grad <- fiss_sum_grad |>
  dplyr::filter(gradient_id != 99) |>
  ggplot2::ggplot(ggplot2::aes(x = Gradient, y = Percent)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(~species_code, ncol = 2) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::labs(x = "Average Stream Gradient", y = "Occurrences (%)")

plot_grad


# Calculate the fish observations vs. channel width ---------------------
fiss_sum_width_prep1 <- fiss_sum |>
  dplyr::mutate(Width = dplyr::case_when(
    channel_width < 2 ~ "0 - 2m",
    channel_width >= 2 & channel_width < 4 ~ "02 - 04m",
    channel_width >= 4 & channel_width < 6 ~ "04 - 06m",
    channel_width >= 6 & channel_width < 10 ~ "06 - 10m",
    channel_width >= 10 & channel_width < 15 ~ "10 - 15m",
    channel_width >= 15 ~ "15m+"
  )) |>
  dplyr::mutate(width_id = dplyr::case_when(
    channel_width < 2 ~ 2,
    channel_width >= 2 & channel_width < 4 ~ 4,
    channel_width >= 4 & channel_width < 6 ~ 6,
    channel_width >= 6 & channel_width < 10 ~ 10,
    channel_width >= 10 & channel_width < 15 ~ 15,
    channel_width >= 15 ~ 99
  ))

fiss_sum_width_prep2 <- fiss_sum_width_prep1 |>
  dplyr::group_by(species_code) |>
  dplyr::summarise(total_spp = dplyr::n(), .groups = "drop")

fiss_sum_width_prep3 <- fiss_sum_width_prep1 |>
  dplyr::group_by(species_code, Width, width_id) |>
  dplyr::summarise(Count = dplyr::n(), .groups = "drop")

fiss_sum_width <- dplyr::left_join(
  fiss_sum_width_prep3,
  fiss_sum_width_prep2,
  by = "species_code"
) |>
  dplyr::mutate(Percent = round(Count / total_spp * 100, 0))

## save this for the report
fiss_sum_width |>
  readr::write_csv(file = "data/inputs_extracted/fiss_sum_width.csv")

# Plot: fish vs. channel width
plot_width <- fiss_sum_width |>
  dplyr::filter(!is.na(width_id)) |>
  ggplot2::ggplot(ggplot2::aes(x = Width, y = Percent)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(~species_code, ncol = 2) +
  ggdark::dark_theme_bw(base_size = 11) +
  ggplot2::labs(x = "Channel Width", y = "Occurrences (%)")

plot_width


# Calculate the fish observations vs. watershed size ---------------------

# Calculate the fish observations vs. watershed size ---------------------

fiss_sum_wshed_prep1 <- fiss_sum |>
  dplyr::mutate(Watershed = dplyr::case_when(
    upstream_area_ha < 2500 ~ "0 - 25km2",
    upstream_area_ha >= 2500 & upstream_area_ha < 5000 ~ "25 - 50km2",
    upstream_area_ha >= 5000 & upstream_area_ha < 7500 ~ "50 - 75km2",
    upstream_area_ha >= 7500 & upstream_area_ha < 10000 ~ "75 - 100km2",
    upstream_area_ha >= 10000 ~ "100km2+"
  )) |>
  dplyr::mutate(watershed_id = dplyr::case_when(
    upstream_area_ha < 2500 ~ 2500,
    upstream_area_ha >= 2500 & upstream_area_ha < 5000 ~ 5000,
    upstream_area_ha >= 5000 & upstream_area_ha < 7500 ~ 7500,
    upstream_area_ha >= 7500 & upstream_area_ha < 10000 ~ 10000,
    upstream_area_ha >= 10000 ~ 99999
  ))

fiss_sum_wshed_prep2 <- fiss_sum_wshed_prep1 |>
  dplyr::group_by(species_code, Watershed) |>
  dplyr::summarise(count_wshd = dplyr::n(), .groups = "drop")

fiss_sum_wshed_prep3 <- fiss_sum_wshed_prep1 |>
  dplyr::group_by(species_code) |>
  dplyr::summarise(total_spp = dplyr::n(), .groups = "drop")

fiss_sum_wshed <- dplyr::left_join(
  fiss_sum_wshed_prep2,
  fiss_sum_wshed_prep3,
  by = "species_code"
) |>
  dplyr::mutate(
    Percent = round(count_wshd / total_spp * 100, 0),
    Watershed = factor(Watershed, levels = c(
      "0 - 25km2", "25 - 50km2", "50 - 75km2", "75 - 100km2", "100km2+"
    ))
  ) |>
  dplyr::arrange(species_code, Watershed)

## save this for the report
fiss_sum_wshed |>
  readr::write_csv(file = "data/inputs_extracted/fiss_sum_wshed.csv")

# Plot: fish vs. watershed size
plot_wshed <- fiss_sum_wshed |>
  dplyr::filter(!is.na(Watershed)) |>
  ggplot2::ggplot(ggplot2::aes(x = Watershed, y = Percent)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(~species_code, ncol = 2) +
  ggdark::dark_theme_bw(base_size = 11) +
  ggplot2::labs(x = "Watershed Area", y = "Occurrences (%)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

plot_wshed



# fiss_sum_wshed_filter <- fiss_sum |>
#   dplyr::filter(upstream_area_ha < 10000)
#
# bin_1 <- 0
# bin_n <- ceiling(max(fiss_sum_wshed_filter$upstream_area_ha, na.rm = TRUE) / 5) * 5
# bins <- seq(bin_1, bin_n, by = 1000)
#
# # Plot: histogram of fish observations vs. upstream watershed area
# plot_wshed_hist <- ggplot2::ggplot(
#   fiss_sum_wshed_filter,
#   ggplot2::aes(x = upstream_area_ha)
# ) +
#   ggplot2::geom_histogram(breaks = bins, position = "identity", size = 0.75) +
#   ggplot2::geom_histogram(
#     ggplot2::aes(y = ..density..),
#     breaks = bins,
#     alpha = 0.5,
#     position = "identity",
#     size = 0.75
#   ) +
#   ggplot2::facet_wrap(~species_code, ncol = 2) +
#   ggplot2::scale_x_continuous(breaks = bins[seq(1, length(bins), by = 2)]) +
#   ggplot2::theme_bw(base_size = 11) +
#   ggplot2::labs(
#     x = "Upstream Watershed Area (ha)",
#     y = "Count Fish (#)"
#   )
#
# plot_wshed_hist
#
#
#
