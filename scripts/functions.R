##function to find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {
  fils <- list.files(path = where, pattern = in_files, recursive = recursive)
  found <- FALSE
  file_cmd <- Sys.which("file")
  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    contents <- readLines(fil)
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    if (length(res) > 0) {
      found <-  TRUE
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
    }
  }
  if (!found) message("(No results found)")
}

# Function to clip a point layer to a polygon layer based on a join column and factor
#' @param schema_table_point String (quoted) name of point layer schema.table
#' @param schema_table_polygon String (quoted) name of polygon layer schema.table
#' @param join_column String (quoted) name of column to join tables on from polygon. See column names of any table with \link{fpr_dbq_lscols}
#' @param join_on String (quoted) or vector of specific terms to join on.
#

lfpr_dbq_clip <- function(
    schema_table_point,
    schema_table_polygon,
    join_column,
    join_on) {

  join_on_string <- paste0("'", join_on, "'", collapse = ", ")

  glue::glue("SELECT point.*, poly.{join_column}
   FROM {schema_table_point} point
   INNER JOIN {schema_table_polygon} poly
   ON ST_Intersects(poly.geom, point.geom)
   WHERE poly.{join_column} IN ({join_on_string});")

}

# Creates hydrographs
#' @param station String (quoted) number of station
#' @param pane_hydat Boolean TRUE if you want a pane layout of all hydrographs
#' @param single_hydat Boolean TRUE if you want a single hydrograph with mean flows
#' @param start_year Specific start year, if not specified, will use the first year of the data
#' @param end_year Specific end year, if not specified, will use the first year of the data
#' @param fig/hydrology_stats_ hydrology stats figure saved to the fig folder
#' @param fig/hydrograph_ hydrograph figure saved to the fig folder

lfpr_create_hydrograph <- function(
    station = NULL,
    pane_hydat = TRUE,
    single_hydat = TRUE,
    start_year = NULL,
    end_year = NULL){

  if(is.null(station)){
    poisutils::ps_error('Please provide a station number, for example "08EE004"')
  }

  chk::chk_string(station)
  chk::chk_flag(pane_hydat)
  chk::chk_flag(single_hydat)

  flow_raw <- tidyhydat::hy_daily_flows(station)

  if(is.null(start_year)){
    start_year <- flow_raw$Date %>% min() %>% lubridate::year()
  }

  if(is.null(end_year)){
    end_year <- flow_raw$Date %>% max() %>% lubridate::year()
  }

  chk::chk_number(start_year)
  chk::chk_number(end_year)

  tidyhat_info <- tidyhydat::search_stn_number(station)


  ##### Hydrograph Stats #####

  ##build caption for the pane figure
  caption_info <- dplyr::mutate(tidyhat_info, title_stats = paste0(stringr::str_to_title(STATION_NAME),
                                                                   " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                                   " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", start_year,
                                                                   # FIRST_YEAR, ##removed the default here
                                                                   " to ",end_year, "."))

  hydrograph1_stats_caption <- caption_info$title_stats



  if (pane_hydat == TRUE){
    #Create pane of hydrographs with "Mean", "Minimum", "Maximum", and "Standard Deviation" flows
    hydrograph_stats_print <- fasstr::plot_data_screening(station_number = station, start_year = start_year,
                                                          include_stats = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
                                                          plot_availability = FALSE)[["Data_Screening"]] + ggdark::dark_theme_bw() ##first version is not dark
    hydrograph_stats_print

    #Save hydrograph pane
    ggplot2::ggsave(plot = hydrograph_stats_print, file=paste0("fig/hydrology_stats_", station, ".png"),
                    h=3.4, w=5.11, units="in", dpi=300)

    cli::cli_alert(hydrograph1_stats_caption)
  }





  ##### Single Hydrograph  #####

  ##build caption for the single figure
  caption_info2 <- dplyr::mutate(tidyhat_info, title_stats2 = paste0(stringr::str_to_title(STATION_NAME),
                                                                     " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                                     " Lon ",round(LONGITUDE,6), "). Available mean daily discharge data from ", start_year,
                                                                     # FIRST_YEAR, ##removed the default here
                                                                     " to ",end_year, "."))

  hydrograph1_stats_caption2 <- caption_info2$title_stats2

  if (single_hydat == TRUE){
    # Create single hydrograph with mean flows from date range
    flow <- flow_raw %>%
      dplyr::mutate(day_of_year = lubridate::yday(Date)) %>%
      dplyr::group_by(day_of_year) %>%
      dplyr::summarise(daily_ave = mean(Value, na.rm=TRUE),
                       daily_sd = sd(Value, na.rm = TRUE),
                       max = max(Value, na.rm = TRUE),
                       min = min(Value, na.rm = TRUE)) %>%
      dplyr::mutate(Date = as.Date(day_of_year))

    plot <- ggplot2::ggplot()+
      ggplot2::geom_ribbon(data = flow, aes(x = Date, ymax = max,
                                            ymin = min),
                           alpha = 0.3, linetype = 1)+
      ggplot2::scale_x_date(date_labels = "%b", date_breaks = "2 month") +
      ggplot2::labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
      ggdark::dark_theme_bw() +
      ggplot2::geom_line(data = flow, aes(x = Date, y = daily_ave),
                         linetype = 1, linewidth = 0.7) +
      ggplot2::scale_colour_manual(values = c("grey10", "red"))
    plot

    ggplot2::ggsave(plot = plot, file=paste0("fig/hydrograph_", station, ".png"),
                    h=3.4, w=5.11, units="in", dpi=300)

    cli::cli_alert(hydrograph1_stats_caption2)
  }
}


#' Determine replacement structure type and size based on measured field metrics.
#' @param dat PSCIS data
#' @param fill_dpth standard fill depth, default is 3m.
#' @param brdg_wdth standard bridge width, default is 15m.
#' @param chn_wdth_max maximum channel width where the bridge should start to be more than brdg_wdth, default is brdg_wdth - 5m.
#' @param fill_dpth_mult for every 1 m deeper than 3m, we need a 1.5:1 slope so there is 3m more bridge required
#'
#' @importFrom dplyr mutate filter select case_when
#' @importFrom plyr round_any
#' @importFrom readr write_csv
#' @importFrom chk chk_numeric
#'
#' @export
#'
#' #' @examples \dontrun{
#' fpr_structure_size_type(dat)
#' }
#'

lfpr_structure_size_type <- function(
    dat = NULL,
    fill_dpth = 3,
    brdg_wdth = 15,
    chn_wdth_max = brdg_wdth - 5,
    fill_dpth_mult = 3) {

  if (is.null(dat))
    stop('please provide "dat" (dataframe) object')
  if (!is.data.frame(dat))
    stop('"dat" must inherit from a data.frame')

  chk::chk_numeric(fill_dpth)
  chk::chk_numeric(brdg_wdth)
  chk::chk_numeric(chn_wdth_max)
  chk::chk_numeric(fill_dpth_mult)

  # Unsure if this still needs to be included, but can't find pcsis2...
  # ##according to the moe specs in MoE 2011 - backwatering requires od<30 and slope <2, swr <1.2 see if there are options
  # tab_backwater <- dat %>%  ##changed this to pscis2!
  #   filter(barrier_result != 'Passable' &
  #            barrier_result != 'Unknown' &
  #            outlet_drop_meters < 0.3 &
  #            stream_width_ratio_score < 1.2 &
  #            culvert_slope_percent <= 2 )


  str_type <- dat %>%
    dplyr::select(rowid, aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, source, barrier_result,
           downstream_channel_width_meters, fill_depth_meters) %>%
    dplyr::mutate(fill_dpth_over = fill_depth_meters - fill_dpth_mult) %>%
    dplyr::mutate(crossing_fix = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                    & downstream_channel_width_meters >= 2 ~ 'Replace with New Open Bottom Structure',
                                    barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_character_,
                                    T ~ 'Replace Structure with Streambed Simulation CBS'))  %>%
    dplyr::mutate(span_input = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                  & downstream_channel_width_meters >= 2 ~ brdg_wdth,
                                  barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_real_,
                                  T ~ 3))  %>%
    dplyr::mutate(span_input = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                  & fill_dpth_over > 0 & !crossing_fix %ilike% 'Simulation' ~
                                    (brdg_wdth + fill_dpth_mult * fill_dpth_over),  ##1m more fill = 3 m more bridge
                                  T ~ span_input)) %>%
    dplyr::mutate(span_input = dplyr::case_when(span_input < (downstream_channel_width_meters + 4) & ##span not need be extended if already 4m bigger than channel width
                                    downstream_channel_width_meters > chn_wdth_max ~
                                    (downstream_channel_width_meters - chn_wdth_max) + span_input,  ##for every m bigger than a 5 m channel add that much to each side in terms of span
                                  T ~ span_input)) %>%
    ##let's add an option that if the stream is under 3.5m wide and under more than 5m of fill we do a streambed simulation with a 4.5m embedded multiplate like 4607464 on Flathead fsr
    dplyr::mutate(crossing_fix = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                    & downstream_channel_width_meters > 2 &
                                      downstream_channel_width_meters <= 3.5 &
                                      fill_depth_meters > 5 ~ 'Replace Structure with Streambed Simulation CBS',
                                    T ~ crossing_fix),
           span_input = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                  & downstream_channel_width_meters > 2 &
                                    downstream_channel_width_meters <= 3.5 &
                                    fill_depth_meters > 5 ~ 4.5,
                                  T ~ span_input)) %>%
    dplyr::mutate(span_input = plyr::round_any(span_input, 0.5))


  ##burn to a csvs so we can copy and paste into spreadsheet

    str_type %>%
      dplyr::filter(source %ilike% 'phase1') %>%
      readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis1.csv'),
                       na = '')
    str_type %>%
      dplyr::filter(source %ilike% 'phase2') %>%
      readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis2.csv'),
                       na = '')
    str_type %>%
      dplyr::filter(source %ilike% 'reasses') %>%
      readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis_reassessments.csv'),
                       na = '')

}


# set up a table format
mygt <- function(x, page_size = 5, font = "10px", ...) {
  x |>
    gt::sub_missing() |>
    # unfortunately we cannot yet adjust the font size... https://github.com/rstudio/gt/issues/1307
    gt::tab_options(table.font.size = font) |>
    gt::opt_interactive(...,
                        use_search = TRUE,
                        use_filters = TRUE,
                        use_compact_mode = TRUE,
                        use_highlight = TRUE,
                        use_page_size_select = TRUE,
                        page_size_values = c(5, 10, 20, 50),
                        use_resizers = TRUE,
                        page_size_default = page_size)
}




