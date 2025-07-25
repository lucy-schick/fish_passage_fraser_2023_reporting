##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

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



lfpr_table_cv_detailed_print <- function(tab_sum,
                                         comments,
                                         photos,
                                         gitbook_switch = gitbook_on) {

  comment_label <- paste0("Comments: ", comments[[1]])

  photo_label <- paste0(
    "Photos: PSCIS ID ", photos[[1]],
    ". From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet."
  )

  photo_insert <- photos[[2]]

  output <- tab_sum |>
    knitr::kable(booktabs = TRUE) |>
    kableExtra::kable_styling(c("condensed"), full_width = TRUE, font_size = 11) |>
    kableExtra::add_footnote(
      label = list(comment_label, photo_label, photo_insert),
      notation = "none"
    )

  # inlclude page breaks so the pdf builds properly and so tables have some seperation in the online report- easier to read
  paste0(output, "<br><br><br><br><br>")
}





