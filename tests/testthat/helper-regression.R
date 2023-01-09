
#' Tests if an object was changed since the last run.
#' If an rds file named \code{snapshot_dir/id.rds} exists its contents are compared with \{obj},
#' otherwise the file is created.
#'
#' @param obj an R object
#' @param id unique test id.
#' @param snapshot_dir directory with rds file containing snapshot of previous versions
#' @param column if \code{obj} is a data.frame, only this column is compared, after the old and new data.frames are merged
expect_regression <- function(obj, id, snapshot_dir = "/net/mraid14/export/tgdata/db/tgdb/emr/naryn_snapshot", column = NULL) {
    regression_file <- file.path(snapshot_dir, glue::glue("{id}.rds"))

    if (!file.exists(regression_file)) {
        readr::write_rds(obj, regression_file)
        system(glue::glue("chmod a-w {regression_file}"))
    }

    # We need testthat to always find the `expect` statement (otherwise - the test would be skipped)
    old <- readr::read_rds(regression_file)

    if (is.data.frame(obj) && !is.null(column)) {
        old$old_column <- old[, column]
        obj$new_column <- obj[, column]
        columns <- intersect(colnames(old), colnames(obj))
        columns <- columns[columns != column]
        df <- old[, c(columns, "old_column")] %>%
            dplyr::full_join(obj[, c(columns, "new_column")], by = columns)
        expect_equal(df$old_column, df$new_column)
    } else {
        expect_equal(obj, old)
    }
}
