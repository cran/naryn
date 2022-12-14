load_test_db()


test_that("emr_vtrack fails when track deosn't exist", {
    expect_error(emr_vtrack.create("v1", "blabla"))
})

test_that("function min does not accept any parameters", {
    expect_error(emr_vtrack.create("v1", "track1", func = "min", params = 2))
})

test_that("Function min is not supported when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "min", keepref = TRUE))
})

test_that("Function value is not supported with quantitative data", {
    expect_error(emr_vtrack.create("v1", "track1", func = "value"))
})

test_that("Function min is not supported with categorical data", {
    expect_error(emr_vtrack.create("v1", "track6", func = "min"))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    vt_name <- emr_vtrack.create("v1", "track6", func = "value")
    expect_regression(emr_extract("v1"), "vtrack.1", column = "v1")
    expect_equal(vt_name, "v1")
})

test_that("function exists requires an additional parameter", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "track6", func = "exists"))
    expect_error(emr_extract("v1"))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "exists", params = c(1:8), time.shift = c(-20, 30))
    expect_regression(emr_extract("v1"), "vtrack.2", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "frequent", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.3", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "size", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.4", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "size", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.5", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track0", func = "size", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.6", column = "v1")
})

test_that("function size does not accept any parameters when applied to quantative data", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "track0", func = "size", params = c(2:5), time.shift = c(-100, 200)))
    expect_error(emr_extract("v1"))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "earliest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.7", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "earliest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.8", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "latest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.9", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "latest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.10", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "closest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.11", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "closest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.12", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track0", func = "stddev")
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.13", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track0", func = "stddev", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.14", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    expect_regression(emr_extract("v1"), "vtrack.15", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", keepref = TRUE)
    expect_regression(emr_extract("v1"), "vtrack.16", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.17", column = "v1")
})

test_that("Function max is not supported when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "max", keepref = TRUE))
})

test_that("Time shift is not allowed when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "avg", keepref = TRUE, time.shift = 2))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "percentile.upper", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.18", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "percentile.lower", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.19", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = FALSE, time.shift = 2)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.20", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = FALSE, time.shift = c(100000, 200000))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.21", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.22", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "min", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.23", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "max", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.24", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "earliest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.25", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.26", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "closest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.27", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "earliest.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.28", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "latest.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.29", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "closest.earlier.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.30", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "closest.later.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.31", column = "v1")
})

test_that("function quantile requires an additional parameter - percentile", {
    expect_error(emr_vtrack.create("v1", "track2", func = "quantile", keepref = FALSE, time.shift = c(-10, 20)))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "quantile", keepref = FALSE, time.shift = c(-10, 20), params = 0.5)
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.32", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "sum", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.33", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "lm.intercept", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.34", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "lm.slope", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.35", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.upper", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.36", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.upper.min", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.37", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.upper.max", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.38", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.lower", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.39", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.lower.min", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.40", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.lower.max", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.41", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt1.earliest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.42", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt1.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.43", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.earliest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.44", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.45", column = "v1")
})

test_that("emr_vtrack works", {
    set.seed(0)
    id2id <- data.frame(id1 = sample(0:999, 500), id2 = sample(0:999, 500))
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "avg", id.map = id2id)
    expect_regression(emr_extract("v1", iterator = "track2"), "vtrack.46", column = "v1")
})

test_that("emr_vtrack works", {
    set.seed(0)
    id2id <- data.frame(id1 = sample(0:999, 500), id2 = sample(0:999, 500), time.shift = sample(-100:200, 500, replace = TRUE))
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "avg", id.map = id2id)
    expect_regression(emr_extract("v1", iterator = "track2"), "vtrack.47", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!track0")
    expect_regression(emr_extract("v1"), "vtrack.48", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "track0")
    expect_regression(emr_extract("v1"), "vtrack.49", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!track0", func = "percentile.upper")
    expect_regression(emr_extract("v1"), "vtrack.50", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "track0", func = "percentile.upper")
    expect_regression(emr_extract("v1"), "vtrack.51", column = "v1")
})

test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!f1", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.52", column = "v1")
})


test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "f1", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.53", column = "v1")
})

test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!f1", keepref = TRUE, func = "percentile.upper")
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.54", column = "v1")
})

test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "f1", keepref = TRUE, func = "percentile.upper")
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.55", column = "v1")
})

test_that("Function max is not supported with categorical data", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = TRUE, names = "value")
    expect_error(emr_vtrack.create("v1", list(r, TRUE), func = "max", time.shift = c(-10, 20)))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = TRUE, names = "value")
    emr_vtrack.create("v1", list(r, FALSE), func = "max", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.56", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = TRUE, names = "value")
    emr_vtrack.create("v1", list(r, TRUE), func = "closest", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.57", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    r$ref <- NULL
    emr_vtrack.create("v1", list(r, FALSE), func = "max", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.58", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    r$ref <- NULL
    emr_vtrack.create("v1", list(r, TRUE), func = "closest", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.59", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    emr_vtrack.create("v1", list(r, FALSE), func = "percentile.upper", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.60", column = "v1")
})

test_that("emr_vtrack works with a single NA value as params", {
    emr_vtrack.clear()
    test_with_func <- function(func) {
        emr_vtrack.create("v1", "ph1", func = func, time.shift = c(-10, 20), params = NA)
        emr_vtrack.create("v2", "ph1", func = func, time.shift = c(-10, 20), params = 800)
        expect_equal(
            emr_extract("v1", names = "value"),
            emr_extract("v2", names = "value")
        )
    }

    categorical_funcs <- c("exists", "value", "sample", "sample.time", "frequent", "size", "earliest", "latest", "earliest.time", "latest.time", "closest.earlier.time", "closest.later.time", "dt1.earliest", "dt1.latest", "dt2.earliest", "dt2.latest")

    for (f in categorical_funcs) {
        test_with_func(f)
    }
})

test_that("emr_vtrack fails when params are invalid", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(NA, NA)))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(TRUE)))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c("savta")))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(15, NA)))
})

test_that("filter cannot be used when 'src' is a data frame", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    expect_error(emr_vtrack.create("v1", list(r, FALSE), func = "percentile.upper", time.shift = c(-10, 20), filter = "track1"))
})

test_that("Unable to implicitly set iterator policy with vtracks", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    emr_vtrack.create("v1", list(r, FALSE), func = "percentile.upper", time.shift = c(-10, 20))
    expect_error(emr_extract(c("v1", "track1"), keepref = TRUE))
})

test_that("Unable to implicitly set iterator policy with logical tracks", {
    emr_vtrack.clear()
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    withr::defer(emr_track.logical.rm("logical_track1", force = TRUE))
    expect_error(emr_extract(c("logical_track1", "track1"), keepref = TRUE))
})


test_that("emr_vtrack.attr.src works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.src("v1", "track2")
    expect_equal(emr_vtrack.attr.src("v1"), "track2")
})

test_that("emr_vtrack.attr.src works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    expect_error(emr_vtrack.attr.src("v1", "track10"))
    expect_equal(emr_vtrack.attr.src("v1"), "track1")
})

test_that("Invalid source used in a virtual track", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "track10"))
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    expect_error(emr_vtrack.attr.src("v1", list(head(r), FALSE)))
    expect_error(emr_vtrack.attr.src("v1"))
})

test_that("emr_vtrack.attr.func works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.func("v1", "value")
    expect_equal(emr_vtrack.attr.func("v1"), "value")
})

test_that("emr_vtrack.attr.params works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.params("v1", 26)
    expect_equal(emr_vtrack.attr.params("v1"), 26)
})

test_that("emr_vtrack.attr.keepref works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.keepref("v1", TRUE)
    expect_true(emr_vtrack.attr.keepref("v1"))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.time.shift("v1", c(-10, 20))
    expect_equal(emr_vtrack.attr.time.shift("v1"), c(-10, 20))
})

test_that("emr_vtrack.attr.id.map works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.id.map("v1", data.frame(id1 = 10, id2 = 20))
    expect_equal(
        emr_vtrack.attr.id.map("v1"),
        structure(list(id1 = 10, id2 = 20), class = "data.frame", row.names = c(
            NA,
            -1L
        ))
    )
})

test_that("emr_vtrack.attr.filter works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.filter("v1", "track2 & track3")
    # NOTE: it returns a call - not a string!
    expect_equal(emr_vtrack.attr.filter("v1"), quote(track2 & track3))
})

test_that("emr_vtrack.exists works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_true(emr_vtrack.exists("v1"))
})

test_that("emr_vtrack.exists works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_false(emr_vtrack.exists("sdaf"))
})

test_that("emr_vtrack.info works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_equal(
        emr_vtrack.info("v1"),
        list(
            src = "track2", time_shift = c(-10, 20), func = "dt2.latest",
            params = NULL, keepref = FALSE, id_map = NULL, filter = NULL
        )
    )
})

test_that("emr_vtrack.ls works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_equal(emr_vtrack.ls(), c("v1", "v2"))
})

test_that("emr_vtrack.ls works", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "track10"))
    emr_vtrack.create("v2", "track1")
    expect_equal(emr_vtrack.ls(), "v2")
})

test_that("emr_vtrack.ls works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.rm("v1")
    expect_equal(emr_vtrack.ls(), "v2")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "value")
    expect_regression(emr_extract("v1"), "vtrack.61", column = "v1")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "frequent", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.62", column = "v1")
})

test_that("vtrack and extract return the same", {
    emr_vtrack.clear()
    emr_vtrack.create("vt", "track1")
    expect_equal(emr_extract("vt"), emr_extract("track1", names = "vt"))
})

test_that("emr_vtrack.clear works", {
    emr_vtrack.clear()
    expect_equal(emr_vtrack.ls(), character(0))
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.clear()
    expect_equal(emr_vtrack.ls(), character(0))
})
