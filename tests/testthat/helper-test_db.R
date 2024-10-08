load_test_db <- function(testdb_dir = tempfile(pattern = "testdb_", tmpdir = test_path("..")),
                         image_dir = "/net/mraid20/export/tgdata/db/tgdb/emr/naryn_testdb/") {
    dir.create(testdb_dir)

    # Note: we should change this to download.file from a public link
    system(glue::glue("cp -rf {image_dir}/* {testdb_dir}/"))

    emr_db.connect(c(testdb_dir, file.path(testdb_dir, "utest")))
    emr_db.reload()
}

load_minimock_db <- function() {
    load_test_db(image_dir = "/net/mraid20/export/tgdata/db/tgdb/emr/minimock")
}


load_test_dbs <- function() {
    testdb_dir <- test_path("../testdb")
    testdb_dirs <- purrr::map_chr(c(1:4), ~ {
        glue::glue("{testdb_dir}_{.x}")
    })

    purrr::walk(testdb_dirs, ~ {
        if (dir.exists(.x)) {
            system(glue::glue("rm -rf {.x}"))
        }
    })

    purrr::walk2(c(1:4), testdb_dirs, ~ {
        system(glue::glue("cp -rf /net/mraid20/export/tgdata/db/tgdb/emr/nr_test_db_{.x}/ {.y}"))
    })

    emr_db.connect(db_dirs = testdb_dirs, do_reload = TRUE)
}

copy_test_db <- function(db, new_db = tempfile(pattern = "testdb_", tmpdir = test_path(".."))) {
    dir.create(new_db)
    system(glue::glue("cp -rf {db}/* {new_db}/"))
    return(normalizePath(new_db))
}
