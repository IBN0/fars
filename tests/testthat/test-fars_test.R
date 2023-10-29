test_that("fars_readoutput ", {
  expect_s3_class(fars_read('accident_2014.csv.bz2'), 'tbl_df')
})

test_that("fars_read error", {
  expect_error(fars_read('accident_2017.txt'))
})

test_that("make_filename output", {
  expect_equal(make_filename(2014), 'accident_2014.csv.bz2')
})
