test_that("str_glimpse() gives the R 4.7.0 spacing for atomic vectors", {
  expect_equal(capture.output(str_glimpse(5)), "num 5")
  expect_equal(capture.output(str_glimpse(1:3)), "int [1:3] 1 2 3")
  expect_equal(capture.output(str_glimpse(NULL)), "NULL")
  expect_equal(
    capture.output(str_glimpse(factor(c("a", "b")))),
    'Factor w/ 2 levels "a","b": 1 2'
  )
  expect_equal(
    capture.output(str_glimpse(c(a = 1, b = 2))),
    c(
      "Named num [1:2] 1 2",
      ' - attr(*, "names")= chr [1:2] "a" "b"'
    )
  )
})

test_that("str_glimpse() gives the R 4.7.0 spacing for nested lists", {
  x <- list(a = 1:2, b = list(c = "x", d = list()), e = NULL)
  expect_equal(
    capture.output(str_glimpse(x)),
    c(
      "List of 3",
      " $ a: int [1:2] 1 2",
      " $ b: List of 2",
      '  ..$ c: chr "x"',
      "  ..$ d: list()",
      " $ e: NULL"
    )
  )
  expect_equal(
    capture.output(str_glimpse(x, max.level = 1)),
    c(
      "List of 3",
      " $ a: int [1:2] 1 2",
      " $ b: List of 2",
      " $ e: NULL"
    )
  )
})

test_that("str_glimpse() gives the R 4.7.0 spacing for attributes", {
  x <- structure(list(a = 1), meta = list(x = "y"), note = "z")
  expect_equal(
    capture.output(str_glimpse(x)),
    c(
      "List of 1",
      " $ a: num 1",
      ' - attr(*, "meta")= List of 1',
      '  ..$ x: chr "y"',
      ' - attr(*, "note")= chr "z"'
    )
  )
  expect_equal(
    capture.output(str_glimpse(x, give.attr = FALSE)),
    c("List of 1", " $ a: num 1")
  )
})

test_that("str_glimpse() keeps names and values that contain separators", {
  x <- list(
    `a:b` = list(1),
    `c: d` = structure(list(x = "1:2"), `e")=f` = "g")
  )
  expect_equal(
    capture.output(str_glimpse(x)),
    c(
      "List of 2",
      " $ a:b : List of 1",
      "  ..$ : num 1",
      " $ c: d: List of 1",
      '  ..$ x: chr "1:2"',
      '  ..- attr(*, "e")=f")= chr "g"'
    )
  )
})

test_that("str_glimpse() keeps the output of give.head = FALSE", {
  x <- list(a = list(b = "x"), c = 1)
  expect_equal(
    capture.output(str_glimpse(x, give.head = FALSE)),
    c(
      "List of 2",
      " $ a: List of 1",
      '  ..$ b: "x"',
      " $ c: 1"
    )
  )
})

test_that("str_rewrite() converts old-style str() output", {
  # Output of R 4.6 for the object below, rewritten on all R versions
  x <- list(
    a = 1,
    b = structure(list(`c:d` = y ~ x), foo = "bar"),
    e = expression(1),
    f = vctrs::new_vctr(1:3)
  )
  old <- c(
    "List of 4",
    " $ a: num 1",
    " $ b:List of 1",
    "  ..$ c:d:Class 'formula'  language y ~ x",
    '  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> ',
    '  ..- attr(*, "foo")= chr "bar"',
    " $ e:  expression(1)",
    " $ f: vctrs_vc [1:3] 1, 2, 3"
  )
  expect_equal(
    str_rewrite(old, x),
    c(
      "List of 4",
      " $ a: num 1",
      " $ b: List of 1",
      "  ..$ c:d: Class 'formula' language y ~ x",
      '  .. .. ..- attr(*, ".Environment")= <environment: R_GlobalEnv> ',
      '  ..- attr(*, "foo")= chr "bar"',
      " $ e:   expression(1)",
      " $ f:  vctrs_vc [1:3] 1, 2, 3"
    )
  )
  expect_equal(str_rewrite(" num 5", 5), "num 5")
  expect_equal(str_rewrite(" vctrs_vc [1:3] 1, 2, 3", NULL), " vctrs_vc [1:3] 1, 2, 3")
})
