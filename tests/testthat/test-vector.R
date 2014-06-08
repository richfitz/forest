source("helper-forest.R")

context("Vector image utilities")

path <- tempdir()
file.svg <- "fish.svg"
file.eps <- "fish.eps"
file.xml <- "fish.xml"

test_that("SVG -> EPS conversion", {
  file.copy("files/fish.svg", path)

  # TODO: Generates noise on OS X - swallow that somehow.
  forest:::inkscape_svg_to_eps(file.path(path, file.svg),
                               file.path(path, file.eps))
  expect_that(file.exists(file.path(path, file.eps)), is_true())

  # These files should match except for the date created.  I'll match
  # the 'Creator' field too, because that will depend on things like
  # Cairo version.  This might turn out to be fragile.
  eps_new <- readLines(file.path(path, file.eps))
  eps_old <- readLines("files/fish.eps")
  re <- "^%%Creat"
  expect_that(eps_new[!grepl(re, eps_new)],
              equals(eps_old[!grepl(re, eps_old)]))
})

test_that("EPS -> XML conversion", {
  file.copy("files/fish.eps", path)

  forest:::ghostscript_eps_to_xml(file.path(path, file.eps),
                                  file.path(path, file.xml))
  expect_that(file.exists(file.path(path, file.xml)), is_true())

  # These files should match except for the date created.  There's
  # actually other information on that line worth checking, and this
  # is totally the wrong way of checkin XML.  But it works for now!
  xml_new <- readLines(file.path(path, file.xml))
  xml_old <- readLines("files/fish.xml")
  re <- "creator"
  expect_that(xml_new[!grepl(re, xml_new)],
              equals(xml_old[!grepl(re, xml_old)]))
})

# Next, read things in:
test_that("vector_read_xml", {
  file.copy("files/fish.xml", path)
  # Now, the directory 'path' has all 3 files

  expect_that(vector_read_xml(character(0)),
              throws_error("scalar"))
  expect_that(vector_read_xml(c("file1", "file2")),
                              throws_error("scalar"))

  pic <- vector_read_xml(file.path(path, file.xml))
  expect_that(pic, is_a("Picture"))
})

# It would be nice to check that the xml file is actually rebuilt
# here, but it's not strictly needed and I'm having a hard time
# getting the file timestamps to play nice.  So not bothering!
test_that("vector_read_eps", {
  expect_that(vector_read_eps(character(0)),
              throws_error("scalar"))
  expect_that(vector_read_eps(c("file1", "file2")),
                              throws_error("scalar"))

  pic <- vector_read_eps(file.path(path, file.eps))
  expect_that(pic, is_a("Picture"))

  # Delete the XML and see that it is regenerated:
  file.remove(file.path(path, file.xml))
  pic <- vector_read_eps(file.path(path, file.eps))
  expect_that(file.exists(file.path(path, file.xml)), is_true())
})

# It would be nice to check that the xml and eps files are actually
# rebuilt here, but it's not strictly needed and I'm having a hard
# time getting the file timestamps to play nice.  So not bothering!
test_that("vector_read_svg", {
  expect_that(vector_read_svg(character(0)),
              throws_error("scalar"))
  expect_that(vector_read_svg(c("file1", "file2")),
                              throws_error("scalar"))

  pic <- vector_read_svg(file.path(path, file.svg))
  expect_that(pic, is_a("Picture"))

  # Delete the eps and XML and see that it is regenerated:
  file.remove(file.path(path, file.eps))
  file.remove(file.path(path, file.xml))
  pic <- vector_read_svg(file.path(path, file.svg))
  expect_that(file.exists(file.path(path, file.eps)), is_true())
  expect_that(file.exists(file.path(path, file.xml)), is_true())
})

test_that("vector_read_svg", {
  # Now, read in files using read_svg:
  file.remove(file.path(path, file.eps))
  file.remove(file.path(path, file.xml))

  t_svg <- system.time(pic <- vector_read(file.path(path, file.svg)))
  expect_that(pic, is_a("Picture"))
  expect_that(file.exists(file.path(path, file.eps)), is_true())
  expect_that(file.exists(file.path(path, file.xml)), is_true())

  file.remove(file.path(path, file.svg)) # so it can't be read
  file.remove(file.path(path, file.xml))
  t_eps <- system.time(pic <- vector_read(file.path(path, file.svg)))
  expect_that(pic, is_a("Picture"))

  file.remove(file.path(path, file.xml)) # so it can't be read
  t_xml <- system.time(pic <- vector_read(file.path(path, file.svg)))
  expect_that(pic, is_a("Picture"))
})
