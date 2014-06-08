source("helper-forest.R")

context("Plotting (utilities)")

test_that("flip vector image", {
  fish <- vector_read("files/fish.svg")
  # Nothing happens:
  expect_that(flip(fish, FALSE, FALSE), equals(fish))
  expect_that(flip(fish, FALSE),        equals(fish))

  fish.h <- flip(fish, horizontal=TRUE)
  fish.v <- flip(fish, horizontal=FALSE, vertical=TRUE)
  fish.hv <- flip(fish, horizontal=TRUE, vertical=TRUE)

  # TODO: Would be good to check this on a two path image, especially
  # where scales differ between paths.
  expect_that(fish.h@paths[[1]]@x,
              equals(fish@summary@xscale[[2]] - fish@paths[[1]]@x))
  expect_that(fish.h@paths[[1]]@y, equals(fish@paths[[1]]@y))
  expect_that(fish.hv@paths[[1]]@x,
              equals(fish@summary@xscale[[2]] - fish@paths[[1]]@x))

  expect_that(fish.v@paths[[1]]@y,
              equals(fish@summary@yscale[[2]] - fish@paths[[1]]@y +
                     fish@summary@yscale[[1]]))
  expect_that(fish.v@paths[[1]]@x, equals(fish@paths[[1]]@x))
  expect_that(fish.hv@paths[[1]]@y,
              equals(fish@summary@yscale[[2]] - fish@paths[[1]]@y +
                     fish@summary@yscale[[1]]))

  expect_that(range(fish.hv@paths[[1]]@x),
              equals(unname(fish@summary@xscale)))
  expect_that(range(fish.hv@paths[[1]]@y),
              equals(unname(fish@summary@yscale)))

  if (interactive()) {
    grid.newpage()
    for (h in c(FALSE, TRUE)) {
      for (v in c(FALSE, TRUE)) {
        pushViewport(viewport(width=.5, height=.5, x=.5, y=.5, just=c(h, v)))
        grid.rect(gp=gpar(col="grey"))
        grImport::grid.picture(flip(fish, h, v))
        popViewport()
      }
    }
  }
})

test_that("flip raster (as array)", {
  pic.filename <- system.file("img", "Rlogo.png", package="png")
  pic <- readPNG(pic.filename)

  pic.h <- flip(pic, horizontal=TRUE)
  pic.v <- flip(pic, horizontal=FALSE, vertical=TRUE)
  pic.hv <- flip(pic, horizontal=TRUE, vertical=TRUE)

  expect_that(pic.h, equals(pic[,ncol(pic):1,]))
  expect_that(pic.v, equals(pic[nrow(pic):1,,]))
  expect_that(pic.hv, equals(pic[nrow(pic):1,ncol(pic):1,]))

  expect_that(dim(pic.h),  equals(dim(pic)))
  expect_that(dim(pic.v),  equals(dim(pic)))
  expect_that(dim(pic.hv), equals(dim(pic)))

  if (interactive()) {
    grid.newpage()
    for (h in c(FALSE, TRUE)) {
      for (v in c(FALSE, TRUE)) {
        pushViewport(viewport(width=.5, height=.5, x=.5, y=.5, just=c(h, v)))
        grid.rect(gp=gpar(col="grey"))
        grid.raster(flip(pic, h, v))
        popViewport()
      }
    }
  }
})

test_that("flip raster (as matrix)", {
  pic.filename <- system.file("img", "Rlogo.png", package="png")
  pic <- 1 - apply(readPNG(pic.filename), 1:2, max)

  pic.h <- flip(pic, horizontal=TRUE)
  pic.v <- flip(pic, horizontal=FALSE, vertical=TRUE)
  pic.hv <- flip(pic, horizontal=TRUE, vertical=TRUE)

  expect_that(pic.h, equals(pic[,ncol(pic):1]))
  expect_that(pic.v, equals(pic[nrow(pic):1,]))
  expect_that(pic.hv, equals(pic[nrow(pic):1,ncol(pic):1]))

  expect_that(dim(pic.h),  equals(dim(pic)))
  expect_that(dim(pic.v),  equals(dim(pic)))
  expect_that(dim(pic.hv), equals(dim(pic)))

  if (interactive()) {
    grid.newpage()
    for (h in c(FALSE, TRUE)) {
      for (v in c(FALSE, TRUE)) {
        pushViewport(viewport(width=.5, height=.5, x=.5, y=.5, just=c(h, v)))
        grid.rect(gp=gpar(col="grey"))
        grid.raster(flip(pic, h, v))
        popViewport()
      }
    }
  }
})

test_that("flip raster (as raster)", {
  pic.filename <- system.file("img", "Rlogo.png", package="png")
  pic <- as.raster(readPNG(pic.filename))

  pic.h <- flip(pic, horizontal=TRUE)
  pic.v <- flip(pic, horizontal=FALSE, vertical=TRUE)
  pic.hv <- flip(pic, horizontal=TRUE, vertical=TRUE)

  pic.mat <- as.matrix(pic)

  expect_that(as.matrix(pic.h),
              equals(pic.mat[,ncol(pic.mat):1]))
  expect_that(as.matrix(pic.v),
              equals(pic.mat[nrow(pic.mat):1,]))
  expect_that(as.matrix(pic.hv),
              equals(pic.mat[nrow(pic.mat):1,ncol(pic.mat):1]))

  if (interactive()) {
    grid.newpage()
    for (h in c(FALSE, TRUE)) {
      for (v in c(FALSE, TRUE)) {
        pushViewport(viewport(width=.5, height=.5, x=.5, y=.5, just=c(h, v)))
        grid.rect(gp=gpar(col="grey"))
        grid.raster(flip(pic, h, v))
        popViewport()
      }
    }
  }
})

test_that("Aspect ratio", {
  pic.filename <- system.file("img", "Rlogo.png", package="png")
  pic <- readPNG(pic.filename)
  pic.bw <- 1 - apply(readPNG(pic.filename), 1:2, max)
  pic.native <- readPNG(pic.filename, native=TRUE)

  r <- ncol(pic) / nrow(pic)

  expect_that(aspect_ratio(pic),            equals(r))
  expect_that(aspect_ratio(pic.native),     equals(r))
  expect_that(aspect_ratio(as.raster(pic)), equals(r))
  expect_that(aspect_ratio(pic.bw),         equals(r))

  fish <- vector_read("files/fish.svg")
  r <- unname(diff(fish@summary@xscale) / diff(fish@summary@yscale))

  expect_that(aspect_ratio(fish),              equals(r))
  expect_that(aspect_ratio(pictureGrob(fish)), equals(r))
})
