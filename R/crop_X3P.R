crop_X3P <- function(x3p) {
  stopifnot(class(x3p) == "x3p")
  dims <- dim(x3p$surface.matrix)
  cropped_x3p <- x3p_crop(x3p, x = 0.1 * dims[1], y = 0.1 * dims[2], width = dims[1], height = dims[2] - (0.1* dims[2]))
  dims <- dim(cropped_x3p$surface.matrix)
  cropped_x3p <- x3p_crop(cropped_x3p, x = 0, width = dims[1] - (0.1 * dims[1]), y = 0, height = dims[2])
  return(cropped_x3p)
}
