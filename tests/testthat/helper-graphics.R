# bitmap devices like png() default to displaylist = "inhibit", so
# recordPlot() captures nothing unless the display list is explicitly
# enabled first. This helper builds a real, replayable recordedplot for
# use as a test fixture.
make_recorded_plot <- function() {
  path <- tempfile(fileext = ".png")
  grDevices::png(path)
  grDevices::dev.control(displaylist = "enable")
  plot(1:3)
  rp <- grDevices::recordPlot()
  grDevices::dev.off()
  rp
}
