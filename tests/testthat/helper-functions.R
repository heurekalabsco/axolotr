# Helper function to check if we're offline
skip_if_offline <- function() {
  testthat::skip_if_not_installed("curl")
  if (!curl::has_internet()) {
    testthat::skip("Offline, skipping")
  }
}
