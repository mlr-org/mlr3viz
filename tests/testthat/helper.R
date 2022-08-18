expect_doppelganger = function(id, p) {
  if (requireNamespace("vdiffr")) {
    vdiffr::expect_doppelganger(id, p)
  }
}
