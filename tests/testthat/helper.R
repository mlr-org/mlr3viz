expect_doppelganger = function(id, p) {
  if (requireNamespace("vdiffr")) {
    expect_doppelganger(id, p)
  }
}
