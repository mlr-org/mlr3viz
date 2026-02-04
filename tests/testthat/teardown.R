if (requireNamespace("lgr")) {
  lg = lgr::get_logger("mlr3")
  lg$set_threshold(old_threshold)
}

setDTthreads(old_dt_threads)
