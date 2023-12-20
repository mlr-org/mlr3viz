if (requireNamespace("lgr")) {
  lg = lgr::get_logger("mlr3")
  old_threshold = lg$threshold
  lg$set_threshold("warn")
}

old_dt_threads = data.table::getDTthreads()
setDTthreads(1)

