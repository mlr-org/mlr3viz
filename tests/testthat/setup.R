if (requireNamespace("lgr")) {
  lg = lgr::get_logger("mlr3")
  old_threshold = lg$threshold
  lg$set_threshold("warn")
}

old_dt_threads = data.table::getDTthreads()
old_omp_threads = Sys.getenv("OMP_THREAD_LIMIT")

setDTthreads(1)
Sys.setenv("OMP_THREAD_LIMIT" = 1)
