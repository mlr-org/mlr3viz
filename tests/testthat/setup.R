library(mlr3)

lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")
