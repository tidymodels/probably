.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("tune::collect_metrics", "cal_rset")
}
