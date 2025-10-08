.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("tune::collect_metrics", "cal_rset")
  vctrs::s3_register("tune::collect_predictions", "cal_rset")

  vctrs::s3_register("butcher::axe_call", "int_conformal_cv")
  vctrs::s3_register("butcher::axe_ctrl", "int_conformal_cv")
  vctrs::s3_register("butcher::axe_data", "int_conformal_cv")
  vctrs::s3_register("butcher::axe_env", "int_conformal_cv")
  vctrs::s3_register("butcher::axe_fitted", "int_conformal_cv")

  vctrs::s3_register("butcher::axe_call", "int_conformal_full")
  vctrs::s3_register("butcher::axe_ctrl", "int_conformal_full")
  vctrs::s3_register("butcher::axe_data", "int_conformal_full")
  vctrs::s3_register("butcher::axe_env", "int_conformal_full")
  vctrs::s3_register("butcher::axe_fitted", "int_conformal_full")

  vctrs::s3_register("butcher::axe_call", "int_conformal_split")
  vctrs::s3_register("butcher::axe_ctrl", "int_conformal_split")
  vctrs::s3_register("butcher::axe_data", "int_conformal_split")
  vctrs::s3_register("butcher::axe_env", "int_conformal_split")
  vctrs::s3_register("butcher::axe_fitted", "int_conformal_split")

  vctrs::s3_register("butcher::axe_call", "int_conformal_quantile")
  vctrs::s3_register("butcher::axe_ctrl", "int_conformal_quantile")
  vctrs::s3_register("butcher::axe_data", "int_conformal_quantile")
  vctrs::s3_register("butcher::axe_env", "int_conformal_quantile")
  vctrs::s3_register("butcher::axe_fitted", "int_conformal_quantile")
}
