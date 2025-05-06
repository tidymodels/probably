# lower_limit bounds for numeric predictions

    Code
      bound_prediction(modeldata::solubility_test, lower_limit = 2)
    Condition
      Error in `bound_prediction()`:
      ! The argument `x` should have a column named `.pred`.

---

    Code
      bound_prediction(mutate(modeldata::solubility_test, .pred = format(prediction)),
      lower_limit = 2)
    Condition
      Error in `bound_prediction()`:
      ! Column `.pred` should be numeric.

---

    Code
      bound_prediction(sol, lower_limit = tune2())
    Condition
      Error in `bound_prediction()`:
      ! `lower_limit` must be a number or `NA`, not a call.

---

    Code
      bound_prediction(as.matrix(sol), lower_limit = 1)
    Condition
      Error in `bound_prediction()`:
      ! `x` must be a data frame, not a double matrix.

# upper_limit bounds for numeric predictions

    Code
      bound_prediction(modeldata::solubility_test, lower_limit = 2)
    Condition
      Error in `bound_prediction()`:
      ! The argument `x` should have a column named `.pred`.

---

    Code
      bound_prediction(mutate(modeldata::solubility_test, .pred = format(prediction)),
      lower_limit = 2)
    Condition
      Error in `bound_prediction()`:
      ! Column `.pred` should be numeric.

---

    Code
      bound_prediction(sol, upper_limit = tune2())
    Condition
      Error in `bound_prediction()`:
      ! `upper_limit` must be a number or `NA`, not a call.

