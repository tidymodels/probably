# lower_limit bounds for numeric predictions

    Code
      bound_prediction(solubility_test, lower_limit = 2)
    Condition
      Error:
      ! The argument `x` should have a column named `.pred`

---

    Code
      solubility_test %>% mutate(.pred = format(prediction)) %>% bound_prediction(
        lower_limit = 2)
    Condition
      Error:
      ! Column `.pred` should be numeric.

# upper_limit bounds for numeric predictions

    Code
      bound_prediction(solubility_test, lower_limit = 2)
    Condition
      Error:
      ! The argument `x` should have a column named `.pred`

---

    Code
      solubility_test %>% mutate(.pred = format(prediction)) %>% bound_prediction(
        lower_limit = 2)
    Condition
      Error:
      ! Column `.pred` should be numeric.

