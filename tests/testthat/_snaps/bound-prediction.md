# lower bounds for numeric predictions

    Code
      bound_prediction(solubility_test, lower = 2)
    Condition
      Error:
      ! The argument `x` should have a column named `.pred`

---

    Code
      solubility_test %>% mutate(.pred = format(prediction)) %>% bound_prediction(
        lower = 2)
    Condition
      Error:
      ! Column `.pred` should be numeric.

# upper bounds for numeric predictions

    Code
      bound_prediction(solubility_test, lower = 2)
    Condition
      Error:
      ! The argument `x` should have a column named `.pred`

---

    Code
      solubility_test %>% mutate(.pred = format(prediction)) %>% bound_prediction(
        lower = 2)
    Condition
      Error:
      ! Column `.pred` should be numeric.

