# tidy model approach for frontier package
tidy_sfa <-  function(model) {
  coef <- summary(model)
  coef$mleParam %>% 
  data.frame() %>% 
  rownames_to_column('term') %>%
  rename(estimate = 2, se = 3, z_value = 4, p_value = 5) %>%
  add_row(term = 'efficiency', estimate = coef$efficMean) %>%
  add_row(term = 'log likelihood', estimate = coef$mleLogl) %>%
  mutate( 
      sig = case_when(
        p_value < 0.01 ~ "***",
        p_value > 0.01 & p_value < 0.05 ~ "**",
        p_value > 0.05 & p_value < 0.1 ~ "*"
        ),
        model =  Reduce(paste, deparse(formula(model)))
        ) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  mutate_if(is.character, ~replace_na(.,""))
}