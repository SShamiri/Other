# tidy model with R-square
tidy_mod <-  function(model) {
  tidy(model) %>%
  add_row(term = 'rsq', estimate =glance(model)$r.squared) %>%
  add_row(term = 'adj_rsq', estimate =glance(model)$adj.r.squared) %>%
  add_row(term = 'f_stat', estimate =glance(model)$statistic, p.value = glance(model)$p.value ) %>%
  mutate( 
  sig = case_when(
      p.value < 0.01 ~ "***",
      p.value > 0.01 & p.value < 0.05 ~ "**",
      p.value > 0.05 & p.value < 0.1 ~ "*",
      term == 'rsq'  ~ "",
      term == 'adj_rsq' ~ ""
      ),
    model =  Reduce(paste, deparse(formula(model))) 
      ) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  relocate(sig, .after = p.value) %>%
  mutate_if(is.character, ~replace_na(.,"")) 
}