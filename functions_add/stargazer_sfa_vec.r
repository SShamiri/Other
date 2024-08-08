# stargazer model compare approach for frontier package
stargazer_sfa_vec <- function(models = c("mod_1"," mod_2"), 
                          column_labels = NULL
                          ){
  # save model objects into list
  lst <- list()
  for(i in 1: length(models)){
  mod <- eval(parse(text = models[i])) %>% 
         tidy_sfa() %>%
        mutate_if(is.numeric, round,3)

  lst[[i]] <- mod
  }
  # model compact in list
  out <- list()
  for(i in 1: length(lst)) {
        eff = lst[[i]] %>% filter(term == 'efficiency') %>% pull(estimate)
        loglike = lst[[i]] %>% filter(term == 'log likelihood') %>% pull(estimate)
        dat = data.frame(term = lst[[i]]$term, m1 =paste0(lst[[i]]$estimate, "(", lst[[i]]$se,")", lst[[i]]$sig)) %>%
              mutate(m1 = ifelse(term == 'efficiency', eff, m1),
                    m1 = ifelse(term == 'log likelihood', loglike, m1) )
        mod_name <- paste0('mod_', i)
        names(dat)[2] = mod_name
        out[[i]] <- dat
  }
# re-arrange variables
n = c()
for(i in 1:length(lst)) {
  n = c(n, out[[i]]$term)
}
n = unique(n)
a = n[!grepl('sigmaSq|gamma|mu|efficiency|log likelihood', n)]
b = n[grepl('sigmaSq|gamma|mu|efficiency|log likelihood', n)]
n =c(a,b)
term_df = data.frame(term = n)
out1 = c(list(term_df),out)
# convert list of compact models into data frame
df <- out1 %>%
reduce(left_join, by = "term") %>%
mutate_if(is.character, ~replace_na(., ""))
# rename columns
if(length(column_labels)>0) {
 names(df)[2:ncol(df)] = column_labels
} 
return(list(compare = df, models = lst))
}