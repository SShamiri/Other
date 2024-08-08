# stargazer model compare approach for ols models
stargazer_mod_vec <- function(models = c("mod_1"," mod_2"), 
                          column_labels = NULL
                          ){
  # save model objects into list
  lst <- list()
  for(i in 1: length(models)){
  mod <- eval(parse(text = models[i])) %>% 
         tidy_mod() 
  lst[[i]] <- mod
  }
  # model compact in list
  out <- list()
  for(i in 1: length(lst)) {
        rsq = lst[[i]] %>% filter(term == 'rsq') %>% pull(estimate)
        adj_rsq = lst[[i]] %>% filter(term == 'adj_rsq') %>% pull(estimate)
        f_stat = lst[[i]] %>% filter(term == 'f_stat') %>% pull(estimate)
        f_sig = lst[[i]] %>% filter(term == 'f_stat') %>% pull(sig)
        f_stat = paste0(f_stat, f_sig)

        dat = data.frame(term = lst[[i]]$term, m1 =paste0(lst[[i]]$estimate, "(", lst[[i]]$std.error,")", lst[[i]]$sig)) %>%
             mutate(
                m1 = ifelse(term == 'rsq', rsq, m1),
                m1 = ifelse(term == 'adj_rsq', adj_rsq, m1),
                m1 = ifelse(term == 'f_stat', f_stat, m1)
              )
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
a = n[!grepl('rsq|adj_rsq|f_stat', n)]
b = n[grepl('rsq|adj_rsq|f_stat', n)]
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