
compare_models <- function(models, model_names =NULL){
  
  if (!requireNamespace("texreg", quietly = TRUE)) {
    stop("jsar does not directly support models of class ",
         class(model)[1],
         ", but it can sometimes use the ``broom`` or ``texreg`` package to extract model ",
         "information. Call texreg again after installing the ``broom`` ",
         "package to see if this is possible.")
  }
  
  lst_mods <- lst_models(models)
  coef_names <- character()
  coef_values <- numeric()
  
  coef_names <- tibble::tibble(term = coef_names)
  out <- list()
  lst_gof <- list()
  for(i in 1 : length(lst_mods)){
    mod_name <- paste0('mod_', i)
    estimate = round(lst_mods[[i]]@coef,3)
    se = round(lst_mods[[i]]@se,3)
    sig = stars(lst_mods[[i]]@se)
    param <- paste0(estimate, '(', se, ')', sig)
    
    dat <- tibble::tibble(term = lst_mods[[i]]@coef.names, mod = param )
    names(dat)[2] = mod_name
    out[[i]] <- dat
    gof <- tibble::tibble(term = lst_mods[[i]]@gof.names, mod = round(lst_mods[[i]]@gof,3))
    names(gof)[2] = mod_name
    lst_gof[[i]] <- gof
    
  }
  out_df <- reduce(out, full_join) |> 
    mutate(across(everything(), ~ replace(.x, is.na(.x), "-"))) |>
    add_row() |> 
    mutate(across(everything(), ~ replace(.x, is.na(.x), "####"))) |>
    bind_rows(
             reduce(lst_gof, full_join) |> 
             mutate(across(everything(), ~ replace(.x, is.na(.x), "-")))
    )
  # rename columns
  if(length(model_names)>0) {
    names(out_df)[2:ncol(out_df)] = model_names
  } 
  
  
  return(out_df)
}


lst_models <- function(mods, ...){
  # if a single model is handed over, put model inside a list
  if (!"list" %in% class(mods)[1]) {
    mods <- list(mods)
  }
  
  # create list of texreg objects
  models <- NULL
  for (i in 1:length(mods)) {
    model <- texreg::extract(mods[[i]], ...)
    if ("list" %in% class(model)) { # must be a nested list of models (e.g., systemfit)
      models <- append(models, model)
    } else { # normal case; one model
      models <- append(models, list(model))
    }
  }
  return(models)
  
}


stars <- function(p_value){
  stars <- NULL
  for(i in 1:length(p_value)){
    if(p_value[i] < 0.01){
      star <- '***'
    } else if(p_value[i] > 0.01 & p_value[i] < 0.05){
      star <- '**'
    } else if(p_value[i] > 0.05 & p_value[i] < 0.1){
      star <- '*'
    } else{
      star <- ''
    }
    stars <- append(stars, star)
  }
  return(stars)
}

# ------------------------- example 
rr =lst_models(lmod)
rr[[1]]@gof.names
rr[[1]]@gof




ss = dd[[2]]@pvalues

stars(ss)

compare_models(lmod, model_names = "lm")
compare_models(list(lmod,smod), model_names = c("lm", "sfa")) |> print(n = 100)

mm = compare_models(list(lmod,smod))

mm %>%
  #replace_na(list(mod_1 = '-'))
  mutate(across(everything(), ~ replace(.x, is.na(.x), "-")))
  
  mutate(across(everything(), ~ replace_na( "")))
  