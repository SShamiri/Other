
dat_a = read_csv('J:\\My Documents\\Data linkage methodological references\\Examples\\Input file_csv\\file_a.csv')
dat_b = read_csv('J:\\My Documents\\Data linkage methodological references\\Examples\\Input file_csv\\file_b.csv')

## passes as an input
pass_1 = c('MB_11', 'BYEAR', 'SEX', 'SURNAME', 'COB')
pass_2 = c('MB_11', 'BYEAR', 'SEX', 'SURNAME')
pass_3 = c('SA1_11', 'BYEAR', 'SEX', 'SURNAME', 'COB')
pass_4 = c('SA1_11', 'BYEAR', 'SEX', 'SURNAME')
pass_5 = c('MB_10', 'BYEAR', 'SEX', 'SURNAME', 'COB')
pass_6 = c('MB_10', 'BYEAR', 'SEX', 'SURNAME')
pass_7 = c('SA1_10', 'BYEAR', 'SEX', 'SURNAME', 'COB')
pass_8 = c('SA1_10', 'BYEAR', 'SEX', 'SURNAME')



passes = list(list(Pass = pass_1, Pass_n =1),list(Pass = pass_2, Pass_n =2),
              list(Pass = pass_3, Pass_n =3),list(Pass = pass_4, Pass_n =4),
              list(Pass = pass_5, Pass_n =5),list(Pass = pass_6, Pass_n =6),
              list(Pass = pass_7, Pass_n =7),list(Pass = pass_8, Pass_n =8))


d = d_link( data_a = dat_a, data_b = dat_a, passes = passes)




d_link = function(data_a = file_a, data_b = file_a, passes = passes) {
  
  dat_a = data_a
  dat_b = data_b
  size_a = nrow(dat_a)
  ### select the data (a and b) for each pass and removing NA
  list_a = list()
  list_b = list()
  for(i in 1:length(passes)){
    #  i = 1
    list_a[[i]] = list(pass = passes[[i]]$Pass, 
                       pass_n = passes[[i]]$Pass_n,
                       data = dat_a %>% select(!!passes[[i]]$Pass,ID_A) %>% drop_na()
    )
    
    list_b[[i]] = list(pass = passes[[i]]$Pass, 
                       pass_n = passes[[i]]$Pass_n,
                       data = dat_b %>% select(!!passes[[i]]$Pass,ID_B) %>% drop_na()
    )
  }
  
  all_agreements = list()
  for(i in 1:length(passes)){
    all_agreements[[i]] = list_a[[i]]$data %>% left_join(list_b[[i]]$data, by = passes[[i]]$Pass) %>% 
      filter(!is.na(ID_B)) %>% select(ID_A, ID_B) %>%
      mutate(pass = paste(passes[[i]]$Pass,collapse = '-'),pass_n = passes[[i]]$Pass_n)
  }
  
  summaryList = list()  
  duplicatesList = list()
  for(i in 1:length(passes)) {
    duplicates = all_agreements[[i]][duplicated(all_agreements[[i]]$ID_A)|duplicated(all_agreements[[i]]$ID_B, fromLast=TRUE),]
    ur = all_agreements[[i]] %>% filter(!(ID_A %in% duplicates$ID_A) & !(ID_B %in% duplicates$ID_B)) %>% 
      n_distinct() /nrow(all_agreements[[i]])
    summaryList[[i]] = data.frame(pass_n =passes[[i]]$Pass_n, 
                                  pass = paste(passes[[i]]$Pass,collapse = '-'),
                                  agreements = nrow(all_agreements[[i]]),
                                  ur = ur, stringsAsFactors = F)
    duplicatesList[[i]] = duplicates
  }
  # arrange by UR and pass
  summaryDat = bind_rows(summaryList) %>% arrange(desc(ur), pass_n)
  tmp_1 =  all_agreements[[summaryDat$pass_n[1]]]
  dup_1 = duplicatesList[[summaryDat$pass_n[1]]]
  IDs  = tmp_1 %>% filter(!(ID_A %in% dup_1$ID_A) & !(ID_B %in% dup_1$ID_B ))  %>% select(ID_A, ID_B)
  Marginal_link_1 = nrow(IDs)
  IDs_m_1 = tmp_1 %>% filter(!(ID_A %in% IDs$ID_A) & !(ID_B %in% IDs$ID_B )) %>% select(ID_A, ID_B) %>% n_distinct()
  IDs_m_1 = ifelse (IDs_m_1==0,Marginal_link_1,IDs_m_1)
  out_1 = data.frame(pass_n = summaryDat$pass_n[1],
                     pass = summaryDat$pass[1],
                     Marginal_link = Marginal_link_1,
                     links = nrow(IDs),
                     mur = Marginal_link_1/IDs_m_1,
                     stringsAsFactors = F)
  matchFinal = list()
  matchFinal[[1]] = IDs %>% mutate(pass_n = summaryDat$pass_n[1],pass = summaryDat$pass[1])
  
  out = list()
  
  for(i in 2:length(passes)) {
    tmp = all_agreements[[summaryDat$pass_n[i]]]
    dup = duplicatesList[[summaryDat$pass_n[i]]]
    #IDs = IDs_1
    IDs_n = tmp %>% filter(!(ID_A %in% IDs$ID_A) & !(ID_B %in% IDs$ID_B ) & !(ID_A %in% dup$ID_A) & !(ID_B %in% dup$ID_B))  %>% select(ID_A, ID_B) 
    IDs_m = tmp %>% filter(!(ID_A %in% IDs$ID_A) & !(ID_B %in% IDs$ID_B )) %>% select(ID_A, ID_B) %>% n_distinct()
    IDs = IDs %>% bind_rows(IDs_n)
    Marginal_link = nrow(IDs_n)
    mur = Marginal_link/IDs_m
    out[[i]] = data.frame(pass_n = summaryDat$pass_n[i],
                          pass = summaryDat$pass[i],
                          Marginal_link = Marginal_link,
                          links = nrow(IDs),
                          mur = mur,
                          stringsAsFactors = F)
    matchFinal[[i]] = IDs_n %>% mutate(pass_n = summaryDat$pass_n[i],pass = summaryDat$pass[i])
  }
  out[[1]] = out_1
  summaryFinal = bind_rows(out) %>% left_join(summaryDat) %>% mutate(link_rate = links/size_a)
  matchFinal = bind_rows(matchFinal) %>% left_join(summaryFinal) %>% select( pass_n, pass, ur,mur, ID_A,ID_B)
  
  results = list(Summary = summaryFinal, linkage = matchFinal )
}