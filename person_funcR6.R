########################################################
# functions
########################################################
pacman::p_load(tidyverse, R6)

colsum <- function(dat, name) { 
  res <- dat %>% t() %>% data.frame() %>% dplyr::summarise_all(dplyr::funs(sum)) %>% 
    t() %>% data.frame() %>% tibble::as_tibble() 
  names(res) <- name
  return(res)
}

get_subset = function(score, subset, pername) {
  subscore <- score %>% dplyr::select(all_of(subset))
  names(subscore) = paste0(pername, seq(ncol(subscore)))
  res <- subscore %>% dplyr::bind_cols(subscore %>% colsum(pername))
  return(res)
}

scraping = function(url) {
  htmldat = httr::GET(url) %>% httr::content('raw') %>% xml2::read_html()
  answer = readr::read_csv(htmldat %>% rvest::html_nodes("p") %>% rvest::html_text())
}


scoring = R6Class('scoring', public = list(
  
  subjinf = function(dat) {
    inf <- dat %>% dplyr::select(attribute_1, token, sbjage, sbjgender)
    inf$sbjgender <- inf$sbjgender %>% str_replace("M", "1") %>% str_replace("F", "2") %>% 
      as.numeric()
    
    return(inf)
  },
  
  AQ = function(dat) {
    score <- dat %>% dplyr::select(`AQQ[1]`:`AQQ[50]`)
    
    type1 <- score %>% 
      dplyr::select(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
    for(i in seq(ncol(type1))) {
      type1[,i] <- type1[,i] %>% unlist %>% str_replace("A1", "1") %>% 
        str_replace("A2", "1") %>% str_replace("A3", "0") %>% str_replace("A4", "0") %>% 
        as.numeric()
    }
    type2 <- score %>% 
      dplyr::select(1,3,8,10,11,14,15,17,24,25,27,28,29,30,31,32,34,36,37,38,40,44,47,48,49,50)
    for(i in seq(ncol(type2))) {
      type2[,i] <- type2[,i] %>% unlist %>% str_replace("A1", "0") %>% 
        str_replace("A2", "0") %>% str_replace("A3", "1") %>% str_replace("A4", "1") %>% 
        as.numeric()
    }
    aqscore = type1 %>% dplyr::bind_cols(type2) %>% dplyr::select(names(score))
    
    AQ1 = get_subset(aqscore, c(1,11,13,15,22,36,44,45,47,48), "AQ_S")
    AQ2 = get_subset(aqscore, c(2,4,10,16,25,32,34,37,43,46), "AQ_A")
    AQ3 = get_subset(aqscore, c(5,6,9,12,19,23,28,29,30,49), "AQ_D")
    AQ4 = get_subset(aqscore, c(7,17,18,26,27,31,33,35,38,39), "AQ_C")
    AQ5 = get_subset(aqscore, c(3,8,14,20,21,24,40,41,42,50), "AQ_I")
    
    return(AQ1 %>% dplyr::bind_cols(AQ2, AQ3, AQ4, AQ5))
  },
  
  BAQ = function(dat) {
    score <- dat %>% dplyr::select(`BAQQ[1]`:`BAQQ[24]`) %>% 
      dplyr::mutate(`BAQQ[2]`= 6-`BAQQ[2]`, `BAQQ[5]`= 6-`BAQQ[5]`,
                    `BAQQ[10]`= 6-`BAQQ[10]`, `BAQQ[18]`= 6-`BAQQ[18]`,
                    `BAQQ[12]`= 6-`BAQQ[12]`)
    
    BAQ1 <- get_subset(score, c(21,14,19,17,2,5),"BAQ_P")
    BAQ2 <- get_subset(score, c(6,8,11,13,4), "BAQ_A")
    BAQ3 <- get_subset(score, c(23,15,7,20,10,18), "BAQ_H")
    BAQ4 <- get_subset(score, c(9,22,1,3,12), "BAQ_V")
    
    return(BAQ1 %>% dplyr::bind_cols(BAQ2, BAQ3, BAQ4))
  },
  
  SPQ = function(dat) {
    score <- dat %>% dplyr::select(`SPQQ[1]`:`SPQQ[74]`)
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_replace("A1", "1") %>% 
        str_replace("A2", "0") %>% as.numeric()
    }
    
    SPQ1 <- get_subset(score, 
                       c(1,10,19,28,37,45,53,60,63, 2,12,21,30,39,47,55, 
                         3,13,22,31,40,48,56,61,64, 9,18,27,36,44,52,59,65), 
                       "SPQ_C")
    SPQ2 <- get_subset(score, 
                       c(9,18,27,36,44,52,59,65, 4,11,20,29,38,46,54,71,
                         5,15,24,33,41,49,57,62,66, 6,17,26,35,43,51,68,73), 
                       "SPQ_I")
    SPQ3 <- get_subset(score, 
                       c(7,14,23,32,67,70,74, 8,16,25,34,42,50,58,69,72), 
                       "SPQ_D")
    
    return(SPQ1 %>% dplyr::bind_cols(SPQ2, SPQ3))
  },
  
  PSPS = function(dat) {
    score <- dat %>% dplyr::select(`PSPSQ[1]`:`PSPSQ[26]`)
    
    for(s in seq(ncol(score)))  score[,s] = score[,s] %>% unlist %>% as.numeric()
    score = score %>% 
      dplyr::mutate(`PSPSQ[10]`= 5-`PSPSQ[10]`, `PSPSQ[14]`= 5-`PSPSQ[14]`, 
                    `PSPSQ[6]`= 5-`PSPSQ[6]`,
                    `PSPSQ[15]`= 5-`PSPSQ[15]`, `PSPSQ[16]`= 5-`PSPSQ[16]`)
    
    PSPS1 <- get_subset(score, c(12,13,2,19,25,23,20,1,11,4,10,6,14,26,7), "PSPS_P")
    PSPS2 <- get_subset(score, c(15,18,24,16,3,22), "PSPS_S")
    
    return(PSPS1 %>% dplyr::bind_cols(PSPS2))
  },
  
  PSS = function(dat) {
    score <- dat %>% dplyr::select(`PSSQ[1]`:`PSSQ[10]`)
    
    for(i in 1:10)  score[,i] <- score[,i] %>% unlist %>% as.numeric
    score <- score %>% 
      dplyr::mutate(`PSSQ[4]`= 6-`PSSQ[4]`, `PSSQ[5]`= 6-`PSSQ[5]`, `PSSQ[8]`= 6-`PSSQ[8]`) - 1
    return(get_subset(score, seq(ncol(score)), "PSS") %>% tibble::as_tibble())
  },
  
  RSS = function(dat) {
    score <- dat %>% dplyr::select(`RSSQ[SQ001]`:`RSSQ[SQ010]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove('A') %>% as.numeric()
    
    score <- score %>% 
      dplyr::mutate(`RSSQ[SQ002]`= 5-`RSSQ[SQ002]`, `RSSQ[SQ005]`= 5-`RSSQ[SQ005]`,
                    `RSSQ[SQ006]`= 5-`RSSQ[SQ006]`, `RSSQ[SQ008]`= 5-`RSSQ[SQ008]`,
                    `RSSQ[SQ009]`= 5-`RSSQ[SQ009]`)
    return(get_subset(score, seq(ncol(score)), "RSS"))
  },
  
  IRI = function(dat) {
    score <- dat %>% dplyr::select(`IRI2Q[1]`:`IRI2Q[28]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("a") %>% as.numeric()
    
    IRI1 <- get_subset(score, c(1,5,7,12,16,23,26), "IRI_F")
    IRI2 <- get_subset(score, c(3,8,11,15,21,25,28), "IRI_PT")
    IRI3 <- get_subset(score, c(2,4,9,14,18,20,22), "IRI_EC")
    IRI4 <- get_subset(score, c(6,10,13,17,19,24,27), "IRI_PD")
    return(IRI1 %>% dplyr::bind_cols(IRI2, IRI3, IRI4))
  },
  
  AUDIT = function(dat) {
    score <- dat %>% dplyr::select(AUDITQ001:AUDITQ010)
    
    for(s in seq(ncol(score)))  score[,s] = score[,s] %>% unlist %>% as.numeric()
    score[,1:8] <- score[,1:8] - 1
    score[,9:10] <- (score[,9:10] - 1) * 2
    score[is.na(score)] <- 0
    return(get_subset(score, seq(ncol(score)), "AUDIT"))
  },
  
  Big5 = function(dat) {
    age <- dat %>% dplyr::select(sbjage) %>% dplyr::rename(age=sbjage)
    score <- dat %>% dplyr::select(`BIGFIVEQ[SQ001]`:`BIGFIVEQ[SQ070]`)
    
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_replace("A1", "1") %>% 
        str_replace("A2", "0") %>% as.numeric()
    }
    
    BIGFIVE1 <- score %>% dplyr::select(31,44,50,5,15,25,48,65) %>%
      dplyr::mutate(`BIGFIVEQ[SQ005]`= 1-`BIGFIVEQ[SQ005]`, `BIGFIVEQ[SQ015]`= 1-`BIGFIVEQ[SQ015]`,
                    `BIGFIVEQ[SQ025]`= 1-`BIGFIVEQ[SQ025]`, `BIGFIVEQ[SQ048]`= 1-`BIGFIVEQ[SQ048]`, 
                    `BIGFIVEQ[SQ065]`= 1-`BIGFIVEQ[SQ065]`)
    for(i in seq(ncol(BIGFIVE1))) {
      BIGFIVE1[,i] = replace(BIGFIVE1[,i] %>% unlist, 
                             is.na(BIGFIVE1[,i]) %>% which, 0) %>% data.frame
    }
    BF1 = get_subset(BIGFIVE1, seq(ncol(BIGFIVE1)), "Big5_F")
    
    BIGFIVE2 <- score %>% dplyr::select(9,20,41,43,50,33,39,52,58,69) %>% 
      dplyr::mutate(`BIGFIVEQ[SQ033]`= 1-`BIGFIVEQ[SQ033]`, `BIGFIVEQ[SQ039]`= 1-`BIGFIVEQ[SQ039]`,
                    `BIGFIVEQ[SQ052]`= 1-`BIGFIVEQ[SQ052]`, `BIGFIVEQ[SQ058]`= 1-`BIGFIVEQ[SQ058]`, 
                    `BIGFIVEQ[SQ069]`= 1-`BIGFIVEQ[SQ069]`)
    for(i in seq(ncol(BIGFIVE2))) {
      BIGFIVE2[,i] = replace(BIGFIVE2[,i] %>% unlist, 
                             is.na(BIGFIVE2[,i]) %>% which, 0) %>% data.frame
    }
    BF2 = get_subset(BIGFIVE2, seq(ncol(BIGFIVE2)), "Big5_S")
    
    BIGFIVE3 <- score %>% dplyr::select(3,11,19,38,53,57,4,14,23,35,37,60) %>% 
      dplyr::mutate(`BIGFIVEQ[SQ004]`= 1-`BIGFIVEQ[SQ004]`, `BIGFIVEQ[SQ014]`= 1-`BIGFIVEQ[SQ014]`,
                    `BIGFIVEQ[SQ023]`= 1-`BIGFIVEQ[SQ023]`, `BIGFIVEQ[SQ035]`= 1-`BIGFIVEQ[SQ035]`,
                    `BIGFIVEQ[SQ037]`= 1-`BIGFIVEQ[SQ037]`, `BIGFIVEQ[SQ060]`= 1-`BIGFIVEQ[SQ060]`)
    for(i in seq(ncol(BIGFIVE3))) {
      BIGFIVE3[,i] = replace(BIGFIVE3[,i] %>% unlist, 
                             is.na(BIGFIVE3[,i]) %>% which, 0) %>% data.frame
    }
    BF3 = get_subset(BIGFIVE3, seq(ncol(BIGFIVE3)), "Big5_E")
    
    BIGFIVE4 <- score %>% dplyr::select(5,15,25,48,65,68,70,6,31,45,49,63) %>% 
      dplyr::mutate(`BIGFIVEQ[SQ006]`= 1-`BIGFIVEQ[SQ006]`, `BIGFIVEQ[SQ031]`= 1-`BIGFIVEQ[SQ031]`,
                    `BIGFIVEQ[SQ045]`= 1-`BIGFIVEQ[SQ045]`, `BIGFIVEQ[SQ049]`= 1-`BIGFIVEQ[SQ049]`,
                    `BIGFIVEQ[SQ063]`= 1-`BIGFIVEQ[SQ063]`)
    for(i in seq(ncol(BIGFIVE4))) {
      BIGFIVE4[,i] = replace(BIGFIVE4[,i] %>% unlist, 
                             is.na(BIGFIVE4[,i]) %>% which, 0) %>% data.frame
    }
    BF4 = get_subset(BIGFIVE4, seq(ncol(BIGFIVE4)), "Big5_A")
    
    BIGFIVE5 <- score %>% dplyr::select(12,21,51,55,67,1,2,10,27,28,36,64) %>% 
      dplyr::mutate(`BIGFIVEQ[SQ001]`= 1-`BIGFIVEQ[SQ001]`, `BIGFIVEQ[SQ002]`= 1-`BIGFIVEQ[SQ002]`,
                    `BIGFIVEQ[SQ010]`= 1-`BIGFIVEQ[SQ010]`, `BIGFIVEQ[SQ027]`= 1-`BIGFIVEQ[SQ027]`,
                    `BIGFIVEQ[SQ028]`= 1-`BIGFIVEQ[SQ028]`, `BIGFIVEQ[SQ036]`= 1-`BIGFIVEQ[SQ036]`,
                    `BIGFIVEQ[SQ064]`= 1-`BIGFIVEQ[SQ064]`)
    for(i in seq(ncol(BIGFIVE5))) {
      BIGFIVE5[,i] = replace(BIGFIVE5[,i] %>% unlist, 
                             is.na(BIGFIVE5[,i]) %>% which, 0) %>% data.frame
    }
    BF5 = get_subset(BIGFIVE5, seq(ncol(BIGFIVE5)), "Big5_C")
    
    BIGFIVE6 <- score %>% dplyr::select(16,17,8,13,18,24,26,29,44,56,62,66) %>% 
      dplyr::mutate(`BIGFIVEQ[SQ008]`= 1-`BIGFIVEQ[SQ008]`, `BIGFIVEQ[SQ013]`= 1-`BIGFIVEQ[SQ013]`,
                    `BIGFIVEQ[SQ018]`= 1-`BIGFIVEQ[SQ018]`, `BIGFIVEQ[SQ024]`= 1-`BIGFIVEQ[SQ024]`,
                    `BIGFIVEQ[SQ026]`= 1-`BIGFIVEQ[SQ026]`, `BIGFIVEQ[SQ029]`= 1-`BIGFIVEQ[SQ029]`,
                    `BIGFIVEQ[SQ044]`= 1-`BIGFIVEQ[SQ044]`, `BIGFIVEQ[SQ056]`= 1-`BIGFIVEQ[SQ056]`,
                    `BIGFIVEQ[SQ062]`= 1-`BIGFIVEQ[SQ062]`, `BIGFIVEQ[SQ066]`= 1-`BIGFIVEQ[SQ066]`)
    for(i in seq(ncol(BIGFIVE6))) {
      BIGFIVE6[,i] = replace(BIGFIVE6[,i] %>% unlist, 
                             is.na(BIGFIVE6[,i]) %>% which, 0) %>% data.frame
    }
    BF6 = get_subset(BIGFIVE6, seq(ncol(BIGFIVE6)), "Big5_N")
    
    BIGFIVE7 <- score %>% dplyr::select(7,30,34,40,42,46,54,59,61,22,32,47) %>% 
      dplyr::mutate(`BIGFIVEQ[SQ022]`= 1-`BIGFIVEQ[SQ022]`, `BIGFIVEQ[SQ032]`= 1-`BIGFIVEQ[SQ032]`,
                    `BIGFIVEQ[SQ047]`= 1-`BIGFIVEQ[SQ047]`)
    for(i in seq(ncol(BIGFIVE7))) {
      BIGFIVE7[,i] = replace(BIGFIVE7[,i] %>% unlist, 
                             is.na(BIGFIVE7[,i]) %>% which, 0) %>% data.frame
    }
    BF7 = get_subset(BIGFIVE7, seq(ncol(BIGFIVE7)), "Big5_O")
    
    BIGFIVE_allitem = BF1 %>% dplyr::bind_cols(BF2, BF3, BF4, BF5, BF6, BF7, age) %>% 
      dplyr::mutate(ID = seq(nrow(dat)), who = dat$id)
    
    nodat = BIGFIVE_allitem %>% dplyr::filter(age >= 999 | age < 12) %>% 
      dplyr::filter(age!=0) # age check
    nodat$Big5_F = 0
    
    BIGFIVE = BIGFIVE_allitem %>% 
      dplyr::select(Big5_F, Big5_S, Big5_E, Big5_A, Big5_C, Big5_N, Big5_O, age, ID, who)
    BIGFIVE$age[is.na(BIGFIVE$age)] <- 0
    
    # youth:12-22, pre-adult:23-39, mid-adult:40-59, post-adult:>60
    answer <- scraping('https://raw.githubusercontent.com/haruno-group/personality/master/Big5.csv')
    point <- answer %>% dplyr::select(`12-22`:`60-999`) %>% 
      dplyr::mutate(`12-22` = as.numeric(`12-22`), `23-39` = as.numeric(`23-39`), 
                    `40-59` = as.numeric(`40-59`), `60-999` = as.numeric(`60-999`), `0` = 0)
    porder <- list(e = seq(21,33), a = seq(34,46), c = seq(47,59), 
                   n = seq(60,72), o = seq(73,85))
    agest <- c(12,23,40,60,0); ageen <- c(22,39,59,999,0)
    
    allBIGFIVE <- NULL
    for(j in 1:5) {
      pBIGFIVE <- BIGFIVE %>% dplyr::filter(dplyr::between(age, agest[j], ageen[j]))
      for(i in 1:7) {
        if(i==1) {
          bdat <- ifelse(pBIGFIVE[,i] %>% unlist==0, point[i,j] %>% unlist, 
                         pBIGFIVE[,i] %>% unlist)
          bdat <- ifelse(bdat==1, point[i+1,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==2, point[i+2,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==3, point[i+3,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==4, point[i+4,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==5, point[i+5,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==6, point[i+6,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==7, point[i+7,j] %>% unlist, bdat)
          pBIGFIVE[,i] <- ifelse(bdat==8, point[i+8,j] %>% unlist, bdat)
        } else if(i==2) {
          bdat <- ifelse(pBIGFIVE[,i] %>% unlist==0, point[i+9,j] %>% unlist, 
                         pBIGFIVE[,i] %>% unlist)
          bdat <- ifelse(bdat==1, point[i+10,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==2, point[i+11,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==3, point[i+12,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==4, point[i+13,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==5, point[i+14,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==6, point[i+15,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==7, point[i+16,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==8, point[i+17,j] %>% unlist, bdat)
          bdat <- ifelse(bdat==9, point[i+18,j] %>% unlist, bdat)
          pBIGFIVE[,i] <- ifelse(bdat==10, point[i+19,j] %>% unlist, bdat)
        } else {
          bdat <- ifelse(pBIGFIVE[,i] %>% unlist==0, 
                         point[porder[[i-2]][1],j] %>% unlist, pBIGFIVE[,i] %>% unlist)
          bdat <- ifelse(bdat==1, point[porder[[i-2]][2],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==2, point[porder[[i-2]][3],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==3, point[porder[[i-2]][4],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==4, point[porder[[i-2]][5],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==5, point[porder[[i-2]][6],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==6, point[porder[[i-2]][7],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==7, point[porder[[i-2]][8],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==8, point[porder[[i-2]][9],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==9, point[porder[[i-2]][10],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==10, point[porder[[i-2]][11],j] %>% unlist, bdat)
          bdat <- ifelse(bdat==11, point[porder[[i-2]][12],j] %>% unlist, bdat)
          pBIGFIVE[,i] <- ifelse(bdat==12, point[porder[[i-2]][13],j] %>% unlist, bdat)
        }
      }
      allBIGFIVE <- allBIGFIVE %>% dplyr::bind_rows(pBIGFIVE)
    }
    
    if(nrow(nodat) > 0) {
      nodat_sum = nodat %>% 
        dplyr::select(Big5_F, Big5_S, Big5_E, Big5_A, Big5_C, Big5_N, Big5_O, age, ID, who)
      nodat_add = allBIGFIVE %>% dplyr::bind_rows(nodat_sum) %>% dplyr::arrange(ID) %>% 
        dplyr::select(-age, -ID, -who)
      
      BIGFIVE = BIGFIVE_allitem %>% 
        dplyr::select(-Big5_F, -Big5_S, -Big5_E, -Big5_A, -Big5_C, -Big5_N, -Big5_O) %>%
        dplyr::bind_cols(nodat_add) %>% dplyr::select(-age, -ID, -who)
    } else {
      BIGFIVE = BIGFIVE_allitem %>% 
        dplyr::select(-Big5_F, -Big5_S, -Big5_E, -Big5_A, -Big5_C, -Big5_N, -Big5_O) %>% 
        dplyr::bind_cols(allBIGFIVE %>% dplyr::arrange(ID) %>% dplyr::select(-age, -ID, -who)) %>% 
        dplyr::select(-age, -ID, -who)
    }
    
    return(BIGFIVE)
  },
  
  BISBAS = function(dat) {
    score <- dat %>% dplyr::select(`BISBASQ[1]`:`BISBASQ[20]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    score = score %>% 
      dplyr::mutate(`BISBASQ[1]`= 5-`BISBASQ[1]`, `BISBASQ[18]`= 5-`BISBASQ[18]`)
    
    BIS <- get_subset(score, c(1,6,10,13,15,18,20), "BIS")
    BAS1 <- get_subset(score, c(2,7,9,17), "BAS_D")
    BAS2 <- get_subset(score, c(3,5,11,14,19), "BAS_R")
    BAS3 <- get_subset(score, c(4,8,12,16), "BAS_S")
    
    return(BIS %>% dplyr::bind_cols(BAS1, BAS2, BAS3))
  },
  
  BDI = function(dat) {
    bdilen = dat[names(dat) %>% str_detect("BDIQ")] %>% ncol()
    bdilen2 = dat[names(dat) %>% str_detect("BDI2Q")] %>% ncol()
    if(bdilen > bdilen2)
      score <- dat %>% dplyr::select(BDIQ001:BDIQ021)
    else
      score <- dat %>% dplyr::select(BDI2Q001:BDI2Q021)
    
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_replace("A", "") %>% 
        str_replace("010", "0") %>% str_replace("011", "1") %>% str_replace("012", "2") %>% 
        str_replace("013", "3") %>% str_replace("020", "0") %>% str_replace("021", "1") %>% 
        str_replace("022", "2") %>% str_replace("023", "3") %>% str_replace("030", "0") %>% 
        str_replace("031", "1") %>% str_replace("032", "2") %>% str_replace("033", "3") %>% 
        str_replace("040", "0") %>% str_replace("041", "1") %>% str_replace("042", "2") %>% 
        str_replace("043", "3") %>% str_replace("050", "0") %>% str_replace("051", "1") %>% 
        str_replace("052", "2") %>% str_replace("053", "3") %>% str_replace("060", "0") %>% 
        str_replace("061", "1") %>% str_replace("062", "2") %>% str_replace("063", "3") %>% 
        str_replace("070", "0") %>% str_replace("071", "1") %>% str_replace("072", "2") %>% 
        str_replace("073", "3") %>% str_replace("080", "0") %>% str_replace("084", "1") %>% 
        str_replace("082", "2") %>% str_replace("083", "3") %>% str_replace("090", "0") %>% 
        str_replace("091", "1") %>% str_replace("092", "2") %>% str_replace("093", "3") %>% 
        str_replace("100", "0") %>% str_replace("101", "1") %>% str_replace("102", "2") %>% 
        str_replace("103", "3") %>% str_replace("110", "0") %>% str_replace("111", "1") %>% 
        str_replace("112", "2") %>% str_replace("113", "3") %>% str_replace("120", "0") %>% 
        str_replace("121", "1") %>% str_replace("122", "2") %>% str_replace("123", "3") %>% 
        str_replace("130", "0") %>% str_replace("131", "1") %>% str_replace("132", "2") %>% 
        str_replace("133", "3") %>% str_replace("140", "0") %>% str_replace("141", "1") %>% 
        str_replace("142", "2") %>% str_replace("143", "3") %>% str_replace("150", "0") %>% 
        str_replace("151", "1") %>% str_replace("152", "2") %>% str_replace("153", "3") %>% 
        str_replace("170", "0") %>% str_replace("171", "1") %>% str_replace("172", "2") %>% 
        str_replace("173", "3") %>% str_replace("190", "0") %>% str_replace("191", "1") %>% 
        str_replace("192", "2") %>% str_replace("193", "3") %>% str_replace("200", "0") %>% 
        str_replace("201", "1") %>% str_replace("202", "2") %>% str_replace("203", "3") %>% 
        str_replace("210", "0") %>% str_replace("211", "1") %>% str_replace("212", "2") %>% 
        str_replace("213", "3") %>%
        str_replace("160", "0") %>% str_replace("161a", "1") %>% str_replace("161b", "1") %>% 
        str_replace("162a", "2") %>% str_replace("162b", "2") %>% str_replace("163a", "3") %>% 
        str_replace("163b", "3") %>% str_replace("180", "0") %>% str_replace("181a", "1") %>% 
        str_replace("181b", "1") %>% str_replace("182a", "2") %>% str_replace("182b", "2") %>% 
        str_replace("183a", "3") %>% str_replace("183b", "3") %>% as.numeric()
    }
    
    return(get_subset(score, seq(ncol(score)), "BDI"))
  },
  
  ERQ = function(dat) {
    score <- dat %>% dplyr::select(`ERQQ[1]`:`ERQQ[10]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    ERQ1 <- get_subset(score, c(1,2,4,5,7,9), "ERQ_R")
    ERQ2 <- get_subset(score, c(3,6,8,10), "ERQ_S")
    
    return(ERQ1 %>% dplyr::bind_cols(ERQ2))
  },
  
  FAD = function(dat) {
    score <- dat %>% dplyr::select(`FADQ[1]`:`FADQ[27]`)
    for(i in 1:27)  score[,i] <- score[,i] %>% unlist %>% as.numeric
    
    FAD1 <- get_subset(score, c(4,8,12,16,21,23,26), "FAD_FW")
    FAD2 <- get_subset(score, c(2,6,10,14,18,22,24), "FAD_SD")
    FAD3 <- get_subset(score, c(1,5,9,13,17), "FAD_FD")
    FAD4 <- get_subset(score, c(3,7,11,15,19,20,25,27), "FAD_UP")
    
    return(FAD1 %>% dplyr::bind_cols(FAD2, FAD3, FAD4))
  },
  
  FTND = function(dat) {
    score <- dat %>% dplyr::select(`FTNDQ00[SQ001]`:FTNDQ06)
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_replace("A1", "1") %>% str_replace("A2", "0") %>%
        str_replace("A2", "2") %>% str_replace("A3", "3") %>% as.numeric()
      score[is.na(score[,i]), i] = 0
    }
    
    score = score %>% dplyr::select(-`FTNDQ00[SQ001]`)
    return(get_subset(score, seq(ncol(score)), "FTND"))
  },
  
  HAP = function(dat) {
    score <- dat %>% dplyr::select(`HAPQ01[SQ001]`, `HAPQ02[1]`:`HAPQ02[14]`)
    
    score[,1] <- score[,1] %>% unlist %>% str_replace("A1", "10") %>% str_replace("A2", "9") %>%
      str_replace("A3", "8") %>% str_replace("A4", "7") %>% str_replace("A5", "6") %>%
      str_replace("A6", "5") %>% str_replace("A7", "4") %>% str_replace("A8", "3") %>%
      str_replace("A9", "2") %>% str_replace("A10", "1") %>% str_replace("A11", "0") %>%
      as.numeric()
    for(i in 2:15)  score[,i] <- score[,i] %>% unlist %>% as.numeric
    
    score = score %>% dplyr::select(2:15) %>%
      dplyr::mutate(`HAPQ02[1]`= 5-`HAPQ02[1]`, `HAPQ02[4]`= 5-`HAPQ02[4]`,
                    `HAPQ02[10]`= 5-`HAPQ02[10]`)
    return(get_subset(score, seq(ncol(score)), "HAP"))
  },
  
  RavenIQ = function(dat) {
    score <- dat %>% dplyr::select(`IQ001[SQ001]`:`IQ060[SQ001]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    adat <- scraping('https://raw.githubusercontent.com/haruno-group/personality/master/IQ.csv') %>% 
      dplyr::select(A1:A8)
    adat$A2 <- replace(adat$A2, which(adat$A2=="1"), 2)
    adat$A3 <- replace(adat$A3, which(adat$A3=="1"), 3)
    adat$A4 <- replace(adat$A4, which(adat$A4=="1"), 4)
    adat$A5 <- replace(adat$A5, which(adat$A5=="1"), 5)
    adat$A6 <- replace(adat$A6, which(adat$A6=="1"), 6)
    adat$A7 <- replace(adat$A7, which(adat$A7=="1"), 7)
    adat$A8 <- replace(adat$A8, which(adat$A8=="1"), 8)
    
    for(i in seq(ncol(adat)))
      adat[,i] <- adat[,i] %>% unlist %>% as.numeric()
    answer <- adat %>% colsum("Answer") %>% t %>% data.frame()
    for(i in seq(ncol(answer)))
      score[,i] <- ifelse(score[,i] %>% unlist==answer[,i], 1, 0)
    
    # IQ = get_subset(score, seq(ncol(score)), "IQ")
    return(score %>% colsum("RavenIQ"))
  },
  
  LPC = function(dat) {
    score <- dat %>% dplyr::select(`LPCQ001[SQ001]`:`LPCQ018[SQ001]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    score <- score %>%
      dplyr::mutate(`LPCQ001[SQ001]`= 9-`LPCQ001[SQ001]`, `LPCQ002[SQ001]`= 9-`LPCQ002[SQ001]`,
                    `LPCQ007[SQ001]`= 9-`LPCQ007[SQ001]`, `LPCQ011[SQ001]`= 9-`LPCQ011[SQ001]`,
                    `LPCQ014[SQ001]`= 9-`LPCQ014[SQ001]`, `LPCQ016[SQ001]`= 9-`LPCQ016[SQ001]`,
                    `LPCQ018[SQ001]`= 9-`LPCQ018[SQ001]`)
    
    return(get_subset(score, seq(ncol(score)), "LPC"))
  },
  
  MVS = function(dat) {
    score <- dat %>% dplyr::select(`MVSQ[1]`:`MVSQ[20]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    return(get_subset(score, seq(ncol(score)), "MVS"))
  },
  
  OCI = function(dat) {
    score <- dat %>% dplyr::select(`OCIRQ[1]`:`OCIRQ[18]`)
    
    for(i in 1:18)  score[,i] <- score[,i] %>% unlist %>% as.numeric
    OCIR1 <- get_subset(score, c(5,11,17), "OCIR_WA")
    OCIR2 <- get_subset(score, c(6,12,18), "OCIR_OB")
    OCIR3 <- get_subset(score, c(1,7,13), "OCIR_ST")
    OCIR4 <- get_subset(score, c(3,9,15), "OCIR_OR")
    OCIR5 <- get_subset(score, c(2,8,14), "OCIR_CH")
    OCIR6 <- get_subset(score, c(4,10,16), "OCIR_NE")
    return(OCIR1 %>% dplyr::bind_cols(OCIR2, OCIR3, OCIR4, OCIR5, OCIR6))
  },
  
  PANAS = function(dat) {
    score <- dat %>% dplyr::select(`PANAS20Q[1]`:`PANAS20Q[20]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    PANAS1 <- get_subset(score, c(2,4,7,8,10,12,13,14,17,20), "PosA")
    PANAS2 <- get_subset(score, c(1,3,5,6,9,11,15,16,18,19), "NegA")
    
    return(PANAS1 %>% dplyr::bind_cols(PANAS2))
  },
  
  PDI = function(dat) {
    score <- dat %>% dplyr::select(PDIG001Q01:PDIG040Q04)
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% 
        str_replace("Y", "1") %>% str_replace("N", "0") %>% as.numeric()
    }
    score[is.na(score)] <- 0
    
    PDI1 <- get_subset(score, c(seq(1,160,4)), "PDI_DE")
    PDI2 <- get_subset(score, c(seq(2,160,4)), "PDI_DI")
    PDI3 <- get_subset(score, c(seq(3,160,4)), "PDI_FR")
    PDI4 <- get_subset(score, c(seq(4,160,4)), "PDI_CO")
    
    return(PDI1 %>% dplyr::bind_cols(PDI2, PDI3, PDI4))
  },
  
  RA = function(dat) {
    score <- dat %>% dplyr::select(`RAQ[1]`:`RAQ[10]`)
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_replace("A1", "1") %>%
        str_replace("A2", "0") %>% as.numeric()
    }
    
    RA <- score %>% colsum("RA")
    RA$RA <- replace(RA$RA, which(RA$RA==0), 1); RA$RA <- replace(RA$RA, which(RA$RA==1), 1)
    RA$RA <- replace(RA$RA, which(RA$RA==2), 1); RA$RA <- replace(RA$RA, which(RA$RA==3), 2)
    RA$RA <- replace(RA$RA, which(RA$RA==4), 3); RA$RA <- replace(RA$RA, which(RA$RA==5), 4)
    RA$RA <- replace(RA$RA, which(RA$RA==6), 5); RA$RA <- replace(RA$RA, which(RA$RA==7), 6)
    RA$RA <- replace(RA$RA, which(RA$RA==8), 7); RA$RA <- replace(RA$RA, which(RA$RA==9), 7)
    RA$RA <- replace(RA$RA, which(RA$RA==10), 7)
    
    return(RA)
  },
  
  RISC = function(dat) {
    score <- dat %>% dplyr::select(`ResilienceQ[SQ001]`:`ResilienceQ[SQ025]`)
    return(get_subset(score, seq(25), "RISC"))
  },
  
  RRQ = function(dat) {
    score <- dat %>% dplyr::select(`RRQQ[1]`:`RRQQ[24]`)
    for(i in 1:24)  score[,i] <- score[,i] %>% unlist %>% as.numeric
    score = score %>% 
      dplyr::mutate(`RRQQ[6]`= 6-`RRQQ[6]`, `RRQQ[9]`= 6-`RRQQ[9]`,`RRQQ[10]`= 6-`RRQQ[10]`) %>%
      dplyr::mutate(`RRQQ[13]`= 6-`RRQQ[13]`, `RRQQ[14]`= 6-`RRQQ[14]`,`RRQQ[17]`= 6-`RRQQ[17]`,
                    `RRQQ[20]`= 6-`RRQQ[20]`,`RRQQ[24]`= 6-`RRQQ[24]`) 
    
    RRQ1 <- get_subset(score, c(5,2,12,3,7,8,4,11,6,1,10,9), "RRQ_RU")
    RRQ2 <- get_subset(score, c(22,24,21,23,20,18,15,16,13,17,14,19), "RRQ_RE")
    
    return(RRQ1 %>% dplyr::bind_cols(RRQ2))
  },
  
  SHS = function(dat) {
    score <- dat %>% dplyr::select(`SHSQ01[SQ001]`:`SHSQ04[SQ001]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    score = score %>% dplyr::mutate(`SHSQ04[SQ001]`= 8-`SHSQ04[SQ001]`)
    
    return(get_subset(score, seq(4), "SHS"))
  },
  
  SVO = function(dat) {
    score <- dat %>% dplyr::select(`SVOQ001`:`SVOQ008`)
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_replace("A0101", "A") %>%
        str_replace("A0102", "B") %>% str_replace("A0103", "C")
    }
    
    score$SVOQ001 = score$SVOQ001 %>% 
      str_replace('A','3') %>% str_replace('B','2') %>% str_replace('C','1') %>% as.numeric()
    score$SVOQ002 = score$SVOQ002 %>% 
      str_replace('A','1') %>% str_replace('B','2') %>% str_replace('C','3') %>% as.numeric()
    score$SVOQ003 = score$SVOQ003 %>% 
      str_replace('A','1') %>% str_replace('B','2') %>% str_replace('C','3') %>% as.numeric()
    score$SVOQ004 = score$SVOQ004 %>% 
      str_replace('A','2') %>% str_replace('B','1') %>% str_replace('C','3') %>% as.numeric()
    score$SVOQ005 = score$SVOQ005 %>% 
      str_replace('A','3') %>% str_replace('B','1') %>% str_replace('C','2') %>% as.numeric()
    score$SVOQ006 = score$SVOQ006 %>% 
      str_replace('A','1') %>% str_replace('B','3') %>% str_replace('C','2') %>% as.numeric()
    score$SVOQ007 = score$SVOQ007 %>% 
      str_replace('A','3') %>% str_replace('B','2') %>% str_replace('C','1') %>% as.numeric()
    score$SVOQ008 = score$SVOQ008 %>% 
      str_replace('A','1') %>% str_replace('B','3') %>% str_replace('C','2') %>% as.numeric()
    
    SVO <- NULL
    for(i in 1:nrow(score)) {
      svodat = score[i,] %>% dplyr::mutate(SVO_P = which(score[i,]==1) %>% length,
                                           SVO_I = which(score[i,]==2) %>% length,
                                           SVO_C = which(score[i,]==3) %>% length)
      SVO <- SVO %>% dplyr::bind_rows(svodat)
    }
    
    return(SVO %>% dplyr::select(SVO_P:SVO_C))
  },
  
  TIM = function(dat) {
    score <- dat %>% dplyr::select(`TIMQ[1]`:`TIMQ[15]`)
    for(i in seq(ncol(score))) {
      score[,i] <- score[,i] %>% unlist %>% str_replace("A1", "0") %>%
        str_replace("A2", "1") %>% as.numeric()
    }
    
    TIM <- NULL
    for(i in seq(nrow(dat))) {
      if(sum(score[i,]) != 0)  TIM <- c(TIM, which(score[i,] %>% unlist==1) %>% min)
      else                     TIM <- c(TIM, 0)
    }
    TIM <- replace(TIM, which(TIM==1), -10); TIM <- replace(TIM, which(TIM==2), 1)
    TIM <- replace(TIM, which(TIM==3), 10); TIM <- replace(TIM, which(TIM==4), 15)
    TIM <- replace(TIM, which(TIM==5), 20); TIM <- replace(TIM, which(TIM==6), 25)
    TIM <- replace(TIM, which(TIM==7), 30); TIM <- replace(TIM, which(TIM==8), 40)
    TIM <- replace(TIM, which(TIM==9), 50); TIM <- replace(TIM, which(TIM==10), 60)
    TIM <- replace(TIM, which(TIM==11), 80); TIM <- replace(TIM, which(TIM==12), 100)
    TIM <- replace(TIM, which(TIM==13), 200); TIM <- replace(TIM, which(TIM==14), 300)
    TIM <- replace(TIM, which(TIM==15), 400); TIM <- replace(TIM, which(TIM==0), 400)
    
    return(data.frame(TIM = TIM) %>% tibble::as_tibble())
  },
  
  TAS = function(dat) {
    score <- dat %>% dplyr::select(`TAS2Q[1]`:`TAS2Q[20]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    score = score %>% 
      dplyr::mutate(`TAS2Q[4]`= 6-`TAS2Q[4]`, `TAS2Q[5]`= 6-`TAS2Q[5]`, `TAS2Q[10]`= 6-`TAS2Q[10]`, 
                    `TAS2Q[18]`= 6-`TAS2Q[18]`, `TAS2Q[19]`= 6-`TAS2Q[19]`)
    
    TAS1 <- get_subset(score, c(1,3,6,7,9,13,14), "TAS_DTI")
    TAS2 <- get_subset(score, c(2,4,11,12,17), "TAS_DDF")
    TAS3 <- get_subset(score, c(5,8,10,15,16,18,19,20), "TAS_EOT")
    
    return(TAS1 %>% dplyr::bind_cols(TAS2, TAS3))
  },
  
  STAI = function(dat) {
    score <- dat %>% dplyr::select(`STAI2Q01[1]`:`STAI2Q02[20]`)
    for(i in 1:40)  score[,i] <- score[,i] %>% unlist %>% as.numeric
    
    score = score %>%
      dplyr::mutate(`STAI2Q01[1]`= 5-`STAI2Q01[1]`, `STAI2Q01[2]`= 5-`STAI2Q01[2]`,
                    `STAI2Q01[5]`= 5-`STAI2Q01[5]`, `STAI2Q01[8]`= 5-`STAI2Q01[8]`,
                    `STAI2Q01[10]`= 5-`STAI2Q01[10]`, `STAI2Q01[11]`= 5-`STAI2Q01[11]`,
                    `STAI2Q01[15]`= 5-`STAI2Q01[15]`, `STAI2Q01[16]`= 5-`STAI2Q01[16]`,
                    `STAI2Q01[19]`= 5-`STAI2Q01[19]`, `STAI2Q01[20]`= 5-`STAI2Q01[20]`) %>%
      dplyr::mutate(`STAI2Q02[1]`= 5-`STAI2Q02[1]`, `STAI2Q02[6]`= 5-`STAI2Q02[6]`,
                    `STAI2Q02[7]`= 5-`STAI2Q02[7]`, `STAI2Q02[10]`= 5-`STAI2Q02[10]`,
                    `STAI2Q02[13]`= 5-`STAI2Q02[13]`, `STAI2Q02[16]`= 5-`STAI2Q02[16]`,
                    `STAI2Q02[19]`= 5-`STAI2Q02[19]`)
    
    STAI1 <- get_subset(score, c(1:20), "STAI_S")
    STAI2 <- get_subset(score, c(21:40), "STAI_T")
    
    return(STAI1 %>% dplyr::bind_cols(STAI2))
  },
  
  SDS = function(dat) {
    score <- dat %>% dplyr::select(`SDSJQ[1]`:`SDSJQ[20]`)
    for(i in 1:20)  score[,i] <- score[,i] %>% unlist %>% as.numeric
    
    score <- score %>%
      dplyr::mutate(`SDSJQ[2]`= 5-`SDSJQ[2]`, `SDSJQ[5]`= 5-`SDSJQ[5]`,
                    `SDSJQ[6]`= 5-`SDSJQ[6]`, `SDSJQ[11]`= 5-`SDSJQ[11]`,
                    `SDSJQ[12]`= 5-`SDSJQ[12]`, `SDSJQ[14]`= 5-`SDSJQ[14]`,
                    `SDSJQ[16]`= 5-`SDSJQ[16]`, `SDSJQ[17]`= 5-`SDSJQ[17]`,
                    `SDSJQ[18]`= 5-`SDSJQ[18]`, `SDSJQ[20]`= 5-`SDSJQ[20]`)
    
    return(get_subset(score, seq(ncol(score)), "SDS"))
  },
  
  SES = function(dat) {
    # dat[names(dat) %>% str_detect("SESQ")]
    # dat[names(dat) %>% str_detect("SES2Q")]
    dat$SESQ01 <- dat$SESQ01 %>% str_remove("A") %>% as.integer()
    dat$SESQ02 <- dat$SESQ02 %>% str_remove("A") %>% as.integer()
    dat$SESQ03 <- dat$SESQ03 %>% str_remove("A") %>% as.integer()
    
    SES <- data.frame(SES_self    = dat$SESQ01,
                      SES_parents = dplyr::if_else(dat$SESQ02 > dat$SESQ03,dat$SESQ02,dat$SESQ03),
                      SES_salary  = dat$SES2Q01, 
                      SES_job     = dat$SES2Q02,
                      SES_subj    = dat$`SES2Q03[SQ001]`) %>% tibble::as_tibble()
    
    return(SES)
  },
  
  IMC = function(dat) {
    score <- dat %>% dplyr::select(`IMCQ1[1]`:`IMCQ1[3]`)
    score[is.na(score)] <- 100
    score <- round(score / 100) %>% tibble::as_tibble()
    
    score2 <- dat %>% dplyr::select(IMCQ2) %>% dplyr::rename("Webfreq"=IMCQ2)
    score2[is.na(score2)] <- 0
    
    return(score %>% colsum("IMC") %>% dplyr::bind_cols(score2))
    # dat %>% dplyr::select(`IMCEVAL[Total]`)
  },
  
  ARS = function(dat) {
    score <- dat %>% dplyr::select(`ARSQ[1]`:`ARSQ[33]`)
    score[is.na(score)] <- 0
    score = score %>% 
      dplyr::mutate(`ARSQ[3]`= 6-`ARSQ[3]`, `ARSQ[21]`= 6-`ARSQ[21]`, `ARSQ[12]`= 6-`ARSQ[12]`,
                    `ARSQ[27]`= 6-`ARSQ[27]`, `ARSQ[4]`= 6-`ARSQ[4]`)
    
    arsdat = data.frame(Incon1 = abs(score$`ARSQ[1]` - score$`ARSQ[14]`), 
                        Incon2 = abs(score$`ARSQ[2]` - score$`ARSQ[19]`),
                        Incon3 = abs(score$`ARSQ[17]` - score$`ARSQ[33]`), 
                        Incon4 = abs(score$`ARSQ[13]` - score$`ARSQ[30]`),
                        Incon5 = abs(score$`ARSQ[16]` - score$`ARSQ[32]`), 
                        Incon6 = abs(score$`ARSQ[8]` - score$`ARSQ[25]`),
                        Incon7 = abs(score$`ARSQ[5]` - score$`ARSQ[22]`), 
                        Incon8 = abs(score$`ARSQ[6]` - score$`ARSQ[23]`),
                        Incon9 = abs(score$`ARSQ[9]` - score$`ARSQ[26]`), 
                        Incon10 = abs(score$`ARSQ[11]` - score$`ARSQ[28]`),
                        Incon11 = abs(score$`ARSQ[14]` - score$`ARSQ[31]`))
    ARS1 = get_subset(arsdat, seq(11), "ARS_incon")
    ARS2 = get_subset(score, c(3,21,24,7,10,12,20,29,27,4,15), "ARS_infreq")
    
    return(ARS1 %>% dplyr::bind_cols(ARS2) %>% tibble::as_tibble())
  },
  
  SNS = function(dat) {
    score <- dat %>% dplyr::select(`SNSG001Q01[SQ001]`:`SNSG003Q01[SQ002]`, -SNSG002Q01)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    names(score) = c("FB", "Twi", "Insta", "LINE", "SG_freq", "SG_time", "SG_buy", 
                     "SNS_freq", "Real_freq")
    return(score)
  },
  
  NET = function(dat) {
    score <- dat %>% dplyr::select(`NETQ[NETSQ001]`:`NETQ[NETSQ0020]`)
    
    NET1 <- get_subset(score, c(20,15,3,19,18,11,12,13,10,4,14), "NET_emocog")
    NET2 <- get_subset(score, c(2,1,16,6,5,9,7), "NET_loss")
    NETall <- get_subset(score, seq(20), "NET_all")
    
    return(NET1 %>% dplyr::bind_cols(NET2, NETall))
  },
  
  UCLA = function(dat) {
    score <- dat %>% dplyr::select(`UCLAQ[1]`:`UCLAQ[20]`)
    score <- score %>% 
      dplyr::mutate(`UCLAQ[1]`= 5-`UCLAQ[1]`, `UCLAQ[5]`= 5-`UCLAQ[5]`, 
                    `UCLAQ[6]`= 5-`UCLAQ[6]`, `UCLAQ[9]`= 5-`UCLAQ[9]`, 
                    `UCLAQ[10]`= 5-`UCLAQ[10]`, `UCLAQ[15]`= 5-`UCLAQ[15]`, 
                    `UCLAQ[16]`= 5-`UCLAQ[16]`, `UCLAQ[19]`= 5-`UCLAQ[19]`, 
                    `UCLAQ[20]`= 5-`UCLAQ[20]`)
    
    return(get_subset(score, seq(20), "UCLA"))
  },
  
  LSNS = function(dat) {
    score <- dat %>% dplyr::select(`LSNS6G001Q01[SQ001]`:`LSNS6G001Q02[SQ006]`)
    for(i in seq(ncol(score)))
      score[,i] <- score[,i] %>% unlist %>% str_remove("A") %>% as.numeric()
    
    return(get_subset(score, seq(6), "LSNS"))
  },
  
  time = function(dat, thre=60) {
    stdat <- data.frame(time = dat$startdate) %>% tibble::as_tibble() %>%
      tidyr::separate(col = time, into = c("ymd", "time"), sep = " ")
    endat <- data.frame(time = dat$datestamp) %>% tibble::as_tibble() %>%
      tidyr::separate(col = time, into = c("ymd", "time"), sep = " ")
    
    day <- data.frame(day = as.difftime(endat$ymd, format="%Y-%m-%d", units="mins") -
                        as.difftime(stdat$ymd, format="%Y-%m-%d", units="mins") )
    time <- data.frame(time = as.difftime(endat$time, units="mins") - 
                         as.difftime(stdat$time, units="mins") %>% round) %>% tibble::as_tibble()
    
    short <- data.frame(duration = (day+time), id = dat$id) %>% 
      dplyr::filter(time <= thre, time >= 0) %>% dplyr::mutate(time="short")
    long <- data.frame(duration = (day+time), id = dat$id) %>% 
      dplyr::filter(time > thre | time < 0) %>% dplyr::mutate(time="long")
    sortdat <- long %>% dplyr::bind_rows(short) %>% tibble::as_tibble() %>% 
      dplyr::arrange(id) %>% dplyr::select(-id)
    names(sortdat)[1] <- "duration"
    
    return(sortdat)
  })
)
