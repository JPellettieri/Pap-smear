CMT_maker <- function(df,a,b,c,d,e,f,g){
  
  NBI_1 <- df[[a]] #bhcv01
  NBI_1[NBI_1 == 1 | NBI_1 == 3] <- 0
  NBI_1[!(NBI_1 == 0)] <- 1                       
  # NBI_1 tiene 28738 datos sin carencias y 486 datos con carencias
  
  NBI_2 <- df[[b]] #bhcv10
  NBI_2[is.na(NBI_2)] <- 99
  NBI_2[NBI_2 == 1] <- 0
  NBI_2[!(NBI_2 == 0)] <- 1 
  # NBI_2 tiene 29102 datos sin carencias y 122 con carencias 
  
  miembros <- df[[c]] #cant_componentes
  ambientes <- df[[d]] #bhcv02
  NBI_3 <- miembros/ambientes
  NBI_3 [NBI_3 < 3] <- 0
  NBI_3 [NBI_3 >= 3] <- 1
  # NBI_3 tiene 28335 datos sin carencias y 889 con carencias
  
  NBI_4 <- df[[e]] # bhcv08
  NBI_4[NBI_4 == 1] <- 0
  NBI_4[!(NBI_4 == 0)] <- 1
  
  NBI_5 <- df[[f]] # bhcv03
  NBI_5[NBI_5 == 1] <- 0
  NBI_5[!(NBI_5 == 0)] <- 1
  
  NBI_6 <- df[[g]] # bhcv05
  NBI_6[NBI_6 == 1] <- 0
  NBI_6[NBI_6 == 2] <- 1
  NBI_6[NBI_6 == 9] <- NA
  
  CMT <- NBI_1 + NBI_2 + NBI_3 + NBI_4 + NBI_5 + NBI_6
  CMT[!(CMT == 0)] <- 1
  CMT
}

df_mod$CMT <- CMT_maker(df_mod, 'bhcv01', 'bhcv10', 'cant_componentes', 'bhcv02', 'bhcv08', 'bhcv03', 'bhcv05')