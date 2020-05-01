library(dplyr)
library(multiplex)

load("ecpp_pu_pert.rdata")



# transforme the indicator
indicator <- ecpp_pu_pert %>% select(dcloa,dcost,drely,dlern,dchil,
                                     dhrop,dnbgrp,drtweb,drecfam,drelor)
indicator[indicator==-1] = NA
indicator[indicator<=2] = 0
indicator[indicator>2] = 1
indicator$ncnow  <- ecpp_pu_pert$ncnow  -1
indicator$rcnow  <- ecpp_pu_pert$rcnow  -1
indicator$cpnnowx  <- ecpp_pu_pert$cpnnowx  -1

# transforme the context data
context <- ecpp_pu_pert %>% select(hdintdis,hdspeechx,hddistrbx,hddeafimx,hdblindx,hdorthox,hdautismx,hdpddx,hdaddx,hdlearnx,hddelayx,hdtrbrain,hdotherx,
                                   hhtotalxx,useintrnt,ttlhhinc,csex)
context[context==-1] = NA
context$DISB <- as.numeric(context$hdintdis == 1 |
                        context$hdspeechx == 1 | 
                        context$hddistrbx == 1 | 
                        context$hddeafimx == 1 | 
                        context$hdblindx == 1 |
                        context$hdorthox == 1 | 
                       context$hdautismx == 1 |
                        context$hdpddx == 1 |
                        context$hdaddx == 1 |
                        context$hdlearnx == 1 |
                        context$hddelayx == 1 | 
                        context$hdtrbrain == 1 |
                        context$hdotherx == 1)
context$hhtotalxx[context$hhtotalxx<=4] = 0
context$hhtotalxx[context$hhtotalxx>4] = 1
context$useintrnt[context$useintrnt<=2] = 1
context$useintrnt[context$useintrnt>2] = 0
context$ttlhhinc[context$ttlhhinc<=5] =0
context$ttlhhinc[context$ttlhhinc>5] = 1
context$cwhite <- -1 * (ecpp_pu_pert$cwhite-2)
context$csex <- -1 * (ecpp_pu_pert$csex-2)
context <- context %>% select(DISB,hhtotalxx,useintrnt,ttlhhinc,csex,cwhite)



# transform the outcome varaible
outcome <- ecpp_pu_pert %>% select(dpcolor, dpletter, dpcount, dpname)
outcome[outcome==-1] = NA
outcome$dpcolor[outcome$dpcolor<=2] = 0
outcome$dpcolor[outcome$dpcolor>2] = 1
outcome$dpletter[outcome$dpletter<=2] = 0
outcome$dpletter[outcome$dpletter>2] = 1
outcome$dpcount[outcome$dpcount<=3] = 0
outcome$dpcount[outcome$dpcount>3] = 1
outcome$dpname[outcome$dpname==1] = 0
outcome$dpname[outcome$dpname==2] = 1

data <- cbind(indicator,context,outcome)
data[is.na(data)] <-999


write.dat(data,'data.dat')



exp(c(  0.738,
        -0.726,
        0.829,
        -1.041,
        -0.054,
        0.275  )) / (1+exp(c(  0.738,
                               -0.726,
                               0.829,
                               -1.041,
                               -0.054,
                               0.275  )))
