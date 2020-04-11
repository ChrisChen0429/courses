library(readxl)
library(dplyr)
contingency <- read_excel("contingency.xlsx")

contingency %>% filter(Urban==1,Suburban==0,Public==1,Catholic==0,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==1,Suburban==0,Public==1,Catholic==0,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))

contingency %>% filter(Urban==0,Suburban==1,Public==1,Catholic==0,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==0,Suburban==1,Public==1,Catholic==0,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))

contingency %>% filter(Urban==0,Suburban==0,Public==1,Catholic==0,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==0,Suburban==0,Public==1,Catholic==0,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))



contingency %>% filter(Urban==1,Suburban==0,Public==0,Catholic==1,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==1,Suburban==0,Public==0,Catholic==1,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))

contingency %>% filter(Urban==0,Suburban==1,Public==0,Catholic==1,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==0,Suburban==1,Public==0,Catholic==1,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))

contingency %>% filter(Urban==0,Suburban==0,Public==0,Catholic==1,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==0,Suburban==0,Public==0,Catholic==1,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))


contingency %>% filter(Urban==1,Suburban==0,Public==0,Catholic==0,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==1,Suburban==0,Public==0,Catholic==0,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))

contingency %>% filter(Urban==0,Suburban==1,Public==0,Catholic==0,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==0,Suburban==1,Public==0,Catholic==0,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))

contingency %>% filter(Urban==0,Suburban==0,Public==0,Catholic==0,BYSEX==0) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
contingency %>% filter(Urban==0,Suburban==0,Public==0,Catholic==0,BYSEX==1) %>% summarise(std = sd(F1TXMSTD),avg = mean(F1TXMSTD))
