Chapter 4:Three-Level Univariate Regression Models

**********
Model 1 (Null): Table 4.2 (ch4threelevelURM.sav)

MIXED math
 /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=| SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(schcode*Rteachid) COVTYPE(ID).

******** 
Defining Predictors at Each Level
Model 2:  Table 4.4 (ch4threelevelURM.sav)

MIXED math WITH gmschlowSES_mean gmaggtcheffect gmteacheffect gmclasslowses_mean gmlowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=gmschlowSES_mean gmaggtcheffect gmteacheffect gmclasslowses_mean gmlowses | SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(schcode*Rteachid) COVTYPE(ID).

********
Group-Mean Centering
Model 3: Tables 4.5, 4.6  (ch4threelevelURM.sav)

MIXED math WITH gmschlowSES_mean gmaggtcheffect groupteacheffect groupclasslowses_mean grouplowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=gmschlowSES_mean gmaggtcheffect groupteacheffect groupclasslowses_mean grouplowses | 
    SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(schcode*Rteachid) COVTYPE(ID).

********
Does the Slope Vary Randomly Across Schools?
Model 4:  Tables 4.7, 4.8, 4.13 (ch4threelevelURM.sav)

MIXED math WITH gmschlowSES_mean gmaggtcheffect gmteacheffect gmclasslowses_mean gmlowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=gmschlowSES_mean gmaggtcheffect gmteacheffect gmclasslowses_mean gmlowses | SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT gmteacheffect | SUBJECT(schcode) COVTYPE(UN)
  /RANDOM=INTERCEPT | SUBJECT(schcode*Rteachid) COVTYPE(ID).

********
Preliminary Investigation of the Interaction
Test Interaction Model A:  Table 4.9 (ch4threelevelURM.sav)

MIXED math WITH teacheffect classlowses_mean 
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=teacheffect classlowses_mean teacheffect*classlowses_mean   | SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(schcode*Rteachid) COVTYPE(ID). 


**Test Interaction Model B(Grand Mean Centered Variables):  Table 4.10 (ch4threelevelURM.sav)

MIXED math WITH gmteacheffect gmclasslowses_mean 
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=gmteacheffect gmclasslowses_mean gmteacheffect*gmclasslowses_mean   | SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(schcode*Rteachid) COVTYPE(ID). 

********
Examining a Level 2 Interaction
Model 5: Tables 4.11, 4.12, 4.14 (ch4threelevelURM.sav)

MIXED math WITH gmschlowSES_mean gmaggtcheffect gmteacheffect gmclasslowses_mean gmlowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=gmschlowSES_mean gmaggtcheffect gmteacheffect gmclasslowses_mean 
    gmclasslowses_mean*gmteacheffect gmlowses | SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT gmteacheffect | SUBJECT(schcode) COVTYPE(UN)
  /RANDOM=INTERCEPT | SUBJECT(schcode*Rteachid) COVTYPE(ID).

********** 