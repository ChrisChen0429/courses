Chapter 8: Cross-Classified Multilevel Models
(ch8crossclass1.sav, ch8crossclass2.sav)
**************************************************
 
Table 8.7 (ch8crossclass1.sav)

MIXED CUM_GPR
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
    /FIXED= | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(nschcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(campus) COVTYPE(ID).

**********

Adding a Set of Level-1 and Level-2 Predictors
Model 1.1: Tables 8.8, 8.9 (ch8crossclass1.sav)
  
MIXED CUM_GPR WITH gmfouryear gmlowSES_mean gmlowses gmfemale
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=gmfouryear gmlowSES_mean gmlowses gmfemale | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(nschcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(campus) COVTYPE(ID).

**********
Investigating a Random Slope
Model 1.2: Table 8.10 (ch8crossclass1.sav)

MIXED CUM_GPR WITH gmfouryear gmlowSES_mean gmlowses gmfemale
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=gmfouryear gmlowSES_mean gmlowses gmfemale | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT gmfemale | SUBJECT(nschcode) COVTYPE(DIAG)
  /RANDOM=INTERCEPT gmfemale | SUBJECT(campus) COVTYPE(DIAG).

**********
Explaining Variation Between Variables
Model 1.3: Tables 8.11, 8.12 (ch8crossclass1.sav)

MIXED CUM_GPR WITH gmlowSES_mean gmlowses gmfemale
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=gmlowSES_mean gmlowses gmfemale gmlowSES_mean*gmfemale | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT gmfemale | SUBJECT(nschcode) COVTYPE(DIAG)
  /RANDOM=INTERCEPT | SUBJECT(campus) COVTYPE(ID).
 
**********
Intercept-only Model 
Model 2.1: Tables 8.14, 8.15, 8.16  (ch8crossclass2.sav)

MIXED math2
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=| SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach2id) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach1id) COVTYPE(ID).

**********
Defining the Cross-Classified Model With Previous Achievement
Model 2.2: Tables 8.17, 8.18 (ch8crossclass2.sav) 

MIXED math2 WITH Zmath1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=Zmath1 | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach2id) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach1id) COVTYPE(ID).

********** 

Adding Teacher Effectiveness and a Student Background Control 
Model 2.3: Tables 8.21, 8.22  (ch8crossclass2.sav) 

MIXED math2 WITH effmath2 effmath1 Zmath1 lowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=effmath2 effmath1 Zmath1 lowses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach2id) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach1id) COVTYPE(ID).

**********
School-Level Predictor and Random Slope 
 Model 2.4: Table 8.23 (ch8crossclass2.sav) 

MIXED math2 WITH effmath2 effmath1 Zmath1 lowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=effmath2 effmath1 Zmath1 lowses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT effmath2 effmath1 | SUBJECT(schcode) COVTYPE(DIAG)
  /RANDOM=INTERCEPT | SUBJECT(teach2id) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach1id) COVTYPE(ID).

*********
Level-3 Differences Between Institutions 
Model 2.5: Tables 8.24, 8.25 (ch8crossclass2.sav)

MIXED math2 WITH schqual effmath2 effmath1 Zmath1 lowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
    /FIXED=schqual effmath2 effmath1 Zmath1 lowses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT effmath2 | SUBJECT(schcode) COVTYPE(DIAG)
  /RANDOM=INTERCEPT | SUBJECT(teach2id) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach1id) COVTYPE(ID).

**********
Adding a Level-3 Cross-Level Interaction  
Model 2.6: Tables 8.26, 8.27 (ch8crossclass2.sav)

MIXED math2 WITH schqual effmath2 effmath1 Zmath1 lowses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
   /FIXED=schqual effmath2 effmath1 Zmath1 lowses effmath2*schqual | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT effmath2 | SUBJECT(schcode) COVTYPE(DIAG)
  /RANDOM=INTERCEPT | SUBJECT(teach2id) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(teach1id) COVTYPE(ID).

**********