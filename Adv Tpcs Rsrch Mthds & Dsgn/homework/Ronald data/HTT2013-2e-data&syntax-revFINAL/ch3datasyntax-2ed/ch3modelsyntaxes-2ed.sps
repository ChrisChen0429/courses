Chapter 3: Defining a Basic Two-Level Multilevel Regression Model (ch3multilevel.sav)

**********
Table 3.2, 3.3 (ch3multilevel.sav)

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT math
  /METHOD=ENTER ses.

**********
Step 1: Examining Variance Components Using the Null Model
Model 1 (Null): Tables 3.4, 3.5, 3.6, 3.7 (ch3multilevel.sav)

MIXED math
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=| SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(VC).

**********
Step 2: Building the Individual-Level (or Level 1) Random Intercept Model 
Model 2: Tables 3.8, 3.9, 3.10, 3.11, 3.12 (ch3multilevel.sav)
 
MIXED math WITH ses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=ses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(VC).

**********
Step 3: Building the Group-Level (or Level 2) Random Intercept Model
Model 3:  Tables 3.13, 3.14 (ch3multilevel.sav)

MIXED math BY public WITH ses_mean pro4yrc ses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=public ses_mean pro4yrc ses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(VC).

**********
Treat "Public" Variable as a Covariate
Model 3A: Tables 3.15, 3.16 (ch3multilevel.sav)

MIXED math WITH public ses_mean pro4yrc ses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=public ses_mean pro4yrc ses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(VC).

**********
Step 4: Adding a Randomly Varying Slope (the Random Slope and Intercept Model) 
Model 4: Tables 3.17, 3.18, 3.19 (ch3multilevel.sav)

 MIXED math WITH public ses_mean pro4yrc ses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=public ses_mean pro4yrc ses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT ses | SUBJECT(schcode) COVTYPE(VC).

********** 
Step 5: Explaining Variability in the Random Slope (More Complex Random Slopes and Intercept Models)
Table 3.20 (ch3multilevel.sav)

MIXED math WITH ses_mean ses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=ses_mean ses ses*ses_mean | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schcode) COVTYPE(ID).

**********
Step 5: Explaining Variability in the Random Slope (More Complex Random Slopes and Intercept Models)
Model 5: Tables 3.21, 3.22, 3.23 (ch3multilevel.sav)

MIXED math WITH public ses_mean pro4yrc ses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=public ses_mean pro4yrc ses ses_mean*ses pro4yrc*ses public*ses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT ses | SUBJECT(schcode) COVTYPE(VC).
 
**********
Removed Nonsignificant Interactions (ses_men*ses, pro4yrc*ses)
Model 5A: Table 3.24 (ch3multilevel.sav)

MIXED math WITH public ses_mean pro4yrc ses
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED=public ses_mean pro4yrc ses public*ses | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT ses | SUBJECT(schcode) COVTYPE(VC).

**********


 

