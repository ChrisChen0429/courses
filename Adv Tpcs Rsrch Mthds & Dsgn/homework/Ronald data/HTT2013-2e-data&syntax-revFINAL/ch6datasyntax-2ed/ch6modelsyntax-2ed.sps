Chapter 6: Methods for Examining Organizational-Level Change
(ch6graduationdata.sav, ch6RD-1data.sav)

Model 1.1 (Null): Tables 6.2, 6.3 (ch6graduationdata.sav)

MIXED gradproportion
********** 
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=| SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(stateid) COVTYPE(ID)
  /RANDOM=INTERCEPT | SUBJECT(rid*stateid) COVTYPE(ID)
  /REPEATED=time | SUBJECT(rid*stateid) COVTYPE(AR1).

********** 
Adding Growth Rates
 Model 1.2: Tables 6.6, 6.7  (ch6graduationdata.sav)

MIXED gradproportion WITH time1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=time1 | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time1 | SUBJECT(stateid) COVTYPE(DIAG)
  /RANDOM=INTERCEPT time1 | SUBJECT(rid*stateid) COVTYPE(DIAG)
  /REPEATED=time | SUBJECT(rid*stateid) COVTYPE(AR1).

********** 
Adding Time-Varying Covariates   
 Model 1.3: Tables 6.8, 6.9   (ch6graduationdata.sav)

MIXED gradproportion WITH percentFinAid tuition time1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=percentFinAid tuition time1 | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time1 | SUBJECT(stateid) COVTYPE(DIAG)
  /RANDOM=INTERCEPT time1 | SUBJECT(rid*stateid) COVTYPE(DIAG)
  /REPEATED=time | SUBJECT(rid*stateid) COVTYPE(AR1).

********** 
Explaining Differences in Growth Trajectories Between Institutions  
 Model 1.4: Tables 6.10, 6.11 (ch6graduationdata.sav)

MIXED gradproportion WITH aveFamilyshare aveRetention mathselect percentFTfaculty percentFinAid 
    tuition time1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=aveFamilyshare aveRetention mathselect percentFTfaculty percentFinAid tuition time1 
    time1*mathselect time1*percentFTfaculty | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time1 | SUBJECT(stateid) COVTYPE(DIAG)
  /RANDOM=INTERCEPT time1 | SUBJECT(rid*stateid) COVTYPE(DIAG)
  /REPEATED=time | SUBJECT(rid*stateid) COVTYPE(AR1).

********** 
Adding a Model to Examine Growth Rates at Level 3 
Model 1.5: Tables 6.12, 6.13 (ch6graduationdata.sav)

MIXED gradproportion WITH aveFamilyshare aveRetention mathselect percentFTfaculty percentFinAid 
    tuition time1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=aveFamilyshare aveRetention mathselect percentFTfaculty percentFinAid tuition time1 
    time1*aveFamilyshare time1*aveRetention time1*mathselect time1*percentFTfaculty | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time1 | SUBJECT(stateid) COVTYPE(DIAG)
  /RANDOM=INTERCEPT time1 | SUBJECT(rid*stateid) COVTYPE(DIAG)
  /REPEATED=time | SUBJECT(rid*stateid) COVTYPE(AR1).

********** 
Regression Discontinuity Models to Explain Learning Differences
Model 2.1: Tables 6.16, 6.17 (ch6RD-1data.sav)

MIXED nmath WITH npretest treatment
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=npretest treatment | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT treatment | SUBJECT(teachcode) COVTYPE(UN).

********** 
Adding Explanatory Variables at Level 2
Model 2.2: Table 6.18  (ch6RD-1data.sav)

MIXED nmath WITH teachqual classcomp npretest treatment
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=teachqual classcomp npretest treatment teachqual*treatment classcomp*treatment | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT treatment | SUBJECT(teachcode) COVTYPE(UN).

********** 
Establishing the Pre-Policy and Policy Trends
Model 3.1: Tables 6.20, 6.21, 6.22  (ch6RD-2data.sav)

 MIXED freshadmit WITH implement0 implement1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=implement0 implement1 | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schid) COVTYPE(ID)
  /REPEATED=Index1 | SUBJECT(schid) COVTYPE(DIAG).

********** 
Final Model with Covariates Added
Model 3.2: Tables 6.23, 6.24 (ch6RD-2data.sav)

MIXED freshadmit WITH private prestige implement0 implement1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=private prestige implement0 implement1 implement0*private implement0*prestige 
    implement1*private implement1*prestige | SSTYPE(3)
  /METHOD=REML
  /PRINT=COVB  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(schid) COVTYPE(ID)
  /REPEATED=Index1 | SUBJECT(schid) COVTYPE(DIAG).



