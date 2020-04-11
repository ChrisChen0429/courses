Chapter 5: Examining Individual Change With Repeated Measures Data
(ch5growthdata-vertical.sav, ch5growthdata-horizontal.sav, ch5experimentaldesigndata.sav)

**********
Graphing the Linear and Nonlinear Growth Trajectories With IBM SPSS Menu Commands
Figures 5.2, 5.3, 5.4 (Select Cases and Generate Graph) (ch5growthdata-vertical.sav )

**Select Subset of Individuals

USE ALL.
COMPUTE filter_$=(id < 18).
VARIABLE LABELS filter_$ 'id < 18 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


**Figures. 5.2, 5.3, 5.4 (ch5growthdata-vertical.sav )
(variations achieved by changing the Chart Editor's Properties options)

GRAPH
  /SCATTERPLOT(BIVAR)=time WITH test BY id
  /MISSING=LISTWISE.

**********
Coding Time Interval Variables (time to quadtime) With IBM SPSS Menu Commands
(ch5growthdata-vertical.sav )

RECODE time (0=0) (1=1) (2=4) INTO quadtime.
EXECUTE.

**********
Coding Time Interval Variables (time to orthtime, orthquad) With IBM SPSS Menu Commands
(ch5growthdata-vertical.sav )

**orthtime

RECODE time (0=1) (1=0) (2=1) INTO orthtime.
EXECUTE.

**orthquad

RECODE time (0=1) (1=-2) (2=1) INTO orthquad.
EXECUTE.
 
**********
Model with No Predictors
Model 1.1: Table 5.4 (ch5growthdata-vertical.sav)

MIXED test WITH time quadtime
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=| SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(ID)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(ID).

**********
What is the Shape of the Trajectory?
Model 1.1A: Tables 5.5, 5.6 (ch5growthdata-vertical.sav)

MIXED test WITH time quadtime
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=time quadtime | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(ID)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(ID). 

**********
Does The Time-Related Slope Vary Across Groups?
Model 1.1B: Tables 5.7, 5.8, 5.9 (ch5growthdata-vertical.sav)

MIXED test WITH time quadtime
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=time quadtime | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(ID).

**********
Examining Orthogonal Components
Model 1.2: Tables 5.10, 5.12 (ch5growthdata-vertical.sav)

MIXED test WITH orthtime orthquad
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=orthtime orthquad | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT orthtime | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(ID).


**********
Investigating Other Level 1 Covariance Structures
Repeated Measures ANOVA Tests of Within-Subjects Contrasts 
Table 5.11: (ch5growthdata-horizontal.sav)

GLM test1 test2 test3
  /WSFACTOR=time 3 Polynomial 
  /MEASURE=test 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(time) 
  /PRINT=OPOWER HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=time.

**********
Investigating Other Level 1 Covariance Structures
Model 1: Table 5.13 Identity covariance matrix, Level 1, Unstructured covariancecmatrix, Level 2 (ch5growthdata-vertical.sav)

MIXED test WITH orthtime orthquad
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=orthtime orthquad | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT orthtime | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(ID).

**
Investigating Other Level 1 Covariance Structures
Model 2: Table 5.13  Diagonal covariance matrix, Level 1, Diagonal covariance matrix, Level 2  (ch5growthdata-vertical.sav)

MIXED test WITH orthtime orthquad
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=orthtime orthquad | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT orthtime | SUBJECT(id) COVTYPE(DIAG)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(DIAG).

**
Investigating Other Level 1 Covariance Structures
Model 3: Table 5.13  Diagonal covariance matrix, Level 1, Unstructured covariance matrix, Level 2  (ch5growthdata-vertical.sav)

MIXED test WITH orthtime orthquad
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=orthtime orthquad | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT orthtime | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(DIAG).

**
Investigating Other Level 1 Covariance Structures
Model 4: Table 5.13  Autoregressive covariance matrix, Level 1, Diagonal covariance matrix, Level 2  (ch5growthdata-vertical.sav)

MIXED test WITH orthtime orthquad
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=orthtime orthquad | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT orthtime | SUBJECT(id) COVTYPE(DIAG)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(AR1).

**********
Adding the Between-Subjects Predictors
Model 1.3: Tables 5.15, 5.16, 5.17 (ch5growthdata-vertical.sav)

MIXED test WITH orthtime orthquad ses effective
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED = ses effective orthtime ses*orthtime effective*orthtime orthquad | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT orthtime | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(DIAG).

**********
Tests of Within-Subjects Contrasts (repeated measures ANOVA, Tests of Within-Subject Contrasts) 
Table 5.18  (ch5growthdata-horizontal.sav)

GLM test1 test2 test3 BY effective WITH ses
  /WSFACTOR=time 3 Polynomial 
  /MEASURE=test 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(OVERALL) WITH(ses=MEAN)
  /EMMEANS=TABLES(time) WITH(ses=MEAN)
  /PRINT=OPOWER HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=time 
  /DESIGN=ses effective.

*************************************************
Table 5.20 summary - no syntax for Appendix

*************************************************
**********
Graphing the Growth Rate Trajectories With SPSS Menu Commands
Figure 5.8: Growth Rate Trajectories by Teacher Effectiveness 

GRAPH
  /LINE(MULTIPLE)=MEAN(test) BY time BY effective.

**********
Examining Growth Using an Alternative Specification of the Time-Related Variable
Coding Time Interval Variables

**Recoding time to timenonlin1 (ch5growthdata-vertical.sav)

RECODE time (0=0) (1=0.5) (2=1) INTO timenonlin1.
EXECUTE.

**Recoding time to timenonlin2 (ch5growthdata-vertical.sav)

RECODE time (0=0) (1=0.6) (2=1) INTO timenonlin2.
EXECUTE.

**Recoding time to timenonlin3 (ch5growthdata-vertical.sav)

RECODE time (0=0) (1=7) (2=1) INTO timenonlin3.
EXECUTE.

**Recoding time to timenonlin (ch5growthdata-vertical.sav)

RECODE time (0=0) (1=.53) (2=1) INTO timenonlin.
EXECUTE.

**Table 5.20 (ch5growthdata-vertical.sav)

MIXED test WITH timenonlin1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED = timenonlin1 | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT timenonlin1 | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(DIAG).


**********
Estimating the Final Time-Related Model 
Model 2.1: (ch5growthdata-vertical.sav)

MIXED test WITH timenonlin
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED = timenonlin | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT timenonlin | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(DIAG).

**********
Adding the Two Predictors
Model 2.2: Tables 5.21, 5.22 (ch5growthdata-vertical.sav)

MIXED test WITH timenonlin ses effective
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED = ses effective timenonlin ses*timenonlin effective*timenonlin | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT timenonlin | SUBJECT(id) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(id) COVTYPE(DIAG).  


**********
An Example Experimental Design 
Tables 5.24, 5.25 (ch5experimentaldesigndata.sav)

MIXED math WITH time treatment 
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treatment time time*treatment  | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time | SUBJECT(id) COVTYPE(UNR)
  /REPEATED=time | SUBJECT(id) COVTYPE(DIAG).

*********
Table 5.26 (ch5experimentaldesigndata.sav)

MIXED math By time WITH treatment 
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treatment time time*treatment  | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(UNR)
  /REPEATED=time | SUBJECT(id) COVTYPE(DIAG).

*********
Table 5.27 (ch5experimentaldesigndata.sav)

MIXED math WITH timenonlin treatment
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treatment timenonlin timenonlin*treatment  | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT timenonlin | SUBJECT(id) COVTYPE(VC)
  /REPEATED=time | SUBJECT(id) COVTYPE(DIAG).

**********
 
