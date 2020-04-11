Ch7: Multivariate Multilevel Models
(ch7latentconstructs.sav, ch7worklifeorg.sav,ch7achievement.sav, ch7PGachievement.sav)

**********
Table 7.2 (ch7latentconstructs.sav)

DESCRIPTIVES VARIABLES=W1varied W2value W3team P1assess P2progress P3evstand
  /STATISTICS=MEAN STDDEV KURTOSIS SKEWNESS.

**********
Table 7.3 (ch7latentconstructs.sav)

FACTOR
  /VARIABLES W1varied W2value W3team P1assess P2progress P3evstand
  /MISSING LISTWISE 
  /ANALYSIS W1varied W2value W3team P1assess P2progress P3evstand
  /PRINT INITIAL EXTRACTION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PAF
  /ROTATION NOROTATE
  /METHOD=CORRELATION.

**********
Table 7.4 (ch7latentconstructs.sav)

FACTOR
  /VARIABLES W1varied W2value W3team P1assess P2progress P3evstand
  /MISSING LISTWISE 
  /ANALYSIS W1varied W2value W3team P1assess P2progress P3evstand
  /PRINT INITIAL EXTRACTION ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PAF
  /CRITERIA ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.

**********
Model 1.1 (Null): Table 7.5, 7.6, 7.7 (ch7worklifeorg.sav)

MIXED work by assessjob
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob | noint SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
 /Random = assessjob | Subject (orgcode) COVTYPE(DIAG)
  /Random = assessjob | Subject(Rid*orgcode) COVTYPE(DIAG)
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(ID).

**********
Table 7.8 (ch7worklifeorg.sav)

MIXED work by assessjob
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob | noint SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
 /Random = assessjob | Subject (orgcode) COVTYPE(UNR)
  /Random = assessjob | Subject(Rid*orgcode) COVTYPE(UNR) 
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(ID).

********** 
Model 1.2 (Final Null Model): Table: 7.10 (ch7worklifeorg.sav)

MIXED work by assessjob
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob | noint SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /Random = assessjob | Subject (orgcode) COVTYPE(UNR)
  /Random = assessjob | Subject(Rid*orgcode) COVTYPE(UNR) 
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(DIAG).

**********
Adding Level-2 Predictors
Model 1.3: Tables 7.11, 7.12, 7.13 (ch7worklifeorg.sav)

MIXED work by assessjob with stability female
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob stability*assessjob female*assessjob | noint SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /Random = assessjob | Subject (orgcode) COVTYPE(UNR)
  /Random = assessjob | Subject(Rid*orgcode) COVTYPE(UNR) 
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(DIAG).

**********
Adding the Organizational Predictors
Model 1.4: Tables 7.14, 7.15 (ch7worklifeorg.sav)

MIXED work by assessjob with female stability gmresources gmorgprod
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob gmorgprod*assessjob  gmresources*assessjob  
  stability*assessjob female*assessjob| noint SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /Random = assessjob | Subject (orgcode) COVTYPE(UN)
  /Random = assessjob | Subject(orgcode*Rid) COVTYPE(UN) 
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(DIAG).

**********
Examining Equality Constraints
Table 7.16 (ch7worklifeorg.sav)

MIXED work by assessjob with female stability gmresources gmorgprod
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob female stability gmorgprod 
  gmresources| noint SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /Random = assessjob | Subject (orgcode) COVTYPE(UN)
  /Random = assessjob | Subject(orgcode*Rid) COVTYPE(UN) 
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(DIAG).

**********
Investigating a Random Level 2 Slope
Defining Models 1.6 and 1.7 with IBM SPSS  Menu Commands
Model 1.6 (ch7worklifeorg.sav)
 
MIXED work by assessjob with female stability gmresources gmorgprod
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob female stability gmorgprod 
  gmresources| noint SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /Random = assessjob stability | Subject (orgcode) COVTYPE(DIAG)
  /Random = assessjob | Subject(orgcode*Rid) COVTYPE(UN) 
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(DIAG).

**********
Model 1.7 (ch7worklifeorg.sav)

MIXED work by assessjob with female stability gmresources gmorgprod
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= assessjob gmorgprpd*assessjob gmresources*assessjob
  stability*assessjob female*assessjob| noint SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /Random = assessjob stability | Subject (orgcode) COVTYPE(DIAG)
  /Random = assessjob | Subject(orgcode*Rid) COVTYPE(UN) 
  /REPEATED=Index1 | SUBJECT(orgcode*Rid) COVTYPE(DIAG).

*********
Multivariate Multilevel Model for Correlated Observed Outcomes  
Model 2.1 (Null): Tables 7.18, 7.19  (ch7achievement.sav)

MIXED achieve BY Index1
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED= Index1 | NOINT SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=Index1 | SUBJECT(schcode) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(UN).

**********
Building a Complete Model (Predictors and Cross-Level Interactions)  
Model 2.2: Table 7.20  (ch7achievement.sav) 

MIXED achieve BY female Index1 WITH gmacadpress
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=Index1 Index1*gmacadpress Index1*female | NOINT SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=Index1 | SUBJECT(schcode) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(UN).

**********
Treating female as  covariate
Table 7.21: Treating female as  covariate (ch7achievement.sav)

MIXED achieve BY Index1 WITH gmacadpress female
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=Index1 Index1*gmacadpress Index1*female | NOINT SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=Index1 | SUBJECT(schcode) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(UNR).

**********
**Table 7.22: Treating female as categorical (Factor) (ch7achievement.sav)

MIXED achieve BY female Index1 WITH gmacadpress
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=Index1 Index1*gmacadpress Index1*female | NOINT SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=Index1 | SUBJECT(schcode) COVTYPE(UN)
  /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(UNR).

**********
Correlations Between Tests at Each Level 
Model 2.3: Table 7.23  (ch7achievement.sav)

 MIXED achieve BY Index1  with gmacadpress female
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
    /FIXED= Index1 gmacadpress*Index1 index1*female  | NOINT SSTYPE(3)
  /METHOD=REML 
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=Index1 | SUBJECT(schcode) COVTYPE(UNR)
  /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(UNR).

**********
Ch7, Final Section: Defining A Parallel Growth Process
Specifying the Time Model 
Model 3.1: Tables 7.25, 7.26, 7.27 (ch7PGachievement.sav)

MIXED achieve by math with orthtime orthquadtime
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= math math*orthtime math*orthquadtime | noint SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /Random = math | Subject (schcode) COVTYPE(UN)
 /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(AR1).

**********
Model 3.2: Adding the Predictors
Model 3.2: Tables 7.29, 7.30, 7.31 (ch7PGachievement.sav)

MIXED achieve by math with orthtime orthquadtime female schcontext
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED= math math*schcontext math*female math*orthtime math*orthquadtime
     schcontext*math*orthtime female*math*orthtime| noint SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /Random = math  | Subject (schcode) COVTYPE(UN)
 /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(AR1).

**********
Table 7.32 (ch7PGachievement.sav)

MIXED achieve BY math WITH orthtime orthquadtime female schcontext
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE) 
  /FIXED=math math*schcontext math*female math*orthtime math*orthquadtime schcontext*math*orthtime 
    female*math*orthtime | NOINT SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=math math*orthtime | SUBJECT(schcode) COVTYPE(CSH)
  /REPEATED=Index1 | SUBJECT(schcode*Rid) COVTYPE(AR1).

**********








