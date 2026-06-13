options obs=100;

/* ------------------------------------------------------------------ *
 * Macro %CalcAge, copied verbatim from standard_macros.sas (the HCSRN *
 * VDW standard-macros library in this repository).  It returns an     *
 * age-in-whole-years expression that drops straight into a DATA step. *
 * ------------------------------------------------------------------ */

** Utility macro for fairly precisely calculating age. ;
%macro CalcAge(BDtVar, RefDate) ;
  floor ((intck('month',&BDTVar,&RefDate) - (day(&RefDate) < min (day(&BDTVar),
  day (intnx ('month',&RefDate, 1) - 1) ) ) ) /12 )
%mend CalcAge ;

/* A small cohort of birth dates to age, supplied here so the bundle
   is self-contained (the real macro is called against VDW demographics). */
data demographics ;
  input mrn $ birth_date :date9. ;
  format birth_date date9. ;
  datalines ;
A001 14feb1980
A002 29feb2000
A003 01jan1990
A004 31dec1999
A005 15jun1975
A006 25dec2009
A007 30jun2005
A008 02mar1953
;
run ;
