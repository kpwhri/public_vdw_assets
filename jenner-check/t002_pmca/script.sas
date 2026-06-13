/* ------------------------------------------------------------------ *
 * Demonstrates %pmca from this repository's standard_macros.sas.       *
 * It reads the COHORT of children and their index dates, pulls their   *
 * diagnoses from the VDW dx table within the lookback window, and       *
 * writes one row per child with the body-system flags and the two      *
 * PMCA complexity scores (cond_less, cond_more).                       *
 * ------------------------------------------------------------------ */

%pmca(inset        = cohort
    , index_date   = idx_date
    , outset       = pmca_results
    , days_lookback = 365) ;

proc print data = pmca_results ;
  var mrn index_date cond_less cond_more
      anyhemato anymh anyprogressive ;
  title "PMCA complexity classification per child" ;
run ;
