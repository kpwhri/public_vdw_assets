/* ------------------------------------------------------------------ *
 * Demonstrates %CalcAge from this repository's standard_macros.sas.    *
 * The macro expands to a SAS age-in-whole-years expression, so it      *
 * drops straight into a DATA-step column assignment -- the primary     *
 * use shown in the README.  Here it ages a small demographics cohort   *
 * as of two different reference dates.                                 *
 * ------------------------------------------------------------------ */

data ages ;
  set demographics ;
  age_2026 = %CalcAge(birth_date, "01jan2026"d) ;
  age_2000 = %CalcAge(birth_date, "01jan2000"d) ;
run ;

proc print data = ages ;
  var mrn birth_date age_2026 age_2000 ;
  title "Whole-year age via %nrstr(%CalcAge), as of 01-Jan-2026 and 01-Jan-2000" ;
run ;
