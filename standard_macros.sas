/*********************************************
* Roy Pardee
* Center For Health Studies
* (206) 287-2078
* pardee.r@ghc.org
*
* Contains standard VDW macros for use against VDW data.
*
* These macros are documented here:
*   https://www.hcsrn.org/share/page/site/VDW/wiki-page?title=VDW_Standard_Macros&listViewLinkBack=true
* and also at
*   https://github.com/kpwhri/hcsrn-standard-macros
*
* Anything documented on those pages is supported--please report bugs to roy.e.pardee@kp.org.
*
* You will find other macros in this file--most are helpers for the documented macros.  There may
* also be a vestigial macro or two (there are none when I write this comment, but who knows what
* the future may bring?).  Any such macros are unsupported.
*
*********************************************/

%macro new_macro_for_testing(say_this) ;
  %do i = 1 %to 12 ;
    %put INFO: &say_this ;
  %end ;
%mend new_macro_for_testing ;

** Utility macro for fairly precisely calculating age. ;
%macro CalcAge(BDtVar, RefDate) ;
  floor ((intck('month',&BDTVar,&RefDate) - (day(&RefDate) < min (day(&BDTVar),
  day (intnx ('month',&RefDate, 1) - 1) ) ) ) /12 )
%mend CalcAge ;

%macro GetRxForPeople(
         People   /* The name of a dataset containing the MRNs of people
                     whose fills you want. */
       , StartDt  /* The date on which you want to start collecting fills. */
       , EndDt    /* The date on which you want to stop collecting fills. */
       , Outset   /* The name of the output dataset containing the fills. */
       ) ;

   /*
      Gets the pharmacy fills for a specified set of people (identified by MRNs)
      which ocurred between the dates specified in StartDt and EndDt.
   */


   %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;
      proc sql ;
         create table &OutSet as
         select r.*
         from &_vdw_rx as r INNER JOIN
               &People as p
         on    r.MRN = p.MRN
         where r.RxDate BETWEEN "&StartDt"d AND "&EndDt"d ;
      quit ;
   %end ;

%mend GetRxForPeople ;
/*********************************************************;
* Testing GetRxForPeople (tested Ok 20041230 gh);
* ;
%include '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\StdVars.sas';

data PeopleIn;
  infile '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\testchs100.txt';
  input mrn $char10.;
run;

%GetRxForPeople(PeopleIn,01Jan2002,31Dec2002,RxOut) ;
**********************************************************/;
%macro GetRxForDrugs(
            DrugLst  /* The name of a dataset containing the NDCs of the drugs
                         whose fills you want. */
          , StartDt  /* The date on which you want to start collecting fills.*/
          , EndDt    /* The date on which you want to stop collecting fills. */
          , Outset   /* The name of the output dataset containing the fills. */
          ) ;

   /*
      Gets the pharmacy fills for a specified set of drugs (identified by NDCs)
      which ocurred between the dates specified in StartDt and EndDt.
   */

   %if &DrugLst = &Outset %then %do ;
     %put PROBLEM: Drug List dataset must be different from the OutSet dataset.;
     %put PROBLEM: Both parameters are set to "&DrugLst". ;
     %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;

      proc sql ;
         create table &OutSet as
         select r.*
         from &_vdw_rx as r INNER JOIN
               &DrugLst as p
         on    r.NDC = p.NDC
         where r.RxDate BETWEEN "&StartDt"d AND "&EndDt"d ;
      quit ;

   %end ;
%mend GetRxForDrugs ;

%macro GetRxForPeopleAndDrugs(
           People   /* The name of a dataset containing the people
                       whose fills you want. */
         , DrugLst  /* The NDC codes of interest */
         , StartDt  /* The date on which you want to start collecting fills.*/
         , EndDt    /* The date on which you want to stop collecting fills. */
         , Outset   /* The name of the output dataset containing the fills. */
         ) ;

   /*
      Gets the pharmacy fills for a specified set of people (identified by MRNs)
      which occurred between the dates specified in StartDt and EndDt.
   */

   %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;

      proc sql ;
  	    create table &OutSet as
	  			select r.*
   				from  &_vdw_rx as r
   				INNER JOIN &People as p
   				on    r.MRN = p.MRN
   				where r.RxDate BETWEEN "&StartDt"d AND "&EndDt"d AND
         				r.NDC in (select _x.NDC from &DrugLst as _x) ;
      quit ;

   %end ;
%mend GetRxForPeopleAndDrugs ;

%macro GetDxForPeopleAndDx (
           People  /* The name of a dataset containing the people whose
                      fills you want. */
         , DxLst   /* The ICD9 codes of interest */
         , StartDt /* The date on which you want to start collecting fills.*/
         , EndDt   /* The date on which you want to stop collecting fills. */
         , Outset  /* The name of the output dataset containing the fills. */
         ) ;


   %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
   %end ;

   %else %do ;
      proc sql ;
        create table &outset as
			  select d.*
   			from  &_vdw_dx as d
   			INNER JOIN &People as p
   			on    d.MRN = p.MRN
   			where d.ADate BETWEEN "&StartDt"d AND "&EndDt"d AND
   						d.dx in (select _x.dx from &DxLst as _x)
        ;
      quit ;
   %end ;


%mend GetDxForPeopleAndDx;

%macro CountFills(DrugList) ;
   /*
      Counts the number of fills for each of the NDC codes specified in
      the input dataset.
   */



   proc sql ;
      title2 "Extent of pharmacy data." ;
      select count(*) as NumFills label = "Total rx records"
            , min(RxDate) as FirstFill
                format = mmddyy10. label = "Earliest recorded fill"
            , max(RxDate) as LastFill
                format = mmddyy10. label = "Most recent recorded fill"
      from &_vdw_rx ;

      title2 "Number of fills for the list of NDCs in &DrugList" ;
      select d.generic
            , d.NDC
            , count(r.NDC) as NumFills label = "Number of Fills"
            , min(r.RxDate) as FirstFill
                format = mmddyy10. label = "Date of first fill"
            , max(r.RxDate) as LastFill
                format = mmddyy10. label = "Date of most recent fill"
      from  &DrugList as d LEFT JOIN
            &_vdw_rx as r
      on    d.NDC = r.NDC
      group by d.generic, d.NDC ;
   quit ;


%mend CountFills ;

%macro BreastCancerDefinition01(StartDt = 01Jan1997
                              , EndDt = 31Dec2003
                              , OutSet = brca) ;
   /*
      Pulls the set of "incident" (that is, first-ocurring during the
      specified date range) breast cancers, both invasive and in-situ
      (but excluding LCIS).

      These criteria are based on the ones used for the Early Screening study.
      See: https://www.kpchr.org/CRN2/apps/storage/docs/
                            20000823whmesprogramming_case_criteria.doc.
   */



   proc sql number ;
      create table _AllBreastTumors as
      select mrn
            , DxDate
            , DtMrk1 as ERMarker
            , StageGen
            , StageAJ
      from  &_vdw_tumor
      where DxDate between "&StartDt"d and "&EndDt"d  AND
            ICDOSite between 'C500' and 'C509'        AND
            Gender = '2'                              AND
            Morph NOT between '9590' and '9979'       AND
            ( (behav in ('3', '6', '9')) OR
              (behav = '2' AND MORPH ne '8520')) ;

      create table _FirstBTs as
      select DISTINCT b.*
      from  _AllBreastTumors as b
        INNER JOIN
            (select MRN, min(DxDate) as FirstBTDate
              from _AllBreastTumors group by MRN) as b2
      on    b.MRN = b2.MRN AND
            b.DxDate = b2.FirstBTDate
      order by mrn, ERMarker ;

      drop table _AllBreastTumors ;

      title "These people had >1 tumor dxd on the same day, ";
      title2 "each with different receptor statuses or stages." ;
      select *
      from  _FirstBTs
      where MRN in (select _FirstBTs.MRN from _FirstBTs group by _FirstBTs.MRN having count(*) > 1);
      title ;

      * Some women will have > 1 breast tumor dxd on the same day, each with ;
      * different stages and/or receptor statuses.  We want to call the ;
      * receptor status positive if any tumor is positive, and we want to ;
      * take the greatest stage. ;

      create table &OutSet as
      select mrn
            , dxdate
            , min(case ERMarker
                     when '0' then 10
                     else input(ERMarker, 2.0)
                  end) as ERMarker format = ERM.
            , max(case lowcase(StageGen)
                     when '9' then -1
                     when 'b' then -2
                     else input(StageGen, 2.0)
                  end) as StageGen format = StageGen.
            , max(case lowcase(StageAJ)
                     when '' then -1
                     when 'unk' then -1
                     when '2a' then 2
                     when '2b' then 2.5
                     when '3a' then 3
                     when '3b' then 3.5
                     else input(StageAJ, 2.0)
                  end) as StageAJ format = StageAJ.
      from _FirstBTs
      group by mrn, dxdate ;
   quit ;



%mend BreastCancerDefinition01 ;

%macro BreastCancerDefinition02(StartDt = 01Jan1997
                              , EndDt = 31Dec2003
                              , OutSet = brca
                              , OutMultFirsts =
                              ) ;
   /*
    Adapted from 01, for Pharmacovigilance.  Differences:
      01 includes DCIS (but not LCIS).  This is invasive tumors only.
      Returns a dataset of *tumors*, not *women*.  There are frequently > 1 tumor discovered as a
      "first tumor" so applications will have to reduce that to women if necessary.
      Not so much printing (but optional output dset).
      More warnings.

      TODO: Try and nail down a more HMO-site-general list of morphology codes
            to exclude.  These have just had scrutiny from the Group Health
            staff and on the Group Health data.
   */

  %local female ;
  %let   female = 2 ;

  %local collab_stage_year ;
  %let collab_stage_year = 2003 ;

  %local behav_primary ;
  %local behav_metastatic ;
  %local behav_unknown_prim_meta ;

  %let behav_primary = 3 ;
  %let behav_metastatic = 6 ;
  %let behav_unknown_prim_meta = 9 ;

  %* These appeared in some GH breast tumor data--they are undesirable. ;
  %local non_small_cell ;
  %local neuroendicrine ;
  %local fibrosarcoma ;

  %let non_small_cell = 8046 ;
  %let neuroendicrine = 8246 ;
  %let fibrosarcoma = 8810 ;

  %local in_situ ;
  %let in_situ = 0 ;

  proc sql number ;
    * We take these over all time since the file is small and we want to be ;
    * able to call the resulting cases "incident" insofar as we can ;
    create table _AllBreastTumors as
    select *
    from  &_vdw_tumor
    where DxDate le "&EndDt"d                  AND
          Gender = "&female"                   AND
          ICDOSite between 'C500' and 'C509'   AND
          Morph NOT between '9590' and '9979'  AND
          Morph NOT in ("&non_small_cell", "&neuroendicrine", "&fibrosarcoma" ) AND
          behav in ("&behav_primary", "&behav_unknown_prim_meta") AND
          stagegen ^= "&in_situ"
    ;

    * Check desired date limits against observed, and warn as necessary. ;
    select  min(dxdate) as first_tumor   format = yymmddn8. label = "First observed breast tumor (over all time)"
          , max(dxdate) as last_tumor    format = yymmddn8. label = "Last observed breast tumor (over all time)"
          , "&StartDt"d as desired_first format = yymmddn8.
          , "&EndDt"d   as desired_last  format = yymmddn8.
    into :first_tumor, :last_tumor, :desired_first, :desired_last
    from _AllBreastTumors
    ;

    %if &desired_first < &first_tumor %then %do i = 1 %to 10 ;
      %put WARNING: NO BREAST TUMORS FOUND PRIOR TO &STARTDT (EARLIEST IS &FIRST_TUMOR)--EARLY TUMORS MAY NOT BE AS "INCIDENT" AS LATER ONES!!! ;
    %end ;

    %if &desired_last > &last_tumor %then %do i = 1 %to 10 ;
      %put WARNING: NO BREAST TUMORS FOUND AFTER &LAST_TUMOR.--THE &_TUMORDATA FILE IS NOT EXTENSIVE ENOUGH TO MEET THIS REQUEST!!! ;
    %end ;

    * Grab all tumors from each womans first dxdate. ;
    create table _FirstBTs as
    select DISTINCT b.*
    from  _AllBreastTumors as b
      INNER JOIN
          (select MRN, min(DxDate) as FirstBTDate
           from _AllBreastTumors
           group by MRN
           having min(DxDate) between "&StartDt"d and "&EndDt"d) as b2
    on    b.MRN = b2.MRN AND
          b.DxDate = b2.FirstBTDate
    order by mrn, dxdate;

    drop table _AllBreastTumors ;

    * Who had > 1 breast tumor discovered on the day of their first tumor? ;
    create table _multiple_firsts as
    select *
    from  _FirstBTs
    where MRN in (select _FirstBTs.MRN
                  from _FirstBTs
                  group by _FirstBTs.MRN
                  having count(*) > 1)
    ;

    %if &SQLOBS > 0 %then %do ;
      %do i = 1 %to 5 ;
        %PUT BCD2 NOTE: There is at least one, and may be as many as %eval(&SQLOBS/2) people with > 1 tumor dignosed on the same day, each with different receptor statuses or stages. ;
      %end ;
      %if %length(&OutMultFirsts) > 0 %then %do ;
        create table &OutMultFirsts as
        select *
        from _multiple_firsts
        ;
        %do i = 1 %to 5 ;
          %put BCD2 NOTE: The multiple-first-tumor records have been written out to &OutMultFirsts ;
        %end ;
      %end ;

    %end ;

    create table &OutSet as
    select *
    from _FirstBTs
    ;
  quit ;

%mend BreastCancerDefinition02 ;

%macro PullContinuous(InSet                     /* The name of the input dataset of MRNs of the ppl whose enrollment you want to check. */
                     , OutSet                    /* The name of the output dataset of only the continuously enrolled people. */
                     , IndexDate                 /* Either the name of a date variable in InSet, or, a complete date literal (e.g., "01Jan2005"d) */
                     , PreIndexEnrolledMonths    /* The # of months of enrollment required prior to the index date. */
                     , PreIndexGapTolerance      /* The length of enrollment gaps in months you consider to be ignorable for pre-index date enrollment. */
                     , PostIndexEnrolledMonths   /* The # of months of enrollment required post index date. */
                     , PostIndexGapTolerance     /* The length of enrollment gaps in months you consider to be ignorable for post-index date enrollment.*/
                     , DebugOut = work           /* Libname to save interim dsets to for debugging--leave set to work to discard these. */
                     , EnrollDset = &_vdw_enroll /* For testing. */
                     ) ;

   %* Validate the arguments. ;
   %if &PreIndexGapTolerance > &PreIndexEnrolledMonths %then %do ;
      %put WARNING: Pre-index gap tolerance cannot be greater than the number;
      %put WARNING: of months of desired pre-index enrollment.;

      %let PreIndexGapTolerance = %eval(&PreIndexEnrolledMonths - 1) ;
      %put Setting the pre-index gap tolerance to &PreIndexGapTolerance ;
   %end ;

   %if &PostIndexGapTolerance > &PostIndexEnrolledMonths %then %do ;
      %put WARNING: Post-index gap tolerance cannot be greater than the number;
      %put WARNING: of months of desired Post-index enrollment.;

      %let PostIndexGapTolerance = %eval(&PostIndexEnrolledMonths - 1) ;
      %put Setting the Post-index gap tolerance to &PostIndexGapTolerance ;
   %end ;


   %put ;
   %put ;
   %put ============================================================== ;
   %put ;
   %put Macro PullContinuous: ;
   %put ;
   %put Creating a dataset "&OutSet", which will look exactly like            ;
   %put dataset "&InSet", except that anyone not enrolled for                 ;
   %put &PreIndexEnrolledMonths months prior to &IndexDate (disregarding gaps ;
   %put of up to &PreIndexGapTolerance month(s)) AND &PostIndexEnrolledMonths ;
   %put months after &IndexDate (disregarding gaps of up to                   ;
   %put &PostIndexGapTolerance month(s)) will be eliminated.                  ;
   %put ;
   %put ============================================================== ;
   %put ;
   %put ;



   proc sql ;
      * Table of unique MRNs and the dates setting out the period of interest (earliest & latest). ;
      create table __ids as
      select distinct mrn
         , &IndexDate                                                         as idate    format = mmddyy10.
         , intnx('MONTH', &IndexDate, -&PreIndexEnrolledMonths, 'BEGINNING')  as earliest format = mmddyy10.
         , intnx('MONTH', &IndexDate,  &PostIndexEnrolledMonths, 'END')       as latest   format = mmddyy10.
      from &InSet
      ;

      * Make sure we only have one record per MRN. ;
      create table __drop_me as
      select mrn, count(* ) as appears_num_times
      from __ids
      group by mrn
      having count(*) > 1 ;

      %if &sqlobs > 0 %then %do ;
         %PUT ;
         %PUT ;
         %PUT ;
         %PUT ;
         %PUT ;
         %PUT ERROR: &SQLOBS MRNs appear more than once in the input datset with different index dates! ;
         %PUT ERROR: &SQLOBS MRNs appear more than once in the input datset with different index dates! ;
         %PUT ERROR: &SQLOBS MRNs appear more than once in the input datset with different index dates! ;
         %PUT ;
         %PUT See the .lst file for a list of duplicated MRNs ;
         %PUT ;
         %PUT ;
         %PUT ;
         %PUT ;
         %PUT ;
         reset outobs = 20 nowarn ;
         select * from __drop_me ;
         %*abort return ;
         %goto exit;
      %end ;

      reset outobs = max ;

      drop table __drop_me ;

      * Grab out the enroll records that could possibly contribute to the period of interest. ;
      create table __enroll as
      select i.mrn, i.earliest, i.latest, i.idate, e.enr_start, e.enr_end
      from  __ids as i INNER JOIN
            &EnrollDset as e
      on    i.MRN = e.MRN
      where i.earliest le e.enr_end AND
            i.latest   ge e.enr_start
      order by mrn, enr_start
      ;

      * Anybody w/no recs in __enroll could not possibly have been sufficiently enrolled. ;
      create table __not_enrolled as
      select i.mrn
      from __ids as i LEFT JOIN
            __enroll as e
      on    i.mrn = e.mrn
      where e.mrn IS NULL ;
   quit ;

   * Now we loop through the enroll records looking for gaps. ;
   * There are 3 places where gaps can occur--before the start of enrollment, ;
   * in the middle of enrollment (inter-record), and past the end of enrollment. ;
   * First records for a person can only have before-enrollment gaps.  Middle records ;
   * can only have an inter-record gap.  But Last records can have either or ;
   * both an inter and a post-enrollment gap. ;
   data &debugout..__insufficiently_enrolled ;
      retain _last_end . ;
      length reason $ 4 pre_gap_length post_gap_length 4 ;
      set __enroll ;
      by mrn ;
      num_possible_gaps = 1 ;
      if first.mrn then do ;
         * Earliest period for this person--there may be a gap between earliest & enr_start. ;
         possible_gap_start1 = earliest ;
         possible_gap_end1   = enr_start ;
      end ;
      else do ;
         * Middle or last rec--maybe an inter-record gap. ;
         possible_gap_start1 = _last_end ;
         possible_gap_end1   = enr_start ;
      end ;
      if last.mrn then do ;
         * Last period--may be a gap between end and latest. ;
         possible_gap_start2 = enr_end ;
         possible_gap_end2 = latest ;
         num_possible_gaps = 2 ;
      end ;

      array starts{2} possible_gap_start: ;
      array ends{2}   possible_gap_end: ;
      * Loop through the 2 possible gaps, outputting anybody w/an out of tolerance gap. ;
      do i = 1 to num_possible_gaps ;
         * We knock 1 off the number of months b/c we expect at least one month gap between contiguous periods. ;
         * No we dont. ;
         this_gap = intck('MONTH', starts{i}, ends{i}) ;

         if this_gap > 0 then do ;
            /*
               We have an actual gap.  There are 3 possibilities.
                  - The whole gap falls before the index date (possible_gap_end lt idate).
                  - The whole gap falls after the index date (possible_gap_start gt idate).
                  - The gap straddles the index date.
            */
            if ends{i} lt idate then do ;
               pre_gap_length = this_gap ;
               post_gap_length = 0 ;
            end ;
            else if starts{i} gt idate then do ;
               pre_gap_length = 0 ;
               post_gap_length = this_gap ;
            end ;
            else do ;   * Straddle gap--idate falls between gap start & gap end. ;
               pre_gap_length  = intck('MONTH', starts{i}, idate) ;
               post_gap_length = intck('MONTH', idate, ends{i})   ;
            end ;

         end ;

         if (pre_gap_length > &PreIndexGapTolerance) then do ;
            reason = 'pre' ;
            output ;
         end ;
         else if (post_gap_length > &PostIndexGapTolerance) then do ;
            reason = 'post' ;
            output ;
         end ;

      end ;
      _last_end = enr_end + 1 ;
      format _last_end possible_gap_start: possible_gap_end: mmddyy10. ;
   run ;

   proc sql ;
      create table &OutSet as
      select * from &InSet
      where mrn not in (select mrn from &debugout..__insufficiently_enrolled
                        UNION ALL
                        select mrn from __not_enrolled) ;
      drop table __enroll ;
      drop table __not_enrolled ;
   quit ;
%exit:
%mend PullContinuous ;


%macro ndclookup(
         inds     /* An input dataset of strings to search for,
                       in a var named "drugname".  */
       , outds    /* The name of the output dset of NDCs,
                       which contain one of the input strings. */
       , EverNDC  /* The name of your local copy of the EverNDC file. */
       );

*******************************************************************************;
* look up NDC codes by drugnames or fragments of drugnames
* Check the results file to see that they are all drugs of interest
*
* Input:
*	inds is the name of the input SAS dataset with the list of character strings
*		to match contains the variable "drugname"
*		Both the Generic and Brand fields are searched for all input strings
*
*	outds is the name of the output SAS dataset
*       EverNDC is the SAS dataset name of the file of all NDCcodes
*
* EverNDC is the fully qualified name of your local copy of the EverNDC dataset
*
*    Example:
*Data StringsOfInterest;
*   input  drugname $char20.;
*   datalines;
*TAMOX
*Ralox
*NOLVADEX
*LETROZOLE
*EXEMESTANE
*ANASTROZOLE

*%ndclookup(StringsOfInterest, NDCs_of_Interest, mylib.EverNDC);
*******************************************************************************;

  proc sql noprint ;

    * Create a monster WHERE clause to apply to ever_ndc from the contents   ;
    * of InDs. The embedded single quotes can get a bit confusing--just      ;
    * remember that one single quote character escapes the following one.  So;
    * a string of 4 single-quote chars in a row defines a string containing  ;
    * one single quote--the two on the ends delimit the string, and the two  ;
    * in the middle resolve to one (the first one escaping the second).      ;
    * SQL written by Roy Pardee                                              ;

    select 'upcase(n.Generic) LIKE ''%' || trim(upcase(s.DrugName))|| '%''' ||
       ' OR upcase(n.Brand)   LIKE ''%' || trim(upcase(s.DrugName))|| '%'''
          as where_clause
    into :wh separated by ' OR '
    from &inds as s ;

    * First pull all of the NDCs that meet the WHERE clause above. ;
    create table _OfInterest as
    select distinct *
    from &everndc as n
    where &wh ;

    * Now pull drugs that *dont* match the WHERE clause, but share an NDC ;
    *   with one that does. ;
    create table _Suspicious as
    select distinct n.*
    from _OfInterest as a inner join &everndc as n
    on a.ndc = n.ndc
    where not (&wh)
    ;

    * Mash the two dsets together ;
    create table &outds as
    select *, 0 as Suspicious
      label = "Flag for whether Generic or Brand contained a string of interest"
    from _OfInterest
    UNION ALL
    select *, 1 as Suspicious
    from _Suspicious ;

    drop table _OfInterest ;
    drop table _Suspicious ;
  quit ;
%mend ndclookup;

%macro GetPxForPeople(
           People  /* The name of a dataset containing the people whose
                         procedures you want. */
         , StartDt /* The date on which you want to start collecting procs*/
         , EndDt   /* The date on which you want to stop collecting procedures*/
         , Outset  /* The name of the output dataset containing the procedures*/
         ) ;

   /*
      Gets the procedures for a specified set of people (identified by MRNs)
      which ocurred between the dates specified in StartDt and EndDt.
   */


   %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;
      proc sql ;
         create table &OutSet as
         select r.*
         from &_vdw_px as r INNER JOIN
               &People as p
         on    r.MRN = p.MRN
         where r.ADate BETWEEN "&StartDt"d AND "&EndDt"d ;
      quit ;
   %end ;

%mend GetPxForPeople ;

%macro GetUtilizationForPeople(
          People  /* The name of a dataset containing the people whose
                       procedures you want*/
        , StartDt /* The date on which you want to start collecting procedures*/
        , EndDt   /* The date on which you want to stop collecting procedures*/
        , Outset  /* The name of the output dataset containing the procedures*/
        ) ;

   /*
      Gets the utilization records for a specified set of people (identified
      by MRNs) hich ocurred between the dates specified in StartDt and EndDt.
   */

   %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;
      proc sql ;
         create table &OutSet as
         select r.*
         from &_vdw_utilization as r INNER JOIN
               &People as p
         on    r.MRN = p.MRN
         where r.ADate BETWEEN "&StartDt"d AND "&EndDt"d ;
      quit ;
   %end ;

%mend GetUtilizationForPeople ;

/*********************************************************;
* Testing GetPxForPeople (tested ok 20041230 gh);
* ;
%include '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\StdVars.sas';

data PeopleIn;
  infile '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\testchs100.txt';
  input mrn $char10.;
run;

%GetPxForPeople(PeopleIn,01Jan2002,31Dec2002,PxOut) ;
**********************************************************/;

%macro GetDxForPeople(
          People  /* The name of a dataset containing the people whose
                       diagnoses you want. */
        , StartDt /* The date on which you want to start collecting diagnoses.*/
        , EndDt   /* The date on which you want to stop collecting diagnoses. */
        , Outset  /* The name of the output dataset containing the diagnoses. */
        ) ;

   /*
      Gets the diagnoses for a specified set of people (identified by MRNs)
      which ocurred between the dates specified in StartDt and EndDt.
   */

   %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;
      proc sql ;
         create table &OutSet as
         select r.*
         from &_vdw_dx as r INNER JOIN
               &People as p
         on    r.MRN = p.MRN
         where r.ADate BETWEEN "&StartDt"d AND "&EndDt"d ;
      quit ;
   %end ;

%mend GetDxForPeople ;
/*********************************************************;
* Testing GetDxForPeople (tested ok 20041230 gh);
* ;
%include '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\StdVars.sas';

data PeopleIn;
  infile '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\testchs100.txt';
  input mrn $char10.;
run;

%GetDxForPeople(PeopleIn,01Jan2002,31Dec2002,DxOut) ;
**********************************************************/;


%macro GetDxForDx(
          DxLst     /* The name of a dataset containing the diagnosis
                         list you want. */
        , DxVarName /* The name of the DX variable in DxLst  */
        , StartDt   /* The date on which you want to start collecting fills. */
        , EndDt     /* The date on which you want to stop collecting fills. */
        , Outset    /* The name of the output dataset containing the fills. */
        ) ;

   /*
     Gets the records for a specified set of diagnoses (identified by ICD9 code)
     which ocurred between the dates specified in StartDt and EndDt.
   */

   %if &DxLst = &Outset %then %do ;
    %put PROBLEM: The Diagnosis List dataset must be different from the;
    %put PROBLEM:   OutSet dataset;
    %put PROBLEM: Both parameters are set to "&DxLst". ;
    %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;
      proc sql ;
         create table &OutSet as
         select DBig.*
         from  &_vdw_dx as DBig INNER JOIN
               &DxLst as DLittle
         on    DBIG.DX = Dlittle.&DxVarName.
         where Dbig.ADate BETWEEN "&StartDt"d AND "&EndDt"d ;
      quit ;
   %end ;

%mend GetDxForDx ;
/*********************************************************;
* Testing GetDxForDx (tested 20041230 gh);
* ;
%include '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\StdVars.sas';

data DxOfInterest;
  input dx $char6.;
  cards;
V22   Normal pregnancy
V22.0       Supervision of normal first pregnancy
V22.1       Supervision of other normal pregnancy
V22.2       Pregnant state, incidental
run;

%GetDxForDx(DxOfInterest, dx,01Jan2002,31Dec2002,DxOut) ;
**********************************************************/;

%macro GetPxForPx(
          PxLst             /*The name of a dataset containing the procedure
                                list you want. */
        , PxVarName         /*The name of the Px variable in PxLst  */
        , PxCodeTypeVarName /*Px codetype variable name in PxLst  */
        , StartDt           /*The date when you want to start collecting data*/
        , EndDt             /*The date when you want to stop collecting data*/
        , Outset            /*Name of the output dataset containing the data*/
        ) ;

  /*
  Gets the records for a specified set of diagnoses (identified by ICD9 code)
  which ocurred between the dates specified in StartDt and EndDt.
  */

  %if &PxLst = &Outset %then %do ;
    %** Make sure we arent going to overwrite the pxlist dset. ;
    %put PROBLEM: The Px List dataset must be different from the OutSet dataset;
    %put PROBLEM: Both parameters are set to "&PxLst". ;
    %put PROBLEM: Doing nothing. ;
  %end ;
  %else %do ;
    ** Make sure input variables exist in the pxlist dset. ;
    proc contents noprint data = &PxLst out = __varlist ;

    proc sql noprint ;
      select count(*) as num_vars
      into :num_vars
      from __varlist
      where upcase(name) in (%upcase("&PxVarName"), %upcase("&PxCodeTypeVarName")) ;
      drop table __varlist ;
    quit ;
    %if (&num_vars < 2) %then %do ;
      %put ERROR: The Px List dataset you supplied does not have one or both of the variables you named in the input parameters!!! ;
      %put ERROR: Expected to find both variables "&PxVarName" and "&PxCodeTypeVarName" in dataset "&PxLst", but one or both are missing. ;
      %put ERROR: Doing nothing. ;
    %end ;
    %else %do ;
      proc sort nodupkey data = &pxlst dupout = in_dupes ;
        by &pxvarname &pxcodetypevarname ;
      run ;
      proc sql noprint ;

        select count(*) as frq
        into :num_dupes
        from in_dupes
        ;

        reset print ;

        %if &num_dupes > 0 %then %do ;
          %put WARNING: There were duplicated values in your &PxLst dataset--THEY HAVE BEEN REMOVED!  Please see output for a listing of the codes that were duplicated. ;
          select 'WARNING: These values were duplicated in your input dataset of procedure codes.' as msg, *
          from in_dupes
          ;
          drop table in_dupes ;
        %end ;
        create table &OutSet as
        select PBig.*
        from  &_vdw_px as PBig INNER JOIN
              &PxLst as PLittle
        on    PBig.PX = PLittle.&PxVarName.  and
              PBig.PX_CodeType = PLittle.&PxCodeTypeVarName.
        where Pbig.ADate BETWEEN "&StartDt"d AND "&EndDt"d ;
      quit ;
    %end ;
  %end ;
%mend GetPxForPx ;
*********************************************************;
* Testing GetPxForPx (tested 20041230 gh);
* ;
/*
%include '\\Groups\data\CTRHS\Crn\S D R C\VDW\Macros\StdVars.sas';

data PxOfInterest;
  input Px $char6. CodeType $char1.;
  cards;
59409 C VAGINAL DELIVERY ONLY
59410 C  VAGINAL DELIVERY INCL POSTPARTUM CARE
59510 C  ROUTINE OB CARE INCL ANTEPARTUM CAR, CESAREAN DELIVER, POSTPARTUM CARE
59514 C  CESAREAN DELIVERY ONLY
run;

%GetPxForPx(PxOfInterest, px, CodeType, 01Jan2002,31Dec2002,PxOut) ;
**********************************************************;
*/

/* *********************************************************
* Takes an input dataset bearing a specified ID code (e.g.,
* MRN) and replaces it with an arbitrary
* StudyID, creating a crosswalk dset that relates the
* original ID codes to the new StudyIDs.
*
* Sample call:
*
* %DeIDDset( InSet = phe.people    The input dataset.
*          , XWalkSet = phe.xwalk  Name of the crosswalk dset
*          , OldIDVar = CHSID      Name of the ID variable you want removed.
*          , NewIDVar = StudyID    Name for the new ID variable.
*          , NewIDLen = 8          The length of the new ID variable.
*          ) ;
********************************************************* */

%macro DeIDDset( InSet     /* Name of the dataset you want de-identified. */
               , XWalkSet  /* Name of the output ID-crosswalk dset. */
               , OldIDVar  /* Name of the ID variable you want removed. */
               , NewIDVar  /* Name for the new ID variable the macro creates. */
               , NewIDLen  /* The length of the new ID variable.*/
               , StartIDsAt = 0
               ) ;

   proc sql nowarn ;
      create table _UIDs as
      select distinct &OldIDVar
      from &InSet
      ;
   quit ;

   %if %length(%trim(&sqlobs)) > &NewIDLen %then %do ;
      %put ;
      %put PROBLEM: THE ID LENGTH SPECIFIED IS INSUFFICIENT!!! ;
      %put ;
      %put DOING NOTHING!!! ;
      %put ;
   %end ;
   %else %do ;

      data _UIDs ;
        set _UIDs ;
        randy = uniform(675555) ;
      run ;

      proc sort data = _UIDs ;
        by randy ;
      run ;

      data &XWalkSet(keep = &NewIDVar &OldIDVar) ;
         set _UIDs ;
         &NewIDVar = put((_N_ + &StartIDsAt), z&NewIDLen..0) ;
         drop randy ;
      run ;

      proc sql ;
         create table &InSet._DeIDed(drop = &OldIDVar) as
         select x.&NewIDVar
               , i.*
         from  &XWalkSet as x INNER JOIN
               &InSet as i
         on    x.&OldIDVar = i.&OldIDVar ;
      quit ;

      data &InSet ;
         set &InSet._DeIDed ;
      run ;

      proc sql ;
         drop table &InSet._DeIDed ;
      quit ;

   %end ;

%mend DeIDDset ;

%macro LastWord(WordList) ;
   %** This is a helper macro for CollapsePeriods--it just returns the last word (variable name) in a string (var list). ;
   %local i ;
   %local ThisWord ;
   %let i = 0 ;
   %do %until(&ThisWord = ) ;
      %let i = %eval(&i + 1) ;
      %let ThisWord = %scan(&WordList, &i) ;
   %end ;
   %let i = %eval(&i - 1) ;
   %* Note the lack of a semicolon on the next line--thats on purpose! ;
   %scan(&WordList, &i)
%mend LastWord ;

%macro GetVarList(DSet         /* Name of the dset you want collapsed. */
                , RecStart     /* Name of the var that contains the period start dates. */
                , RecEnd       /* Name of the var that contains the period end dates. */
               , PersonID  = MRN   /* Name of the var that contains a unique person identifier. */
                ) ;

   %** This is also a helper macro for CollapsePeriods--it creates a global macro var ;
   %** containing a list of all vars in the input named dset *other than* the ones that ;
   %** define the start/end of each record. ;

   %** I dont know a good way of passing a return value out of a macro--so this is made global. ;
   %global VarList ;
   %global __PidLenSpec ;

   %** If we got just a one-part dset name for a WORK dataset, add the WORK libname explicitly. ;

   %if %index(&Dset, .) = 0 %then %do ;
      %let Dset = work.&Dset ;
   %end ;

   %**put Dset is &Dset ; ;

  %** This used to use a query to dictionary.columns to grab this info--huge. pain. in. the. ass. ;
  %** Dont use d.c--SAS is not a good citizen when you have libnames defined against RDBMSs. ;
  proc contents noprint data = &dset out = __varlist ;
  run ;

  proc sql noprint ;
    ** describe table dictionary.columns ;
    select name
    into :VarList separated by ' '
    from __varlist
    where upcase(name) not in (%upcase("&RecStart"), %upcase("&RecEnd"), %upcase("&PersonID")) ;

    select catx(' ', name, case type when 2 then '$' else '' end, put(length, best.)) as y
    into :__PidLenSpec
    from __varlist
    where upcase(name) = %upcase("&PersonID") ;


    drop table __varlist ;
  quit ;

%mend GetVarList ;

%macro CollapsePeriods(Lib          /* Name of the library containing the dset you want collapsed */
                     , DSet         /* Name of the dset you want collapsed. */
                     , RecStart     /* Name of the var that contains the period start dates. */
                     , RecEnd       /* Name of the var that contains the period end dates. */
                     , PersonID   = MRN /* Name of the var that contains a unique person identifier. */
                     , OutSet     = &lib..&dset /* In case you dont want this to overwrite your input dataset, specify another. */
                     , DaysTol    = 1   /* The number of days gap to tolerate in evaluating whether one period is contiguous w/another. */
                     , Debug      = 0   /* 0/1 flag indicating whether you want the PUT statements to run (PRODUCES A LOT OF OUTPUT!). */
                     ) ;

  %** Takes an input mbhist dataset and collapses contiguous time periods where the variables ;
  %** other than the ones defining period start/stop dates dont change. ;

  %** Adapted from Mark Terjesons code posted to sas-l: http://www.listserv.uga.edu/cgi-bin/wa?A2=ind0003d&L=sas-l&D=0&P=18578 ;

  %** preparing to go to a single inset param. ;
  %local inset ;
  %let inset = &Lib..&Dset ;

  %** This defines VarList ;
  %GetVarList( Dset = &inset
            , RecStart = &RecStart
            , RecEnd = &RecEnd
            , PersonID = &PersonID) ;

  %put VarList is &VarList ;

  %put Length of varlist is %length(&varlist) ;

  %local LastVar ;

  %if %length(&varlist) = 0 %then %do ;
    %let LastVar = &PersonID ;
  %end ;
  %else %do ;
    %let LastVar = %LastWord(&VarList) ;
  %end ;

  * Changed sort order ;
  proc sort nodupkey data = &inset ;
    by &VarList &PersonID &RecStart &RecEnd ;
  run ;

   data &outset ;
      retain PeriodStart PeriodEnd ;
      length PeriodStart PeriodEnd 4 ;
      format PeriodStart PeriodEnd mmddyy10. ;
      set &inset(rename = (&RecStart = _&RecStart
                           &RecEnd   = _&RecEnd)) ;

      by &PersonID &VarList NOTSORTED ;

      %if &Debug = 1 %then %do ;
        first_lastvar = first.&LastVar ;
        last_lastvar  = last.&LastVar ;
      %end ;

      if first.&LastVar then do ;
         ** Start of a new period--initialize. ;
         PeriodStart = _&RecStart ;
         PeriodEnd   = _&RecEnd ;
         %if &Debug = 1 %then %do ;
            put "First &LastVar:          " _N_ = PeriodStart =  _&RecStart =  PeriodEnd =  _&RecEnd = ;
         %end ;
      end ;
       /*
          Checking "contiguousity":
          If this records start date falls w/in (or butts up against) the
          current period (plus tolerance), then extend the current period out to this
          records end date.
       */
       ** if (PeriodStart <= _&RecStart <= PeriodEnd + 1) then do ;
       ** RP20100504: fixing a bug when using a tolerance of zero days. ;
       ** RP20101210: fixing a bug that fails to collapse gaps of exactly &daystol length. ;
       if (PeriodStart <= _&RecStart <= (PeriodEnd +(&DaysTol + 1))) then do ;
          ** Extend the period end out to whichever is longer--the period or the record. ;
          PeriodEnd = max(_&RecEnd, PeriodEnd) ;
          %if &Debug = 1 %then %do ;
             put "Extending period end:   " _N_ = PeriodStart =  _&RecStart =  PeriodEnd =  _&RecEnd = ;
          %end ;
       end ;
       else do ;
          * We are in a new period--output the last rec & reinitialize. ;
          output ;
          PeriodStart = _&RecStart ;
          PeriodEnd   = _&RecEnd ;
       end ;
      /*
         Likewise, if this is our last value of the last var on our BY list, we are about to start a new period.
         Spit out the record--the new period vars get initialized above in the "if first.&LastVar..."
         block.
      */
      if last.&LastVar then do ;
         %if &Debug = 1 %then %do ;
            put "Last &LastVar:           " _N_ = PeriodStart =  _&RecStart =  PeriodEnd =  _&RecEnd = ;
         %end ;
         output ;
      end ;
   run ;
  ** Now we have the actual start/stop dates in PeriodStart & PeriodEnd--rename those to ;
  ** the original record start/stop variable names, and strip out any wacky recs where start comes after end ;
  data &outset ;
    length &__PIDLenSpec &RecStart &RecEnd 4 ;
    set &outset(rename = (PeriodStart = &RecStart
                          PeriodEnd   = &RecEnd)) ;
    ** if PeriodStart le PeriodEnd ;
    drop _&RecStart _&RecEnd ;
  run ;
  %** This is obscure, but seems like good hygeine.  Tyler called cp twice, the second time with a dset that had nothing but mrn, start and stop. ;
  %** Looks like the second call did not overwrite the value in varlist, and he got errors about named vars not being present. ;
  %** So now we null out the var to keep that from happening. ;
  %let VarList = ;
%mend CollapsePeriods ;

%macro GetFollowUpTime(People    /* Dset of MRNs */
               , IndexDate       /* Name of a date var in &People, or else a
                                    date literal, marking the start of the
                                    follow-up period. */
               , EndDate         /* Name of a date var in &People, or else a
                                    complete date literal, marking the end of
                                    the period of interest. */
               , GapTolerance    /* Number of days disenrollment to ignore in
                                    deciding the disenrollment date. */
               , CallEndDateVar  /* What name should we give the date var that
                                    will hold the end of the f/up period? */
               , OutSet          /* The name of the output dataset. */
               , DebugOut = work /* Libname to save interim dsets to for debugging--leave set to work to discard these. */
               , EnrollDset = &_vdw_enroll /* Supply your own enroll data if you like. */
               , Reverse = 0     /* **(JW 30DEC2009) Look backwards from IndexDate? 1=Reverse */
                 ) ;

   %put ;
   %put ;
   %put ============================================================== ;
   %put ;
   %put Macro GetFollowUpTime V0.91 (REVISED for Reverse look by JW):  ;
   %put ;
   %put Creating a dset "&OutSet", which will look just like "&People" except  ;
   %put that it will have an additional variable "&CallEndDateVar", which will ;
   %put hold the earliest of date-of-last-enrollment, or &EndDate (or, if the  ;
   %put person was not enrolled at all a missing value). ;
   %put ;
   %put THIS IS BETA SOFTWARE-PLEASE SCRUTINIZE THE RESULTS AND REPORT PROBLEMS;
   %put ;
   %put ============================================================== ;
   %put ;
   %put ;




   proc sql noprint ;


      %** Grab ENROLL recs for our ppl of interest where the periods overlap the period between &IndexDate and EndDate ;
      create table &DebugOut..__enroll as
      select p.mrn
            , e.enr_start
            , e.enr_end
            , &IndexDate as idate format = mmddyy10.
            , &EndDate   as edate format = mmddyy10.
      from  &People as p INNER JOIN
            &EnrollDset as e
      on    p.MRN = e.MRN

      %IF &Reverse.=1 %THEN %DO;
      %**(JCW 18FEB2010);
          where intnx('day', &EndDate.  , -&GapTolerance, 'sameday') <= e.enr_end
            and intnx('day', &IndexDate.,  &GapTolerance, 'sameday') >= e.enr_start
      %END;
      %ELSE %DO;
          where intnx('day', &IndexDate., -&GapTolerance, 'sameday') <= e.enr_end
            AND intnx('day', &EndDate.  ,  &GapTolerance, 'sameday') >= e.enr_start
      %END;

      order by mrn,

      %IF &Reverse.=1 %THEN %DO;
      %**(JCW 30DEC2009);
          enr_end DESC
      %END;
      %ELSE %DO;
          enr_start
      %END;

      ;
   quit ;

  data &debugout..__pre_collapse_enroll ;
    set &debugout..__enroll ;
  run ;

   *** Collapse contiguous periods down. ;
   %CollapsePeriods(Lib      = &DebugOut     /* Name of the library containing the dset you want collapsed */
                  , DSet     = __enroll      /* Name of the dset you want collapsed. */
                  , RecStart = enr_start     /* Name of the var that contains the period start dates. */
                  , RecEnd   = enr_end       /* Name of the var that contains the period end dates. */
                  , PersonID = MRN
                  , DaysTol  = &GapTolerance /* The number of days gap to tolerate in evaluating whether one period is contiguous w/another. */
                  ) ;

  ** Handle the (I would hope rare) case where someone is not enrolled on their index ;
  ** date, and the start of their enrollment is more than &GapTolerance days away. ;
  proc sql ;
    delete from &DebugOut..__enroll
    %IF &Reverse.=1 %THEN %DO;
      where (idate-enr_end) gt &GapTolerance
    %END;
    %ELSE %DO;
      where (enr_start - idate) gt &GapTolerance
    %END;
    ;
  quit ;


   ** The end of contiguous enrollment is enr_end on the rec w/the earliest enr_start ;
   proc sort data = &DebugOut..__enroll out = &DebugOut..__collapsed_enroll ;
      by mrn enr_start ;
   run ;

   proc sort nodupkey data = &DebugOut..__collapsed_enroll out = &DebugOut..__first_periods ;
      by mrn ;
   run ;

   proc sql ;
      create table &OutSet as
      select p.* ,

      %IF &Reverse.=1 %THEN %DO;
      %**(JCW 30DEC2009);
        max(e.edate, e.enr_start)
      %END;
      %ELSE %DO;
        min(e.edate, e.enr_end)
      %END;
      as &CallEndDateVar format = mmddyy10.

      from  &People as p LEFT JOIN
            &DebugOut..__first_periods as e
      on    p.mrn = e.mrn
      ;
   quit ;

%mend GetFollowUpTime;

%macro PrettyVar(VarName) ;
  %local __allups ;
  %local __4thups ;
  %local __delims ;
  %local __upprefixes ;

   %let __allups = %str('Po', 'Ne', 'Nw', 'Se', 'Sw', 'N.e.', 'N.w.', 'S.e.'
                      , 'S.w.', 'C/o', 'P.o.', 'P.o', 'Ii', 'Iii', 'Iv', 'Mlk', 'Us') ;
   %let __4thUps = %str('Maccoll', 'Maccubbin', 'Macdonald', 'Macdougall'
                      , 'Macgregor', 'Macintyre', 'Mackenzie', 'Macmenigall'
                      , 'Macneil', 'Macneill') ;
   %** These signify apartment numbers like #3A ;
   %let __upprefixes = %str('#', '1', '2', '3', '4'
                          , '5', '6', '7', '8', '9', '0') ;

   %let __delims = " -'." ;

   __i            = 0 ;
   __word         = '~' ;
   __pretty_var   = '' ;
   __propcased    = propcase(&VarName, &__delims) ;

   * We have had a lot of backticks & double-quotes in our address components at GHC. ;
   __propcased    = compress(__propcased, '`"') ;

   do while (__word ne '') ;
      * put __word = ;
      __i + 1 ;

      * This will eat any delimiters other than the space char. ;
      __word = scan(__propcased, __i, " ")  ;

      %* TODO: Apply address-word-regularizing format to __word here. ;

      if __word in(&__allups) then do ;
         __word = upcase(__word) ;
      end ;

      if __word in(&__4thUps) then do ;
         substr(__word, 4, 1) = upcase(substr(__word, 4, 1)) ;
      end ;

      * Try out upcasing the 3rd char for any word beginning with Mc ;
      if substr(__word, 1, 2) = 'Mc' then do ;
         substr(__word, 3, 1) = upcase(substr(__word, 3, 1)) ;
      end ;

      * Pound symbols signify apt. numbers--upcase those. ;
      /* if substr(__word, 1, 1) in (&__upprefixes)
          and reverse(substr(reverse(compress(__word)), 1, 2))
              not in (&__downsuffixes) then do ; */
      if substr(__word, 1, 1) in (&__upprefixes)  then do ;
         if prxmatch(__ordinal_regex, __word) > 0 then do ;
            * This is a 22nd, 34th type thing--leave it alone ;
         end ;
         else do ;
            * This is most likely an apartment number--upcase it. ;
            __word = upcase(__word) ;
         end ;
      end ;

      __pretty_var = compbl(__pretty_var || __word || ' ') ;

   end ;

   &VarName = left(__pretty_var) ;

%mend PrettyVar ;

%macro PrettyCase(InSet = , OutSet = , VarList = , MaxLength = 500) ;
   data &OutSet ;
      length __word __propcased __pretty_var $ &MaxLength ;
      retain __ordinal_regex ;
      set &InSet ;
      if _n_ = 1 then do ;
         %* This matches things like 21st, 5th, etc. ;
         __ordinal_regex = prxparse("/\d+(st|nd|rd|th)/i") ;
      end ;
      %local i ;
      %local ThisVar ;
      %let i = 0 ;
      %let ThisVar = ~ ;
      %do %until(%length(&ThisVar) = 0) ;
         %let i = %eval(&i + 1) ;
         %let ThisVar = %scan(&VarList, &i) ;
         %put Working on &ThisVar %length(&ThisVar) ;
         %if %length(&ThisVar) gt 0 %then %do ;
            %PrettyVar(VarName = &ThisVar) ;
         %end ;
      %end ;

      drop __: ;
   run ;
%mend PrettyCase ;

%macro GetCensusForPeople(InSet  , OutSet  ) ;
 /*Removed the year parameter so that vdw_census will always point
at the standard vars reference. DLK 08-19-2010 */
   proc sql ;
      create table &OutSet (drop = _mrn) as
      select *
      from  &InSet as i LEFT JOIN
            &_vdw_census (rename = (mrn = _mrn)) as c
      on    i.mrn = c._mrn
      ;
   quit ;

%mend GetCensusForPeople ;

%macro CleanRx(OutLib, Clean=N, Dirty=N, Report=Y);
/***************************************************************************
* Parameters:
*   OutLib  = The library name you've already declared where you want output
*             you elect to save (Clean="Y", Dirty="Y") to go.
*   Clean   = "Y" will output a table (in OutLib) with Rx fills deemed clean.
*             Any other value will not output this table.
*   Dirty   = "Y" will output a table (in Outlib) with Rx fills deemed dirty.
*             along with DirtyReason, a text variable explaining why the record
*             is dirty.  Any other value will not output this file.
*   Report  = "Y" will do a freq tabulation on the dirty data by DirtyReason,
*             report misspecified variable lengths, and perform freq tables on
*             the clean data.
*             Any other value will suppress this calculation.
*
* Programmer:
*   Tyler Ross
*   Center For Health Studies
*   (206) 287-2927
*   ross.t@ghc.org
*
* History:
*   Created August 1, 2006
**************************************************************************/

  /*Catch Errors*/
  %if &Clean ^= Y AND &Dirty ^= Y AND &Report ^= Y %then %do;
    %put ERROR: YOU MUST SPECIFY AT LEAST ONE TABLE TO OUTPUT OR TO PRODUCE;
    %put ERROR: A REPORT. SET <<CLEAN>>, <<DIRTY>>, AND/OR <<REPORT>> TO "Y";
  %end;
  %else %do;
    %local DataStatement ;
    %local DirtyReturn ;
    %local CleanReturn ;
    /*This mess is so that we save a little IO time depending on whether
      programmer wants the datasets saved.*/
    %if &Report ^= Y AND &Clean ^= Y %then %do;
      %let DataStatement = &OutLib..Dirty;
      %let DirtyReturn   = output &Outlib..dirty;
      %let CleanReturn   = ;
    %end;
    %else %if &Report ^= Y AND &Dirty ^= Y %then %do;
      %let DataStatement = &OutLib..Clean (drop=DirtyReason);
      %let DirtyReturn = ;
      %let CleanReturn = output &Outlib..clean;
    %end;
    %else %if &Report = Y AND &Clean ^= Y AND &Dirty ^= Y %then %do;
      %let DataStatement = Clean (drop=DirtyReason) Dirty;
      %let DirtyReturn = output dirty;
      %let CleanReturn = output clean;
    %end;
    %else %if &Report = Y AND &Clean = Y AND &Dirty ^= Y %then %do;
      %let DataStatement = &Outlib..Clean (drop=DirtyReason) Dirty;
      %let DirtyReturn = output dirty;
      %let CleanReturn = output &Outlib..clean;
    %end;
    %else %if &Report = Y AND &Clean ^= Y AND &Dirty = Y %then %do;
      %let DataStatement = Clean (drop=DirtyReason) &Outlib..Dirty;
      %let DirtyReturn = output &Outlib..dirty;
      %let CleanReturn = output clean;
    %end;
    %else %do; /*They want both clean and dirty, regardless of report*/
      %let DataStatement = &Outlib..Clean (drop=DirtyReason) &Outlib..Dirty;
      %let DirtyReturn = output &Outlib..dirty;
      %let CleanReturn = output &Outlib..clean;
    %end;

    /*Clean the data*/

    proc sort data=&_vdw_rx out=ToClean;
      by mrn rxdate ndc;
    run;

    data &DataStatement;
      set ToClean;
      by mrn rxdate ndc;
      length DirtyReason $30;

      if MISSING(MRN)=1 then do;
        DirtyReason = "Missing MRN";
        &DirtyReturn;
      end;
      else if MISSING(RxDate)=1 then do;
        DirtyReason = "Missing RxDate";
        &DirtyReturn;
      end;
      else if MISSING(NDC)=1 then do;
        DirtyReason = "Missing NDC";
        &DirtyReturn;
      end;
      else if rxdate > "&Sysdate."d then do;
        DirtyReason = "Dispense date in the future";
        &DirtyReturn;
      end;
      else if length(NDC) ^= 11 then do;
        DirtyReason = "NDC is improper length";
        &DirtyReturn;
      end;
      else if length(compress(NDC,'1234567890', 'k')) ^= 11 then do;
        DirtyReason = "NDC has non numeric values";
        &DirtyReturn;
      end;
      else if (MISSING(RxSup)=0 AND RxSup <= 0) then do;
        DirtyReason = "RxSup is non-positive";
        &DirtyReturn;
      end;
      else if (MISSING(RxAmt)=0 AND RxAmt <= 0) then do;
        DirtyReason = "RxAmt is non-positive";
        &DirtyReturn;
      end;
      else if (first.MRN=0 AND first.RxDate=0 AND first.NDC=0) then do;
        DirtyReason = "Duplicate MRN, RxDate, NDC";
        &DirtyReturn;
      end;
      else do;
        &CleanReturn;
      end;
    run;
    %if &Report = Y %then %do;
      proc format;
       	value RXSUPf
       	  low -  0 = 'Less than zero'
       	  0 <-< 28 = '1 to 27'
       	  28 - 32  = '28 to 32'
       	  32 <-< 58 = '33 to 57'
          58 - 63 = '58 to 63'
       	  63 <-< 88 = '64 to 87'
       	  88 - 95   = '88 to 95'
       	  95 <- high = 'More than 95'
       	;
        value RXAMTf
       	  low - 0 = 'Less than zero'
       	  0 <- 20 = '1 to 20'
       	  20 <- 40 = '21 to 40'
          40 <- 60 = '41 to 60'
       	  60 <- 80 = '61 to 80'
       	  80 <- 100 = '81 to 100'
       	  100 <- 200 = '101 to 200'
       	  200 <- high = 'More than 200'
       	;
      run;
      proc freq data= %if(&Clean=Y) %then &Outlib..Clean; %else Clean;;
        title "Frequency Distributions of Obs That Are Clean";
        format RxDate Year. RXSUP RXSUPf. RXAMT RXAMTf.;
        table RxDate RXSUP RXAMT /missing;
      run;
      proc freq data= %if(&Dirty=Y) %then &Outlib..Dirty noprint;
                      %else Dirty noprint;;
        table DirtyReason / out=DirtyReport;
      run;
      proc contents data=&_vdw_rx out=RxContents noprint; run;
      data WrongLength (keep=vname YourLength SpecLength);
        set RxContents;
        length vname $32. YourLength 8 SpecLength 8;
        vname = upcase(compress(name));
        if vname='MRN' then do;
          call symput('TotalRecords', compress(nobs));
          return;
        end;
        else if vname="RXDATE" AND length^=4 then do;
          YourLength=length;
          SpecLength=4;
          output;
        end;
        else if vname="NDC" AND length^=11 then do;
          YourLength=length;
          SpecLength=11;
          output;
        end;
        else if vname="RXSUP" AND length^=4 then do;
          YourLength=length;
          SpecLength=4;
          output;
        end;
        else if vname="RXAMT" AND length^=4 then do;
          YourLength=length;
          SpecLength=4;
          output;
        end;
        else return;
      run;
      *This should not error nor print if WrongLength is empty;
      proc print data=WrongLength;
        title "Table of Variables Having the Wrong Length";
      run;
      title "Frequency of Observations Not up to Specs by Reason";
      proc sql;
        select DirtyReason
             , COUNT as Frequency
             , COUNT/&TotalRecords. *100 as PercentOfAllRx
             , Percent as PercentOfDirtyRx
        from DirtyReport
        ;
      quit;
    %end;
  %end;
%mend CleanRx;

%macro GetVitalSignsForPeople (
              People  /* The name of a dataset containing the people whose
                           vitals you want*/
            , StartDt /* The date on which you want to start collecting vitals*/
            , EndDt   /* The date on which you want to stop collecting vitals */
            , Outset  /* The name of the output dataset containing the vitals */
            ) ;
   *Author: Tyler Ross, ross.t@ghc.org , 206-287-2927;


   /*Catch and Throw*/
   %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
   %end ;
   %else %if %sysfunc(abs("&StartDt"d > "&EndDt"d))=1 %then %do ;
     %put PROBLEM: The start date you entered occurrs after the end date ;
     %put PROBLEM: Start date is "&StartDt." and end date is "&EndDt." ;
     %put PROBLEM: Doing nothing. ;
   %end ;
   %else %if %sysfunc(exist(&People))=0 %then %do;
     %put PROBLEM: The People dataset (&People.) does not exist. ;
     %put PROBLEM: Doing nothing. ;
   %end;
   %else %do;
   /*Get Vitals*/
     proc sql;
       create table &OutSet. as
         select v.*
         from &People as p
           INNER JOIN
              &_vdw_vitalsigns as v
         on p.mrn = v.mrn
         where v.Measure_Date BETWEEN "&StartDt"d AND "&EndDt"d
       ;
     quit;
   %end;
%mend GetVitalSignsForPeople;

/********************************************************************
* TESTING GetVitalSignsForPeople;
*
* *GRAB TEST PEOPLE;
* proc sql;
*   create table Afew as select distinct mrn from vdw.&_VitalData
*     where Measure_Date between '01May2005'd AND '15May2005'd;
* quit;
* *Problem 1;
* %*GetVitalSignsForPeople(afew,   05May2005, 10May2005, afew);
* *Problem 2;
* %*GetVitalSignsForPeople(afew,   05May2005, 01Jan1999, myout);
* *Problem 3;
* %*GetVitalSignsForPeople(nodata, 05May2005, 10May2005, myout);
* *No problems;
* %*GetVitalSignsForPeople(afew,   05May2005, 10May2005, myout);
*
**********************************************************************/


%macro Diabetes_Charlson(outfile, startdate, enddate, EncType = A);
/***************************************************************************
****************************************************************************
* Programmer:
*   Tyler Ross
*   Center For Health Studies
*   (206) 287-2927
*   ross.t@ghc.org
*
* History:  Created September 15, 2006
*
* Purpose:
*   For the diabetes dx as defined by Charlson, this macro creates
*    - A dataset called Diabetes_Charlson with the dx and descriptions
*    - A format called Diabetes_Charlson with the dx
*    - A dataset called &outfile with all people having diabetes
*
* Parameters:
*   Outfile = the file that will contain the list of MRN of those with diabetes
*   StartDate = Date from which you want to start looking for diabetes dx
*   EndDate   = Date from which you want to stop looking for diabetes dx
*   EncType   = Value of A will search All encounters (default),
*               Value of I will search only Inpatient encounters
*               Value of B will search Both IP and OP for dx (but not others)
*
* Dependencies:
*   The Dx file (with the EncType variable as the char(2) version if you use
*                EncType = I or B options)
*   A call to input standard vars before running the macro
*
***************************************************************************
**************************************************************************/

**Catch and Throw;
%let EncType = %upcase(&EncType.);
%if (&EncType.^= A AND &EncType. ^= I AND &EncType. ^= B) %then %do;
  %put PROBLEM: The parameter 'Inpatient' must be among 'A', 'I', or 'B';
  %put PROBLEM: Doing Nothing;
  %goto exit;
%end;
%else %if %sysfunc(abs("&StartDate"d > "&EndDate"d))=1 %then %do;
  %put PROBLEM: The Startdate must be on or before the EndDate;
  %put PROBLEM: Doing Nothing;
  %goto exit;
%end;

/**************************************
*From the Charlson Macro
***Diabetess;
     "250   "-"250.33",
	   "250.7 "-"250.73" = "DIAB"
***Diabetes with chronic complications
	   "250.4 "-"250.63" = "DIABC"
**************************************/



* TODO: Break this out into a separate macro that just defines the format, which we can share with the charlson macro--keep it DRY. ;
proc format;
  value $Diabetes_Charlson
     "250   "-"250.33",
	   "250.7 "-"250.73",
	   "250.4 "-"250.63"  = "DIABC"
  ;
run;

data Diabetes_Charlson;
*Note - Datalines are not allowed in macros;
  length diabetes_dx $6 description $50;

diabetes_dx="250"   ; description="DIABETES MELLITUS"          ; output;
*Just in case lets throw one in with the decimal;
diabetes_dx="250."  ; description="DIABETES MELLITUS"          ; output;
diabetes_dx="250.22"; description="DM2/NOS W HYPEROSMOL UNC"   ; output;
diabetes_dx="250.50"; description="DM2/NOS W EYE MANIF NSU"    ; output;
diabetes_dx="250.0" ; description="DIABETES MELLITUS UNCOMP"   ; output;
diabetes_dx="250.23"; description="DM1 HYPEROSMOLARITY UNC"    ; output;
diabetes_dx="250.51"; description="DM1 W EYE MANIFEST NSU"     ; output;
diabetes_dx="250.00"; description="DM2/NOS UNCOMP NSU"         ; output;
diabetes_dx="250.29"; description="Unspec: adult-onset vs juvenile type";output;
diabetes_dx="250.52"; description="DM2/NOS W EYE MANIF UNC"    ; output;
diabetes_dx="250.01"; description="DM1 UNCOMP NSU"             ; output;
diabetes_dx="250.3" ; description="DIABETES W COMA NEC"        ; output;
diabetes_dx="250.53"; description="DM1 W EYE MANIFEST UNC"     ; output;
diabetes_dx="250.02"; description="DM2/NOS UNCOMP UNC"         ; output;
diabetes_dx="250.30"; description="DM2/NOS W COMA NEC NSU"     ; output;
diabetes_dx="250.59"; description="Unspec: adult-onset vs juvenile type";output;
diabetes_dx="250.03"; description="DM1 UNCOMP UNC"             ; output;
diabetes_dx="250.31"; description="DM1 W COMA NEC NSU"         ; output;
diabetes_dx="250.6" ; description="DM2 NEUROLOGIC MANIFEST"    ; output;
diabetes_dx="250.09"; description="Unspec: adult-onset vs juvenile type";output;
diabetes_dx="503.2" ; description="DM2/NOS W COMA NEC UNC"     ; output;
diabetes_dx="250.60"; description="DM2/NOS W NEUR MANIF NSU"   ; output;
diabetes_dx="250.1" ; description="DIABETES W KETOACIDOSIS"    ; output;
diabetes_dx="250.33"; description="DM1 W COMA NEC UNC"         ; output;
diabetes_dx="250.61"; description="DM1 W NEURO MANIFEST NSU"   ; output;
diabetes_dx="250.10"; description="DM2/NOS W KETOACID NSU"     ; output;
diabetes_dx="250.4" ; description="DM W RENAL MANIFESTATION"   ; output;
diabetes_dx="250.62"; description="DM2/NOS W NEUR MANIF UNC"   ; output;
diabetes_dx="250.11"; description="DM1 W KETOACIDOSIS NSU"     ; output;
diabetes_dx="250.40"; description="DM2/NOS W REN MANIF NSU"    ; output;
diabetes_dx="250.63"; description="DM1 W NEURO MANIFEST UNC"   ; output;
diabetes_dx="250.12"; description="DM2/NOS W KETOACID UNC"     ; output;
diabetes_dx="250.41"; description="DM1 W RENAL MANIFEST NSU"   ; output;
diabetes_dx="250.7" ; description="DM W CIRC DISORDER"         ; output;
diabetes_dx="250.13"; description="DM1 W KETOACIDOSIS UNC"     ; output;
diabetes_dx="250.42"; description="DM2/NOS W REN MANIF UNC"    ; output;
diabetes_dx="250.70"; description="DM2/NOS W CIRC DIS NSU"     ; output;
diabetes_dx="250.19"; description="Unspec: adult-onset vs juvenile type";output;
diabetes_dx="250.43"; description="DM1 W RENAL MANIFEST UNC"   ; output;
diabetes_dx="250.71"; description="DM1 W CIRC DISORD NSU"      ; output;
diabetes_dx="250.2" ; description="DM W HYPEROSMOLARITY"       ; output;
diabetes_dx="250.49"; description="Unspec: adult-onset vs juvenile type";output;
diabetes_dx="250.72"; description="DM2/NOS W CIRC DIS UNC"     ; output;
diabetes_dx="250.20"; description="DM2/NOS W HYPEROSMOL NSU"   ; output;
diabetes_dx="250.5" ; description="DM W OPHTHALMIC MANIFEST"   ; output;
diabetes_dx="250.73"; description="DM1 W CIRC DISORD UNC"      ; output;
diabetes_dx="250.21"; description="DM1 HYPEROSMOLARITY NSU";   ; output;
run;

proc sql noprint;
  create table &outfile as
    select distinct mrn
    from &_vdw_dx
    where dx in(select diabetes_dx from diabetes_charlson)
      AND adate between "&startdate"d AND "&EndDate"d
%if       %upcase(&EncType.) = I %then AND EncType = "IP";
%else %if %upcase(&EncType.) = B %then AND EncType in("AV", "IP");
  ;
quit;

%exit: %mend Diabetes_Charlson;

/*TEST SECTION;
*Problem 1;
%Diabetes_Charlson(outfile=MyTest, startdate=01May2004, enddate=15May2004
                 , EncType = Z);
*Problem 2;
%Diabetes_Charlson(outfile=MyTest, startdate=15May2004, enddate=01Mar2002
                 , EncType = A);
*Success 1;
%Diabetes_Charlson(outfile=MyTest, startdate=01May2004, enddate=15May2004
                 , EncType = A);
*Success 2;
%Diabetes_Charlson(outfile=MyTest, startdate=01May2004, enddate=15May2004
                 , EncType = I);
*Success 3;
%Diabetes_Charlson(outfile=MyTest, startdate=01May2004, enddate=15May2004
                 , EncType = B);
*/



%macro GetDateRange(path, filename, print=1);
/***************************************************************************
****************************************************************************
* Programmer:
*   Tyler Ross
*   Center For Health Studies
*   (206) 287-2927
*   ross.t@ghc.org
*
* History:
*   Created Sept 18, 2006
*
* Purpose:
*   For every variable in path.filename that has a date format, this macro
*     creates two global macro variables in date9 format with the names
*     TheDateVar_Min and TheDateVar_Max
*   You may optionally print the results to the lst file
*
* Parameters:
*   Path = The path name to the data file (which will get called as a libname)
*   Filename = The name of the data set
*   Print = Set to 0 will supress the date ranges printed to screen
*           Set to 1 (default) will show all date vars min and max values
*
* Examples:
*   %GetDateRange(&_TumorLib., &_TumorData.);
*   ...will create the global variables
*     DOD_Max, DOD_Min, BDate_Max, BDate_Min, DxDate_Max, DxDate_Min,
*     DT_Surg_Max, DT_Surg_Min and so forth where...
*   &DOD_MAX = 09Sep2006
*   &DOD_Min = 01Feb1982
*   and so forth
*
***************************************************************************
**************************************************************************/
  libname __PATH "&path.";

  %if %sysfunc(exist(__Path.&filename.)) = 0 %then %do;
    %put PROBLEM: The file &filename. does not exist in the path you specified;
    %put PROBLEM: Path = &path.;
    %put PROBLEM: DOING NOTHING;
    %goto exit;
  %end;
  %else %if (&print. ^=0 AND &print. ^=1) %then %do;
    %put PROBLEM: The print parameter must be equal to zero (0) or one (1);
    %put PROBLEM: DOING NOTHING;
    %goto exit;
  %end;

  *Go through the select twice -once for the globals that will be made;
  *  Once for the locals for the summary proc;
  proc sql noprint;
      select compress(name) || "_Max " || compress(name) || "_Min"
             into: ForGlobals separated by " "
      from dictionary.columns
      where upcase(compress(type))    = "NUM"
        AND upcase(compress(libname)) = "__PATH"
        AND upcase(compress(MemName)) = upcase("&filename")
        AND (
             index(upcase(format), "DATE") > 0
            OR
             index(upcase(format), "YY") > 0
            OR
             index(upcase(format), "JULIAN") > 0
            )
    ;
    select name into: DateVars_&filename. separated by " "
      from dictionary.columns
      where upcase(compress(type))    = "NUM"
        AND upcase(compress(libname)) = "__PATH"
        AND upcase(compress(MemName)) = upcase("&filename")
        AND (
             index(upcase(format), "DATE") > 0
            OR
             index(upcase(format), "YY") > 0
            OR
             index(upcase(format), "JULIAN") > 0
            )
    ;
  quit;

  *Verify that the macro variable exists (that there is at least one date var);
  %if %symexist(DateVars_&filename.) %then %do;

    %put The date variables in &filename. are &&DateVars_&filename;
    *Get the min and max of the date vars;
    proc summary data= __Path.&filename. noprint min max;
      var &&DateVars_&filename;
      output out=Ranges;
    run;
    *Make MAX come before MIN;
    proc sort data=Ranges; by _STAT_; run;

    *Allow user to see the results in the .lst file;
    %if &print. = 1 %then %do;
     proc print data=Ranges;
     title "The minimum and maximum values of the date variables in &filename.";
       where upcase(compress(_STAT_)) in("MIN", "MAX");
     run;
    %end;

    *Declare the variables as global - call symput will default to local o.w.;
    %global &ForGlobals;
    *Create local variables holding the min and max values;
    data _NULL_;
      set Ranges (where=(upcase(compress(_STAT_)) in("MIN", "MAX")));

      array datevars {*} _NUMERIC_ ;
      if _n_ = 1 then do;
        do i=1 to dim(datevars);
          if vname(datevars{i}) NOT IN("_TYPE_", "_FREQ_", "_STAT_") then
            call symput(vname(datevars{i}) || "_Max", put(datevars{i}, date9.));
        end;
      end;
      else do;
        do i=1 to dim(datevars);
          if vname(datevars{i}) NOT IN("_TYPE_", "_FREQ_", "_STAT_") then
            call symput(vname(datevars{i}) || "_Min", put(datevars{i}, date9.));
        end;
      end;
    run;

    *Clean up;
    proc sql;
      drop table Ranges;
    quit;
  %end;
  %else %do;
    %put PROBLEM: Sorry, but no date variables were found in &filename;
    %put PROBLEM: Verify that &filename. has at least one numeric variable;
    %put PROBLEM: formatted as a date variable;
    %goto exit;
  %end;

%exit: %mend GetDateRange;

*TEST SECTION;
/*******************
* Raise exceptions *
*******************/
*Print var out of range;
%*GetDateRange(&_RxLib. , &_RxData., print=2);
*Data that doesnt exist;
%*GetDateRange(&_RxLib. , NotReal);
*A file with no date variables;
%*GetDateRange(&_RxLib. , &_EverNDCData);

*NOW FOR SUCCESSFUL RUNS;
%*GetDateRange(&_UtilizationLib. , &_DXDATA.   , print=0);
%*GetDateRange(&_TumorLib.       , &_TUMORDATA., print=1);
%*GetDateRange(&_RxLib.          , &_RxData.   , print=1);
%*GetDateRange(&_VitalLib.       , &_VitalData          );

%*put _user_;


%macro Hypertension_BP(outfile, startdate, enddate,
                       Diastolic_Min = 90, Systolic_Min = 140,
                       Strict_Equality = 0, Either = 1);

/***************************************************************************
****************************************************************************
* Programmer:
*   Tyler Ross
*   Center For Health Studies
*   (206) 287-2927
*   ross.t@ghc.org
*
* History: Created September 27, 2006

* Purpose:
*   Pulls all people with a systolic and-or diastolic BP reading above
*     specified threasholds over specified dates along with their highest
*     systolic and diastolic readings in that period. Can be used to defined
*     hypertension.
*
* Parameters:
*   Outfile = The name of the file that will be output
*   StartDate = The date from which you want to start looking for BP
*   EndDate   = The date to which you want to end looking for BP
*   Diastolic_Min = The minimum diastolic value that will be allowed in output
*   Systolic_Min  = The minimum systolic  value that will be allowed in output
*   Strict_Equality = 0 allows BP readings of min values and above
*                     1 only allows BP readings above the min values
*   Either = 0 requires a systolic AND a diastolic reading above the min
*            1 allows either a systolic OR a diastolic reading above the min
*
* Notes:
*   Systolic and diastolic readings above mins are not required to be on
*   the same day when Either = 0 is specified.
*
***************************************************************************
**************************************************************************/

%if %sysfunc(abs("&StartDate"d > "&EndDate"d))=1 %then %do;
  %put PROBLEM: The StartDate must be on or before the EndDate;
  %put StartDate is &StartDate., EndDate is &EndDate.;
  %put PROBLEM: Doing Nothing;
  %goto exit;
%end;
%if (&Diastolic_Min < 0 OR &Systolic_Min < 0) %then %do;
  %put PROBLEM: The min values for BP must be non-negative;
  %put Diastolic_Min = &Diastolic_Min. , Systolic_Min = &Systolic_Min.;
  %put PROBLEM: Doing Nothing;
  %goto exit;
%end;
%if (&Strict_Equality. ^= 0 AND &Strict_Equality. ^= 1) %then %do;
  %put PROBLEM: The Strict_Equality variable must be 0 or 1;
  %put Strict_Equality = &Strict_Equality;
  %put PROBLEM: Doing Nothing;
  %goto exit;
%end;
%if (&Either. ^= 0 AND &Either. ^= 1) %then %do;
  %put PROBLEM: The Either variable must be 0 or 1;
  %put Either = &Either;
  %put PROBLEM: Doing Nothing;
  %goto exit;
%end;

*Create conditional;
%local conditional ;
%if (&Strict_Equality. = 0 AND &Either. = 1) %then %do;
  %let Conditional= max(Diastolic) >= &Diastolic_Min.
                 OR max(Systolic)  >= &Systolic_Min.;
%end;
%else %if (&Strict_Equality. = 1 AND &Either. = 1) %then %do;
  %let Conditional= max(Diastolic) > &Diastolic_Min.
                 OR max(Systolic)  > &Systolic_Min.;
%end;
%else %if (&Strict_Equality. = 0 AND &Either. = 0) %then %do;
  %let Conditional= max(Diastolic) >= &Diastolic_Min.
                AND max(Systolic)  >= &Systolic_Min.;
%end;
%else %if (&Strict_Equality. = 1 AND &Either. = 0) %then %do;
  %let Conditional= max(Diastolic) > &Diastolic_Min.
                AND max(Systolic)  > &Systolic_Min.;
%end;



proc sql;
 create table &outfile. as
   select mrn
        , max(Diastolic) as Max_Diastolic
 label = "Person's highest diastolic reading between &StartDate. and &EndDate."
        , max(Systolic)  as Max_Systolic
 label = "Person's highest systolic reading between &StartDate. and &EndDate."
   from &_vdw_vitalsigns (where=(Measure_Date between "&StartDate"d
                                                 AND "&EndDate"d  ))
   group by mrn
   having &Conditional.
 ;
quit;

%exit: %mend Hypertension_BP;

/*TEST SECTION;
proc format;
  value sysf
    low  -  0    = "Non-positive"
    0   <-< 130  = "<130"
    130  -< 140  = "130 to 139"
    140          = "140"
    140 <-< 160  = "140 to 159"
    160  -< 180  = "160 to 179"
    180  -  high = "180+"
  ;
  value diaf
    low  -  0    = "Non-positive"
    0   <-< 80   = "<80"
    80   -< 90   = "80 to 89"
    90           = "90"
    90  <-< 100  = "90 to 99"
    100  -< 110  = "100 to 110"
    110  -  high = "110+"
  ;
quit;
*Problem 1;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2002);
*Problem 2;
%Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
                 Diastolic_Min=-5, Systolic_Min=10);
*Problem 3;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
                 Strict_Equality=Y);
*Problem 4;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
                 Either=2);


*Success 1;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006);
proc freq data=testing;
  title "Success 1";
  format Max_Diastolic diaf. Max_Systolic sysf.;
  table Max_Diastolic*Max_Systolic /missing;
run;
proc sort data=testing NODUPKEY; by mrn; run;
*Success 2;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
                 Diastolic_Min=85, Systolic_Min=135);
proc freq data=testing;
  title "Success 2";
  format Max_Diastolic diaf. Max_Systolic sysf.;
  table Max_Diastolic*Max_Systolic /missing;
run;
*Success 3;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
                 Strict_Equality=1, Either=1);
proc freq data=testing;
  title "Success 3";
  format Max_Diastolic diaf. Max_Systolic sysf.;
  table Max_Diastolic*Max_Systolic /missing;
run;
*Success 4;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
                 Strict_Equality=0, Either=0);
proc freq data=testing;
  title "Success 4";
  format Max_Diastolic diaf. Max_Systolic sysf.;
  table Max_Diastolic*Max_Systolic /missing;
run;
*Success 5;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
                 Strict_Equality=1, Either=0);
proc freq data=testing;
  title "Success 5";
  format Max_Diastolic diaf. Max_Systolic sysf.;
  table Max_Diastolic*Max_Systolic /missing;
run;
*Success 6;
%*Hypertension_BP(outfile= testing, startdate= 04Jan2005, enddate= 15Feb2006,
               Diastolic_Min=90, Systolic_Min=150, Strict_Equality=1, Either=0);
proc freq data=testing;
  title "Success 6";
  format Max_Diastolic diaf. Max_Systolic sysf.;
  table Max_Diastolic*Max_Systolic /missing;
run;
*/

%macro CleanEnroll(OutLib, Clean=N, Dirty=N, Report=Y, EnrollDset = &_vdw_enroll);
/***************************************************************************
* Parameters:
*   OutLib  = The library name you've already declared where you want output
*             you elect to save (Clean="Y", Dirty="Y") to go.
*   Clean   = "Y" outputs a table (in OutLib) with enroll records deemed clean.
*             Any other value will not output this table.
*   Dirty   = "Y" outputs a table (in Outlib) with enroll records deemed dirty.
*             along with DirtyReason, a text variable explaining why the record
*             is dirty.  Any other value will not output this file.
*   Report  = "Y" will do a freq tabulation on the dirty data by DirtyReason,
*             report misspecified variable lengths, and perform freq tables on
*             the clean data.
*             Any other value will suppress this calculation.
*
* Programmer:
*   Tyler Ross
*   Center For Health Studies
*   (206) 287-2927
*   ross.t@ghc.org
*
* Modified by:
*   David Tabano
*   Institute for Health Research, Kaiser Permanente Colorado
*   (303)614-1348
*   david.c.tabano@kp.org
*
* History:
*   Created October 13, 2006
*   Modified August 23, 2011
**************************************************************************/

  /*Catch Errors*/
  %if &Clean ^= Y AND &Dirty ^= Y AND &Report ^= Y %then %do;
    %put ERROR: YOU MUST SPECIFY AT LEAST ONE TABLE TO OUTPUT OR TO PRODUCE;
    %put ERROR: A REPORT. SET <<CLEAN>>, <<DIRTY>>, AND/OR <<REPORT>> TO "Y";
  %end;
  %else %do;
    /*This mess is so that we save a little IO time depending on whether
      programmer wants the datasets saved.*/
    %local DataStatement ;
    %local DirtyReturn ;
    %local CleanReturn ;
    %if &Report ^= Y AND &Clean ^= Y %then %do;
      %let DataStatement = &OutLib..Dirty;
      %let DirtyReturn   = output &Outlib..dirty;
      %let CleanReturn   = ;
    %end;
    %else %if &Report ^= Y AND &Dirty ^= Y %then %do;
      %let DataStatement = &OutLib..Clean (drop=DirtyReason LastEnd);
      %let DirtyReturn = ;
      %let CleanReturn = output &Outlib..clean;
    %end;
    %else %if &Report = Y AND &Clean ^= Y AND &Dirty ^= Y %then %do;
      %let DataStatement = Clean (drop=DirtyReason LastEnd) Dirty;
      %let DirtyReturn = output dirty;
      %let CleanReturn = output clean;
    %end;
    %else %if &Report = Y AND &Clean = Y AND &Dirty ^= Y %then %do;
      %let DataStatement = &Outlib..Clean (drop=DirtyReason LastEnd) Dirty;
      %let DirtyReturn = output dirty;
      %let CleanReturn = output &Outlib..clean;
    %end;
    %else %if &Report = Y AND &Clean ^= Y AND &Dirty = Y %then %do;
      %let DataStatement = Clean (drop=DirtyReason LastEnd) &Outlib..Dirty;
      %let DirtyReturn = output &Outlib..dirty;
      %let CleanReturn = output clean;
    %end;
    %else %do; /*They want both clean and dirty, regardless of report*/
  %let DataStatement = &Outlib..Clean (drop=DirtyReason LastEnd) &Outlib..Dirty;
      %let DirtyReturn = output &Outlib..dirty;
      %let CleanReturn = output &Outlib..clean;
    %end;


    %** helper macro ;
    %macro checkflag(var, flag_vals = %str('Y', 'N', 'U')) ;
      else if &var NOT IN(&flag_vals) then do;
        DirtyReason = "Invalid value for &var";
        &DirtyReturn;
      end;
    %mend checkflag ;

    /*Clean the data*/

    proc sort data=&_vdw_enroll out=ToClean;
      by mrn enr_start;
    run;

    data &DataStatement;
      set &_vdw_enroll end = last ;
      by mrn enr_start;
      length DirtyReason $40 LastEnd 4 DaysEnrolled 8;

      DaysEnrolled = Enr_End - Enr_Start + 1;

      if MISSING(MRN)=1 then do;
        DirtyReason = "Missing MRN";
        &DirtyReturn;
      end;
      else if MISSING(enr_start)=1 then do;
        DirtyReason = "Missing ENR_Start";
        &DirtyReturn;
      end;
      else if MISSING(enr_end)=1 then do;
        DirtyReason = "Missing ENR_End";
        &DirtyReturn;
      end;
      else if enr_end < enr_start then do;
        DirtyReason = "Enr_end is BEFORE enr_start";
        &DirtyReturn;
      end;
      else if first.MRN = 0 AND LastEND > enr_start then do;
        DirtyReason = "Enroll period overlaps with other obs";
        &DirtyReturn;
      end;

      %checkflag(var = ins_medicare)
      %checkflag(var = ins_medicare_a)
      %checkflag(var = ins_medicare_b)
      %checkflag(var = ins_medicare_c)
      %checkflag(var = ins_medicare_d)
      %checkflag(var = ins_medicaid)
      %checkflag(var = ins_commercial)
      %checkflag(var = ins_privatepay)
      %checkflag(var = ins_selffunded)
      %checkflag(var = ins_statesubsidized)
      %checkflag(var = ins_highdeductible)
      %checkflag(var = ins_other)

      %checkflag(var = enrollment_basis, flag_vals = %str('G', 'I', 'B'))

      %checkflag(var = plan_hmo)
      %checkflag(var = plan_ppo)
      %checkflag(var = plan_pos)
      %checkflag(var = plan_indemnity)

      %checkflag(var = outside_utilization)

      %checkflag(var = drugcov)
      else do;
        &CleanReturn;
      end;
      LastEnd = enr_end;
      retain LastEnd;
      ** Putting this here b/c proc contents spits out a missing for obs when run against a view. ;
      if last then do ;
        call symput('TotalRecords', put(_n_, best.)) ;
      end ;
      format LastEnd mmddyy10. ;
    run;

    %if &Report = Y %then %do;
      proc format;
        value DEnrollf
          1           = "1 Day"
          2    - 27   = "2 to 27 days"
          28   - 31   = "28 to 31 days"
          32   - 93   = "32 to 93 days"
          94   - 186  = "94 to 186 days"
          187  - 363  = "187 to 363 days"
          364  - 366  = "364 to 366 days"
          367  - 1096 = "367 to 1096 days (3 years)"
          1096 - high = "More than 1096 days"
          other       = "Other?!"
        ;
      run;
      proc freq data= %if(&Clean=Y) %then &Outlib..Clean; %else Clean;;
        title "Frequency Distributions of Obs That Are Clean";
        format Enr_Start MMYY. Enr_End MMYY. DaysEnrolled DEnrollf.;
        table Enr_Start Enr_End DaysEnrolled Ins_Medicare Ins_Medicaid
              Ins_Commercial Ins_PrivatePay Ins_Other DRUGCOV;
      run;
      proc freq data= %if(&Dirty=Y) %then &Outlib..Dirty noprint;
                      %else Dirty noprint;;
        table DirtyReason / out=DirtyReport;
      run;
      proc contents data=&_vdw_enroll out=EnrollContents noprint;
      run;

      data WrongLength (keep=vname YourLength SpecLength);
        set EnrollContents;
        length vname $32. YourLength 8 SpecLength 8;

        vname = upcase(compress(name));

        select (vname) ;
          when ('MRN') do ;
            ** Doing this is the dsetp above b/c the below does not work if _vdw_enroll points to a view. ;
            ** call symput('TotalRecords', put(nobs, best.));
            return;
          end ;
          when ('INS_MEDICARE'
                , 'INS_MEDICAID'
                , 'INS_COMMERCIAL'
                , 'INS_PRIVATEPAY'
                , 'INS_OTHER'
                , 'DRUGCOV'
                , "INS_STATESUBSIDIZED"
                , "INS_SELFFUNDED"
                , "INS_HIGHDEDUCTIBLE"
                , "INS_MEDICARE_A"
                , "INS_MEDICARE_B"
                , "INS_MEDICARE_C"
                , "INS_MEDICARE_D"
                , "PLAN_HMO"
                , "PLAN_POS"
                , "PLAN_PPO"
                , "PLAN_INDEMNITY"
                , "OUTSIDE_UTILIZATION"
                , "ENROLLMENT_BASIS"
                ) do ;
            YourLength = length ;
            SpecLength = 1 ;
          end ;
          ** Dropping the PCC PCP length checks b/c the spec allows those to vary. ;
          otherwise do ;
            ** put "Got " vname= "--doing nothing." ;
          end ;
        end ;
        if YourLength ne SpecLength then output ;
      run ;

      **This should not error nor print if WrongLength is empty;
      proc print data=WrongLength;
        title "Table of Variables Having the Wrong Length";
      run;
      title "Frequency of Observations Not up to Specs by Reason";
      proc sql;

        select DirtyReason
             , COUNT as Frequency
             , (COUNT / &TotalRecords ) * 100 as PercentOfAllEnroll
             , Percent as PercentOfDirtyEnroll
          from DirtyReport
        ;
      quit;
    %end;
  %end;
%mend CleanEnroll;
%macro CleanVitals(OutLib, Clean=N, Dirty=N, Report=Y, Limits=N);
/***************************************************************************
* Parameters:
*   OutLib  = The library name you've already declared where you want output
*             you elect to save (Clean="Y", Dirty="Y") to go.
*   Clean   = "Y" outputs a table (in OutLib) with records deemed clean.
*             Any other value will not output this table.
*   Dirty   = "Y" outputs a table (in Outlib) with records deemed dirty.
*             along with DirtyReason, a text variable explaining why the record
*             is dirty.  Any other value will not output this file.
*   Report  = "Y" will do a freq tabulation on the dirty data by DirtyReason,
*             report misspecified variable lengths, and perform freq tables on
*             the clean data.
*             Any other value will suppress this calculation.
*   Limits  = "Y outputs a table called LIMITS (in Outlib) with only those
*             values in the vitals sign dataset that values compatible with life
*
* Programmer:
*   Tyler Ross
*   Center For Health Studies
*   (206) 287-2927
*   ross.t@ghc.org
*
* History:
*   Created January 8, 2007
**************************************************************************/

  /*Catch Errors*/
  %if &Clean ^= Y AND &Dirty ^= Y AND &Report ^= Y %then %do;
    %put ERROR: YOU MUST SPECIFY AT LEAST ONE TABLE TO OUTPUT OR TO PRODUCE;
    %put ERROR: A REPORT. SET <<CLEAN>>, <<DIRTY>>, AND/OR <<REPORT>> TO "Y";
  %end;
  %else %do;
    /*This mess is so that we save a little IO time depending on whether
      programmer wants the datasets saved.*/

    %local DataStatement ;
    %local DirtyReturn   ;
    %local CleanReturn   ;

    %if &Report ^= Y AND &Clean ^= Y %then %do;
      %let DataStatement = &OutLib..Dirty;
      %let DirtyReturn   = output &Outlib..dirty;
      %let CleanReturn   = output clean;
    %end;
    %else %if &Report ^= Y AND &Dirty ^= Y %then %do;
      %let DataStatement = &OutLib..Clean (drop=DirtyReason);
      %let DirtyReturn = ;
      %let CleanReturn = output &Outlib..clean;
    %end;
    %else %if &Report = Y AND &Clean ^= Y AND &Dirty ^= Y %then %do;
      %let DataStatement = Clean (drop=DirtyReason) Dirty;
      %let DirtyReturn = output dirty;
      %let CleanReturn = output clean;
    %end;
    %else %if &Report = Y AND &Clean = Y AND &Dirty ^= Y %then %do;
      %let DataStatement = &Outlib..Clean (drop=DirtyReason) Dirty;
      %let DirtyReturn = output dirty;
      %let CleanReturn = output &Outlib..clean;
    %end;
    %else %if &Report = Y AND &Clean ^= Y AND &Dirty = Y %then %do;
      %let DataStatement = Clean (drop=DirtyReason ) &Outlib..Dirty;
      %let DirtyReturn = output &Outlib..dirty;
      %let CleanReturn = output clean;
    %end;
    %else %do; /*They want both clean and dirty, regardless of report*/
  %let DataStatement = &Outlib..Clean (drop=DirtyReason) &Outlib..Dirty;
      %let DirtyReturn = output &Outlib..dirty;
      %let CleanReturn = output &Outlib..clean;
    %end;

    /*Clean the data*/


    **IMPUTE BMI FROM SCRATCH;
    %local verybig ;
    %LET verybig = 10000000000000000;
    data NumberOff;
      set &_vdw_vitalsigns;
      length id 8;
      id=_n_;
    run;
    proc sort data=NumberOff out=Forwards;  by MRN            Measure_Date; run;
    proc sort data=NumberOff out=Backwards; by MRN Descending Measure_Date; run;

    data forwardBMI (keep=id CDays_Diff CBMI absdiff);
      set forwards (keep=MRN HT WT Measure_Date id Days_Diff BMI);
      length
        oldht      8
    	  CDays_Diff 4
    	  olddt      4
    	  absdiff    4
    	  CBMI       8
      ;

      by mrn;

      *Calculate BMI, take old HT when HT is missing;
      if WT = . then do; CBMI = .; CDays_Diff=.; absdiff=&verybig; end;
      else do;
        if (HT^=.)  then do;
          CBMI = round((WT*0.454)/(HT*0.0254 * HT*0.0254), 0.1);
    	    CDays_Diff=0;
    	    absdiff=.;
          oldht = HT;
          olddt=Measure_Date;
        end;
        else if(oldht=.) then do; CBMI=.; CDays_Diff=.; absdiff=&verybig; end;
        else do;
          CBMI = round((WT*0.454)/(oldht*0.0254 * oldht*0.0254), 0.1);
          CDays_Diff = (olddt - Measure_Date) ;
    	    absdiff = abs(CDays_Diff);
        end;
      end;

      if last.mrn=1 then do; oldht = .; olddt=.; end;
      retain oldht olddt;
    run;

    data backwardBMI (keep=id CDays_Diff CBMI absdiff);
      set backwards (keep=MRN HT WT Measure_Date id Days_Diff BMI);
      length
        oldht      8
    	  CDays_Diff 4
    	  olddt      4
    	  absdiff    4
    	  CBMI       8
      ;

      by mrn;

      *Calculate BMI, take old HT when HT is missing;
      if WT = . then do; CBMI = .; CDays_Diff=.; absdiff=&verybig; end;
      else do;
        if (HT^=.)  then do;
          CBMI = round((WT*0.454)/(HT*0.0254 * HT*0.0254), 0.1);
    	    CDays_Diff=0;
    	    absdiff=0;
          oldht = HT;
          olddt=Measure_Date;
        end;
        else if(oldht=.) then do; CBMI=.; CDays_Diff=.; absdiff=&verybig; end;
        else do;
          CBMI = round((WT*0.454)/(oldht*0.0254 * oldht*0.0254), 0.1);
    	  /*VERY TRICKY - This line stays the same despite the backward run*/
          CDays_Diff = (olddt - Measure_Date);
    	    absdiff = abs(CDays_Diff);
        end;
      end;

      if last.mrn=1 then do; oldht = .; olddt=.; end;
      retain oldht olddt;
    run;
    /*Keep the version with the smaller date difference*/
    proc append base=backwardBMI  data=forwardBMI; run;
    proc sort   data=backwardBMI; by id absdiff; run;
    proc sort   data=backwardBMI NODUPKEY; by id; run;

    *APPEND CBMI to dataset;
    data &DataStatement;
      merge NumberOff
            BackwardBMI;
      by id;

      length DirtyReason $40 ;

      if MISSING(MRN)=1 then do;
        DirtyReason = "Missing MRN";
        &DirtyReturn;
      end;
      else if MISSING(Measure_Date)=1 then do;
        DirtyReason = "Missing Measure_Date";
        &DirtyReturn;
      end;
      else if HT < 0 AND MISSING(HT)=0 then do;
        DirtyReason = "HT is less than zero";
        &DirtyReturn;
      end;
      else if WT < 0 AND MISSING(WT)=0 then do;
        DirtyReason = "WT is less than zero";
        &DirtyReturn;
      end;
      else if Diastolic < 0 AND MISSING(Diastolic)=0 then do;
        DirtyReason = "Diastolic is less than zero";
        &DirtyReturn;
      end;
      else if Systolic < 0 AND MISSING(Systolic)=0 then do;
        DirtyReason = "Systolic is less than zero";
        &DirtyReturn;
      end;
      else if BMI < 0 AND MISSING(BMI)=0 then do;
        DirtyReason = "BMI is less than zero";
        &DirtyReturn;
      end;
      else if POSITION NOT IN("1", "2", "3", "") then do;
        DirtyReason = "Invalid value for Position";
        &DirtyReturn;
      end;
      else if round(BMI, 0.1) ^= round(CBMI, 0.1) then do;
        DirtyReason = "BMI value is imputed incorrectly";
        &DirtyReturn;
      end;
      else if Days_Diff ^= CDays_Diff then do;
        if (Days_Diff = CDays_Diff *-1 AND Days_Diff ^= 0)
          then DirtyReason = "Days_Diff is of the opposite sign";
          else DirtyReason = "Days_Diff is not correct";
        &DirtyReturn;
      end;
      else do;
        &CleanReturn;
      end;
    run;

    *MANUFACTURE REPORT;
    %if &Report = Y %then %do;
      proc format;
        value HTf
          0          = 'Exactly zero'
          0  <-  12  = '0 to 1 foot'
          12 <-  24  = '1 to 2 feet'
          24 <-  36  = '2 to 3 feet'
          36 <-  48  = '3 to 4 feet'
          48 <-  60  = '4 to 5 feet'
          60 <-  72  = '5 to 6 feet'
          72 <-  84  = '6 to 7 feet'
          84 <-  96  = '7 to 8 feet'
          96 <-  108 = '8 to 9 feet'
          108 <- 120 = '9 to 10 feet'
          120 <- high = 'Over 10 feet'
          .          = 'Missing'
          other      = 'Less than zero?!'
        ;
        value WTf
          0          = 'Zero exactly'
          0   <-  40 = '0 to 40 pounds'
          40  <-  80 = '40 to 80 pounds'
          80  <- 120 = '80 to 120 pounds'
          120 <- 160 = '120 to 160 pounds'
          160 <- 200 = '160 to 200 pounds'
          200 <- 240 = '200 to 240 pounds'
          240 <- 280 = '240 to 280 pounds'
          280 <- 320 = '280 to 320 pounds'
          320 <- 400 = '320 to 400 pounds'
          400 <- 500 = '400 to 500 pounds'
          500 <- 600 = '500 to 600 pounds'
          600 - high = 'More than 600 pounds'
          .          = 'Missing'
          other      = 'Less than zero?!'
        ;
        value datediff
         -10000 -< -5000  = '-10000 to -5000'
          -5000 -< -2500  = '-5000 to -2500'
          -2500 -< -365   = '-2500 to -365'
          -365  -< -180   = '-365 to -180'
          -180  -< -30    = '-180 to -30'
          -30   -< 0      = '-30 to 0'
          0               = '0 exactly'
          0     <-< 30    = '0 to 30'
          30     -< 180   = '30 to 180'
          180    -< 365   = '180 to 365'
          365    -< 2500  = '365 to 2500'
          2500   -< 5000  = '2500 to 5000'
          5000   -< 10000 = '5000 to 10000'
          .               = 'Missing'
          other           = 'Other'
        ;
        value systolicf
          0            = "Exactly zero"
          0   <-< 110  = "<110"
          110  -< 120  = "110 to 119"
          120  -< 130  = "120 to 129"
          130  -< 140  = "130 to 139"
          140          = "140"
          140 <-< 150  = "140 to 149"
          150  -< 160  = "150 to 159"
          160  -< 170  = "160 to 169"
          170  -< 180  = "170 to 179"
          180  -  high = "180+"
          .            = "Missing"
          other        = "Less than zero?!"
        ;
        value diastolicf
          0            = "Exactly zero"
          0   <-< 60   = "<60"
          60   -< 70   = "60 to 69"
          70   -< 80   = "70 to 79"
          80   -< 90   = "80 to 89"
          90           = "90"
          90  <-< 100  = "90 to 99"
          100  -< 110  = "100 to 109"
          110  -< 120  = "110 to 119"
          120  -< 130  = "120 to 129"
          130  -  high = "130+"
          .            = "Missing"
          other        = "Less than zero?!"
        ;
        value BMIf
  	      0             = 'Zero'
	        0    <-< 18.5 = 'Adult Underweight'
	        18.5  -< 25   = 'Adult Normal'
          25    -< 30   = 'Adult Overweight'
	        30    - 50    = 'Adult Obese to 50'
	        50   <- high  = 'Greater than 50'
	        .             = 'Missing'
	        other         = "Other?!"
        ;
        value $ positionf
          "1" = "Sitting"
          "2" = "Standing"
          "3" = "Supine"
          " "  = "Unkown"
          other = "Unexpected Value!?"
        ;
      run;
      proc freq data= %if(&Clean=Y) %then &Outlib..Clean; %else Clean;;
        title "Frequency Distributions of Obs That Are Clean";
        format HT HTf. WT WTf. Days_Diff datediff. Measure_Date year. BMI BMIf.
               systolic systolicf. diastolic diastolicf. position $positionf.;
        table HT WT BMI Days_Diff Measure_Date
              Systolic Diastolic position/missing;
      run;
      proc freq data= %if(&Dirty=Y) %then &Outlib..Dirty noprint;
                      %else Dirty noprint;;
        table DirtyReason / out=DirtyReport;
      run;
      proc contents data=&_vdw_vitalsigns out=VitalContents noprint;
      run;

      data WrongLength (keep=vname YourLength SpecLength);
        set VitalContents;
        length vname $32. YourLength 8 SpecLength 8;
        vname = upcase(compress(name));
        if vname='MRN' then do;
          call symput('TotalRecords', compress(nobs));
          return;
        end;
        else if vname="Measure_Date" AND length^=4 then do;
          YourLength=length;
          SpecLength=4;
          output;
        end;
        else if vname="HT" AND length^=8 then do;
          YourLength=length;
          SpecLength=8;
          output;
        end;
        else if vname="WT" AND length^=8 then do;
          YourLength=length;
          SpecLength=8;
          output;
        end;
        else if vname="BMI" AND length^=8 then do;
          YourLength=length;
          SpecLength=8;
          output;
        end;
        else if vname="DAYS_DIFF" AND length^=4 then do;
          YourLength=length;
          SpecLength=4;
          output;
        end;
        else if vname="DIASTOLIC" AND length^=4 then do;
          YourLength=length;
          SpecLength=4;
          output;
        end;
        else if vname="SYSTOLIC" AND length^=4 then do;
          YourLength=length;
          SpecLength=4;
          output;
        end;
        else if vname="POSITION" AND length^=1 then do;
          YourLength=length;
          SpecLength=1;
          output;
        end;
        else return;
      run;

      *This should not error nor print if WrongLength is empty;
      proc print data=WrongLength;
        title "Table of Variables Having the Wrong Length";
      run;
      title "Frequency of Observations Not up to Specs by Reason";
      proc sql;
        select DirtyReason
             , COUNT as Frequency
             , COUNT/&TotalRecords. * 100 as PercentOfAllVitalData
             , Percent as PercentOfDirtyVitalData
          from DirtyReport
        ;
      quit;
    %end;
  %end;
  %if &Limits=Y %then %do;
  %local cleaner ;
    %if &Clean=Y %then %let cleaner=&Outlib..clean;
      %else %let cleaner=clean;
    proc sql;
      create table &Outlib..LIMITS (drop=_age) as
        select a.*
             , %CalcAge(b.Birth_Date, a.Measure_Date) as _age
        from &cleaner. as a
          INNER JOIN &_vdw_demographic as b
          on a.mrn=b.mrn
        where
         (    (calculated _age = 0               AND a.HT between 3 and 41)
           OR (calculated _age between 1  and 5  AND a.HT between 12 and 60)
           OR (calculated _age between 6  and 12 AND a.HT between 20 and 84)
           OR (calculated _age between 13 and 17 AND a.HT between 30 and 108)
           OR (calculated _age >= 18             AND a.HT between 36 and 108)
           OR MISSING(a.HT)=1
         )
         AND
         (    (calculated _age = 0               AND a.WT between 0 and  80)
           OR (calculated _age between 1  and 5  AND a.WT between 9 and 200)
           OR (calculated _age between 6  and 12 AND a.WT between 20 and 350)
           OR (calculated _age between 13 and 17 AND a.WT between 25 and 650)
           OR (calculated _age >= 18             AND a.WT between 50 and 1000)
           OR MISSING(a.WT)=1
         )
         AND (a.BMI between 8 and 200        OR MISSING(a.BMI)=1)
         AND (a.Systolic between 50 and 300  OR MISSING(a.Systolic)=1)
         AND (a.Diastolic between 20 and 160 OR MISSING(a.Diastolic)=1)
      ;
    quit;
  %end;
%mend CleanVitals;

%macro SimpleContinuous(People      /* A dataset of MRNs whose enrollment we are considering. */
                     , StartDt      /* A date literal identifying the start of the period of interest. */
                     , EndDt        /* A date literal identifying the end of the period of interest. */
                     , DaysTol      /* The # of days gap between otherwise contiguous periods of enrollment (or at the beginning or end of an otherwise contiguous period) that is tolerable. */
                     , OutSet       /* Name of the desired output dset */
                     , EnrollDset = &_vdw_enroll /* For testing. */
                     ) ;

/*

   A simple macro to evaluate whether a group of people were
   continuously enrolled over a period of interest.
   Motivated by a desire for a simpler macro than VDWs
   PullContinuous().

   Produces a dset detailing the enrollment of the MRNs in &People, including a flag
   signifying whether the person was continuously enrolled between &StartDt and &EndDt.
*/

   proc sql noprint ;
      ** How many days long is the period of interest? ;
      create table dual (x char(1)) ;
      insert into dual(x) values ('x') ;
      select ("&EndDt"d - "&StartDt"d + 1) as TotDays
               into :TotDays
      from  dual ;
      drop table dual ;
   quit ;

   %put ;
   %put ;
   %put SimpleContinuous macro--pulling continuous enrollment information for the MRNs in &People ;
   %put between &StartDt and &EndDt (&TotDays days total).;
   %put ;
   %put ;


   proc sql ;
      ** Uniquify the list of MRNs, just in case ;
      create table _ids as
      select distinct MRN
      from &People ;

      ** Gather start/end dates from enroll that could possibly cover the period of interest. ;
      ** We no longer look out past the POI--now we just manually correct gaps at the beginning and end of the POI. ;
      create table _periods as
      select e.MRN
           , e.enr_start
           , e.enr_end
      from &EnrollDset as e INNER JOIN
          _ids as i
      on    e.MRN = i.MRN
      where "&StartDt"d le e.enr_end AND
            "&EndDt"d   ge e.enr_start
            ;

      /* where "&EarliestStart"d le e.enr_end AND
            "&LatestEnd"d     ge e.enr_start
      */


   ** Collapse any contiguous periods of enrollment. ;
   %CollapsePeriods(Lib       = work      /* Name of the library containing the dset you want collapsed */
                  , DSet      = _periods  /* Name of the dset you want collapsed. */
                  , RecStart  = enr_start   /* Name of the var that contains the period start dates. */
                  , RecEnd    = enr_end     /* Name of the var that contains the period end dates. */
                  , PersonID  = MRN
                  , DaysTol   = &DaysTol  /* The number of days gap to tolerate in evaluating whether one period is contiguous w/another. */
                  ) ;

  **  Now we worry about pre- and post-POI gaps. ;
  proc sort data = _periods ;
    by mrn enr_start ;
  run ;

  data _periods ;
    set _periods ;
    by mrn enr_start ;
    if first.mrn then do ;
      ** If enr_start is within &daystol days after &StartDt, we move enr_start to &StartDt (thereby closing the gap). ;
      x = (enr_start - "&StartDt"d) ;
      if 1 le (enr_start - "&StartDt"d) le &DaysTol then do ;
         ** put 'Correcting enr_start for ' mrn= ;
         enr_start = "&StartDt"d ;
       end ;
       else do ;
        ** put 'No need to correct enr_start for ' mrn= enr_start= 'diff is ' x ;
       end ;
    end ;
    if last.mrn then do ;
      ** If enr_end is within &daystol days before &EndDt, we move enr_end to &EndDt ;
      if 1 le ("&EndDt"d - enr_end) le &DaysTol then do ;
        ** put 'Correcting enr_end for ' mrn= ;
        enr_end = "&EndDt"d ;
      end ;
    end ;
    drop x ;
  run ;

   ** Calculate # of days between start & end date. ;
   proc sql ;
      create table _period_days as
      select MRN
            , (min("&EndDt"d, enr_end) - max("&StartDt"d, enr_start) + 1) as Days
      from _periods
      ;

      drop table _periods ;

      create table &OutSet(label = "Enrollment information for the MRNs in &People") as
      select mrn
            , sum(days) as CoveredDays label = "Number of enrolled days between &StartDt and &EndDt"
            , (sum(days) ge &TotDays) as ContinuouslyEnrolled label = "0/1 flag answering was this person continuously enrolled from &StartDt to &EndDt. (disregarding gaps up to &DaysTol days)?"
      from _period_days
      group by mrn
      ;
      insert into &OutSet (MRN, CoveredDays, ContinuouslyEnrolled)
      select MRN, 0, 0
      from _ids
      where mrn not in (select mrn from _period_days)
      ;
      drop table _period_days ;
      drop table _ids ;
   quit ;

%mend SimpleContinuous ;

%macro GetPxForPeopleAndPx (
                           People  /* The name of a dataset containing the people whose fills you want. */
                           , PxLst   /* The PROC codes of interest */
                           , StartDt /* The date on which you want to start collecting fills.*/
                           , EndDt   /* The date on which you want to stop collecting fills. */
                           , Outset  /* The name of the output dataset containing the fills. */
                           ) ;

   %if &People = &Outset %then %do ;
      %put PROBLEM: The People dataset must be different from the OutSet dataset. ;
      %put PROBLEM: Both parameters are set to "&People". ;
      %put PROBLEM: Doing nothing. ;
   %end ;
   %else %do ;

      proc sql ;
      create table &OutSet as
      			  select d.*
      			from  &_vdw_px as d
      			INNER JOIN &People as p
      			on    d.MRN = p.MRN
      			where d.ADate BETWEEN "&StartDt"d AND "&EndDt"d AND
      						d.px in (select pl.px from &PxLst as pl)
      ;
      quit ;
   %end ;

%mend GetPxForPeopleAndPx ;

%macro RemoveDset(dset = ) ;
  %if %sysfunc(exist(&dset)) %then %do ;
    %* experimenting to see if this fixes my teradata hung sessions. ;
    data _null_ ;
      rc = sleep(3) ;
    run ;
    proc sql ;
      drop table &dset ;
    quit ;
  %end ;
%mend RemoveDset ;

%macro GetKidBMIPercentiles(Inset  /* Dset of MRNs on whom you want kid BMI recs */
                          , OutSet
                          , StartDt = 01jan1960
                          , EndDt = &sysdate9
                          ) ;


  %put ;
  %put ;
  %put ==================================================================== ;
  %put                                                                      ;
  %put Macro GetKidBMIPercentiles:                                          ;
  %put                                                                      ;
  %put Creating a dataset "&OutSet", which will contain all BMI measures    ;
  %put on record for the people whose MRNs are contained in "&InSet" which  ;
  %put were taken while the people were between the ages of 2 and 17 and    ;
  %put taken between "&StartDt" and "&EndDt".                               ;
  %put                                                                      ;
  %put The output dataset will contain a variable calculated by the CDCs    ;
  %put normative sample percentile score program found here:                ;
  %put http://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm      ;
  %put                                                                      ;
  %put From this variable (called BMIPCT) you can categorize the children   ;
  %put into normal/overweight/obese brackets with the following format:     ;
  %put                                                                      ;
  %put proc format ;                                                        ;
  %put    value bmipct                                                      ;
  %put       low -< 5    = 'Underweight < 5th percentile'                   ;
  %put       5   -< 85   = 'Normal weight 5th to 84.9th percentile'         ;
  %put       85  -< 95   = 'Overweight 85th to 94.9th percentile'           ;
  %put       95  -  high = 'Obese >=95th percentile'                        ;
  %put    ;                                                                 ;
  %put quit ;                                                               ;
  %put                                                                      ;
  %put ==================================================================== ;
  %put ;
  %put ;

  proc sql ;
    ** Gather the demog data for our input dset. ;
    create table __demog as
    select i.mrn
          , case d.sex_admin when 'M' then 1 when 'F' then 2 else . end as sex label = '1 = Male; 2 = Female'
          , d.birth_date
    from  &InSet as i LEFT JOIN
          &_vdw_demographic as d
    on    i.mrn = d.mrn
    ;

    ** Now gather any ht/wt measures that occurred prior to the 18th birthday. ;
    create table mydata as
    select d.mrn
          , d.sex
          , d.birth_date
          , measure_date
          , ht*2.54         as height label = 'Height in centimeters'
          , wt*0.45359237   as weight label = 'Weight in kilograms'
          , ((measure_date - birth_date)/365.25 * 12) as agemos label = 'Age at measure in months'
          , %CalcAge(bdtvar = birth_date, refdate = measure_date) as age_at_measure
          , . as recumbnt   label = 'Recumbent flag (not implemented in VDW)'
          , input(head_cir_raw, best.) as headcir    label = 'Head circumference'
    from  __demog as d INNER JOIN
          &_vdw_vitalsigns as v
    on    d.mrn = v.mrn
    where calculated age_at_measure between 2 and 17 AND
          ht IS NOT NULL AND
          wt IS NOT NULL AND
          measure_date between "&StartDt"d and "&EndDt"d
    ;
  quit ;

  * Grab the code... ;
  filename kid_bmi &_vdw_asset_engine "&_vdw_asset_loc/cdc-child-bmi-code.sas" ;

  *... and the reference data. ;
  filename refdat &_vdw_asset_engine "&_vdw_asset_loc/CDCref_d.xpt" ;

  libname refdat xport ;

  data CDCref_d ;
    set refdat.CDCref_d ;
  run ;

  %let refdata = CDCref_d ;

  data mydata ;
    set mydata ;
    %include kid_bmi ;
  run ;

  data &OutSet ;
    set _cdcdata ;

    * label
      HTPCT    = 'percentile for length-for-age or stature-for-age'
      HAZ      = 'z-score for length-for-age or stature-for-age'
      WTPCT    = 'percentile for weight-for-age'
      WAZ      = 'z-score for weight-for-age'
      WHPCT    = 'percentile for weight-for-length or weight-for-stature'
      WHZ      = 'z-score for weight-for-length or weight-for-stature'
      BMIPCT   = 'percentile for body mass index-for-age'
      BMIZ     = 'z-score for body mass index-for-age'
      BMI      = 'calculated body mass index value [weight(kg)/height(m)2 ]'
      HCPCT    = 'percentile for head circumference-for-age'
      HCZ      = 'z-score for head circumference-for-age'
      _BIVHT   = 'outlier variable for height-for-age (0 – acceptable normal range, 1 – too low, 2 – too high)'
      _BIVWT   = 'outlier variable for weight-for-age (0 – acceptable normal range, 1 – too low, 2 – too high)'
      _BIVWHT  = 'outlier variable for weight-for-height (0 – acceptable normal range, 1 – too low, 2 – too high)'
      _BIVBMI  = 'outlier variable for body mass index-for-age (0 – acceptable normal range, 1 – too low, 2 – too high)'
    ;
    %* Note--these are dropped only b/c I dont know what they are--could not find ;
    %* documentation on them on the CDC website. ;
    * drop
      _SDLGZLO
      _SDLGZHI
      _FLAGLG
      _SDSTZLO
      _SDSTZHI
      _FLAGST
      _SDWAZLO
      _SDWAZHI
      _FLAGWT
      _SDBMILO
      _SDBMIHI
      _FLAGBMI
      _SDHCZLO
      _SDHCZHI
      _FLAGHC
      _BIVHC
      _FLAGWLG
      _FLAGWST
    ;
  run ;
%mend GetKidBMIPercentiles ;

%macro GetLabForPeopleAndLab(
							People
						, LabLst
						, StartDt
						, EndDt
						, Outset
						) ;

  *****************************************************************************
  Gets the Lab results for a specified set of people (identified by MRNs)
  which occurred between the dates specified in StartDt and EndDt.
  *****************************************************************************;

  %if &People = &Outset %then %do ;
    %put PROBLEM: The People dataset must be different from the OutSet dataset.;
    %put PROBLEM: Both parameters are set to "&People". ;
    %put PROBLEM: Doing nothing. ;
    %end ;
  %else %do ;


    proc sql ;
      create table __ids as
      select distinct mrn
      from &people
      ;
      create table &OutSet as
    			select l.*
  			from &_vdw_lab as l INNER JOIN
  			      __ids as p
  			on    l.MRN = p.MRN
  			where l.Lab_dt BETWEEN "&StartDt"d AND "&EndDt"d AND
  						l.Test_Type in (select &LabLst..test_type from &LabLst) ;
    quit ;

  %end;
%mend GetLabForPeopleAndLab ;

%macro make_inclusion_table(cohort = ) ;

  proc format ;
    value $Hispani
      'Y' = 'Hispanic or Latino'
      'N' = ' Not Hispanic or Latino'
      'U'
      , ' '
      , ''
      , Missing = 'Unknown (Individuals not reporting ethnicity)'
    ;
    value $Gender
      'M' = 'Male'
      'F' = 'Female'
      Other = 'Unknown/ Not Reported'
    ;

    /*
    I would love it if someone could check my geography here--I am especially
    unsure of the Native Hawaiian or Other Pac Islander category.
    */
    value $Race
      'IN' = '1 American Indian/Alaska Native'
      'AS' = '2 Asian'
      'HP' = '3 Native Hawaiian or Other Pacific Islander'
      'BA' = '4 Black or African American'
      'WH' = '5 White'
      'MU' = '6 More than one race'
      Other = '7 Unknown or Not Reported'
    ;
  quit ;

  proc sql ;
    create table _reportable as
    select d.mrn, case when d.race2 ne 'UN' then 'MU' else d.race1 end as race label = "Racial Category"
          , d.hispanic label = "Ethnic Category"
          , d.sex_admin as gender label = "Sex"
    from &_vdw_demographic as d INNER JOIN
         &cohort as c
    on    d.mrn = c.mrn
    ;

    create table genders (gender char(1)) ;
    insert into genders(gender) values ('F') ;
    insert into genders(gender) values ('M') ;
    insert into genders(gender) values ('U') ;

    create table races(race char(2)) ;
    insert into races(race) values ('IN') ;
    insert into races(race) values ('AS') ;
    insert into races(race) values ('HP') ;
    insert into races(race) values ('BA') ;
    insert into races(race) values ('WH') ;
    insert into races(race) values ('MU') ;

    insert into races(race) values ('01') ;
    insert into races(race) values ('02') ;
    insert into races(race) values ('03') ;
    insert into races(race) values ('04') ;
    insert into races(race) values ('  ') ;
    insert into races(race) values ('97') ;
    insert into races(race) values ('-1') ;

    create table ethnicities(hispanic char(1)) ;
    insert into ethnicities(hispanic) values('Y') ;
    insert into ethnicities(hispanic) values('N') ;
    insert into ethnicities(hispanic) values('U') ;
    insert into ethnicities(hispanic) values(' ') ;

    create table class_levels as
    select gender, race, hispanic
    from genders CROSS JOIN races CROSS JOIN ethnicities ;

  quit ;

  * Since its just a single table now, we can let users set whatever title(s) they like. ;
  * title1 "PHS Inclusion Enrollment Report" ;

  proc tabulate data = _reportable missing format = comma15.0 order = formatted classdata = class_levels ;
    class hispanic race gender ;
    keylabel N = ' ' ;
    tables race     all='Total',  hispanic*gender=" " all = 'Total' / printmiss misstext = '0' box = 'Racial Categories' ;
    format race     $race.
           hispanic $hispani.
           gender   $gender. ;
  quit ;

%mend make_inclusion_table ;

************************************************************************;
** Program: BMI_adult_macro.sas                                        *;
**                                                                     *;
** Purpose: Calculate BMI for adults and include a flag for reason     *;
**          that a BMI was not calculated. This flag can have values   *;
**          of:  MISSING AGE                                           *;
**               UNDER AGE 18                                          *;
**               NO WT                                                 *;
**               NO HT                                                 *;
**               NO HT OR WT                                           *;
**               WT OUT OF RANGE                                       *;
**               BMI OUT OF RANGE                                      *;
**                                                                     *;
**          The BMI algorithm and cut-off recommendations were         *;
**          reviewed by the Obesity special interest group.            *;
**          This is meant to flag only those extreme values or         *;
**          situations where there is reason to suspect a data entry   *;
**          error, and further review may be warranted.                *;
**                                                                     *;
**          The macro assumes that the program is placed into the      *;
**          middle of a program. It assumes that libnames have been    *;
**          defined prior to the macro call, and indicates that the    *;
**          macro parameters have be fully qualified dataset names.    *;
**                                                                     *;
**                                                                     *;
**         Three variables are created:                                *;
**                                                                     *;
**         VARIABLE       DECRIPTION                  FORMAT           *;
**         ---------------------------------------------------------   *;
**         BMI            BMI FOR ADULTS              Numeric          *;
**         HT_MEDIAN      MEDIAN HT FOR ADULTS        Numeric          *;
**         BMI_flag       BMI QC FLAG                 $16.             *;
**                                                                     *;
** Author: G. Craig Wood, Geisinger Health System                      *;
**         cwood@geisinger.edu                                         *;
**                                                                     *;
** Revisions: Intial Creation 6/7/2010                                 *;
**                                                                     *;
************************************************************************;
** Macro Parameters:                                                   *;
**                                                                     *;
** VITALS_IN: These needs to have the following variables:             *;
**        MRN, HT, WT, and measure_date.                               *;
**        Feed in fully qualified name, i.e. use libname and           *;
**        dataset name together if reading a permanent dataset.        *;
**        This macro program assumes that desired libraries have been  *;
**        defined previously in the program. StandardVars macro        *;
**        variables can be used.                                       *;
**                                                                     *;
** DEMO_IN: These needs to have the following variables:               *;
**        MRN, birth_date.                                             *;
**        Feed in fully qualified name, i.e. use libname and           *;
**        dataset name together if reading a permanent dataset.        *;
**        This macro program assumes that desired libraries have been  *;
**        defined previously in the program. StandardVars macro        *;
**        variables can be used.                                       *;
**                                                                     *;
** VITALS_OUT: Feed in fully qualified name, i.e. use libname and      *;
**        dataset name together if writing to a permanent dataset.     *;
**                                                                     *;
** KEEPVARS: Optional parameter indicating the values to keep in your  *;
**           quality checking dataset.  May be left blank to simply    *;
**           attach the two new variables to an existing dataset.      *;
************************************************************************;
** Examples of use:                                                    *;
**                                                                     *;
** %BMI_adult_macro(vitals_in=&_vdw_vitalsigns,                        *;
**                  demo_in=&_vdw_demographic,                         *;
**                  vitals_out=BMI_qc,                                 *;
**                  keepvars= mrn measure_date BMI BMIFLAG)            *;
**                                                                     *;
** %BMI_adult_macro(vitals_in=cohort_vitals,                           *;
**                  demo_in=cohort_demo,                               *;
**                  vitals_out=BMI_qc,                                 *;
**                  keepvars= mrn measure_date BMI BMIFLAG ht wt)      *;
**                                                                     *;
** %BMI_adult_macro(vitals_in=cohort_vitals,                           *;
**                  demo_in=&_vdw_demographic,                         *;
**                  vitals_out=lib_out.BMI_qc,                         *;
**                  keepvars= )                                        *;
**                                                                     *;
************************************************************************;


%MACRO BMI_adult_macro(vitals_in, demo_in, vitals_out, keepvars);

  PROC SQL;
    CREATE TABLE one AS
    SELECT A.*, B.birth_date, %CalcAge(BDtVar = birth_date, RefDate = measure_date) AS AGE
    FROM &vitals_in A LEFT OUTER JOIN &demo_in  B
    ON A.MRN = B.MRN;
  QUIT;

  proc sort data=one; by mrn; run;

  proc means noprint nway data=one;
    class mrn;
    var ht;
    WHERE (ht>=48 AND ht<=84) AND AGE>=18;
    output out=outHT (drop=_type_ _freq_) median=HT_median;
  run;

  proc sort data=outht; by mrn; run;

  data &vitals_out; merge one outht; by mrn;
    %if &keepvars ne %then %do; keep &keepvars; %end;

    format BMIflag $16.;

    if age = .                                        then BMIflag = 'MISSING AGE';
    if age<18 AND age NE .                            then BMIflag = 'UNDER AGE 18';
    if age<18 then HT_median=.;
    if BMIflag=' ' and HT_median=. and wt=.           then BMIflag = 'NO HT OR WT';
    if BMIflag=' ' and HT_median=.                    then BMIflag = 'NO HT';
    if BMIflag=' ' and wt=.                           then BMIflag = 'NO WT';
    if BMIflag=' ' and wt ne . and (wt<50 or wt>700)  then BMIflag = 'WT OUT OF RANGE';
    if BMIflag=' ' then BMI=round((703*wt/(HT_median*HT_median)),0.01);
    if BMIflag=' ' and BMI ne . and (BMI<15 or BMI>90) then do;
      BMIflag ='BMI OUT OF RANGE';
      BMI     =.;
    end;
    drop age birth_date;
  run;


  PROC DATASETS NOLIST; DELETE one outht; QUIT;

%MEND BMI_adult_macro;

/*
  GetAdultBMI

  A little wrapper macro that lets users supply a cohort dset & an optional time period, for whom/over which they
  would like BMI data (as calculated by the vital signs WGs official code).

  Author: Roy Pardee
*/
%macro GetAdultBMI(people = , outset = , StartDt = "01jan1960"d, EndDt = "&sysdate"d) ;
  proc sql ;
    create table __in_demog as
    select distinct p.mrn, birth_date
    from  &people as p INNER JOIN
          &_vdw_demographic as d
    on    p.mrn = d.mrn
    ;
  quit ;


  ** Vars in v3 of vitals: ;
  ** enc_id, enctype, mrn, measure_date, ht_raw, wt_raw, ht, wt, bmi_raw, tobacco, tobacco_type, diastolic, systolic, diastolic_raw, systolic_raw, bp_type, position, measure_time, head_cir_raw, respir_raw, temp_raw, pulse_raw  ;

  proc sql ;
    create table __in_vitals as
    select p.*, measure_date, ht, wt
    from  &_vdw_vitalsigns as v INNER JOIN
          &people as p
    on    v.mrn = p.mrn
    where v.measure_date between &StartDt and &EndDt
    ;
  quit ;

  %BMI_adult_macro(vitals_in = __in_vitals, demo_in = __in_demog, vitals_out = &outset) ;

%mend GetAdultBMI ;

************************************************************************;
** Program: BP_FLAG.sas                                                 *;
**                                                                     *;
** Purpose: Create flags that can be used to determine quality of      *;
**          systolic and diastolic blood pressure fields.              *;
**          Cut-off recommendations reviewed by CVRN HTN Registry      *;
**          site PIs on 5/12/2010. This is meant to flag only those    *;
**          extreme values or situations where there is reason to      *;
**          suspect a data entry error, and further review may be      *;
**          warranted.                                                 *;
**                                                                     *;
**         Three variables are created:                                *;
**                                                                     *;
**         VARIABLE        VALUES                                      *;
**         ---------------------------------------------------------   *;
**         SYSTOLIC_QUAL   NULL, ABN_HIGH, ABN_LOW                     *;
**         DIASTOLIC_QUAL  NULL, ABN_HIGH                              *;
**         SYS_DIA_QUAL    SYSTOLIC <= DIASTOLIC, DIFFERENCE < 20,     *;
**                         DIFFERENCE > 100                            *;
**                                                                     *;
**         Note that NULL is only used when the other paired value for *;
**         the blood pressure is not null.                             *;
**                                                                     *;
** Author: Heather Tavel, KPCO                                         *;
**         Heather.M.Tavel@kp.org                                      *;
**                                                                     *;
** Revisions: Intial Creation 5/28/2010                                *;
**                                                                     *;
************************************************************************;
** Macro Parameters:                                                   *;
**                                                                     *;
** DSIN: Feed in fully qualified name, i.e. use libname and dataset    *;
**       name together if reading a permanent dataset. This macro      *;
**       program assumes that desired libraries have been defined      *;
**       previously in the program. StandardVars macro variables can   *;
**       be used.                                                      *;
**                                                                     *;
** DSOUT: Feed in fully qualified name, i.e. use libname and dataset   *;
**        name together if writing to a permanent dataset.             *;
**                                                                     *;
** KEEPVARS: Optional parameter indicating the values to keep in your  *;
**           quality checking dataset.  May be left blank to simply    *;
**           attach the three quality checking variables to an         *;
**           existing dataset.                                         *;
************************************************************************;
** Examples of use:                                                    *;
**                                                                     *;
** %bp_flag(dsin=&_vdw_vitalsigns,                                     *;
**          dsout=bp_qc,                                               *;
**          keepvars= mrn measure_date systolic diastolic)             *;
**                                                                     *;
** %bp_flag(dsin=cohort_vitals,                                        *;
**          dsout=cohort_vitals,                                       *;
**          keepvars=)                                                 *;
**                                                                     *;
** %bp_flag(dsin=&_vdw_vitalsigns,                                     *;
**          dsout=studylib.cohort_vitals,                              *;
**          keepvars=mrn measure_date systolic diastolic ht wt)        *;
**                                                                     *;
************************************************************************;
%macro bp_flag(dsin, dsout, keepvars);

data &dsout;
 set &dsin
     %if &keepvars ne %then %do; (keep=&keepvars)%end;
     ;

 ** Flag Systolic quality. Null values are suspect if diastolic exists ;

 if systolic gt 300          then SYSTOLIC_QUAL = 'ABN_HIGH';
  else if . < systolic < 50  then SYSTOLIC_QUAL = 'ABN_LOW';
  else if (systolic = . and
           diastolic ne .)   then SYSTOLIC_QUAL = 'NULL';

 ** Flag diastolic quality. Diastolic can go as low as 0, so there is no;
 ** lower limit.  Null values are OK in studies that may only care about;
 ** systolic.  This is just informative just in case it is needed.      ;
 ** DIA_ABN is set to 'NULL' only if a systolic value is entered on the ;
 ** same record.                                                        ;

 if diastolic gt 160          then DIASTOLIC_QUAL = 'ABN_HIGH';
  else if (systolic ne . and
           diastolic eq .)    then DIASTOLIC_QUAL = 'NULL';

 ** Now look at a comparative view between systolic and diastolic       ;
 ** systolic should always be greater than diastolic, and any difference;
 ** less than 20 or greater than 100 is suspect and should be reviewed  ;
 ** further.                                                            ;

 if systolic ne .
    and diastolic ne . then
    do;
	 if systolic < = diastolic
                            then SYS_DIA_QUAL = 'SYSTOLIC <= DIASTOLIC';
      else if systolic - diastolic < 20
                            then SYS_DIA_QUAL = 'DIFFERENCE < 20';
	  else if systolic-diastolic > 100
                            then SYS_DIA_QUAL = 'DIFFERENCE > 100';
	end;
run;

** Run a frequency on the results.  Can have more than one condition at ;
** a time;

proc freq data=&dsout;
 tables SYSTOLIC_QUAL
        DIASTOLIC_QUAL
        SYS_DIA_QUAL
        SYSTOLIC_QUAL*DIASTOLIC_QUAL
		SYSTOLIC_QUAL*SYS_DIA_QUAL
		DIASTOLIC_QUAL*SYS_DIA_QUAL/missing;
run;

%mend bp_flag;

%macro vdw_formats(lib = work, tweaked_descriptions = 0) ;
  filename vdw_fmt &_vdw_asset_engine "&_vdw_asset_loc/formats.xpt" ;
  libname  vdw_fmt xport ;

  *recode label to be of the form CODE(DESCRIPTION) if label only contains DESCRIPTION;
  data vdw_formats;
    set vdw_fmt.formats;
    %if &tweaked_descriptions = 1 %then %do ;
      if (not(label =: strip(start)) or index(label,'(')=0) and start = end then
        label = strip(start) || ' (' || strip(label) || ')' ; *prepend code to desc;
    %end ;
  run;

  proc format lib = &lib cntlin = vdw_formats ;
  run ;

  proc datasets nolist;  delete vdw_formats;  run; *Clean up workspace;

%mend vdw_formats ;

%macro get_preg_events(inset =
                      , start_date =
                      , end_date =
                      , out_events = pregnancy_events) ;

  %** This is a helper macro for pregnancy_periods below--you will almost always want to use that. ;

  %** I split this up in order to make testing easier. ;


  %** Obtain the list of pregnancy-related codes from the "vdw asset location" ;
  %** Note that the _vdw_asset_* vars are new as of June 2015 and may need to be ;
  %** added to your StdVars.sas. ;

  filename prg_cds &_vdw_asset_engine "&_vdw_asset_loc/pregnancy_codes.xpt" ;
  libname  prg_cds xport ;

  %** Note to self: these code lists are managed in: ;
  %** $repo\manage_pregnancy_codes.sas ;

  proc copy in = prg_cds out = work ;
  run ;

  libname prg_cds clear ;

  %** Pull the events. ;
  proc sql ;
    create table dx as
    select d.mrn, adate, d.dx as event_code, descr as description, sigs as signifies
    from  &_vdw_dx as d INNER JOIN
          &inset as c
    on    d.mrn = c.mrn INNER JOIN
          preg_dx as p
    on    d.dx = p.dx
    where d.adate between "&start_date"d and "&end_date"d
    ;
    create table px as
    select p.mrn, adate, p.px as event_code, descr as description, sigs as signifies
    from  &_vdw_px as p INNER JOIN
          &inset as c
    on    p.mrn = c.mrn  INNER JOIN
          preg_px as pp
    on    p.px = pp.px AND
          p.px_codetype = pp.px_ct
    where p.adate between "&start_date"d and "&end_date"d
    ;

    create table &out_events as
    select *, 'dx' as source from dx
    UNION ALL
    select *, 'px' as source from px
    ;

    drop table dx ;
    drop table px ;

  quit ;

  proc sort nodupkey data = &out_events ;
    by mrn adate signifies event_code ;
  run ;

%mend get_preg_events ;

%macro make_preg_periods(inevents = pregnancy_events
                      , out_periods =
                      , max_pregnancy_length = 270) ;

  %** Make episodes out of the events. ;
  proc format ;
    %** The set of codes has a type G that signifies testing events--this version of the macro does not ;
    %** take those into account. ;
    value $evnt
      'P'   = 'pregnant'
      'D'   = 'delivered'
      'M'   = 'miscarried'
      'S'   = 'stillborn'
      'T'   = 'terminated'
      other = 'zah?'
    ;

    value $daysb
      'D'       = '270'
      'M'       = '98'
      'S', 'T'  = '112'
      other     = 'zah?'
    ;
  quit ;


  proc sort data = &inevents out = all_events ;
    by mrn adate ;
    where put(signifies, $evnt.) ne 'zah?' ;
  run ;

  proc sort data = all_events out = terminating_events ;
    by mrn adate signifies event_code ;
    where put(signifies, $evnt.) in ('delivered', 'miscarried', 'stillborn', 'terminated') ;
  run ;

  ** Reduce to one terminating event per person/date. ;
  ** Putting signifies in the above sort means that deliveries are favored over miscarriages, miscars are favored over ;
  ** stills, and stills over terminations. ;
  data terminating_events ;
    set terminating_events ;
    by mrn adate ;
    if first.adate ;
  run ;

  proc sql ;
    ** Remove all events happening on a termination date. ;
    create table events as
    select a.*
    from  all_events as a LEFT JOIN
          terminating_events as t
    on    a.mrn = t.mrn AND
          a.adate = t.adate
    where t.mrn IS NULL
    ;
  quit ;

  ** In GH data we frequently had multiple termination events w/in a few days of one another (e.g., missed abortion on day 1, followed by d and c on day 3);
  ** Reduce any such to a single termining event. ;
  data terminating_events ;
    retain _last_adate . ;
    set terminating_events ;
    by mrn adate ;
    if not first.mrn then do ;
      if (adate - _last_adate) le 2 then delete ;
    end ;
    _last_adate = adate ;
    drop _last_adate ;
  run ;

  ** Put those termination dates back in. ;
  proc append base = events data = terminating_events ;
  run ;

  ** Re-do the sort. ;
  proc sort data = events ;
    by mrn adate ;
  run ;


  ** ...and now the fun begins. ;
  data __outeps ;
    length
      outcome_category  $ 10
      outcome_code      $ 8
      first_sign_code   $ 8
    ;
    retain
      preg_episode      0
      first_sign_date   .
      first_sign_code   ''
      preg_code_count   0
      _last_event_date  .
    ;
    set events ;
    by mrn ;


    ** New episodes happen when: ;
      ** this is the first record for a new woman. ;
      ** the immediately prior record for this woman ended a pregnancy episode (in which case the _last_event_date will be set to missing.) ;
      ** the date on this record is too far away from the last date for it to be the same pregnancy . ;

    if first.mrn then do ;
      _last_event_date = . ;
      preg_episode = 0 ;
    end ;

    days_since = adate - coalesce(_last_event_date, adate) ;

    new_episode = (first.mrn or
                   (n(_last_event_date) = 0 ) or
                   (days_since gt &max_pregnancy_length)
                   ) ;

    if new_episode then do ;
      ** If we have exceeded max_pregnancy_length we need to output a record w/what we know thus far for the previous episode. ;
      if days_since gt &max_pregnancy_length then do ;
        outcome_category = 'unknown' ;
        outcome_date = _last_event_date ;
        outcome_code = '' ;
        output ;
      end ;

      preg_episode + 1 ;

      ** reset vars ;
      first_sign_date   = .  ;
      first_sign_code   = '' ;
      status            = '' ;
      _last_event_date  = .  ;
      preg_code_count   = 0  ;

      ** if this rec signifies a pregnancy, then it is the first sign. ;
      select(put(signifies, $evnt.)) ;
        when('pregnant') do ;
          first_sign_date = adate ;
          first_sign_code = event_code ;
        end ;
        otherwise do ;
          ** nothing ;
        end ;
      end ;
    end ;

    ** Regardless of where we are in an episode, these things need to be done. ;
    outcome_date = adate ;

    select(put(signifies, $evnt.)) ;
      when('pregnant') do ;
        preg_code_count + 1 ;
        _last_event_date = adate ;

        ** Special-case: if the record for this woman ends w/a pregnant event, spit out whatever we know by now. ;
        if last.mrn then do ;
          outcome_code = '' ;
          outcome_category = 'unknown' ;
          output ;
        end ;

      end ;
      when('delivered', 'miscarried', 'stillborn', 'terminated') do ;
        ** End this episode. ;
        outcome_code = event_code ;
        outcome_category = put(signifies, $evnt.) ;
        probable_onset = adate - input(put(signifies, $daysb.), best.) ;
        output ;
        ** reset _last_event_date so we start a new episode on the next record for this woman.. ;
        _last_event_date = . ;
      end ;
    end ;
  run ;

  proc sql ;
    create table &out_periods as
    select mrn
         , preg_episode      label = "Ordinal counter for the pregancy episode for the woman."
         , probable_onset    label = "Likely onset of the pregnancy.  Imputed from the type of outcome--be skeptical for outcomes other than 'delivered'." format = mmddyy10.
         , first_sign_date   label = "Date of the first 'prenatal' type event attributable to this pregnancy." format = mmddyy10.
         , first_sign_code   label = "The first-ocurring event code (dx/px) for a 'prenatal' type event during this pregnancy."
         , outcome_date      label = "The date the pregnancy episode ended (e.g., birth/termination/miscarriage if that is the outcome, or last sign of pregnacy if outcome unknown)" format = mmddyy10.
         , outcome_code      label = "The event code (dx/px) for the event that ended this pregnancy."
         , outcome_category  label = "Type of pregnancy outcome."
         , preg_code_count   label = "The number of 'prenatal' type events found during this pregnancy."
    from __outeps
    order by mrn, outcome_date, preg_episode
    ;
  quit ;
%mend make_preg_periods ;

%macro pregnancy_periods(inset                 =
                        , out_periods          =
                        , start_date           = 01jan1966
                        , end_date             = &sysdate9
                        , out_events           = pregnancy_events
                        , max_pregnancy_length = 270) ;

  %** Broke this macro into two in order to facilitate testing. ;
  %get_preg_events(inset = &inset
                  , start_date = &start_date
                  , end_date = &end_date
                  , out_events = &out_events) ;


  %make_preg_periods(inevents = &out_events
                  , out_periods = &out_periods
                  , max_pregnancy_length = &max_pregnancy_length) ;


%mend pregnancy_periods ;

%macro make_denoms(start_year, end_year, outset, extra_wh) ;
  %local round_to ;
  %let round_to = 0.0001 ;

  proc format ;
    ** 0-17, 18-64, 65+ ;
    value shrtage
      low -< 18 = '0 to 17'
      18  -< 65 = '18 to 64'
      65 - high = '65+'
    ;
    value agecat
      low -< 5 =  '00to04'
      5   -< 10 = '05to09'
      10  -< 15 = '10to14'
      15  -< 20 = '15to19'
      20  -< 30 = '20to29'
      30  -< 40 = '30to39'
      40  -< 50 = '40to49'
      50  -< 60 = '50to59'
      60  -< 65 = '60to64'
      65  -< 70 = '65to69'
      70  -< 75 = '70to74'
      75 - high = 'ge_75'
    ;
    ** For setting priority order to favor values of Y. ;
    value $dc
      'Y'   = 'A'
      'K'   = 'A'
      'N'   = 'B'
      other = 'C'
    ;
    ** For translating back to permissible values of DrugCov ;
    value $cd
      'A' = 'Y'
      'B' = 'N'
      'C' = 'U'
    ;
    value $Race
      'WH' = 'White'
      'BA' = 'Black'
      'IN' = 'Native'
      'AS' = 'Asian'
      'HP' = 'Pac Isl'
      'MU' = 'Multiple'
      Other = 'Unknown'
    ;
    value $eb
      'I' = 'Insurance'
      'G' = 'Geography'
      'B' = 'Both Ins + Geog'
      'P' = 'Non-member patient'
    ;
  quit ;

  data all_years ;
    do year = &start_year to &end_year ;
      first_day = mdy(1, 1, year) ;
      last_day  = mdy(12, 31, year) ;
      ** Being extra anal-retentive here--we are probably going to hit a leap year or two. ;
      num_days  = last_day - first_day + 1 ;
      output ;
    end ;
    format first_day last_day mmddyy10. ;
  run ;

  proc sql ;
    /*
      Dig this funky join--its kind of a cartesian product, limited to
      enroll records that overlap the year from all_years.
      enrolled_proportion is the # of days between <<earliest of enr_end and last-day-of-year>>
      and <<latest of enr_start and first-day-of-year>> divided by the number of
      days in the year.

      Nice thing here is we can do calcs on all the years desired in a single
      statement.  I was concerned about perf, but this ran quite quickly--the
      whole program took about 4 minutes of wall clock time to do 1998 - 2007 @ GH.

    */
    create table gnu as
    select mrn
          , year
          , min(put(drugcov, $dc.)) as drugcov
          , min(put(enrollment_basis, $eb.)) as enrollment_basis
          /* This depends on there being no overlapping periods to work! */
          , sum((min(enr_end, last_day) - max(enr_start, first_day) + 1) / num_days) as enrolled_proportion
    from  &_vdw_enroll as e INNER JOIN
          all_years as y
    on    e.enr_start le y.last_day AND
          e.enr_end   ge y.first_day
    &extra_wh
    group by mrn, year
    ;

    reset outobs = max warn ;

    create table with_agegroup as
    select g.mrn
        , year
        , put(%calcage(birth_date, refdate = mdy(1, 1, year)), agecat.) as agegroup label = "Age on 1-jan of [[year]]"
        , d.sex_admin as gender
        , put(race1, $race.) as race length = 10
        , put(drugcov, $cd.) as drugcov
        , enrollment_basis
        , enrolled_proportion
    from gnu as g LEFT JOIN
         &_vdw_demographic as d
    on   g.mrn = d.mrn
    ;

    create table &outset as
    select year
        , agegroup
        , drugcov label = "Drug coverage status (set to 'Y' if drugcov was 'Y' even once in [[year]])"
        , enrollment_basis label = "What sort of relationship between person and HMO does this record document?"
        , race
        , gender label = "Administrative Sex"
        , round(sum(enrolled_proportion), &round_to) as prorated_total format = comma20.2 label = "Pro-rated number of people enrolled in [[year]] (accounts for partial enrollments)"
        , count(mrn)               as total          format = comma20.0 label = "Number of people enrolled at least one day in [[year]]"
    from with_agegroup
    group by year, agegroup, drugcov, enrollment_basis, race, gender
    order by year, agegroup, drugcov, enrollment_basis, race, gender
    ;

    /*
    ** Create a dset of (masked) counts by race for submission to GH for collation. ;
    create table race_counts_&_SiteAbbr as
    select year, agegroup, race
          , case when sum(prorated_total) between .01 and 4 then .a else sum(prorated_total) end as prorated_total format = comma20.2
          , case when sum(total)          between 1   and 4 then .a else sum(total)          end as total          format = comma20.0
    from &outset
    group by year, agegroup, race
    ;
    */

  quit ;

%mend make_denoms ;
%macro vdwcountsandrates1(medcodes=,start_date=,end_date=,fileIN=,cohort=,outpath=,outname=);
/****************************************************************************************************/
/****************************************************************************************************/
/*	This macro collects counts and rates of supplied codes at a given site over a specified time period. 								  */
/*	Over this time period, and for each category in the study, counts are collected for 												  */
/*	(i) the total number of times a certain medcode (px, dx, ndc) was encountered														  */
/* (ii) the total number of people who were assigned this code																			  */
/*(iii) the total number of enrolled people who were assigned this code.																  */
/* (iv) A rate of incidence per 10,000 people is calculated over the enrolled people. 													  */
/*  																																	  */
/* An example of calling the macro in general (when there is no cohort file)is as follows (leave the cohort= argument blank): 			  */
/* %vdwcountsandrates1(px,'01jan09'd,'31dec09'd, fileIN=lib1.outds, COHORT=, 															  */
/*		outpath=\\groups\data\CTRHS\Crn\S D R C\VDW\Radiology\sasdata\InputData\Finalized Input Counts and Rates);						  */
/*																																		  */
/* When a cohort file exists, here is another way to call the macro: 																	  */
/* %vdwcountsandrates1(px,'01jan09'd,'31dec09'd, fileIN=path.outds, COHORT=lib2.cohorttest, 											  */
/*		outpath=\\groups\data\CTRHS\Crn\S D R C\VDW\Data\Counts and Rates\Data) 														  */
/******************************************************************************************************************************************/
options  mprint nocenter msglevel = i NOOVP dsoptions="note2err" ;
libname path "&outpath.";

/*The lack of symmetry in the table names requires a small twist*/
%let _vdw_px=&_vdw_px; %let _vdw_dx=&_vdw_dx; %let _vdw_ndc=&_vdw_rx;
%let ndcdate=rxdate;   %let pxdate=adate;     %let dxdate=adate;

/*Determine which medical codes are available in the supplied data*/
data combined_px combined_dx combined_ndc;
	set &fileIN.;
	if substr(upcase(medcode),1,2)="PX" then do;
		CodeType=substr(upcase(medcode),4,1);
		output combined_px;
	end;
	else if substr(upcase(medcode),1,2)="DX" then output combined_dx;
	else if substr(upcase(medcode),1,3)="NDC" then output combined_ndc;
run;

%if %upcase(&cohort.) eq %then %do;
/*We'll later use this step to calc rates - rates are count/enrPple *10k
Here and below, handling is required for the cohort yes or no instance*/
proc sql;
	select count(distinct mrn) as EnrPple into :EnrPple
	from &_vdw_enroll.
	where &start_date between enr_start and enr_end;
quit;
%end;

%else %if %upcase(&cohort.) ne %then %do;
/*We'll later use this next step to calc rates - rates are count/enrPple *10k*/
proc sql;
	select count(distinct mrn) as EnrPple into :EnrPple
	from &cohort. where mrn in (select mrn from &_vdw_enroll
	where &start_date between enr_start and enr_end);
quit;
%end;

/*Run for all specified medical code categories: 'px dx ndc' or a combo of the 3 - if they're not in &fileIn, skip to next*/
%local i cat;
  %do i=1 %to %sysfunc(countw(&medcodes));
		%let cat = %scan(%bquote(&medcodes),&i,' ');
   		proc sql; select count(*) into :numb from combined_&cat.; quit;
  			%if &numb=0 %then %do;
		%put **********************************************************;
		%put No %upcase(&cat) Codes in &FileIn.. Skipping.;
		%put **********************************************************;
			%end;
  		%if &numb=0 %then %goto skip;
		/*else, start processing the medcode*/
		%put NOTE: Commencing to Run &cat. codes.;


		/*Create tables- All People, Modify code if cohort is/is not available*/
		%if %upcase(&cohort.) ne %then %let addcohort = %str(AND mrn in (select mrn from &cohort.));
		%else %if %upcase(&cohort.) eq %then %let addcohort=;

		proc sql;
		/*Count all people with PX/DX/NDC of Interest*/
			create table allPple as
			select description, &cat., Category, count(&cat.) as &cat.count, count(distinct mrn) as allPeople
			from combined_&cat. as a
			left join &&&_vdw_&cat. as b
			on a.code = b.&cat.
			where &&&cat.date between &start_date. and &end_date. &addcohort.
			group by &cat., description, category;

		/*Create tables- All Enrolled People, modify code for cohort yes or no*/
		create table enrolledpple as
			select &cat.,description, count(distinct mrn) as allEnrolledPeople

			from
			(select mrn, &cat., description
			from combined_&cat. as a left join &&&_vdw_&cat. as b on a.code =b.&cat.
			where &&&cat.date between &start_date. and &end_date. &addcohort.)

			where mrn in
 			(select mrn from &_vdw_enroll where &start_date. between enr_start and enr_end)
		group by &cat., description;
    quit ;

		proc sql;
		/*Create table with distinct totals*/
			create table combined&cat. as
			select distinct a.description, a.&cat., Category, a.&cat.count label="%upcase(&cat.) Count",
			allPeople label="All People", allEnrolledPeople label="All Enrolled People"
			from allpple as a
			left join enrolledpple as b
			on a.&cat.=b.&cat.;

  		/*There may be many categories in the study - account for each here*/
  		select distinct category into :category separated by "/" from combined_&cat;
  		select count(distinct category) as catgct into :catgct from combined_&cat;
    quit ;

		%do j=1 %to &catgct.;
			%let catg = %scan(&category., &j., '/');

			proc sql;
				select count(distinct mrn) as EnrPple&cat. into :EnrPple&cat.

				from
	 			(select distinct mrn, &cat.
 	  			from combined_&cat. as a left join &&&_vdw_&cat. as b on a.code =b.&cat.
	  			where compress(category)=%sysfunc(compress("&catg.")) and
				&&&cat.date between &start_date. and &end_date.)

 				where mrn in
  	 			(select mrn from &_vdw_enroll where &start_date between enr_start and enr_end &addcohort.);

			insert into combined&cat.
				(description, &cat., Category, &cat.count, allPeople, allEnrolledPeople)
				values("~TOTAL FOR %upcase(&catg.) %upcase(&cat.) CODES**","~Total","&catg.",.,.,&&EnrPple&cat.);
		%end;

		/*Create tables- Include the rates - one on all enrolled people*/
		create table combined2&cat. as
			select distinct description, a.&cat., Category, a.&cat.count label="%upcase(&cat.) Count",
			allPeople label="All People",
			allEnrolledPeople label="All Enrolled People",
			int((allEnrolledPeople/&EnrPple.)*10000) as RateAllEnr label="Rate/10K Over All Enrolled People"
			from combined&cat. as a order by &cat. ;
		/*the final stage- creating the end table*/
		create table catg&cat. as
			select distinct description, Category, Code as &cat. label="Medical Code"
			from combined_&cat. order by &cat. ;
    quit ;
		data final&cat.;
			merge catg&cat. (in=a) combined2&cat. (in=b);
			by &cat. ;
			if a or b;
		run;

		/*Clean up a little, add Site Code*/
		data final&cat.;
			set final&cat.;
			if  1 <= allPeople <=6 then allPeople=.a;
			if  1 <= pxcount <=6 then pxcount = .a;
			if  1 <= allEnrolledPeople<=6 then do;
               allEnrolledPeople=.a;
               rateallenr=.a;
      End;
			Sitecode="&_sitecode.";
		run;

		proc sort data=final&cat. out=path.final_&OutName._&cat._&_sitecode._&sysdate. /*nodupkey*/; by &cat.; run;

    proc format;
    	value LessSix
    	.a = '<6'
    	other=[9.0];
    run;

		proc print data=path.final_&OutName._&cat._&_sitecode._&sysdate. (obs=200);  /* fix this for final */
			title "Here is a sample of what you are sending out";
			format _numeric_ LessSix. ;
		run;
  %skip:
  %end;
%mend vdwcountsandrates1;

%macro titlefoots;
title "Some Codes Related to %upcase(&catg.)";
	title2 "Counts of All %upcase(&cat.) over period of interest by site within HMORN.";
	title3 "&titl3.";
	footnote1 "Preliminary: The above exhibit shows the incidence of selected codes at sites. The list is not exhaustive," ;
	footnote2 " and may need augumenting. The motive is to assess data quality across sites over selected codes." ;
	footnote3 "**Only Calculated on Enrolled so far";
	footnote4 "Prepared on &sysdate.";
%mend titlefoots;
/********************************************************************************************************************************/
/* This macro tabulates results from sites 4 ways: (i) procedure counts, (ii) all people who ever had a procedure, 		    	*/
/* (iii) all Enrolled People who ever had a procedure, then (iv) rates of incidence over those enrolled. 3 output files 		*/
/* (PX, DX, NDC) containing each of the four will be output.;													                */
/* This macro puts results from sites together for further analysis or comparison; 							  					*/
/********************************************************************************************************************************/
%macro VDWCountsAndRates2(medcodes=, /*Any combo of 'PX DX NDC' - no quotes - that you need tabulated					 */
						      path=, /*path to data files from sites, which SHOULD be stored in a directory by themselves*/
						     titl3=,  /*Optional additional title*/
						     InName= /*Text imbedded in filename */ );
libname path "&path";
options mprint;
/* Make a dummy dataset of site names so that each site ends up in the final table */
data SiteNames;
	length sitecode $4;
	sitecode='01 '; output;	 sitecode='02 '; output;  sitecode='03 '; output;  sitecode='04 '; output;
	sitecode='05 '; output;  sitecode='06 '; output;  sitecode='07 '; output;  sitecode='08 '; output;
	sitecode='09 '; output;  sitecode='10 '; output;  sitecode='11 '; output;  sitecode='12 '; output;
	sitecode='13 '; output;  sitecode='14 '; output;  sitecode='15 '; output;  sitecode='16 '; output;
	sitecode='17 '; output;
;
/*
	sitecode='01a';
	sitecode='01b';
*/
run;
/*Create a site name format;*/
proc format;
	value $sitef
	'01 ' = "GHC"  '02 ' = "KPNW"  '03 ' = "KPNC"  '04 ' = "KPSC"
	'05 ' = "KPHI" '06 ' = "KPCO"  '07 ' = "HPRF"	 '08 ' = "HPHC"
	'09 ' = "MPCI" '10 ' = "HFHS"  '11 ' = "KPGA"  '12 ' = "LHS"
	'13 ' = "MCRF" '14 ' = "GHS"   '15 ' = "SWH"   '16 ' = "MHS"
	'17 ' = "KPMA"
	'01a'='GHC-IGP'
	'01b'='GHC-Network'
	;
proc format;
    	value LessSix
    	.a = '<6'
    	other=[comma5.0];
run;

  proc sql noprint;
	create table thenames as
    select memname from dictionary.tables
    where libname = "PATH" and
          index(upper(memname),upper("_&InName._"))
    ;
    select  memname
    into :n1 separated by " " from thenames;
  quit;

options user=path;
data work.alldata;
	length sitecode $4 ;
	set &n1;
run;

%put &n1= ;

options user = work;

/* Count number of obs in each file to determine whether to run it*/
  proc sql;
	select count(distinct category) into :numcat from work.alldata;
	select distinct category into :catgn separated by "/" from work.alldata;
  quit;

%do j=1 %to &numcat.;
  %let catg = %scan(&catgn.,&j.,'/');

   ods tagsets.ExcelXP file="&path.\&sysdate. &catg. file &InName .xls" style=analysis
	options
    (embedded_titles="yes"	Embedded_footnotes="yes" 	Autofit_Height = "YES"
	default_column_width="50,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10");

    %local i cat;  %let i = 1;
    %let cat = %scan(%bquote(&medcodes),&i);

    %do %while(%bquote(&cat) ne);
		data _null_;
			dset=open('alldata');
			call symput('chk',left(trim(put(varnum(dset,"px"),2.))));
		run;

		%if &chk=0 %then %goto skip;

     	data &cat.;
			set alldata;
			if category="&catg.";
			if &cat. ne "" then output &cat.;
	 	run;

/* Create the format;*/
		data _g;
  			set &cat.;
  			retain fmtname "$fmt&cat.";
  			start = &cat.;
  			Label = description;
  			keep Fmtname Start Label;
		run;

		proc sort data=_g nodupkey; by start; run;
		proc format cntlin=_g ;

/* Prepare for tabulation*/
    	data dataclass;
     		set &cat. sitenames ;
	 		keep category  sitecode Description &cat;  *added description for non-ndc;
	 		if category='' then category="%upcase(&catg.)";
    	run;

/* Start tabulation*/
   		ods tagsets.ExcelXP options (Sheet_name="(%upcase(&cat.)) All %upcase(&cat)s");

		data dataclass; set dataclass;
		if &cat.='~Total' then description="~All %upcase(&cat) - Number of All &cat.**";
		data &cat; set &cat;
		if &cat.='~Total' then description="~All %upcase(&cat) - Number of All &cat.**";

   		proc tabulate data = &cat. missing format=LessSix. classdata=dataclass;
			where upcase(Category)="%upcase(&catg.)";
			freq &cat.count;  format sitecode $sitef.;
			keylabel N="";
			class Description category sitecode;
			table Description, sitecode /box="All %upcase(&cat.)s" misstext='.'; *added description for non-ndc;
		%titlefoots;
   		run;

   		ods tagsets.ExcelXP options (Sheet_name="(%upcase(&cat.)) People with %upcase(&cat.)s");

		data dataclass; set dataclass;
		if &cat.='~Total' then description="~All People - Number of All People with any &cat.**";
		data &cat; set &cat;
		if &cat.='~Total' then description="~All People - Number of All People with any &cat.**";

   		proc tabulate data = &cat. missing format=LessSix. classdata=dataclass;
			where upcase(Category)="%upcase(&catg.)";
			freq allpeople;  format sitecode $sitef.;
			keylabel N="";
			class &cat. Description category sitecode;
			table Description, sitecode /box="All People" misstext='.'; *added description for non-ndc;
	   	%titlefoots;
   		run;

   		ods tagsets.ExcelXP options (Sheet_name="(%upcase(&cat.)) Enrolled With %upcase(&cat.)s");

		data dataclass; set dataclass;
		if &cat='~Total' then description="~Enrolled - Number of enrolled people with any &cat.**";
		data &cat; set &cat;
		if &cat='~Total' then description="~Enrolled - Number of enrolled people with any &cat.**";
		run;

   		proc tabulate data = &cat. missing format=LessSix. classdata=dataclass;
			where upcase(Category)="%upcase(&catg.)";
			freq allEnrolledPeople;  format sitecode $sitef.;
			keylabel N="";
			class Description category sitecode; *class &cat. category sitecode;
			table Description, sitecode /box="Enrolled People" misstext='.';
		%titlefoots;
   		run;


   		ods tagsets.ExcelXP options (Sheet_name="(%upcase(&cat.)) Rates - All Enrollees");

		data dataclass; set dataclass;
		if &cat.='~Total' then description="~Rates - Rate of enrolled people with any &cat.**";run;
		data &cat; set &cat;
		if &cat.='~Total' then description="~Rates - Rate of enrolled people with any &cat.**";run;

   		proc tabulate data = &cat. missing format=LessSix. classdata=dataclass;
			where upcase(Category)="%upcase(&catg.)";
			freq rateallenr;  format sitecode $sitef.;
			keylabel N="Rate/10k";
			class Description category sitecode;
			table Description, sitecode /box="Enrolled Rates" misstext='.'; *added description for non-ndc;
	   	%titlefoots;
   		run;
   		title;
		   %skip:;
   		%let i = %eval(&i + 1);
   		%let cat = %scan(%bquote(&medcodes),&i);
   	%end; /*MEDCODES END*/
   ods tagsets.ExcelXP close;
%end;   /*CATG END*/

%mend VDWCountsAndRates2;

%macro check_dataset(dset =, obs_lim = max, eldest_age = 89) ;
  %local i ;
  %local inset_name ;
  %let inset_name = &dset ;

  %if %lowcase(&obs_lim) = max %then %do ;
    %** Nothing ;
  %end ;
  %else %do ;
    proc surveyselect
      data      = &inset_name
      out       = __sub_dset
      method    = srs
      sampsize  = &obs_lim SELECTALL
      seed      = 1234567
      noprint
    ;
    run;
    %let dset = __sub_dset ;
  %end ;

  %macro check_varname(regx, msg) ;
    create table possible_bad_vars as
    select name, label
    from these_vars
    where prxmatch(compress("/(&regx)/i"), name)
    ;

    %if &sqlobs > 0 %then %do ;
      insert into phi_warnings(dset, variable, label, warning)
      select "&inset_name" as dset, name, label, "&msg"
      from possible_bad_vars
      ;
    %end ;

  %mend check_varname ;

  %macro check_vars_for_mrn(length_limit = 6, obs_lim = max) ;
    %local char ;
    %let char = 2 ;
    proc sql noprint ;
      select name
      into :mrn_array separated by ' '
      from these_vars
      where type = &char and length ge &length_limit
      ;
    quit ;
    %if &sqlobs > 0 %then %do ;
      %put Checking these vars for possible MRN contents: &mrn_array ;
      data __gnu ;
        retain
          mrn_regex_handle
          badcount
        ;
        set &inset_name (obs = &obs_lim keep = &mrn_array) ;
        if _n_ = 1 then do ;
          mrn_regex_handle = prxparse("/&mrn_regex/") ;
          badcount = 0 ;
        end ;
        array p &mrn_array ;
        do i = 1 to dim(p) ;
          if prxmatch(mrn_regex_handle, p{i}) then do ;
            badvar = vname(p{i}) ;
            badvalue = p{i} ;
            badcount = _n_ ;
            output ;
          end ;
          keep badvar badvalue badcount ;
        end ;
      run ;
      proc sql noprint ;
        select compress(put(max(badcount), best.))
        into :badcount
        from __gnu
        ;
        insert into phi_warnings(dset, variable, warning)
        select distinct "&inset_name", badvar, "Could this var hold MRN values?  Contents of %trim(&badcount) records match the pattern given for MRN values.  MRNs should never move across sites."
        from __gnu ;
        drop table __gnu ;
      quit ;
    %end ;
  %mend check_vars_for_mrn ;

  %macro check_vars_for_oldsters(eldest_age = 89, obs_lim = max) ;
    %local dtfmts ;
    %let dtfmts = 'B8601DA','B8601DN','B8601DT','B8601DZ','B8601LZ','B8601TM','B8601TZ','DATE','DATEAMPM','DATETIME','DAY','DDMMYY',
                  'DDMMYYB','DDMMYYC','DDMMYYD','DDMMYYN','DDMMYYP','DDMMYYS','DOWNAME','DTDATE','DTMONYY','DTWKDATX','DTYEAR',
                  'DTYYQC','E8601DA','E8601DN','E8601DT','E8601DZ','E8601LZ','E8601TM','E8601TZ','HHMM','HOUR','JULDAY','JULIAN',
                  'MMDDYY','MMDDYYB','MMDDYYC','MMDDYYD','MMDDYYN','MMDDYYP','MMDDYYS','MMSS','MMYY','MMYY','MONNAME','MONTH','MONYY',
                  'PDJULG','PDJULI','QTR','QTRR','WEEKDATE','WEEKDATX','WEEKDAY','WEEKU','WEEKV','WEEKW','WORDDATE','WORDDATX',
                  'YEAR','YYMM','YYMMC','YYMMD','YYMMN','YYMMP','YYMMS','YYMMDD','YYMMDDB','YYMMDDC','YYMMDDD','YYMMDDN','YYMMDDP',
                  'YYMMDDS','YYMON','YYQ','YYQC','YYQD','YYQN','YYQP','YYQS','YYQR','YYQRC','YYQRD','YYQRN','YYQRP','YYQRS' ;

    %local num ;
    %let num = 1 ;

    proc sql noprint ;
      select name
      into :dat_array separated by ' '
      from these_vars
      where type = &num and (format in (&dtfmts) or lowcase(name) like '%date%')
      ;
      /* added by cb to shorten the process of looking at all dates */
      %if &sqlobs > 0 %then %do ;
        %put Checking these vars for possible DOB contents: &dat_array ;
        select 'min(' || trim(name) || ') as ' || name into :var_list separated by ','
        from these_vars
        where type = &num and (format in (&dtfmts) or lowcase(name) like '%date%')
        ;
        create table __gnu as
        select &var_list from &inset_name
        ;
      /* end cb additions */
    quit ;
      data __gnu ;
        set __gnu (obs = &obs_lim keep = &dat_array) ;
        array d &dat_array ;
        do i = 1 to dim(d) ;
          if n(d{i}) then maybe_age = %calcage(bdtvar = d{i}, refdate = "&sysdate9."d) ;
          if maybe_age ge &eldest_age then do ;
            badvar = vname(d{i}) ;
            badvalue = d{i} ;
            output ;
          end ;
          keep badvar badvalue maybe_age ;
        end ;
      run ;
      proc sql outobs = 30 nowarn ;
        insert into phi_warnings(dset, variable, warning)
        select distinct "&inset_name", badvar, "If this is a date, at least one value is " || compress(put(maybe_age, best.)) || " years ago, which is older than &eldest_age..  " ||
        "If this date applies to a person, the record is probably PHI."
        from __gnu ;
        drop table __gnu ;
      quit ;
    %end ;
    %else %do ;
      %put No obvious date variables found in &inset_name.--skipping age checks. ;
    %end ;
  %mend check_vars_for_oldsters ;

  proc contents noprint data = &inset_name out = these_vars ;
  run ;

  proc sql noprint ;
    create table phi_warnings (dset char(50), variable char(256), label char(256), warning char(200)) ;

    %check_varname(regx = mrn|hrn|patid|pat_id                                  , msg = %str(Name suggests this var may be an MRN or other patient identifier, which should never move across sites.)) ;
    %check_varname(regx = birth_date|BirthDate|DOB|BDate                        , msg = %str(Name suggests this var may be a date of birth.)) ;
    %check_varname(regx = SSN|SocialSecurityNumber|social_security_number|socsec, msg = %str(Name suggests this var may be a social security number.)) ;

    %if %symexist(locally_forbidden_varnames) %then %do ;
      %check_varname(regx = &locally_forbidden_varnames, msg = %str(May be on the locally defined list of variables not allowed to be sent to other sites.)) ;
    %end ;

  quit ;

  %check_vars_for_mrn(obs_lim = &obs_lim) ;
  %check_vars_for_oldsters(obs_lim = &obs_lim, eldest_age = &eldest_age) ;

  title3 "WARNINGS for dataset &inset_name:" ;

  proc sql noprint ;
    select count(*) as num_warns into :num_warns from phi_warnings ;

    %if &num_warns = 0 %then %do ;
      reset print outobs = 5 NOWARN ;
      select "No obvious PHI-like data elements in &inset_name--BUT PLEASE INSPECT THE CONTENTS AND PRINTs TO FOLLOW" as x label = "No warnings for &inset_name"
      from &inset_name
      ;
      %do i = 1 %to 5 ;
        %put No obvious phi-like data elements in &inset_name.  BUT PLEASE INSPECT THE CONTENTS AND PRINTs CAREFULLY TO MAKE SURE OF THIS! ;
      %end ;
    %end ;
    %else %do ;
      reset print ;
      select variable, warning from phi_warnings
      order by variable, warning
      ;
      quit ;
    %end ;
    title3 "Dataset &inset_name" ;
    proc contents data = &inset_name varnum ;
    run ;
  /*
    proc print data = &inset_name (obs = 20) ;
    run ;
  */
    ** TODO: make the print print out recs that trip the value warnings. ;
    proc sql number ;
      select *
      from &inset_name (obs = 20)
      ;
    quit ;

  quit ;

  %RemoveDset(dset = __sub_dset) ;
  %RemoveDset(dset = possible_bad_vars) ;
  %RemoveDset(dset = phi_warnings) ;
  %RemoveDset(dset = these_vars) ;

%mend check_dataset ;

%macro detect_phi(transfer_lib, obs_lim = max, eldest_age = 89) ;

  %put ;
  %put ;
  %put ============================================================== ;
  %put ;
  %put Macro detect_phi: ;
  %put ;
  %put Checking all datasets found in %sysfunc(pathname(&transfer_lib)) for the following signs of PHI: ;
  %put   - Variable names signifying sensitive items like 'MRN', 'birth_date', 'SSN' and so forth. ;
  %if %symexist(locally_forbidden_varnames) %then %do ;
    %put   - Variable names on the list defined in the standard macro variable locally_forbidden_varnames (here those names are: &locally_forbidden_varnames). ;
  %end ;
  %put   - Contents of CHARACTER variables that match the pattern given in the standard macro variable mrn_regex (here that var is &mrn_regex) ;
  %put     Please note that numeric variables ARE NOT CHECKED FOR MRN-LIKE CONTENT. ;
  %put   - The contents of date variables (as divined by their formats) for values that, if they were DOBs, would indicate a person older than &eldest_age years. ;
  %put ;
  %put THIS IS BETA SOFTWARE-PLEASE SCRUTINIZE THE RESULTS AND REPORT PROBLEMS TO pardee.r@ghc.org. ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put ;
  %put ============================================================== ;
  %put ;
  %put ;

  title1 "PHI-Detection Report for the datasets in %sysfunc(pathname(&transfer_lib))." ;
  title2 "please inspect all output carefully to make sure it comports with your data sharing agreement!!!" ;

  proc sql noprint ;
    ** describe table dictionary.tables ;

    select trim(libname) || '.' || memname as dset
    into   :d1-:d999
    from dictionary.tables
    where libname = "%upcase(&transfer_lib)" AND
          memtype = 'DATA'
    ;
    %local num_dsets ;
    %let num_dsets = &sqlobs ;
  quit ;

  %local i ;

  %if &num_dsets = 0 %then %do i = 1 %to 10 ;
    %put ERROR: NO DATASETS FOUND IN &transfer_lib!!!! ;
  %end ;

  %do i = 1 %to &num_dsets ;
    %put about to check &&d&i ;
    %check_dataset(dset = &&d&i, obs_lim = &obs_lim, eldest_age = &eldest_age) ;
  %end ;

%mend detect_phi ;

%** Author on this is Jack Hamilton of KPNC <Jack.Hamilton@kp.org>. ;
%macro data_set_list(
    library=work,
    macvar=data_set_list,   /* Name of macro variable in which to put list  */
    filter=1,               /* Data set name filter                         */
    order=1,                /* Default order is by name                     */
    in_prefix=,             /* If valued, create IN= data set option        */
    readpw=                 /* If there's a common read password            */
    );

    /* Make sure the output macro variable list exists */
    %global &MACVAR.;

    /* Get a list of data sets.  Could have used dictionary tables or ODS   */
    /* OUTPUT, but both of those methods may have bad side effects.         */
    proc datasets library=&LIBRARY. nolist nodetails
                %if %length(&READPW.) ne 0
                %then
                    %do;
                    read=&READPW.
                    %end;
                ;
        contents data=_all_ memtype=(data view) out=work._data_ noprint;
        run;
    quit;

    proc sql noprint;
        /* Filter and order the names and put into the macro variable.      */
        select distinct
            catt(
                "&LIBRARY..",
                memname
                %if %length(&IN_PREFIX.) ne 0
                %then
                    %do;
                    ,
                    catt(
                        " (in=&IN_PREFIX." ,
                        memname           ,
                        ')'
                        )
                    %end;
                )
        into
            :&MACVAR. separated by ' '
        from
            _last_
        where
            &FILTER.
        order by
            &ORDER.
        ;

        /* Drop the contents data set.   */
        drop table _last_;
    quit;

    /* Show what we got.  */
    %put INFO: &MACVAR.=&&&MACVAR.;

%mend data_set_list;

%** Author on this is Jack Hamilton of KPNC <Jack.Hamilton@kp.org>. ;
%macro Write_File(
    file= ,     /* Name of file to write to        */
    text= ,     /* Text to write to &FILE.         */
    mode=a      /* Write mode, A=Append, O=Output  */
    );

    %local fileref rc fid;
    %let fileref = ;

    /* Generate fileref name and assign to file.  */
    %let rc = %sysfunc(filename(fileref, &FILE.));

    /* Open file with Append access (if mode=a).  */
    %let fid = %sysfunc(fopen(&FILEREF., &MODE.));

    /* Write the text to the file, then close it.  */
    %if &FID. > 0
    %then
        %do;
        %let rc = %sysfunc(fput(&FID., &TEXT.));
        %let rc = %sysfunc(fwrite(&FID.));
        %let rc = %sysfunc(fclose(&FID.));
        %end;
    %else
        %put %sysfunc(sysmsg());

    %let rc = %sysfunc(filename(fileref));

%mend write_file;

%macro stack_datasets(inlib =, nom = , outlib = , srcvar = site, outnom = , delete_insets = no) ;
  ** All input datasets live in inlib.
  ** All input dataset names begin with <<site abbreviation>>_ and end with the text passed in the nom parameter. ;
  ** This guy creates a big old UNION query against them all and then executes it to create a dataset named <<nom>> in the outlib library. ;

  ** New param srcvar: name the var in the output dataset that signifies the name of the input dataset. ;
  ** New param outnom: name the output dataset. ;
  ** New param delete_insets: remove the datasets that contributed to the output. ;

  %if &outnom = %then %let outnom = &nom ;

  %local i rgx ;
  %let rgx = (.*)_&nom.\s*$ ;

  proc sql ;
    ** create table s.drop_me as    select *    from dictionary.tables    ;

    ** Do we have any dsets w/0 vars?  These will cause barfage. ;
    create table __novars as
    select memname label = "THESE DATASETS HAVE 0 VARIABLES AND CANNOT BE USED!!!", memlabel
    from dictionary.tables
    where libname = "%upcase(&inlib)" AND
          nvar = 0 AND
          prxmatch("/&rgx./i", memname) > 0
    ;

    %if &sqlobs > 0 %then %do ;
      %do i = 1 %to 5 ;
        %put WARNING: There are %trim(&sqlobs) datasets in &inlib that have 0 variables.  See the output for a list. ;
      %end ;
      select * from __novars ;
    %end ;

    drop table __novars ;

    reset noprint feedback ;

    select memname as dset
         , 'select *, "' || prxchange("s/&rgx./$1/i", -1, memname) || '" as &srcvar from ' || "&inlib.." || memname as sequel
         ,                  prxchange("s/&rgx./$1/i", -1, memname) as &srcvar
    into   :dset1-:dset100
         , :union_stmt separated by ' UNION ALL CORRESPONDING '
         , :sitelist separated by ', '
    from dictionary.tables
    where libname = "%upcase(&inlib)" AND
          nvar > 0 AND
          prxmatch("/&rgx./i", memname) > 0
    ;

    %let num_inputs = &sqlobs ;

    %if &num_inputs = 0 %then %do ;
      %do i = 1 %to 5 ;
        %put ERROR: No datasets whose names end with "%trim(&nom)" found in input location %sysfunc(pathname(&inlib)) !!! ;
      %end ;
      reset print ;
      select libname, memname, '%' || "%upcase(&nom)" as match_expression, memlabel
      from dictionary.tables
      ;

    %end ;
    %else %do ;
      create table &outlib..&outnom as
      &union_stmt
      ;
      %if &delete_insets = yes %then %do i = 1 %to &num_inputs ;
        drop table &inlib..&&dset&i ;
      %end ;
    %end ;
  quit ;

  * SQL UNION does well w/var lengths--takes max one, which is great. ;
  * Its less great w/formats though--those dont get maxed out. ;
  * Removing them so they dont print truncated & cause confusion. ;
  proc datasets nolist library = &outlib ;
    modify &outnom ;
    attrib _character_ format= ;
  run ;

%mend stack_datasets ;

%macro generate_counts_rates(incodeset = /* Name of an input dset of data types, code types, categories and codes (see below). */
                          , start_date = /* Beginning of the period over which you want the counts/rates. */
                          , end_date   = /* End of the period over which you want the counts/rates. */
                          , cohort     = /* Optional--if your interest is limited to an enumerated population of peple, name the dset of MRNs identifying them here. */
                          , outpath    = /* Path giving the location where you want the output files that will contain the counts/rates. */
                          , outfile    = /* Base name of the output files (so--no extension).  'my_file' will produce '<<siteabbrev>>_my_file.sas7bdat' */
                          , censor_low = Y /* If set to N, it will skip the lowest-count redacting (mostly useful for debugging and single-site use). */
                        ) ;

  /*
    InCodeSet
      data_type: one of PX, DX, NDC, LAB.
      code_type: one of the valid values for px_codetype, dx_codetype, or null for NDCs/Labs.
      category: a user-specified string that can be used to group codes into categories (e.g., 'Analgesics', 'Therapeutic Radiation').
      descrip: a more fine-grained description of the particular code.  You could think of this as a subcategory (since codes w/the same descrip value get rolled up at the reporting stage).
      code: the actual NDC, ICD-9 dx code, etc.
  */

  libname __out "&outpath" ;

  %local __out ;
  %let __out = __out.&_SiteAbbr._&outfile ;

  %local _proceed ;

  %macro validate_codeset() ;
    %** Makes sure the input codeset has expected vars and values. ;
    proc contents noprint data = &incodeset out = _codevars ;
    run ;
    proc sql noprint ;
      ** select * from _codevars ;
      **describe table _codevars ;
      select count(*)
      into :num_vars
      from _codevars
      where lowcase(name) in ('data_type', 'code_type', 'category', 'code', 'descrip')
      ;
      drop table _codevars ;
    quit ;
    %if &num_vars < 5 %then %do ;
      %let _proceed = 0 ;
    %end ;
    %else %do ;
      %let _proceed = 1 ;
    %end ;
  %mend validate_codeset ;

  %validate_codeset ;

  %if &_proceed = 0 %then %do ;
    %do i = 1 %to 5 ;
      %put ERROR: Input dataset of codes &incodeset does not have the expected variables!!! ;
    %end ;
    %goto exit ;
  %end ;

  %macro gather_any_data(dtype = PX, dset = &_vdw_px, date_var = adate, join_condition = %str(d.px_codetype = i.code_type AND d.px = i.code), outset = __blah) ;
    proc sql ;
      create table __grist as
      select d.mrn, e.mrn as e_mrn, data_type, code_type, category, code, descrip
      from &dset as d INNER JOIN
           &incodeset as i
      on   &join_condition
           %if %length(&cohort) > 0 %then %do ;
            INNER JOIN &cohort as c
            on  d.mrn = c.mrn
           %end ;
           LEFT JOIN
           &_vdw_enroll as e
      on   d.mrn = e.mrn AND
           &date_var between e.enr_start and e.enr_end
      where  i.data_type = "&dtype" AND
             &date_var between "&start_date"d and "&end_date"d
      ;
      create table &outset as
      select data_type, category, descrip
            , count(*) as num_recs                      format = comma9.0 label = "No. records"
            , count(distinct mrn)   as num_ppl          format = comma9.0 label = "No. people (enrolled or not)"
            , count(distinct e_mrn) as num_enrolled_ppl format = comma9.0 label = "No. *enrolled* people"
      from __grist
      group by data_type, category, descrip
      ;

      create table __subt as
      select data_type, category
            , count(*) as num_recs                      format = comma9.0 label = "No. records"
            , count(distinct mrn)   as num_ppl          format = comma9.0 label = "No. people (enrolled or not)"
            , count(distinct e_mrn) as num_enrolled_ppl format = comma9.0 label = "No. *enrolled* people"
      from __grist
      group by data_type, category
      ;

      drop table __grist ;

      insert into &outset(data_type, category, descrip, num_recs, num_ppl, num_enrolled_ppl)
      select              data_type, category, "~SUBTOTAL for &dtype in this category:" as descrip, num_recs, num_ppl, num_enrolled_ppl
      from __subt
      ;

      drop table __subt ;

    quit ;

  %mend gather_any_data ;

  %macro gather_px(outset = _pxcounts) ;
    %gather_any_data(dtype = PX, dset = &_vdw_px, date_var = adate, join_condition = %str(d.px_codetype = i.code_type AND d.px = i.code), outset = &outset) ;
  %mend gather_px ;

  %macro gather_dx(outset = _dxcounts) ;
    %gather_any_data(dtype = DX, dset = &_vdw_dx, date_var = adate, join_condition = %str(d.dx_codetype = i.code_type AND d.dx = i.code), outset = &outset) ;
  %mend gather_dx ;

  %macro gather_rx(outset = _rxcounts) ;
    %gather_any_data(dtype = NDC, dset = &_vdw_rx, date_var = rxdate, join_condition = %str(d.ndc = i.code), outset = &outset) ;
  %mend gather_rx ;

  %macro gather_lab(outset = _labcounts) ;
    %gather_any_data(dtype = LAB, dset = &_vdw_lab, date_var = %str(coalesce(result_dt, lab_dt, order_dt)), join_condition = %str(d.test_type = i.code), outset = &outset) ;
  %mend gather_lab ;

  proc sql noprint ;
    select distinct lowcase(data_type) as dt
    into :dt1 - :dt4
    from &incodeset
    ;
    %let num_data_types = &sqlobs ;
  quit ;

  %if &num_data_types > 0 %then %do ;
    %removedset(dset = &__out) ;

    proc sql noprint ;
    	select count(distinct e.mrn) as EnrPple into :EnrPple
      from &_vdw_enroll as e
      %if %length(&cohort) > 0 %then %do ;
        INNER JOIN &cohort as c
        on  e.mrn = c.mrn
      %end ;
    	where "&start_date"d between e.enr_start and e.enr_end ;
    quit;

  %end ;

  %do i = 1 %to &num_data_types ;
    %let this_one = &&dt&i ;
    %put Working on &this_one ;
    %if &this_one = dx %then %do ;
      %gather_dx(outset = _counts) ;
    %end ;
    %else %if &this_one = ndc %then %do ;
      %gather_rx(outset = _counts) ;
    %end ;
    %else %if &this_one = lab %then %do ;
      %gather_lab(outset = _counts) ;
    %end ;
    %else %if &this_one = px %then %do ;
      %gather_px(outset = _counts) ;
    %end ;
    %else %do ;
      %do j = 1 %to 5 ;
        %put ERROR: Do not understand data_type value "&this_one"--skipping! ;
      %end ;
      %**goto exit ;
    %end ;
    proc append base = &__out data = _counts ;
    run ;
    %removedset(dset = _counts) ;
  %end ;

  data &__out (label = "Counts at site &_SiteName for period from &start_date to &end_date") ;
    set &__out ;
    %if %upcase(&censor_low) = Y %then %do ;
      ** Redact any counts that are less than &lowest_count ;
      array n num_recs num_ppl num_enrolled_ppl ;
      do i = 1 to dim(n) ;
        if n{i} gt 0 and n{i} lt &lowest_count then n{i} = .a ;
      end ;
      drop i ;
    %end ;
    if num_enrolled_ppl then rate_enrolled_ppl = int((num_enrolled_ppl / &EnrPple.) * 10000) ;

    label
      rate_enrolled_ppl = "Rate of enrolled people per 10k enrollees"
    ;
    format rate_enrolled_ppl comma8.0 ;
  run ;

  proc sort data = &__out ;
    by data_type category ;
  run ;

  %** Now supplement the output dset w/any codes that did not appear anywhere in the site data. ;
  proc sql ;
    create table __not_found as
    select distinct i.data_type
            , i.category
            , i.descrip
    from  &incodeset as i LEFT JOIN
          &__out as o
    on    i.data_type = o.data_type AND
          i.category = o.category
    where o.data_type IS NULL
    ;

    %if &sqlobs > 0 %then %do ;
      insert into &__out (data_type
                        , category
                        , descrip
                        , num_recs
                        , num_ppl
                        , num_enrolled_ppl
                        , rate_enrolled_ppl)
      select     data_type
                , category
                , descrip
                , 0 as num_recs
                , 0 as num_ppl
                , 0 as num_enrolled_ppl
                , 0 as rate_enrolled_ppl
      from __not_found
      ;
    %end ;

    drop table __not_found ;

  quit ;

  %** Switching to tabulate in order to avoid ERR: The ID columns were too wide for the LINESIZE to print
  %** the special report usually generated when BY and ID lists are identical. ;
	proc tabulate data = &__out (obs=200) missing format = comma9.0 ; ** classdata = &incodeset ;
		title1 "Here is a sample of what you are sending out" ;
		title2 "Please inspect the full dataset in &outpath.&_SiteAbbr._&outfile..sas7bdat before sending." ;
		class data_type descrip category / missing ;
		classlev descrip / style=[outputwidth=5.5in] ;
		var num_: rate_enrolled_ppl ;
		table data_type="Type of data" * (category * descrip="Event") , (num_recs num_ppl num_enrolled_ppl rate_enrolled_ppl)*SUM=" " / misstext = '.' box = "Data to be sent" ;
	run;

  %exit: ;

%mend generate_counts_rates ;

%macro report_counts_rates(inlib =        /* lib where the site-submitted dsets live */
                          , dset_name =   /* the stub dataset name to use to identify which dsets should be part of this report */
                          , outlib =      /* the lib where you want the output--a single aggregated dset + xls files for each category found */
                          , sitefmt =     /* optional--the name of the format to use for the site variable. */
                          ) ;

  ** title1 "Counts/Rates from &dset_name.." ;
  %local i rgx ;
  %let rgx = s/[^a-z]/_/ ;

  %stack_datasets(inlib = &inlib, nom = &dset_name, outlib = &outlib) ;

  proc format ;
    value $dt
      "PX" = "Procedure"
      "DX" = "Diagnosis"
      "NDC" = "Rx Fill"
      "LAB" = "Lab Result"
    ;
  quit ;

  %macro distinct(var =, outset = ) ;
    proc sort nodupkey data = gnu(keep = &var) out = &outset ;
      by &var ;
    run ;
  %mend distinct ;

  %macro new_sheet(tab_name, var, box_text = " ") ;
    ods tagsets.ExcelXP options (sheet_interval = 'none' sheet_name = "&tab_name") ;

    ** proc print ;
    **   var category code descrip &var ;
    ** run ;

 		proc tabulate data = gnu missing format = comma10.0 classdata = classes ;
  		freq &var;  ;
  		keylabel N=" ";
  		class data_type descrip category site / missing ;
   ** table category="Category" * (data_type="Type of data" * descrip="Event" * code="Signifying Code" all="Category Totals") , site*N*[style=[tagattr='format:#,###']] / misstext = '.' box = &box_text ;
   ** table category="Category" * (data_type="Type of data" * descrip="Event" all="Category Totals") , site*N*[style=[tagattr='format:#,###']] / misstext = '.' box = &box_text ;
  		table data_type="Type of data" * (descrip="Event") , site*N*[style=[tagattr='format:#,###']] / box = &box_text ; ** misstext = '.' ;
  		format data_type $dt. ;
  		%if %length(&sitefmt) > 0 %then %do ;
  		  format site &sitefmt ;
  		%end ;
 		run;


  %mend new_sheet ;

  %macro do_category(cat) ;
    %** Purpose: Runs a report for one of the categories. ;
    %let this_file = "%sysfunc(pathname(&outlib))/&cat..xls" ;
    %put Working on &cat.. ;
    %put File will be &this_file.. ;

    %** Subset to our category of interest ;
    proc sort data = &outlib..&dset_name out = gnu ;
      by data_type site descrip ;
      where prxchange("&rgx", -1, trim(lowcase(category))) = "&cat" ;
    run ;

    %** Create the classdata dataset (used in new_sheet above). ;
    %distinct(var = site, outset = _site) ;
    %distinct(var = %str(data_type category descrip), outset = _descr) ;

    proc sql noprint ;
      create table classes as
      select data_type, descrip, category, site
      from _descr CROSS JOIN _site
      ;

      select category
      into :category
      from classes
      ;
    quit ;

    title "&category" ;

    ods tagsets.ExcelXP
      file = &this_file
      style = analysis
      options (
                Frozen_Headers        = "5"
                Frozen_RowHeaders     = "2"
                embedded_titles       = "yes"
                embedded_footnotes    = "yes"
                autofit_height        = "yes"
                absolute_column_width = "12, 40, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5"
                orientation           = "landscape"
                )
    ;

      %new_sheet(tab_name = Records   , var = num_recs          , box_text = "Raw record counts") ;
      %new_sheet(tab_name = People    , var = num_ppl           , box_text = "Counts of people (enrolled or not)") ;
      %new_sheet(tab_name = Enrollees , var = num_enrolled_ppl  , box_text = "Counts of people enrolled at the time of the event.") ;
      %new_sheet(tab_name = Rates     , var = rate_enrolled_ppl , box_text = "Rates per 10k enrollees ") ;

    ods tagsets.ExcelXP close ;
  %mend do_category ;

  proc sql noprint ; ;
    select distinct prxchange("&rgx", -1, trim(lowcase(category))) as cat
    into :cat1 - :cat999
    from &outlib..&dset_name
    ;
    %local num_cats ;
    %let num_cats = &sqlobs ;
  quit ;

  %do i = 1 %to &num_cats ;
    %let this_cat = &&cat&i ;
    %do_category(cat = &this_cat) ;
  %end ;

%mend report_counts_rates ;


/*********************************************
* Sharon Fuller
* Group Health Research Institute
* (206) 287-2552
* fuller.s@ghc.org
*
*Purpose: Prioritize and assign a single race per person
*					If only one race is listed, set CombinedRace to that race
*					If person is WH and one other, set CombinedRace to that other (or to "MU" depending on parameters)
*					If there is more than one non-WH race, or one of the races is "MU", set CombinedRace to "MU"
*					Otherwise, set CombinedRace to "UN"
*
*NOTES: this macro depends on the races being listed in the following order (which they currently are per VDW spec)
*				"HP" = "Native Hawaiian or Other Pacific Islander"
*				"IN" = "American Indian/Alaska Native"
*				"AS" = "Asian"
*				"BA" = "Black or African American"
*				"WH" = "White"
*				"MU" = "More than one race"
*				"UN" = "Unknown or Not Reported"
*      so, for example, if Race1 is WH, you know there will only be MU or UN in subsequent race fields
*
*********************************************/

%macro GetCombinedRace (Inset,        /* The name of a dataset containing the MRNs of people
                                        whose race you want to identify. */
                    Outset,           /* The name of the output dataset - contains vars from inset,
                                        plus CombinedRace and Race1--Race5.
                                        Inset and Outset can be the same.*/
                    Freqs = 'N',      /*Defaults to 'N'.  If change to 'Y' will get some freqs in list file.*/
                    WHOther = 'other' /*Options are 'other' and 'MU'.
                                        If a person is White and one other race, should they be counted as
                                        that other race (default), or as multiple.*/
                    );

  proc sql;
    create table work.race as
    select i.*
      , d.Race1, d.Race2, d.Race3, d.Race4, d.Race5
    from &Inset.                    i
      left join &_vdw_demographic.  d on i.mrn = d.mrn/*change from vdw.demog*/
    ;
  quit;

  *added upcase throughout - I believe this was based on bad experience with multisite code;
  data &Outset. (drop = i);
    set work.race;
    length combinedrace $2. i 8.;
    combinedrace = "UN";
    array aryrace {*} Race1 Race2 Race3;/*added Race3 so combos like AS WH OT will be counted as MU, rather than AS*/
    do i=1 to dim(aryrace);
      /*if aryrace{i} = "HP" then do;CombinedRace = "HP";end;*//*handling below so macro no longer dependent on races being in a standard order*/
      /*else*/ if upcase(aryrace{i}) in ("HP", "IN" , "AS", "BA", "OT") then do;/*added HP to list*/
        /*if combinedrace in ("UN", "WH") then CombinedRace = aryrace{i};*/
        if combinedrace ="UN" then CombinedRace = upcase(aryrace{i});
        else if CombinedRace = "WH" then do; /*added clause to handle situations like WH OT*/
          if &WHOther. = "MU" then CombinedRace = "MU"; else CombinedRace = upcase(aryrace{i});
        end;
        else combinedrace = "MU";
      end;
      else if upcase(aryrace{i}) = "WH" then do;
        if combinedrace = "UN" then CombinedRace = "WH";
        else if &WHOther = "MU" then combinedrace = "MU";
      end;
      else if upcase(aryrace{i})="MU" then CombinedRace = "MU";
    end;
  run;

  %if &freqs. = 'Y' %then %do;
    proc freq data=&outset.;
    tables Race1 Race2 race3 race4 race5
              CombinedRace
              CombinedRace*Race1*Race2*Race3*Race4*Race5 /list nocum missing;
    run;
  %end;

%mend getcombinedrace;

%macro BloodCancerDefinition01(outds=CancerCases, startdt=01jan1990, enddt=31dec2007) ;
  **************************************************************************
  ** This defintion was used in the Stroke and Chemotherapy Project
  ** Added at the request of Gene Hart (206) 287-2949
  **************************************************************************;
  data &outds ;
      set &_vdw_tumor ;
  	where DxDate between "&startdt."d and "&enddt."d;
  	if morph ge '9650' and morph le '9667' then Hodgkin_Lymphoma = 1;
  		else Hodgkin_Lymphoma = 0;
  		if (morph ge '9590' and morph le '9596') then NonHodgkin_Lymphoma = 1;
  		else if (morph ge '9670' and morph le '9719') then  NonHodgkin_Lymphoma = 1;

  		else if (morph ge '9727' and morph le '9729')then NonHodgkin_Lymphoma = 1;
  		else NonHodgkin_Lymphoma = 0;
  	if  (morph ge '9800' and morph le '9948') then Leukemia = 1;
  		else Leukemia = 0;
  	if substr(icdosite, 1, 2)= 'C4'  and morph = '9731'  then Multiple_Myeloma = 1;
  		else if substr(icdosite, 1, 2)= 'C4' and morph = '9732' then Multiple_Myeloma = 1;
  		else if  substr(icdosite, 1, 2)= 'C4' and morph = '9761' then Multiple_Myeloma = 1;
  		else  Multiple_Myeloma = 0;
  run;
%mend BloodCancerDefinition01 ;

%macro CancerSchema(OutputDS,StartDt,EndDt);
/************************************************************************************************************************
* This macro takes cancers in your VDW Tumor file for the time period of interest and allocates them to cancer schema
* For AJCC 6th edition the variable created is CancerSchemaAjcc6thEdition - all cancers are included
*     https://cancerstaging.org/references-tools/deskreferences/Documents/AJCC6thEdCancerStagingManualPart1.pdf
*
* For AJCC 7th edition the variable created is CancerSchmaAjcc7thEdition - only 9 common cancers are currently defined in this macro
*     http://ebookee.org/AJCC-Cancer-Staging-Manual-7th-Edition_1494657.html
*
* Arguments
*   OutputDS is the name of the SAS dataset that will be output
*   StartDt is the start date of interest in the format 01jan2000
*   EndDt is the end date of interest in the format 31dec2000
************************************************************************************************************************/
;
proc sql;
  create table &OutputDS as
  select t.*,
         case
         when (ICDOSite between 'C500' and 'C506' or
              ICDOsite between 'C508' and 'C509') and
              (MORPH between '8000' and '8576' or
               MORPH between '8940' and '8950' or
               MORPH between '8980' and '8981' or
               Morph=        '9020')
              then 'Breast'

         when (ICDOSite='C619') and
              (MORPH between '8000' and '8110' or
               MORPH between '8140' and '8576' or
               MORPH between '8940' and '8950' or
               MORPH between '8980' and '8981')
              then 'Prostate'

         when (ICDOSite='C180' or
               ICDOsite between 'C182' and 'C189') and
              (MORPH between '8000' and '8152' or
               MORPH between '8154' and '8231' or
               MORPH between '8243' and '8245' or
               MORPH      in('8247','8248')    or
               MORPH between '8250' and '8576' or
               MORPH between '8940' and '8950' or
               MORPH between '8980' and '8981')
              then 'Colon'

         when (ICDOSite between 'C340' and 'C343' or
               ICDOsite between 'C348' and 'C349') and
              (MORPH between '8000' and '8576' or
               MORPH between '8940' and '8950' or
               MORPH between '8980' and '8981')
              then 'Lung'

         when (ICDOSite between 'C670' and 'C679') and
              (MORPH between '8000' and '8576' or
               MORPH between '8940' and '8950' or
               MORPH between '8980' and '8981')
              then 'Bladder'

         when (ICDOSite='C569' ) and
              (MORPH between '8000' and '8576' or
               MORPH between '8590' and '8671' or
               MORPH between '8930' and '9110')
              then 'Ovary'

         when (ICDOSite between 'C440' and 'C449' or
               ICDOsite between 'C510' and 'C512' or
               ICDOsite between 'C518' and 'C519' or
               ICDOsite between 'C600' and 'C602' or
               ICDOsite between 'C608' and 'C609' or
               ICDOsite= 'C632' ) and
              (MORPH between '8720' and '8790')
              then 'Melanoma'

         when (ICDOSite between 'C530' and 'C531' or
               ICDOsite between 'C538' and 'C539' ) and
              (MORPH between '8000' and '8576' or
               MORPH between '8940' and '8950' or
               MORPH between '8980' and '8981')
              then 'Cervix'

         else 'Not yet coded'
         end as CancerSchemaAjcc7thEdition,

          case
    when ( ICDOsite BETWEEN 'C000' and 'C009' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Lip'

    when ( ICDOsite BETWEEN 'C019' and 'C029' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Tongue'

    when ( ICDOsite BETWEEN 'C079' and 'C089' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Salivary Gland'

    when ( ICDOsite BETWEEN 'C040' and 'C049' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Floor of Mouth'

    when ( (ICDOsite BETWEEN 'C030' and 'C039') or (ICDOsite BETWEEN 'C050' and 'C059') or ICDOsite BETWEEN 'C060' and 'C069' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Gum and other mouth'

    when ( ICDOsite BETWEEN 'C110' and 'C119' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Nasopharynx'

    when ( ICDOsite BETWEEN 'C090' and 'C099' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Tonsil'

    when ( ICDOsite BETWEEN 'C100' and 'C109' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Oropharynx'

    when ( (ICDOsite='C129') or (ICDOsite BETWEEN 'C130' and 'C139') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Oral Cavity and Pharynx'

    when ( (ICDOsite='C140') or (ICDOsite BETWEEN 'C142' and 'C148') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Hypopharynx'

    when ( ICDOsite BETWEEN 'C150' and 'C159' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Esophagus'

    when ( ICDOsite BETWEEN 'C160' and 'C169' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Stomach'

    when ( ICDOsite BETWEEN 'C170' and 'C179' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Lip'

    when ( ICDOsite='C180' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Cecum'

    when ( ICDOsite='C181' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Appendix'

    when ( ICDOsite='C182' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Ascending Colon'

    when ( ICDOsite='C183' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Hepatic Flexure'

    when ( ICDOsite='C184' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Transverse Colon'

    when ( ICDOsite='C185' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Splenic Flexure'

    when ( ICDOsite='C186' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Descending Colon'

    when ( ICDOsite='C187' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Sigmoid Colon'

    when ( (ICDOsite BETWEEN 'C188' and 'C189') or (ICDOsite='C260') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Large Intestine, NOS'

    when ( ICDOsite='C199' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Rectosigmoid Junction'

    when ( ICDOsite='C209' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Rectum'

    when ( (ICDOsite BETWEEN 'C210' and 'C212') or (ICDOsite='C218') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Anus, Anal Canal and Anorectum'

    when ( ICDOsite='C220' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Liver'

    when ( ICDOsite='C221' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Intrahepatic Bile Duct'

    when ( ICDOsite='C239' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Gall Bladder'

    when ( ICDOsite BETWEEN 'C240' and 'C249' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Biliary'

    when ( ICDOsite BETWEEN 'C250' and 'C259' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Pancreas'

    when ( ICDOsite='C480' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Retroperitoneum'

    when ( ICDOsite BETWEEN 'C481' and 'C482' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Peritoneum, Omentum and Mesentery'

    when ( (ICDOsite BETWEEN 'C268' and 'C269') or (ICDOsite='C488') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Digestive Organs'

    when ( (ICDOsite BETWEEN 'C300' and 'C301') or (ICDOsite BETWEEN 'C310' and 'C319') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Nose, Nasal Cavity and Middle Ear'

    when ( ICDOsite BETWEEN 'C320' and 'C329' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Larynx'

    when ( ICDOsite BETWEEN 'C340' and 'C349' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Lung and Bronchus'

    when ( (ICDOsite BETWEEN 'C381' and 'C383') or (ICDOsite in ('C339' 'C388' 'C390' 'C398' 'C399')) )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Trachea, Mediastinum and Other Respiratory Organs'

    when ( ICDOsite BETWEEN 'C400' and 'C419' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Bones and Joints'

    when ( (ICDOsite='C380') or (ICDOsite BETWEEN 'C470' and 'C479') or (ICDOsite BETWEEN 'C490' and 'C499') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Soft Tissue, including Heart'

    when ( ICDOsite BETWEEN 'C440' and 'C449' )
     and ( MORPH BETWEEN '8720' and '8790' )
       then 'Melanoma of the Skin'

    when ( ICDOsite BETWEEN 'C440' and 'C449' )
     and NOT( MORPH BETWEEN '8000' and '8005' ) and NOT( MORPH BETWEEN '8010' and '8046')
     and NOT( MORPH BETWEEN '8050' and '8084' ) and NOT( MORPH BETWEEN '8090' and '8110')
     and NOT( MORPH BETWEEN '8720' and '8790' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Non-Epithelial Skin'

    when ( ICDOsite BETWEEN 'C500' and 'C509' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Breast'

    when ( ICDOsite BETWEEN 'C530' and 'C539' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Cervix Uteri'

    when ( ICDOsite BETWEEN 'C540' and 'C549' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Corpus Uteri'

    when ( ICDOsite='C559' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Uterus, NOS'

    when ( ICDOsite='C569' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Ovary'

    when ( ICDOsite='C529' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Vagina'

    when ( ICDOsite BETWEEN 'C510' and 'C519' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Vulva'

    when ( ICDOsite BETWEEN 'C570' and 'C589' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Female Genital Organs'

    when ( ICDOsite='C619' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Prostate'

    when ( ICDOsite BETWEEN 'C620' and 'C629' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Testis'

    when ( ICDOsite BETWEEN 'C600' and 'C609' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Penis'

    when ( ICDOsite BETWEEN 'C630' and 'C639' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Male Genital Organs'

    when ( ICDOsite BETWEEN 'C670' and 'C679' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Urinary Bladder'

    when ( (ICDOsite='C649') or (ICDOsite='C659') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Kidney and Renal Pelvis'

    when ( ICDOsite='C669' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Ureter'

    when ( ICDOsite BETWEEN 'C680' and 'C689' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Urinary Organs'

    when ( ICDOsite BETWEEN 'C690' and 'C699' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Eye and Orbit'

    when ( ICDOsite BETWEEN 'C710' and 'C719' )
     and NOT( MORPH BETWEEN '9530' and '9539')
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Brain'

    when ( ICDOsite BETWEEN 'C710' and 'C719' )
       and ( MORPH BETWEEN '9530' and '9539' )
       then 'Cranial Nerves Other Nervous System'

    when ( (ICDOsite BETWEEN 'C700' and 'C709') or (ICDOsite BETWEEN 'C720' and 'C729') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Cranial Nerves Other Nervous System'

    when ( ICDOsite='C739' )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Thyroid'

    when ( (ICDOsite='C739') or (ICDOsite BETWEEN 'C740' and 'C749') or (ICDOsite BETWEEN 'C750' and 'C759') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Other Endocrine including Thymus'

    when ( (ICDOsite in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C422')) or (ICDOsite BETWEEN 'C770' and 'C779') )
       and (MORPH BETWEEN '9650' and '9667')
       then 'Hodgkin - Nodal'

    when ( (ICDOsite NOT in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C422')) and NOT(ICDOsite BETWEEN 'C770' and 'C779') )
       and (MORPH BETWEEN '9650' and '9667')
       then 'NHL - Extranodal'

  %if dxdate<='31DEC2009'd %then %do;
    when ( (ICDOsite in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C422')) or (ICDOsite BETWEEN 'C770' and 'C779') )
     and (    ( MORPH BETWEEN '9590' and '9595' ) or ( MORPH BETWEEN '9670' and '9677' )
           or ( MORPH BETWEEN '9680' and '9688' ) or ( MORPH BETWEEN '9690' and '9698' )
       or ( MORPH BETWEEN '9700' and '9717' )
       or   MORPH in ( '9823' '9827' ) )
       then 'NHL - Nodal'

    when ( (ICDOsite NOT in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C422')) and NOT(ICDOsite BETWEEN 'C770' and 'C779') )
     and (    ( MORPH BETWEEN '9590' and '9595' ) or ( MORPH BETWEEN '9670' and '9677' )
           or ( MORPH BETWEEN '9680' and '9688' ) or ( MORPH BETWEEN '9690' and '9698' )
       or ( MORPH BETWEEN '9700' and '9717' ) )
       then 'NHL - Extranodal'

    when ( (ICDOsite NOT in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C420' 'C421' 'C422')) and NOT(ICDOsite BETWEEN 'C770' and 'C779') )
     and (  MORPH in ('9823' '9827') )
       then 'NHL - Extranodal'

   when ( MORPH in ('9731' '9732') )
   then 'Myeloma'

   when ( MORPH in ('9821' '9828' '9836' '9837') )
   then 'Acute Lymphocytic Leukemia'

    when ( ICDOsite in ('C420' 'C421' 'C424') )
     and (  MORPH in ('9823') )
       then 'Chronic Lymphocytic Leukemia'

   when ( MORPH in ('9820' '9822' '9824' '9825' '9826') )
   then 'Other Lymphocytic Leukemia'

   when ( MORPH in ('9840' '9861' '9866' '9867' '9871' '9872' '9873' '9874') )
   then 'Acute Myeloid Leukemia'

   when ( MORPH in ('9891') )
   then 'Acute Monocytic Leukemia'

   when ( MORPH in ('9893') )
   then 'Chronic Monocytic Leukemia'

   when ( MORPH in ('9863' '9868') )
   then 'Chronic Myeloid Leukemia'

   when ( MORPH in ('9860' '9892' '9894') )
   then 'Other Monocytic Leukemia'

   when ( MORPH in ('9801' '9841' '9931' '9932') )
   then 'Other Acute Leukemia'

   when ( MORPH in ('9803' '9842') )
   then 'Other Chronic Leukemia'

   when ( MORPH in ('9800' '9802' '9804' '9830' '9850' '9870' '9880' '9900' '9910' '9930' '9940' '9941') )
   then 'Aleukemic, subleukemic and NOS'

   when ICDOsite in ('C420' 'C421' 'C424')
   and ( MORPH in ('9827') )
   then 'Aleukemic, subleukemic and NOS'

   when ( MORPH BETWEEN '9050' and '9055' )
   then 'Mesothelioma'

   when ( MORPH ='9140' )
   then 'Kaposi Sarcoma'

    when ( MORPH in ('9720' '9721' '9722' '9723' '9740' '9741' '9950' '9970' '9989')
       or (MORPH BETWEEN '9980' and '9984') or (MORPH BETWEEN '9760' and '9764') or (MORPH BETWEEN '9960' and '9962') )
    then 'Miscellaneous'

    when ( (MORPH='C809') or (ICDOsite BETWEEN 'C420' and 'C424') or (ICDOsite BETWEEN 'C760' and 'C768') or (ICDOsite BETWEEN 'C770' and 'C779') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Miscellaneous'
  %end;
  %if dxdate>='01JAN2010'd %then %do;
   when ( (ICDOsite in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C422')) or (ICDOsite BETWEEN 'C770' and 'C779') )
     and (    ( MORPH BETWEEN '9590' and '9596' ) or ( MORPH BETWEEN '9689' and '9691' )
           or ( MORPH BETWEEN '9698' and '9702' ) or ( MORPH BETWEEN '9714' and '9719' )
       or ( MORPH BETWEEN '9727' and '9729' )
       or   MORPH in ( '9670' '9671' '9673' '9675' '9678' '9679' '9680' '9684' '9687' '9695'
                       '9705' '9708' '9709' '9823' '9827' ) )
       then 'NHL - Nodal'

    when ( (ICDOsite NOT in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C422')) and NOT(ICDOsite BETWEEN 'C770' and 'C779') )
     and (    ( MORPH BETWEEN '9590' and '9596' ) or ( MORPH BETWEEN '9689' and '9691' )
           or ( MORPH BETWEEN '9698' and '9702' ) or ( MORPH BETWEEN '9714' and '9719' )
       or ( MORPH BETWEEN '9727' and '9729' )
       or   MORPH in ( '9670' '9671' '9673' '9675' '9678' '9679' '9680' '9684' '9687' '9695'
                       '9705' '9708' '9709' '9823' '9827' ) )
       then 'NHL - Extranodal'

    when ( (ICDOsite NOT in ('C024' 'C098' 'C099' 'C111' 'C142' 'C379' 'C420' 'C421' 'C422')) and NOT(ICDOsite BETWEEN 'C770' and 'C779') )
     and (  MORPH in ('9823' '9827') )
       then 'NHL - Extranodal'

   when ( MORPH in ('9731' '9732' '9734') )
   then 'Myeloma'

   when ( MORPH in ('9826' '9835' '9836' '9837') )
   then 'Acute Lymphocytic Leukemia'

    when ( ICDOsite in ('C420' 'C421' 'C424') )
     and (  MORPH in ('9823') )
       then 'Chronic Lymphocytic Leukemia'

   when ( MORPH in ('9820' '9832' '9833' '9834' '9940') )
   then 'Other Lymphocytic Leukemia'

   when ( MORPH in ('9840' '9861' '9866' '9867' '9871' '9872' '9873' '9874' '9895' '9896' '9897' '9910' '9920') )
   then 'Acute Myeloid Leukemia'

   when ( MORPH in ('9891') )
   then 'Acute Monocytic Leukemia'

   when ( MORPH in ('9863' '9875' '9876' '9945' '9946') )
   then 'Chronic Myeloid Leukemia'

   when ( MORPH in ('9860' '9930') )
   then 'Other Myeloid/Monocytic Leukemia'

   when ( MORPH in ('9801' '9805' '9931') )
   then 'Other Acute Leukemia'

   when ( MORPH in ('9733' '9742' '9800' '9831' '9870' '9948' '9963' '9964') )
   then 'Aleukemic, subleukemic and NOS'

   when ICDOsite in ('C420' 'C421' 'C424')
   and ( MORPH in ('9827') )
   then 'Aleukemic, subleukemic and NOS'

   when ( MORPH BETWEEN '9050' and '9055' )
   then 'Mesothelioma'

   when ( MORPH ='9140' )
   then 'Kaposi Sarcoma'

    when ( MORPH in ('9740' '9741' '9950' '9960' '9961' '9962' '9970' '9975' '9980' '9989')
       or (MORPH BETWEEN '9750' and '9758') or (MORPH BETWEEN '9760' and '9769') or (MORPH BETWEEN '9982' and '9987') )
    then 'Miscellaneous'

    when ( (MORPH='C809') or (ICDOsite BETWEEN 'C420' and 'C429') or (ICDOsite BETWEEN 'C760' and 'C768') or (ICDOsite BETWEEN 'C770' and 'C779') )
       and NOT( MORPH BETWEEN '9050' and '9055' ) and NOT( MORPH = '9140' ) and NOT( MORPH BETWEEN '9590' and '9989' )
       then 'Miscellaneous'

  %end;
  else ''
  end as CancerSchemaAjcc6thEdition

  from &_vdw_tumor as t
  where dxdate between "&startdt."d and "&enddt."d
  ;
QUIT;

%mend CancerSchema;


******************************* CESR_SYMBOLCHECK ***********************************
******************************* CESR_SYMBOLCHECK ***********************************

CESR_SYMBOLCHECK - This macro checks that the macro variables CONTENT_AREA, ERA, and
VERSION have been assigned correctly.

If any of the variables do not exist or is not
assigned a string in the correct format, the macro will stop program execution (but
not end an interactive SAS session). The macro also generates error messages
instructing the user to create appropriate macro variable assignments.

If all variables are correctly assigned, no message will be produced, and the program
will continue to execute normally.

No data set is created.
No report is created.

Contact Information: CESR VDW Consultants
                     CESR Data Coordinating Center
                     Center for Health Research
                     Kaiser Permanente Northwest

                     Gwyn.Saylor@kpchr.org      503-335-2447
           Jenny.Staab@kpchr.org    503-335-6683
                     Don.Bachman@kpchr.org      503-335-6731
                     Karen.Riedlinger@kpchr.org 503-335-2464

Authors:    CESR VDW Consultants and CESR Analysts
Published:  08/2012
******************************* CESR_SYMBOLCHECK ***********************************
******************************* CESR_SYMBOLCHECK ***********************************;

%macro CESR_SYMBOLCHECK;

  %local CONTENT_AREA_EXIST CONTENT_AREA_CORRECT ERA_EXIST ERA_CORRECT VERSION_EXIST VERSION_CORRECT STOPEXECUTION;

  %let CONTENT_AREA_EXIST=0;
  %let CONTENT_AREA_CORRECT=0;
  %let ERA_EXIST=0;
  %let ERA_CORRECT=0;
  %let VERSION_EXIST=0;
  %let VERSION_CORRECT=0;
  %let STOPEXECUTION=0;

  data _null_;
    set sashelp.vmacro (where=(scope="GLOBAL")) ;
    if upcase(name)="CONTENT_AREA" then do;
      call symput ('CONTENT_AREA_EXIST','1');
      if anyfirst(value)=1 and length(value) le 8 then call symput ('CONTENT_AREA_CORRECT','1');
    end;
    if upcase(name)="ERA" then do;
      call symput ('ERA_EXIST','1');
      if notdigit(TRIM(value))=0 and length(value)=6 then call symput ('ERA_CORRECT','1');
    end;
    if upcase(name)="VERSION" then do;
      call symput ('VERSION_EXIST','1');
      if 1 le lengthn(value) le 4 then call symput ('VERSION_CORRECT','1');
    end;
  run;

  %if not &CONTENT_AREA_EXIST %then %do;
    %put ERROR: The macro variable CONTENT_AREA does not exist.;
    %put ERROR: Please assign CONTENT_AREA a string of 1 to 8 characters starting with a letter or _.;
    %let STOPEXECUTION=1;
  %end;
  %else %if not &CONTENT_AREA_CORRECT %then %do;
    %put ERROR: The macro variable CONTENT_AREA has not been assigned correctly.;
    %put ERROR: Please assign CONTENT_AREA a string of up to 8 characters starting with a letter or _.;
    %let STOPEXECUTION=1;
  %end;
  %if not &ERA_EXIST %then %do;
    %put ERROR: The macro variable ERA does not exist.;
    %put ERROR: Please assign ERA a string of exactly 6 digits (YYYYMM).;
    %let STOPEXECUTION=1;
  %end;
  %else %if not &ERA_CORRECT %then %do;
    %put ERROR: The macro variable ERA was not assigned correctly.;
    %put ERROR: Please assign ERA a string of exactly 6 digits (YYYYMM).;
    %let STOPEXECUTION=1;
  %end;
  %if not &VERSION_EXIST %then %do;
    %put ERROR: The macro variable VERSION does not exist.;
    %put ERROR: Please assign VERSION a string of 1 to 4 characters.;
    %let STOPEXECUTION=1;
  %end;
  %else %if not &VERSION_CORRECT %then %do;
    %put ERROR: The macro variable VERSION has not been assigned correctly.;
    %put ERROR: Please assign VERSION a string of 1 to 4 characters.;
    %let STOPEXECUTION=1;
  %end;

  %if &STOPEXECUTION %then %do;
    %put ERROR: The program will stop executing and all submitted statements be canceled.;
    %abort cancel;
  %end;


%mend CESR_SYMBOLCHECK;


******************************* CESR_AppendDS ***********************************
******************************* CESR_AppendDS ***********************************

This macro creates the DCO_file that is called in each of the TLC/VLC
  macros.

Note that the macro variable ERA must have been assigned a 6-digit string for the
  macro to execute and processing to continue.

Creates 1 dataset (DCO_file)with the following variables:
  Site $4. MemName $32. VarName $32.
  Check_description $256. Result $8. Reason $256. DateRan 8. Era 4. Version $4.
  Content_Area $8. Check $120. OutDataSet $ 41

Contact Information: CESR VDW Consultants
                     CESR Data Coordinating Center
                     Center for Health Research
                     Kaiser Permanente Northwest

                     Gwyn.Saylor@kpchr.org      503-335-2447
           Jenny.Staab@kpchr.org    503-335-6683
                     Don.Bachman@kpchr.org      503-335-6731
                     Karen.Riedlinger@kpchr.org 503-335-2464

Authors:    CESR VDW Consultants and CESR Analysts
Published:  11/2011
Revised:  08/2012

******************************* CESR_AppendDS ***********************************
******************************* CESR_AppendDS ***********************************;
%macro CESR_AppendDS(indataset=);

*verify that ERA has been assigned correctly;
*if not, print error message and stop program execution;
  %local era_exist era_correct;
  %let ERA_EXIST=0;
  %let ERA_CORRECT=0;
  data _null_;
    set sashelp.vmacro (where=(scope="GLOBAL")) ;
    if upcase(name)="ERA" then do;
      call symput ('ERA_EXIST','1');
      if notdigit(TRIM(value))=0 and length(value)=6 then call symput ('ERA_CORRECT','1');
    end;
  run;

  %if not &ERA_EXIST %then %do;
    %put ERROR: The macro variable ERA does not exist.;
    %put ERROR: Please assign ERA a string of exactly 6 digits (YYYYMM).;
    %put ERROR: The program will stop executing and all submitted statements be canceled.;
    %abort cancel;
  %end;
  %else %if not &ERA_CORRECT %then %do;
    %put ERROR: The macro variable ERA was not assigned correctly.;
    %put ERROR: Please assign ERA a string of exactly 6 digits (YYYYMM).;
    %put ERROR: The program will stop executing and all submitted statements be canceled.;
    %abort cancel;
  %end;

*Checks to see if the DCO_file has been created
  before setting the indataset to it;
  %if %sysfunc(exist(&QAoutlib..DCO_file)) %then %do;
  data &QAoutlib..dco_file;
    set &QAoutlib..DCO_file &indataset (in=newfile);
    check=lowcase(check);
    outdataset=lowcase(outdataset);
    if newfile then Era=&era.;
  run;
  %end;
  %else %do;
    data &QAoutlib..DCO_file;
    length Site $4. MemName $32. VarName $32.
      Check_description $256. Result $8. Reason $256. DateRan 8. Era 4. Version $4. Content_Area $8. Check $120. OutDataSet $ 41;
    set &indataset;
    check=lowcase(check);
    Era=&era.;
    outdataset=lowcase(outdataset);
          label site = "Site" memname = "Table Name" varname = "Variable Name"
      check = "Check Name" Result = "Result" Reason = "Reason" content_area = "Content Area Being QAed"
            dateran = "Date/Time Program Ran" version = "VDW Version" Check_Description = "Check Performed"
            OutDataSet = 'Data Set Containing Check Specifics' Era="Era of QA Program";
    format dateran datetime20.;
  run;
  %end;

%mend CESR_AppendDS;



****************************** CESR_VLC_XREF ***************************
****************************** CESR_VLC_XREF ***************************
CESR_VLC_XREF:

This macro performs a data check comparing the specified variables actual type and/or length against a reference variable's
type and/or length.


This macro:
1. Prints 1 output report per type and length check
       Columns in Report are:   Site, Content Area Being QAed, Table Name, Variable Name, Reference Dataset,
                Reference Variable, Check Type (Type vs Length), Found, Reference, Result

2. Appends 1 row explaining the result for the requested variable and data set to the DCO_file.
       Columns in DCO_file are: site, data set name, variable name, check performed, result, reason,
            date/time program ran, VDW version, content area being QAedtype, name of the check,
            output data set containing check specifics.

3. Creates 1 dataset named by the user
       The value of &QAoutlib is established by the user in the edit section of the calling program.
       Columns in table are:  Site, Content Area Being QAed, Table Name, Variable Name, Reference Dataset,
                Reference Variable, Check Type (Type vs Length), Found, Reference, Result, Reason


Title lines 1 and 2 along with Footnote line 1 are reserved for global use by the wrapper QA program.
Title lines 3 and 4 as well as Footnote lines 2 and 3 can be used by the programmer.
Title lines 5 and higher and Footnote lines 4 and higher are reserved for use by the macros and will be cleared
    as each macro finishes executing.

To invoke the macro you must:

    a) supply the names of table/dataset to check
  b) supply the name of the target variable to check
  c) supply the name of the reference variable to which the target variable will be compared
  d) supply the name of the reference variable (if the name if different from the target variable-
    if not supplied, refvariable will be set to the same values as variable)
  e) indicate whether the variable type is to be checked
  f) indicate whether variable length is to be checked
  g) supply then name of the output dataset

Example Calls

%CESR_VLC_XREF(indataset=&_vdw_px, variable=performing_provider, refdataset=&_vdw_provider_specialty, refvariable=provider,
        checktype = Y, checklength = Y, outdataset = out.provider_xref)
%CESR_VLC_XREF(indataset=&_vdw_dx, variable=mrn, refdataset=&_vdw_demographic, checktype = N, checklength = Y, outdataset = out.mrn_xref)


Contact Information: CESR VDW Consultants
                     CESR Data Coordinating Center
                     Center for Health Research
                     Kaiser Permanente Northwest

           Jenny Staab        503-335-6683
                     Gwyn.Saylor@kpchr.org      503-335-2447
                     Don.Bachman@kpchr.org      503-335-6731

Authors:    CESR VDW Consultants and CESR Analysts
Published:  06/2014
****************************** CESR_VLC_XREF ***************************
****************************** CESR_VLC_XREF ***************************;


%macro CESR_VLC_XREF(indataset=, variable=, refdataset=, refvariable=, checktype = Y, checklength = Y, outdataset = );

  %CESR_SymbolCheck;

  %local inname refname ;

  *establish macro variable values;
  %let inname=%upcase(%substr(&indataset,%eval(%index(&indataset,.)+1)));
  %let refname=%upcase(%substr(&refdataset,%eval(%index(&refdataset,.)+1)));
  %let variable=%upcase(&variable);
  %if &refvariable = %then %let refvariable=&variable;
  %let refvariable=%upcase(&refvariable);

  PROC FORMAT;
    value typ 1='numeric' 2='character' .='not in dataset';
    value leng .='not in dataset';
  RUN;

  proc contents data=&refdataset. out=ref
    (keep=name type length where=(upcase(name)="&refvariable.") rename=(type=reftype length=reflength)) noprint
    ;
  run;
  data ref; set ref; name='0'; run;
  proc contents data=&indataset. out=check
    (keep=name type length where=(upcase(name)="&variable.")) noprint
    ;
  run;
  data check; set check; name='0'; run;

  data
    dco_feed (keep=Site  MemName VarName Result Reason Check_description DateRan Version Content_Area Check OutDataSet)
    &outdataset (keep=Site Content_Area MemName VarName RefDataSet RefVar CheckType Result Reason CheckValue RefValue
    label = "Type/length of variable &variable compared to type/length of variable &refvariable. in &refname."
  )
    ;
    Length Site $4. MemName VarName RefDataSet RefVar $32. CheckType $6. CheckValue RefValue 8
         Check_description $256. Result $8. Reason $256. DateRan 8. Version $4. Content_Area $8. Check $120. OutDataSet $ 41;
    label site = "Site" memname = "Table Name" varname = "Variable Name"
          check = "Check Name" Result = "Result" Reason = "Reason" content_area = "Content Area Being QAed"
          dateran = "Date/Time Program Ran" version = "VDW Version" Check_Description = "Check Performed"
          OutDataSet = 'Data Set Containing Check Specifics'
      RefDataset = "Reference Dataset" RefVar ="Reference Variable"
      RefValue = "Reference"
      CheckValue = "Found";

    merge check ref;
    by name;

      memname = "&inname";
      varname = "&variable";
      refdataset="&refname.";
      refvar="&refvariable.";
      site="&_siteabbr";
      content_area="&content_area";
      dateran=datetime();
      version="&version";
      outdataset="&outdataset.";
      format dateran datetime.;


    %if &checktype. = Y %then %do;
      CHECK=compress("&CONTENT_area._"||varname||'_'||"type"||'_'||"Descriptive",' ');
      CHECK_DESCRIPTION="Compares type of variable to type of reference variable.";
      CHECKTYPE="TYPE";
      CHECKVALUE=TYPE;
      REFVALUE=REFTYPE;
      if type=reftype then do;
        RESULT="PASS";
        IF refTYPE=1 THEN REASON="Reference variable and check variable are numeric.";
        ELSE REASON="Reference variable and check variable are character.";
      end;
      else do;
        RESULT="FAIL";
        if missing(type) or missing(reftype) then reason="Check and/or reference variable is not in the dataset.";
        else IF refTYPE=1 THEN REASON="Reference variable is numeric, but check variable is character.";
        ELSE REASON="Reference variable is character, but check variable is numeric.";
      end;
      OUTPUT;
    %end;
    %if &checklength. = Y %then %do;
      CHECK=compress("&CONTENT_area._"||varname||'_'||"length"||'_'||"Descriptive",' ');
      CHECK_DESCRIPTION="Compares length of variable to length of reference variable.";
      CHECKTYPE="LENGTH";
      CHECKVALUE=LENGTH;
      REFVALUE=REFLENGTH;
      if length=reflength then do;
        RESULT="PASS";
        REASON=compress(put(length,best12.)||"="||put(reflength,best12.))||" is true.";
      end;
      else do;
        RESULT="FAIL";
        if missing(length) or missing(reflength) then reason="Check and/or reference variable is not in the dataset.";
        else REASON=compress(put(length,best12.)||"="||put(reflength,best12.))||" is false.";
      end;
      OUTPUT;
    %end;
  RUN;

  %CESR_APPENDDS (indataset=dco_feed);

  %if &checktype. = Y %then %do;
  ods proclabel="Type of variable &variable compared to type of variable &refvariable. in &refname.";
  title5 "Check: Comparison of Variable Type Against Reference Variable Type";
  title6 "&variable in &inname vs. &refvariable in &refname..";
  proc print data=&outdataset.   noobs label;
  where checktype="TYPE";
    var Site Content_Area MemName VarName  RefDataSet RefVar  CheckType CheckValue RefValue Result ;
    format CheckValue RefValue typ.;
  run;
  %end;
  %if &checklength. = Y %then %do;
  ods proclabel="Length of variable &variable compared to length of variable &refvariable. in &refname.";
  title5 "Check: Comparison of Variable Length Against Reference Variable Length";
  title6 "&variable in &inname vs. &refvariable in &refname..";
  proc print data=&outdataset.   noobs label;
  where checktype="LENGTH";
    var Site Content_Area MemName VarName  RefDataSet RefVar  CheckType CheckValue RefValue Result ;
    format CheckValue RefValue leng.;
  run;
  %end;

  proc datasets lib=work nolist;
    delete dco_feed check ref; run;
  quit;

%mend CESR_VLC_XREF;

****************************** CESR_VLC_XREF ***************************
****************************** CESR_VLC_XREF ***************************;



****************************** CESR_VLC_Linkage **********************************
****************************** CESR_VLC_Linkage **********************************

CESR_VLC_Linkage:

This Macro produces counts of (distinct or raw) values of a specified linking variable that can be
linked to a values of a target variable (in the same or a different dataset).
Percentages are calculated and compared to the fail and warn cutoff points that the user
supplies.  Then values less than &lowest_count are set to 0 and percentages are recalculated.

This macro:
1. Prints 1 output report for the user to review.
       Report contains
         -  one row for mislinked and one row for unlinked counts and percentages, unless the count in the
      dataset is 0.
       Columns in Report are: data set name, variable name, frequency and percent with counts
       less than &lowest_count hidden and percents adjusted accordingly.

2. Appends 1 row explaining the result for the requested variable and data set to the DCO_file.
       Columns in DCO_file are: site, data set name, variable name, check performed, result, reason,
            date/time program ran, VDW version, content area being QAedtype, name of the check,
            output data set containing check specifics.

3. Creates 1 dataset that is either named by the user with the optional parameter &outdataset or called
       &QAoutlib..&content_area._&linkingvar._&linkname._link where &content_area._&linkingvar._&linkname._link is truncated at 32 characters.
       &linkname is either named by the user or called to_&targetfile, where &targetfile is the name of the dataset to which the
     linking variable (&linkingvar) is linked without the libname in front.
       The value of &QAoutlib is established by the user in the edit section of the calling program.
       &indataset and &outdataset receive their values from the user as parameter values when this macro is invoked.
       Columns in table are: site memname varnmae status count percent warn_at fail_at result

4. Can create 1 dataset that is named &unlinkeddataset, if the parameter unlinkeddataset is set during the macro call. If no name is supplied,
     the dataset will not be created.

Title lines 1 and 2 along with Footnote line 1 are reserved for global use by the wrapper QA program.
Title lines 3 and 4 as well as Footnote lines 2 and 3 can be used by the programmer.
Title lines 5 and higher and Footnote lines 4 and higher are reserved for use by the macros and will be cleared
    as each macro finishes executing.

To invoke the macro you must:

  a) supply the name of a SAS table to check.  The table can be permanent with two level name or temporary with single
       level name.
  b) supply the name of the linking variable (aka foreign key) whose presence in the target data set/variable is to be
    checked
  c) supply the name of the dataset to which the linking variable is to be linked
  d) if different from the linking variable, supply the name of the variable to which the linking variable is to be linked
  e) optionally, supply the fail cutoff point (% of unlinked values).  For example, enter 3 if the fail point is >3%.  Enter =3 if the fail point is >=3%.
  f) optionally, supply the warn cutoff point (% of unlinked values).  For example, enter 3 if the fail point is >3%.  Enter =3 if the fail point is >=3%.
  g) indicate whether missing values are to be counted (Y/N). The default is N.
  h) supply the name of the dataset to contain the check results
  i) optionally, supply the name of the dataset that will contain all unlinked values of the linking variable
  j) optionally, supply a phrase to be inserted into the check name (and the output dataset name if &OutDataSet is not specified)
  k) indicate whether results are to be computed for distinct values of the linking variable (i.e. regardless how often a value occurs) or for every record.
     The default is Y (distinct values).

Note that if neither fail nor warn cutoff points are set, the macro will produce a descriptive report without pass/warn/fail message.
Results will be written to the DCO file with result="N/A"

Macro Call:
%CESR_VLC_Linkage (InDataSet =, LinkingVar =, TargetDataSet =, TargetVar =, FailPercent =, WarnPercent =, Missing = N, OutDataSet =, UnlinkedDataSet =,LinkName =, Distinct = Y)

Example Calls:
%CESR_VLC_Linkage ( InDataSet = &_vdw_px , LinkingVar = performingprovider,
          TargetDataSet =&_vdw_provider_specialty, TargetVar =provider,
          FailPercent =5, WarnPercent =0,
          Missing = N,
          OutDataSet =px_performingprov_to_prov,
          LinkName = to_prov,
          Distinct = Y)
%CESR_VLC_Linkage ( InDataSet = &_vdw_dx , LinkingVar = provider,
          TargetDataSet =&_vdw_provider_specialty,
          WarnPercent =0,
          Missing = N,
          OutDataSet =dx_prov_to_prov,
          LinkName = to_prov,
          Distinct = N)
%CESR_VLC_Linkage ( InDataSet = &_vdw_everndc , LinkingVar = ndc,
          TargetDataSet =&_vdw_rx,
          Missing = N,
          OutDataSet =everndc_ndc_link_to_rx,
          LinkName = to_rx,
          Distinct = Y)

Contact Information: CESR VDW Consultants
                     CESR Data Coordinating Center
                     Center for Health Research
                     Kaiser Permanente Northwest

           Jenny.Staab@kpchr.org    503-335-6683
                     Gwyn.Saylor@kpchr.org      503-335-2447
                     Don.Bachman@kpchr.org      503-335-6731
                     Karen.Riedlinger@kpchr.org 503-335-2464

Authors:    CESR VDW Consultants and CESR Analysts
Published:  02/2013
;


****************************** CESR_VLC_Linkage **********************************
****************************** CESR_VLC_Linkage **********************************;
%macro CESR_VLC_Linkage (
            InDataSet       = ,    /* Dataset with Linking Variable*/
            LinkingVar      = ,    /* Variable whose linkage to another dataset/variable is to be checked*/
            TargetDataSet   = ,    /* Dataset in which linking variable is looked up*/
            TargetVar       = ,    /* optional, will be set to Linkingvar if not assigned*/
            WarnPercent     = ,    /* Enter 1 if the warn point is >1%. */
            FailPercent     = ,    /* Enter 3 if the fail point is >3%. */
            Missing         = N ,  /* Include missing values of the linking variable*/
            OutDataSet      = ,    /* Name of dataset with check results*/
            UnlinkedDataSet = ,    /* If desired, name of dataset containing unlinked values of the linking variable*/
            LinkName        = ,    /* optional, part of check name expressing what the linking variable is linked to, e.g. to_demog*/
            Distinct        = Y    /* Y/N if status is computed for distinct values of LinkingVar vs all values/rows in dataset */
            );

  %CESR_SymbolCheck;
  %local infile targetfile title_7 title9 nolinkingvar notargetvar notargetdataset save_unlinked nlimits highpercent;

  %let linkingvar = %upcase (&linkingvar.);
  %if &targetvar  = %then %let targetvar=&linkingvar ; *set targetvar to linkingvar if no targetvar given;

  %let targetvar     = %upcase(&targetvar.);
  %let targetdataset = %upcase(&targetdataset);
  %let indataset     = %upcase(&indataset);
  %let targetfile    = %substr(&targetdataset, %eval(%index(&targetdataset., . )+1) ); *make short name for display purposes;
  %let infile        = %substr(&indataset    , %eval(%index(&indataset.    , . )+1) ); *make short name for display purposes;

  %if &linkname = %then %let linkname = TO_&TARGETFILE ; *if no linkname given, create default;
  %if &outdataset= %then %let outdataset=&QAoutlib..%substr(&content_area._&linkingvar._&linkname._link                        x,1,32); *if no outdataset name given, craete default;
  %if &UnlinkedDataSet = %then %let save_unlinked=N;
  %else %let save_unlinked=Y;

  *assess which limits were set and set parameters accordingly;
  %let nlimits = 0;
  %if &failpercent ne %then %do;
    %let nlimits=%eval(&nlimits.+1);
    %let highpercent=&failpercent;
  %end;
  %if &warnpercent ne %then %do;
    %let nlimits=%eval(&nlimits.+1);
    %let highpercent=&warnpercent;
  %end;

  *set program statements as a function of user input;;
  %if &distinct.=Y %then %let distinct=Distinct%str( );
  %else %if &distinct.=N %then %let distinct=;
  %if &missing.=N %then %do;
    %let mis = ;
    %let andmis = and a.&linkingvar. is not null;
  %end;
  %else %if &missing.=Y %then %do;
    %let mis= + nmiss (&distinct. a.&linkingvar.);
    %let andmis = ;
  %end;

  *set parameters that keep track of whether all required tables and variables exist;
  %let nolinkingvar     = 1;
  %let notargetdataset  = 1;
  %let notargetvar      = 1;

  *check whether everything needed exists;
  proc contents data=&indataset. out=invars (keep=name) noprint;
  run;
  data _null_;
    set invars;
    if upcase(name)="&linkingvar." then call symput('nolinkingvar','0');
  run;
  proc delete data=invars; run;
  %if %sysfunc(exist(&targetdataset., data)) or %sysfunc(exist(&targetdataset., view)) %then %do;
    %let notargetdataset=0;
    proc contents data=&targetdataset. out=targetvars (keep=name) noprint;
    run;
    data _null_;
      set targetvars;
      if upcase(name)="&targetvar." then call symput('notargetvar','0');
    run;
    proc delete data=targetvars; run;
  %end;

  *if a piece is missing, assign fail message;
  %if &nolinkingvar or &notargetvar or &notargetdataset %then %do;
    data dco_feed;
      length Site $4. MemName $32. VarName $32.
               Check_description $256. Result $8. Reason $256. DateRan 8. Version $4. Content_Area $8. Check $120. OutDataSet $ 41;
      label
        site              = "Site"
        memname           = "Table Name"
        varname           = "Variable Name"
        check             = "Check Name"
        Result            = "Result"
        Reason            = "Reason"
        content_area      = "Content Area Being QAed"
        dateran           = "Date/Time Program Ran"
        version           = "VDW Version"
        Check_Description = "Check Performed"
        OutDataSet        = 'Data Set Containing Check Specifics'
      ;
      site               = "&_siteabbr.";
      memname            = "&infile";
      varname            = "&linkingvar";
      Version            = "&version";
      content_area       = "&content_area";
      check              = "&content_area._&targetvar._&linkname._descriptive";
      Check_description  = "Percent of "||lowcase("&distinct")||"values of &linkingvar. that can be linked to &targetfile..";
      outdataset         = "&outdataset.";
      DateRan            = datetime();
      Reason=
          %if &nolinkingvar %then %do;
              "Variable &linkingvar does not exist in &indataset.";
          %end;
          %else %if &notargetdataset %then %do;
            "&targetdataset. does not exist";
          %end;
          %else %do;
            "Variable &targetvar does not exist in &targetdataset.";
          %end;

      Result=
        %if &failpercent ne %then %do;
            "FAIL";
        %end;
        %else %if &warnpercent ne %then %do;
          "WARN";
        %end;
        %else %do;
          "N/A";
        %end;
      call symput ('title_7',
          %if &nlimits. > 0 %then %do;
            trim(result)
          %end;
          %else %do;
            'Attention'
          %end;
            ||': '||trim(reason));
      format dateran datetime.;
    run;
    %CESR_AppendDS(indataset=dco_feed);
    data &outdataset. (keep=site  memname  varname  status count  percent  warn_at fail_at result);
      length Site $4 MemName $32 VarName $32 status $10 count 8 percent 8 warn_at $17 fail_at $17 result $8;
      Site      = "&_siteabbr.";
      memname   = "&infile.";
      Varname   = "&linkingvar.";
      call missing(status,count,percent);
      Reason=
          %if &nolinkingvar %then %do;
              "Variable &linkingvar does not exist in &indataset.";
          %end;
          %else %if &notargetdataset %then %do;
            "&targetdataset. does not exist";
          %end;
          %else %do;
            "Variable &targetvar does not exist in &targetdataset.";
          %end;

      Result=
          %if &failpercent ne %then %do;
              "FAIL";
          %end;
          %else %if &warnpercent ne %then %do;
            "WARN";
          %end;
          %else %do;
            "N/A";
          %end;
      warn_at=
          %if &warnpercent ne %then %do;
            compress(">"||"&warnpercent.");
          %end;
          %else %do;
            "No Warn Limit Set";
          %end;
      fail_at=
          %if &failpercent ne %then %do;
            compress(">"||"&failpercent.");
          %end;
          %else %do;
            "No Fail Limit Set";
          %end;
      label
        site      = "Site"
        memname   = "Table Name"
        varname   = "Variable Name"
        Result    = "Result"
        Reason    = "Reason"
        warn_at   = "Warning issued if Linked Percent is"
        fail_at   = "Fail issued if Linked Percent is"
        percent   = "Percent"
        count     = "Count"
        status    = "Status"
        ;
    run;

    title5 "Check: Percent of &Distinct.Values of &linkingvar. in &infile. That Can Be Linked to &targetvar. in &targetfile..";
    title7 "&title_7";
    ods proclabel="Linkage to &targetvar. in &targetfile.";
    proc print data=&outdataset. noobs label;
        var site memname varname result;
    run;
    title5;

    *clean up;
    proc delete data=dco_feed; run;
  %end;
  %else %do;
    *if all tables and variables exist;
    proc format;
      value hide 0='Hidden';
    run;
    proc sql noprint;
      *count total number of (distinct) linkingvar values;
      create table all as
        select "&_siteabbr." as Site, count(&distinct. a.&linkingvar) &mis. as totalN
        from &indataset  a
        ;
    quit;
    %if &save_unlinked=Y %then %do; *if dataset of unlinked linkingvar values requested;
      proc sql;
        *create dataset with (distinct) unlinked linkingvar values;

        create table &unlinkeddataset
          (label="This file contains all &distinct.values of &linkingvar. from the &infile. file that cannot be linked to &targetvar. in the &targetfile. file.") as
          select &distinct. a.&linkingvar
          from &indataset a
          where a.&linkingvar not in (select distinct b.&targetvar from &targetdataset b)
          &andmis. ;
      quit;
      *get number of unlinked linkingvar values;
      proc contents data=&unlinkeddataset  noprint out=unlinked (keep=nobs);
      run;
      data unlinked;
        set unlinked;
        site="&_siteabbr.";
        rename nobs=unlinkedn;
      run;
    %end;
    %else %do; *if no dataset of unlinked linkingvar values requested;
      proc sql;
        *count number of unlinked linkingvar values;
        create table unlinked as
          select "&_siteabbr." as site, count( &distinct. a.&linkingvar) &mis. as unlinkedN
          from &indataset a
          where a.&linkingvar not in (select distinct b.&targetvar from &targetdataset b)
          &andmis.
          ;
      quit;
    %end;
    *combine information about total and unlinked number of records to compute percentages and result;
    data
      &outdataset
        (keep= site  memname  varname  status count  percent  warn_at fail_at result )
      dco_add
        (keep = Site  MemName  VarName Check_description  Result  Reason  DateRan  Version  Content_Area  Check  OutDataSet )
      ;

      length Site $4. MemName $32 VarName $32 status $10 count 8 percent 8 warn_at $17 fail_at $17 result $8
          Check_description $256 Reason $256 DateRan 8 Version $4 Content_Area $8 Check $120 OutDataSet $ 41;

      merge all unlinked ;
      by site;

      memname           = "&infile";
      varname           = "&linkingvar";
      Version           = "&version";
      content_area      = "&content_area";
      check             = "&content_area._&targetvar._&linkname._descriptive";
      Check_description = "Percent of "||lowcase("&Distinct.")||"values of &linkingvar. that can be linked to &targetfile..";
      outdataset        = "&outdataset.";
      DateRan           = datetime();

      warn_at=
        %if &warnpercent ne %then %do;
          compress(">"||"&warnpercent.");
        %end;
        %else %do;
          "No Warn Limit Set";
        %end;
      fail_at=
        %if &failpercent ne %then %do;
          compress(">"||"&failpercent.");
        %end;
        %else %do;
          "No Fail Limit Set";
        %end;


      LinkedN=totaln-unlinkedN;
      UnlinkedPercent=100*UnlinkedN/TotalN; *this is used for result determination;

      *adjust totalN if low counts will be hidden;
      if LinkedN < &lowest_count. then totaln=totaln-linkedn;
      if UnlinkedN < &lowest_count. then totaln=totaln-unlinkedn;

      *if there are unlinked values and the user requested a dataset with unlinked values, set title9 to refer user to dataset;
      if unlinkedN=0 or "&save_unlinked."="N" then call symput("title9","");
      else call symput("title9", 'Title9 "'||"Unlinked values of &linkingvar. can be found in the file &unlinkeddataset. ."||'"');

      *compute result and reason if warn and/or fail percent set;
    %if &nlimits > 0 %then %do;
      %if &failpercent. ne %then %do;
        if UnlinkedPercent > &failpercent. then do;
          result="FAIL";
          reason="Unlinked values of &linkingvar occur at too high a rate (>&failpercent.%).";
        end;
        %if &warnpercent. ne %then %do;
          else if UnlinkedPercent > &warnpercent. then do;
            result="WARNING";
            reason="Unlinked values of &linkingvar occur at a concerning rate (>&warnpercent.%, but not >&failpercent.%).";
          end;
        %end;
      %end;
      %else %if &warnpercent. ne %then %do;
        if UnlinkedPercent > &warnpercent. then do;
          result="WARNING";
          reason="Unlinked values of &linkingvar occur at a concerning rate (>&warnpercent.%).";
        end;
      %end;
        else  do;
          result="PASS";
          reason="Unlinked values of &linkingvar occur at an acceptable rate (not >&highpercent.%).";
        end;
      call symput ('title_7', trim(result)||': '||trim(reason));
    %end;
    %else %do;
      result="N/A";
      reason="Unlinked values of &linkingvar occur at rate of "||compress(put(unlinkedpercent,8.4))||"%.";
      call symput ('title_7', trim(reason));
    %end;


      *create line with Unlinked records;
      Status="Not Linked";
      Count=UnlinkedN;
      *Percent=100*count/totaln; *commented out 11/6/13;
      if count < &lowest_count. then do;
        count=0;
        Percent=0;
      end;
      else Percent=100*count/totaln; *added 11/6/13;

      if unlinkedN>0 then output &outdataset.; *output only if there were unlinked linkingvar values;
      output dco_add; *put all the required info in the dco_add file that will be appended to the DCO file;


      Status="Linked";
      count = LinkedN;
      *Percent=100*count/totaln; *commented out 11/6/13;
      if count < &lowest_count. then do;
        count=0;
        Percent=0;
      end;
      else Percent=100*count/totaln; *added 11/6/13;

      if unlinkedN>0 then call missing (warn_at, fail_at, result); *set these vars to missing if they were already outputted on the unlinked line;

      if LinkedN>0 then output &outdataset.; *output only if there were linked values;

      label
        site              = "Site"
        memname           = "Table Name"
        varname           = "Variable Name"
        check             = "Check Name"
        Result            = "Result"
        Reason            = "Reason"
        content_area      = "Content Area Being QAed"
        dateran           = "Date/Time Program Ran"
        version           = "VDW Version"
        Check_Description = "Check Performed"
        OutDataSet        = 'Data Set Containing Check Specifics'
        Count             = "Count - Counts Less Than &lowest_count. Are Hidden"
        warn_at           = "Warning Issued if Unlinked Percent is"
        fail_at           = "Fail Issued if Unlinked Percent is"
        percent           = "Percent"
        status            = "Status"
      ;
    run;
    %CESR_AppendDS (indataset=dco_add);

    *clean up;
    proc datasets library=work nolist;
      delete dco_add all unlinked; run;
    quit;

    title5 "Check: Percent of &Distinct.Values of &linkingvar. in &infile. That Can Be Linked to &targetvar. in &targetfile..";
    title7 "&title_7";
    &title9.;
    footnote4 "Hidden indicates a count less than &lowest_count..";
    footnote5 "FIRST comparisons are made to the fail and warn percentage cutoff values.";
    footnote6 "THEN counts less than &lowest_count are hidden and percentages are recalculated.";
    ods proclabel="Linkage to &targetvar. in &targetfile.";
    proc print data=&outdataset. label noobs;
    format count percent hide.;
    run;
    title5;
    footnote4;
  %end;
%mend CESR_VLC_Linkage;

%macro seer_site_recode(inds=, outds=);
  *******************************************************************************;
  * PROGRAM DETAILS                                                             *;
  *   Filename: SEER_SITE_RECODE_MACRO.sas                                      *;
  *   Purpose:  This macro recodes VDW.TUMOR-formatted records into SEER site   *;
  *             recode values, which define the major cancer site/histology     *;
  *             groups that are commonly used in reporting of cancer incidence  *;
  *             data. Additional information about the SEER site recode can be  *;
  *             found at seer.cancer.gov/siterecode. As of 08/18/2015, this     *;
  *             macro uses the Site Recode ICD-O-3/WHO 2008 definition, which   *;
  *             is defined here: seer.cancer.gov/siterecode/icdo3_dwhoheme.     *;
  *   Author:   Rebecca Ziebell (ziebell.r@ghc.org)                             *;
  *   Updated:  17 September 2015                                               *;
  *******************************************************************************;
  * UPDATE HISTORY                                                              *;
  * Date      Comments                                                          *;
  * =========================================================================== *;
  * 20JAN2015 Initial version finalized.                                        *;
  * 05MAR2015 Fixed default OUTDS option to write over &INDS (instead of        *;
  *           storing as a data set literally called "INDS").                   *;
  * 18AUG2015 Removed default OUTDS option to avoid potential issues with       *;
  *           attempting to write over VDW.TUMOR (if used as INDS). Updated     *;
  *           PROC FORMAT to accommodate multilabel functionality. Reorganized  *;
  *           DATA step to reduce length. Removed statement assigning format to *;
  *           SITE_RECODE variable to prevent missing-format warnings if OUTDS  *;
  *           is saved for later use (in the absence of PROC FORMAT code).      *;
  * 17SEP2015 Added single-level format (site_recode) along with multilevel     *;
  *           format (recode_mlf). Fixed recode section so the site_recode var  *;
  *           will never be blank, always 99999 if otherwise unknown.           *;
  *******************************************************************************;

  %if %length(&inds) = 0 %then
    %put ERROR: You must specify a value for INDS=.
  ;
    %else %if %length(&outds) = 0 %then
      %put ERROR: You must specify a value for OUTDS=.
    ;
    %else %if &inds = &outds %then
      %put ERROR: INDS= data set must be different from OUTDS= data set.
    ;
    %else %do;
      proc format;
        value site_recode
          20010 = 'Lip'
          20020 = 'Tongue'
          20030 = 'Salivary Gland'
          20040 = 'Floor of Mouth'
          20050 = 'Gum and Other Mouth'
          20060 = 'Nasopharynx'
          20070 = 'Tonsil'
          20080 = 'Oropharynx'
          20090 = 'Hypopharynx'
          20100 = 'Other Oral Cavity and Pharynx'
          21010 = 'Esophagus'
          21020 = 'Stomach'
          21030 = 'Small Intestine'
          21041 = 'Cecum'
          21042 = 'Appendix'
          21043 = 'Ascending Colon'
          21044 = 'Hepatic Flexure'
          21045 = 'Transverse Colon'
          21046 = 'Splenic Flexure'
          21047 = 'Descending Colon'
          21048 = 'Sigmoid Colon'
          21049 = 'Large Intestine, NOS'
          21051 = 'Rectosigmoid Junction'
          21052 = 'Rectum'
          21060 = 'Anus, Anal Canal and Anorectum'
          21071 = 'Liver'
          21072 = 'Intrahepatic Bile Duct'
          21080 = 'Gallbladder'
          21090 = 'Other Biliary'
          21100 = 'Pancreas'
          21110 = 'Retroperitoneum'
          21120 = 'Peritoneum, Omentum and Mesentery'
          21130 = 'Other Digestive Organs'
          22010 = 'Nose, Nasal Cavity and Middle Ear'
          22020 = 'Larynx'
          22030 = 'Lung and Bronchus'
          22050 = 'Pleura'
          22060 = 'Trachea, Mediastinum and Other Respiratory Organs'
          23000 = 'Bones and Joints'
          24000 = 'Soft Tissue including Heart'
          25010 = 'Melanoma of the Skin'
          25020 = 'Other Non-Epithelial Skin'
          26000 = 'Breast'
          27010 = 'Cervix Uteri'
          27020 = 'Corpus Uteri'
          27030 = 'Uterus, NOS'
          27040 = 'Ovary'
          27050 = 'Vagina'
          27060 = 'Vulva'
          27070 = 'Other Female Genital Organs'
          28010 = 'Prostate'
          28020 = 'Testis'
          28030 = 'Penis'
          28040 = 'Other Male Genital Organs'
          29010 = 'Urinary Bladder'
          29020 = 'Kidney and Renal Pelvis'
          29030 = 'Ureter'
          29040 = 'Other Urinary Organs'
          30000 = 'Eye and Orbit'
          31010 = 'Brain'
          31040 = 'Cranial Nerves Other Nervous System'
          32010 = 'Thyroid'
          32020 = 'Other Endocrine including Thymus'
          33011 = 'Hodgkin - Nodal'
          33012 = 'Hodgkin - Extranodal'
          33041 = 'NHL - Nodal'
          33042 = 'NHL - Extranodal'
          34000 = 'Myeloma'
          35011 = 'Acute Lymphocytic Leukemia'
          35012 = 'Chronic Lymphocytic Leukemia'
          35013 = 'Other Lymphocytic Leukemia'
          35021 = 'Acute Myeloid Leukemia'
          35031 = 'Acute Monocytic Leukemia'
          35022 = 'Chronic Myeloid Leukemia'
          35023 = 'Other Myeloid/Monocytic Leukemia'
          35041 = 'Other Acute Leukemia'
          35043 = 'Aleukemic, subleukemic and NOS'
          36010 = 'Mesothelioma'
          36020 = 'Kaposi Sarcoma'
          37000 = 'Miscellaneous'
          99999 = 'Invalid'
        ;
        value recode_mlf (multilabel notsorted)
          20010-20100 = 'Oral Cavity and Pharynx'
          20010       = '    Lip'
          20020       = '    Tongue'
          20030       = '    Salivary Gland'
          20040       = '    Floor of Mouth'
          20050       = '    Gum and Other Mouth'
          20060       = '    Nasopharynx'
          20070       = '    Tonsil'
          20080       = '    Oropharynx'
          20090       = '    Hypopharynx'
          20100       = '    Other Oral Cavity and Pharynx'
          21010-21130 = 'Digestive System'
          21010       = '    Esophagus'
          21020       = '    Stomach'
          21030       = '    Small Intestine'
          21041-21052 = '    Colon and Rectum'
          21041-21049 = '        Colon excluding Rectum'
          21041       = '            Cecum'
          21042       = '            Appendix'
          21043       = '            Ascending Colon'
          21044       = '            Hepatic Flexure'
          21045       = '            Transverse Colon'
          21046       = '            Splenic Flexure'
          21047       = '            Descending Colon'
          21048       = '            Sigmoid Colon'
          21049       = '            Large Intestine, NOS'
          21051-21052 = '        Rectum and Rectosigmoid Junction'
          21051       = '            Rectosigmoid Junction'
          21052       = '            Rectum'
          21060       = '    Anus, Anal Canal and Anorectum'
          21071-21072 = '    Liver and Intrahepatic Bile Duct'
          21071       = '        Liver'
          21072       = '        Intrahepatic Bile Duct'
          21080       = '    Gallbladder'
          21090       = '    Other Biliary'
          21100       = '    Pancreas'
          21110       = '    Retroperitoneum'
          21120       = '    Peritoneum, Omentum and Mesentery'
          21130       = '    Other Digestive Organs'
          22010-22060 = 'Respiratory System'
          22010       = '    Nose, Nasal Cavity and Middle Ear'
          22020       = '    Larynx'
          22030       = '    Lung and Bronchus'
          22050       = '    Pleura'
          22060       = '    Trachea, Mediastinum and Other Respiratory Organs'
          23000       = 'Bones and Joints'
          24000       = 'Soft Tissue including Heart'
          25010-25020 = 'Skin excluding Basal and Squamous'
          25010       = '    Melanoma of the Skin'
          25020       = '    Other Non-Epithelial Skin'
          26000       = 'Breast'
          27010-27070 = 'Female Genital System'
          27010       = '    Cervix Uteri'
          27020-27030 = '    Corpus and Uterus, NOS'
          27020       = '        Corpus Uteri'
          27030       = '        Uterus, NOS'
          27040       = '    Ovary'
          27050       = '    Vagina'
          27060       = '    Vulva'
          27070       = '    Other Female Genital Organs'
          28010-28040 = 'Male Genital System'
          28010       = '    Prostate'
          28020       = '    Testis'
          28030       = '    Penis'
          28040       = '    Other Male Genital Organs'
          29010-29040 = 'Urinary System'
          29010       = '    Urinary Bladder'
          29020       = '    Kidney and Renal Pelvis'
          29030       = '    Ureter'
          29040       = '    Other Urinary Organs'
          30000       = 'Eye and Orbit'
          31010-31040 = 'Brain and Other Nervous System'
          31010       = '    Brain'
          31040       = '    Cranial Nerves Other Nervous System'
          32010-32020 = 'Endocrine System'
          32010       = '    Thyroid'
          32020       = '    Other Endocrine including Thymus'
          33011-33042 = 'Lymphoma'
          33011-33012 = '    Hodgkin Lymphoma'
          33011       = '        Hodgkin ^{unicode 2013} Nodal'
          33012       = '        Hodgkin ^{unicode 2013} Extranodal'
          33041-33042 = '    Non-Hodgkin Lymphoma'
          33041       = '        NHL ^{unicode 2013} Nodal'
          33042       = '        NHL ^{unicode 2013} Extranodal'
          34000       = 'Myeloma'
          35011-35043 = 'Leukemia'
          35011-35013 = '    Lymphocytic Leukemia'
          35011       = '        Acute Lymphocytic Leukemia'
          35012       = '        Chronic Lymphocytic Leukemia'
          35013       = '        Other Lymphocytic Leukemia'
          35021-35031 = '    Myeloid and Monocytic Leukemia'
          35021       = '        Acute Myeloid Leukemia'
          35031       = '        Acute Monocytic Leukemia'
          35022       = '        Chronic Myeloid Leukemia'
          35023       = '        Other Myeloid/Monocytic Leukemia'
          35041-35043 = '    Other Leukemia'
          35041       = '        Other Acute Leukemia'
          35043       = '        Aleukemic, subleukemic and NOS'
          36010       = 'Mesothelioma'
          36020       = 'Kaposi Sarcoma'
          37000       = 'Miscellaneous'
          99999       = 'Invalid'
        ;
      run;

      data &outds;
        set &inds;
        site = input(substr(icdosite, 2), 3.);
        hist = input(morph, 4.);
        if site in (000:009)
          and hist not in (9050:9055, 9140, 9590:9992)
          then site_recode = 20010
        ;
          else if site in (019:029)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20020
          ;
          else if site in (079:089)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20030
          ;
          else if site in (040:049)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20040
          ;
          else if site in (030:039, 050:059, 060:069)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20050
          ;
          else if site in (110:119)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20060
          ;
          else if site in (090:099)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20070
          ;
          else if site in (100:109)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20080
          ;
          else if site in (129, 130:139)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20090
          ;
          else if site in (140, 142, 148)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 20100
          ;
          else if site in (150:159)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21010
          ;
          else if site in (160:169)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21020
          ;
          else if site in (170:179)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21030
          ;
          else if site in (180)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21041
          ;
          else if site in (181)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21042
          ;
          else if site in (182)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21043
          ;
          else if site in (183)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21044
          ;
          else if site in (184)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21045
          ;
          else if site in (185)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21046
          ;
          else if site in (186)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21047
          ;
          else if site in (187)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21048
          ;
          else if site in (188:189, 260)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21049
          ;
          else if site in (199)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21051
          ;
          else if site in (209)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21052
          ;
          else if site in (210:212, 218)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21060
          ;
          else if site in (220)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21071
          ;
          else if site in (221)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21072
          ;
          else if site in (239)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21080
          ;
          else if site in (240:249)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21090
          ;
          else if site in (250:259)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21100
          ;
          else if site in (480)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21110
          ;
          else if site in (481:482)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21120
          ;
          else if site in (268:269, 488)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 21130
          ;
          else if site in (300:301, 310:319)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 22010
          ;
          else if site in (320:329)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 22020
          ;
          else if site in (340:349)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 22030
          ;
          else if site in (384)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 22050
          ;
          else if site in (339, 381:383, 388, 390, 398, 399)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 22060
          ;
          else if site in (400:419)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 23000
          ;
          else if site in (380, 470:479, 490:499)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 24000
          ;
          else if site in (440:449)
            and hist in (8720:8790)
            then site_recode = 25010
          ;
          else if site in (440:449)
            and hist not in (8000:8005, 8010:8046, 8050:8084, 8090:8110,
            8720:8790, 9050:9055, 9140, 9590:9992)
            then site_recode = 25020
          ;
          else if site in (500:509)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 26000
          ;
          else if site in (530:539)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 27010
          ;
          else if site in (540:549)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 27020
          ;
          else if site in (559)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 27030
          ;
          else if site in (569)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 27040
          ;
          else if site in (529)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 27050
          ;
          else if site in (510:519)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 27060
          ;
          else if site in (570:579, 589)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 27070
          ;
          else if site in (619)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 28010
          ;
          else if site in (620:629)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 28020
          ;
          else if site in (600:609)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 28030
          ;
          else if site in (630:639)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 28040
          ;
          else if site in (670:679)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 29010
          ;
          else if site in (649, 659)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 29020
          ;
          else if site in (669)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 29030
          ;
          else if site in (680:689)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 29040
          ;
          else if site in (690:699)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 30000
          ;
          else if site in (710:719)
            and hist not in (9050:9055, 9140, 9530:9539, 9590:9992)
            then site_recode = 31010
          ;
          else if site in (710:719)
            and hist in (9530:9539)
            then site_recode = 31040
          ;
          else if site in (700:709, 720:729)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 31040
          ;
          else if site in (739)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 32010
          ;
          else if site in (379, 740:749, 750:759)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 32020
          ;
          else if site in (024, 098:099, 111, 142, 379, 422, 770:779)
            and hist in (9650:9667)
            then site_recode = 33011
          ;
          else if hist in (9650:9667)
            then site_recode = 33012
          ;
          else if site in (024, 098, 099, 111, 142, 379, 422, 770:779)
            and hist in (9590:9597, 9670:9671, 9673, 9675, 9678:9680, 9684,
            9687:9691, 9695, 9698:9702, 9705, 9708:9709, 9712, 9714:9719,
            9724:9729, 9735, 9737:9738, 9811:9818, 9823, 9827, 9837)
            then site_recode = 33041
          ;
          else if site not in (024, 098:099, 111, 142, 379, 422, 770:779)
            and hist in (9590:9597, 9670:9671, 9673, 9675, 9678:9680, 9684,
            9687, 9688, 9689:9691, 9695, 9698:9702, 9705, 9708:9709, 9712,
            9714:9719, 9724:9729, 9735, 9737, 9738)
            then site_recode = 33042
          ;
          else if site not in (024, 098:099, 111, 142, 379, 420:422, 424, 770:779)
            and hist in (9811:9818, 9823, 9827, 9837)
            then site_recode = 33042
          ;
          else if hist in (9731:9732, 9734)
            then site_recode = 34000
          ;
          else if hist in (9826, 9835:9836)
            then site_recode = 35011
          ;
          else if site in (420, 421, 424)
            and hist in (9811:9818, 9837)
            then site_recode = 35011
          ;
          else if site in (420, 421, 424)
            and hist in (9823)
            then site_recode = 35012
          ;
          else if hist in (9820, 9832:9834, 9940)
            then site_recode = 35013
          ;
          else if hist in (9840, 9861, 9865:9867, 9869, 9871:9874, 9895:9897,
            9898, 9910:9911, 9920)
            then site_recode = 35021
          ;
          else if hist in (9891)
            then site_recode = 35031
          ;
          else if hist in (9863, 9875:9876, 9945:9946)
            then site_recode = 35022
          ;
          else if hist in (9860, 9930)
            then site_recode = 35023
          ;
          else if hist in (9801, 9805:9809, 9931)
            then site_recode = 35041
          ;
          else if hist in (9733, 9742, 9800, 9831, 9870, 9948, 9963:9964)
            then site_recode = 35043
          ;
          else if site in (420, 421, 424)
            and hist in (9827)
            then site_recode = 35043
          ;
          else if hist in (9050:9055)
            then site_recode = 36010
          ;
          else if hist in (9140)
            then site_recode = 36020
          ;
          else if hist in (9740:9741, 9750:9769, 9950, 9960:9962, 9965:9967,
            9970:9971, 9975, 9980, 9982:9987, 9989, 9991:9992)
            then site_recode = 37000
          ;
          else if site in (760:768, 809)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 37000
          ;
          else if site in (420:424)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 37000
          ;
          else if site in (770:779)
            and hist not in (9050:9055, 9140, 9590:9992)
            then site_recode = 37000
          ;
          else site_recode = 99999;
        label site_recode="SEER Site Recode";
        drop site hist;
      run;
    %end;
%mend seer_site_recode;
/*********************************************

* Charlson comorbidity macro.sas
*
* Computes the Deyo version of the Charleson
*
*
*  Programmer
*     Hassan Fouayzi
*
*
* Input data required:
*
*     VDW Utilization files
*     Input SAS dataset INPUTDS
*        contains the variables MRN, STUDYID, and INDEXDT
*        INPATONLY flag - defauts to Inpatient only (I).  Valid values are
*                           I-inpatient or B-Both inpatient and outpatient
*                           or A-All encounter types
*        MALIG flag - Defaults to no(N).  If MALIG is yes (Y) then the weights
*                         of Metastasis and Malignancy are set to zero.
*                     This may be useful in a study of cancer.
*
*        NoEncounterGetsMissing - Defaults to (N).  Controls whether people for
*                                 whom no dx/px data is found get a charlson score
*                                 of 0 (default) or a missing value.  For cohorts whose
*                                 year-pre-index-date data capture is assured (usually via
*                                 enrollment data), having no encounters should legitimately
*                                 indicate a lack of any comorbidities & therefore a legit
*                                 score of 0.  Cohorts *not* previously vetted in this way may
*                                 not support that inference, and users should specify a Y
*                                 for this parameter to prevent unwarranted interpretation
*                                 of the Charlson score.
*
* Outputs:
*     Dataset &outputsd with on record per studyid
*     Variables
*       MI            = "Myocardial Infarction: "
*       CHD           = "Congestive heart disease: "
*       PVD           = "Peripheral vascular disorder: "
*       CVD           = "Cerebrovascular disease: "
*       DEM           = "Dementia: "
*       CPD           = "Chronic pulmonary disease: "
*       RHD           = "Rheumatologic disease: "
*       PUD           = "Peptic ulcer disease: "
*       MLIVD         = "Mild liver disease: "
*       DIAB          = "Diabetes: "
*       DIABC         = "Diabetes with chronic complications: "
*       PLEGIA        = "Hemiplegia or paraplegia: "
*       REN           = "Renal Disease: "
*       MALIGN        = "Malignancy, including leukemia and lymphoma: "
*       SLIVD         = "Moderate or severe liver disease: "
*       MST           = "Metastatic solid tumor: "
*       AIDS          = "AIDS: "
*       &IndexVarName = "Charlson score: "
*
*
* Dependencies:
*
*     To using this macro, better to add %include to include this macro before call it.
*     StdVars.sas--the site-customized list of standard macro variables.
*     The DX and PROC files to which stdvars.sas refer
*
*
* Example of use:
*     %charls10(testing,oot, Charles, inpatonly=B)
*
* Notes:
*   You will often need to remove certain disease format categories for your
*   project. For instance, the Ovarian Ca EOL study removed Metastatic Solid
*   Tumor since all were in end stages. It would be inappopriate not to exclude
*   this category in this instance. Please use this macro wisely.
*
*   There are several places that need to be modified.
*     1.  Comment the diagnosis category in the format.
*     2.  Remove that diagnosis category in 2 arrays.
*     3.  Select the time period for the source data and a reference point.
*     4.  Data selection.  All diagnoses and procedures?  Inpt only?  The user
*         may want to remove certain types of data to make the sources from all
*         sites consistent.
*
* Version History
*
*     Written by Hassan Fouayzi starting with source from Rick Krajenta
*     Modified into a SAS Macro format           Gene Hart         2005/04/20
*     Malig flag implemented                     Gene Hart         2005/05/04
*     Add flag to mark thos with no visits       Gene Hart         2005/05/09
*     Add additional codes to disease            Tyler Ross        2006/03/31
*     Changed EncType for IP visits to new ut
*       specs and allowed all visit types option Tyler Ross       2006/09/15
*     Removed "456" from Moderate/Severe Liver   Hassan Fouayzi    2006/12/21
*     Add ICD-10 Format (in below C001 mark)   Wei Tao           2015/11/18
*
*     Should the coalesce function be on studyid or mrn?  1 MRN with 2 STUDYIDs
*       could happen
*
*     move then proc codes to a format
*
* Source publication
*     From: Fouayzi, Hassan [mailto:hfouayzi@meyersprimary.org]
*     Sent: Wednesday, May 04, 2005 9:07 AM
*     Subject: RE: VDW Charlson macro
...
*     “Deyo RA, Cherkin DC, Ciol MA. Adapting a clinical comorbidity Index for
*     use with ICD-9-CM administrative databases.
*       J Clin Epidemiol 1992; 45: 613-619”.
*     We added CPT codes and a couple of procedures for Peripheral
*       vascular disorder.
*
*********************************************/
%macro charlson(inputds
               , IndexDateVarName
               , outputds
               , IndexVarName
               , inpatonly=I
               , malig=N
               , NoEncounterGetsMissing = N
               , enctype_list =
               , days_lookback = 365
               );

   /**********************************************/
   /*Define and format diagnosis codes*/
   /**********************************************/

   ** TODO:  Come up with an ICD-10 version of this format!!! ;
   ** C001: Adding ICD-10 version format below****************;
    PROC FORMAT;
      VALUE $ICD10CF
       /* Myocardial infraction */
        "I21  "-"I22.9",
        "I25.2"  = "MI"
       /* Congestive heart disease */
         "I50  "-"I50.999" = "CHD"
       /* Peripheral vascular disorder */
        "I70  "-"I71.9",
        "I73.01 ",
        "I73.1",
        "I73.9",
        "I79.0",
        "I96",
        "Z95.8"-"Z95.9" = "PVD"
       /* Cerebrovascular disease */
            "G45  "-"G46.999",
        "I60  "-"I69.999" = "CVD"
       /* Dementia */
        "F00  "-"F03.999",
        "F05  "- "F05.999" = "DEM"
       /* Chronic pulmonary disease */
        "J40  "-"J47.999",
        "J60  "-"J67.999",
        "J68.4" =  "CPD"
       /* Rheumatologic disease */
        "M05  "-"M06.999",
        "M32  "-"M34.999",
        "M35.3" = "RHD"
       /* Peptic ulcer disease */
        "K25  "-"K28.999",
        "K56.60 " = "PUD"
       /* Mild liver disease */
        "K70.0"-"K70.31",
        "K73  "-"K74.999",
        "K75.4  " = "MLIVD"
       /* Diabetes */
        "E10.10 "-"E10.11 ",
        "E10.51 "-"E10.52 ",
        "E10.59 ",
        "E10.641",
        "E10.65 ",
        "E10.69 ",
        "E10.9  ",
        "E11.00 "-"E11.01 ",
        "E11.51 "-"E11.52 ",
        "E11.59 ",
        "E11.641",
        "E11.65 ",
        "E11.69 ",
        "E11.9  ",
        "E13.00 "-"E13.01 ",
        "E13.10 "-"E13.11 ",
        "E13.51 "-"E13.52 ",
        "E13.59 ",
        "E13.641",
        "E13.9  " = "DIAB"
       /* Diabetes with chronic complications */
        "E10.2 "-"E10.5",
        "E10.61 "-"E10.619",
        "E11.2 "-"E11.5",
        "E11.61 "-"E11.619",
        "E13.2 "-"E13.5",
        "E13.61 "-"E13.619"= "DIABC "
       /* Hemiplegia or paraplegia */
        "G04.1",
        "G81  "-"G82.999" = "PLEGIA"
       /* Renal Disease */
        "N03.0  "-"N03.9  ",
        "N05.2  "-"N05.5  ",
        "N05.9  ",
        "N06.2  "-"N06.5  ",
        "N07.2  "-"N07.5  ",
        "N08    ",
        "N17.1  "-"N17.2  ",
        "N18.1  "-"N18.6  ",
        "N18.9  ",
        "N19    ",
        "N25.0  ",
        "N25.1  ",
        "N25.81 ",
        "N25.89 ",
        "N25.9  " = "REN"
       /*Malignancy, including leukemia and lymphoma */
        "C00  "-"C26.999",
        "C30  "-"C34.999",
        "C37  "-"C41.999",
        "C43  "-"C43.999",
        "C45  "-"C45.7 ",
        "C46  "-"C58.999",
        "C60  "-"C76.999",
        "C81  "-"C85.999",
        "C86  "-"C86.999",
        "C88  "-"C88.999",
        "C90  "-"C97.999",
        "D03.0  ",
        "D03.10 "-"D03.12 ",
        "D03.20 "-"D03.22 ",
        "D03.30 ",
        "D03.39 ",
        "D03.4  ",
        "D03.51 "-"D03.52 ",
        "D03.59 ",
        "D03.60 "-"D03.62 ",
        "D03.70 "-"D03.72 ",
        "D03.8  ",
        "D03.9  ",
        "D45    " = "MALIGN"
       /* Moderate or severe liver disease */
        "I85.00 "-"I85.01 ",
        "I85.10 "-"I85.11 ",
        "K70.41 ",
        "K71.11 "-"K72.01 ",
        "K72.10 "-"K72.11 ",
        "K72.90 "-"K72.91 ",
        "K76.6  "-"K76.7  "  = "SLIVD"
       /* Metastatic solid tumor */
        "C45.9  ",
        "C77  "-"C80.999"  = "MST"
       /* AIDS */
        "B20  "-"B20.999" = "AIDS"
       /* Other */
          other   = "other"
     ;
   ** TODO: character formats w/ranges make me nervous--this should be vetted against a lookup dataset. ;
      VALUE $ICD9CF
       /* Myocardial infraction */
        "410   "-"410.92",
        "412   " = "MI"
       /* Congestive heart disease */
        "428   "-"428.9 " = "CHD"
       /* Peripheral vascular disorder */
        "440.20"-"440.24",
        "440.31"-"440.32",
        "440.8 ",
        "440.9 ",
        "443.9 ",
        "441   "-"441.9 ",
        "785.4 ",
        "V43.4 ",
        "v43.4 " = "PVD"
       /* Cerebrovascular disease */
           "430   "-"438.9 " = "CVD"
       /* Dementia */
        "290   "-"290.9 " = "DEM"
       /* Chronic pulmonary disease */
        "490   "-"496   ",
        "500   "-"505   ",
        "506.4 " =  "CPD"
       /* Rheumatologic disease */
        "710.0 ",
         "710.1 ",
          "710.4 ",
         "714.0 "-"714.2 ",
         "714.81",
         "725   " = "RHD"
       /* Peptic ulcer disease */
        "531   "-"534.91" = "PUD"
       /* Mild liver disease */
        "571.2 ",
        "571.5 ",
        "571.6 ",
        "571.4 "-"571.49" = "MLIVD"
       /* Diabetes */
        "250   "-"250.33",
        "250.7 "-"250.73" = "DIAB"
       /* Diabetes with chronic complications */
        "250.4 "-"250.63" = "DIABC"
       /* Hemiplegia or paraplegia */
        "344.1 ",
        "342   "-"342.92" = "PLEGIA"
       /* Renal Disease */
        "582   "-"582.9 ",
        "583   "-"583.7 ",
        "585   "-"586   ",
        "588   "-"588.9 " = "REN"
       /*Malignancy, including leukemia and lymphoma */
        "140   "-"172.9 ",
        "174   "-"195.8 ",
        "200   "-"208.91" = "MALIGN"
       /* Moderate or severe liver disease */
        "572.2 "-"572.8 ",
        "456.0 "-"456.21" = "SLIVD"
       /* Metastatic solid tumor */
        "196   "-"199.1 " = "MST"
       /* AIDS */
        "042   "-"044.9 " = "AIDS"
       /* Other */
          other   = "other"
     ;
   run;

   ** For debugging. ;
   %local sqlopts ;
   %let sqlopts = feedback sortmsg stimer ;
   %**let sqlopts = ;

   *******************************************************************************;
   ** subset to the utilization data of interest (add the people with no visits  *;
   **    back at the end                                                         *;
   *******************************************************************************;


   ***********************************************;
   ** implement the Inpatient and Outpatient Flags;
   *********************************************** ;
  %if       &inpatonly =I %then %let inpatout= AND EncType in ('IP');
  %else %if &inpatonly =B %then %let inpatout= AND EncType in ('IP','AV');
  %else %if &inpatonly =A %then %let inpatout=;
  %else %if &inpatonly =C %then %let inpatout= AND EncType in (&enctype_list);
  %else %do;
   %Put ERROR in Inpatonly flag.;
   %Put Valid values are I for Inpatient and B for both Inpatient and Outpatient (AV), A for All Encounters or C for a custom list (use the enctype_list parameter) ;
  %end;

   proc sql &sqlopts ;

      create table _ppl as
      select MRN, Min(&IndexDateVarName) as &IndexDateVarName format = mmddyy10.
      from &inputds
      group by MRN ;

     %local TotPeople ;
      %let TotPeople = &SQLOBS ;

     alter table _ppl add primary key (MRN) ;

     create table  _DxSubset as
     select sample.mrn
    , &IndexDateVarName
    , adate
    , case dx_codetype when '09' then put(dx, $icd9cf.)
                 when '10' then put (dx, $ICD10CF.)
               else '???' end as CodedDx  /*C001*/
     from &_vdw_dx as d INNER JOIN _ppl as sample
     ON    d.mrn = sample.mrn
     where  adate between sample.&IndexDateVarName-1
                     and sample.&IndexDateVarName-&days_lookback
               &inpatout.
     ;

      * select count(distinct MRN) as DxPeople format = comma.
        label = "No. people having any Dxs w/in a year prior to &IndexDateVarName"
            , (CALCULATED DxPeople / &TotPeople) as PercentWithDx
               format = percent6.2 label = "Percent of total"
      from _DxSubset ;

     create table _PxAssign as
     select distinct p.mrn, 1 as PVD
     from &_vdw_px (where = ( "35355" <= PX <= "35381" or
            PX in ("34201","34203","35454","35456","35459","35470", "38.48", "93668"
                   "35473","35474","35482","35483","35485","35492","35493",
                   "35495","75962","75992"
                   "35521","35533","35541","35546","35548","35549","35551",
                   "35556","35558","35563","35565","35566","35571","35582",
                   "35583","35584","35585","35586","35587","35621","35623",
                   "35641","35646","35647","35651","35654","35656","35661",
                   "35663","35665","35666","35671"
                  '04RK07Z', '04RK0JZ', '04RK0KZ', '04RK47Z', '04RK4JZ',
                  '04RK4KZ', '04RL07Z', '04RL0JZ', '04RL0KZ', '04RL47Z', '04RL4JZ',
                  '04RL4KZ', '04RM07Z', '04RM0JZ', '04RM0KZ', '04RM47Z', '04RM4JZ',
                  '04RM4KZ', '04RN07Z', '04RN0JZ', '04RN0KZ', '04RN47Z', '04RN4JZ',
                  '04RN4KZ', '04RP07Z', '04RP0JZ', '04RP0KZ', '04RP47Z', '04RP4JZ',
                  '04RP4KZ', '04RQ07Z', '04RQ0JZ', '04RQ0KZ', '04RQ47Z', '04RQ4JZ',
                  '04RQ4KZ', '04RR07Z', '04RR0JZ', '04RR0KZ', '04RR47Z', '04RR4JZ',
                  '04RR4KZ', '04RS07Z', '04RS0JZ', '04RS0KZ', '04RS47Z', '04RS4JZ',
                  '04RS4KZ', '04RT07Z', '04RT0JZ', '04RT0KZ', '04RT47Z', '04RT4JZ',
                  '04RT4KZ', '04RU07Z', '04RU0JZ', '04RU0KZ', '04RU47Z', '04RU4JZ',
                  '04RU4KZ', '04RV07Z', '04RV0JZ', '04RV0KZ', '04RV47Z', '04RV4JZ',
                  '04RV4KZ', '04RW07Z', '04RW0JZ', '04RW0KZ', '04RW47Z', '04RW4JZ',
                  '04RW4KZ', '04RY07Z', '04RY0JZ', '04RY0KZ', '04RY47Z', '04RY4JZ',
                  '04RY4KZ'))) as p INNER JOIN
          _ppl as sample
     on   p.mrn = sample.mrn
           where px_codetype in ('C4', 'H4', '09', '10')
           and adate between sample.&IndexDateVarName-1
                         and sample.&IndexDateVarName-&days_lookback
           &inpatout.
     ;

      * select count(distinct MRN) as PxPeople format = comma.
        label = "No. people who had any Pxs w/in a year prior to &IndexDateVarName"
            , (CALCULATED PxPeople / &TotPeople) as PercentWithPx
                format = percent6.2 label = "Percent of total sample"
      from _PxAssign ;

   quit ;

   proc sort data = _DxSubset ;
      by MRN ;
   run ;

   proc sort data = _PxAssign ;
      by MRN ;
   run ;

   /**********************************************/
   /*** Assing DX based flagsts                ***/
   /***                                        ***/
   /***                                        ***/
   /**********************************************/

   %local var_list ;
   %let var_list = MI CHD PVD CVD DEM CPD RHD PUD MLIVD DIAB
                   DIABC PLEGIA REN MALIGN SLIVD MST AIDS ;

   data _DxAssign ;
     length &var_list 3 ;
     retain           &var_list ;
     set _DxSubset;
     by mrn;
     array COMORB (*) &var_list ;
     if first.mrn then do;
        do I=1 to dim(COMORB);
           COMORB(I) = 0 ;
        end;
     end;
     select (CodedDx);
        when ('MI')    MI     = 1;
        when ('CHD')   CHD    = 1;
        when ('PVD')   PVD    = 1;
        when ('CVD')   CVD    = 1;
        when ('DEM')   DEM    = 1;
        when ('CPD')   CPD    = 1;
        when ('RHD')   RHD    = 1;
        when ('PUD')   PUD    = 1;
        when ('MLIVD') MLIVD  = 1;
        when ('DIAB')  DIAB   = 1;
        when ('DIABC') DIABC  = 1;
        when ('PLEGIA')PLEGIA = 1;
        when ('REN')   REN    = 1;
        when ('MALIGN')MALIGN = 1;
        when ('SLIVD') SLIVD  = 1;
        when ('MST')   MST    = 1;
        when ('AIDS')  AIDS   = 1;
        otherwise ;
     end;
     if last.mrn then output;
     keep   mrn  &var_list ;
   run;

   /** Connect DXs and PROCs together  **/
   proc sql &sqlopts ;
     ** Adding a bunch of coalesces here in case there are ppl w/procs but no dxs. ;
     create table _DxPxAssign as
      select  coalesce(D.MRN, P.MRN) as MRN
            , coalesce(D.MI    , 0)  as MI
            , coalesce(D.CHD   , 0)  as CHD
            , coalesce(D.CVD   , 0)  as CVD
            , coalesce(D.DEM   , 0)  as DEM
            , coalesce(D.CPD   , 0)  as CPD
            , coalesce(D.RHD   , 0)  as RHD
            , coalesce(D.PUD   , 0)  as PUD
            , coalesce(D.MLIVD , 0)  as MLIVD
            , coalesce(D.DIAB  , 0)  as DIAB
            , coalesce(D.DIABC , 0)  as DIABC
            , coalesce(D.PLEGIA, 0)  as PLEGIA
            , coalesce(D.REN   , 0)  as REN
            , coalesce(D.MALIGN, 0)  as MALIGN
            , coalesce(D.SLIVD , 0)  as SLIVD
            , coalesce(D.MST   , 0)  as MST
            , coalesce(D.AIDS  , 0)  as AIDS
            , max(D.PVD, P.PVD)      as PVD
      from  WORK._DXASSIGN as D full outer join
            WORK._PXASSIGN P
      on    D.MRN = P.MRN
      ;
   quit ;

   *****************************************************;
   * Assign the weights and compute the index
   *****************************************************;

   Data _WithCharlson;
     set _DxPxAssign;
     M1=1;M2=1;M3=1;

   ** implement the MALIG flag;
            %if &malig =N %then %do; O1=1; O2=1; %end;
      %else %if &malig =Y %then %do; O1=0; O2=0; %end;
      %else %do;
        %Put ERROR in MALIG flag.  Valid values are Y (Cancer study. Zero weight;
        %Put ERROR the cancer vars)  and N (treat cancer normally);
      %end;

     if SLIVD = 1 then M1=0;
     if DIABC = 1 then M2=0;
     if MST   = 1 then M3=0;


   &IndexVarName =   sum(MI , CHD , PVD , CVD , DEM , CPD , RHD ,
                     PUD , M1*MLIVD , M2*DIAB , 2*DIABC , 2*PLEGIA , 2*REN ,
                     O1*2*M3*MALIGN , 3*SLIVD , O2*6*MST , 6*AIDS) ;

   Label
     MI            = "Myocardial Infarction: "
     CHD           = "Congestive heart disease: "
     PVD           = "Peripheral vascular disorder: "
     CVD           = "Cerebrovascular disease: "
     DEM           = "Dementia: "
     CPD           = "Chronic pulmonary disease: "
     RHD           = "Rheumatologic disease: "
     PUD           = "Peptic ulcer disease: "
     MLIVD         = "Mild liver disease: "
     DIAB          = "Diabetes: "
     DIABC         = "Diabetes with chronic complications: "
     PLEGIA        = "Hemiplegia or paraplegia: "
     REN           = "Renal Disease: "
     MALIGN        = "Malignancy, including leukemia and lymphoma: "
     SLIVD         = "Moderate or severe liver disease: "
     MST           = "Metastatic solid tumor: "
     AIDS          = "AIDS: "
     &IndexVarName = "Charlson score: "
   ;

   keep MRN &var_list &IndexVarName ;

   run;

   /* add the people with no visits back in, and create the final dataset */
   /* people with no visits or no comorbidity DXs have all vars set to zero */

   proc sql &sqlopts ;
     create table &outputds as
     select distinct i.MRN
         , i.&IndexDateVarName
         , coalesce(w.MI           , 0) as  MI
                      label = "Myocardial Infarction: "
         , coalesce(w.CHD          , 0) as  CHD
                      label = "Congestive heart disease: "
         , coalesce(w.PVD          , 0) as  PVD
                      label = "Peripheral vascular disorder: "
         , coalesce(w.CVD          , 0) as  CVD
                      label = "Cerebrovascular disease: "
         , coalesce(w.DEM          , 0) as  DEM
                      label = "Dementia: "
         , coalesce(w.CPD          , 0) as  CPD
                      label = "Chronic pulmonary disease: "
         , coalesce(w.RHD          , 0) as  RHD
                      label = "Rheumatologic disease: "
         , coalesce(w.PUD          , 0) as  PUD
                      label = "Peptic ulcer disease: "
         , coalesce(w.MLIVD        , 0) as  MLIVD
                      label = "Mild liver disease: "
         , coalesce(w.DIAB         , 0) as  DIAB
                      label = "Diabetes: "
         , coalesce(w.DIABC        , 0) as  DIABC
                      label = "Diabetes with chronic complications: "
         , coalesce(w.PLEGIA       , 0) as  PLEGIA
                      label = "Hemiplegia or paraplegia: "
         , coalesce(w.REN          , 0) as  REN
                      label = "Renal Disease: "
         , coalesce(w.MALIGN       , 0) as  MALIGN
                      label = "Malignancy, including leukemia and lymphoma: "
         , coalesce(w.SLIVD        , 0) as  SLIVD
                      label = "Moderate or severe liver disease: "
         , coalesce(w.MST          , 0) as  MST
                      label = "Metastatic solid tumor: "
         , coalesce(w.AIDS         , 0) as  AIDS
                      label = "AIDS: "
         %if %upcase(&NoEncounterGetsMissing) = Y %then %do ;
           , w.&IndexVarName
         %end ;
         %else %do ;
           , coalesce(w.&IndexVarName, 0) as &IndexVarName
         %end ;
          label = "Charlson score: "
         , (w.MRN is null)              as  NoVisitFlag
                      label = "No diagnoses or procedures found in the year prior to &IndexDateVarName for this person"
     from _ppl as i left join _WithCharlson as w
     on i.MRN = w.MRN
     ;

  /* clean up work sas datasets */
  proc datasets nolist ;
    delete _DxSubset
           _PxSubset
           _DxAssign
           _PxAssign
           _DxPxAssign
           _WithCharlson
           _NoVisit
           _ppl
           ;
  quit ;
%mend charlson;


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* PROGRAM NAME: ElixhauserICD9and10HCUP.sas
* AUTHOR: Joey Eavey (KPWA), based on code from Anne Elixhauser at HCUP, Hao He (UW) and
*      Elham Sagheb (Marshfield)
*      contact information: eavey.j@ghc.org
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* PURPOSE: This program calculates the index and prior Elixhasuer comorbidity scores for
*      patients in the input table using code from Anne Elixhauser's group at ARHQ/HCUP.
*
*          This program uses the VDW dx table to calculate the  Elix score and can accomodate
*      both ICD9 and ICD10CM diagnosis codes.  The time frame for prior diagnoses can be
*      adjusted using days_lookback parameter.  Additionally, this code will calculate an
*      Elixhauser score for each visit/index date in the dataset - if a patient has
*      multiple records by index date, this program computes an Elixhauser score for each
*      of the patients' records.
*
*  Note that this program relies on two supplementary programs to run - one that assigns dx
*          formats and another that uses those formats to assign comorbidity flags.
*
* REFERENCES:
*      https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp
*      https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp
*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* DATE CREATED: 11/27/2017
* MODIFICATION HISTORY: ICD10 CODES LAST UPDATED 12/1/2017 - check HCUP website for updates!
*  [RP 20180103] Consolidated several programs/macros into this single file/macro.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*  !!!NOTE TO PROGRAMMERS!!! -- The Elixhauser score was developed by Anne Elixhauser at HCUP/
*      AHRQ and has been validated using index date diagnoses for inpatient visits
*      only with the DRG restriction applied.  Any deviation from these parameters
*      will result in a non-standard, non-validated Elixhauser score and should be
*      discussed with your principal investigator or research team.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* PLACES USER SHOULD LOCALIZE CODE:
*   Users should modify all of the code after this introductory section and STOP modifying code
*  when they reach the line that says 'PLEASE DO NOT CHANGE CODE AFTER THIS POINT'
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* INPUTS:
*    Study dataset that should contain a unique person identifier and an index date for each
*    record.  The name of this dataset is specified in the input_table parameter of the macro
*    Again, there can be multiple visit records/index dates per person.
*
*    VDW diagnosis table
*
*    SAS program containing ICD9 and ICD10 formats from HCUP
*    SAS program  that uses dx formats to calculate comorbidity flags
*
* OUTPUTS:
*    out.comorbidity - dataset that contains all relevant comorbidity flags and Elixhauser
*    scores with and without cancer included - unique score for each patient-index_date
*    combination.
*
*    Elixhauser_indices.xls - statistical reports on Elixhauser scores for study population
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

%macro elixhauser(inputds
                , mrn             = mrn
                , index_date      = index_date
                , days_lookback   = 365
                , inpatonly       = I
                , enctype_list    =
                , drg_restriction = Y
                , outputds        = );

  /********************************************************************************/
  /* Title:  CREATION OF FORMAT  FOR ELIXHAUSER COMORBIDITY GROUPS            */
  /*         ELIXHAUSER COMORBIDITY SOFTWARE, VERSION 3.7                        */
  /*                                                                        */
  /* Description:                                                           */
  /*    Define all ICD 9 and 10 codes, V29 and V35 MS-DRGS, and labels for each   */
  /*    comorbidity format.                                        */
  /*                                                                        */
  /********************************************************************************/
  /********************************************************************************/

  /* 12-11-2017:  Last updated from HCUP Elixhauser website:
  /*    https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp   */

  /*    Users should reference the website above to see if updates are needed to ICD10
  /*    diagnosis and/or DRG formats.                             */

  /*    Note that the ICD9 DRG and dx coding will not change.  DRG formats between ICD9
  /*     and ICD10 are very similar and there are only slight differences in the CARDDRG
  /*     format - be mindful when updating the DRG codes!                   */

  /***************************************************/
  /***************************************************/

  /* ICD 10 DIAGNOSIS CODE FORMATTING
  /***************************************************/

  /***************************************************/
  proc format;
    value $RCOMFMT

      "D473"="NONE"            /*NONE*/

      "B20"="AIDS"             /*HIV and AIDS (Acquired immune deficiency syndrome)*/

      "F1010",  "F1011",  "F10120",  "F10121",  "F10129",  "F1014",  "F10150",  "F10151",  "F10159",  "F10180",  "F10181",
      "F10182", "F10188", "F1019", "F1020", "F1021", "F10220", "F10221", "F10229", "F10230", "F10231", "F10232",
      "F10239", "F1024", "F10250", "F10251", "F10259", "F1026", "F1027", "F10280", "F10281", "F10282", "F10288",
      "F1029", "F10921", "F1094", "F10950", "F10951", "F10959", "F1096", "F1097", "F10980", "F10981", "F10982", "F10988",
      "F1099" = "ALCOHOL"        /*Alcohol abuse*/

      "D501", "D508", "D509", "D510", "D511", "D512", "D513", "D518", "D519", "D520", "D521",
      "D528", "D529", "D530", "D531", "D532", "D538", "D539", "D630", "D631", "D638"
      , "D649" = "ANEMDEF"         /*Deficiency anemias*/

      "L900", "L940", "L941", "L943", "M0500", "M05011", "M05012", "M05019", "M05021", "M05022", "M05029",
      "M05031", "M05032", "M05039", "M05041", "M05042", "M05049", "M05051", "M05052", "M05059", "M05061", "M05062",
      "M05069", "M05071", "M05072", "M05079", "M0509", "M0510", "M05111", "M05112", "M05119", "M05121", "M05122",
      "M05129", "M05131", "M05132", "M05139", "M05141", "M05142", "M05149", "M05151", "M05152", "M05159", "M05161",
      "M05162", "M05169", "M05171", "M05172", "M05179", "M0519", "M0520", "M05211", "M05212", "M05219", "M05221",
      "M05222", "M05229", "M05231", "M05232", "M05239", "M05241", "M05242", "M05249", "M05251", "M05252", "M05259",
      "M05261", "M05262", "M05269", "M05271", "M05272", "M05279", "M0529", "M0530", "M05311", "M05312", "M05319",
      "M05321", "M05322", "M05329", "M05331", "M05332", "M05339", "M05341", "M05342", "M05349", "M05351", "M05352",
      "M05359", "M05361", "M05362", "M05369", "M05371", "M05372", "M05379", "M0539", "M0540", "M05411", "M05412",
      "M05419", "M05421", "M05422", "M05429", "M05431", "M05432", "M05439", "M05441", "M05442", "M05449", "M05451",
      "M05452", "M05459", "M05461", "M05462", "M05469", "M05471", "M05472", "M05479", "M0549", "M0550", "M05511",
      "M05512", "M05519", "M05521", "M05522", "M05529", "M05531", "M05532", "M05539", "M05541", "M05542", "M05549",
      "M05551", "M05552", "M05559", "M05561", "M05562", "M05569", "M05571", "M05572", "M05579", "M0559", "M0560",
      "M05611", "M05612", "M05619", "M05621", "M05622", "M05629", "M05631", "M05632", "M05639", "M05641", "M05642",
      "M05649", "M05651", "M05652", "M05659", "M05661", "M05662", "M05669", "M05671", "M05672", "M05679", "M0569",
      "M0570", "M05711", "M05712", "M05719", "M05721", "M05722", "M05729", "M05731", "M05732", "M05739", "M05741",
      "M05742", "M05749", "M05751", "M05752", "M05759", "M05761", "M05762", "M05769", "M05771", "M05772", "M05779",
      "M0579", "M0580", "M05811", "M05812", "M05819", "M05821", "M05822", "M05829", "M05831", "M05832", "M05839",
      "M05841", "M05842", "M05849", "M05851", "M05852", "M05859", "M05861", "M05862", "M05869", "M05871", "M05872",
      "M05879", "M0589", "M059", "M0600", "M06011", "M06012", "M06019", "M06021", "M06022", "M06029", "M06031",
      "M06032", "M06039", "M06041", "M06042", "M06049", "M06051", "M06052", "M06059", "M06061", "M06062", "M06069",
      "M06071", "M06072", "M06079", "M0608", "M0609", "M061", "M0620", "M06211", "M06212", "M06219", "M06221",
      "M06222", "M06229", "M06231", "M06232", "M06239", "M06241", "M06242", "M06249", "M06251", "M06252", "M06259",
      "M06261", "M06262", "M06269", "M06271", "M06272", "M06279", "M0628", "M0629", "M0630", "M06311", "M06312",
      "M06319", "M06321", "M06322", "M06329", "M06331", "M06332", "M06339", "M06341", "M06342", "M06349", "M06351",
      "M06352", "M06359", "M06361", "M06362", "M06369", "M06371", "M06372", "M06379", "M0638", "M0639", "M064",
      "M0680", "M06811", "M06812", "M06819", "M06821", "M06822", "M06829", "M06831", "M06832", "M06839", "M06841",
      "M06842", "M06849", "M06851", "M06852", "M06859", "M06861", "M06862", "M06869", "M06871", "M06872", "M06879",
      "M0688", "M0689", "M069", "M0800", "M08011", "M08012", "M08019", "M08021", "M08022", "M08029", "M08031",
      "M08032", "M08039", "M08041", "M08042", "M08049", "M08051", "M08052", "M08059", "M08061", "M08062", "M08069",
      "M08071", "M08072", "M08079", "M0808", "M0809", "M081", "M0820", "M08211", "M08212", "M08219", "M08221",
      "M08222", "M08229", "M08231", "M08232", "M08239", "M08241", "M08242", "M08249", "M08251", "M08252", "M08259",
      "M08261", "M08262", "M08269", "M08271", "M08272", "M08279", "M0828", "M0829", "M083", "M0840", "M08411",
      "M08412", "M08419", "M08421", "M08422", "M08429", "M08431", "M08432", "M08439", "M08441", "M08442", "M08449",
      "M08451", "M08452", "M08459", "M08461", "M08462", "M08469", "M08471", "M08472", "M08479", "M0848", "M0880",
      "M08811", "M08812", "M08819", "M08821", "M08822", "M08829", "M08831", "M08832", "M08839", "M08841", "M08842",
      "M08849", "M08851", "M08852", "M08859", "M08861", "M08862", "M08869", "M08871", "M08872", "M08879", "M0888",
      "M0889", "M0890", "M08911", "M08912", "M08919", "M08921", "M08922", "M08929", "M08931", "M08932", "M08939",
      "M08941", "M08942", "M08949", "M08951", "M08952", "M08959", "M08961", "M08962", "M08969", "M08971", "M08972",
      "M08979", "M0898", "M0899", "M1200", "M12011", "M12012", "M12019", "M12021", "M12022", "M12029", "M12031",
      "M12032", "M12039", "M12041", "M12042", "M12049", "M12051", "M12052", "M12059", "M12061", "M12062", "M12069",
      "M12071", "M12072", "M12079", "M1208", "M1209", "M320", "M3210", "M3211", "M3212", "M3213", "M3214",
      "M3215", "M3219", "M328", "M329", "M3300", "M3301", "M3302", "M3303", "M3309", "M3310", "M3311",
      "M3312", "M3313", "M3319", "M3320", "M3321", "M3322", "M3329", "M3390", "M3391", "M3392", "M3393",
      "M3399", "M340", "M341", "M342", "M3481", "M3482", "M3483", "M3489", "M349", "M3500", "M3501",
      "M3502", "M3503", "M3504", "M3509", "M351", "M353", "M355", "M358", "M359", "M360", "M368",
      "M450", "M451", "M452", "M453", "M454", "M455", "M456", "M457", "M458", "M459", "M4600",
      "M4601", "M4602", "M4603", "M4604", "M4605", "M4606", "M4607", "M4608", "M4609", "M461", "M4650",
      "M4651", "M4652", "M4653", "M4654", "M4655", "M4656", "M4657", "M4658", "M4659", "M4680", "M4681",
      "M4682", "M4683", "M4684", "M4685", "M4686", "M4687", "M4688", "M4689", "M4690", "M4691", "M4692",
      "M4693", "M4694", "M4695", "M4696", "M4697", "M4698", "M4699", "M488X1", "M488X2", "M488X3", "M488X4",
      "M488X5", "M488X6", "M488X7", "M488X8", "M488X9", "M4980", "M4981", "M4982", "M4983", "M4984", "M4985", "M4986",
      "M4987", "M4988", "M4989" = "ARTH"           /*Rheumatoid arthritis/collagen vascular diseases*/

      "D500", "O9081", "O99011", "O99012", "O99013", "O99019", "O9902", "O9903"="BLDLOSS"        /*Blood loss anemia*/

      "I0981", "I501", "I5020", "I5021", "I5022", "I5023", "I5030", "I5031", "I5032", "I5033", "I5040",
      "I5041", "I5042", "I5043", "I50810", "I50811", "I50812", "I50813", "I50814", "I5082", "I5083", "I5084", "I5089",
      "I509"="CHF"             /*Congestive heart failure*/

      "J40", "J410", "J411", "J418", "J42", "J430", "J431", "J432", "J438", "J439", "J440",
      "J441", "J449", "J4520", "J4521", "J4522", "J4530", "J4531", "J4532", "J4540", "J4541", "J4542",
      "J4550", "J4551", "J4552", "J45901", "J45902", "J45909", "J45990", "J45991", "J45998", "J470", "J471", "J479",
      "J60", "J61", "J620", "J628", "J630", "J631", "J632", "J633", "J634", "J635", "J636",
      "J64", "J660", "J661", "J662", "J668", "J670", "J671", "J672", "J673", "J674", "J675",
      "J676", "J677", "J678", "J679", "J684"="CHRNLUNG"        /*Chronic pulmonary disease*/

      "D65", "D66", "D67", "D680", "D681", "D682", "D68311", "D68312", "D68318", "D6832", "D684",
      "D688", "D689", "D691", "D693", "D6941", "D6942", "D6949", "D6951", "D6959", "D696", "D7582",
      "O99111", "O99112", "O99113", "O99119", "O9912", "O9913"="COAG"           /*Coagulation deficiency*/

      "F320", "F321", "F322", "F323", "F328", "F3281", "F3289", "F329", "F330", "F331", "F332",
      "F333", "F338", "F339", "F341", "F4321"="DEPRESS"        /*Depression*/

      "E0800", "E0801", "E0810", "E0811", "E089", "E0900", "E0901", "E0910", "E0911", "E099", "E1010",
      "E1011", "E109", "E1100", "E1101", "E1110", "E1111", "E119", "E1300", "E1301", "E1310", "E1311",
      "E139", "O24011", "O24012", "O24013", "O24019", "O2402", "O2403", "O24111", "O24112", "O24113", "O24119",
      "O2412", "O2413", "O24311", "O24312", "O24313", "O24319", "O2432", "O2433", "O24811", "O24812", "O24813",
      "O24819", "O2482", "O2483", "O24911", "O24912", "O24913", "O24919", "O2492", "O2493"="DM"             /*Diabetes without chronic complications*/

      "E0821", "E0822", "E0829", "E08311", "E08319", "E08321", "E083211", "E083212", "E083213", "E083219", "E08329",
      "E083291", "E083292", "E083293", "E083299", "E08331", "E083311", "E083312", "E083313", "E083319", "E08339", "E083391",
      "E083392", "E083393", "E083399", "E08341", "E083411", "E083412", "E083413", "E083419", "E08349", "E083491", "E083492",
      "E083493", "E083499", "E08351", "E083511", "E083512", "E083513", "E083519", "E083521", "E083522", "E083523", "E083529",
      "E083531", "E083532", "E083533", "E083539", "E083541", "E083542", "E083543", "E083549", "E083551", "E083552", "E083553",
      "E083559", "E08359", "E083591", "E083592", "E083593", "E083599", "E0836", "E0837X1", "E0837X2", "E0837X3", "E0837X9", "E0839",
      "E0840", "E0841", "E0842", "E0843", "E0844", "E0849", "E0851", "E0852", "E0859", "E08610", "E08618",
      "E08620", "E08621", "E08622", "E08628", "E08630", "E08638", "E08641", "E08649", "E0865", "E0869", "E088",
      "E0921", "E0922", "E0929", "E09311", "E09319", "E09321", "E093211", "E093212", "E093213", "E093219", "E09329",
      "E093291", "E093292", "E093293", "E093299", "E09331", "E093311", "E093312", "E093313", "E093319", "E09339", "E093391",
      "E093392", "E093393", "E093399", "E09341", "E093411", "E093412", "E093413", "E093419", "E09349", "E093491", "E093492",
      "E093493", "E093499", "E09351", "E093511", "E093512", "E093513", "E093519", "E093521", "E093522", "E093523", "E093529",
      "E093531", "E093532", "E093533", "E093539", "E093541", "E093542", "E093543", "E093549", "E093551", "E093552", "E093553",
      "E093559", "E09359", "E093591", "E093592", "E093593", "E093599", "E0936", "E0937X1", "E0937X2", "E0937X3", "E0937X9",
      "E0939", "E0940", "E0941", "E0942", "E0943", "E0944", "E0949", "E0951", "E0952", "E0959", "E09610",
      "E09618", "E09620", "E09621", "E09622", "E09628", "E09630", "E09638", "E09641", "E09649", "E0965", "E0969",
      "E098", "E1021", "E1022", "E1029", "E10311", "E10319", "E10321", "E103211", "E103212", "E103213", "E103219",
      "E10329", "E103291", "E103292", "E103293", "E103299", "E10331", "E103311", "E103312", "E103313", "E103319", "E10339",
      "E103391", "E103392", "E103393", "E103399", "E10341", "E103411", "E103412", "E103413", "E103419", "E10349", "E103491",
      "E103492", "E103493", "E103499", "E10351", "E103511", "E103512", "E103513", "E103519", "E103521", "E103522", "E103523",
      "E103529", "E103531", "E103532", "E103533", "E103539", "E103541", "E103542", "E103543", "E103549", "E103551", "E103552",
      "E103553", "E103559", "E10359", "E103591", "E103592", "E103593", "E103599", "E1036", "E1037X1", "E1037X2", "E1037X3",
      "E1037X9", "E1039", "E1040", "E1041", "E1042", "E1043", "E1044", "E1049", "E1051", "E1052", "E1059",
      "E10610", "E10618", "E10620", "E10621", "E10622", "E10628", "E10630", "E10638", "E10641", "E10649", "E1065",
      "E1069", "E108", "E1121", "E1122", "E1129", "E11311", "E11319", "E11321", "E113211", "E113212", "E113213",
      "E113219", "E11329", "E113291", "E113292", "E113293", "E113299", "E11331", "E113311", "E113312", "E113313", "E113319",
      "E11339", "E113391", "E113392", "E113393", "E113399", "E11341", "E113411", "E113412", "E113413", "E113419", "E11349",
      "E113491", "E113492", "E113493", "E113499", "E11351", "E113511", "E113512", "E113513", "E113519", "E113521", "E113522",
      "E113523", "E113529", "E113531", "E113532", "E113533", "E113539", "E113541", "E113542", "E113543", "E113549", "E113551",
      "E113552", "E113553", "E113559", "E11359", "E113591", "E113592", "E113593", "E113599", "E1136", "E1137X1", "E1137X2",
      "E1137X3", "E1137X9", "E1139", "E1140", "E1141", "E1142", "E1143", "E1144", "E1149", "E1151", "E1152",
      "E1159", "E11610", "E11618", "E11620", "E11621", "E11622", "E11628", "E11630", "E11638", "E11641", "E11649",
      "E1165", "E1169", "E118", "E1321", "E1322", "E1329", "E13311", "E13319", "E13321", "E133211", "E133212",
      "E133213", "E133219", "E13329", "E133291", "E133292", "E133293", "E133299", "E13331", "E133311", "E133312", "E133313",
      "E133319", "E13339", "E133391", "E133392", "E133393", "E133399", "E13341", "E133411", "E133412", "E133413", "E133419",
      "E13349", "E133491", "E133492", "E133493", "E133499", "E13351", "E133511", "E133512", "E133513", "E133519", "E133521",
      "E133522", "E133523", "E133529", "E133531", "E133532", "E133533", "E133539", "E133541", "E133542", "E133543", "E133549",
      "E133551", "E133552", "E133553", "E133559", "E13359", "E133591", "E133592", "E133593", "E133599", "E1336", "E1337X1",
      "E1337X2", "E1337X3", "E1337X9", "E1339", "E1340", "E1341", "E1342", "E1343", "E1344", "E1349", "E1351",
      "E1352", "E1359", "E13610", "E13618", "E13620", "E13621", "E13622", "E13628", "E13630", "E13638", "E13641",
      "E13649", "E1365", "E1369", "E138", "P702"="DMCX"            /*Diabetes with chronic complications*/

      "F1110", "F1111", "F11120", "F11121", "F11122", "F11129", "F1114", "F11150", "F11151", "F11159", "F11181",
      "F11182", "F11188", "F1119", "F1120", "F1121", "F11220", "F11221", "F11222", "F11229", "F1123", "F1124",
      "F11250", "F11251", "F11259", "F11281", "F11282", "F11288", "F1129", "F1210", "F1211", "F12120", "F12121",
      "F12122", "F12129", "F12150", "F12151", "F12159", "F12180", "F12188", "F1219", "F1220", "F1221", "F12220",
      "F12221", "F12222", "F12229", "F12250", "F12251", "F12259", "F12280", "F12288", "F1229", "F1310", "F1311",
      "F13120", "F13121", "F13129", "F1314", "F13150", "F13151", "F13159", "F13180", "F13181", "F13182", "F13188",
      "F1319", "F1320", "F1321", "F13220", "F13221", "F13229", "F13230", "F13231", "F13232", "F13239", "F1324",
      "F13250", "F13251", "F13259", "F1326", "F1327", "F13280", "F13281", "F13282", "F13288", "F1329", "F1410",
      "F1411", "F14120", "F14121", "F14122", "F14129", "F1414", "F14150", "F14151", "F14159", "F14180", "F14181",
      "F14182", "F14188", "F1419", "F1420", "F1421", "F14220", "F14221", "F14222", "F14229", "F1423", "F1424",
      "F14250", "F14251", "F14259", "F14280", "F14281", "F14282", "F14288", "F1429", "F1510", "F1511", "F15120",
      "F15121", "F15122", "F15129", "F1514", "F15150", "F15151", "F15159", "F15180", "F15181", "F15182", "F15188",
      "F1519", "F1520", "F1521", "F15220", "F15221", "F15222", "F15229", "F1523", "F1524", "F15250", "F15251",
      "F15259", "F15280", "F15281", "F15282", "F15288", "F1529", "F1610", "F1611", "F16120", "F16121", "F16122",
      "F16129", "F1614", "F16150", "F16151", "F16159", "F16180", "F16183", "F16188", "F1619", "F1620", "F1621",
      "F16220", "F16221", "F16229", "F1624", "F16250", "F16251", "F16259", "F16280", "F16283", "F16288", "F1629",
      "F1810", "F1811", "F18120", "F18121", "F18129", "F1814", "F18150", "F18151", "F18159", "F1817", "F18180",
      "F18188", "F1819", "F1820", "F1821", "F18220", "F18221", "F18229", "F1824", "F18250", "F18251", "F18259",
      "F1827", "F18280", "F18288", "F1829", "F1910", "F1911", "F19120", "F19121", "F19122", "F19129", "F1914",
      "F19150", "F19151", "F19159", "F1916", "F1917", "F19180", "F19181", "F19182", "F19188", "F1919", "F1920", "F1921",
      "F19220", "F19221", "F19222", "F19229", "F19230", "F19231", "F19232", "F19239", "F1924", "F19250", "F19251",
      "F19259", "F1926", "F1927", "F19280", "F19281", "F19282", "F19288", "F1929", "F550", "F551", "F552",
      "F553", "F554", "F558", "O99320", "O99321", "O99322", "O99323", "O99324", "O99325"="DRUG"          /*Drug abuse*/

      "I130"="HHRWCHF"         /**Hyp hrt & chr kdny dis w hrt fail and stg 1-4/unsp chr kdny*/
      "I132"="HHRWHRF"         /**Hyp hrt & chr kdny dis w hrt fail and w stg 5 chr kdny/ESRD*/
      "I1310"="HHRWOHRF"       /*Hyp hrt & chr kdny dis w/o hrt fail, w stg 1-4/unsp chr kdny*/
      "I1311"="HHRWRF"         /*Hyp hrt and chr kdny dis w/o hrt fail, w stg 5 chr kdny/ESRD*/

      "I129",
      "I150",
      "I151"="HRENWORF"        /*Hypertension secondary to other renal disorders*/

      "I120"="HRENWRF"         /*Hyp chr kidney disease w stage 5 chr kidney disease or ESRD*/

      "I10", "O10011", "O10012", "O10013", "O10019", "O1002", "O1003", "O10911", "O10912", "O10913", "O10919",
      "O1092", "O1093"="HTN"            /*Unsp pre-existing hypertension complicating the puerperium*/

      "I160", "I161", "I169", "I674"="HTNCX"           /*Hypertensive encephalopathy*/

      "O10111", "O10112", "O10113", "O10119", "O1012", "O1013", "O10211", "O10212", "O10213", "O10219", "O1022",
      "O1023", "O10311", "O10312", "O10313", "O10319", "O1032", "O1033", "O10411", "O10412", "O10413", "O10419",
      "O1042", "O1043", "O111", "O112", "O113", "O114", "O115", "O119"="HTNPREG"         /*Pre-existing hypertension with pre-eclampsia, unsp trimester*/

      "I110"="HTNWCHF"         /*Hypertensive heart disease with heart failure*/

      "I119",
      "I152",
      "I158",
      "I159"="HTNWOCHF"        /*Secondary hypertension, unspecified*/

      "E000", "E001", "E002", "E009", "E018", "E02", "E030", "E031", "E032", "E033", "E038",
      "E039", "E890"="HYPOTHY"         /*Hypothyroidism*/

      "B180", "B181", "B182", "I8500", "I8501", "I8510", "I8511", "K700", "K702", "K7030", "K7031",
      "K7040", "K7041", "K709", "K7210", "K7211", "K7290", "K7291", "K730", "K731", "K732", "K738",
      "K739", "K740", "K741", "K742", "K743", "K744", "K745", "K7460", "K7469", "K754", "K7581",
      "K760", "K766", "K7689", "K769", "Z944"="LIVER"           /*Liver disease*/

      "C8100", "C8101", "C8102", "C8103", "C8104", "C8105", "C8106", "C8107", "C8108", "C8109", "C8110",
      "C8111", "C8112", "C8113", "C8114", "C8115", "C8116", "C8117", "C8118", "C8119", "C8120", "C8121",
      "C8122", "C8123", "C8124", "C8125", "C8126", "C8127", "C8128", "C8129", "C8130", "C8131", "C8132",
      "C8133", "C8134", "C8135", "C8136", "C8137", "C8138", "C8139", "C8140", "C8141", "C8142", "C8143",
      "C8144", "C8145", "C8146", "C8147", "C8148", "C8149", "C8170", "C8171", "C8172", "C8173", "C8174",
      "C8175", "C8176", "C8177", "C8178", "C8179", "C8190", "C8191", "C8192", "C8193", "C8194", "C8195",
      "C8196", "C8197", "C8198", "C8199", "C8200", "C8201", "C8202", "C8203", "C8204", "C8205", "C8206",
      "C8207", "C8208", "C8209", "C8210", "C8211", "C8212", "C8213", "C8214", "C8215", "C8216", "C8217",
      "C8218", "C8219", "C8220", "C8221", "C8222", "C8223", "C8224", "C8225", "C8226", "C8227", "C8228",
      "C8229", "C8230", "C8231", "C8232", "C8233", "C8234", "C8235", "C8236", "C8237", "C8238", "C8239",
      "C8240", "C8241", "C8242", "C8243", "C8244", "C8245", "C8246", "C8247", "C8248", "C8249", "C8250",
      "C8251", "C8252", "C8253", "C8254", "C8255", "C8256", "C8257", "C8258", "C8259", "C8260", "C8261",
      "C8262", "C8263", "C8264", "C8265", "C8266", "C8267", "C8268", "C8269", "C8280", "C8281", "C8282",
      "C8283", "C8284", "C8285", "C8286", "C8287", "C8288", "C8289", "C8290", "C8291", "C8292", "C8293",
      "C8294", "C8295", "C8296", "C8297", "C8298", "C8299", "C8300", "C8301", "C8302", "C8303", "C8304",
      "C8305", "C8306", "C8307", "C8308", "C8309", "C8310", "C8311", "C8312", "C8313", "C8314", "C8315",
      "C8316", "C8317", "C8318", "C8319", "C8330", "C8331", "C8332", "C8333", "C8334", "C8335", "C8336",
      "C8337", "C8338", "C8339", "C8350", "C8351", "C8352", "C8353", "C8354", "C8355", "C8356", "C8357",
      "C8358", "C8359", "C8370", "C8371", "C8372", "C8373", "C8374", "C8375", "C8376", "C8377", "C8378",
      "C8379", "C8380", "C8381", "C8382", "C8383", "C8384", "C8385", "C8386", "C8387", "C8388", "C8389",
      "C8390", "C8391", "C8392", "C8393", "C8394", "C8395", "C8396", "C8397", "C8398", "C8399", "C8400",
      "C8401", "C8402", "C8403", "C8404", "C8405", "C8406", "C8407", "C8408", "C8409", "C8410", "C8411",
      "C8412", "C8413", "C8414", "C8415", "C8416", "C8417", "C8418", "C8419", "C8440", "C8441", "C8442",
      "C8443", "C8444", "C8445", "C8446", "C8447", "C8448", "C8449", "C8460", "C8461", "C8462", "C8463",
      "C8464", "C8465", "C8466", "C8467", "C8468", "C8469", "C8470", "C8471", "C8472", "C8473", "C8474",
      "C8475", "C8476", "C8477", "C8478", "C8479", "C8490", "C8491", "C8492", "C8493", "C8494", "C8495",
      "C8496", "C8497", "C8498", "C8499", "C84A0", "C84A1", "C84A2", "C84A3", "C84A4", "C84A5", "C84A6",
      "C84A7", "C84A8", "C84A9", "C84Z0", "C84Z1", "C84Z2", "C84Z3", "C84Z4", "C84Z5", "C84Z6", "C84Z7",
      "C84Z8", "C84Z9", "C8510", "C8511", "C8512", "C8513", "C8514", "C8515", "C8516", "C8517", "C8518",
      "C8519", "C8520", "C8521", "C8522", "C8523", "C8524", "C8525", "C8526", "C8527", "C8528", "C8529",
      "C8580", "C8581", "C8582", "C8583", "C8584", "C8585", "C8586", "C8587", "C8588", "C8589", "C8590",
      "C8591", "C8592", "C8593", "C8594", "C8595", "C8596", "C8597", "C8598", "C8599", "C860", "C861",
      "C862", "C863", "C864", "C865", "C866", "C880", "C882", "C883", "C884", "C888", "C889",
      "C9000", "C9001", "C9002", "C9010", "C9011", "C9012", "C9020", "C9021", "C9022", "C9030", "C9031",
      "C9032", "C960", "C962", "C9620", "C9621", "C9622", "C9629", "C964", "C969", "C96A", "C96Z",
      "D47Z9"="LYMPH"          /*Lymphoma*/

      "E860", "E861", "E869", "E870", "E871", "E872", "E873", "E874", "E875", "E876", "E8770",
      "E8771", "E8779", "E878"="LYTES"           /*Fluid and electrolyte disorders*/

      "C770", "C771", "C772", "C773", "C774", "C775", "C778", "C779", "C7800", "C7801", "C7802",
      "C781", "C782", "C7830", "C7839", "C784", "C785", "C786", "C787", "C7880", "C7889", "C7900",
      "C7901", "C7902", "C7910", "C7911", "C7919", "C792", "C7931", "C7932", "C7940", "C7949", "C7951",
      "C7952", "C7960", "C7961", "C7962", "C7970", "C7971", "C7972", "C7981", "C7982", "C7989", "C799",
      "C7B00", "C7B01", "C7B02", "C7B03", "C7B04", "C7B09", "C7B1", "C7B8", "C800", "C801", "R180"="METS"            /*Metastatic cancer*/

      "E7500", "E7501", "E7502", "E7509", "E7510", "E7511", "E7519", "E7523", "E7525", "E7529", "E754",
      "F842", "G10", "G110", "G111", "G112", "G113", "G114", "G118", "G119", "G120", "G121",
      "G1220", "G1221", "G1222", "G1223", "G1224", "G1225", "G1229", "G128", "G129", "G132", "G138",
      "G20", "G214", "G2401", "G2402", "G2409", "G242", "G248", "G254", "G255", "G2581", "G300",
      "G301", "G308", "G309", "G3101", "G3109", "G311", "G312", "G3181", "G3182", "G3183", "G3184",
      "G3185", "G3189", "G319", "G3281", "G35", "G361", "G368", "G369", "G370", "G371", "G372",
      "G373", "G374", "G375", "G378", "G379", "G40001", "G40009", "G40011", "G40019", "G40101", "G40109",
      "G40111", "G40119", "G40201", "G40209", "G40211", "G40219", "G40301", "G40309", "G40311", "G40319", "G40401",
      "G40409", "G40411", "G40419", "G40501", "G40509", "G40801", "G40802", "G40803", "G40804", "G40811", "G40812",
      "G40813", "G40814", "G40821", "G40822", "G40823", "G40824", "G4089", "G40901", "G40909", "G40911", "G40919",
      "G40A01", "G40A09", "G40A11", "G40A19", "G40B01", "G40B09", "G40B11", "G40B19", "G47411", "G47419", "G47421",
      "G47429", "G803", "G890", "G910", "G911", "G912", "G913", "G914", "G918", "G919", "G937",
      "G9389", "G939", "G94", "O99350", "O99351", "O99352", "O99353", "O99354", "O99355", "P9160", "P9161",
      "P9162", "P9163", "R410", "R4182", "R4701", "R5600", "R5601", "R561",
      "R569"="NEURO"           /*Other neurological disorders*/

      "E6601", "E6609", "E661", "E662", "E668", "E669", "O99210", "O99211", "O99212", "O99213", "O99214", "O99215",
      "R939", "Z6830", "Z6831", "Z6832", "Z6833", "Z6834", "Z6835", "Z6836", "Z6837", "Z6838", "Z6839",
      "Z6841", "Z6842", "Z6843", "Z6844", "Z6845",
      "Z6854"="OBESE"          /*Obesity*/

      "O161",
      "O162",
      "O163",
      "O164",
      "O165",
      "O169"="OHTNPREG"        /*Unspecified maternal hypertension, unspecified trimester*/

      "G041", "G800", "G801", "G802", "G804", "G808", "G809", "G8100", "G8101", "G8102", "G8103",
      "G8104", "G8110", "G8111", "G8112", "G8113", "G8114", "G8190", "G8191", "G8192", "G8193",
      "G8194", "G8220", "G8221", "G8222", "G8250", "G8251", "G8252", "G8253", "G8254", "G830", "G8310",
      "G8311", "G8312", "G8313", "G8314", "G8320", "G8321", "G8322", "G8323", "G8324", "G8330", "G8331",
      "G8332", "G8333", "G8334", "G834", "G835", "G8381", "G8382", "G8383", "G8384", "G8389", "G839",
      "I69031", "I69032", "I69033", "I69034", "I69039", "I69041", "I69042", "I69043", "I69044", "I69049", "I69051",
      "I69052", "I69053", "I69054", "I69059", "I69061", "I69062", "I69063", "I69064", "I69065", "I69069", "I69131",
      "I69132", "I69133", "I69134", "I69139", "I69141", "I69142", "I69143", "I69144", "I69149", "I69151", "I69152",
      "I69153", "I69154", "I69159", "I69161", "I69162", "I69163", "I69164", "I69165", "I69169", "I69231", "I69232",
      "I69233", "I69234", "I69239", "I69241", "I69242", "I69243", "I69244", "I69249", "I69251", "I69252", "I69253",
      "I69254", "I69259", "I69261", "I69262", "I69263", "I69264", "I69265", "I69269", "I69331", "I69332", "I69333",
      "I69334", "I69339", "I69341", "I69342", "I69343", "I69344", "I69349", "I69351", "I69352", "I69353", "I69354",
      "I69359", "I69361", "I69362", "I69363", "I69364", "I69365", "I69369", "I69831", "I69832", "I69833", "I69834",
      "I69839", "I69841", "I69842", "I69843", "I69844", "I69849", "I69851", "I69852", "I69853", "I69854", "I69859",
      "I69861", "I69862", "I69863", "I69864", "I69865", "I69869", "I69931", "I69932", "I69933", "I69934", "I69939",
      "I69941", "I69942", "I69943", "I69944", "I69949", "I69951", "I69952", "I69953", "I69954", "I69959", "I69961",
      "I69962", "I69963", "I69964", "I69965", "I69969",
      "R532"="PARA"            /*Paralysis*/

      "I700", "I701", "I70201", "I70202", "I70203", "I70208", "I70209", "I70211", "I70212", "I70213", "I70218",
      "I70219", "I70221", "I70222", "I70223", "I70228", "I70229", "I70231", "I70232", "I70233", "I70234", "I70235",
      "I70238", "I70239", "I70241", "I70242", "I70243", "I70244", "I70245", "I70248", "I70249", "I7025", "I70261",
      "I70262", "I70263", "I70268", "I70269", "I70291", "I70292", "I70293", "I70298", "I70299", "I70301", "I70302",
      "I70303", "I70308", "I70309", "I70311", "I70312", "I70313", "I70318", "I70319", "I70321", "I70322", "I70323",
      "I70328", "I70329", "I70331", "I70332", "I70333", "I70334", "I70335", "I70338", "I70339", "I70341", "I70342",
      "I70343", "I70344", "I70345", "I70348", "I70349", "I7035", "I70361", "I70362", "I70363", "I70368", "I70369", "I70391",
      "I70392", "I70393", "I70398", "I70399", "I70401", "I70402", "I70403", "I70408", "I70409", "I70411", "I70412", "I70413",
      "I70418", "I70419", "I70421", "I70422", "I70423", "I70428", "I70429", "I70431", "I70432", "I70433", "I70434",
      "I70435", "I70438", "I70439", "I70441", "I70442", "I70443", "I70444", "I70445", "I70448", "I70449", "I7045", "I70461",
      "I70462", "I70463", "I70468", "I70469", "I70491", "I70492", "I70493", "I70498", "I70499", "I70501", "I70502", "I70503",
      "I70508", "I70509", "I70511", "I70512", "I70513", "I70518", "I70519", "I70521", "I70522", "I70523", "I70528", "I70529",
      "I70531", "I70532", "I70533", "I70534", "I70535", "I70538", "I70539", "I70541", "I70542", "I70543", "I70544", "I70545",
      "I70548", "I70549", "I7055", "I70561", "I70562", "I70563", "I70568", "I70569", "I70591", "I70592", "I70593",
      "I70598", "I70599", "I70601", "I70602", "I70603", "I70608", "I70609", "I70611", "I70612", "I70613", "I70618", "I70619",
      "I70621", "I70622", "I70623", "I70628", "I70629", "I70631", "I70632", "I70633", "I70634", "I70635", "I70638",
      "I70639", "I70641", "I70642", "I70643", "I70644", "I70645", "I70648", "I70649", "I7065", "I70661", "I70662",
      "I70663", "I70668", "I70669", "I70691", "I70692", "I70693", "I70698", "I70699", "I70701", "I70702", "I70703",
      "I70708", "I70709", "I70711", "I70712", "I70713", "I70718", "I70719", "I70721", "I70722", "I70723", "I70728",
      "I70729", "I70731", "I70732", "I70733", "I70734", "I70735", "I70738", "I70739", "I70741", "I70742", "I70743",
      "I70744", "I70745", "I70748", "I70749", "I7075", "I70761", "I70762", "I70763", "I70768", "I70769", "I70791",
      "I70792", "I70793", "I70798", "I70799", "I708", "I7090", "I7091", "I7092", "I7100", "I7101", "I7102", "I7103",
      "I711", "I712", "I713", "I714", "I715", "I716", "I718", "I719", "I720", "I721", "I722",
      "I723", "I724", "I725", "I726", "I728", "I729", "I731", "I7381", "I7389", "I739", "I742",
      "I743", "I744", "I76", "I771", "I7770", "I7771", "I7772", "I7773", "I7774", "I7775", "I7776",
      "I7777", "I7779", "I790", "I791", "I798", "K551", "K558", "K559", "Z95820",
      "Z95828"="PERIVASC"      /*Peripheral vascular disease*/

      "F200", "F201", "F202", "F203", "F205", "F2081", "F2089", "F209", "F22", "F23", "F24",
      "F250", "F251", "F258", "F259", "F28", "F29", "F3010", "F3011", "F3012", "F3013", "F302",
      "F303", "F304", "F308", "F309", "F310", "F3110", "F3111", "F3112", "F3113", "F312", "F3130", "F3131",
      "F3132", "F314", "F315", "F3160", "F3161", "F3162", "F3163", "F3164", "F3170", "F3171", "F3172",
      "F3173", "F3174", "F3175", "F3176", "F3177", "F3178", "F3181", "F3189", "F319", "F324", "F325",
      "F3340", "F3341", "F3342", "F348", "F3481", "F3489", "F349", "F39", "F4489",
      "F843"="PSYCH"           /*Psychoses*/

      "I2601", "I2602", "I2609", "I2690", "I2692", "I2699", "I270", "I271", "I2781", "I2782", "I2783", "I2789",
      "I279", "I289", "T800XXA", "T82817A",
      "T82818A"="PULMCIRC"     /*Pulmonary circulation disorders*/

      "N183", "N184", "N185", "N186", "N189", "N19", "Z4901", "Z4902", "Z4931", "Z4932", "Z9115",
      "Z940", "Z992"="RENLFAIL"        /*Renal failure*/

      "C000", "C001", "C002", "C003", "C004", "C005", "C006", "C008", "C009", "C01", "C020",
      "C021", "C022", "C023", "C024", "C028", "C029", "C030", "C031", "C039", "C040", "C041",
      "C048", "C049", "C050", "C051", "C052", "C058", "C059", "C060", "C061", "C062", "C0680",
      "C0689", "C069", "C07", "C080", "C081", "C089", "C090", "C091", "C098", "C099", "C100",
      "C101", "C102", "C103", "C104", "C108", "C109", "C110", "C111", "C112", "C113", "C118", "C119",
      "C12", "C130", "C131", "C132", "C138", "C139", "C140", "C142", "C148", "C153", "C154",
      "C155", "C158", "C159", "C160", "C161", "C162", "C163", "C164", "C165", "C166", "C168", "C169",
      "C170", "C171", "C172", "C173", "C178", "C179", "C180", "C181", "C182", "C183", "C184",
      "C185", "C186", "C187", "C188", "C189", "C19", "C20", "C210", "C211", "C212", "C218", "C220",
      "C221", "C222", "C223", "C224", "C227", "C228", "C229", "C23", "C240", "C241", "C248", "C249",
      "C250", "C251", "C252", "C253", "C254", "C257", "C258", "C259", "C260", "C261", "C269", "C300",
      "C301", "C310", "C311", "C312", "C313", "C318", "C319", "C320", "C321", "C322", "C323",
      "C328", "C329", "C33", "C3400", "C3401", "C3402", "C3410", "C3411", "C3412", "C342", "C3430", "C3431",
      "C3432", "C3480", "C3481", "C3482", "C3490", "C3491", "C3492", "C37", "C380", "C381", "C382", "C383",
      "C384", "C388", "C390", "C399", "C4000", "C4001", "C4002", "C4010", "C4011", "C4012", "C4020",
      "C4021", "C4022", "C4030", "C4031", "C4032", "C4080", "C4081", "C4082", "C4090", "C4091", "C4092", "C410",
      "C411", "C412", "C413", "C414", "C419", "C430", "C4310", "C4311", "C4312", "C4320", "C4321",
      "C4322", "C4330", "C4331", "C4339", "C434", "C4351", "C4352", "C4359", "C4360", "C4361", "C4362",
      "C4370", "C4371", "C4372", "C438", "C439", "C450", "C451", "C452", "C457", "C470", "C4710",
      "C4711", "C4712", "C4720", "C4721", "C4722", "C473", "C474", "C475", "C476", "C478", "C479", "C480",
      "C481", "C482", "C488", "C490", "C4910", "C4911", "C4912", "C4920", "C4921", "C4922", "C493",
      "C494", "C495", "C496", "C498", "C499", "C49A0", "C49A1", "C49A2", "C49A3", "C49A4", "C49A5",
      "C49A9", "C4A0", "C4A10", "C4A11", "C4A12", "C4A20", "C4A21", "C4A22", "C4A30", "C4A31", "C4A39",
      "C4A4", "C4A51", "C4A52", "C4A59", "C4A60", "C4A61", "C4A62", "C4A70", "C4A71", "C4A72", "C4A8",
      "C4A9", "C50011", "C50012", "C50019", "C50021", "C50022", "C50029", "C50111", "C50112", "C50119", "C50121", "C50122",
      "C50129", "C50211", "C50212", "C50219", "C50221", "C50222", "C50229", "C50311", "C50312", "C50319", "C50321",
      "C50322", "C50329", "C50411", "C50412", "C50419", "C50421", "C50422", "C50429", "C50511", "C50512", "C50519", "C50521",
      "C50522", "C50529", "C50611", "C50612", "C50619", "C50621", "C50622", "C50629", "C50811", "C50812", "C50819",
      "C50821", "C50822", "C50829", "C50911", "C50912", "C50919", "C50921", "C50922", "C50929", "C510", "C511", "C512",
      "C518", "C519", "C52", "C530", "C531", "C538", "C539", "C540", "C541", "C542", "C543", "C548",
      "C549", "C55", "C561", "C562", "C569", "C5700", "C5701", "C5702", "C5710", "C5711", "C5712", "C5720",
      "C5721", "C5722", "C573", "C574", "C577", "C578", "C579", "C58", "C600", "C601", "C602", "C608",
      "C609", "C61", "C6200", "C6201", "C6202", "C6210", "C6211", "C6212", "C6290", "C6291", "C6292",
      "C6300", "C6301", "C6302", "C6310", "C6311", "C6312", "C632", "C637", "C638", "C639", "C641", "C642",
      "C649", "C651", "C652", "C659", "C661", "C662", "C669", "C670", "C671", "C672", "C673", "C674",
      "C675", "C676", "C677", "C678", "C679", "C680", "C681", "C688", "C689", "C6900", "C6901", "C6902",
      "C6910", "C6911", "C6912", "C6920", "C6921", "C6922", "C6930", "C6931", "C6932", "C6940", "C6941", "C6942",
      "C6950", "C6951", "C6952", "C6960", "C6961", "C6962", "C6980", "C6981", "C6982", "C6990", "C6991", "C6992",
      "C700", "C701", "C709", "C710", "C711", "C712", "C713", "C714", "C715", "C716", "C717", "C718",
      "C719", "C720", "C721", "C7220", "C7221", "C7222", "C7230", "C7231", "C7232", "C7240", "C7241", "C7242",
      "C7250", "C7259", "C729", "C73", "C7400", "C7401", "C7402", "C7410", "C7411", "C7412", "C7490",
      "C7491", "C7492", "C750", "C751", "C752", "C753", "C754", "C755", "C758", "C759", "C760", "C761",
      "C762", "C763", "C7640", "C7641", "C7642", "C7650", "C7651", "C7652", "C768", "C7A00", "C7A010", "C7A011",
      "C7A012", "C7A019", "C7A020", "C7A021", "C7A022", "C7A023", "C7A024", "C7A025", "C7A026", "C7A029", "C7A090", "C7A091",
      "C7A092", "C7A093", "C7A094", "C7A095", "C7A096", "C7A098", "D030", "D0310", "D0311", "D0312", "D0320", "D0321", "D0322",
      "D0330", "D0339", "D034", "D0351", "D0352", "D0359", "D0360", "D0361", "D0362", "D0370", "D0371", "D0372",
      "D038", "D039", "E3121", "E3122", "E3123"="TUMOR"          /*Solid tumor without metastasis*/

      "K254", "K255", "K256", "K257", "K259", "K264", "K265", "K266", "K267", "K269", "K274",
      "K275", "K276", "K277", "K279", "K284", "K285", "K286", "K287",
      "K289"="ULCER"           /*Chronic peptic ulcer disease*/

      "A5203", "I050", "I051", "I052", "I058", "I059", "I060", "I061", "I062", "I068", "I069", "I070",
      "I071", "I072", "I078", "I079", "I080", "I081", "I082", "I083", "I088", "I089", "I091",
      "I0989", "I340", "I341", "I342", "I348", "I349", "I350", "I351", "I352", "I358", "I359", "I360",
      "I361", "I362", "I368", "I369", "I370", "I371", "I372", "I378", "I379", "I38", "I39", "Q230",
      "Q231", "Q232", "Q233", "Z952", "Z953",
      "Z954"="VALVE"           /*Valvular disease*/

      "E40", "E41", "E42", "E43", "E440", "E441", "E45", "E46", "E640", "R634",
      "R636"="WGHTLOSS"        /*Weight loss*/

      Other = "NA"
    ;

    /***************************************************/
    /***************************************************/

    /* DRG CODE FORMATTING
    /***************************************************/

    /***************************************************/
    VALUE CARDDRG                      /* Cardiac – ICD9 and 10 */
      /* note that there are differences in ICD9 and 10 Cardiac DRG codes -
      ICD9 contains 237-238 which do not exist in ICD10; ICD10 contains
      268-274 which do not exist in ICD 9 */

      001-002, 215-236,
      237-238, 242-252,
      253-254, 258-262,
      265-267,
      280-293,
      296-298, 302-303,
      306-313 = "YES"
    ;
    VALUE PERIDRG                      /* Peripheral vascular */
      299-301 = "YES"
    ;
    VALUE RENALDRG                     /* Renal */
      652, 656-661, 673-675,
      682-700 = "YES"
    ;
    VALUE NERVDRG                      /* Nervous system */
      020-042,
      052-103 = "YES"
    ;
    VALUE CEREDRG                      /* Cerebrovascular */
      020-022, 034-039,
      064-072 = "YES"
    ;
    VALUE PULMDRG                      /* COPD asthma */
      190-192, 202-203 = "YES"
    ;
    VALUE  DIABDRG                     /* Diabetes */
      637-639 = "YES"
    ;
    VALUE HYPODRG                      /* Thyroid endocrine */
      625-627,
      643-645 = "YES"
    ;
    VALUE RENFDRG                      /* Kidney transp, renal fail/dialysis */
      652, 682-685 = "YES"
    ;
    VALUE LIVERDRG                     /* Liver */
      420-425, 432-434,
      441-446 = "YES"
    ;
    VALUE ULCEDRG                      /* GI hemorrhage or ulcer */
      377-384 = "YES"
    ;
    VALUE HIVDRG                       /* Human immunodeficiency virus */
      969-970,
      974-977 = "YES"
    ;
    VALUE LEUKDRG                      /* Leukemia/lymphoma */
      820-830,
      834-849 = "YES"
    ;
    VALUE CANCDRG                      /* Cancer, lymphoma */
      054, 055, 146-148, 180-182,
      374-376, 435-437, 542-544,
      582-585, 597-599, 656-658,
      686-688, 715-716, 722-724,
      736-741, 754-756, 826-830,
      843-849 = "YES"
    ;
    VALUE ARTHDRG                      /* Connective tissue */
      545-547 = "YES"
    ;
    VALUE NUTRDRG                      /* Nutrition/metabolic */
      640-641 = "YES"
    ;
    VALUE ANEMDRG                      /* Anemia */
      808-812 = "YES"
    ;
    VALUE ALCDRG                       /* Alcohol drug */
      894-897 = "YES"
    ;
    VALUE COAGDRG                      /*Coagulation disorders*/
      813 = "YES"
    ;
    VALUE HTNCXDRG                     /*Hypertensive Complicated  */
      077,078,304 = "YES"
    ;
    VALUE HTNDRG                       /*Hypertensive Uncomplicated  */
      079,305 = "YES"
    ;
    VALUE PSYDRG                       /* Psychoses */
      885     = "YES"
    ;
    VALUE OBESEDRG                     /* Obesity */
      619-621 = "YES"
    ;
    VALUE DEPRSDRG                     /* Depressive Neuroses */
      881     = "YES"
    ;
    /***************************************************/
    /***************************************************/

    /* ICD 9 DIAGNOSIS CODE FORMATTING
    /***************************************************/

    /***************************************************/
    VALUE $RCOMFMTnine

      "39891",
      "4280 "-"4289 " = "CHF"       /* Congestive heart failure */

      "09320"-"09324",
      "3940 "-"3971 ",
      "3979 ",
      "4240 "-"42499",
      "7463 "-"7466 ",
      "V422 ",
      "V433 "         = "VALVE"     /* Valvular disease */

      "41511"-"41519",
      "4160 "-"4169 ",
      "4179 "         = "PULMCIRC"  /* Pulmonary circulation disorder */

      "4400 "-"4409 ",
      "44100"-"4419 ",
      "4420 "-"4429 ",
      "4431 "-"4439 ",
      "44421"-"44422",
      "4471 ",
      "449  ",
      "5571 ",
      "5579 ",
      "V434 "         = "PERIVASC"  /* Peripheral vascular disorder */

      "4011 ",
      "4019 ",
      "64200"-"64204" = "HTN"       /* Hypertension, uncomplicated */

      "4010 ",
      "4372 "         = "HTNCX"     /* Hypertension, complicated */

      /******************************************************************/
      /* The following are special, temporary formats used in the       */
      /* creation of the hypertension complicated comorbidity when      */
      /* overlapping with congestive heart failure or renal failure     */
      /* occurs. These temporary formats are referenced in the program  */
      /* called comoanaly2009.txt.                                      */
      /******************************************************************/
      "64220"-"64224" = "HTNPREG"   /* Pre-existing hypertension complicating pregnancy */

      "40200",
      "40210",
      "40290",
      "40509",
      "40519",
      "40599"         = "HTNWOCHF"  /* Hypertensive heart disease without heart failure */

      "40201",
      "40211",
      "40291"         = "HTNWCHF"   /* Hypertensive heart disease with heart failure */

      "40300",
      "40310",
      "40390",
      "40501",
      "40511",
      "40591",
      "64210"-"64214" = "HRENWORF"  /* Hypertensive renal disease without renal failure */

      "40301",
      "40311",
      "40391"         = "HRENWRF"   /* Hypertensive renal disease with renal failure */

      "40400",
      "40410",
      "40490"         = "HHRWOHRF"  /* Hypertensive heart and renal disease without heart or renal failure */

      "40401",
      "40411",
      "40491"         = "HHRWCHF"   /* Hypertensive heart and renal disease with heart failure */

      "40402",
      "40412",
      "40492"         = "HHRWRF"    /* Hypertensive heart and renal disease with renal failure */

      "40403",
      "40413",
      "40493"         = "HHRWHRF"   /* Hypertensive heart and renal disease with heart and renal failure */

      "64270"-"64274",
      "64290"-"64294" = "OHTNPREG"  /* Other hypertension in pregnancy */

      /******************** End Temporary Formats ***********************/

      "3420 "-"3449 ",
      "43820"-"43853",
      "78072"         = "PARA"      /* Paralysis */

      "3300 "-"3319 ",
      "3320 ",
      "3334 ",
      "3335 ",
      "3337 ",
      "33371","33372","33379","33385","33394",
      "3340 "-"3359 ",
      "3380 ",
      "340  ",
      "3411 "-"3419 ",
      "34500"-"34511",
      "3452 "-"3453 ",
      "34540"-"34591",
      "34700"-"34701",
      "34710"-"34711",
      "64940"-"64944",
      "7687 ",
      "76870"-"76873",
      "7803 ",
      "78031",
      "78032",
      "78033",
      "78039",
      "78097",
      "7843 "         = "NEURO"     /* Other neurological */

      "490  "-"4928 ",
      "49300"-"49392",
      "494  "-"4941 ",
      "4950 "-"505  ",
      "5064 "         = "CHRNLUNG"  /* Chronic pulmonary disease */

      "25000"-"25033",
      "64800"-"64804",
      "24900"-"24931" = "DM"        /* Diabetes w/o chronic complications*/

      "25040"-"25093",
      "7751 ",
      "24940"-"24991" = "DMCX"      /* Diabetes w/ chronic complications */

      "243  "-"2442 ",
      "2448 ",
      "2449 "         = "HYPOTHY"   /* Hypothyroidism */

      "5853 ",
      "5854 ",
      "5855 ",
      "5856 ",
      "5859 ",
      "586  ",
      "V420 ",
      "V451 ",
      "V560 "-"V5632",
      "V568 ",
      "V4511"-"V4512" = "RENLFAIL"  /* Renal failure */

      "07022",
      "07023",
      "07032",
      "07033",
      "07044",
      "07054",
      "4560 ",
      "4561 ",
      "45620",
      "45621",
      "5710 ",
      "5712 ",
      "5713 ",
      "57140"-"57149",
      "5715 ",
      "5716 ",
      "5718 ",
      "5719 ",
      "5723 ",
      "5728 ",
      "5735 ",
      "V427 "         = "LIVER"     /* Liver disease */

      "53141",
      "53151",
      "53161",
      "53170",
      "53171",
      "53191",
      "53241",
      "53251",
      "53261",
      "53270",
      "53271",
      "53291",
      "53341",
      "53351",
      "53361",
      "53370",
      "53371",
      "53391",
      "53441",
      "53451",
      "53461",
      "53470",
      "53471",
      "53491"         = "ULCER"     /* Chronic Peptic ulcer disease (includes bleeding only if obstruction is also present) */

      "042  "-"0449 " = "AIDS"      /* HIV and AIDS */

      "20000"-"20238",
      "20250"-"20301",
      "2386 ",
      "2733 ",
      "20302"-"20382" = "LYMPH"     /* Lymphoma */

      "1960 "-"1991 ",
      "20970"-"20975",
      "20979",
      "78951"         = "METS"      /* Metastatic cancer */

      "1400 "-"1729 ",
      "1740 "-"1759 ",
      "179  "-"1958 ",
      "20900"-"20924",
      "20925"-"2093 ",
      "20930"-"20936",
      "25801"-"25803" = "TUMOR"     /* Solid tumor without metastasis */

      "7010 ",
      "7100 "-"7109 ",
      "7140 "-"7149 ",
      "7200 "-"7209 ",
      "725  " = "ARTH"              /* Rheumatoid arthritis/collagen vascular diseases */

      "2860 "-"2869 ",
      "2871 ",
      "2873 "-"2875 ",
      "64930"-"64934",
      "28984"         = "COAG"      /* Coagulation deficiency - note:
      this comorbidity should be dropped when
      used with the AHRQ Patient Safety Indicators */

      "2780 ",
      "27800",
      "27801",
      "27803",
      "64910"-"64914",
      "V8530"-"V8539",
      "V8541"-"V8545",
      "V8554",
      "79391"         = "OBESE"     /* Obesity      */

      "260  "-"2639 ",
      "78321"-"78322" = "WGHTLOSS"  /* Weight loss */

      "2760 "-"2769 " = "LYTES"     /* Fluid and electrolyte disorders - note:
      this comorbidity should be dropped when
      used with the AHRQ Patient Safety Indicators*/

      "2800 ",
      "64820"-"64824" = "BLDLOSS"   /* Blood loss anemia */

      "2801 "-"2819 ",
      "28521"-"28529",
      "2859 "         = "ANEMDEF"  /* Deficiency anemias */

      "2910 "-"2913 ",
      "2915 ",
      "2918 ",
      "29181",
      "29182",
      "29189",
      "2919 ",
      "30300"-"30393",
      "30500"-"30503" = "ALCOHOL"   /* Alcohol abuse */

      "2920 ",
      "29282"-"29289",
      "2929 ",
      "30400"-"30493",
      "30520"-"30593",
      "64830"-"64834" = "DRUG"      /* Drug abuse */

      "29500"-"2989 ",
      "29910",
      "29911"         = "PSYCH"    /* Psychoses */

      "3004 ",
      "30112",
      "3090 ",
      "3091 ",
      "311  "         = "DEPRESS"  /* Depression */
      Other   = "NA"
    ;
  RUN;

  /****************************************************/
  /* STEP 1 - Setting up patient/dxs table      *******/
  /****************************************************/
  * creating a cohort table that contains the mrn and index date variables;
  proc sort nodupkey data = &inputds out = cohort ;
    by &mrn &index_date ;
  run ;

  * this is where the encounter types to be included gets specified;
  %if       &inpatonly = I %then %let inpatout= AND e.EncType in ('IP');
  %else %if &inpatonly = B %then %let inpatout= AND e.EncType in ('IP','AV');
  %else %if &inpatonly = A %then %let inpatout=;
  %else %if &inpatonly = C %then %let inpatout= AND e.EncType in (&enctype_list);

  * using the cohort ds to select encounters and diagnoses;
  proc sql noprint ;
    create table enc as
      select distinct
        c.&mrn as mrn
        , c.&index_date as index_dt
        , e.adate as enc_dt
        , e.enc_id
        , e.drg_version
        , e.drg_value
        , e.enctype
      from  &_VDW_utilization e
      INNER JOIN cohort c
      on    e.mrn = c.&mrn and
            e.adate between c.&index_date - &days_lookback and c.&index_date
            &inpatout
      order by c.&mrn , c.&index_date, enc_id, drg_value
    ;

  quit;

  proc sql;
    create table dx as
      select distinct
        c.&mrn as mrn
        , c.&index_date as index_dt
        , d.dx
        , d.dx_codetype
        , d.enc_id
    from &_VDW_dx d
    INNER JOIN cohort c
      on    d.mrn = c.&mrn and
            d.adate between c.&index_date - &days_lookback and c.&index_date
      order by c.&mrn , c.&index_date, enc_id
    ;
    create table encdx as
      select distinct
        e.*
        , d.dx
        , d.dx_codetype
      from enc e
      LEFT JOIN dx d
      on  e.enc_id = d.enc_id
      order by e.mrn , e.index_dt, enc_id, drg_value
    ;
    drop table dx ;
  quit;

  /****************************************/
  /* STEP 2 - Formatting diagnoses  *******/
  /****************************************/
  /* formatting diagnoses based on HCUP definitions of ICD9 and 10 codes;*/
  data encdx2;
    set encdx;

    if dx ne '';

    if dx_codetype = '09' then
      do;
        diagnosis = put(compress(dx,'.'),$RCOMFMTnine.);
      end;
    else if dx_codetype = '10' then
      do;
        diagnosis = put(compress(dx,'.'),$RCOMFMT.);
      end;

    mrn_indexdt = catx('_',mrn,index_dt);

    keep mrn index_dt enc_id drg_value drg_version diagnosis dx dx_codetype mrn_indexdt enc_dt;

    if diagnosis not in ('', 'NA');
  run;

  %removedset(dset = encdx) ;

  /************************************************/
  /* STEP 3 - Counting diagnoses by enc_id  *******/
  /************************************************/
  * We will use these count values when creating the arrays for calculating Elixhauser;
  proc sort data=encdx2 (keep = enc_id diagnosis dx) out=encdxuniq1 nodupkey;
    by enc_id dx;
  run;

  proc sql noprint ;
    create table temp_dx as
      select enc_id , count(diagnosis) as dxcount
        from encdxuniq1
          group by 1;
    select max(dxcount) into :maxdxcount trimmed
      from temp_dx
    ;
    select compress('col'||put(max(dxcount),8.)) into :dxmax
      from temp_dx
    ;
  quit;

  /***************************************************/
  /* STEP 4 - Transposing diagnoses by enc_id  *******/
  /***************************************************/
  * This gives us one row per encounter with all relevant diagnoses and drg values;
  proc transpose data=encdx2 out= transdxs;
    var diagnosis;
    by mrn index_dt enc_id drg_value drg_version mrn_indexdt;
  run;

  %removedset(dset = encdx2) ;

  * Suggested by Dustin Hartzel, who had an issue w/the sort order. ;
  proc sort data = transdxs;
    by mrn_indexdt;
  run ;

  /***************************************************************************/
  /* STEP 5 - Scanning diagnoses and DRGs to assign comorbidity flags  *******/
  /***************************************************************************/
  DATA elix;
    RETAIN
      a_CHF
      a_VALVE
      a_PULMCIRC
      a_PERIVASC
      a_DEPRESS
      a_PARA
      a_NEURO
      a_CHRNLUNG
      a_DM
      a_DMCX
      a_HYPOTHY
      a_RENLFAIL
      a_LIVER
      a_ULCER
      a_AIDS
      a_LYMPH
      a_METS
      a_TUMOR
      a_ARTH
      a_COAG
      a_OBESE
      a_WGHTLOSS
      a_LYTES
      a_BLDLOSS
      a_ANEMDEF
      a_ALCOHOL
      a_DRUG
      a_PSYCH
      a_HTN_C
      b_htn
      b_htncx
    ;
    SET transdxs;
    BY mrn_indexdt;
    call missing(cardflg
                , periflg
                , cereflg
                , nervflg
                , pulmflg
                , diabflg
                , hypoflg
                , renalflg
                , renfflg
                , liverflg
                , ulceflg
                , hivflg
                , leukflg
                , cancflg
                , arthflg
                , nutrflg
                , anemflg
                , alcflg
                , htncxflg
                , htnflg
                , coagflg
                , psyflg
                , obeseflg
                , deprsflg)
    ;
    KEEP mrn index_dt a_: b_:;

    /*****************************************/
    /*  Declare variables as array elements  */
    /*****************************************/
    * This array holds our diagnoses;
    ARRAY COL (*)  COL1 - &dxmax;
    ARRAY COM1 (30)  CHF      VALVE    PULMCIRC PERIVASC
      HTN      HTNCX    PARA     NEURO    CHRNLUNG
      DM       DMCX     HYPOTHY  RENLFAIL LIVER
      ULCER    AIDS     LYMPH    METS     TUMOR
      ARTH     COAG     OBESE    WGHTLOSS LYTES
      BLDLOSS  ANEMDEF  ALCOHOL  DRUG     PSYCH
      DEPRESS;
    LENGTH   CHF      VALVE    PULMCIRC PERIVASC
      HTN      HTNCX    PARA     NEURO    CHRNLUNG
      DM       DMCX     HYPOTHY  RENLFAIL LIVER
      ULCER    AIDS     LYMPH    METS     TUMOR
      ARTH     COAG     OBESE    WGHTLOSS LYTES
      BLDLOSS  ANEMDEF  ALCOHOL  DRUG     PSYCH
      DEPRESS 3;

    * set initial values of COMnx and the a_ summary vars to 0;
    If first.mrn_indexdt then DO;
      DO I = 1 TO dim(com1);
        COM1(I) = 0;
      END;
      * Note that b_HTN and b_HTNCX contribute to a_HTN_C but are not included alone in the Elix score;
      a_CHF      = 0;
      a_VALVE    = 0;
      a_PULMCIRC = 0;
      a_PERIVASC = 0;
      a_DEPRESS  = 0;
      b_HTN      = 0;
      b_HTNCX    = 0;
      a_PARA     = 0;
      a_NEURO    = 0;
      a_CHRNLUNG = 0;
      a_DM       = 0;
      a_DMCX     = 0;
      a_HYPOTHY  = 0;
      a_RENLFAIL = 0;
      a_LIVER    = 0;
      a_ULCER    = 0;
      a_AIDS     = 0;
      a_LYMPH    = 0;
      a_METS     = 0;
      a_TUMOR    = 0;
      a_ARTH     = 0;
      a_COAG     = 0;
      a_OBESE    = 0;
      a_WGHTLOSS = 0;
      a_LYTES    = 0;
      a_BLDLOSS  = 0;
      a_ANEMDEF  = 0;
      a_ALCOHOL  = 0;
      a_DRUG     = 0;
      a_PSYCH    = 0;
      a_HTN_C    = 0;
    END;

    /***********************************************/
    /* STEP 6 - DEFINING COMORBIDITY GROUPS  *******/
    /***********************************************/
    /***************************************************/
    /* Looking at the secondary DXs and using formats, */
    /* create DXVALUE to define each comorbidity group */
    /*                                                 */
    /* If DXVALUE is equal to the comorbidity name     */
    /* then a value of 1 is assigned to the            */
    /* corresponding comorbidity group in array COM1   */
    /***************************************************/
    HTNPREG_  = 0;
    HTNWOCHF_ = 0;
    HTNWCHF_  = 0;
    HRENWORF_ = 0;
    HRENWRF_  = 0;
    HHRWOHRF_ = 0;
    HHRWCHF_  = 0;
    HHRWRF_   = 0;
    HHRWHRF_  = 0;
    OHTNPREG_ = 0;

    DO I = 1 TO &maxdxcount;
      select(col(i)) ;
        when('CHF'      ) CHF       = 1 ;
        when('VALVE'    ) VALVE     = 1 ;
        when('PULMCIRC' ) PULMCIRC  = 1 ;
        when('PERIVASC' ) PERIVASC  = 1 ;
        when('HTN'      ) HTN       = 1 ;
        when('HTNCX'    ) HTNCX     = 1 ;
        when('PARA'     ) PARA      = 1 ;
        when('NEURO'    ) NEURO     = 1 ;
        when('CHRNLUNG' ) CHRNLUNG  = 1 ;
        when('DM'       ) DM        = 1 ;
        when('DMCX'     ) DMCX      = 1 ;
        when('HYPOTHY'  ) HYPOTHY   = 1 ;
        when('RENLFAIL' ) RENLFAIL  = 1 ;
        when('LIVER'    ) LIVER     = 1 ;
        when('ULCER'    ) ULCER     = 1 ;
        when('AIDS'     ) AIDS      = 1 ;
        when('LYMPH'    ) LYMPH     = 1 ;
        when('METS'     ) METS      = 1 ;
        when('TUMOR'    ) TUMOR     = 1 ;
        when('ARTH'     ) ARTH      = 1 ;
        when('COAG'     ) COAG      = 1 ;
        when('OBESE'    ) OBESE     = 1 ;
        when('WGHTLOSS' ) WGHTLOSS  = 1 ;
        when('LYTES'    ) LYTES     = 1 ;
        when('BLDLOSS'  ) BLDLOSS   = 1 ;
        when('ANEMDEF'  ) ANEMDEF   = 1 ;
        when('ALCOHOL'  ) ALCOHOL   = 1 ;
        when('DRUG'     ) DRUG      = 1 ;
        when('PSYCH'    ) PSYCH     = 1 ;
        when('DEPRESS'  ) DEPRESS   = 1 ;
        /*********************************************/
        /* Create detailed hypertension flags that   */
        /* cover combinations of Congestive Heart    */
        /* Failure, Hypertension Complicated, and    */
        /* Renal Failure. These will be used in con- */
        /* junction with DRG values to set the HTNCX,*/
        /* CHF, and RENLFAIL comorbidities.          */
        /*********************************************/
        WHEN ("HTNPREG")     HTNPREG_  = 1;
        WHEN ("HTNWOCHF")    HTNWOCHF_ = 1;
        WHEN ("HTNWCHF")     HTNWCHF_  = 1;
        WHEN ("HRENWORF")    HRENWORF_ = 1;
        WHEN ("HRENWRF")     HRENWRF_  = 1;
        WHEN ("HHRWOHRF")    HHRWOHRF_ = 1;
        WHEN ("HHRWCHF")     HHRWCHF_  = 1;
        WHEN ("HHRWRF")      HHRWRF_   = 1;
        WHEN ("HHRWHRF")     HHRWHRF_  = 1;
        WHEN ("OHTNPREG")    OHTNPREG_ = 1;
        otherwise ; * <-- do nothing ;
      end ;
    END;

    /*******************************************/
    /* Initialize Hypertension, CHF, and Renal */
    /* Comorbidity flags to 1 using the detail */
    /* hypertension flags.                     */
    /*******************************************/
    IF HTNPREG_  THEN HTNCX = 1;
    IF HTNWOCHF_ THEN HTNCX = 1;
    IF HRENWORF_ THEN HTNCX = 1;
    IF HHRWOHRF_ THEN HTNCX = 1;
    IF OHTNPREG_ THEN HTNCX = 1;

    IF HTNWCHF_ THEN
      DO;
        HTNCX    = 1;
        CHF      = 1;
      END;


    IF HRENWRF_ THEN
      DO;
        HTNCX    = 1;
        RENLFAIL = 1;
      END;


    IF HHRWCHF_ THEN
      DO;
        HTNCX    = 1;
        CHF      = 1;
      END;

    IF HHRWRF_ THEN
      DO;
        HTNCX    = 1;
        RENLFAIL = 1;
      END;

    IF HHRWHRF_ THEN
      DO;
        HTNCX    = 1;
        CHF      = 1;
        RENLFAIL = 1;
      END;


    /******************************************************/
    /* Examine DRG and set flags to identify a particular */
    /* DRG group if drg_restriction  = Y                  */
    /******************************************************/
    %IF %upcase(&drg_restriction)  = Y or %upcase(&drg_restriction) = 'Y' %then %do;
      if coalescec(drg_version, 'B') ne 'B' then do ;
        put 'ERROR: non-MS-DRG type of DRG found--cannot do DRG restriction on this encounter.' drg_version= ;
        _error_ + 1 ;
      end ;
      else do ;
        * note that CARDDRG is the only DRG flag that has different values in
              ICD9 and ICD10 - all others are the same.  We are able to use the same
              format though because there is no overlap and no exclusion of codes from
              ICD9 to 10;
        IF PUT(INPUT(DRG_VALUE,4.),CARDDRG.)  = 'YES' THEN CARDFLG  = 1;
        * all other DRG codes are the same for ICD 9 and 10;
        IF PUT(INPUT(DRG_VALUE,4.),PERIDRG.)  = 'YES' THEN PERIFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),CEREDRG.)  = 'YES' THEN CEREFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),NERVDRG.)  = 'YES' THEN NERVFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),PULMDRG.)  = 'YES' THEN PULMFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),DIABDRG.)  = 'YES' THEN DIABFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),HYPODRG.)  = 'YES' THEN HYPOFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),RENALDRG.) = 'YES' THEN RENALFLG = 1;
        IF PUT(INPUT(DRG_VALUE,4.),RENFDRG.)  = 'YES' THEN RENFFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),LIVERDRG.) = 'YES' THEN LIVERFLG = 1;
        IF PUT(INPUT(DRG_VALUE,4.),ULCEDRG.)  = 'YES' THEN ULCEFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),HIVDRG.)   = 'YES' THEN HIVFLG   = 1;
        IF PUT(INPUT(DRG_VALUE,4.),LEUKDRG.)  = 'YES' THEN LEUKFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),CANCDRG.)  = 'YES' THEN CANCFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),ARTHDRG.)  = 'YES' THEN ARTHFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),NUTRDRG.)  = 'YES' THEN NUTRFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),ANEMDRG.)  = 'YES' THEN ANEMFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),ALCDRG.)   = 'YES' THEN ALCFLG   = 1;
        IF PUT(INPUT(DRG_VALUE,4.),HTNCXDRG.) = 'YES' THEN HTNCXFLG = 1;
        IF PUT(INPUT(DRG_VALUE,4.),HTNDRG.)   = 'YES' THEN HTNFLG   = 1;
        IF PUT(INPUT(DRG_VALUE,4.),COAGDRG.)  = 'YES' THEN COAGFLG  = 1;
        IF PUT(INPUT(DRG_VALUE,4.),PSYDRG.)   = 'YES' THEN PSYFLG   = 1;
        IF PUT(INPUT(DRG_VALUE,4.),OBESEDRG.) = 'YES' THEN OBESEFLG = 1;
        IF PUT(INPUT(DRG_VALUE,4.),DEPRSDRG.) = 'YES' THEN DEPRSFLG = 1;
      END;
    %end ;
    /************************************************************/
    /* Redefining comorbidities by eliminating the DRG directly */
    /* related to comorbidity, thus limiting the screens to     */
    /* principal diagnoses not directly related to comorbidity  */
    /* in question                                              */
    /************************************************************/
    IF CARDFLG THEN
      DO;
        CHF = 0;
        VALVE = 0;
      END;

    IF PULMCIRC AND ( CARDFLG OR PULMFLG ) THEN PULMCIRC = 0;
    IF PERIVASC AND PERIFLG THEN PERIVASC = 0;
    IF HTN AND HTNFLG THEN HTN = 0;

    /**********************************************************/
    /* Apply DRG Exclusions to Hypertension Complicated, Con- */
    /* gestive Heart Failure, and Renal Failure comorbidities */
    /* using the detailed hypertension flags created above.   */
    /**********************************************************/
    IF HTNCX     AND HTNCXFLG THEN HTNCX = 0;
    IF HTNPREG_  AND HTNCXFLG THEN HTNCX = 0;
    IF HTNWOCHF_ AND (HTNCXFLG OR CARDFLG) THEN HTNCX = 0;
    IF HTNWCHF_ THEN
      DO;
        IF HTNCXFLG THEN HTNCX  = 0;
        IF CARDFLG THEN
          DO;
            HTNCX = 0;
            CHF   = 0;
          END;
      END;

    IF HRENWORF_ AND (HTNCXFLG OR RENALFLG) THEN HTNCX = 0;
    IF HRENWRF_ THEN
      DO;
        IF HTNCXFLG THEN HTNCX = 0;
        IF RENALFLG THEN
          DO;
            HTNCX    = 0;
            RENLFAIL = 0;
          END;
      END;

    IF HHRWOHRF_ AND (HTNCXFLG OR CARDFLG OR RENALFLG) THEN HTNCX = 0;
    IF HHRWCHF_ THEN
      DO;
        IF HTNCXFLG THEN HTNCX = 0;
        IF CARDFLG THEN
          DO;
            HTNCX = 0;
            CHF   = 0;
          END;
        IF RENALFLG THEN HTNCX = 0;
      END;

    IF HHRWRF_ THEN
      DO;
        IF HTNCXFLG OR CARDFLG THEN HTNCX = 0;
        IF RENALFLG THEN
          DO;
            HTNCX    = 0;
            RENLFAIL = 0;
          END;
      END;

    IF HHRWHRF_ THEN
      DO;
        IF HTNCXFLG THEN HTNCX = 0;
        IF CARDFLG THEN
          DO;
            HTNCX = 0;
            CHF   = 0;
          END;
        IF RENALFLG THEN
          DO;
            HTNCX    = 0;
            RENLFAIL = 0;
          END;
      END;

    IF OHTNPREG_ AND (HTNCXFLG OR CARDFLG OR RENALFLG) THEN HTNCX = 0;
    IF NEURO AND NERVFLG THEN  NEURO = 0;
    IF CHRNLUNG AND PULMFLG THEN CHRNLUNG = 0;
    IF DM AND DIABFLG THEN DM = 0;
    IF DMCX AND DIABFLG THEN DMCX = 0;
    IF HYPOTHY AND HYPOFLG THEN HYPOTHY = 0;
    IF RENLFAIL AND RENFFLG THEN RENLFAIL = 0;
    IF LIVER AND LIVERFLG THEN LIVER = 0;
    IF ULCER AND ULCEFLG THEN ULCER = 0;
    IF AIDS AND HIVFLG THEN AIDS = 0;
    IF LYMPH AND LEUKFLG THEN LYMPH = 0;
    IF METS AND CANCFLG THEN METS = 0;
    IF TUMOR AND CANCFLG THEN TUMOR = 0;
    IF ARTH AND ARTHFLG THEN ARTH = 0;
    IF COAG AND COAGFLG THEN COAG = 0;
    IF OBESE AND (NUTRFLG OR OBESEFLG) THEN OBESE = 0;
    IF WGHTLOSS AND NUTRFLG THEN WGHTLOSS = 0;
    IF LYTES AND NUTRFLG THEN LYTES = 0;
    IF BLDLOSS AND ANEMFLG THEN BLDLOSS = 0;
    IF ANEMDEF AND ANEMFLG THEN ANEMDEF = 0;
    IF ALCOHOL AND ALCFLG THEN ALCOHOL = 0;
    IF DRUG AND ALCFLG THEN DRUG = 0;
    IF PSYCH AND PSYFLG THEN PSYCH = 0;
    IF DEPRESS AND DEPRSFLG THEN DEPRESS = 0;
    IF PARA AND CEREFLG THEN PARA = 0;
    /*************************************/
    /*  Combine HTN and HTNCX into HTN_C */
    /*************************************/
    ATTRIB a_HTN_C LENGTH=3 LABEL='Hypertension';

    IF HTN=1 OR HTNCX=1 THEN HTN_C=1;
    ELSE HTN_C=0;

    /***********************************************************************************/
    /* STEP 7 - Adding flags from each mrn_index_dt_encID into summary condition vars  */
    /***********************************************************************************/
    if a_CHF                = 0  and CHF      = 1 then a_CHF      = 1 ;
    if a_VALVE              = 0  and VALVE    = 1 then a_VALVE    = 1 ;
    if a_PULMCIRC           = 0  and PULMCIRC = 1 then a_PULMCIRC = 1 ;
    if a_PERIVASC           = 0  and PERIVASC = 1 then a_PERIVASC = 1 ;
    if a_DEPRESS            = 0  and DEPRESS  = 1 then a_DEPRESS  = 1 ;
    if coalesce(b_HTN, 0)   = 0  and HTN      = 1 then b_HTN      = 1 ;
    if coalesce(b_HTNCX, 0) = 0  and HTNCX    = 1 then b_HTNCX    = 1 ;
    if a_HTN_C              = 0  and HTN_C    = 1 then a_HTN_C    = 1 ;
    if a_PARA               = 0  and PARA     = 1 then a_PARA     = 1 ;
    if a_NEURO              = 0  and NEURO    = 1 then a_NEURO    = 1 ;
    if a_CHRNLUNG           = 0  and CHRNLUNG = 1 then a_CHRNLUNG = 1 ;
    if a_DM                 = 0  and DM       = 1 then a_DM       = 1 ;
    if a_DMCX               = 0  and DMCX     = 1 then a_DMCX     = 1 ;
    if a_HYPOTHY            = 0  and HYPOTHY  = 1 then a_HYPOTHY  = 1 ;
    if a_RENLFAIL           = 0  and RENLFAIL = 1 then a_RENLFAIL = 1 ;
    if a_LIVER              = 0  and LIVER    = 1 then a_LIVER    = 1 ;
    if a_ULCER              = 0  and ULCER    = 1 then a_ULCER    = 1 ;
    if a_AIDS               = 0  and AIDS     = 1 then a_AIDS     = 1 ;
    if a_LYMPH              = 0  and LYMPH    = 1 then a_LYMPH    = 1 ;
    if a_METS               = 0  and METS     = 1 then a_METS     = 1 ;
    if a_TUMOR              = 0  and TUMOR    = 1 then a_TUMOR    = 1 ;
    if a_ARTH               = 0  and ARTH     = 1 then a_ARTH     = 1 ;
    if a_COAG               = 0  and COAG     = 1 then a_COAG     = 1 ;
    if a_OBESE              = 0  and OBESE    = 1 then a_OBESE    = 1 ;
    if a_WGHTLOSS           = 0  and WGHTLOSS = 1 then a_WGHTLOSS = 1 ;
    if a_LYTES              = 0  and LYTES    = 1 then a_LYTES    = 1 ;
    if a_BLDLOSS            = 0  and BLDLOSS  = 1 then a_BLDLOSS  = 1 ;
    if a_ANEMDEF            = 0  and ANEMDEF  = 1 then a_ANEMDEF  = 1 ;
    if a_ALCOHOL            = 0  and ALCOHOL  = 1 then a_ALCOHOL  = 1 ;
    if a_DRUG               = 0  and DRUG     = 1 then a_DRUG     = 1 ;
    if a_PSYCH              = 0  and PSYCH    = 1 then a_PSYCH    = 1 ;

    LABEL
      b_HTNCX    = 'Hypertension w/chronic complications'
      b_HTN      = 'Hypertension w/o chronic complications'
      a_CHF      = 'Congestive heart failure'
      a_VALVE    = 'Valvular disease'
      a_PULMCIRC = 'Pulmonary circulation disease'
      a_PERIVASC = 'Peripheral vascular disease'
      a_PARA     = 'Paralysis'
      a_NEURO    = 'Other neurological disorders'
      a_CHRNLUNG = 'Chronic pulmonary disease'
      a_DM       = 'Diabetes w/o chronic complications'
      a_DMCX     = 'Diabetes w/ chronic complications'
      a_HYPOTHY  = 'Hypothyroidism'
      a_RENLFAIL = 'Renal failure'
      a_LIVER    = 'Liver disease'
      a_ULCER    = 'Peptic ulcer Disease x bleeding'
      a_AIDS     = 'Acquired immune deficiency syndrome'
      a_LYMPH    = 'Lymphoma'
      a_METS     = 'Metastatic cancer'
      a_TUMOR    = 'Solid tumor w/out metastasis'
      a_ARTH     = 'Rheumatoid arthritis/collagen vas'
      a_COAG     = 'Coagulopthy'
      a_OBESE    = 'Obesity'
      a_WGHTLOSS = 'Weight loss'
      a_LYTES    = 'Fluid and electrolyte disorders'
      a_BLDLOSS  = 'Chronic blood loss anemia'
      a_ANEMDEF  = 'Deficiency Anemias'
      a_ALCOHOL  = 'Alcohol abuse'
      a_DRUG     = 'Drug abuse'
      a_PSYCH    = 'Psychoses'
      a_DEPRESS  = 'Depression'
    ;

    /*********************************************************/
    /* Keep last mrn - index_dt row only */
    /*********************************************************/
    If last.mrn_indexdt;

    /*********************************************************/
    /* Set up code to only count the more severe comorbidity */
    /*********************************************************/

    * This comes straight from Dr. Elixhauser herself - note that other
     comorbidity std macros do not do this (that I know of);

    * had to move this to the end to account for each index date if
     there are multiple encounters per mrn-index_dt.;
    IF HTNCX = 1 THEN HTN = 0;
    IF METS  = 1 THEN TUMOR = 0;
    IF DMCX  = 1 THEN DM = 0;
  run;

  data cci;
    set elix;

    /* SumElix = straight sum of comorbidity flags - with and without cancer */
    SumElixWoCancer = sum(of a_:) - a_METS - a_TUMOR;
    SumElixWCancer = sum(of a_:);

    /* WghtElix = weighted sum of comorbidity flags - with and without cancer */
    /* uses vanWalraven's weights from the 2009 MedCare paper */

    /* hypertension, diabetes (complicated and uncomplicated), hypothyroidism,
        peptic ulcer disease, AIDS/HIV, rheumatoid arthritis, alcohol abuse,
        and psychosis have weights of 0 and are not included below.*/
    WghtElixWCancer =
      ( 7 * a_CHF)     +  (-1 * a_VALVE)   +  ( 4 * a_PULMCIRC) +   ( 2 * a_PERIVASC) +
      ( 7 * a_PARA)    +  ( 6 * a_NEURO)   +  ( 3 * a_CHRNLUNG) +   ( 5 * a_RENLFAIL) +
      (11 * a_LIVER)   +  ( 9 * a_LYMPH)   +  (12 * a_METS)     +   ( 4 * a_TUMOR)    +
      ( 3 * a_COAG)    +  (-4 * a_OBESE)   +  ( 6 * a_WGHTLOSS) +   ( 5 * a_LYTES)    +
      (-2 * a_BLDLOSS) +  (-2 * a_ANEMDEF) +  (-7 * a_DRUG)     +   (-3 * a_DEPRESS);
    WghtElixWoCancer =
      ( 7 * a_CHF)     +  (-1 * a_VALVE)   +  ( 4 * a_PULMCIRC) +   ( 2 * a_PERIVASC) +
      ( 7 * a_PARA)    +  ( 6 * a_NEURO)   +  ( 3 * a_CHRNLUNG) +   ( 5 * a_RENLFAIL) +
      (11 * a_LIVER)   +  ( 9 * a_LYMPH)   +
      ( 3 * a_COAG)    +  (-4 * a_OBESE)   +  ( 6 * a_WGHTLOSS) +   ( 5 * a_LYTES)    +
      (-2 * a_BLDLOSS) +  (-2 * a_ANEMDEF) +  (-7 * a_DRUG)     +   (-3 * a_DEPRESS);

    /* add labels */
    label
      SumElixwoCancer  = 'ELIX_NumDxs - unweighted score without Cancer'
      SumElixWCancer   = 'ELIX_NumDxs - unweighted score with Cancer'
      WghtElixWoCancer = 'ELIX_WghtedDxs - weighted comorbidity score without Cancer'
      WghtElixWCancer  = 'ELIX_WghtedDxs - weighted comorbidity score with Cancer'
    ;
  run;

  * Reduce this to unique people/indexdates, so we know who had encounters but no triggering diags ;
  proc sort nodupkey data = enc ;
    by mrn index_dt ;
  run ;

  proc sql ;
    create table &outputds (label = "Elixhauser scores/flags. Args were: inpatonly = &inpatonly days_lookback = &days_lookback drg_restriction = &drg_restriction..") as
    select  e.mrn as &mrn
          , e.index_dt as &index_date
          , coalesce(cci.a_chf     , 0) as a_chf label = 'Congestive heart failure' length = 3
          , coalesce(cci.a_valve   , 0) as a_valve label = 'Valvular disease' length = 3
          , coalesce(cci.a_pulmcirc, 0) as a_pulmcirc label = 'Pulmonary circulation disease' length = 3
          , coalesce(cci.a_perivasc, 0) as a_perivasc label = 'Peripheral vascular disease' length = 3
          , coalesce(cci.a_depress , 0) as a_depress label = 'Depression' length = 3
          , coalesce(cci.b_htn     , 0) as b_htn label = 'Hypertension w/o chronic complications' length = 3
          , coalesce(cci.b_htncx   , 0) as b_htncx label = 'Hypertension w/ cronic complications' length = 3
          , coalesce(cci.a_para    , 0) as a_para label = 'Paralysis' length = 3
          , coalesce(cci.a_neuro   , 0) as a_neuro label = 'Other neurological disorders' length = 3
          , coalesce(cci.a_chrnlung, 0) as a_chrnlung label = 'Chronic pulmonary disease' length = 3
          , coalesce(cci.a_dm      , 0) as a_dm label = 'Diabetes w/o chronic complications' length = 3
          , coalesce(cci.a_dmcx    , 0) as a_dmcx label = 'Diabetes w/ chronic complications' length = 3
          , coalesce(cci.a_hypothy , 0) as a_hypothy label = 'Hypothyroidism' length = 3
          , coalesce(cci.a_renlfail, 0) as a_renlfail label = 'Renal failure' length = 3
          , coalesce(cci.a_liver   , 0) as a_liver label = 'Liver disease' length = 3
          , coalesce(cci.a_ulcer   , 0) as a_ulcer label = 'Peptic ulcer Disease x bleeding' length = 3
          , coalesce(cci.a_aids    , 0) as a_aids label = 'Acquired immune deficiency syndrome' length = 3
          , coalesce(cci.a_lymph   , 0) as a_lymph label = 'Lymphoma' length = 3
          , coalesce(cci.a_mets    , 0) as a_mets label = 'Metastatic cancer' length = 3
          , coalesce(cci.a_tumor   , 0) as a_tumor label = 'Solid tumor w/out metastasis' length = 3
          , coalesce(cci.a_arth    , 0) as a_arth label = 'Rheumatoid arthritis/collagen vas' length = 3
          , coalesce(cci.a_coag    , 0) as a_coag label = 'Coagulopthy' length = 3
          , coalesce(cci.a_obese   , 0) as a_obese label = 'Obesity' length = 3
          , coalesce(cci.a_wghtloss, 0) as a_wghtloss label = 'Weight loss' length = 3
          , coalesce(cci.a_lytes   , 0) as a_lytes label = 'Fluid and electrolyte disorders' length = 3
          , coalesce(cci.a_bldloss , 0) as a_bldloss label = 'Chronic blood loss anemia' length = 3
          , coalesce(cci.a_anemdef , 0) as a_anemdef label = 'Deficiency Anemias' length = 3
          , coalesce(cci.a_alcohol , 0) as a_alcohol label = 'Alcohol abuse' length = 3
          , coalesce(cci.a_drug    , 0) as a_drug label = 'Drug abuse' length = 3
          , coalesce(cci.a_psych   , 0) as a_psych label = 'Psychoses' length = 3
          , coalesce(cci.a_htn_c   , 0) as a_htn_c label = 'Hypertension' length = 3
          , coalesce(cci.SumElixWoCancer , 0) as SumElixWoCancer label  = 'ELIX_NumDxs - unweighted score without Cancer (missing signifies no encounters found in lookback period)'
          , coalesce(cci.SumElixWCancer  , 0) as SumElixWCancer label   = 'ELIX_NumDxs - unweighted score with Cancer (missing signifies no encounters found in lookback period)'
          , coalesce(cci.WghtElixWCancer , 0) as WghtElixWCancer label  = 'ELIX_WghtedDxs - weighted comorbidity score with Cancer (missing signifies no encounters found in lookback period)'
          , coalesce(cci.WghtElixWoCancer, 0) as WghtElixWoCancer label = 'ELIX_WghtedDxs - weighted comorbidity score without Cancer (missing signifies no encounters found in lookback period)'
    from  enc as e LEFT JOIN
          cci
    on    e.mrn = cci.mrn AND
          e.index_dt = cci.index_dt
    order by 1, 2
    ;

    create table no_encounters as
    select c.&mrn, c.&index_date
    from cohort as c LEFT JOIN
          enc as e
    on    c.&mrn = e.mrn AND
          c.&index_date = e.index_dt
    where e.mrn IS NULL
    ;

    insert into &outputds (&mrn, &index_date)
    select &mrn, &index_date
    from no_encounters
    ;

  quit ;

  proc sort data = &outputds ;
    by &mrn &index_date ;
  run ;

%mend elixhauser ;

/******************************************************/
/******************************************************/
/*****************************************************


/*******************************************************
OBJECTIVE   : Gagne's Combined Comorbidity Score;
              To compute combined comorbidity scores based on ICD-9 codes;
Dependencies: VDW's Diagnosis table (encounter types: IP, AV and ED);
              Elixhauser's ICD9 diagnosis codes;
Citation    : Gagne JJ, Glynn RJ, Avorn J, Levin R, Schneeweiss S. A combined;
              comorbidity score predicted mortality in elderly patients better;
              than existing scores. Journal of Clinical Epidemiology 2011 Jan;
              3 [Epub ahead of print];
Project     : Lung Nodule Study;
Programmer  : Arvind Ramaprasan (KPWHRI);
PI          : Farhood Farjah (UW);
              Diana Buist (HPWHRI);
Date        : 9/5/2017;
*********************************************************/
%Macro Gagne(Inputds          = /* Input filename containing cohort (MRNs and Index date). Example: Work.CohortFile */
          , IndexDateVarName  = /* Index date */
          , outputds          = /* Output filename. Explicitly specify the output library name. Example: WOR.GagneScore */
          );

  %local Elixhauser_Dx_Codes ;
  %Let Elixhauser_Dx_Codes =  '196', '197', '198', '199'
  , '402.01', '402.11', '402.91', '425', '428','429.3'
  , '290', '331.0', '331.1', '331.2', '403.11', '403.91'
  , '404.12', '404.92', '585', '586', 'V42.0', 'V45.1'
  , 'V56.0', 'V56.8', '260', '261', '262', '263', '342'
  , '344', '291.1', '291.5', '291.8', '291.9', '303.90'
  , '303.91', '303.92', '303.93', '305.00', '305.01', '305.02', '305.03'
  , 'V11.3', '140', '141', '142', '143', '144', '145'
  , '146', '147', '148', '149', '150', '151', '152'
  , '153', '154', '155', '156', '157', '158', '159', '160', '161'
  , '162', '163', '164' , '165', '170', '171', '174', '175', '176', '179', '180'
  , '181', '182', '183', '184', '185', '186', '187', '188', '189', '190', '191'
  , '192', '193', '194', '195', '200', '201', '202', '203', '204', '205', '206'
  , '207', '208', '273.0', '273.3', 'V10.46', '426.10', '426.11', '426.13'
  , '426.2', '426.3', '426.4', '426.50', '426.51', '426.52', '426.53', '426.6'
  , '426.7', '426.8', '427.0', '427.2', '427.31', '427.60', '427.9', '785.0'
  , 'V45.0', 'V53.3', '415.0', '491' , '492', '493', '494', '496', '286.0'
  , '286.1', '286.2', '286.3', '286.4', '286.5', '286.6', '286.7', '286.9'
  , '287.1', '287.3', '287.4' , '287.5', '250.4', '250.5', '250.6', '250.7'
  , '250.9', '280.1', '280.8', '280.9', '281', '281.0', '281.1', '281.2', '281.3'
  , '281.4', '281.8', '281.9', '285.9', '276', '070.32', '070.33', '070.54'
  , '456.0' , '456.1', '456.20', '456.21', '571.0', '571.2', '571.3', '571.4'
  , '571.5', '571.6', '571.8', '571.9', '572.3', '572.8', 'V42.7', '440', '441.2'
  , '441.4', '441.7', '441.9', '443.1', '443.2', '443.8', '443.9' , '447.1'
  , '557.1', '557.9', 'V43.4', '295', '295.00', '295.01', '295.02', '295.03'
  , '295.04', '295.05', '296', '297', '298', '299.10', '299.11', '416', '417.9'
  , '042', '043', '044', '401.1', '401.9', '402.10', '402.90', '404.10', '404.90'
  , '405.11', '405.19', '405.91', '405.99'
  ;

  Proc sql;
    Create table diag as
      Select distinct b.MRN
        , b.&IndexDateVarName as IndexCTDate
        , a.Adate
        , a.Dx as dx_orig
        , compress(dx,'','p') as ICD

      From &_VDW_DX as A Inner Join &Inputds. as B on a.MRN=b.MRN
        and intnx('day',b.&IndexDateVarName.,-365)<=a.Adate<=intnx('day',b.&IndexDateVarName.,-1)
        and a.EncType in ('IP','AV','ED')
    where dx_codetype = '09'
    ;
  quit;

  *Flag Elixhauser diagnosis;
  data conditions;
    set DIAG;
    length disease $25;

    if dx_orig in: (&Elixhauser_Dx_Codes.) ;

    disease = 'nopoints';

    if substr(ICD,1,3) = '196' or substr(ICD,1,3) = '197' or
      substr(ICD,1,3) = '198' or substr(ICD,1,3) = '199' then
      disease = 'metastatic_romano';

    if ICD = '40201' or ICD = '40211' or ICD = '40291' or
      substr(ICD,1,4) = '4293' or substr(ICD,1,3) = '425' or
      substr(ICD,1,3) = '428' then
      disease = 'chf_romano';

    if substr(ICD,1,4) = '3310' or substr(ICD,1,4) = '3311' or
      substr(ICD,1,4) = '3312' or  substr(ICD,1,3) = '290' then
      disease = 'dementia_romano';

    if ICD = '40311' or ICD = '40391' or ICD = '40412' or
      ICD = '40492' or substr(ICD,1,3) = '585' or
      substr(ICD,1,3) = '586' or substr(ICD,1,4) = 'V420' or
      substr(ICD,1,4) = 'V451' or substr(ICD,1,4) = 'V560' or
      substr(ICD,1,4) = 'V568' then
      disease = 'renal_elixhauser';

    if '260' <= substr(ICD,1,3) <= '263' then
      disease = 'wtloss_elixhauser';

    if substr(ICD,1,3) = '342' or substr(ICD,1,3) = '344' then
      disease = 'hemiplegia_romano';

    if substr(ICD,1,4) = '2911' or substr(ICD,1,4) = '2912' or
      substr(ICD,1,4) = '2915' or substr(ICD,1,4) = '2918' or
      substr(ICD,1,4) = '2919'         or
      '30390' <= ICD <= '30393'   or
      '30500' <= ICD <= '30503'   or substr(ICD,1,4) = 'V113' then
      disease = 'alcohol_elixhauser';

    if '140' <= substr(ICD,1,3) <= '171'  or
      '174' <= substr(ICD,1,3) <= '195'    or
      substr(ICD,1,4) = '2730'        or
      substr(ICD,1,4) = '2733'        or
      substr(ICD,1,5) = 'V1046'        or
      '200' <= substr(ICD,1,3) <= '208' then
      disease = 'tumor_romano';

    if ICD = '42610' or ICD = '42611' or ICD = '42613' or
      '4262' <= substr(ICD,1,4) <= '4264' or
      '42650' <= ICD <= '42653'       or
      '4266' <= substr(ICD,1,4) <= '4268'  or
      substr(ICD,1,4) = '4270' or substr(ICD,1,4) = '4272' or
      ICD = '42731' or ICD = '42760' or substr(ICD,1,4) = '4279' or
      substr(ICD,1,4) = '7850' or substr(ICD,1,4) = 'V450' or
      substr(ICD,1,4) = 'V533' then
      disease = 'arrhythmia_elixhauser';

    if substr(ICD,1,4) = '4150' or substr(ICD,1,4) = '4168' or
      substr(ICD,1,4) = '4169' or substr(ICD,1,3) = '491'  or
      substr(ICD,1,3) = '492'  or substr(ICD,1,3) = '493'  or
      substr(ICD,1,3) = '494'  or substr(ICD,1,3) = '496' then
      disease = 'pulmonarydz_romano';

    if '2860' <= substr(ICD,1,4) <= '2869'  or
      substr(ICD,1,4) = '2871'        or
      '2873' <= substr(ICD,1,4) <= '2875' then
      disease = 'coagulopathy_elixhauser';

    if '25040' <= ICD <= '25073'       or '25090' <= ICD <= '25093' then
      disease = 'compdiabetes_elixhauser';

    if '2801' <= substr(ICD,1,4) <= '2819'  or substr(ICD,1,4) = '2859' then
      disease = 'anemia_elixhauser';

    if '2760' <= substr(ICD,1,4) <= '2769' then
      disease = 'electrolytes_elixhauser';

    if ICD = '07032' or ICD = '07033' or ICD = '07054' or
      substr(ICD,1,4) = '4560' or substr(ICD,1,4) = '4561' or
      ICD = '45620' or ICD = '45621' or
      substr(ICD,1,4) = '5710' or substr(ICD,1,4) = '5712' or
      substr(ICD,1,4) = '5713' or
      '57140' <= ICD <= '57149' or substr(ICD,1,4) = '5715' or
      substr(ICD,1,4) = '5716' or substr(ICD,1,4) = '5718' or
      substr(ICD,1,4) = '5719' or substr(ICD,1,4) = '5723' or
      substr(ICD,1,4) = '5728' or substr(ICD,1,4) = 'V427' then
      disease = 'liver_elixhauser';

    if '4400' <= substr(ICD,1,4) <= '4409' or
      substr(ICD,1,4) = '4412' or  substr(ICD,1,4) = '4414' or
      substr(ICD,1,4) = '4417' or substr(ICD,1,4) = '4419' or
      '4431' <= substr(ICD,1,4) <= '4439' or
      substr(ICD,1,4) = '4471' or substr(ICD,1,4) = '5571' or
      substr(ICD,1,4) = '5579' or substr(ICD,1,4) = 'V434' then
      disease = 'pvd_elixhauser';

    if '29500' <= ICD <= '29899' or ICD = '29910' or ICD = '29911' then
      disease = 'psychosis_elixhauser';

    if substr(ICD,1,3) = '416' or substr(ICD,1,4) = '4179' then
      disease = 'pulmcirc_elixhauser';

    if substr(ICD,1,3) = '042' or substr(ICD,1,3) = '043' or substr(ICD,1,3) = '044' then
      disease = 'hivaids_romano';

    if substr(ICD,1,4) = '4011' or substr(ICD,1,4) = '4019' or
      ICD = '40210' or ICD = '40290' or ICD = '40410' or ICD = '40490' or
      ICD = '40511' or ICD = '40519' or ICD = '40591' or ICD = '40599' then
      disease = 'hypertension_elixhauser';

    if disease ^= 'nopoints';

    *check work;
    *if disease = 'nopoints';
  run;

  proc sort nodupkey data=conditions;
    by mrn disease;
  run;

  *Applying the weights;
  data conditionweights;
    set conditions;
    weight = 0;

    if disease = 'metastatic_romano' then
      weight = 5;

    if disease = 'chf_romano' then
      weight = 2;

    if disease = 'dementia_romano' then
      weight = 2;

    if disease = 'renal_elixhauser' then
      weight = 2;

    if disease = 'wtloss_elixhauser' then
      weight = 2;

    if disease = 'hemiplegia_romano' then
      weight = 1;

    if disease = 'alcohol_elixhauser' then
      weight = 1;

    if disease = 'tumor_romano' then
      weight = 1;

    if disease = 'arrhythmia_elixhauser' then
      weight = 1;

    if disease = 'pulmonarydz_romano' then
      weight = 1;

    if disease = 'coagulopathy_elixhauser' then
      weight = 1;

    if disease = 'compdiabetes_elixhauser' then
      weight = 1;

    if disease = 'anemia_elixhauser' then
      weight = 1;

    if disease = 'electrolytes_elixhauser' then
      weight = 1;

    if disease = 'liver_elixhauser' then
      weight = 1;

    if disease = 'pvd_elixhauser' then
      weight = 1;

    if disease = 'psychosis_elixhauser' then
      weight = 1;

    if disease = 'pulmcirc_elixhauser' then
      weight = 1;

    if disease = 'hivaids_romano' then
      weight = -1;

    if disease = 'hypertension_elixhauser' then
      weight = -1;
    keep mrn disease weight;
  run;

  *Summing the weights;
  data combinedcomorbidityscore;
    set conditionweights(keep = mrn weight);
    by mrn;

    if first.mrn then
      combinedscore = 0;
    combinedscore + weight;

    if last.mrn then
      output;
    keep mrn combinedscore;
  run;

  *Note: patients not included in the final data set (combinedcomorbidityscore) did not
  have any of the component conditions. Therefore, be sure to set their combined comorbidity
  score values to zero;
  Proc sql;
    Create table &outputds (label = "Gagne's Combined Comorbidity Score") as
      Select a.*
        ,
      case
        when a.MRN=b.MRN then b.combinedscore
        else 0
      end
    as combinedscore label = "Gagne's Combined Comorbidity Score"
      From &Inputds. as A Left Join combinedcomorbidityscore as B on a.MRN=b.MRN;
  quit;

%Mend Gagne;

* Received 14-nov-2017 from Arvind Ramaprasan of KPWHRI ;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
*Frailty Index: based on last 6 months of data;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%Macro FrailtyIndex(Inputds         =
                ,  IndexDateVarName =
                ,  outputds         =
                ,  worklib          = work
                );

  *Administrative data (inpatient and outpatient data for six months window);
   %charlson(inputds          = &inputds
           , IndexDateVarName = &IndexDateVarName
           , outputds         = frail_charls
           , IndexVarName     = Charlson_6mo
           , inpatonly        = B
           , malig            = Y
           , days_lookback    = 183
           );

  Proc sql;
    create table &worklib..demogs as
    select i.mrn
        , (sex_admin in ('M', 'm')) as Male
        , %calcage(bdtvar = birth_date, refdate = &IndexDateVarName) as Age_Years label = "Age on &IndexDateVarName"
        , (race1 = 'WH') as Race label = "Is patient known to be White?"
    from  &inputds as i INNER JOIN
          &_vdw_demographic as d
    on    i.mrn = d.mrn
    ;

    create table &worklib..IP_ENC_6MoPre as
      Select a.Mrn
        , a.&IndexDateVarName. as IndexCTDate  Format=Date9.
        , Count(distinct
      case
        when a.MRN=b.MRN
        and intnx('Day',a.&IndexDateVarName.,-183,'sameday')<= b.Adate <= a.&IndexDateVarName.
        and b.Enctype in ('IP') Then  enc_id
        else ''
      end)
      as N_IP_ENC_PRE
        ,
      Case
        when calculated N_IP_ENC_PRE >= 1 then 1
        else 0
      end
      as With_ip_6mopre label = "Had an inpatient encounter starting w/in 6 mos of index date?"
    From &Inputds. as A Left Join &_VDW_Utilization. as B on a.MRN=b.MRN
      and intnx('Day',a.&IndexDateVarName.,-183,'sameday')<= b.Adate <= a.&IndexDateVarName.
      and b.Enctype in ('IP')
    Group by a.Mrn
      ,   a.&IndexDateVarName.;

    *Patient file;
    Create table &worklib..Patient as
    Select a.Mrn as Bene_Id
    ,
    Case
      when a.MRN=c.MRN and c.Male in (1)  Then 1
      else 0
    end
    as Male
    ,
    Case
      when a.MRN=c.MRN then c.Age_Years
      else .
    end
    as Age
    ,
    Case
      when a.MRN=c.MRN and c.Race in (0) Then 1
      else 0
    end
    as White
    ,
    Case
      when a.MRN=b.MRN and b.Charlson_6mo >= 1 then 1
      else 0
    end
    as Chrlson_b label = "6-mo Charlson score >=1?"
    ,
    Case
      when a.MRN=d.MRN Then With_ip_6mopre
      else 0
    end
    as With_ip_6mopre
    From  &Inputds. as A Left Join
          frail_charls as B
    on a.MRn=b.MRN  Left Join demogs as C
    on a.MRN=c.MRN  Left Join &worklib..IP_ENC_6MoPre as D
    on a.MRN=d.MRN
    ;
  Quit;

  *ICD: Restrict ICD codes to the time range for your study. The index was developed using 6 months of data;
  *N=466,530;
  *Rsubmit;
  Proc sql;
    Create table &worklib..ICD as
      Select distinct a.Mrn as Bene_Id
        ,
      case
        when a.MRN=b.MRN
        and intnx('Day',a.&IndexDateVarName,-183,'sameday')<= b.Adate <= a.&IndexDateVarName
        and dx_codetype in ('09') Then b.Dx
        else ''
      end
    as Dx
      ,
    case
      when a.MRN=b.MRN
      and intnx('Day',a.&IndexDateVarName,-183,'sameday')<= b.Adate <= a.&IndexDateVarName
      and dx_codetype in ('09') Then compress(b.Dx,'.')
      else ''
    end
  as icd_9_cm_code
    , b.DX_CodeType
  From &Inputds. as A Inner Join &_VDW_Dx as B on a.MRN=b.MRN
    and intnx('Day',a.&IndexDateVarName,-183,'sameday')<= b.Adate <= a.&IndexDateVarName
    and dx_codetype in ('09')
    and b.Enctype in ('IP','AV');
  quit;

  *HCPCS
  Restrict HCPCS to the time range for your study. The index was developed using 6 months of data;

  *Rsubmit;
  *N=65495;
  Proc sql;
    Create table &worklib..HCPCS as
      Select distinct a.Mrn as Bene_Id
        ,
      case
        when a.MRN=b.MRN
        and intnx('Day',a.&IndexDateVarName,-183,'sameday')<= b.Adate <= a.&IndexDateVarName
        and px_codetype in ('H4') Then b.PX
        else ''
      end
    as hcpcs
      , b.PX_CodeType
    From &Inputds. as A Inner Join &_VDW_Px as B on a.MRN=b.MRN
      and intnx('Day',a.&IndexDateVarName,-183,'sameday')<= b.Adate <= a.&IndexDateVarName
      and b.px_codetype in ('H4')
      and b.Enctype in ('IP','AV');
  quit;

  **  Define conditions using ICD codes  --  ICD code level  **;
  *Rsubmit;
  Data icd;
    set &worklib..icd;

    **1**;
    **this is CCS 202 and 203**;
    length arthritis 3;
    arthritis=0;

    if  icd_9_cm_code in ('7140' , '7141' , '7142' , '71430', '71431', '71432', '71433', '7144', '71481' ,
      '71489', '7149' , '7200' , '71500', '71504', '71509', '71510', '71511', '71512', '71513', '71514',
      '71515', '71516', '71517', '71518', '71520', '71521', '71522', '71523', '71524', '71525', '71526',
      '71527', '71528', '71530', '71531', '71532', '71533', '71534', '71535', '71536', '71537', '71538',
      '71580', '71589', '71590', '71591', '71592', '71593', '71594', '71595', '71596', '71597', '71598',
      'V134') then
      arthritis=1;

    **2**;
    **this is CCS 653**;
    length cognitive_impairment 3;
    cognitive_impairment=0;

    if  icd_9_cm_code in (  '2900','29010','29011','29012','29013','29020','29021','2903' ,'29040','29041',
      '29042','29043','2908','2909','2930' ,'2931' ,'2940' ,'2941' ,'29410','29411','29420','29421','2948' ,
      '2949' ,'3100' ,'3102','3108','31081','31089','3109' ,'3310' ,'3311' ,'33111','33119','3312' ,'33182',
      '797') then
      cognitive_impairment=1;

    **3**;
    **this is CCS 108**;
    length con_heart_failure 3;
    con_heart_failure=0;

    if  icd_9_cm_code in ('39891','4280','4281','42820','42821','42822','42823','42830','42831','42832','42833',
      '42840','42841','42843','4289') then
      con_heart_failure=1;

    **4**;
    ** this is CCS 650 and 657**;
    length depression 3;
    depression=0;

    if  icd_9_cm_code in ('3090','3091','30922','30923','30924','30928','30929','3093','3094','30982','30983','30989',
      '3099','29383','29600','29601','29602','29603','29604','29605','29606','29610','29611','29612','29613','29614',
      '29615','29616','29620','29621','29622','29623','29624','29625','29626','29630','29631','29632','29633','29634',
      '29635','29636','29640','29641','29642','29643','29644','29645','29646','29650','29651','29652','29653','29654',
      '29655','29656','29660','29661','29662','29663','29664','29665','29666','2967','29680','29681','29682','29689',
      '29690','29699','3004','311') then
      depression=1;

    **5**;
    length falls 3;
    falls=0;

    if  icd_9_cm_code in ('E8800','E8801','E8809','E8810','E8811','E882','E8830','E8831','E8832','E8839','E8840','E8841',
      'E8842','E8843','E8844','E8845','E8846','E8849','E885','E8850','E8851','E8852','E8853','E8854','E8859','E8860',
      'E8869','E888','E8880','E8881','E8888','E8889','E9681','E9870','E9871','E9872','E9879') then
      falls=1;

    **6**;
    length impair_mob_dx 3;
    impair_mob_dx=0;

    if  icd_9_cm_code in ('V463') then
      impair_mob_dx=1;

    **impaired mobility also uses HCPCS codes as shown below**;
    **7**;
    length musculo_prob 3;
    musculo_prob=0;

    if icd_9_cm_code in ('7130','7131','7132','7133','7134','7135','7136','7137','7138','71600','71601','71602','71603',
      '71604','71605','71606','71607','71608','71609','71620','71621','71622','71623','71624','71625','71626','71627',
      '71629','71629','71630','71631','71632','71633','71634','71635','71636','71637','71638','71639','71640','71641',
      '71642','71643','71644','71645','71646','71647','71648','71649','71650','71651','71652','71653','71654','71655',
      '71656','71657','71658','71659','71660','71661','71662','71663','71664','71665','71666','71667','71668','71680',
      '71681','71862','71683','71684','71685','71686','71687','71688','71689','71690','71691','71692','71693','71694',
      '71695','71696','71697','71698','71699','71810','71811','71812','71813','71814','71815','71817','71818','71819',
      '71820','71821','71822','71823','71824','71825','71826','71827','71828','71829','71850','71851','71852','71853',
      '71854','71855','71856','71857','71858','71859','71860','71865','71870','71871','71872','71873','71874','71875',
      '71876','71877','71878','71879','71880','71881','71882','71883','71884','71885','71886','71887','71888','71889',
      '71890','71891','71892','71893','71894','71895','71897','71898','71899','71900','71901','71902','71903','71904',
      '71905','71906','71907','71908','71909','71910','71911','71912','71913','71914','71915','71916','71917','71918',
      '71919','71920','71921','71922','71923','71924','71925','71926','71927','71928','71929','71930','71931','71932',
      '71933','71934','71935','71936','71937','71938','71939','71940','71941','71942','71943','71944','71945','71946',
      '71947','71948','71949','71950','71951','71952','71953','71954','71955','71956','71957','71958','71959','71960',
      '71961','71962','71963','71964','71965','71966','71967','71968','71969','7197' ,'71970','71975','71976','71977',
      '71978','71979','71980','71981','71982','71983','71984','71985','71986','71987','71988','71989','71990','71991',
      '71992','71993','71994','71995','71996','71997','71998','71999','7201' ,'7202' ,'72081','72089','7209' ,'7210' ,
      '7211' ,'7212' ,'7213' ,'72141','72142','7215' ,'7216' ,'7217' ,'7218' ,'72190','72191','7220' ,'72210','72211',
      '7222' ,'72230','72231','72232','72239','7224' ,'72251','72252','7226' ,'72270','72271','72272','72273','72280',
      '72281','72282','72283','72290','72291','72292','72293','7230' ,'7231' ,'7232' ,'7233' ,'7234' ,'7235' ,'7236' ,
      '7237' ,'7238' ,'7239' ,'72400','72401','72402','72403','72409','7241' ,'7242' ,'7243' ,'7244' ,'7245' ,'7246' ,
      '72470','72471','72479','7248' ,'7249' ,'73300','73301','73302','73393','73309','7331' ,'73310','73311','73312',
      '73313','73314','73315','73316','73319','73393','73394','73395','73396','73397','73398','V1351','4350' ,'4351',
      '4352' ,'4353' ,'4358' ,'4359') then
      musculo_prob=1;

    **8**;
    length paranoid 3;
    paranoid=0;

    if  icd_9_cm_code in ('29381','29382','29500','29501','29502','29503','29504','29505','29510','29511','29512','29513',
      '29514','29515','29520','29521','29522','29523','29524','29525','29530','29531','29532','29533','29534',
      '29535','29540','29541','29542','29543','29544','29545','29550','29551','29552','29553','29554','29555',
      '29560','29561','29562','29563','29564','29565','29570','29571','29572','29573','29574','29575','29580',
      '29581','29582','29583','29584','29585','29590','29591','29592','29593','29594','29595','2970' ,'2971' ,
      '2972' ,'2973' ,'2978 ','2979' ,'2980' ,'2981' ,'2982' ,'2983' ,'2984' ,'2988' ,'2989') then
      paranoid=1;

    **9**;
    **this is CCS 199 **;
    length skin 3;
    skin=0;

    if icd_9_cm_code in ('7070' ,'70700','70701','70702','70703','70704','70705','70706','70707','70709','7071','70710','70711',
      '70712','70713','70714','70715','70719','70720','70721','70722','70723','70724','70725','7078' ,'7079') then
      skin=1;

    **10**;
    **this is CCS 4**;
    length myco 3;
    myco=0;

    if icd_9_cm_code in ('1100','1101','1102','1103','1104' ,'1105' ,'1106' ,'1108' ,'1109','1110','1111','1112','1113','1118',
      '1119','1120' ,'1121','1122','1123','1125','11282','11284','11285','11289','1129','1141','1143','1149','11500',
      '11509','11510','11519','11590','11599','1160','1161','1162','1170','1171' ,'1172', '1173','1174','1175','1176',
      '1177' ,'1178' ,'1179' ,'118') then
      myco=1;

    **11**;
    **this is CCS 79**;
    length park 3;
    park=0;

    if icd_9_cm_code in ('3320') then
      park=1;

    **12**;
    **this is CCS 122**;
    length pneum 3;
    pneum=0;

    if icd_9_cm_code in ('00322','0203','0204','0205','0212','0221','0310','0391','0521','0551','0730','0830','1124','1140',
      '1144','1145', '11505', '11515','11595','1304','1363','4800','4801','4802','4803','4808','4809','481','4820','4821',
      '4822','4823','48230', '48231', '48232', '48239','4824','48240','48241','48242','48249','4828','48281','48282',
      '48283','48284','48289','4829','483','4830','4831','4838','4841','4843','4845','4846','4847','4848','485','486',
      '5130','5171') then
      pneum=1;

    **13**;
    **this is CCS 197**;
    length soft 3;
    soft=0;

    if icd_9_cm_code in ('0201','0210','0220','0311','03285','035','0390','6800','6801','6802','6803','6804','6805','6806',
      '6807','6808','6809','68100','68101','68102','68110','68111','6819','6820','6821','6822','6823','6824','6825',
      '6826','6827','6828','6829','684','6850','6851','6860','68600','68601','68609','6861','6868','6869') then
      soft=1;

    **14**;
    length stroke 3;
    stroke=0;

    if  icd_9_cm_code in ('34660','34661','34662','34663','430','431', '4320','4321','4329','43301','43311','43321','43331',
      '43381','43391','4340','43400','43401','4341','43410','43411','4349','43490','43491','436','438','4380','43810',
      '43811','43812','43813','43814','43819','43820','43821','43822','43840','43841','43842','43850','43851','43852',
      '43853','4386','4387','43881','43882','43883','43884','43885','43889','4389') then
      stroke=1;

    **15**;
    **this is CCS 159**;
    length uti 3;
    uti=0;

    if icd_9_cm_code in ('03284','59000','59001','59010','59011','5902','5903','59080','59081','5909','5950','5951','5952',
      '5953','5954','59581','59582','59589','5959','5970','59780','59781','59789','59800','59801','5990') then
      uti=1;

    **16**;
    **this is CCS 54**;
    Length gout 3;
    gout=0;

    if icd_9_cm_code in ('2740','27400','27401','27402','27403','27410','27411','27419','27481','27482','27489','2749',
      '71210','71211','71212','71213','71214','71215','71216','71217','71218', '71219', '71220', '71221', '71222',
      '71223', '71224','71225','71226','71227','71228','71229','71230','71231','71232','71233','71234','71235','71236',
      '71237','71238','71239','71280','71281','71282','71283','71284','71285','71286','71287','71288','71289','71290',
      '71291','71292','71293','71295','71296','71297','71298','71299') Then
      gout=1;
  run;

  **  Aggregate by patient ID to create binary variables at patient level  --  Patient level   **;
  *Rsubmit;
  proc sort data= Icd;
    by bene_id;
  run;

  proc summary data=ICD;
    var impair_mob_dx
      con_heart_failure
      cognitive_impairment
      stroke
      arthritis
      depression
      paranoid
      gout
      falls
      musculo_prob
      uti
      myco
      park
      pneum
      soft
      skin;
    by bene_id;
    output out=icd_pt_condition
      max(impair_mob_dx
      con_heart_failure
      cognitive_impairment
      stroke
      arthritis
      depression
      paranoid
      gout
      falls
      musculo_prob
      uti
      myco
      park
      pneum
      soft
      skin) = impair_mob_dx
      con_heart_failure
      cognitive_impairment
      stroke
      arthritis
      depression
      paranoid
      gout
      falls
      musculo_prob
      uti
      myco
      park
      pneum
      soft
      skin;
  run;

  proc means data=icd_pt_condition;
    var impair_mob_dx
      con_heart_failure
      cognitive_impairment
      stroke
      arthritis
      depression
      paranoid
      gout
      falls
      musculo_prob
      uti
      myco
      park
      pneum
      soft
      skin;
  run;

  Data icd_pt_condition;
    set icd_pt_condition;
    drop _freq_ _type_;
  run;

  proc sort data=icd_pt_condition;
    by bene_id;
  run;

  **  Define one condition using HCPCS codes  --  HCPCS code level  **;
  *Rsubmit;
  Data hcpcs;
    set &worklib..hcpcs;
    length hcpcs_1 $1. hcpcs_2_5 4 impaired_mobility_hcpcs 3;
    impaired_mobility_hcpcs=0;
    hcpcs_1=substr(hcpcs,1,1);
    hcpcs_2_5=input(substr(hcpcs,2,4), best.);

    if hcpcs_1="E" and
      (1050<=hcpcs_2_5<=1093
      or 1100<=hcpcs_2_5<=1110
      or 1130<=hcpcs_2_5<=1161
      or 1170<=hcpcs_2_5<=1200
      or 1220<=hcpcs_2_5<=1239
      or 1240<=hcpcs_2_5<=1270
      or 1280<=hcpcs_2_5<=1298) then
      impaired_mobility_hcpcs=1;
  run;

  proc means Data=hcpcs;
    var impaired_mobility_hcpcs;
  run;

  **  Aggregate by patient ID to create binary variables at patient level  --  Patient level   **;
  proc sort Data=hcpcs;
    by bene_id;
  run;

  proc summary Data=hcpcs;
    var impaired_mobility_hcpcs;
    by bene_id;
    output out=hcpcs_pt_condition max(impaired_mobility_hcpcs)=impaired_mobility_hcpcs;
  run;

  Data hcpcs_pt_condition;
    set hcpcs_pt_condition;
    drop _freq_ _type_;
  run;

  proc sort Data=hcpcs_pt_condition;
    by bene_id;
  run;

  proc means Data=hcpcs_pt_condition;
    var impaired_mobility_hcpcs;
  run;

  **  Merge two condition datasets at the patient level **;
  *rsubmit;
  Data pt_condition;
    merge icd_pt_condition
      hcpcs_pt_condition;
    by bene_id;
  run;

  Data pt_condition;
    set pt_condition;
    length impaired_mobility 3;
    impaired_mobility=0;
    array ab(16) impair_mob_dx--skin;

    do i=1 to 16;
      if ab(i)=. then
        ab(i)=0;
    end;

    drop i;

    if impaired_mobility_hcpcs=. then
      impaired_mobility_hcpcs=0;
    impaired_mobility=max(of impair_mob_dx impaired_mobility_hcpcs);
  run;

  proc freq Data=pt_condition;
    table impaired_mobility*(impair_mob_dx impaired_mobility_hcpcs);
  run;

  Data pt_condition;
    set pt_condition;
    keep bene_id con_heart_failure--skin impaired_mobility;
  run;

  **  Merge with “patient” file to obtain complete patient-level information **;
  *Rsubmit;
  Data all;
    merge pt_condition
      &worklib..patient;
    by bene_id;
  run;

  Data all;
    set all;
    array ab(16) con_heart_failure--impaired_mobility;

    do i=1 to 16;
      if ab(i)=. then
        ab(i)=0;
    end;

    drop i;
  run;

  proc means data=All;
    var male
      age
      white
      chrlson_b
      with_ip_6mopre
      con_heart_failure--impaired_mobility;
  run;

  **   Generate frailty index   **;
  Data &outputds.;
    set all (rename = (bene_id = mrn));
    a=  - (9.00)
    + (1.24*impaired_mobility)
    + (0.50*con_heart_failure)
    + (0.54*depression)
    + (0.43*arthritis)
    - (0.49*white)
    + (0.33*cognitive_impairment)
    + (0.31*chrlson_b)
    + (0.28*stroke)
    + (0.50*park)
    + (0.24*paranoid)
    + (0.23*skin)
    - (0.19*male)
    + (0.09*age)
    + (0.09*with_ip_6mopre)
    + (0.14*myco)
    + (0.21*pneum)
    + (0.18*soft)
    + (0.08*gout)
    + (0.08*falls)
    + (0.05*musculo_prob)
    + (0.05*uti);
    p_frailty= exp(a)/(1 + exp(a));

    If p_frailty >= 0.2 then
      Frail=1;
    else Frail=0;
    label
      p_frailty = "Segal's Frailty Indicator Score"
      frail     = "Categorical Frailty Indicator"
    ;
  run;

%Mend FrailtyIndex;

/*****************************************************************************************************/
/* Program:  Faurotfrailty.sas                                                                       */
/*           ADL-Dependency in older patients based on Faurot, et al, PDS 2015                       */
/*Reference : Faurot KR, Funk MJ, Pate V et al. Using claims data to predict dependency in activities*/
/*          of daily living as a proxy for frailty. Pharmacoepidemiol. Drug Saf. 24(1), 59–66 (2015) */
/*****************************************************************************************************/
/* Last updated: 12/28/2017 by J Eavey at KPWAHRI (eavey.j@ghc.org)                                  */
/*****************************************************************************************************/
/*                                                                                                   */
/* Overview: This macro uses an input cohort dataset that contains MRN and index date to calculate   */
/*       the Faurot frailty index score using the VDW demographics, diagnosis and procedure      */
/*       tables and the Faurot frailty model parameters available at:                */
/*       http://sph.unc.edu/files/2015/11/HGRC_updated_supplemental_material_11_10_15.pdf    */
/*       The cohort dataset must contain one record per cohort member and must contain a unique  */
/*         patient-level identifier and a cohort entry date.                     */
/*                                                                                                   */
/* NOTE – THIS MACRO IS INTENDED FOR USE IN POPULATIONS AGES 66+ YEARS OLD ONLY                      */
/*                                                                                                   */
/* Output:   This macro will output a dataset with the same records as your input cohort dataset.    */
/*           The output dataset will contain your &IDVAR, &DATEVAR, a flag for each of the frailty   */
/*           parameters, and a variable named PREDICTED that contains the person's predicted         */
/*           probability of ADL-Dependence based on Faurot, et al, PDS 2015.                         */
/*                                                                                                   */
/* User specifications:                                        */
/*    IN:   location of input dataset                              */
/*    OUT:      location to store output dataset                                                     */
/*                                                   */
/* Macro parameters:                                                                                 */
/*    INPUTDS:  name of cohort dataset containing one record per cohort member
/*    MRN:      variable name of unique person-level identifier in the input cohort dataset          */
/*    INDEXDT:  variable name of cohort entry date or index datein the input dataset                 */
/*    DAYS_LOOKBACK:  length of look-back period for assessing frailty- note that the original       */
/*        analysis included 8 months of claims data, where days_lookback = 238.  Any deviation */
/*          from this value has not been validated in the literature - beware and be careful!    */
/*    OUTPUTDS: name for the output dataset. ;                                                       */
/*****************************************************************************************************/

%macro Faurot(INPUTDS     = val_cohort1
          , MRN           = mrn
          , INDEX_DATE    = adate
          , DAYS_LOOKBACK = 238
          , outputds      = ) ;
  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  /*~~~~~~         END OF USER SPECIFIED INPUTS        ~~~~~~~~~~*/
  /*~~~~~~   PLEASE DO NOT CHANGE CODE AFTER THIS POINT ~~~~~~~~~*/
  /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  /**********************************************************/
  /* STEP 0 - Creating necessary formats for ICD10 dx codes */
  /**********************************************************/

  /**/
  /* creating the format for ICD10 codes - this format is based exclusively on the 2018 CMS
  GEMS icd9 to icd10 file available online here:
  https://www.cms.gov/Medicare/Coding/ICD10/2018-ICD-10-CM-and-GEMs.html
  This file is valid through 9/30/2018.  If you are using this macro after that date, you should
  check for ICD10 updates. */
  /* There is supplemental code at the bottom of this macro to easily turn the Faurot ICD9 codes
  into an ICD10 format using the GEMS text file */

  %put WARNING: DOES YOUR VDW UTILIZATION INCLUDE INFO ON DME AND AMBULANCE RIDES????;
  %put WARNING: This macro needs both types of utilization to return a valid Faurot score. ;
  %put WARNING: VDW Implementations differ on this (or did as of the May 2019 discussion on the VIG list) ;
  %put WARNING: Some sites (e.g. KPWA, KPSC) purposely exclude this utilization. Other ;
  %put          sites (e.g. KPNC, Geisinger, KPH) have questionable capture. ;
  %put INFO: THE Faurot MACRO IS INTENDED FOR USE IN POPULATIONS AGES 66+ YEARS OLD ONLY. ;

  proc format;
    value $ICD10Faurot
      'M0000',  'M00019', 'M00029', 'M00039', 'M00049', 'M00059', 'M00069', 'M00079', 'M0008',
      'M0009',  'M0010',  'M00119', 'M00129', 'M00139', 'M00149', 'M00159', 'M00169', 'M00179',
      'M0018',  'M0019',  'M0020',  'M00219', 'M00229', 'M00239', 'M00249', 'M00259', 'M00269',
      'M00279', 'M0028',  'M0029',  'M0080',  'M00819', 'M00829', 'M00839', 'M00849', 'M00859',
      'M00869', 'M00879', 'M0088',  'M0089',  'M009',   'M01X0',  'M01X19', 'M01X29', 'M01X39',
      'M01X49', 'M01X59', 'M01X69', 'M01X79', 'M01X8',  'M01X9',  'M0210',  'M02119', 'M02129',
      'M02139', 'M02149', 'M02159', 'M02169', 'M02179', 'M0218',  'M0219',  'M0230',  'M02319',
      'M02329', 'M02339', 'M02349', 'M02359', 'M02369', 'M02379', 'M0238',  'M0239',  'M0500',
      'M0510',  'M0530',  'M0560',  'M061',   'M064',   'M069',   'M0800',  'M083',   'M0840',
      'M1120',  'M11219', 'M11229', 'M11239', 'M11249', 'M11259', 'M11269', 'M11279', 'M1128',
      'M1129',  'M1180',  'M11819', 'M11829', 'M11839', 'M11849', 'M11859', 'M11869', 'M11879',
      'M1188',  'M1189',  'M119',   'M1200',  'M1280',  'M12819', 'M12829', 'M12839', 'M12849',
      'M12859', 'M12869', 'M12879', 'M1288',  'M1289',  'M129',   'M130',   'M1310',  'M13119',
      'M13129', 'M13139', 'M13149', 'M13159', 'M13169', 'M13179', 'M150',   'M151',   'M152',
      'M153',   'M158',   'M159',   'M1610',  'M167',   'M169',   'M1710',  'M175',   'M179',  'M189',
      'M19019', 'M19029', 'M19039', 'M19049', 'M19079', 'M19219', 'M19229', 'M19239', 'M19249',
      'M19279', 'M1990',  'M1991',  'M1993',  'M2350',  'M238X9', 'M2400',  'M24019', 'M24029',
      'M24039', 'M24049', 'M24059', 'M24073', 'M24076', 'M2408',  'M2410',  'M24119', 'M24129',
      'M24139', 'M24149', 'M24159', 'M24173', 'M24176', 'M2430',  'M24319', 'M24329', 'M24339',
      'M24349', 'M24359', 'M24369', 'M24373', 'M24376', 'M2440',  'M24419', 'M24429', 'M24439',
      'M24443', 'M24446', 'M24459', 'M24469', 'M24473', 'M24476', 'M2450',  'M24519', 'M24529',
      'M24539', 'M24549', 'M24559', 'M24569', 'M24573', 'M24576', 'M2460',  'M24619', 'M24629',
      'M24639', 'M24649', 'M24659', 'M24669', 'M24673', 'M24676', 'M247',   'M2480',  'M24819',
      'M24829', 'M24839', 'M24849', 'M24859', 'M24873', 'M24876', 'M249',   'M2500',  'M25019',
      'M25029', 'M25039', 'M25049', 'M25059', 'M25069', 'M25073', 'M25076', 'M2508',  'M2540', 'M25419',
      'M25429', 'M25439', 'M25449', 'M25459', 'M25469', 'M25473', 'M25476', 'M2548',  'M2550', 'M25519',
      'M25529', 'M25539', 'M25541', 'M25542', 'M25549', 'M25559', 'M25569', 'M25579', 'M2560', 'M25619',
      'M25629', 'M25639', 'M25649', 'M25659', 'M25669', 'M25673', 'M25676', 'M259',   'M3210', 'M3303',
      'M3313',  'M3320',  'M3390',  'M3393',  'M340',   'M341',   'M349',   'M3500',  'M3501', 'M352',
      'M353',   'M355',   'M358',   'M359',   'M434',   'M435X9', 'M79646'
      = "ARTHRITIS"

      'N139',  'N312',  'N318',   'N319',   'N3281',   'N3644', 'N393', 'N3941', 'N3942', 'N3943', 'N3944',
      'N3945', 'N3946', 'N39490', 'N39491', 'N39492', 'N39498', 'R32',  'R338',  'R339',  'R3914'
      = "BLADDER"

      'G92',    'G930',   'G931',   'G932',   'G9340',  'G9341',  'G9349',  'G935',   'G936',   'G9381', 'G9382', 'G9389',
      'G939',   'I609',   'I619',   'I6200',  'I621',   'I629',   'I63019', 'I63119', 'I63139', 'I6320', 'I63219',
      'I6322',  'I63239', 'I6330',  'I6340',  'I6350',  'I6359',  'I6783',  'I6789',  'S0190X', 'S060X0',
      'S061X0', 'S061X1', 'S061X2', 'S061X3', 'S061X4', 'S061X5', 'S061X6', 'S061X7', 'S061X8', 'S061X9',
      'S06360', 'S06361', 'S06362', 'S06363', 'S06364', 'S06365', 'S06366', 'S06367', 'S06368', 'S06369',
      'S064X0', 'S064X1', 'S064X2', 'S064X3', 'S064X4', 'S064X5', 'S064X6', 'S064X7', 'S064X8', 'S064X9',
      'S065X0', 'S065X1', 'S065X2', 'S065X3', 'S065X4', 'S065X5', 'S065X6', 'S065X7', 'S065X8', 'S065X9',
      'S066X0', 'S066X1', 'S066X2', 'S066X3', 'S066X4', 'S066X5', 'S066X6', 'S066X7', 'S066X8', 'S066X9',
      'S06890', 'S06891', 'S06892', 'S06893', 'S06894', 'S06895', 'S06896', 'S06897', 'S06898', 'S06899',
      'S069X0', 'S069X1', 'S069X2', 'S069X3', 'S069X4', 'S069X5', 'S069X6', 'S069X9'
      = "BRAIN_INJ"

      'L89009', 'L89119', 'L89129', 'L89139', 'L89149', 'L89159', 'L89209', 'L89309', 'L89509',
      'L89609', 'L89819', 'L89899', 'L8990',  'L97109', 'L97209', 'L97309', 'L97409', 'L97509',
      'L97809', 'L97909', 'L98419', 'L98429', 'L98499'
      = "DECUB"

      'G210',   'G2589',  'G259',   'G309',   'G3101',  'G3109', 'G311', 'G3183',  'G3184',  'G3185',
      'G3189',  'G319',   'G910',   'G911',   'G912',   'G937',  'G94',  'I69910', 'I69911', 'I69912',
      'I69913', 'I69914', 'I69915', 'I69918', 'I69919', 'R412',  'R413', 'R4181'
      = "DEMENT"

      'I421',  'I422',   'I423',   'I424',   'I425',   'I426',   'I427',   'I428',   'I43',   'I501', 'I5020',
      'I5021', 'I5022',  'I5023',  'I5030',  'I5031',  'I5032',  'I5033',  'I5040',  'I5041', 'I5042',
      'I5043', 'I50810', 'I50811', 'I50812', 'I50813', 'I50814', 'I5082',  'I5083',  'I5084',
      'I5089', 'I509',   'I514',   'I515',   'I517',   'I970',   'I97110', 'I97130', 'I97190'
      = "HF"

      'I951', 'I952', 'I953', 'I9581', 'I9589', 'I959', 'R570', 'R571', 'R578', 'R579', 'R6521', 'T794XX',
      'T8110X', 'T8111X', 'T8112X', 'T8119X'
      = "HYPOSHOCK"

      'E7521', 'E7522', 'E75249', 'E770',  'E771',  'E7800', 'E7801', 'E781', 'E782', 'E783',
      'E784',  'E785',  'E786',   'E7881', 'E7889', 'E789',  'E881',  'E8889'
      = "LIPID"

      'G8100', 'G8101',   'G8102',  'G8103',  'G8104',  'G8110',  'G8111',  'G8112',  'G8113',  'G8114',  'G8190',
      'G8191', 'G8192',   'G8193',  'G8194',  'G8220',  'G8250',  'G8251',  'G8252',  'G8253',  'G8254',  'G830',
      'G8310', 'G8311',   'G8312',  'G8313',  'G8314',  'G8320',  'G8321',  'G8322',  'G8323',  'G8324',  'G8330',
      'G834',  'G835',    'G8381',  'G8384',  'G8389',  'G839',   'I69931', 'I69932', 'I69933', 'I69934', 'I69939',
      'I69941', 'I69942', 'I69943', 'I69944', 'I69949', 'I69951', 'I69952', 'I69953', 'I69954', 'I69959',
      'I69961', 'I69962', 'I69963', 'I69964', 'I69965', 'I69969', 'R295'
      = "PARALYSIS"

      'L03039', 'L03049', 'L600', 'L601', 'L602', 'L603', 'L604', 'L608', 'L609', 'L84'
      = "PODIATRIC"

      'G20', 'G2111', 'G2119', 'G218' = "PD"

      'F0150',  'F0151',  'F0280',  'F0281',  'F0390',  'F0391',  'F04',    'F05',    'F060',   'F061',
      'F062',   'F0630',  'F064',   'F068',   'F070',   'F0781',  'F0789',  'F09',    'F10159', 'F10180',
      'F10181', 'F10182', 'F10188', 'F10231', 'F10239', 'F10259', 'F1027',  'F10280', 'F10281',
      'F10282', 'F10288', 'F10929', 'F10950', 'F10951', 'F10959', 'F1096',  'F10980', 'F10982',
      'F1099',  'F11159', 'F11181', 'F11182', 'F11188', 'F11222', 'F11259', 'F11281', 'F11282',
      'F11288', 'F11922', 'F11959', 'F11981', 'F11982', 'F11988', 'F12122', 'F12159', 'F12180',
      'F12188', 'F12222', 'F12259', 'F12280', 'F12288', 'F12922', 'F12959', 'F12980', 'F12988',
      'F13159', 'F13180', 'F13181', 'F13182', 'F13188', 'F13259', 'F13280', 'F13281', 'F13282',
      'F13288', 'F13959', 'F13980', 'F13981', 'F13982', 'F13988', 'F14122', 'F14159', 'F14180',
      'F14181', 'F14182', 'F14188', 'F14222', 'F14259', 'F14280', 'F14281', 'F14282', 'F14288',
      'F14922', 'F14959', 'F14980', 'F14981', 'F14982', 'F14988', 'F15122', 'F15159', 'F15180',
      'F15181', 'F15182', 'F15188', 'F15222', 'F15259', 'F15280', 'F15281', 'F15282', 'F15288',
      'F15920', 'F15922', 'F15959', 'F15980', 'F15981', 'F15982', 'F15988', 'F16122', 'F16159',
      'F16180', 'F16183', 'F16188', 'F16259', 'F16280', 'F16283', 'F16288', 'F16959', 'F16980',
      'F16983', 'F16988', 'F17208', 'F17218', 'F17228', 'F17298', 'F18159', 'F18180', 'F18188',
      'F18259', 'F18280', 'F18288', 'F18959', 'F18980', 'F18988', 'F19122', 'F19159', 'F19180',
      'F19181', 'F19182', 'F19188', 'F19222', 'F19259', 'F19280', 'F19281', 'F19282', 'F19288',
      'F19921', 'F19922', 'F19939', 'F1994',  'F19950', 'F19951', 'F19959', 'F1996',  'F1997',
      'F19980', 'F19981', 'F19982', 'F19988', 'F1999',  'F200',   'F201',   'F202',   'F205',
      'F2081', 'F2089',   'F209',   'F22',    'F23',    'F24',    'F259',   'F28',    'F29',
      'F3010', 'F3011',   'F3012',  'F3013',  'F302',   'F303',   'F304',   'F308',   'F3110',
      'F3111', 'F3112',   'F3113',  'F312',   'F3130',  'F3131',  'F3132',  'F314',   'F315',
      'F3160', 'F3161',   'F3162',  'F3163',  'F3164',  'F3173',  'F3174',  'F3175',  'F3176',
      'F3177', 'F3178',   'F3181',  'F319',   'F320',   'F321',   'F322',   'F323',   'F324',
      'F325',  'F3289',   'F329',   'F330',   'F331',   'F332',   'F333',   'F3341',  'F3342',
      'F339',  'F3481',   'F3489',  'F39',  'F410',   'F411',   'F418',   'F419',   'F4489',
      'F482',  'F53',     'F840',   'F843',   'F845',   'F848',   'F849'
      = "PSYCH"

      'Z5189' = "REHAB"

      'Z1210', 'Z1211', 'Z1212', 'Z1213', 'Z122',  'Z1231', 'Z1239', 'Z124', 'Z125', 'Z126',
      'Z1271', 'Z1272', 'Z1273', 'Z1281', 'Z1282', 'Z1283', 'Z1289', 'Z129' = "SCREENING"

      'H8109', 'H8113', 'H8123',  'H81319', 'H81399', 'H8149', 'H818X9', 'H8193', 'H829',
      'H8309', 'H8319', 'H832X9', 'H838X9', 'H8393', 'I69998', 'R42'
      = "VERTIGO"

      'M623', 'M6250', 'M6281', 'M6284', 'M6289', 'R5381', 'Z7401'
      = "WEAKNESS"
    ;
  run;

  /*********************************************************************************/
  /* STEP 1 - Setting up patient table with demographics, diagnoses and procedures */
  /*********************************************************************************/

  * creating a cohort table that contains the mrn and index date variables ;
  data cohort (keep= &mrn &index_date);
    set &inputds;
  run;

  * get patient demographics from VDW demographics table and format;
  proc sql;
    create table ptdemo as
      select  distinct
        c.&mrn as mrn
        , c.&index_date as index_dt
        , case when upcase(d.sex_admin)="M" then 0 else 1 end as sex
        , d.race1 as race1
        , d.race2 as race2
        , d.race3 as race3
        , d.race4 as race4
        , d.race5 as race5
        , d.hispanic
        , floor((intck('month',d.Birth_Date,c.&index_date)
        - (day(c.&index_date) < day(d.Birth_Date))) / 12) as age
    FROM cohort as c
    LEFT JOIN &_vdw_demographic as d
      on c.&MRN. = d.MRN;
  quit;

  * Coalesce race into a single category - based on %GetCombinedRace from std macros;
  data ptdemo1 ;
    set ptdemo;
    length combinedrace $2. i 8.;
    combinedrace = "UN";
    array aryrace {*} Race1 Race2;
    do i=1 to dim(aryrace);
      if upcase(aryrace{i}) = "HP" then do;
        CombinedRace = "HP";
      end;
      else if upcase(aryrace{i}) in ( "IN" , "AS", "BA") then do;
        if combinedrace = "UN" then CombinedRace = aryrace{i};
        else combinedrace = "MU";
      end;
      else if upcase(aryrace{i}) = "WH" then do;
        if combinedrace ="UN" then CombinedRace = "WH";
      end;
      else if upcase(aryrace{i}) = "MU" then do;
        CombinedRace = "MU";
      end;
    end;
    if hispanic = 'Y' then race = 'H';
    else if combinedrace = 'BA' then race = 'B';
    else if combinedrace = 'WH' then race = 'W';
    else race = 'O';
    keep mrn index_dt age sex race;
  run;

  /*********************************************************************************/
  /* STEP 2 - Get diagnoses and create flags for frailty indicators */
  /*********************************************************************************/
  /* create indicators based on diagnoses */
  proc sql;
    create table _temp_dx as
      select distinct a.&mrn as mrn, a.&index_date as index_dt, dx, dx_codetype
      from &_vdw_dx as d
      inner join cohort as a
        on d.mrn = a.&mrn and a.&index_date-&days_lookback<=d.adate<=a.&index_date ;
  quit;

  data _temp_dxflags_;

    set _temp_dx;

      /* creating substrings for parsing dx values */
      dxa = compress(dx,'.');
      dxvar2 = substr(dxa,1,2);
      dxvar3 = substr(dxa,1,3);
      dxvar4 = substr(dxa,1,4);
      dxvar5 = substr(dxa,1,5);

      IF dx_codetype = '09' THEN do;

        IF dxvar3 in ('710' '711' '712' '714' '715' '718' '725')
                     or dxvar4 in ('7165' '7166' '7168' '7169' '7190' '7191' '7194' '7195' '7199')
              THEN arthritis = 1;
        ELSE arthritis = 0;

              IF dxvar4 in ('5965' '5996' '7882' '7883')
              THEN bladder = 1;
        ELSE bladder = 0;

              IF dxvar3 in ('348' '430' '431' '432' '436' '852' '853' '854')
              or dxvar5 in ('34982' '43301' '43311' '43321' '43331' '43391' '43401' '43411' '43491')
              THEN brain_inj = 1;
        ELSE brain_inj = 0;

              IF dxvar3 = '707'
              THEN decub = 1;
        ELSE decub = 0;

              IF dxvar3 in ('290' '294' '331' '797')
              or dxvar4 = '4380'
              or dxvar5 in ('33390' '33392' '33399' '78093')
              THEN dement = 1;
        ELSE dement = 0;

              IF dxvar3 in ('425' '428')
              or dxvar4 in ('4290' '4291' '4293' '4294')
              THEN hf = 1;
        ELSE hf = 0;

              IF dxvar3 in ('458')
              or dxvar4 in ('7855' '9584' '9980')
              THEN hyposhock = 1;
        ELSE hyposhock = 0;

             IF dxvar3 in ('272')
             THEN lipid = 1;
         ELSE lipid = 0;

              IF dxvar3 in ('342' '344')
              or dxvar4 in ('4382' '4383' '4384' '4385' '7814')
              THEN paralysis = 1;
        ELSE paralysis = 0;

              IF dxvar3 in ('332')
              THEN pd = 1;
        ELSE pd = 0;

              IF dxvar3 in ('700' '703')
              or dxvar4 in ('6811')
              THEN podiatric =1;
        ELSE podiatric = 0;

              IF dxvar2 in ('29')
              or dxvar3 in ('310' '311')
              or dxvar4 in ('3000')
              THEN psych = 1;
        ELSE psych = 0;

              IF dxvar4 in ('V571' 'V573' 'V578' 'V579')
              or dxvar5 in ('V5721')
              THEN rehab = 1;
        ELSE rehab = 0;

              IF dxvar3 in ('V76')
              THEN screening = 1;
        ELSE screening = 0;

              IF dxvar3 in ('386')
              or dxvar4 in ('7804')
              or dxvar5    in ('43885')
              THEN vertigo = 1;
        ELSE vertigo = 0;

              IF dxvar4 in ('7282' '7283' '7993')
              or dxvar5 in ('72887' 'V4984')
              THEN weakness = 1;
        ELSE weakness = 0;

      END;

      IF dx_codetype = '10' THEN do;

        IF put (dxa,$icd10faurot.) = "ARTHRITIS" THEN arthritis = 1;
        ELSE arthritis = 0;

        IF put(dxa,$icd10faurot.) = "BLADDER" THEN bladder = 1;
        ELSE bladder = 0;

        IF put(dxa,$icd10faurot.) = "BRAIN_INJ" THEN BRAIN_INJ = 1;
        ELSE BRAIN_INJ = 0;

        IF put(dxa,$icd10faurot.) = "DECUB" THEN DECUB = 1;
        ELSE DECUB = 0;

        IF put(dxa,$icd10faurot.) = "DEMENT" THEN DEMENT = 1;
        ELSE DEMENT = 0;

        IF put(dxa,$icd10faurot.) = "HF" THEN HF = 1;
        ELSE HF = 0;

        IF put(dxa,$icd10faurot.) = "HYPOSHOCK" THEN HYPOSHOCK = 1;
        ELSE HYPOSHOCK = 0;

        IF put(dxa,$icd10faurot.) = "LIPID" THEN LIPID = 1;
        ELSE LIPID = 0;

        IF put(dxa,$icd10faurot.) = "PARALYSIS" THEN PARALYSIS = 1;
        ELSE PARALYSIS = 0;

        IF put(dxa,$icd10faurot.) = "PD" THEN PD = 1;
        ELSE PD = 0;

        IF put(dxa,$icd10faurot.) = "PODIATRIC" THEN PODIATRIC = 1;
        ELSE PODIATRIC = 0;

        IF put(dxa,$icd10faurot.) = "PSYCH" THEN PSYCH = 1;
        ELSE PSYCH = 0;

        IF put(dxa,$icd10faurot.) = "REHAB" THEN REHAB = 1;
        ELSE REHAB = 0;

        IF put(dxa,$icd10faurot.) = "SCREENING" THEN SCREENING = 1;
        ELSE SCREENING = 0;

        IF put(dxa,$icd10faurot.) = "VERTIGO" THEN VERTIGO = 1;
        ELSE VERTIGO = 0;

        IF put(dxa,$icd10faurot.) = "WEAKNESS" THEN WEAKNESS = 1;
        ELSE WEAKNESS = 0;

      END;
  RUN;

  /*********************************************************************************/
  /* STEP 3 - Get procedure codes and flag for frailty indicators */
  /*********************************************************************************/
  /* create indicators based on procedures */
  proc sql;
    create table _temp_px as
      select distinct a.&mrn as mrn, a.&index_date as index_dt, px, px_codetype
      from &_vdw_px as p
      inner join cohort as a
        on p.mrn = a.&mrn and a.&index_date-&days_lookback<=p.procdate<=a.&index_date
        and px_codetype in ('H4','C4');
    /* Flag procedures */
    create table _temp_pxflags_ as
       select distinct a.mrn, a.index_dt,

          case when a.px in ('A0426' 'A0427' 'A0428' 'A0429' 'A0999')
              then 1 else 0 end as ambulance,

          case when a.px in ('E0250' 'E0251' 'E0255' 'E0256' 'E0260' 'E0261' 'E0265' 'E0266' 'E0270'
                                   'E0290' 'E0291' 'E0292' 'E0293' 'E0294' 'E0295' 'E0296' 'E0297' 'E0301'
                                   'E0302' 'E0303' 'E0304' 'E0316')
              then 1 else 0 end as hospbed,

          case when a.px in ('E1390' 'E1391' 'E1392' 'E0431' 'E0433' 'E0434' 'E0435' 'E0439' 'E0441'
                                   'E0442' 'E0443')
              then 1 else 0 end as oxygen,

          case when a.px in ('E1050' 'E1060' 'E1070' 'E1083' 'E1084' 'E1085' 'E1086' 'E1087' 'E1088'
                                   'E1089' 'E1090' 'E1091' 'E1092' 'E1093' 'E1100' 'E1110' 'E1140' 'E1150'
                                   'E1160' 'E1161' 'E1170' 'K0001' 'K0002' 'K0003' 'K0004' 'K0005' 'K0006'
                                   'K0007' 'K0008' 'K0009')
              then 1 else 0 end as wheelchair

       from _temp_px as a ;
  quit;

/**********************************************************************************************/
/* STEP 4 - Combine cohort, dx and px tables and calculate frailty score using Faurot weights */
/**********************************************************************************************/
 proc sql;
      /* Combine diagnoses and procedures and collapse into person level dataset */
      create table _temp_cohort_ as
         select distinct a.mrn, a.index_dt, a.sex, a.race,
            a.age - 65 as age65
            , calculated age65 * calculated age65 as age65sq,
            case when a.race='B' then 1 else 0 end as raceB,
            case when a.race='H' then 1 else 0 end as raceH,
            case when a.race='O' then 1 else 0 end as raceO,
            max(b.arthritis)  as arthritis label='Arthritis',
            max(b.bladder)    as bladder label='Bladder dysfunction',
            max(b.brain_inj)  as brain_inj label='Stroke/brain injury',
            max(b.decub)      as decub label='Skin ulcer (decubitus)',
            max(b.dement)     as dement label='Dementias',
            max(b.hf)         as hf label='Heart failure',
            max(b.hyposhock)  as hyposhock label='Hypotension or shock',
            max(b.lipid)      as lipid label='Lipid abnormality',
            max(b.paralysis)  as paralysis label='Paralysis',
            max(b.pd)         as pd label='Parkinsons disease',
            max(b.podiatric)  as podiatric label='Podiatric care',
            max(b.psych)      as psych label='Psychiatric illness',
            max(b.rehab)      as rehab label='Rehabilitation care',
            max(b.screening)  as screening label='Cancer screening',
            max(b.vertigo)    as vertigo label='Vertigo',
            max(b.weakness)   as weakness label='Weakness',
            max(c.ambulance)  as ambulance label='Ambulance',
            max(c.hospbed)    as hospbed label='Home Hospital Bed',
            max(c.oxygen)     as oxygen label='Home oxygen',
            max(c.wheelchair) as wheelchair label='Wheelchair'
         from ptdemo1 as a
            left join _temp_dxflags_ as b on a.mrn = b.mrn and a.index_dt = b.index_dt
            left join _temp_pxflags_ as c on a.mrn = c.mrn and a.index_dt = c.index_dt
         group by 1, 2, 3, 4, 5, 6, 7, 8, 9
         ;
   quit;

   /* Apply parameter estimates from Faurot, et al */

    data &outputds ;
      set _temp_cohort_;

      if n(arthritis, bladder, brain_inj, decub, dement, hf, hyposhock, lipid
          , paralysis, pd, podiatric, psych, rehab, screening, vertigo, weakness, ambulance
          , hospbed, oxygen, wheelchair) = 20 then do ;

        odds = -3.7466 + 0.0028*age65 + 0.0021*age65*age65 + 0.3530*sex +
               0.3126*(race='B') + -0.5097*(race='H') + 0.8228*(race='O') +
               -0.5559*screening + -0.3544*lipid + -0.5925*vertigo +
               0.3598*arthritis + 0.4161*bladder + 0.5322*podiatric +
               0.4120*hf + 0.5363*psych + -0.4315*rehab + 0.9094*oxygen +
               0.6038*hyposhock + 0.4423*ambulance + 0.4745*brain_inj +
               0.7917*dement + 1.1873*pd + 0.4921*weakness + 0.8464*decub +
               1.8541*paralysis +1.5094*wheelchair + 1.4995*hospbed;
        predicted = exp(odds) / (1+exp(odds));
      end ;
      label
        predicted = "Faurot Frailty Score: probability of being dependent on others for Activities of Daily Living"
      ;
      drop age65sq raceB raceH raceO odds;
    run;
%mend faurot ;

%macro util_delete_file(file);

    %local tempref;

    %if %sysfunc(fileexist(&FILE.)) ge 1 %then
        %do;
        %let rc = %sysfunc(filename(tempref, &FILE.));
        %let rc = %sysfunc(fdelete(&TEMPREF.));
        %if &RC. ne 0 %then
            %put %sysfunc(sysmsg());
        %else
            %put NOTE: Deleted file %superq(file) ;
        %let rc = %sysfunc(filename(tempref));
        %if &RC. ne 0 %then
            %put %sysfunc(sysmsg());
        %end;
    %else
        %put NOTE: The file &FILE. does not exist.;

%mend util_delete_file;

%macro util_drop_table(data);
/* Drop a table or view without warning or error if   */
/* it doesn't exist.                                  */
/* If table name contains special characters, you'll  */
/* have to use this one instead of util_drop_tables.  */
/* Jack Hamilton, May 2019                            */

    %if %length(&DATA.) = 0 %then
        %do;
        %put NOTE: No data set specified for %str(%%)&SYSMACRONAME..;
        %return;
    %end;

    %local save_sastrace;
    %let save_sastrace = %sysfunc(getoption(sastrace));
    options sastrace=none;

    %local not_in_sql;
    %let not_in_sql = %eval(&SYSPROCNAME. ne SQL and &SYSPROCNAME. ne FEDSQL);

    %if &NOT_IN_SQL. %then
        %do;
        proc sql;
        %end;

    %if %sysfunc(exist(&DATA., data)) %then
        %do;
        drop table &DATA.;
        %end;
    %else %if %sysfunc(exist(&DATA., view)) %then
        %do;
        drop view &DATA.;
        %end;
    %else
        %do;
        %put NOTE: %UPCASE(&DATA.) does not exist.;
        %end;

    %if &NOT_IN_SQL. %then
        %do;
        quit;
        %end;

    options sastrace="&SAVE_SASTRACE.";

%mend util_drop_table;

%macro util_drop_tables(debug=0) / parmbuff ;

/* Drop a table or view with only a note if it doesn't exist.       */
/* &SYSCC is not reset by this macro under normal circumstances.    */
/* Enter one or more data set names, separated by blanks or commas. */
/* If the data set name uses name quoting, such as 'my data'n ,     */
/* use UTIL_DROP_TABLE instead to drop one table at a time.         */
/* Jack Hamilton, May 2019                                          */

    %let datalist = %cmpres(%sysfunc(translate(%superq(SYSPBUFF)%str( ), %str(   ), %str((,)))));

    %if %length(&DATALIST.) = 0 %then
        %do;
        %put NOTE: No data set(s) specified for &SYSMACRONAME..;
        %return;
    %end;

    %local save_sastrace;
    %let save_sastrace = %sysfunc(getoption(sastrace));
    options sastrace=none;

    %local not_in_sql;
    %let not_in_sql = %eval(&SYSPROCNAME. ne SQL and &SYSPROCNAME. ne FEDSQL);

    %local save_stimer save_fullstimer;
    %let save_stimer = %sysfunc(getoption(stimer));
    %let save_fullstimer = %sysfunc(getoption(fullstimer));
    options nostimer nofullstimer;


    %if &NOT_IN_SQL. %then
        %do;
        proc sql;
        %end;

    %local tablecount i data;

    %let tablecount = %sysfunc(countw(&DATALIST., %str( )));

    %do i = 1 %to &TABLECOUNT.;
        %let data = %sysfunc(scan(&DATALIST., &I., %str( )));
        %if %sysfunc(scan(%upcase(&DATA.), 1, %str(=))) = DEBUG %then
            %do;
            %end;
        %else
            %do;
        %if %sysfunc(exist(&DATA., data)) %then
            %do;
            drop table &DATA.;
            %end;
        %else %if %sysfunc(exist(&DATA., view)) %then
            %do;
            drop view &DATA.;
            %end;
        %else
            %do;
            %put NOTE: Data set %UPCASE(&DATA.) does not exist.;
            %end;
            %end;
    %end;

    %if &NOT_IN_SQL. %then
        %do;
        quit;
        %end;

    options sastrace="&SAVE_SASTRACE.";
    options &SAVE_STIMER. &SAVE_FULLSTIMER.;


%mend util_drop_tables;

%macro pmca(inset = , index_date = , outset = , days_lookback = 365) ;
  /*
    Adapted from the v3.1 code downloaded from
      https://www.kpwashingtonresearch.org/our-research/our-scientists/rita-mangione-smith-md-mph/measurement-tools-research-dr-rita-mangione-smith
    around about 5-jan-2020 by Phil Crawford.

    Original header follows.

    'PEDIATRIC MEDICAL COMPLEXITY ALGORITHM v3.1'

    Programmer(s):  Wren Haaland, Kathryn Whitlock
    Center for Child Health, Behavior, and Development
    Seattle Children's Research Institute

    Dorothy Lyons, Peter Woodcox
    First Steps Database
    Research and Data Analysis Division
    Washington State Department of Social and Health Services

    Date:           July 2019
    Revision from v3.0

    Description:

    This program implements the Pediatric Medical Complexity Algorithm to identify
    children with complex and non-complex chronic conditions using commercial or
    public administrative claims data or other sources of diagnostic coding data
    (e.g. electronic medical records; hospital discharge data) and to distinguish
    them from children with neither chronic nor chronic complex conditions
    (healthy children). 'Complex' and 'Non-Complex' designations are assigned
    based on whether a child's condition(s) identified by ICD-9 and ICD-10 code(s)
    can be considered chronic, malignant, or progressive, and whether multiple
    body systems are involved.

    The AHRQ-funded Center of Excellence on Quality of Care Measures for Children
    with Complex Needs developed consensus definitions for children with complex
    chronic disease (C-CD), children with non-complex chronic disease (NC-CD), and
    children without CD (healthy). Development and testing of this PMCA was
    supported by the COE (Rita Mangione-Smith, MD MPH, Principal Investigator).

    To assign involved body system(s) and progressive status, Seattle Children's
    Research Institute evaluated conditions defined by diagnostic codes outlined
    in the Chronic Illness and Disability Payment System (CDPS) version 5.3, and
    identified conditions of interest based on their chronic nature.

    References:
    Chronic Illness and Disability Payment System (CDPS), University of California, San Diego.

    Input Source Data:
    This program was designed for use with three years of data from a Medicaid
    claims file that includes all claims (inpatient, outpatient, managed care
    encounter data) and has at least the following fields:

    1) Person ID unique to a person
    2) Claim ID unique to a claim
    3) ICD-9 or ICD-10 diagnosis code(s)

    In the source data for which this program was designed, a single claim may
    have multiple lines, and a single record/line may contain a single field or
    multiple fields for diagnosis codes. ICD-9 and ICD-10 codes may be intermixed
    within a single patient but not within a single encounter. The program
    identifies whether an encounter's codes are ICD-9 or ICD-10 format. Then the
    program rolls up assignments by claim and sets an array for ICD-9 or ICD-10
    codes, so that it accommodates source files that have a different number of
    possible diagnosis code fields, or source files that have one row for each
    diagnosis code.

    Format for ICD-9 or ICD-10 codes:

    This program assumes that incoming ICD-9 or ICD-10 codes (as the variable
    called 'icdvar') are in xxx.xx character format with leading zeros for codes
    that do not have 3 characters before the decimal. The program then strips out
    the decimals.  If original source data do not have decimals, the stripping
    command can be deleted from the processing.

    The program is designed to process records with a single diagnosis code field
    or diagnosis code fields. A macro statement has been provided at the start of
    the code to set the number of fields. To change the number of diagnosis fields
    processed, change 'icdnum' in the SET VALUES FOR VARIABLES section to the
    number of fields in a single record of the source data.

    The Process:

    1) 'Claims' dataset:  claims are read in and the decimals are stripped from
    the ICD-9 or ICD-10 codes.

    If incoming data do not have decimals, the do-end 'compress' segment can be
    commented out. ICD-9 or ICD-10 codes with or without decimals are thus
    accommodated.

    2)  The program identifies whether the codes provided are from the ICD-9 or
    ICD-10 codeset.

    3)  Claims are sorted by person ID and claim ID.

    4)  'Flagclaims' processing steps through the multiple lines (or single line)
    of individual claims for a person, identifying body systems involved,
    progressive status of a condition, and malignancy. The final result is a
    dataset where identifications of body system, progressivity, and malignancy
    have been rolled up to a single record per claim, with a single indication of
    any specified occurrence (i.e. once per claim).

    5)  'Results' processing rolls up to one record per child, with
      a) a single indication whether each body type is identified as affected,
      b) a sum across claims for each body type affected
      (i.e. how many claims for this child have this kind of indication?),
      c) indication if a progressive condition, and
      d) indication if malignancy.

    Finally, this collected information is used to calculate the child's status.

    Final Condition Definitions:

    This program calculates two separate variables containing condition
    assignments for 'Complex Chronic', 'Non-complex Chronic', and 'Non-Chronic'.

    The variable 'cond_less' contains values from the less conservative algorithm,
    and is designed to be used with less detailed data sources, such as those
    without outpatient claims.

    The variable 'cond_more' contains values from the more conservative algorithm,
    and is designed to be used with more detailed data sources such as those
    including inpatient and outpatient claims.

    Values for both of the above variables are
    '3 Complex Chronic'
    '2 Non-complex Chronic'
    '1 Non-Chronic'

    Definitions of the categories assigned by the Algorithm:

    The less conservative version (cond_less) calculates values as

    'Complex Chronic':
    1) more than one body system is involved, or
    2) one or more conditions are progressive, or
    3) one or more conditions are malignant

    'Non-complex Chronic':
    1) only one body system is involved, and
    2) the condition is not progressive or malignant

    'Non-Chronic':
    1) no body system indicators are present, and
    2) the condition is not progressive or malignant

    The more conservative version (cond_more) calculates values as

    'Complex Chronic':
    1) more than one body system is involved, and each must be indicated in more
       than one claim, or
    2) one or more conditions are progressive, or
    3) one or more conditions are malignant

    'Non-complex Chronic':
    1) only one body system is indicated in more than one claim, and
    2) the condition is not progressive or malignant

    'Non-Chronic':
    1) no body system indicators are present in more than one claim, and
    2) the condition is not progressive or malignant

    Body Systems of interest and the variables used to indicate them:

    Datasets Created:

    The FLAGCLAIMS dataset contains 1 record for each claim record, with the
    condition flags added.

    The RESULTS dataset creates 1 record per client with accumulated indicators,
    and keeps final condition determinations.


    Macro statements have been provided to more easily adapt the program to
    different data sources (see directions below).

    Created December 2012; revised May 2015, April 2017, July 2019.  Future revisions to the ICD
    classification system may result in the need for revisions to this program.

    Inputs:

    indata.claims            (Source file with claims data)

    Outputs:
    OPUT.RESULTS_PMCA_v3_1    (Output file with 1 record per person and condition classifications)
  */

  *Read in and select the diagnosis data by timeframe and patient cohort.  Keep MRN, ENC_ID, date of service, and diagnosis codes, and strip out the diagnosis code decimal;
  proc sql;
    create table dxes as select
        d.mrn
        , &index_date as index_date format = mmddyy10. label = "Index date passed in to PMCA (lookback was &days_lookback)"
        , d.enc_id
        , d.adate
        , compress(d.dx, '.') as dx
        , case d.dx_codetype
          when '09' then 0
          when '10' then 1
        end as icd10codeset
    from &_vdw_dx as d INNER JOIN
        &inset as i
    on  d.mrn = i.mrn
    where d.adate between intnx('day', &index_date, (-&days_lookback + 1)) and &index_date
          and d.dx_codetype in ('09', '10')
    ;
  quit;

  proc sort data = dxes ;
    by mrn index_date enc_id ;
  run ;

  /*  Assign values to the appropriate flags based on conditions found and roll up by claim. */
  data flagDXes(keep=MRN index_date ENC_ID
    cardiac cranio derm endo gastro genito hemato immuno malign mh metab musculo neuro genetic opthal
    otol otolar pulresp renal progressive);
    set DXes;
    by mrn index_date enc_id;
    retain cardiac cranio derm endo gastro genetic genito hemato immuno  malign mh
        metab musculo neuro opthal otol otolar pulresp renal progressive;

    * preset all the flags to 0 for rollup to one record with designations per claim;
    if first.enc_id then do;
      cardiac     = 0;
      cranio      = 0;
      derm        = 0;
      endo        = 0;
      gastro      = 0;
      genetic     = 0;
      genito      = 0;
      hemato      = 0;
      immuno      = 0;
      malign      = 0;
      mh          = 0;
      metab       = 0;
      musculo     = 0;
      neuro       = 0;
      opthal      = 0;
      otol        = 0;
      otolar      = 0;
      pulresp     = 0;
      renal       = 0;
      progressive = 0;
    end;

    if icd10codeset=0 then do ;
      if dx in: ('010' '011' '012' '013' '014' '015' '016' '017' '018') then
          pulresp=1;

      if dx =:'030' then
          neuro=1;

      if dx =:'0402' then
          gastro=1;

      if dx in: ('042' '043' '044') then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx in: ('046' '0582' '0785') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =:'07953' then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx in: ('090' '094' '095') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =:'135' then
          immuno=1;

      if dx =:'1363' then
          pulresp=1;

      if dx =:'137' then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in: ('138' '139') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in: ('14' '15' '16' '17' '18' '19' '200' '201' '202' '203' '204' '205'
          '206' '207' '208' '2090' '2091' '2092' '2093' '2097' '23877') then
          malign=1;

      if dx =:'2377' then
          neuro=1;

      if dx in: ('240' '241' '242' '243' '244' '245'
          '246' '249' '250' '2510' '252'
          '2530' '2531' '2533' '2534' '2535' '2537' '2538'
          '254' '255' '256' '257' '258') then
          endo=1;

      if dx=:'2532' then
          do;
              endo=1;
              progressive=1;
          end;

      if dx in: ('260' '261' '262'
          '2630' '2632'
          '264' '265' '266' '267'
          '2680' '2681' '2682'
          '273' '274') then
          metab=1;

      if dx in: ('270' '271' '272' '2750') then
          do;
              metab=1;
              progressive=1;
          end;

      if dx =:'2770' then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in: ('2771' '2774' '2777') then
          metab=1;

      if dx in: ('2772' '2773' '2775' '27781' '27782' '27783'
          '27784' '27785' '27786' '27787' '27788') then
          do;
              metab=1;
              progressive=1;
          end;

      if dx =:'2776' then
          immuno=1;

      if dx =:'27801' then
          metab=1;

      if dx =:'279' then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx =:'2810' then
          hemato=1;

      if (dx =:'282' and dx ^=:'2825') then
          hemato=1;

      if dx in: ('2824' '2826') then
          do;
              hemato=1;
              progressive=1;
          end;

      if dx =:'283' then
          hemato=1;

      if dx =:'284' then
          do;
              hemato=1;

              if dx ^=:'2841' then
                  progressive=1;
          end;

      if dx in:('2850' '28521' '28522' '28529' '2858') then
          hemato=1;

      if dx =:'286' then
          do;
              hemato=1;

              if dx in: ('2860' '2861' '2862' '2863') then
                  progressive=1;
          end;

      if dx in:('2871' '2873') then
          do;
              hemato=1;
          end;

      if dx in:('28802' '2885') then
          immuno=1;

      if dx in:('28801' '2881' '2882' '2884') then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx in:('28951' '28952' '28953'
          '28981' '28983' '28989') then
          do;
              immuno=1;
          end;

      if dx in:('2911' '2921' '2940') then
          mh=1;

      if dx in:('2950' '2951' '2952' '2953' '2954' '2955' '2956' '2957' '2958') then
          do;
              mh=1;
              progressive=1;
          end;

      if dx in: (  '296' '2971' '2973'
          '2990' '2991' '2998'
          '3001' '3003' '3007' '30081'
          '3010' '3011' '3012' '3013' '3014' '3015' '3016' '3017' '3018'
          '3039'
          '3040' '3041' '3042' '3044' '3045' '3046' '3047' '3048' '3049'
          '30722' '30723' '3073' '3077') then
          mh=1;

      if dx in:('3071' '30751' ) then
          do;
              mh=1;
              progressive=1;
          end;

      if dx in: ( '3100' '3101' '311'
          '3120' '3121' '3122' '3123' '3124' '3125' '3126' '3127' '31281' '31282' '3129'
          '31381' '3140' '3141' '3142'
          '31501' '31502' '3151' '3152' '31531' '31532' '31534' '3154' '3155' '3158' '3159'
          '317' '3180' '3181' '3182' '319') then
          mh=1;

      if dx =:'326' then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in:('32720' '32721' '32723' '32724' '32725' '32726' '32727' '32729' ) then
          do;
              pulresp=1;

              if dx =:'32725' then
                  progressive=1;
          end;

      if dx =:'330' then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in:('3313' '3314') then
          neuro=1;

      if dx in:('3330' '3332' '3334' '3335' '3336' '33371' '33391') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =:'334' then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in:('335' '336') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =: '337' then
          neuro=1;

      if dx in:('340' '341') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =:'342' then
          neuro=1;

      if dx =:'343' then
          do;
              neuro=1;

              if dx in:('3432' '3438' '3439') then
                  progressive=1;
          end;

      if dx =:'344' then
          do;
              neuro=1;

              if dx =:'3440' then
                  progressive=1;
          end;

      if dx in:('345' '347') then
          neuro=1;

      if dx in:('3481' '3482' '3492' '3499') then
          do;
              neuro=1;

              if dx =:'3481' then
                  progressive=1;
          end;

      if dx in:('350' '351' '352' '353' '354' '355' '356' '357' '358') then
          neuro=1;

      if dx in:('3590' '3591' '3592') then
          do;
              musculo=1;
              progressive=1;
          end;

      if dx in:('3593' '3594' '3595' '3596' '3598' '3599') then
          musculo=1;

      if dx in:('3602' '3603' '3604' '361'
          '3620' '3621' '36226' '36227'
          '36230' '36231' '36232' '36233' '36234' '36235' '36236' '36237'
          '36240' '36241' '36242' '36243'
          '36250' '36251' '36252' '36253' '36254' '36255' '36256' '36257'
          '36260' '36261' '36262' '36263' '36264' '36265' '36266'
          '36270' '36271' '36272' '36273' '36274' '36275' '36276' '36277'
          '36285'
          '363'
          '3641' '3642' '3645' '3647' '3648'
          '365'
          '3690' '3691' '3692' '3693' '3694'
          '36960' '36961' '36962' '36963' '36964' '36965' '36966' '36967' '36968' '36969'
          '36970' '36971' '36972' '36973' '36974' '36975' '36976'
          '377'
          '3780' '3781' '3782' '3783' '3784' '3785' '3786' '3787' '3788'
          '3790' '3791' '3792' '3793' '3794' '3795' '3796') then
          opthal=1;

      if dx in:('385' '386' '387' '3880' '3881' '3883' '3885') then
          otol=1;

      if dx in:('3891' '3892' '3897' '3898' '3899') then
          neuro=1;

      if dx =:'390' then
          immuno=1;

      if dx in:('393' '394' '395' '396' '397' '398' '401' '402') then
          do;
              cardiac=1;

              if dx in:('40201' '40211' '40291') then
                  progressive=1;
          end;

      if dx =:'403' then
          do;
              renal=1;

              if dx in:('40301' '40311' '40391') then
                  progressive=1;
          end;

      if dx =:'404' then
          do;
              renal=1;

              if dx in:('40401' '40411' '40491' '40402' '40403' '40412' '40413' '40492' '40493') then
                  progressive=1;
          end;

      if dx =:'405' then
          cardiac=1;

      if dx in:('410' '411' '412') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx =:'414' and dx ^=:'4144' then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in:('416' '417') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in:('424' '426') then
          cardiac=1;

      if (dx =:'425' and dx ^=:'4259') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in:('4270' '4271' '4273' '4274' '42781') then
          cardiac=1;

      if (dx in:('428' '4291') and dx ^=:'4289') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx =:'4293' then
          cardiac=1;

      if dx in:('433'
          '4372' '4373' '4374' '4375' '4376' '4377'
          '438') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in:('441') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in:('442' '443') then
          cardiac=1;

      if dx =:'446' then
          do;
              immuno=1;

              if dx in:('4460' '4462' '4463') then
                  do;
                      progressive=1;
                  end;
          end;

      if dx in:('447') then
          cardiac=1;

      if dx in:('452' '4530') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in:('45350' '45351' '45352'
          '45371' '45372' '45373' '45374' '45375' '45376' '45377' '45379'
          '4570' '4571' '4572') then
          cardiac=1;

      if dx =:'493' then
          pulresp=1;

      if dx in:('4940' '4941') then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx =:'495' then
          pulresp=1;

      if dx =:'496' then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in:('515' '516') then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in:('5190' '5193' '5194') then
          pulresp=1;

      if dx =:'526' then
          musculo=1;

      if dx in: ('5270' '5271' '5277') then
          gastro=1;

      if dx in:('5300' '53013' '5303' '5305' '5306' '53083' '53084' '53085') then
          gastro=1;

      if dx in: ('531' '532' '533' '534' '5362' '5363') then
          gastro=1;

      if dx in:('555' '556' '5651' '5690' '5691' '5692' '56944' '56981' ) then
          gastro=1;

      if dx in:('5714' '5715' '5716' '5718' '5719') then
          do;
              gastro=1;
              progressive=1;
          end;

      if dx in:('5723' '5724' '5730') then
          do;
              gastro=1;
              progressive=1;
          end;

      if dx in:('5732' '5734' '5738' '5739'
          '57511' '5755' '5756' '5758'
          '5760' '5761' '5764' '5765' '5768'
          '5771' '5772' '5778'
          '5790' '5791' '5792' '5794') then
          gastro=1;

      if dx =:'581' then
          renal=1;

      if dx in:('582' '583' '585' '586') then
          do;
              renal=1;
              progressive=1;
          end;

      if dx =:'5880' then
          do;
              musculo=1;
              progressive=1;
          end;

      if dx =:'5881' then
          renal=1;

      if dx =:'591' then
          genito=1;

      if dx in:('59371' '59372' '59373' '59382'
          '5960'  '5961'  '5962'   '5964'
          '59652' '59653' '59654' '59655'
          '598'
          '5991' '5992' '5993' '5994' '5995'
          '59981' '59982' '59983') then
          renal=1;

      if dx in:(
          '60785'
          '6083'
          '617' '618' '619'
          '6221'
          '6230'
          '6240'
          '62920' '62921' '62922' '62923' '62929') then
          genito=1;

      if dx =:'694' then
          derm=1;

      if dx =:'6954' then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx in:('7010' '7018' '7050' '707') then
          derm=1;

      if dx =:'710' then
          do;
              immuno=1;

              if dx in:('7100' '7108' '7109') then
                  do;
                      progressive=1;
                  end;
          end;

      if dx in:('712' '714') then
          immuno=1;

      if dx in:('717'
          '7180' '7182' '7183' '7184' '7185' '7186' '7187'
          '7220' '7221' '7222' '7223' '7224' '7225' '7226' '7227' '7228') then
          musculo=1;

      if dx in:('720' '721' '725') then
          immuno=1;

      if dx in:('7281' '7282' '7286' '7287') then
          musculo=1;

      if dx =:'7283' then
          do;
              musculo=1;
              progressive=1;
          end;

      if dx in:('7301'
          '7310' '7311' '7312' '7313' '7318'
          '7320' '7321'
          '7330' '7333' '7334' '7337' ) then
          musculo=1;

      if dx in:('73605' '73606' '73607' '73631' '73632'
          '73671' '73672' '73673' '73674' '73675'
          '73681') then
          musculo=1;

      if dx in:('7370' '7371' '7373' '7378' '7379' '7384' '7385' '7386' ) then
          musculo=1;

      if dx in:('7400' '7401' '7402' '741') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =:'742' then
          do;
              neuro=1;

              if (dx ^=:'7423') then
                  progressive=1;
          end;

      if dx in:('7430' '7431' '7432' '7434' '7435'
          '74361' '74362' '74363' '74366' '74369') then
          do;
              opthal=1;
          end;

      if dx in:('7440' '7442' '7443' '7444' '7449') then
          do;
              otolar=1;
          end;

      if dx in:('7450' '7451' '7452' '7453' '7456' '7457' '7458' '7459') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in:('7454' '7455') then
          cardiac=1;

      if (dx =:'746' and dx ^=:'7469') then
          do;
              cardiac=1;

              if dx in:('7462' '7467') then
                  progressive=1;
          end;

      if dx =:'7474' then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in:('7471' '74721' '74722' '74729' '7473' '74781' '74783' '74789') then
          cardiac=1;

      if dx =:'748' then
          do;
              pulresp=1;

              if dx in:('7484' '7485' '7486') then
                  progressive=1;
          end;

      if dx =:'749' then
          cranio=1;

      if dx in:('7501' '7502' '7503' '7504' '7507' '7509') then
          gastro=1;

      if dx in:('7511' '7512' '7513' '7514' '7515' '75160' '7518' '7519') then
          do;
              gastro=1;
          end;

      if dx in:('75161' '75162' '75169' '7517' ) then
          do;
              gastro=1;
              progressive=1;
          end;

      if  dx in: ('75261' '75262' '7527') then
          genito=1;

      if  dx in:('7530' '7531') then
          do;
              genito=1;
              progressive=1;
          end;

      if  dx in:('7532' '7534' '7535' '7536' '7537' '7538' '7539') then
          genito=1;

      if  dx in:('7540' '7542'
          '75430' '75431' '75432' '75433' '75434' '75435'
          '7547'
          '7552' '7553' '7554' '75553' '75554' '75558') then
          musculo=1;

      if dx in:('7560' '7561' '7563' '7564' '7565' '75683' '75689' '7569') then
          musculo=1;

      if dx in:('7566' '7567') then
          do;
              musculo=1;
              progressive=1;
          end;

      if dx =:'7570' then
          derm=1;

      if  dx in:('7571' '75731') then
          do;
              derm=1;
              progressive=1;
          end;

      if dx in:('7580' '7581' '7582' '7583' '7585' '7586' '7587' '7588' '7589') then
          do;
              genetic=1;

              if dx in:('7581' '7582' '75831' '75833') then
                  progressive=1;
          end;

      if dx =:'759' then
          do;
              genetic=1;

              if dx in:('7590' '7591' '7592' '7593' '7594' '7596') then
                  progressive=1;
          end;

      if dx =:'7707' then
          pulresp=1;

      if dx in:('78001' '78003') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in:('78051' '78053' '78057') then
          neuro=1;

      if dx in:('78833' '78834' '78837' '78838' '78839') then
          genito=1;

      if dx in:('887' '896' '897') then
          musculo=1;

      if dx in:('9066' '9067' '9068') then
          musculo=1;

      if dx =:'952' then
          do;
              neuro   =1;
              progressive=1;
          end;

      if dx =:'V08' then
          immuno  =1;

      if dx =:'V151' then
          cardiac=1;

      if dx =:'V420' then
          do;
              renal   =1;
              progressive=1;
          end;

      if dx =:'V421' then
          do;
              cardiac =1;
              progressive=1;
          end;

      if dx =:'V422' then
          do;
              cardiac =1;
          end;

      if dx =:'V426' then
          do;
              pulresp =1;
              progressive=1;
          end;

      if dx in:('V427' 'V4284') then
          do;
              gastro=1;
              progressive=1;
          end;

      if dx =:'V4281' then
          do;
              hemato  =1;
              progressive=1;
          end;

      if dx =:'V4283' then
          do;
              endo    =1;
              progressive=1;
          end;

      if dx =:'V4322' then
          do;
              cardiac =1;
              progressive=1;
          end;

      if dx in:('V520' 'V521') then
          musculo=1;

      if dx in:('V530' 'V532') then
          neuro   =1;

      if dx =:'V533' then
          cardiac=1;

      if dx =:'V535' then
          gastro  =1;

      if dx =:'V550' then
          pulresp=1;

      if dx in:('V551' 'V552' 'V553' 'V554') then
          gastro  =1;

      if dx in:('V555' 'V556' 'V557') then
          genito =1;

      if dx in:('V560' 'V561' 'V562' 'V568') then
          do;
              renal=1;
              progressive=1;
          end;

      if dx in:('V5781' 'V5789') then
          musculo=1;
    end ;
    else if icd10codeset=1 then do;
      if dx =:'A15' then
          pulresp=1;

      if dx in: ('A171' 'A1781' 'A1783') then
          neuro=1;

      if dx in: ('A170' 'A1782' 'A1789' 'A179') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =:'A180' then
          musculo=1;

      if dx =:'A181' then
          genito=1;

      if dx in:('A182' 'A1885') then
          immuno=1;

      if dx =:'A183' then
          gastro=1;

      if dx =:'A184' then
          derm=1;

      if dx =:'A185' then
          opthal=1;

      if dx =:'A186' then
          otol=1;

      if dx in: ('A187' 'A1881' 'A1882') then
          endo=1;

      if dx in: ('A1889' 'A19') then
          pulresp=1;

      if dx =:'A30' then
          neuro=1;

      if dx in: ('A50' 'A521' 'A522' 'A523' 'A527' 'A528'
          'A529' 'A53' 'A81' 'B100' 'B900' 'B91' ) then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx =:'B20' then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx =:'B901' then
          genito=1;

      if dx =:'B902' then
          musculo=1;

      if dx in:('B908' 'B909') then
          pulresp=1;

      if dx =:'B9735' then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx in: ('C0' 'C1' 'C2' 'C3' 'C40' 'C41'
          'C43' 'C4A' 'C44' 'C46' 'C47' 'C48'
          'C49' 'C5' 'C6' 'C7' 'C8' 'C90'
          'C910' 'C911' 'C913' 'C914' 'C916'
          'C919' 'C91Z' 'C92' 'C93' 'C940'
          'C942' 'C943' 'C948' 'C95' 'C96'
          'D03' 'D45' 'D474' 'D47Z1') then
          malign=1;

      if dx in: ('D510' 'D511' 'D55' 'D565' 'D568'
          'D569' 'D58' 'D591' 'D594' 'D599' 'D600'
          'D608' 'D609' 'D63' 'D640' 'D641' 'D642' 'D643'
          'D6489' 'D680' 'D681' 'D682' 'D68312'
          'D68318' 'D685' 'D6861' 'D6862' 'D688'
          'D689' 'D691' 'D693' 'D6941' 'D6949' 'D7589' ) then
          hemato=1;

      if dx in: ('D560' 'D561' 'D570' 'D571' 'D572'
          'D574' 'D578' 'D610' 'D61818' 'D6182'
          'D6189' 'D619' 'D644' 'D66' 'D67'
          'D68311' 'D6942' 'D7581') then
          do;
              hemato=1;
              progressive=1;
          end;

      if dx in: ('D704' 'D720' 'D763' 'D802' 'D803'
          'D804' 'D805' 'D808' 'D838' 'D839'
          'D841' 'D849' 'D86' 'D890' 'D891'
          'D892' 'D8989' 'D899') then
          immuno=1;

      if dx in: ('D700' 'D71' 'D761' 'D800' 'D801'
          'D810' 'D811' 'D812' 'D814' 'D8189' 'D819'
          'D831' 'D89811' 'D89813') then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx in: ('D820' 'D8982') then
          genetic=1;

      if dx =:'D821' then
          do;
              genetic=1;
              progressive=1;
          end;

      if dx in: ('E018' 'E030' 'E031' 'E032' 'E034'
          'E038' 'E039' 'E04' 'E062' 'E063' 'E064' 'E065'
          'E069' 'E070' 'E071' 'E0789' 'E079' 'E08'
          'E09' 'E10' 'E11' 'E13' 'E209' 'E21'
          'E220' 'E228' 'E229' 'E232' 'E236' 'E243'
          'E248' 'E249' 'E25' 'E260' 'E261' 'E2681'
          'E269' 'E270' 'E271' 'E274' 'E275' 'E278'
          'E279' 'E28' 'E29' 'E310' 'E318' 'E319'
          'E45') then
          endo=1;

      if dx in: ('E00' 'E230') then
          do;
              endo=1;
              progressive=1;
          end;

      if dx =: 'E312' then
          malign=1;

      if dx in: ('E40' 'E43' 'E440' 'E50' 'E52' 'E53' 'E54'
          'E550' 'E643' 'E6601' 'E800' 'E8020'
          'E8029' 'E805' 'E880' 'E888') then
          metab=1;

      if dx =: 'E45' then
          neuro=1;

      if dx in: ('E700' 'E7021' 'E7029' 'E7040' 'E705'
          'E708' 'E710' 'E71120' 'E7119' 'E712'
          'E7131' 'E7141' 'E7142' 'E7144' 'E7150'
          'E71510' 'E71511' 'E71522' 'E71529'
          'E71548' 'E7200' 'E7203' 'E7204' 'E7209'
          'E7210' 'E7211' 'E7219' 'E7220' 'E7222'
          'E7223' 'E7229' 'E723' 'E728' 'E729' 'E7400'
          'E7401' 'E7404' 'E7409' 'E7421' 'E7439'
          'E744' 'E748' 'E749' 'E7502' 'E7519' 'E7521'
          'E7522' 'E7523' 'E75249' 'E7525' 'E7529'
          'E754' 'E7601' 'E7603' 'E761' 'E76219'
          'E7622' 'E7629' 'E763' 'E770' 'E771'
          'E786' 'E7870' 'E7871' 'E7872' 'E788' 'E789'
          'E791' 'E798' 'E83' 'E85' 'E881' 'E884') then
          do;
              metab=1;
              progressive=1;
          end;

      if dx in: ('E84') then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in: ('E890' 'E894' 'E895') then
          endo=1;

      if dx in: ('F04' 'F070' 'F1020' 'F1021' 'F1096'
          'F1120' 'F1121' 'F1320' 'F1321' 'F1420'
          'F1421' 'F1520' 'F1521' 'F1620' 'F1621'
          'F1820' 'F1821' 'F1920' 'F1921' 'F21'
          'F22' 'F24' 'F31' 'F32'
          'F33' 'F340' 'F341' 'F39' 'F429'
          'F444' 'F445' 'F446' 'F447' 'F448'
          'F449' 'F450' 'F4521' 'F4522' 'F60'
          'F63' 'F6812'
          'F840' 'F843' 'F845' 'F848' 'F849'
          'F88' 'F89' 'F90' 'F911' 'F912'
          'F913' 'F918' 'F919' 'F951' 'F952'
          'F981' 'F984' ) then
          mh=1;

      if dx in: ('F200' 'F201' 'F202' 'F203' 'F205' 'F208' 'F25'
          'F50' ) then
          do;
              mh=1;
              progressive=1;
          end;

      if dx in: ('F7' 'F801' 'F802' 'F804' 'F81' 'F82'
          'G110' 'G255' 'G371' 'G372' 'G400'
          'G401' 'G402' 'G403' 'G404' 'G405'
          'G4080' 'G4082' 'G409' 'G40A' 'G40B'
          'G474' 'G50' 'G510' 'G511' 'G512'
          'G518' 'G519' 'G52' 'G54' 'G56' 'G57'
          'G587' 'G588' 'G589' 'G600' 'G603'
          'G608' 'G609' 'G6181' 'G6189' 'G619'
          'G620' 'G621' 'G622' 'G63' 'G7000'
          'G701' 'G702' 'G7080' 'G7089' 'G709'
          'G7114' 'G7119' 'G712' 'G722' 'G723'
          'G7289' 'G729' 'G733' 'G737' 'G801'
          'G802' 'G803' 'G804' 'G81' 'G822'
          'G825' 'G830' 'G831' 'G832' 'G833'
          'G834' 'G835' 'G8381' 'G8389' 'G839'
          'G900' 'G904' 'G905' 'G908' 'G909'
          'G910' 'G911' 'G932' 'G9389' 'G939'
          'G969' 'G990' ) then
          neuro=1;

      if dx in: ('F842' 'G09' 'G10' 'G111' 'G113'
          'G114' 'G118' 'G119' 'G12'
          'G14' 'G23' 'G241' 'G253' 'G2582'
          'G3181' 'G3182' 'G319' 'G320'
          'G3281' 'G35' 'G360' 'G370' 'G375'
          'G378' 'G379' 'G4081' 'G601' 'G710'
          'G7111' 'G7112' 'G7113' 'G800' 'G808'
          'G809' 'G901' 'G931' 'G950' 'G9519'
          'G9589' 'G959' 'G992'  ) then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in: ('G4730' 'G4731' 'G4733' 'G4734' 'G4736' 'G4737' 'G4739' ) then
          pulresp=1;

      if dx in: ('G4735' ) then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in: ('H150' 'H158' 'H201' 'H202'
          'H208' 'H209' 'H212' 'H2150' 'H2151'
          'H2152' 'H2154' 'H2155' 'H2156'
          'H218' 'H270' 'H2711' 'H2712' 'H2713'
          'H278' 'H300' 'H301' 'H302' 'H3081'
          'H309' 'H310' 'H3110' 'H3112' 'H312'
          'H313' 'H314' 'H318' 'H319' 'H330'
          'H3310' 'H3319' 'H332' 'H333' 'H334'
          'H338' 'H34' 'H350' 'H3515' 'H3516' 'H3517'
          'H352' 'H3530' 'H3533' 'H3534' 'H3535' 'H3536'
          'H3537' 'H3538' 'H3540' 'H3541' 'H3542'
          'H3543' 'H3545' 'H3546' 'H355' 'H357'
          'H3589' 'H36' 'H401' 'H402' 'H403'
          'H404' 'H405' 'H406' 'H408' 'H409' 'H42'
          'H430' 'H432' 'H433' 'H4381' 'H4389'
          'H4430' 'H4440' 'H4450' 'H46' 'H4701'
          'H4703' 'H4709' 'H4714' 'H472' 'H4731'
          'H4732' 'H4739' 'H474' 'H475' 'H476'
          'H479' 'H490' 'H491' 'H492' 'H493'
          'H494' ) then
          opthal=1;

      if dx in: ('H4981' ) then
          do;
              metab=1;
              progressive=1;
          end;

      if dx in: ('H4988' 'H5000' 'H5005' 'H5006'
          'H5007' 'H5008' 'H5010' 'H5015' 'H5016'
          'H5017' 'H5018' 'H5030' 'H5032' 'H5034'
          'H5040' 'H5042' 'H5043' 'H505' 'H5060'
          'H5069' 'H5089' 'H51' 'H540' 'H541'
          'H542' 'H543' 'H548' 'H550' 'H57') then
          opthal=1;

      if dx in: ('H71' 'H74' 'H80' 'H81' 'H83'
          'H903' 'H905' 'H906' 'H908'
          'H913' 'H918X3' 'H918X9' 'H9190'
          'H9193' 'H93093' 'H93099' 'H9313'
          'H9319' 'H9325' 'H933X3' 'H933X9') then
          otol=1;

      if dx in: ('I00' 'I05' 'I06' 'I07'
          'I080' 'I088' 'I089' 'I09'
          'I10' 'I119' 'I340' 'I348' 'I35'
          'I370' 'I378' 'I44' 'I4510' 'I4519' 'I452'
          'I453' 'I454' 'I455' 'I456' 'I458'
          'I459' 'I471' 'I472' 'I48' 'I4901'
          'I517' 'I720' 'I721' 'I722' 'I723'
          'I724' 'I728' 'I729' 'I7300' 'I7301'
          'I7381' 'I7389' 'I739' 'I770' 'I771'
          'I773' 'I774' 'I775' 'I776' 'I778'
          'I779' 'I798' 'I822' 'I825'
          'I82709' 'I82719' 'I82729' 'I82891'
          'I82A29' 'I82B29' 'I82C29' 'I890') then
          cardiac=1;

      if dx in: ('I110' 'I200' 'I21' 'I24'
          'I2510' 'I252' 'I253' 'I254' 'I255'
          'I258' 'I259' 'I270' 'I271' 'I272'
          'I278' 'I279' 'I280' 'I281' 'I288' 'I289'
          'I421' 'I422' 'I423' 'I424'
          'I425' 'I426' 'I428' 'I43' 'I4902'
          'I50' 'I515' 'I712' 'I714' 'I716'
          'I719' 'I81' 'I820') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in: ('I150' 'I158' ) then
          renal=1;

      if dx in: ('I12' 'I13' ) then
          do;
              renal=1;
              progressive=1;
          end;

      if dx in: ('I69898' 'I699') then
          neuro=1;

      if dx in: ('I630' 'I631' 'I632' 'I65' 'I671'
          'I674' 'I675' 'I676' 'I677' ) then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in: ('J45' 'J47' 'J84842' 'J950' 'J985' 'J986' ) then
          pulresp=1;

      if dx in: ('J840' 'J8410' 'J84111' 'J84112'
          'J84117' 'J842' 'J8483' 'J84841' 'J84843'
          'J84848' 'J8489' 'J849') then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in: ('K110' 'K111' 'K117' 'K200' 'K220'
          'K224' 'K225' 'K227' 'K228' 'K254'
          'K255' 'K256' 'K257' 'K264' 'K265'
          'K266' 'K267' 'K274' 'K275' 'K276'
          'K277' 'K284' 'K285' 'K286' 'K287'
          'K3184' 'K50' 'K510' 'K512' 'K513'
          'K518' 'K519' 'K624' 'K6282' 'K632'
          'K763' 'K7689' 'K769' 'K77' 'K811'
          'K823' 'K824' 'K828' 'K830' 'K833'
          'K834' 'K835' 'K838' 'K861' 'K862'
          'K863' 'K868' 'K900' 'K901' 'K902'
          'K903' 'K9081' 'K915') then
          gastro=1;

      if dx in: ('K73' 'K74' 'K754' 'K761' 'K766'
          'K767' ) then
          do;
              gastro=1;
              progressive=1;
          end;

      if dx in: ('L100' 'L101' 'L102' 'L104' 'L109'
          'L120' 'L121' 'L122' 'L128' 'L13'
          'L574' 'L744' 'L89' 'L904' 'L940'
          'L943' 'L97' 'L984' 'L988') then
          derm=1;

      if dx in: ('M050' 'M051' 'M0530' 'M0560' 'M060'
          'M062' 'M063' 'M068' 'M069' 'M08' 'M111'
          'M112' 'M118' 'M119' 'M120'
          'M303' 'M311' 'M313' 'M314' 'M316'
          'M33' 'M3500' 'M3501' 'M353' 'M45'
          'M461' 'M468' 'M469' 'M481') then
          immuno=1;

      if dx in: ('L930' 'L932' 'M300' 'M310' 'M312'
          'M321' 'M340' 'M341' 'M349' 'M355'
          'M358' 'M359') then
          do;
              immuno=1;
              progressive=1;
          end;

      if dx in: ('M100' 'M103' 'M104' 'M109' 'M1A0' 'M1A3' 'M1A4' 'M1A9') then
          metab=1;

      if dx in: ('M2105' 'M2115' 'M2133' 'M2137' 'M215'
          'M216X' 'M2175' 'M2176' 'M232' 'M233'
          'M241' 'M242' 'M243' 'M244' 'M245'
          'M246' 'M247' 'M248' 'M278' 'M400'
          'M4020' 'M41' 'M420' 'M430' 'M431'
          'M438' 'M460' 'M471' 'M4781' 'M482'
          'M483' 'M489' 'M498' 'M500' 'M502'
          'M503' 'M5106' 'M513' 'M514' 'M519'
          'M61' 'M625' 'M6289' 'M720' 'M722'
          'M816' 'M818' 'M852' 'M863' 'M864'
          'M865' 'M866' 'M870' 'M88'
          'M890' 'M894' 'M897' 'M908'
          'M918' 'M955' 'M961' 'M998') then
          musculo=1;

      if dx in: ('M623' 'M906' 'N250') then
          do;
              musculo=1;
              progressive=1;
          end;

      if dx in: ('N02' 'N04' 'N05' 'N08' 'N13' 'N1372' 'N251'
          'N2889' 'N312' 'N318' 'N319' 'N320'
          'N321' 'N322' 'N3501' 'N35028' 'N351'
          'N358' 'N359' 'N360' 'N361' 'N362'
          'N364' 'N365' 'N368' 'N37' 'N3942'
          'N3945' 'N3946' 'N39490' 'N39498') then
          renal=1;

      if dx in: ('N03' 'N18' 'N19') then
          do;
              renal=1;
              progressive=1;
          end;

      if dx in: ('N500' 'N80' 'N810' 'N811' 'N812'
          'N813' 'N814' 'N815' 'N816' 'N8181'
          'N8182' 'N8183' 'N8184' 'N8189' 'N819'
          'N820' 'N821' 'N824' 'N825' 'N828'
          'N829' 'N87' 'N880' 'N893' 'N894' 'N900' 'N901'
          'N904' 'N9081' 'N99110' 'N99111'
          'N99112' 'N99113' 'N99114' ) then
          genito=1;

      if dx in: ('P270' 'P271' 'P278' ) then
          pulresp=1;

      if dx in: ('P293') then
          cardiac=1;

      if dx in: ('Q030' 'Q031' 'Q038' 'Q0702') then
          neuro=1;

      if dx in: ('Q00' 'Q01' 'Q02' 'Q041' 'Q042'
          'Q043' 'Q045' 'Q048' 'Q05' 'Q060'
          'Q061' 'Q062' 'Q063' 'Q064' 'Q068'
          'Q0701' 'Q0703' 'Q078' 'Q079') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in: ('Q100' 'Q103' 'Q107' 'Q110' 'Q111'
          'Q112' 'Q130' 'Q131' 'Q132' 'Q133'
          'Q134' 'Q135' 'Q1381' 'Q1389' 'Q140'
          'Q141' 'Q142' 'Q143' 'Q148' 'Q150') then
          opthal=1;

      if dx in: ('Q16' 'Q171' 'Q172' 'Q174' 'Q178' 'Q179' 'Q180' 'Q181' 'Q182' 'Q189') then
          otol=1;

      if dx in: ('Q210' 'Q211' 'Q220' 'Q221' 'Q222'
          'Q223' 'Q229' 'Q230' 'Q231' 'Q232'
          'Q233' 'Q238' 'Q242' 'Q243' 'Q244'
          'Q245' 'Q246' 'Q248' 'Q251' 'Q252'
          'Q253' 'Q254' 'Q255' 'Q256' 'Q257'
          'Q269' 'Q282' 'Q283' 'Q288') then
          cardiac=1;

      if dx in: ('Q200' 'Q201' 'Q202' 'Q203' 'Q204' 'Q205'
          'Q208' 'Q212' 'Q213' 'Q218' 'Q219'
          'Q225' 'Q234') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in: ('Q300' 'Q301' 'Q308' 'Q310' 'Q311'
          'Q318' 'Q321' 'Q324' 'Q332' 'Q338'
          'Q339' 'Q34') then
          pulresp=1;

      if dx in: ('Q330' 'Q333' 'Q334' 'Q336' ) then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in: ('Q351' 'Q353' 'Q355' 'Q359' 'Q36' 'Q37' 'Q385') then
          cranio=1;

      if dx in: ('Q382' 'Q383' 'Q384' 'Q388' 'Q390'
          'Q391' 'Q392' 'Q393' 'Q394' 'Q395'
          'Q396' 'Q398' 'Q402' 'Q409' 'Q41'
          'Q42' 'Q431' 'Q433' 'Q437' 'Q438'
          'Q441' 'Q458' 'Q459') then
          gastro=1;

      if dx in: ('Q442' 'Q443' 'Q445' 'Q446' 'Q447'
          'Q450') then
          do;
              gastro=1;
              progressive=1;
          end;

      if dx in: ('Q540' 'Q541' 'Q542' 'Q543' 'Q548'
          'Q549' 'Q563' 'Q564' 'Q6101' 'Q6210'
          'Q6211' 'Q6212' 'Q6231' 'Q6239' 'Q624'
          'Q625' 'Q6261' 'Q6262' 'Q6263' 'Q628'
          'Q640' 'Q6410' 'Q6419' 'Q6431' 'Q644'
          'Q645' 'Q646' 'Q6471' 'Q6473' 'Q6474'
          'Q6475' 'Q6479' 'Q649') then
          genito=1;

      if dx in: ('Q600' 'Q601' 'Q602' 'Q603' 'Q604'
          'Q605' 'Q6100' 'Q6102' 'Q6119'
          'Q612' 'Q613' 'Q614' 'Q615' 'Q618'
          'Q619') then
          do;
              genito=1;
              progressive=1;
          end;

      if dx in: ('Q650' 'Q651' 'Q652' 'Q653' 'Q654'
          'Q655' 'Q667' 'Q6689' 'Q670' 'Q671'
          'Q672' 'Q673' 'Q674' 'Q675' 'Q688'
          'Q710' 'Q711' 'Q712' 'Q713' 'Q714'
          'Q715' 'Q716' 'Q7189' 'Q719' 'Q720'
          'Q721' 'Q722' 'Q723' 'Q724' 'Q725'
          'Q726' 'Q727' 'Q7289' 'Q73' 'Q740'
          'Q760' 'Q761' 'Q762' 'Q763' 'Q764'
          'Q774' 'Q776' 'Q778' 'Q780'
          'Q781' 'Q782' 'Q783' 'Q784' 'Q788'
          'Q789' 'Q796' 'Q798' 'Q799') then
          musculo=1;

      if dx in: ('Q771' 'Q790' 'Q791' 'Q792' 'Q793'
          'Q794' 'Q7959') then
          do;
              musculo=1;
              progressive=1;
          end;

      if dx in: ('Q750' 'Q759') then
          cranio=1;

      if dx in: ('Q7951') then
          genito=1;

      if dx in: ('Q803' 'Q804' 'Q809' 'Q824') then
          do;
              derm=1;
              progressive=1;
          end;

      if dx in: ('Q820') then
          derm=1;

      if dx in: ('Q850') then
          neuro=1;

      if dx in: ('Q851' 'Q858' 'Q871' 'Q872' 'Q873'
          'Q8740' 'Q875' 'Q8789' 'Q897' 'Q898'
          'Q899' 'Q90' 'Q933' 'Q937' 'Q9381'
          'Q9389' 'Q96' 'Q970' 'Q971' 'Q972' 'Q978'
          'Q980' 'Q981' 'Q984' 'Q985' 'Q987'
          'Q988' 'Q992' 'Q998' 'Q999') then
          genetic=1;

      if dx in: ('Q8781' 'Q8901' 'Q891' 'Q892' 'Q893'
          'Q894' 'Q913' 'Q917' 'Q928' 'Q934'
          'Q9388') then
          do;
              genetic=1;
              progressive=1;
          end;

      if dx in: ('R4020' 'R403' 'S1410' 'S1411' 'S1412'
          'S1413' 'S1415' 'S2410' 'S2411' 'S2413'
          'S2415' 'S3410' 'S3411' 'S3412' 'S3413'
          'S343') then
          do;
              neuro=1;
              progressive=1;
          end;

      if dx in: ('S4801' 'S4802' 'S4811' 'S4812'
          'S4891' 'S4892' 'S5801' 'S5802'
          'S5811' 'S5812' 'S5891' 'S5892'
          'S6841' 'S6842' 'S6871' 'S6872'
          'S7801' 'S7802' 'S7811' 'S7812'
          'S7891' 'S7892' 'S8801' 'S8802'
          'S8811' 'S8812' 'S8891' 'S8892'
          'S9801' 'S9802' 'S9831' 'S9832'
          'S9891' 'S9892') then
          musculo=1;

      if dx in: ('Z21') then
          immuno=1;

      if dx in: ('Z430') then
          pulresp=1;

      if dx in: ('Z431' 'Z432' 'Z433' 'Z434' 'Z465') then
          gastro=1;

      if dx in: ('Z435' 'Z436' 'Z437') then
          genito=1;

      if dx in: ('Z440' 'Z441') then
          musculo=1;

      if dx in: ('Z450' 'Z953') then
          cardiac=1;

      if dx in: ('Z4531') then
          opthal=1;

      if dx in: ('Z45328' 'Z454' 'Z461' 'Z462') then
          neuro=1;

      if dx in: ('Z49' 'Z940') then
          do;
              renal=1;
              progressive=1;
          end;

      if dx in: ('Z941' 'Z95812') then
          do;
              cardiac=1;
              progressive=1;
          end;

      if dx in: ('Z942') then
          do;
              pulresp=1;
              progressive=1;
          end;

      if dx in: ('Z944' 'Z9481' 'Z9482') then
          do;
              gastro=1;
              progressive=1;
          end;

      if dx in: ('Z9483') then
          do;
              endo=1;
              progressive=1;
          end;
    end;

    if last.ENC_ID then output;
  run;

  /*  Roll up to one record per child/index date, with a single flag for each body type, a sum across claims for
    each body type, and presence of a progressive condition or malignancy.
  Calculate final condition determinations. */
  data &outset(
    keep=MRN
        index_date
        anyprogressive
        anymalign
        anycardiac anycranio anyderm anyendo anygastro anygenetic anygenito anyhemato anyimmuno anymalign
        anymetab anymusculo anyneuro anyopthal anyotol anyotolar anypulresp anyrenal anymh
        anyprogressive
        cond_less
        cond_more);
    set flagDXes;
    by MRN index_date;
      /*
        scount_less
        scount_more
      */
    retain
      anycardiac anycranio anyderm anyendo anygastro anygenetic anygenito
      anyhemato anyimmuno anymalign anymetab anymusculo anyneuro anyopthal
      anyotol anyotolar anypulresp anyrenal anymh anyprogressive

      anycardiac2 anycranio2 anyderm2 anyendo2 anygastro2 anygenetic2
      anygenito2 anyhemato2 anyimmuno2 anymalign2 anymetab2 anymusculo2
      anyneuro2 anyopthal2 anyotol2 anyotolar2 anypulresp2 anyrenal2
      anymh2

      cardiac2h  cranio2h  derm2h  endo2h  gastro2h  genetic2h  genito2h
      hemato2h  immuno2h  malign2h metab2h  musculo2h  neuro2h  opthal2h
      otol2h  otolar2h  pulresp2h  renal2h  mh2h
    ;

    length cond_less cond_more  $24.;

    if first.index_date then do;
      anycardiac     = 0;
      anycranio      = 0;
      anyderm        = 0;
      anyendo        = 0;
      anygastro      = 0;
      anygenetic     = 0;
      anygenito      = 0;
      anyhemato      = 0;
      anyimmuno      = 0;
      anymalign      = 0;
      anymetab       = 0;
      anymusculo     = 0;
      anyneuro       = 0;
      anyopthal      = 0;
      anyotol        = 0;
      anyotolar      = 0;
      anypulresp     = 0;
      anyrenal       = 0;
      anymh          = 0;
      anyprogressive = 0;
      anycardiac2    = 0;
      anycranio2     = 0;
      anyderm2       = 0;
      anyendo2       = 0;
      anygastro2     = 0;
      anygenetic2    = 0;
      anygenito2     = 0;
      anyhemato2     = 0;
      anyimmuno2     = 0;
      anymalign2     = 0;
      anymetab2      = 0;
      anymusculo2    = 0;
      anyneuro2      = 0;
      anyopthal2     = 0;
      anyotol2       = 0;
      anyotolar2     = 0;
      anypulresp2    = 0;
      anyrenal2      = 0;
      anymh2         = 0;
      cardiac2h      = 0;
      cranio2h       = 0;
      derm2h         = 0;
      endo2h         = 0;
      gastro2h       = 0;
      genetic2h      = 0;
      genito2h       = 0;
      hemato2h       = 0;
      immuno2h       = 0;
      malign2h       = 0;
      metab2h        = 0;
      musculo2h      = 0;
      neuro2h        = 0;
      opthal2h       = 0;
      otol2h         = 0;
      otolar2h       = 0;
      pulresp2h      = 0;
      renal2h        = 0;
      mh2h           = 0;
    end;

    *if a body system is indicated, create
    1) a flag indicating the presence of that body system involvement (indicator y/n), and
    2) the number of claims with that body system indicated (sum across claims);

    *indicator y/n;
    *sum across claims;
    if cardiac       = 1 then
        do;
            anycardiac       = 1;
            anycardiac2 + 1;
        end;

    if cranio        = 1 then
        do;
            anycranio        = 1;
            anycranio2  + 1;
        end;

    if derm          = 1 then
        do;
            anyderm          = 1;
            anyderm2    + 1;
        end;

    if endo          = 1 then
        do;
            anyendo          = 1;
            anyendo2    + 1;
        end;

    if gastro        = 1 then
        do;
            anygastro        = 1;
            anygastro2  + 1;
        end;

    if genetic       = 1 then
        do;
            anygenetic       = 1;
            anygenetic2 + 1;
        end;

    if genito        = 1 then
        do;
            anygenito        = 1;
            anygenito2  + 1;
        end;

    if hemato        = 1 then
        do;
            anyhemato        = 1;
            anyhemato2  + 1;
        end;

    if immuno        = 1 then
        do;
            anyimmuno        = 1;
            anyimmuno2  + 1;
        end;

    if metab         = 1 then
        do;
            anymetab         = 1;
            anymetab2   + 1;
        end;

    if musculo       = 1 then
        do;
            anymusculo       = 1;
            anymusculo2 + 1;
        end;

    if neuro         = 1 then
        do;
            anyneuro         = 1;
            anyneuro2   + 1;
        end;

    if pulresp       = 1 then
        do;
            anypulresp       = 1;
            anypulresp2 + 1;
        end;

    if renal         = 1 then
        do;
            anyrenal         = 1;
            anyrenal2   + 1;
        end;

    if opthal        = 1 then
        do;
            anyopthal        = 1;
            anyopthal2  + 1;
        end;

    if otol          = 1 then
        do;
            anyotol          = 1;
            anyotol2    + 1;
        end;

    if otolar        = 1 then
        do;
            anyotolar        = 1;
            anyotolar2  + 1;
        end;

    if mh            = 1 then
        do;
            anymh            = 1;
            anymh2      + 1;
        end;

    if progressive   = 1 then
        do;
            anyprogressive   = 1;
        end;

    if malign        = 1 then
        do;
            anymalign        = 1;
        end;

    *roll up to last observation;
    if last.index_date then do;

      *******************************************************************************
      CONDITION DETERMINATION--calculate condition type based on two different algorithms
      *******************************************************************************;

      *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      LESS CONSERVATIVE ALGORITHM
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      The less conservative version (cond_less) calculates values as

      'Complex Chronic':  1) more than one body system is involved, or
                          2) one or more conditions are progressive, or
                          3) one or more conditions are malignant

      'Non-complex Chronic': 1) only one body system is involved, and
                             2) the condition is not progressive or malignant

      'Non-Chronic':      1) no body system indicators are present, and
                          2) the condition is not progressive or malignant

      *count number of different body systems involved;
      scount_less = anycardiac + anycranio + anyderm   + anyendo  + anygastro  + anygenetic +
          anygenito  + anyhemato + anyimmuno + anymetab + anymusculo + anyneuro   +
          anypulresp + anyrenal  + anyopthal + anyotol  + anyotolar  + anymh;

      *set condition based on less conservative algorithm;
      if scount_less     >= 2 or
          anyprogressive   = 1 or
          anymalign        = 1 then
          cond_less = '3 Complex Chronic';
      else if scount_less      = 1         then
          cond_less = '2 Non-complex Chronic';
      else cond_less = '1 Non-Chronic';

      *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      MORE CONSERVATIVE ALGORITHM
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      The more conservative version (cond_more) calculates values as

      'Complex Chronic':  1) more than one body system is involved,
                             and each must be indicated in more than one claim, or
                          2) one or more conditions are progressive, or
                          3) one or more conditions are malignant

      'Non-complex Chronic': 1) only one body system is indicated in more than one claim, and
                             2) the condition is not progressive or malignant

      'Non-Chronic':      1) no body system indicators are present in more than one claim, and
                          2) the condition is not progressive or malignant

      *identify body systems with indications in at least two different claims;
      if anycardiac2 >= 2 then
          cardiac2h = 1;

      if anycranio2  >= 2 then
          cranio2h  = 1;

      if anyderm2    >= 2 then
          derm2h    = 1;

      if anyendo2    >= 2 then
          endo2h    = 1;

      if anygastro2  >= 2 then
          gastro2h  = 1;

      if anygenetic2 >= 2 then
          genetic2h = 1;

      if anygenito2  >= 2 then
          genito2h  = 1;

      if anyhemato2  >= 2 then
          hemato2h  = 1;

      if anyimmuno2  >= 2 then
          immuno2h  = 1;

      if anymetab2   >= 2 then
          metab2h   = 1;

      if anymusculo2 >= 2 then
          musculo2h = 1;

      if anyneuro2   >= 2 then
          neuro2h   = 1;

      if anyopthal2  >= 2 then
          opthal2h  = 1;

      if anyotol2    >= 2 then
          otol2h    = 1;

      if anyotolar2  >= 2 then
          otolar2h  = 1;

      if anypulresp2 >= 2 then
          pulresp2h = 1;

      if anyrenal2   >= 2 then
          renal2h   = 1;

      if anymh2      >= 2 then
          mh2h      = 1;

      * count number of body systems that are indicated in more than one claim;
      scount_more = cardiac2h + cranio2h + derm2h   + endo2h   + gastro2h  + genetic2h + genito2h +
          hemato2h  + immuno2h + metab2h  + musculo2h + neuro2h  + pulresp2h + renal2h  +
          otol2h    + otolar2h + opthal2h + mh2h;

      *set condition based on more conservative algorithm;
      if scount_more    >= 2 or
          anyprogressive  = 1 or
          anymalign       = 1 then
          cond_more = '3 Complex Chronic';
      else if scount_more     = 1         then
          cond_more = '2 Non-complex Chronic';
      else cond_more = '1 Non-Chronic';
      output;
    end;
    label
      cond_less  = "PMCA score using the LESS conservative algorithm"
      cond_more  = "PMCA score using the MORE conservative algorithm"
      anycardiac = "Any body system involvement: cardiac"
      anycranio  = "Any body system involvement: craniofacial"
      anyderm    = "Any body system involvement: dermatological"
      anyendo    = "Any body system involvement: endocrinological"
      anygastro  = "Any body system involvement: gastrointestinal"
      anygenetic = "Any body system involvement: genetic"
      anygenito  = "Any body system involvement: genitourinary"
      anyhemato  = "Any body system involvement: hematological"
      anyimmuno  = "Any body system involvement: immunological"
      anymalign  = "Any body system involvement: malignancy"
      anymh      = "Any body system involvement: mental health"
      anymetab   = "Any body system involvement: metabolic"
      anymusculo = "Any body system involvement: musculoskeletal"
      anyneuro   = "Any body system involvement: neurological"
      anypulresp = "Any body system involvement: respiratorypulmonary-"
      anyrenal   = "Any body system involvement: renal"
      anyopthal  = "Any body system involvement: ophthalmological"
      anyotol    = "Any body system involvement: otologic"
      anyotolar  = "Any body system involvement: otolaryngological"
      anyprogressive = "Any conditions progressive?"
    ;
  run;

%mend pmca ;

/*=========================================================================

 OBJECTIVE   : Gagne's Combined Comorbidity Score based on Quan's "Enhanced"
               ICD-9-CM Codes & ICD-10 diagnoses codes

 Dependencies: VDW's Diagnosis table (encounter types: IP, AV and ED)
       Look back period = 365 days prior to index date (excluding)
        Quan's "Enhanced" ICD-9-CM & ICD-10 diagnoses codes

Citation    :
  1)  Hude Quan, MD, PhD,*† Vijaya Sundararajan, MD, MPH, FACP,‡Patricia
      Halfon, MD,§ Andrew Fong, BCOMM, Bernard Burnand, MD, MPH,§ Jean-Christophe
      Luthi, MD, PhD,§L. Duncan Saunders,MBBCh, PhD,¶ Cynthia A. Beck, MD, MASc,*I
      Thomas E. Feasby, MD,** and William A. Ghali, MD, MPH,*†,††
  2)  Gagne JJ, Glynn RJ, Avorn J, Levin R, Schneeweiss S. A combined,
      comorbidity score predicted mortality in elderly patients better than
      existing scores. Journal of Clinical Epidemiology 2011 Jan, 3

[Epub ahead of print]

 Programmer  : Arvind Ramaprasan (KPWHRI)
 PI          : David E. Arterburn <David.E.Arterburn@kp.org>
 Date        : 1/13/2021

===========================================================================*/
/*
  Parameters
  inputds:    Specify the input filename containing cohort (MRNs and Index date).
              Explicitly specify the input library name. Example: Work.CohortFile
  Index date: name of the var in inputds that holds the index date.
  outputds:   Specify the output filename. Explicitly specify the output library
              name. Example: WORK.GagneScore
*/

%macro GagneQuan(Inputds = , IndexDateVarName = , outputds = );
  %local Quan_ICD9_codes ;
  %Let Quan_ICD9_codes =  /*Renal failure-Elix*/  '403.01','403.11','403.91','404.02','404.03','404.12'
                            ,'404.13','404.92','404.93','588.0','V42.0','V45.1'
                /*Weight loss-Elix*/            ,'783.2','799.4'
                /*Alcohol abuse-Elix*/            ,'265.2','291.1','291.2','291.3', '291.5','291.6' ,'291.7'
                            ,'291.8','291.9','303.0','303.9','305.0','357.5','425.5','535.3','571.0','571.1'
                            ,'571.2','571.3' ,'V11.3'
                /*Cardiac Arrhythmias-Elix*/          ,'426.0','426.13','426.7','426.9','426.10','426.12','427.0'
                            ,'427.1','427.2','427.3','427.4','427.6','427.7','427.8','427.9','785.0','996.01','996.04','V45.0','V53.3'
                /*Coagulopathy-Elix*/           ,'287.1','287.3','287.4' ,'287.5'
                /*Complicated diabetes-elix*/         ,'250.4','250.5' ,'250.6','250.7','250.8' ,'250.9'
              /*Deficiency anemias-elix*/           ,'280.1','280.2','280.3','280.4','280.5','280.6','280.7','280.8','280.9'
              /*Fluid & electrolyte disorders-Elix*/    ,'253.6'
              /*Liver disease-Elix*/            ,'070.22','070.23','070.32','070.33','070.44','070.54','070.6','070.9'
                            ,'456.0','456.1','456.2','572.2','572.3','572.4','572.5','572.6','572.7','572.8', '573.3'
                            , '573.4', '573.8', '573.9', 'V42.7'
              /*Peripheral vascular disorder-Elix*/   ,'093.0','437.3','443.1','443.2','443.3','443.4','443.5','443.6'
                            ,'443.7','443.8','443.9','447.1','557.1','557.9','V43.4'
              /*Psychosis-Elix*/              ,'293.8','296.04','296.14', '296.44', '296.54'
              /*Pulmonary circulation disorders-Elix*/  ,'415.0', '415.1', '417.0', '417.8', '417.9'
              /*Dementia - Romano*/           ,'294.1','331.2'
              /*Hemiplegia - Romano*/           ,'334.1','344.0','344.1','344.2','344.3','344.4','344.5','344.6', '344.9'
              /*Any tumor-Romano*/            ,'195.0', '195.1','195.2','195.3','195.4','195.5','195.6','195.7','195.8', '238.6'
              /*Congestive heart failure*/        ,'398.91','402.01','402.11','402.91','404.01','404.03','404.11'
                    ,'404.13','404.91','404.93','425.4','425.5','425.6', '425.7', '425.8','425.9'
              /*Chronic pulmonary disease same*/      ,'416.8', '416.9', '506.4', '508.1', '508.8';

  %local Quan_ICD9_codes_4 ;
  %Let Quan_ICD9_codes_4 =/*Renal failure-Elix*/             '585.' ,'586.','V56.'
                /*Weight loss-Elix*/              ,'260.','261.','262.','263.'
                /*Alcohol abuse-Elix*/            ,'980.'
                /*Coagulopathy-Elix*/             ,'286.'
                /*Deficiency anemias-Elix*/         ,'281.'
                /*Fluid and electrolyte disorders-Elix*/  ,'276.'
                /*Liver disease-elix*/            ,'570.','571.'
                /*Peripheral vascular disorder-Elix*/     ,'441.','440.'
                /*Psychosis-Elix*/              ,'295.','297.','298.'
                /*Pulmonary circulation disorders-Elix*/  ,'416.'
                /*Hypertension uncomplicated-elix*/     ,'401.'
                /*Hypertension complicated-elix*/       ,'402.','403.','404.','405.'
                /*Dementia-romano*/             ,'290.'
                /*Hemiplegia - Romano*/           ,'342.','343.'
                /*Any tumor-Romano*/  ,'140.','141.','142.','143.','144.','145.','146.','147.'
                                      ,'148.','149.','150.','151.','152.','153.','154.','155.','156.','157.'
                                      ,'158.','159.','160.','161.','162.','163.','164.','165.','166.','167.'
                                      ,'168.','169.','170.','171.','172.'
                                      ,'174.','175.','176.','177.','178.','179.','180.','181.','182.','183.'
                                      ,'184.','185.','186.','187.','188.','189.','190.','191.','192.','193.'
                                      ,'194.','200.','201.','202.','203.','204.','205.','206.','207.','208.'
                /*Congestive heart failure*/        ,'428.'
                /*Chronic pulmonary disease same*/      ,'490.','491.','492.','493.','494.','495.','496.','497.'
                                      ,'498.','499.','500.','501.','502.','503.','504.','505.'
                /*HIV/AIDS  same*/              ,'042.','043.','044.'
              /*Metastatic cancer same*/          ,'196.','197.','198.','199.';

  %local Quan_ICD10_codes ;
  %Let Quan_ICD10_codes ='I09.9','I11.0','I13.0','I13.2','I25.5','I42.0','I42.5','I42.6','I42.7'
                ,'I42.8','I42.9','P29.0', /*CHF Charlson/Romano*/
               'F05.1','G31.1', /*Dementia Charlson/Romano*/
               'I27.8','I27.9','J68.4','J70.1','J70.3',/*Chronic pulmonary disease Charlson/Romano*/
               'G04.1','G11.4','G80.1','G80.2','G83.0','G83.1','G83.2','G83.3','G83.4','G83.9',/*Hemiplegia or paraplegia Charson/Romano*/
               'I44.1','I44.2','I44.3','I45.6','I45.9','R00.0','R00.1','R00.8','T82.1','Z45.0','Z95.0',/*Cardiac arrhythmias-Elixhauser*/
               'I28.0','I28.8','I28.9',/*Pulmonary circulation disorders-Elixhauser*/
               'I73.1','I73.8','I73.9','I77.1','I79.0','I79.2','K55.1','K55.8','K55.9','Z95.8'
               ,'Z95.9',/*Peripheral vascular disorders-Elixhauser*/
               'E10.2','E10.3','E10.4','E10.5','E10.6','E10.7','E10.8','E11.2','E11.3','E11.4'
               ,'E11.5','E11.6','E11.7','E11.8','E12.2','E12.3','E12.4','E12.5','E12.6','E12.7'
               ,'E12.8','E13.2','E13.3','E13.4','E13.5','E13.6','E13.7','E13.8','E14.2','E14.3'
               ,'E14.4','E14.5','E14.6','E14.7','E14.8', /*Diabetes, complicated- Elixhauser*/
               'I12.0','I13.1','N25.0','Z49.0','Z49.1','Z49.2','Z94.0','Z99.2',/*Renal failure-Elixhauser*/
               'I86.4','I98.2','K71.1','K71.3','K71.4','K71.5','K71.7','K76.0','K76.2','K76.3'
               ,'K76.4','K76.5','K76.6','K76.7','K76.8','K76.9','Z94.4',/*Liver disease - Elixhauser*/
               'D65','D69.1','D69.3','D69.4','D69.5','D69.6',/*Coagulopathy-Elixhauser*/
               'R63.4','R64',/*Weight loss- Elixhauser*/
               'E22.2',/*Fluid and electrolyte disorders-Elixhauser*/
               'D50.8','D50.9',/*Deficiency anemia-Elixhauser*/
               'F10','E52','G62.1','I42.6','K29.2','K70.0','K70.3','K70.9','Z50.2','Z71.4','Z72.1',/*Alcohol abuse-Elixhauser*/
               'F30.2''F31.2''F31.5'; /*Psychoses-Elixhauser*/

  %local Quan_ICD10_codes_3;
  %Let Quan_ICD10_codes_3 ='I43','I50',/*CHF Charlson/Romano*/
               'F00','F01','F02','F03','G30', /*Dementia Charlson/Romano*/
               'J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65'
               ,'J66','J67',/*Chronic pulmonary disease Charson/Romano*/
               'G81','G82',/*Hemiplegia or paraplegia Charlson/Romano*/
               'C00','C01','C02','C03','C04','C05','C06','C07','C08','C09','C10','C11','C12'
               ,'C13','C14','C15','C16','C17','C18','C19','C20','C21','C22','C23','C24','C25'
               ,'C26','C30','C31','C32','C33','C34','C37','C38','C39','C40','C41','C43','C45'
               ,'C46','C47','C48','C49','C50','C51','C52','C53','C54','C55','C56','C57','C58'
               ,'C60','C61','C62','C63','C64','C65','C66','C67','C68','C69','C70','C71','C72'
               ,'C73','C74','C75','C76','C81','C82','C83','C84','C85','C88','C90','C91','C92'
               ,'C93','C94','C95','C96','C97', /*Any tumor/malignancy Charlson/Romano*/
               'C77','C78','C79','C80', /*Metastatic solid tumor charlson/Romano*/
               'B20','B21','B22','B24', /*AIDS/HIV Charlson/Romano*/
               'I47','I48','I49', /*Cardiac arrhythmias-Elixhauser*/
               'I26','I27',/*Pulmonary circulation disorders-Elixhauser*/
                 'I70','I71', /*Peripheral vascular disorders-Elixhauser*/
               'I10',/*Hypertension, uncomplicated** -Elixhauser*/
               'I11','I12','I13','I15',/*Hypertension, complicated-Elixhauser***/
                 'N18','N19',/*Renal failure-Elixhauser*/
               'B18','I85','K70','K72','K73','K74',/*Liver disease - Elixhauser*/
               'D65','D66','D67','D68',/*Coagulopathy-Elixhauser*/
               'E40','E41','E42','E43','E44','E45','E46',/*Weight loss- Elixhauser*/
               'E86','E87',/*Fluid and electrolyte disorders-Elixhauser*/
               'D51','D52','D53',/*Deficiency anemia-Elixhauser*/
               'T51',/*Alcohol abuse-Elixhauser*/
               'F20','F22','F23','F24','F25','F28','F29'; /*Psychoses-Elixhauser*/
  /*Program begins*/
  Proc sql;
    Create table diag as
      Select distinct b.MRN
        , b.&IndexDateVarName
        , a.Adate
        , a.Dx as dx_orig
        , a.dx_codetype
        /* , compress(dx,'','p') as ICD*/

      From &_VDW_DX as A Inner Join &Inputds. as B on a.MRN=b.MRN
        and intnx('day',b.&IndexDateVarName.,-365)<=a.Adate<=intnx('day',b.&IndexDateVarName.,-1)
        and a.EncType in ('IP','AV','ED')
    where dx_codetype in ('09','10')
    ;
  quit;

  *Flag Elixhauser diagnosis;
  data conditions;
    set DIAG;
    length disease $25;
    if dx_orig              in: (&Quan_ICD9_codes.,&Quan_ICD10_codes.)
    or substr(dx_orig,1,4)  in (&Quan_ICD9_codes_4.)
    or substr(dx_orig,1,3)  in (&Quan_ICD10_codes_3.)
    ;

    * Now that we are switching to truncated comparison in:() lists above we have to correct for a bit of over-inclusion ;
    * David Arterburn was kind enough to review these ;
    %local bad_pvd ;
    %let bad_pvd = 'Z95.81', 'Z95.810', 'Z95.811', 'Z95.812', 'Z95.818', '443.21', '443.24', '443.82' ;
    %local bad_alcohol ;
    %let bad_alcohol = 'Z71.42' ;
    if dx_orig in (&bad_pvd, &bad_alcohol) then delete ;

    * Similarly, some of the v-codes that signify diseases in ICD-9 actually signify
    * MVAs in ICD-10--so ditch those. ;
    if dx_codetype = '10' and dx_orig =: 'V43' then delete ;

    disease = 'nopoints';

    if substr(dx_orig,1,4) in ('196.','197.','198.','199.')
    or substr(dx_orig,1,3) in ('C77','C78','C79','C80')
    then disease = 'metastatic_romano';

    if dx_orig in: ('398.91', '402.01', '402.11', '402.91', '404.01', '404.03',
                  '404.11', '404.13', '404.91', '404.93', '425.4', '425.5', '425.6',
                  '425.7', '425.8','425.9')
    or substr(dx_orig,1,4) in ('428.')
    or dx_orig in: ('I09.9','I11.0','I13.0','I13.2','I25.5','I42.0','I42.5','I42.6','I42.7','I42.8','I42.9','P29.0')
    or substr(dx_orig,1,3) in ('I43','I50')
    then disease = 'chf_romano';

    if dx_orig in: ('294.1','331.2')
    or substr(dx_orig,1,4) in ('290.')
    or dx_orig in: ('F05.1','G31.1')
    or substr(dx_orig,1,3) in ('F00','F01','F02','F03','G30')
    then disease = 'dementia_romano';

    if dx_orig in: ('403.01','403.11','403.91','404.02','404.03','404.12','404.13','404.92','404.93','588.0','V42.0','V45.1')
    or substr(dx_orig,1,4) in ('585.' ,'586.','V56.')
    or dx_orig in: ('I12.0','I13.1','N25.0','Z49.0','Z49.1','Z49.2','Z94.0','Z99.2')
    or substr(dx_orig,1,3) in ('N18','N19')
    then disease = 'renal_elixhauser';

    if dx_orig in: ('783.2','799.4')
    or substr(dx_orig,1,4) in ('260.','261.','262.','263.')
    or dx_orig in: ('R63.4','R64')
    or substr(dx_orig,1,3) in ('E40','E41','E42','E43','E44','E45','E46')
    then disease = 'wtloss_elixhauser';

    if dx_orig in: ('334.1','344.0','344.1','344.2','344.3','344.4','344.5','344.6', '344.9')
    or substr(dx_orig,1,4) in ('342.','343.')
    or dx_orig in: ('G04.1','G11.4','G80.1','G80.2','G83.0','G83.1','G83.2','G83.3','G83.4','G83.9')
    or substr(dx_orig,1,3) in ('G81','G82')
    then disease = 'hemiplegia_romano';

    if dx_orig in: ('265.2','291.1','291.2','291.3', '291.5','291.6' ,'291.7'
                  ,'291.8','291.9','303.0','303.9','305.0','357.5','425.5','535.3','571.0','571.1','571.2','571.3'
                  ,'V11.3')
    or substr(dx_orig,1,4) in ('980.')
    or dx_orig in: ('F10','E52','G62.1','I42.6','K29.2','K70.0','K70.3','K70.9','Z50.2','Z71.4','Z72.1')
    or substr(dx_orig,1,3) in ('T51')
    then disease = 'alcohol_elixhauser';

    if dx_orig in: ('195.0','195.1','195.2','195.3','195.4','195.5','195.6','195.7','195.8', '238.6')

    or substr(dx_orig,1,4) in ('140.','141.','142.','143.','144.','145.','146.','147.','148.','149.','150.'
                            ,'151.','152.','153.','154.','155.','156.','157.','158.','159.','160.','161.'
                            ,'162.','163.','164.','165.','166.','167.','168.','169.','170.','171.','172.'
                            ,'174.','175.','176.','177.','178.','179.','180.','181.','182.','183.','184.'
                            ,'185.','186.','187.','188.','189.','190.','191.','192.','193.','194.','200.'
                            ,'201.','202.','203.','204.','205.','206.','207.','208.')
    or substr(dx_orig,1,3) in ('C00','C01','C02','C03','C04','C05','C06','C07','C08'
                              ,'C09','C10','C11','C12','C13','C14','C15','C16','C17','C18','C19','C20','C21'
                              ,'C22','C23','C24','C25','C26','C30','C31','C32','C33','C34','C37','C38','C39'
                              ,'C40','C41','C43','C45','C46','C47','C48','C49','C50','C51','C52','C53','C54'
                              ,'C55','C56','C57','C58','C60','C61','C62','C63','C64','C65','C66','C67','C68'
                              ,'C69','C70','C71','C72','C73','C74','C75','C76','C81','C82','C83','C84','C85'
                              ,'C88','C90','C91','C92','C93','C94','C95','C96','C97')
    then disease = 'tumor_romano';

    if dx_orig in: ('426.0','426.13','426.7','426.9','426.10','426.12','427.0','427.1','427.2'
                  ,'427.3','427.4','427.6','427.7','427.8','427.9','785.0','996.01','996.04'
                  ,'V45.0','V53.3')
    or dx_orig in: ('I44.1','I44.2','I44.3','I45.6','I45.9','R00.0','R00.1','R00.8','T82.1','Z45.0','Z95.0')
    or substr(dx_orig,1,3) in ('I47','I48','I49')
    then disease = 'arrhythmia_elixhauser';

    if dx_orig in: ('416.8', '416.9', '506.4', '508.1', '508.8')
    or substr(dx_orig,1,4) in ('490.','491.','492.','493.','494.','495.','496.','497.'
                              ,'498.','499.','500.','501.','502.','503.','504.','505.')
    or dx_orig in: ('I27.8','I27.9','J68.4','J70.1','J70.3')
    or substr(dx_orig,1,3) in ('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61'
                              ,'J62','J63','J64','J65','J66','J67')
    then disease = 'pulmonarydz_romano';

    if dx_orig in: ('287.1','287.3','287.4' ,'287.5')
    or substr(dx_orig,1,4) in ('286.')
    or dx_orig in: ('D69.1','D69.3','D69.4','D69.5','D69.6')
    or substr(dx_orig,1,3) in ('D65','D66','D67','D68')
    then disease = 'coagulopathy_elixhauser';

    if dx_orig in: ('250.4','250.5' ,'250.6','250.7','250.8' ,'250.9')
    or dx_orig in: ('E10.2','E10.3','E10.4','E10.5','E10.6','E10.7','E10.8','E11.2','E11.3'
                  ,'E11.4','E11.5','E11.6','E11.7','E11.8','E12.2','E12.3','E12.4','E12.5'
                  ,'E12.6','E12.7','E12.8','E13.2','E13.3','E13.4','E13.5','E13.6','E13.7'
                  ,'E13.8','E14.2','E14.3','E14.4','E14.5','E14.6','E14.7','E14.8')
    then disease = 'compdiabetes_elixhauser';

    if dx_orig in: ('280.1','280.2','280.3','280.4','280.5','280.6','280.7','280.8','280.9')
    or substr(dx_orig,1,4) in ('281.')
    or dx_orig in: ('D50.8','D50.9')
    or substr(dx_orig,1,3) in ('D51','D52','D53')
    then disease = 'anemia_elixhauser';

    if dx_orig in: ('253.6')
    or substr(dx_orig,1,4) in ('276.')
    or dx_orig in: ('E22.2')
    or substr(dx_orig,1,3) in ('E86','E87')
    then disease = 'electrolytes_elixhauser';

    if dx_orig in: ('070.22','070.23','070.32','070.33','070.44','070.54','070.6','070.9'
                  ,'456.0','456.1','456.2','572.2','572.3','572.4','572.5','572.6','572.7'
                  ,'572.8', '573.3', '573.4', '573.8', '573.9', 'V42.7')
    or substr(dx_orig,1,4) in ('570.','571.')
    or dx_orig in: ('I86.4','I98.2','K71.1','K71.3','K71.4','K71.5','K71.7','K76.0','K76.2'
                  ,'K76.3','K76.4','K76.5','K76.6','K76.7','K76.8','K76.9','Z94.4')
    or substr(dx_orig,1,3) in ('B18','I85','K70','K72','K73','K74')
    then disease = 'liver_elixhauser';

    if dx_orig in: ('093.0','437.3','443.1','443.2','443.3','443.4','443.5','443.6','443.7'
                  ,'443.8','443.9','447.1','557.1','557.9','V43.4')
    or substr(dx_orig,1,4) in ('441.','440.')
    or dx_orig in: ('I73.1','I73.8','I73.9','I77.1','I79.0','I79.2','K55.1','K55.8','K55.9','Z95.8','Z95.9')
    or substr(dx_orig,1,3) in ('I70','I71')
    then disease = 'pvd_elixhauser';

    if dx_orig in: ('293.8','296.04','296.14', '296.44', '296.54')
    or substr(dx_orig,1,4) in ('295.','297.','298.')
    or dx_orig in: ('F30.2''F31.2''F31.5')
    or substr(dx_orig,1,3) in ('F20','F22','F23','F24','F25','F28','F29')
    then disease = 'psychosis_elixhauser';

    if dx_orig in: ('415.0', '415.1', '417.0', '417.8', '417.9')
    or substr(dx_orig,1,4) in ('416.')
    or dx_orig in: ('I28.0','I28.8','I28.9')
    or substr(dx_orig,1,3) in ('I26','I27')
    then disease = 'pulmcirc_elixhauser';

    if substr(dx_orig,1,4) in ('042.','043.','044.')
    or substr(dx_orig,1,3) in ('B20','B21','B22','B24')
    then disease = 'hivaids_romano';

    if substr(dx_orig,1,4) in ('401.')
    or substr(dx_orig,1,3) in ('I10')
    then disease = 'hypertension_elixhauser';

    if disease ^= 'nopoints';

    *check work;
    *if disease = 'nopoints';
  run;

  proc sort nodupkey data=conditions;
    by mrn &IndexDateVarName disease;
  run;

  *Applying the weights;
  data conditionweights;
    set conditions;
    weight = 0;
    select(disease) ;
      when('alcohol_elixhauser')      weight = 1 ;
      when('anemia_elixhauser')       weight = 1 ;
      when('arrhythmia_elixhauser')   weight = 1 ;
      when('chf_romano')              weight = 2 ;
      when('coagulopathy_elixhauser') weight = 1 ;
      when('compdiabetes_elixhauser') weight = 1 ;
      when('dementia_romano')         weight = 2 ;
      when('electrolytes_elixhauser') weight = 1 ;
      when('hemiplegia_romano')       weight = 1 ;
      when('hivaids_romano')          weight = -1 ;
      when('hypertension_elixhauser') weight = -1 ;
      when('liver_elixhauser')        weight = 1 ;
      when('metastatic_romano')       weight = 5 ;
      when('psychosis_elixhauser')    weight = 1 ;
      when('pulmcirc_elixhauser')     weight = 1 ;
      when('pulmonarydz_romano')      weight = 1 ;
      when('pvd_elixhauser')          weight = 1 ;
      when('renal_elixhauser')        weight = 2 ;
      when('tumor_romano')            weight = 1 ;
      when('wtloss_elixhauser')       weight = 2 ;
      otherwise do ;
        put 'ERROR: Unknown value ' disease= ;
        _error_ + 1 ;
      end ;
    end ;

    keep mrn disease weight &IndexDateVarName ;
  run;

  proc sql ;
    * in order to make sure we get flag vars for all the
    * diseases regardless of the health of the ppl in the
    * input dset out of the transpose below, we add some
      phoney data that gets filtered out later ;
    insert into conditionweights (mrn, &IndexDateVarName, disease, weight)
    values('santa', '25-dec-1966'd, 'alcohol_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'anemia_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'arrhythmia_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'chf_romano', 2)
    values('santa', '25-dec-1966'd, 'coagulopathy_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'compdiabetes_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'dementia_romano', 2)
    values('santa', '25-dec-1966'd, 'electrolytes_elixhauser',1)
    values('santa', '25-dec-1966'd, 'hemiplegia_romano', 1)
    values('santa', '25-dec-1966'd, 'hivaids_romano', -1)
    values('santa', '25-dec-1966'd, 'hypertension_elixhauser',-1)
    values('santa', '25-dec-1966'd, 'liver_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'metastatic_romano', 5)
    values('santa', '25-dec-1966'd, 'psychosis_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'pulmcirc_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'pulmonarydz_romano', 1)
    values('santa', '25-dec-1966'd, 'pvd_elixhauser', 1)
    values('santa', '25-dec-1966'd, 'renal_elixhauser', 2)
    values('santa', '25-dec-1966'd, 'tumor_romano', 1)
    values('santa', '25-dec-1966'd, 'wtloss_elixhauser', 2)
    ;
  quit ;

  proc transpose data = conditionweights out = flags (drop = _:) ;
    var weight ;
    id disease ;
    by mrn &IndexDateVarName ;
  run ;

  *Summing the weights;
  data combinedcomorbidityscore;
    set conditionweights(keep = mrn &IndexDateVarName weight where = (mrn ne 'santa'));
    by mrn &IndexDateVarName;

    if first.&IndexDateVarName then
      combinedscore = 0;
    combinedscore + weight;

    if last.&IndexDateVarName then
      output;
    keep mrn &IndexDateVarName combinedscore;
  run;

  *Note: patients not included in the final data set (combinedcomorbidityscore) did not
  have any of the component conditions. Therefore, be sure to set their combined comorbidity
  score values to zero;
  proc sql;
    Create table &outputds (label = "Gagne's Combined Comorbidity Score + constituent weights") as
      Select i.*
        , coalesce(s.combinedscore          , 0) as combinedscore           length = 4 label = "Gagne's Combined Comorbidity Score"
        , coalesce(f.alcohol_elixhauser     , 0) as alcohol_elixhauser      length = 3 label = "Component of total score due to alcohol abuse diagnoses"
        , coalesce(f.anemia_elixhauser      , 0) as anemia_elixhauser       length = 3 label = "Component of total score due to deficiency anemias diagnoses"
        , coalesce(f.arrhythmia_elixhauser  , 0) as arrhythmia_elixhauser   length = 3 label = "Component of total score due to cardiac arrhythmias diagnoses"
        , coalesce(f.chf_romano             , 0) as chf_romano              length = 3 label = "Component of total score due to congestive heart failure diagnoses"
        , coalesce(f.coagulopathy_elixhauser, 0) as coagulopathy_elixhauser length = 3 label = "Component of total score due to coagulopathy diagnoses"
        , coalesce(f.compdiabetes_elixhauser, 0) as compdiabetes_elixhauser length = 3 label = "Component of total score due to complicated diabetes diagnoses"
        , coalesce(f.dementia_romano        , 0) as dementia_romano         length = 3 label = "Component of total score due to dementia diagnoses"
        , coalesce(f.electrolytes_elixhauser, 0) as electrolytes_elixhauser length = 3 label = "Component of total score due to fluid & electrolyte disorders diagnoses"
        , coalesce(f.hemiplegia_romano      , 0) as hemiplegia_romano       length = 3 label = "Component of total score due to hemiplegia diagnoses"
        , coalesce(f.hivaids_romano         , 0) as hivaids_romano          length = 3 label = "Component of total score due to hiv/aids diagnoses"
        , coalesce(f.hypertension_elixhauser, 0) as hypertension_elixhauser length = 3 label = "Component of total score due to hypertension diagnoses"
        , coalesce(f.liver_elixhauser       , 0) as liver_elixhauser        length = 3 label = "Component of total score due to liver disease diagnoses"
        , coalesce(f.metastatic_romano      , 0) as metastatic_romano       length = 3 label = "Component of total score due to metastatic cancer diagnoses"
        , coalesce(f.psychosis_elixhauser   , 0) as psychosis_elixhauser    length = 3 label = "Component of total score due to psychosis diagnoses"
        , coalesce(f.pulmcirc_elixhauser    , 0) as pulmcirc_elixhauser     length = 3 label = "Component of total score due to pulmonary circulation disorders diagnoses"
        , coalesce(f.pulmonarydz_romano     , 0) as pulmonarydz_romano      length = 3 label = "Component of total score due to chronic pulmonary disease diagnoses"
        , coalesce(f.pvd_elixhauser         , 0) as pvd_elixhauser          length = 3 label = "Component of total score due to peripheral vascular disorders diagnoses"
        , coalesce(f.renal_elixhauser       , 0) as renal_elixhauser        length = 3 label = "Component of total score due to liver disease diagnoses"
        , coalesce(f.tumor_romano           , 0) as tumor_romano            length = 3 label = "Component of total score due to any tumor diagnoses"
        , coalesce(f.wtloss_elixhauser      , 0) as wtloss_elixhauser       length = 3 label = "Component of total score due to weight loss diagnoses"
    From &Inputds. as i
      Left Join combinedcomorbidityscore as s on i.MRN=s.MRN and i.&IndexDateVarName = s.&IndexDateVarName
      Left Join flags as f on i.MRN=f.MRN and i.&IndexDateVarName = f.&IndexDateVarName
    ;
  quit;

%Mend GagneQuan;

%macro ReprioritizeRace(inset = , pref_format = , outset = ) ;
  /*********************************************
  * Roy Pardee
  * KP Washington Health Research Institute
  * (306) 362-2638
  * roy.e.pardee@kp.org
  *
  * Re-shuffles values in the race1-race5 vars according to a user-specified hierarchy.
  *********************************************/
  proc sql ;
    create table __grist as
    select i.mrn
        , d.race1
        , d.race2
        , d.race3
        , d.race4
        , d.race5
    from &inset as i
      left join &_vdw_demographic as d on i.mrn = d.mrn
    ;
  quit ;

  data __grist ;
    set __grist ;
    array r race1-race5 ;
    do i = 1 to dim(r) ;
      race = r{i} ;
      sort_order = put(race, &pref_format..) ;
      output ;
    end ;
    keep mrn race sort_order ;
  run ;

  proc sort data = __grist ;
    by mrn sort_order ;
  run ;

  proc transpose data = __grist out = &outset (drop = _:) prefix = race ;
    var race ;
    by mrn ;
  run ;

  %removedset(dset = __grist) ;

%mend ReprioritizeRace ;
