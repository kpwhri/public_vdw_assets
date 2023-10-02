# HCSRN Virtual Data Warehouse Standard Macros

## Background

The VDW standard macros are stored in a single file, the authoritative version of which is stored on the web server at kpwhri.github.io. You can browse this code by [clicking this link](https://kpwhri.github.io/public_vdw_assets/standard_macros.sas) (kpwhr.github.io).

The easiest way to bring these macros into your SAS session is to first %include the StdVars.sas file for your site into your program, and then write this into your program:
```sas
  %include vdw_macs ;
```
The reason that works is because of this standard FILENAME statement, which is contained in StdVars.sas at all the sites:
```sas
   filename vdw_macs &_vdw_asset_engine "&_vdw_asset_loc/standard_macros.sas" ; 
```
Depending on how your site has configured that &\_vdw\_asset\_loc variable, using this method should always get you the most recent version of these macros.

If you have written a macro you would like included please send to [Roy Pardee](https://www.hcsrnalfresco.org/share/page/user/pardee.r@ghc.org/profile).

## Macros

### %PMCA

Adapted for VDW use from [the v3.1 code posted to KPWHRI's web site](https://www.kpwashingtonresearch.org/our-research/our-scientists/rita-mangione-smith-md-mph/measurement-tools-research-dr-rita-mangione-smith) by Phil Crawford.

Purpose: Computes the Pediatric Medical Complexity Algorithm.

Inputs: A dataset of MRNs and index dates. Multiple index dates for the same MRN are allowed, but the combinations of (MRN, index_date) values should be unique.

Outputs: A dataset of MRNs, index dates, condition flags, and overall complexity scores (one conservative, the other less so).

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inset | The name of the input dataset of MRNs and (optionally) index dates.|
|index_date|Either the name of a date variable in the &inset dataset OR a complete SAS date literal (e.g., '14-feb-2019'd). The macro works with both ICD-9 and ICD-10 codes|
|outset|The name you want for the output dataset.|
|days_lookback|Optional. The number of days you want to look back from &index_date for dx codes that trip the various condition flags. The default is 365.|

#### Reference

Article: Pediatric Medical Complexity Algorithm: A New Method to Stratify Children by Medical Complexity; Tamara D. Simon, Mary Lawrence Cawthon, Susan Stanford, Jean Popalisky, Dorothy Lyons, Peter Woodcox, Margaret Hood, Alex Y. Chen and Rita Mangione-Smith,  [Pediatrics; originally published online May 12, 2014;  DOI: 10.1542/peds.2013-3875](http://pediatrics.aappublications.org/content/early/2014/05/07/peds.2013-3875)

#### Sample Call
```sas
%pcma(inset = s.test_kids
    , index_date = my_datevar
    , outset = s.deleteme) ;

%pcma(inset = s.test_kids
    , index_date = '01-jan-2019'd
    , outset = s.deleteme) ;

```

### %CalcAge

Purpose: Calculates age as an integer, producing the number that the person would themselves give if asked their age.

Inputs: Date of birth; date on which to compute age.

Outputs: Age in whole years.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|BDtVar|The name of a variable containing a date of birth.  If not specified, defaults to birth\_date, which is what that var is called in the VDW demographics file.|
|RefDate|The date on which you want to calculate the person's age.|

Caveat:The current version of this macro produces an error when used on dates of birth that fall on leap days.

This macro returns an expression usable from both datastep and SQL code.  For example:
```sas
data gnu ;
  set old ;
  my_age = %CalcAge(RefDate = "&sysdate"d) ;
run ;

proc sql ;
  create table gnu as
  select*, %CalcAge(BDtVar = dob, RefDate = "25dec2009"d) as my_age
  from old
  ;
quit ;
```
### %RemoveDset

Purpose: Deletes a dataset if it exists, without causing a WARNING if it does not exist. Inputs: The name of a dataset to delete. Outputs: n/a Parameters:

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|dset|Name of the dset you want removed.|

Useful for making sure a program is truly producing all expected datasets--keeps you from being fooled by the leavings of prior programs or runs.

### %GetRxForPeople

Purpose: Pulls all rx fills between the supplied dates for the people listed in the user-supplied People dataset.

Inputs: A dataset containing a list of unduplicated MRNs belonging to the people whose fills you want. Dates over which you want the fills.

Output(s): The portion of the Rx dataset that describes fills for the people listed in the People dataset.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|The name of the dataset containing the MRNs of the people whose fills you want. This can be a one-part temporary dataset or a two-part permanent dataset.|
|StartDt|A valid SAS date specification without delimiters (e.g., 01Jan2005) that marks off the beginning of the period during which you want Rx fills.|
|EndDt|A date specification marking off the end of the period during which you want Rx fills.|
|OutSet|The name of the output dataset of Rx fills.|

### %GetRxForDrugs

Purpose: Gets the pharmacy fills for a specified set of drugs (identified by NDCs) which ocurred between the dates specified in StartDt and EndDt.

Inputs: A dataset of unduplicated NDC codes for the drugs of interest.

Output: The portion of the Rx dataset describing fills for the drugs of interest during the time period of interest.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|OutSet|The name of the output dataset of Rx fills.|
|DrugLst|The dataset containing the list of drugs of interest.|
|StartDt|The date that marks off the beginning of the period during which you want Rx fills.|
|EndDt|A date specification marking off the end of the period during which you want Rx fills.|
|OutSet|The name of the output dataset of Rx fills.|

### %GetPxForPeople

Purpose: Pulls the set of procedures performed on any of a user-supplied list of People, during a user-specified period of interest.

Inputs: A dataset containing an unduplicated set of MRNs for the people of interest; a time period of interest.

Output: That portion of the Proc dataset containing procedures performed on one of the people in the supplied People dataset.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|The name of the input People dataset|
|StartDt|The beginning of the period of interest.|
|EndDt|The end of the period of interest.|
|OutSet|The desired name of the output dataset of procedure records.|

### %GetDxForPeople

Purpose: Pulls the set of diagnoses applying to any of a user-supplied list of People, during a user-specified period of interest.

Inputs: A dataset containing an unduplicated set of MRNs for the people of interest; a time period of interest.

Output: That portion of the Diag dataset containing procedures performed on one of the people in the supplied People dataset.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|The name of the input People dataset|
|StartDt|The beginning of the period of interest.|
|EndDt|The end of the period of interest.|
|OutSet|The desired name of the output dataset of diagnosis code records.|

### %GetDxForDx

Purpose: Pulls the set of Diag records bearing any of a user-supplied list of diagnosis codes, during a user specified period of interest.

Inputs: A dataset containing an unduplicated list of ICD-9 diagnosis codes of interest; a time period of interest.

Output: That portion of the Diag dataset containing one of the diagnosis codes of interest during the specified period.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|DxLst|The name of the input dataset of diagnosis codes|
|DxVarName|The name of the variable in DxLst that contains the diagnosis codes.|
|StartDt|The beginning of the period of interest.|
|EndDt|The end of the period of interest.|
|OutSet|The desired name of the output dataset of procedure records.|

### GetPxForPx

Purpose: Pulls the set of Proc records bearing any of a user-supplied list of procedure codes, during a user specified period of interest.

InputsA dataset containing an unduplicated list of any combination of CPT-4 and ICD-9 procedure codes of interest; a time peiod of interest.

Output: That portion of the Proc dataset containing one of the diagnosis codes of interest during the specified period.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|PxLst|The name of the variable in PxLst that contains the procedure codes|
|PxCodeTypeVarName|The name of the variable in PxLst that contains the code type information that modifies the Procedure code var. This var must mirror the format and content of the Codetype in the Proc dataset.|
|StartDt|The beginning of the period of interest.|
|EndDt|The end of the period of interest.|
|Outset|The desired name of the output dataset of procedure records.|

### %GetRxForPeopleAndDrugs

Purpose: Pulls all rx fills for a specific set of drugs between the supplied dates for the people listed in the user-suppied People dataset.

Inputs: A dataset containing a list of unduplicated MRNs belonging to the people whose fills you want. The NDC codes of the drugs of interest. Dates over which you want the fills.

Output(s): The portion of the Rx dataset that describes fills for the people listed in the People dataset.

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|The name of the dataset containing the MRNs of the people whose fills you want. This can be a one-part temporary dataset or a two-part permanent dataset|
|DrugLst|The name of the datasets containing the NDC codes of interest.|
|StartDt|A valid SAS date specification without delimiters (e.g., 01Jan2005) that marks off the beginning of the period during which you want Rx fills.|
|EndDt|A date specification marking off the end of the period during which you want Rx fills.|
|OutSet|The name of the output dataset of Rx fills.|

### %GetDxForPeopleAndDx
Purpose: Pulls all diagnoses for a specific set of diagnoses between the supplied dates for the people listed in the userer-supplied eople datset.

Inputs: A dataset containing a list of unduplicated MRNs belonging to the people whose diagnosis records you want. A dataset containing the Diagnosis codes of interest. The dates over which you want the records.

Output(s): The portion of the DX dataset that describes diagnoses of interest for the people listed in the People dataset.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People |  The name of the dataset containing the MRNs of the people whose dx recs you want. This can be a one-part temporary dataset or a two-part permanent dataset.|
|DxLst | The name of the datasets containing the ICD9 codes of interest.|
|StartDt | A valid SAS date specification without delimiters (e.g., 01Jan2005) that marks off the beginning of the period during which you want dx recs.|
|EndDt | A date specification marking off the end of the period during which you want dx records.|
|OutSet |  The name of the output dataset of diagnoses of interest.|

### %CancerSchema

Purpose: Allocates tumors int CancerSchema according to AJCC 6 and 7 definitions.
Inputs: The VDW Tumor file, start dates, and end dates.
Output(s): The ouput dataset contains the tumor table for time period of interest with 2 additional variables CancerSchemaAjcc7thEdition, and CancerSchemaAjcc6thEdition.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|OutputDS | The name of the output with the 2 new variables.|
|StartDt |A valid SAS date specification without delimiters (e.g., 01Jan2005) that marks off the beginning of the period of interest.|
|EndDt |A date specification marking off the end of the period of interest.|


### %BreastCancerDefinition01

Purpose: Pulls the set of "incident" (that is, first-ocurring during the specified date range) breast cancers, both invasive and in-situ (but excluding LCIS). These criteria are based on the ones used for the Early Screeing study.
Inputs: Endpoints of the time period of interest.
Output: A dataset of MRNs, Dx dates, Estrogen Receptor Mark er statuses, and stage information for each woman's first breast tumor ocurring during the period of interest.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|StartDt| The beginning of the period of interest.|
|EndDt| The end of the period of interest|
|OutSet|  The name of the output dataset.|


### %BreastCancerDefinition02
Purpose: Pulls the set of "incident" (that is, first-ocurring, over the whole range of the tumor file) invasive breast tumors (so no DCIS, unlike the 01 definition). These criteria are based on the ones used for the [Pharmacovigilance Study](resolveuid/f1658048c59d2ac094d4dc267b574acb).
Inputs: Endpoints of the time period of interest, a name for the output dataset of tumors, optionally, a name for another dataset of tumor records for women who have > 1 qualifying tumor on the very same day.
Output: A dataset of **tumors** (not women, as with the 01 definition), all variables in the local file.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|StartDt | The beginning of the period of interest.|
|EndDt | The end of the period of interest|
|OutSet |  The name of the output dataset.|
|OutMultFirsts | Optional--name of a dataset to hold the records for women who have multiple first qualifying tumors.|


### %PullContinuous

Purpose: Pulls the set of people who were continuously enrolled for a user-specified number of months before and after a constant or person-specific Index Date, with optional allowance for user-specified gaps in enrollment.

Inputs: A dataset of MRNs of the people whose enrollment you need evaluated; specification of the Index Date around which you want enrollment evaluated. The Required # of months of continuous enrollment pre- and post- Index Date; Allowed # of months gaps in enrollment tolerated. For cases where index dates are person-specific, the Index Date spec must be the name of a variable in the input dataset that holds each person's index date. For cases where everyone has the same index date, the Index Date spec can be a date constant, e.g., ```'31Dec2005'd```.

Note that this macro assumes that there are no overlapping periods in the enroll data. SDMs are referred to the ```%CleanEnroll``` mcro |for a means of making sure this is true at your sites.
Output: A dataset matching the structure and contents of the input dataset, but limited to people who meet the enrollment criteria.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|InSet| The name of the input dataset of MRNs of the people whose enrollment you want evaluated.|
|OutSet|  The name you want for the output dataset. This dataset will match the structure & contents of InSet, except that anyone whose enrollment is lacking will be eliminated.|
|IndexDate| Either the name of a date variable in InSet, or, a complete date literal (e.g., "01Jan2005"d)|
|PreIndexEnrolledMonths|  The required # of continuous months of enrollment prior to the Index Date, where the definition of 'continuous' is modified by the PreIndexGapTolerance parameter below.|
|PreIndexGapTolerance|  The # of months of disenrollment tolerated in the definition of 'continuous' prior to the Index Date.|
|PostIndexEnrolledMonths| The required # of continuous months of enrollment after the Index Date|
|PostIndexGapTolerance| The # of months of disenrollment tolerated in the definition of 'continuous' after the Index Date.|
|EnrollDSet|  Optional.The name of the dataset to use to pick out enroll records for the group in InSet.Leave this off to have the macro use the library/dataset specified in StdVars.sas. This parameter can be useful in cases where you only want to take particular types of enrollment into account (e.g., only enrollment with Medicare coverage).Just grab out your own subset of the enroll records, and specify the name of your dataset in this parameter.|


### %GetFollowUpTime
Purpose: Evaluates enrollment data for an input dataset of people from a user-specified IndexDate, up until a user specified EndDate, adding a variable to the input dataset giving the earlier of EndDate or the end of each person's continuous enrollment. People with no enrollment during the period between IndexDate and EndDate will have a missing value in the added variable.

Both IndexDate and EndDate can vary from person to person (in which case these parameters should name variables in the People dataset) or can be date constants that apply to everyone.

Inputs: A dataset of unduplicated MRNs

Output(s): A new dataset identical to the one named in the People parameter, but with an additional variable that answers the question 'at what point after index date did this person's continuous enrollment end, if it ended before EndDate?'

#### Parameter|s|

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|  The name of the dataset containing the MRNs of the people whose followup time you want.|
|IndexDate| The start of the period over which you want followup time evaluated.This can be either the name of a variable in the People dataset, or a complete sas date literal (e.g., '25dec2004'd).|
|EndDate| The end of the period over which you would like continuous enrollment evaluated.This can be either the name of a variable in the People dataset, or a complete sas date literal (e.g., '25dec2004'd).|
|GapTolerance|  The largest number of days of non-enrollment in between enrolled periods to ignore in determining the end of continuous enrollment.|
|CallEndDateVar|  The name you want for the additional variable.|
|OutSet|  The name you want for the output dataset.|
|EnrollDset|  Optional.The name of the dataset from which the macro should draw Enroll records.If left off, the macro will use the dataset indicated in the local StdVars.sas file. This can be useful if for example, you only want to consider particular types of enrollment (e.g., enrollment w/Medicaid coverage) for your application.Simply draw out your own subset of the standard Enroll file for the people in People, subset it as you like, and pass the name of your custom Enroll file into the macro.|
|Reverse| Optional. A value of 0 (default) will run the macro going forward in time from IndexDate until EndDate as described in the purpose. Specifying a value of 1 instead answers the question "at what point before index date did the person's continuous enrollment start, if it started after EndDate?"|


### %CollapsePeriods

Purpose: Takes an input dataset of start/stop periods and collapses contiguous, overlapping, or |near-contiguous (e.g., w/in ||tolerance) periods over which none of the variables on the dataset change.

This macro is useful for determining the extent of continuous enrollment. Just draw out a set of enrollment records, drop any variables whose changes are not of interest (e.g., if you don't care about the type of coverage, drop all the ins\_: vars) and run it through this macro.

Inputs: A dataset with a period-start variable (e.g., enr\_start), and a period-end variable (e.g., enr\_end), and any number of additional variables (e.g., mrn, primary care physician indicator, etc.)

Output(s): If the &OutSet parameter is given, a new dataset of collapsed periods. If not, the input dataset is modified in-place.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Lib| The name of the sas library where your input dataset is located (e.g., 'work', 'perm', etc.)|
|DSet|  The name of your input dataset|
|RecStart|  The name of the date variable in Dset holding the start of a period.|
|RecEnd|  The name of the date variable in Dset holding the end of a period.|
|PersonID|  The name of the variable in Dset that uniquely identifies a person.  Defaults to MRN.|
|OutSet|  (Optional) The name of a dataset to put the collapsed records in.  If not given, the dataset specified in &Lib..&DSet is replaced.|
|DaysTol| The number of days gap to ignore between otherwise contiguous periods of no change.|

#### Sample Call
```sas
%CollapsePeriods(Lib        = work
               , DSet       = raw_enroll
               , RecStart   = enr_start
               , RecEnd     = enr_end
               , PersonID   = MRN
               , OutSet     = mylib.collapsed_enroll
               , DaysTol    = 1
               ) ;
```

### %NDCLookup

Purpose: Searches the EverNDC drug lookup table for NDCs of interest, on the basis of a user-supplied dataset of text string to look for (like for instance 'tamox' in a search for Tamoxifen and its synonyms.).

Inputs: A dataset of search strings.

Output: A dataset of NDC codes potentially of interest.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Inds|  The name of the input dataset of text strings to search for. These strings must be in a variable named 'DrugName'.|
|Outds| The portion of EverNDC that contains one or more of the supplied search strings.|
|EverNDC| The dataset name of your local copy of EverNDC.|

### %DeIDDset

Purpose: De-identifies a dataset by replacing a user-specified ID variable (e.g., MRN) with an arbitrary ID variable (e.g., StudyID) and creates a crosswalk dataset that relates the original ID values to the new StudyIDs.

Inputs: A dataset to de-identify.

Output: A crosswalk dataset, and a de-identified version of the input dataset.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|InSet| Name of the dataset you want de-identified. THIS DSET IS ALTERED BY THE MACRO!|
|XWalkSet|  Name of the output ID-crosswalk dataset.|
|OldIDVar|  Name of the ID variable you want removed (e.g., MRN)|
|NewIDVar|  Name for the new ID variable the macro creates (e.g., StudyID).|
|NewIDLen|  The length of the new ID variable.|

### %Charlson

Purpose: Calculates the Deyo variant of the Charlson comorbidity index (basically a count of the number of chronic conditions the person has diagnoses for in the year prior to index date weighted by each condition's mortality influence).

Inputs: A dataset containing a cohort of identified by MRN, each of which has an index date (the date as-of which you want to know their comorbidities).

Output(s): A dataset containing the MRNs, index dates, the charlson score and a series of flags indicating which of the chronic conditions checked the person suffers from.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|InputDS|The name of the input cohort dataset of MRNs and index dates.|
|IndexDateVarName|The name of the variable in InputDS that holds the index date.|
|OutputDS|The name of the file that will be output by the macro with the calculated Charlson score on it.|
|IndexVarName|The name of the variable on the output data set that contains the Charlson score.|
|InpatOnly|Lets you choose which types of utilization records you want to use to calculate the score.<br/>I=inpatient only (enctype = 'IP')<br/>B=both inpatient and ambulatory (enctype in ('IP', 'AV'))<br/>A=all types of encounters<br/>C=custom list of encounter types given in enctype\_list parameter|
|Malig|Allows you to remove malignancies and metastasis as weights in the charlson score (commonly desired for cohorts of cancer patients).<br/>Y = Zero-weight the cancer flags (useful for cancer studies where everyone will have cancer)<br/>N = Let the cancer flags contribute to the overall score as normal.|
|NoEncounterGetsMissing|Optional. Controls whether people for whom no dx/px data is found get a charlson score of 0 (default) or a missing value.<br/>Y = Yes--give a 0 value to ppl w/out any dx/px data<br/>N = No--make the score missing.<br/>For cohorts whose year-pre-index-date data capture is assured (usually via enrollment data), having no encounters should legitimately indicate a lack of any comorbidities & therefore a legit score of 0. Cohorts **not** previously vetted in this way may not support that inference, and users should specify a Y for this parameter to prevent unwarranted interpretation of the Charlson score.|
|enctype\_list|A quote-and-comma-delimited list of the encounter types you want the macro to use.  Used in conjunction with InpatOnly, described above.|
|days\_lookback|Optional. Number of days to go back from index date to look for diagnoses and procedures for the score. Defaults to 365, which is standard for Charlson|

#### Sample Call
```sas
%charlson(inputds                 = perm.my_cohort
         , IndexDateVarName       = index_date
         , outputds               = perm.charlson_scores
         , IndexVarName           = charlson_score
         , inpatonly              = C
         , malig                  = N
         , NoEncounterGetsMissing = N
         , enctype_list           = %str('IP', 'AV', 'ED')
         , days_lookback          = 365
         ) ;
```
For further details, see the comments at the beginning of the macro and check out [this documentation](https://www.hcsrn.org/share/page/site/VDW/document-details?nodeRef=workspace://SpacesStore/d919ae80-2f93-473d-ad4c-40a12390720c) to learn about the Charlson macro.

### %Pretty Case

Purpose: Corrects the case of names and addresses fields for use in mailings. That is, it will change values in the old column (below) to the values in the new column.

|Old|New|
|---|---|
|ROY EDMUND PARDEE III|Roy Edmund Pardee III|
|123 N.E. 4TH STREET|123 N.E. 4th Street|
|PO BOX 12|PO Box 12|
|ROY PARDEE C/O ANGELINA JOLIE|Roy Pardee C/O Angelina Jolie|
|743 NE EVERGREEN ST|743 NE Evergreen St|
|82 SLEATER-KINNEY STREET|82 Sleater-Kinney Street|

#### Sample Call
```sas
%PrettyCase(InSet = source
    , OutSet = prettified
    , VarList = first_name last_name line1 line2 city
    ) ;
```
Notes: This macro is not intended to replace human judgment--it should significantly improve the formatting of all-caps variables, and will likely be good enough for many purposes, but it is not as good as having an actual person making these corrections.

The macro uses some version-9-only functions--to wit: propcase, prxparse and prxmatch. So it will not be usable on versions of sas earlier than 9.

It also relies on a hard-coded list of names whose fourth characters should be upcased (e.g., the Mac names—MacDougall, MacDonald, etc.).

Please let roy.e.pardee@kp.org know about any improvements that could be made.

### %GetCensusForPeople

Purpose: Adds [Census](resolveuid/298755bd417dadd1067f1eb60140461c) variables to an input dataset of MRNs

Inputs: A dataset containing a variable called MRN.

Output: A dataset containing everything from the input dataset, plus all of the variables in the \[\[Census\]\] file.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|InSet|The name of the input dataset of MRNs. If this dset contains other variables, they should appear in the output dataset as well.|
|Outds|The original dataset, along with all variables from \[\[Census\]\] as well. You should get the same number of observations|
|CensusYear (optional—defaults to 2000)|The census files are named after the year during which the demographic data was collected. All sites should have 2000 data at least, other sites may have data from other years.|

#### Sample Call:

```sas
%GetCensusForPeople(InSet = s.drop_me , OutSet = s.drop_me_census , CensusYear = 2010) ;
```

### %CleanRx

Purpose: Performs a quality check on the pharmacy data reporting problem obs and variables and optionally producing datasets with problem and non-problem data.

Inputs: The Rx file, a declared library for output, and user options.

Output(s): All output is optional - A report of problem data, a table of problem data, and a table of non-problem data.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Outlib|The library name you've already declared where you want output you elect to save to go.|
|Clean|A value of "Y" will output a table of non-problem data in &outlib|
|Dirty|A value of "Y" will output a table problem data in &outlib|
|Report|A value of "Y" will produce a report of problem data in the pharmacy file.|

### %GetVitalSignsForPeople

Purpose: Pulls all vital signs between the supplied dates for the people listed in the user-supplied People dataset.

Inputs: A dataset containing a list of unduplicated MRNs belonging to the people whose vitals you want. Dates over which you want the vitals.

Output(s): The portion of the Vital Signs dataset that describes vitals for the people listed in the People dataset.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|The name of the dataset containing the MRNs of the people whose vitals you want. This can be a one-part temporary dataset or a two-part permanent dataset.|
|StartDt|A valid SAS date specification without delimiters (e.g., 01Jan2005) that marks off the beginning of the period during which you want vitals.|
|EndDt|A date specification marking off the end of the period during which you want vitals.|
|OutSet|The name of the output dataset of vitals.|

### %Diabetes\_Charlson

Purpose: Pulls all people with diabetes as defined by the Charlson score. Creates a dataset in the work directory called Diabetes\_Charlson of the diagnoses along with descriptions of each. Creates a format called Diabetes\_Charlson for the diagnoses as well.

Inputs: Dates over which you want to pull diabetics, optional variable of allowed encounter types.

Output(s): A table with one record per person meeting the Charlson defintion of diabetics over the dates specified.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|outfile|Name of the output dataset containing the MRN|
|StartDt|A valid SAS date specification without delimiters (e.g., 01Jan2005) that marks off the beginning of the period during which you want a list of diabetics.|
|EndDt|A date specification marking off the end of the period during which you want a list of diabetics.|
|EncType|A will search All encounters (default), I will search only Inpatient encounters (the traditional Charlson) B will search Both IP and OP for dx, but not other types|

### %GetDateRange

Purpose: Creates global variables holding the minimum and maximum values of all date variables in a dataset.

Inputs: The path and filename of a SAS dataset.

Output: Global variables in the form of DateVariable\_Max and DateVariable\_Min containing the max and min dates in the dataset (see example)

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Path|The directory in which the SAS data set lives.|
|Filename|The name of the SAS data set.|
|Print|Value of 0 will supress the printing of the max and min values to the .lst file Value of 1 (default) will print the max and min values to the .lst file.|

#### Sample Call

```sas
%GetDateRange(&_TumorLib., &_TumorData.);
```

This will create global variables such as &DOD\_Min, &DOD\_Max, DT\_SURG\_Max, DT\_SURG\_MIN, and so forth with values in date9 format.

### %Hypertension\_BP

Purpose: Pulls all people with a systolic and-or diastolic BP reading above specified threasholds over specified dates along with their highest systolic and diastolic readings in that period. Can be used to defined hypertension.

Requirements: %include your standard vars file

Output: A list of MRN with max values of systolic and diastolic measurements over the date range that meet the defined threashold.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|OutFile|Name of the output dataset you would like to create.|
|StartDate|The date from which you want to start looking for BP.|
|EndDate|The date to which you want to end looking for BP.|
|Diastolic\_Min|The minimum diastolic value you wish to allow in Outfile|
|Systolic\_Min|The minimum systolic value you wish to allow in the Outfile|
|Strict\_Equality|0 (default) allows BP readings of the min values or greater 1 allows only BP readings greater than the min values|
|Either|1 (default) will return a person with either a diastolic OR a systolic value above mins 0 returns only people with both a diastolic AND a systolic value above mins|

### %CleanEnroll

Purpose: Performs a quality check on enrollment data reporting problem obs and variables and optionally producing datasets with problem and non-problem data.

Inputs: The Enroll file, a declared library for output, and user options.

Output(s): All output is optional - A report of problem data, a table of problem data, and a table of non-problem data.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Outlib|The library name you've already declared where you want output you elect to save to go.|
|Clean|A value of "Y" will output a table of non-problem data in &outlib|
|Dirty|A value of "Y" will output a table problem data in &outlib|
|Report|A value of "Y" will produce a report of problem data in the enrollment file.|

### %CleanVitals

Purpose: Performs a quality check on vital signs data reporting problem obs and variables and optionally producing datasets with problem and non-problem data.

Inputs: The Vital Signs file, a declared library for output, and user options.

Output(s): All output is optional - A report of problem data, a table of problem data, and a table of non-problem data.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Outlib|The library name you've already declared where you want output you elect to save to go.|
|Clean|A value of "Y" will output a table of non-problem data in &outlib|
|Dirty|A value of "Y" will output a table problem data in &outlib called DIRTY|
|Report|A value of "Y" will produce a report of problem data in the vital signs file.|
|Limits|A value of "Y" will output a table named LIMITS to the &outlib with only those observations that contain measures that are \[\[Compatible With Life\]\]. Missing values are not excluded. This requires the Birth\_Date variable in Demographics.|

### %SimpleContinuous

Purpose: Motivated by a desire for a simpler version of the %PullContinuous macro (which is hard to explain now that Enroll is a start/stop structure, since it purports to evaluate months of enrollment). This one evaluates enrollment over a single period of interest, allowing any number of gaps of at most &DaysTol days. It returns a dset with a flag var called ContinuouslyEnrolled that signifies whether or not the person was continuously enrolled over the period of interest.

Like PullContinuous, this macro takes an optional input dataset of Enroll records, allowing users to apply constraints to which types of enrollment records are considered.

Inputs: A dataset of MRNs, dates defining the period of interest, a tolerance parameter signifying the number of days disenrollment to close up, and optionally, a dset of Enroll records to use

Output(s): A dataset bearing the MRNs input, a 0/1 ContinuouslyEnrolled variable, and a CoveredDays variable reporting the total number of days during the period of interest that the person was enrolled.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|A dataset of MRNs whose enrollment we are considering.|
|StartDt|A date literal identifying the start of the period of interest.|
|EndDt|A date literal identifying the end of the period of interest.|
|DaysTol|The number of days gap between otherwise contiguous periods of enrollment (or at the start or end of the otherwise contiguous period of enrollment) that is tolerable.|
|OutSet|Name for the output dset.|
|EnrollDset|Optional.Name of a custom-prepared dset of Enroll records to use in evaluating the enrollment|

#### Sample Call
```sas
%SimpleContinuous(People = TestPeople
               , StartDt = 01jan2006
               , EndDt   = 31dec2006
               , DaysTol = 90
               , OutSet  = TestPeopleEnroll
              ) ;
```
### %GetKidBMIPercentiles

Adults can be categorized into underweight/overweight/obese categories in a fairly straightforward fashion, on the basis of their Body Mass Index scores alone. Making a similar classification for children is much less straightforward. Their heights/weights have to be compared to a normative sample of weights developed in 1977 by the National Center for Health Statistics. See details [here](http://www.cdc.gov/nchs/about/major/nhanes/growthcharts/background.htm).

To make these calculations easier, the CDC released[this SAS code for calculating the proper percentile scores](http://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm). The %GetKidBMIPercentiles VDW macro is a thin wrapper around the CDC's code.

Note: This macro uses the VDW Vital Signs Variable Head\_Cir\_Raw, which should be in centimeters.

Note that the CDC's code is valid only for people between the ages of 2 and 17, and the macro will not return any measures for people outside this age range. It will also potentially use imputed heights--be sure to inspect the days\_diff variable to make sure that you are comfortable with the length of time that has passed between the height and weight measures (if any).

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|InSet|The name of an input dataset of MRNs for the people on which you would like BMI percentile scores.|
|OutSet|The name you would like to give the output dataset containing the percentile scores.|
|StartDt|(Optional) The earliest measure date for which you would like the BMI percentile scores.|
|EndDt|(Optional) The latest measure\_date for which you would like the BMI percentile scores.|

#### Sample Call
```sas
%GetKidBMIPercentiles(InSet  = s.test_kids
                   , OutSet  = s.test_bmis
                   , StartDt = 01jan2004
                   , EndDt   = 31dec2007
                   ) ;
```
### %make\_inclusion\_table

Creates a Dept. of Health & Human Services Inclusion Enrollment Report (form PHS 398/2590) report for an input set of MRNs, with Race, Ethnicity and Gender information drawn from the local vdw demographics table. Unlike many other VDW macros, the output of this one just gets spewed into the .lst file (and any ODS destinations you open prior to calling the macro). There is no dataset output.

Note that this output may also be of use for filling out PHS' Targeted/Planned Enrollment form.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Cohort|The name of the dataset containing the MRNs of the people who should go in your report.|

#### Sample Call
```sas
  ods rtf file =  '\\server\share\project\programming\output\inclusion\_table.rtf' ;

    %make_inclusion_table(cohort = perm.my_mrns) ;

  ods rtf close ;
```
### %GetAdultBMI

Wraps the Vital Signs workgroup's official %BMI\_adult\_macro macro (see below), allowing the user to specify a specific set of people and an optional timeframe over which they would like BMI measures for adults.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|The name of a dataset containing the MRNs of the people whose BMI measures you want.|
|OutSet|What would you like the output dataset of BMI measures to be called?|
|StartDt|An optional parameter specifying the **beginning** of the period over which you would like BMI measures.  This should be either a complete date literal (e.g., '25dec2005'd) OR the name of a date variable in &People. Leave this blank to go back as far as the file goes.|
|EndDt|An optional parameter specifying the **end** of the period over which you would like BMI measures.  This should be either a complete date literal (e.g., '25dec2005'd) OR the name of a date variable in &People. Leave this blank to go forward as far as the file goes.|

#### Sample Call
```sas
%GetAdultBMI(people = s.test_vitals
           , outset = s.test_adult_bmi
           , StartDt = '01jan2006'd
           , EndDt = '30jun2007'd) ;
```
### %BMI\_adult\_macro

Calculates BMI for adults and include a flag for reason that a BMI was not calculated.  This flag can have values of:

*   MISSING AGE
*   UNDER AGE 18
*   NO WT
*   NO HT
*   NO HT OR WT
*   WT OUT OF RANGE
*   BMI OUT OF RANGE

The BMI algorithm and cut-off recommendations were reviewed by the Obesity special interest group. This is meant to flag only those extreme values or situations where there is reason to suspect a data entry error, and further review may be warranted.

The macro assumes that the program is placed into the middle of a program. It assumes that libnames have been defined prior to the macro call, and indicates that the macro parameters have be fully qualified dataset names.

Three variables are created:

|Variable|Purpose|Format|
|--------|-------|------|
|BMI|BMI FOR ADULTS|Numeric|
|HT\_MEDIAN|MEDIAN HT FOR ADULTS|Numeric|
|BMI\_flag|BMI QC FLAG|$16.|

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|VITALS\_IN|These needs to have the following variables: MRN, HT, WT, and measure\_date. Feed in fully qualified name, i.e. use libname and dataset name together if reading a permanent dataset.|
|DEMO\_IN|These needs to have the following variables: MRN, birth\_date. Feed in fully qualified name, i.e. use libname and dataset name together if reading a permanent dataset.|
|VITALS\_OUT|Feed in fully qualified name, i.e. use libname and dataset name together if writing to a permanent dataset.|
|KEEPVARS|Optional parameter indicating the values to keep in your quality checking dataset. May be left blank to simply attach the two new variables to an existing dataset.|

#### Sample Call
```sas
%BMI_adult_macro(vitals_in = perm.my_vitals
               , demo_in = &_vdw_demographic
              , vitals_out = perm.with_bmi) ;
```
### %BP\_FLAG

Creates flags that can be used to determine quality of systolic and diastolic blood pressure fields. Cut-off recommendations reviewed by CVRN HTN Registry site PIs on 5/12/2010. This is meant to flag only those extreme values or situations where there is reason to suspect a data entry error, and further review may be warranted.

Three variables are created:

|Variable|Possible Values|
|--------|---------------|
|SYSTOLIC\_QUAL|NULL, ABN\_HIGH, ABN\_LOW|
|DIASTOLIC\_QUAL|NULL, ABN\_HIGH|
|SYS\_DIA\_QUAL|SYSTOLIC <= DIASTOLIC, DIFFERENCE < 20, DIFFERENCE > 100|

Note that NULL is only used when the other paired value for the blood pressure is not null.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|DSIN|Feed in fully qualified name, i.e. use libname and dataset name together if reading a permanent dataset. This macro program assumes that desired libraries have been defined previously in the program. StandardVars macro variables can be used.|
|DSOUT|Feed in fully qualified name, i.e. use libname and dataset name together if writing to a permanent dataset.|
|KEEPVARS|Optional parameter indicating the values to keep in your quality checking dataset.  May be left blank to simply attach the three quality checking variables to an existing dataset.|

#### Sample Call
```sas
%bp_flag(dsin=&_vdw_vitalsigns,
         dsout=bp_qc,
         keepvars= mrn measure_date systolic diastolic)

%bp_flag(dsin=cohort_vitals,
         dsout=cohort_vitals,
         keepvars=)

%bp_flag(dsin=&_vdw_vitalsigns,
         dsout=studylib.cohort_vitals,
         keepvars=mrn measure_date systolic diastolic ht wt)
```
### %pregnancy\_periods

Purpose: produces a dataset of probable periods of pregnancy for a cohort of women, as divined by the incidence of pregnancy-significant events found in Utilization data (procedures and diagnoses).

Inputs: A dataset with the MRNs of the women whose pregnancies you are interested in.

Output(s): A dataset of probable pregnancy periods. Optionally, the dataset of raw pregnancy-significant events.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inset|The name of the input dataset of MRNs whose utilization data you want to scan forPPPs|
|out\_periods|What you want the output dataset of PPPs to be called.|
|start\_date|Optional. The date limits over which you want to scan for PPPs. If not given, the range 1-jan-1966 through \[current date\] is used.|
|end\_date|Optional. |
|out\_events|Optional. A name to use for the dataset of pregnancy-signifying events that the macro generates prior to attempting to resolve those events into periods. Give a valid two-part name (e.g., my\_lib.preg\_events) to preserve the dataset for later inspection. Useful for debugging the macro and evaluating the code set used.|
|max\_pregnancy\_length|Optional. The number of days to use in figuring out when a new pregnancy must have started, in the absence of an intervening pregnancy-ending event (delivery, miscarriage, termination or stillbirth). Defaults to 270.|

#### Details & Caveats

Resolving individual pregnancy-signifying-events into periods of probable pregnancy is a difficult task--particularly for women whose utilization data capture is not complete. For best results limit your input dataset to women/periods where enrollment.outside\_utilization = N. In any event, the output of this macro should be closely scrutinized. At this writing it has passed basic sanity checks at Group Health (using the pharmacovigilance study cohort) but nothing more rigorous than that.

Dirty data will of course confuse this macro. One common form of dirty data found in the course of development was multiple pregnancy-ending events found within a few days of one another (e.g., a 'missed abortion' code on day 1, followed by a 'legal abortion with pelvic inflammation' on day 3). This macro currently merges any such events together into a single pregnancy, so long as they occur within 3 days of one another. Anything found later than 3 days becomes its own episode.

The diagnosis and procedure codes used to detect pregnancy events was developed at Harvard by Darren Toh. The code was written at Group Health by Roy Pardee.  You can see what codes are used to signify what by downloading [this access database](resolveuid/356229e5b6895e900cf07c141b3b3bdc). Please report bugs and missing codes to Roy.

The codeset provided by Harvard includes some codes that signify pregnancy testing. These are pulled and included in the out\_events dataset, but not used in the process of actually divining periods of pregnancy because so many of these tests are negative. Incorporating that information would be an obvious next step to take.

#### out\_periods Dataset:

|Variable|Details|
|--------|-------|
|preg\_episode|An ordinal counter (1st, 2d, 3rd) for the probable pregnancy signified on this record during the date range given.|
|probable\_onset|The date the pregnancy signified on this record probably began. This date is imputed from the type of outcome by counting back a variable number of days from the outcome. The number of days varies by the type of the outcome, according to the following scheme:<dl><dt>Delivery</dt><dd>270 days</dd><dt>Still Birth or Termination</dt><dd>112 days</dd><dt>Miscarriage</dt><dd>98 days</dd></dl><br>Because this variable is based on outcome\_category, it will be missinging for all periods whose outcome\_category = 'unknown'.|
|first\_sign\_date|Date of the first 'prenatal' type event attributable to the pregnancy signified on this record--possibly the date the pregnancy first became known to the health plan, or the date on which prenatal care began. For periods where there were no prenatal events (e.g., just a single 'delivered' type event) this variable will be missing.|
|first\_sign\_code|A procedure or diagnosis code that was found on first\_sign\_date. It is common for a single prenatal care visit to bear > of these codes--this is the first one the program ran across. Possibly useful for debugging the pregnancy code set.|
|outcome\_date|The date the last event signifying this pregnancy was found. For PPPs where there is a definite pregnancy-ending event found (e.g., a delivery, miscarriage, etc.) this is interpretable as the end of the pregnancy period.|
|outcome\_code|A procedure or diagnosis code that was found on outcome\_date. It is common for a single pregnancy-ending visit to bear > of these codes--this is the first one the program ran across.|
|outcome\_category|The outcome of the pregnancy signified on this record. One of:<ul><li>delivered</li><li>miscarried</li><li>terminated</li><li>stillborn</li><li>unknown</li></ul>|
|preg\_code\_count|The number of codes of type 'prenatal' found during the period described on this record. Higher numbers indicate more capture of pregnancy events.|

### %make\_denoms

Purpose: Creates a dataset of yearly counts of enrollees by age, sex and race, for use as denominators in calculating rates in VDW programs. Written to support general QA efforts.

Inputs: The start and end year over which you would like the denominators; the name of the output dataset.

Output: A dataset of counts, described in detail below.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|start\_year|The first year for which you would like enrollee counts.|
|end\_year|The last year for which you would like enrollee counts.|
|outset|The name you want for the output dataset of counts.|
|extra\_wh|An optional complete WHERE clause to limit the enrollment records to consider.  Useful for specifying that you e.g., only want to count people with Medicaid coverage, or those whose capture of lab results is not known to be incomplete.|

#### Output Dataset

Rows within a year can be summed without fear of double-counting.

|Variable|Contains|
|--------|--------|
|year|The year to which the data on this row pertain.|
|agegroup|Age group as of 1-January of \[\[year\]\].|
|drugcov|As defined in the enrollment file. Set to 'Y' if _any_ portion of a person's enrollment in \[\[year\]\] had drugcov = 'Y'.|
|race|Display-ready (not coded) race.|
|gender|As defined in the demographics file.|
|enrollment_basis|What sort of relationship between person and implementing site does this record document?|
|prorated\_total|Pro-rated number of people enrolled in \[\[year\]\] (accounts for partial enrollments by e.g., counting a person enrolled for half of \[\[year\]\] as .5).|
|total|The number of people enrolled at least one day in \[\[year\]\].|

#### Sample Call
```sas
%make_denoms(start_year = 1998
            , end_year  = 2015
            , outset    = lab_res_denoms
            , extra_wh  = %str(WHERE incomplete_lab ne 'K')
          ) ;
```

### %vdw\_formats

Purpose: Defines several useful formats that add descriptive text to coded values.

Inputs: None

Output: [The formats listed here](resolveuid/4b88b08a31c3e4fc5897a633b373794b) are defined in a format catalog in the current session.

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|lib| Optional.  The library in which you would like the format catalog defined.  By default this is WORK.|
|tweaked_descriptions| Optional.  If set to 1, then makes sure labels begin with the raw value, followed by a textual description in parentheses.|


### %detect\_phi

Purpose: Describes the datasets in a SAS library intended for transfer off-site, calling particular attention to any obvious elements of protected health information, to alert site staff to any possible unintentional disclosures.

The macro checks for:

*   Variable Names commonly given to sensitive data items:
    *   MRN
    *   HRN
    *   birth\_date/DOB/BirthDate/BDate
    *   SSN/SocialSecurityNumber/social\_security\_number/socsec
       Variable names listed in a locally maintained macro var (which lives in [stdvars.sas](resolveuid/729705d1b28ad028b4b6287ddf3713d6)) called &locally\_forbidden\_varnames
*   The contents of all character variables for matches to the local pattern of MRNs (given in another macro var in stdvars).
*   The contents of all numeric variables formatted as dates for values that signify events older than &eldest\_age years ago. Variables are evaluated in this check only if they have a date format applied, or if they are numeric variables whose names containhe string 'dte'.

Inputs: The name of the SAS library containing the datasets to be sent off-site.

Output: Warnings/CONTENTS/PROC PRINT output to the listing file (or ODS destination).

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|transfer_lib|  The name of the library containing the datasets to be sent off-site. Put all datasets to be sent (and only datasets to be sent) in a single location, named in this parameter.|
|obs_lim| Optional. The macro inspects the contents of character and date variables, looking for possibly sensitive data items (MRNs and dates more than &eldest_age years ago). This parameter will limit the number of records so inspected, for applications where the volume of inspected data is so high as to make comprehensive checks prohibitively time-consuming. If not specified the macro will check every record.|
|eldest_age|  Optional. The oldest number of years whose events you want to consider sensitive. By default dates older than 89 years ago are considered sensitive.|

#### Output:

A report for each dataset in &transfer\_lib, consisting of:

*  A list of variable names and associated warnings, if there are any.
*  PROC CONTENTS output
*  A printout of the first 20 records.

#### Instructions:

1. Place all dataset files to be transferred off-site into a single folder/directory (better VDW programs will do this for ou, but if not, segregate the to-be-transferred datasets all by themselves in a single location, to minimize the chance of accidentally sending a dataset you don't mean to).
2. In a new SAS program, define a libname against this location.
3. (Recommended) Open a new ODS destination to enhance the readability of the output.
4. Call %detect\_phi, passing in the libname defined in step 2.
5. Review the output with your local Project Manager to make sure the data elements are all in line with your understanding of what data is to move off-site, any applicable data sharing agreements, etc.
6. In reviewing the output, pay particular attention to any Warnings, but ***carefully scrutinize the whole report***. This macro should detect the most obvious sensitive data items, but **no macro is a substitute for careful inspection and human thought.**

### %generate_counts_rates

Purpose: Creates a dataset of counts and rates of the codes supplied in the &incodeset parameter. Written to support general QA efforts.

Inputs: A list of codes on which you'd like counts/rates; the start and end dates over which you would like the counts; an optional set of MRNs of the people in whose counts you're interested; the path & filename you want for the output dataset.

Output: A dataset of counts and rates, described in detail below.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|incodeset| The name of the input dataset of codes-of-interest (described below).|
|start_date|  A SAS date constant without delimiters (e.g., 01Jan2005) that marks off the beginning of the period during which you want the counts/rates.|
|end_date|  A SAS date constant without delimiters (e.g., 01Jan2005) that marks off the end of the period during which you want the counts/rates.|
|cohort|  The name of a SAS dataset containing the MRNs of a cohort of people that you would like the counts/rates limited to.|
|outpath| A directory specification naming the location where you would like the output dataset.|
|outfile| Base name of the output files (so--no extension). 'my_file' will produce ```<<siteabbrev>>_my_file.sas7bdat```|
|censor_low|  Optional.  If set to N, will leave lower-than-&lowest_count counts in the data.  Useful for applications where IRB approvals are sufficient to transfer very low counts.|

#### InCodeSet Dataset
|Name|Description|Valid Values|
|----|-----------|------------|
|data_type|A code signifying what type of data we are after (e.g, rx fills; diagnoses).|<ul><li>PX</li><li>DX</li><li>LAB</li><li>NDC</li></ul>|
|code_type|One of the valid values for px_codetype, dx_codetype, or null for NDCs/Labs| n/a
|category|  A user-specified string that can be used to group codes into categories (e.g., 'Analgesics', 'Therapeutic Radiation'). This should always have a value in it even if all your codes fall into a single category (e.g., you're only interested in analgesics & don't care to make any finer distinctions) because the values in this var are used by the %report_counts_rates macro described below. |n/a|
|descrip| a more fine-grained description of the particular code. You could think of this as a subcategory (since codes w/the same descrip value get rolled up at the reporting stage). |n/a|
|code|  The actual NDC, ICD-9, etc. code. |n/a|

#### Output Dataset
|Variable|Contains|
|--------|--------|
|num_recs|  The raw number of records found w/the given code in the appropriate VDW dataset, during the period set out by &start_date and &end_date.|
|num_ppl| The number of distinct MRNs (people) found with this code in the appropriate VDW dataset, during the period desired.|
|num_enrolled_ppl|  The number of distinct MRNs of enrolled people in the dataset/during the period. Enrollment here is enrolled at the time of the event (rx fill; diagnosis) in question. (This may be a different count than the old %VDWCountsAndRates1 macro.).|
|rate_enrolled_ppl| The per-10k-enrollees rate of this code in the enrolled population. (The denominator here is the count of people enrolled on &start_date.)|

#### Sample Call
Here is a sample input datastep that produces a valid &InCodeSet dataset:
```sas
data gnu ;
  infile datalines truncover ;
  input
    @1    data_type   $char3.
    @7    code_type   $char2.
    @13   category    $char30.
    @45   code        $char12.
    @59   descrip     $char200.
  ;
  ** if data_type = 'DX' ;
datalines ;
PX    C4    Pretend Category                99211         Evaluation/Maintenance of Existing Patient
PX    C4    DEXA Scans                      3095F         Central dual-energy X-ray absorptiometry (DXA) results documented (OP)
PX    C4    DEXA Scans                      3096F         Central dual-energy X-ray absorptiometry (DXA) ordered (OP)
PX    C4    DEXA Scans                      76075         DXA BONE DENSITY,  AXIAL
PX    C4    DEXA Scans                      76076         DXA BONE DENSITY/PERIPHERAL
PX    C4    DEXA Scans                      76077         DXA BONE DENSITY/V-FRACTURE
PX    C4    DEXA Scans                      77080         Dual-energy X-ray absorptiometry (DXA), bone density study, 1 or more site
PX    C4    DEXA Scans                      77081         Dual-energy X-ray absorptiometry (DXA), bone density study, 1 or more site
PX    C4    DEXA Scans                      77082         Dual-energy X-ray absorptiometry (DXA), bone density study, 1 or more site
PX    H4    DEXA Scans                      G8399         PATIENT WITH CENTRAL DUAL-ENERGY X-RAY ABSORPTIOMETRY (DXA) RESULTS DOCUME
PX    H4    DEXA Scans                      G8400         PATIENT WITH CENTRAL DUAL-ENERGY X-RAY ABSORPTIOMETRY (DXA) RESULTS NOT DO
PX    C4    Radiology Exams                 78315         78315:Bone and/or joint imaging: 3 phase study
PX    C4    Radiology Exams                 78306         78306:Bone and/or joint imaging: whole body
PX    C4    Radiology Exams                 78305         78305:Bone and/or joint imaging: multiple areas
PX    C4    Radiology Exams                 78300         78300:Bone and/or joint imaging: limited area
PX    C4    Radiology Exams                 78320         78320:Bone and/or joint imaging: tomographic (SPECT)
DX    09    Pretend Category                V82.81        Screening for osteoporosis
DX    09    Pretend Category                V72.84        Pre-operative procedure, unspecified
NDC         Pretend Category                00002323704   Fascinating NDC.
LAB         Pretend Category                ALBUMIN       Lab tests I have known.
;
run ;
```
### %report\_counts\_rates

Purpose: Creates aggregate reports from the datasets produced by %generate\_counts\_rates. Written to support general QA efforts.

Inputs: The libname containing the site-submitted datasets generated by runs of %generate\_counts\_rates; a libname giving the location where you would like the output dataset & excel spreadsheet file(s).

Output: An aggregate dataset of counts/rates, and one excel spreadsheet file per value of category found in the input datasets.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inlib|The library where the site-submitted datasets have been placed.|
|dset\_name|The stub name of the datasets you want to aggregate for this report. This value should match the one you specified for &outfile in your call to %generate\_counts\_rates.|
|outlib|The library where you would like the output to be placed (a single, aggregate dataset of counts/rates, plus one or more excel spreadsheet containing counts/rates of the codes by category).|
|sitefmt|Optional.  The name of a character format to use to format site values appearing in the columns of the spreadsheets.|

#### Output Dataset

The Output dataset contains everything from &InCodeSet, plus a 'site' variable giving the abbreviation of the site that supplied that row.

### %stack\_datasets

Purpose: Takes a set of site-submitted datasets and creates a single dataset with all data, and adds a "site" variable indicaing the source of the record. Useful for collating site-submitted data.

Inputs: The libname containing the site-submitted datasets; the base name of the datasets you would like to aggregate; a libname giving the location where you would like the output dataset to be written.

Output: A single aggregate dataset holding all the data in the set of input datasets, with an additional "site" variable.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inlib|The library where the site-submitted datasets have been placed.|
|nom|The stub name of the datasets you want aggregated in the output. This macro assumes that the datasets have all been named according to the following convention:<br>```<<&_SiteAbbr>>_<<base_name>>```<br>That is, that any characters preceding the first underscore are a site abbreviation (probably a numeric code would also work), and all the datasets have the same base name.|
|outlib|The library where you would like the output dataset to be written.|

#### Output Dataset

The output dataset contains everything from the constituent site-submitted datasets, plus a 'site' variable giving the abbreviation of the site that supplied that row.

### %GetCombinedRace

Purpse: Fetches race data for an input set of MRNs and creates a single CombinedRace variable that summarizes the information available in RACE1 - RACE5, according to the following rules:

*   If only one race is listed, set CombinedRace to that race.
*   If person is WH and one other, set CombinedRace to that other (or to "MU" depending on the value given in the &WHOther paraeter
*   If there is more than one non-WH race, or one of the races is "MU", set CombinedRace to "MU".
*   Otherwise, set CombinedRace to "UN".

Inputs: An input dataset of MRNs of the people on whom you would like race info.

Output: A dataset containing MRN, CombinedRace, and RACE1 - RACE5.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inset|The list of MRNs whose race info you want.|
|outset| What you want the output dataset to be called.|
|WHOther |What to do for people with WHite and one other race value.<br>If set to 'other', CombinedRace is assigned whatever the non-WHite value is. This is the default.<br>If set to 'MU', CombinedRace is assigned the value 'MU'.|
|Freqs|If set to 'Y', outputs frequency tables comparing CombinedRace to the native RACE<X> variables. The default is 'N'.|

### %SEER\_SITE\_RECODE

Developed by Rebecca Ziebell (ziebell.r@ghc.org), this macro recodes VDW.TUMOR-formatted records into SEER site recode vales, which define the major cancer site/histology groups that are commonly used in reporting of cancer incidence data. Additional information about the SEER site recode can be found at [seer.cancer.gov/siterecode](seer.cancer.gov/siterecode ). As of 08/08/2015, this macro uses the Site Recode ICD-O-3/WHO 2008 definition, which is defined here: [seer.cancer.gov/siterecode/icdo3\_dwhoheme](seer.cancer.gov/siterecode/icdo3_dwhoheme).

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inds|  Name of an input dataset of tumor records. Must contain VDW Tumor-style variables ICDOSITE and MORPH.|
|outds| Name desired output data set, which will be a copy of inds plus the variable site_recode.|


_Note:_
```%ser_site_recode``` also establishes two numeric formats to be used with the site\_recode variable: simple format site\_recode. and multilabel format recode\_mlf.

#### Sample Call
```sas
%seersite_recode(inds=all_tmr, outds=tmr_recoe) ;
```

### %GetLabForPeopleAndLab

Gets the results for a specified set of test_types, for a specified set of people (identified by MRNs) which occurred between the dates specified in StartDt and EndDt.

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|People|  Name of an input dataset of MRNs representing the people whose lab results you want.|
|LabLst|  Name of an input dataset of TEST_TYPEs representing the result types you want.|
|StartDt| The beginning of the period of interest.|
|EndDt| The end of the period of interest|
|OutSet|  Name for the output dataset of lab results.|

#### Sample Call
```sas
%GetLabForPeopleAndLab(People  = perm.my_cohort
                     , LabLst  = perm.my_test_types
                     , StartDt = 01jan2005
                     , EndDt   = 31dec2007
                     , Outset  = perm.lab_results
                      ) ;
```

### %elixhauser
Calculates the Elixhauser comorbidity score for an input cohort of people.

Input: A dataset of MRNs & index dates.

Output: A dataset of MRNs, disease flags and final Elixhauser scores.  There are four scores reported:

1. The summary score for all conditions detected over the period from (index_date - days_prior) to index_date (SumElixWCancer).
1. The summary score for all conditions OTHER THAN CANCER detected over the period from (index_date - days_prior) to index_date (SumElixWoCancer).
1. The vanWalRaven weighted score for all conditions detected over the period from (index_date - days_prior) to index_date (WghtElixWCancer).
1. The vanWalRaven weighted score for all conditions OTHER THAN CANCER, detected over the period from (index_date - days_prior) to index_date (WghtElixWoCancer).
In addition to these scores there are 31 0/1 flags signifying the component conditions (lymphoma, renal failure, etc.) for each period ('index' and 'prior').

#### Background
Developed at KPWA by Joey Eavey and Arvind Ramaprasan. Based on a macro written by Anne Elixhauser's team at AHRQ/HCUP. This macro calculates the summary Elixhauser score and also calculates the weighted score using vanWalRaven comorbidity weights. The current version of the macro generates one score per patient-index_date combination. So, if a patient has multiple visits within the input dataset, separate Elixhauser scores will be generated for each index date. This code includes both ICD9 and ICD10 diagnoses, as developed by HCUP and updated using FY2018 ICD-10 codes.

ICD10 codes were added to the macro by ARHQ/HCUP. Joey made some comments about ICD code considerations. Joey made some comments about her code validation process [on this page](https://www.hcsrn.org/share/page/site/VDW/wiki-page?title=Elixhauser_Macro_Code_Mapping_Validation).

##### References

[HCP website. Beta Elixhauser Comorbidity Software for ICD10-CM](https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp). Retrieved 1-dec-2017.

[HCUP website. Elixhauser Comoribidity Software, Version 3.7](https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/|comorbidity.jsp) Retrieved 1-dec-2017.
A Modification of the Elixhauser comorbidity Measures Into a Point System for Hospital Death Using Administrative Data. Med Care 2009;47:626-633.

Identifying Increased Risk of Readmission and In-hospital Mortality Using Hospital Administrative Data: The AHRQ Elixhauser Comorbidity Index. Med Care 2017;55: 698–705

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inputds| Name of a dataset containing MRNs and index dates for a cohort of people. It is permissible to have > 1 index date per MRN.|
|mrn| The name of a variable in &inputds that holds MRN values.  Default is 'mrn'.|
|index_date|  The name of a variable in &inputds that holds the index date values. Default is 'index_date'.|
|outputds|  The name of the output dataset with scores and flags to write.|
|days_lookback| The number of days to look back from index_date for defining the period over which to find diagnoses. Default is 365.|
|inpatonly| Specifies the types of encounters to include (I= inpatient only, B= inpatient and ambulatory only, A = all encounters, C = specify a custom encounter type list using &enctype_list)|
|enctype_list|  For use with inpatonly = C, use to specify a custom encounter type list|
|DRG_restriction| Y/N that specifies whether the DRG restriction should be applied to inpatient encounter diagnoses. Default is Y.|

#### Sample Call
```sas
%elixhauser(inputds       = perm.my_cohort
          , mrn           = chsid
          , index_date    = index_dt
          , outputds      = perm.elixhauser_scores
          , inpatonly     = C
          , enctype_list  = %str('AV', 'IP', 'UC')
          , days_lookback = 365
         ) ;
```

### %gagne
Purpose: Calculates Gagne's combined comorbidity score, using Elixhauser's ICD-9 diagnosis codes.

Inputs: A dataset of MRNs and index dates.

Outputs: A dataset with MRN, index date, and a var called combined_score which holds the Gagne score.

**Note that this macro will only process ICD-9 diagnosis codes, and so it is not suitable for index dates that fall on or after 1-october-2015.**

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Inputds| Name of the input dataset of MRNs and index dates.|
|IndexDateVarName|  Name of the variable in &inputds that holds the index date. Must not fall after 1-october-2015.|
|outputds|  Name you want to give the output dataset.|

#### Sample Call
```sas
%gagne(Inputds           = s.test_gagne
      , IndexDateVarName = index_date
      , outputds         = s.test_gagne_out
      );
```

### %GagneQuan
Purpose: Calculates Gagne's Combined Comorbidity Score based on Quan's "Enhanced" ICD-9-CM Codes & ICD-10 diagnoses codes and original weightings per Gagne et. al.

Inputs: A dataset of MRNs and index dates.

Outputs: A dataset with MRN, index date, and a var called CombinedScore which holds the enhanced Gagne score.

> NOTE:  ICD 9 &10 codes used by Quan et. al.  in developing the combined score were based on administrative hospital discharge data for patients aged 18+ year and older from Canadian health region. It might be worth noting that Canadian and US coding practices may differ slightly, affecting the combined score. As a result,  the final score for the US region might not be equivalent to the combined score used when developing the index.

#### References
Hude Quan, Vijaya Sundararajan, Patricia Halfon, Andrew Fong, Bernard Burnand, Jean-Christophe Luthi, L. Duncan Saunders, Cynthia A. Beck, Thomas E. Feasby and William A. Ghali; Coding Algorithms for Defining Comorbidities in ICD-9-CM and ICD-10 Administrative Data; [Medical Care , Nov., 2005, Vol. 43, No. 11 (Nov., 2005), pp. 1130-1139](https://www.jstor.org/stable/3768193)

Gagne JJ, Glynn RJ, Avorn J, Levin R, Schneeweiss S. A combined, comorbidity score predicted mortality in elderly patients better than existing scores. Journal of Clinical Epidemiology 2011 Jan, 3

#### Parameters
|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Inputds| Name of the input dataset of MRNs and index dates.|
|IndexDateVarName|  Name of the variable in &inputds that holds the index date.|
|outputds|  Name you want to give the output dataset.|

#### Sample Call
```sas
%GagneQuan(Inputds          = s.test_gagne
         , IndexDateVarName = index_date
         , outputds         = s.test_gagne_quan_out
         );
```

### %FrailtyIndex

Calculates Segal's Frailty Probability Estimate

Inputs: A dataset of MRNs and index dates.

tputs: A dataset with MRN, a series of flags for various comorbid conditions (e.g., stroke, falls, etc.) a var called p\_frailty which holds the Segal probability of frailness score and a var call frail which is a flag for values of p\_frailty >=.2.

**Note that this macro will only process ICD-9 diagnosis codes, and so it is not suitable for index dates that fall on or after 1-october-2015.**

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|Inputds| Name of the input dataset of MRNs and index dates.|
|IndexDateVarName|  Name of the variable in &inputds that holds the index date. Must not fall after 1-october-2015.|
|outputds|  Name you want to give the output dataset.|
|worklib| Optional. Name of the libname to write interim datasets to. Default is 'work'.|


### %faurot
Calculates the Faurot frailty score for an input cohort of people.

Input: A dataset of MRNs & index dates.

Output: A dataset of MRNs, disease and demographic flags and final Faurot frailty score.

#### Background
Developed at KPWA by Joey Eavey. Based on Dr. Faurot's published code and ICD9 formats. This macro calculates the Faurot frailty index using diagnosis and procedure codes to generate the predicted probability of an individual being 'frail' as of the index date. The current version of the macro generates one score per patient-index date combination. So, if a patient has multiple visits within the input dataset, separate frailty scores will be generated for each index date. This code includes both ICD9 and ICD10 diagnoses.

ICD10 codes were added to the macro by Joey Eavey using the 2018 CMS GEMS file to map ICD9 codes to ICD10. These codes have not yet been validated in a large cohort and programmers should examine trends in frailty scores across years for consistency.

#### References
Using Claims Data to Predict Dependency in Activities of Daily Living as a Proxy for Frailty. Pharmacoepidemiol Drug Saf 2015; 24(1):59-66.

**Note that there are errata to this article that are not readily apparent on PubMed — see updated supplemental information on UNC website below for more info.**

Harry Guess Research Community at University of North Carolina Gillings School of Global Public Health website. [SAS macro for predicting frailty using claims data](http://sph.unc.edu/epid/harry-guess-research-community/). Accessed 1/2/2018.

#### Sample Call
```sas
%faurot(inputds        = perm.my_cohort
     , mrn             = chsid
     , index_date      = index_dt
     , days_lookback   = 238) ;

```

### %ReprioritizeRace

The `race1-race5` variables in &\_vdw\_demographic are set according to a specific preference hierarchy, as described in the spec.  `%ReprioritizeRace` allows users to shuffle the values in those variables according to their own preference hierarchy.  Users define their hierarchy in a format that translates the 8 allowable race values (AS, BA, WH, etc.) into a numeric rank that expresses the preferred rank order (lower numbers are more-favored, higher numbers are less-favored).

Input: A dataset of MRNs, and a preference-ranking format.

Output: A dataset of MRNs and `race1-race5` variables, whose values are set according to the input hierarchy.

#### Parameters

|Parameter Name|Parameter Purpose|
|--------------|-----------------|
|inset|Name of a dataset containing the MRNs of the people whose race values you want to shuffle.|
|pref_format|Name of a format you have defined, which converts the 8 allowable race values (AS, BA, WH, etc.) into a numeric rank that constitutes your preferred rank order (lower numbers are more-favored, higher numbers are less-favored).|
|outset|Name of the dataset the macro should write, with its shuffled race variables|

#### Sample Call
```sas
proc format ;
  * Make Black/African-American the most-preferred value. Demote White under Multi-racial and Other ;
  value $prior
    'BA' = 1
    'HP' = 2
    'IN' = 3
    'AS' = 4
    'MU' = 5
    'OT' = 6
    'WH' = 7
    'UN' = 8
  ;
quit ;

%ReprioritizeRace(inset = s.test_race_reshuffle
              , pref_format = $prior
              , outset = s.reshuffled) ;

```

Hi VIG meeting!
