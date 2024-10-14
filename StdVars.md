# The VDW Standard Variables File: StdVars.sas

## Purpose
Strictly speaking, StdVars.sas defines a set of standard macro variables that VDW programmers can use to refer to the datasets that make up the VDW implementation at a site. More generally, StdVars.sas sets up the VDW environment at a given site. This environment includes:

* dataset variables
* site information variables
* filenames (including especially vdw_macs which you can %include to bring [the VDW standard macros](readme.md) into your session)
* system options

## The Standard Variables
The intention here is that the below table holds a complete and accurate list of the core VDW standard variables available to application programs. To be absolutely sure you are up-to-date, please consult the authoritative copy of [the VDW data specifications](https://hcsrnvdw.sharepoint.com/:x:/r/sites/hcsrn-vdw/_layouts/15/Doc.aspx?sourcedoc=%7B9ED82C09-E95D-4F70-B444-843DA177930D%7D&file=VDWSpecifications_V5.xlsx&action=default&mobileredirect=true) on HCSRN's sharepoint.

|Category|Variable|Description|Sample Value|
|---|---|---|---|
|Dataset Reference|`&_vdw_tumor_main`|A two-part identifier for the local Tumor dataset|\_my\_vdw\_lib.my\_tumor\_dset|
|Dataset Reference|`&_vdw_tumor_supp`|" for the local Tumor (EAV supplement) dataset|\_my\_vdw\_lib.my\_tumor\_supp\_dset|
| |`&_vdw_enroll`|" for the Enrollment dataset|\_my\_vdw\_lib.enroll2|
| |`&_vdw_demographic`|" for the Demographics dataset|\_my\_vdw\_lib.demos|
| |`&_vdw_language`|" for the Person Languages dataset|\_my\_vdw\_lib.languages|
| |`&_vdw_rx`|" for the Pharmacy (rx fills) dataset|\_my\_vdw\_lib.rx|
| |`&_vdw_everndc`|" for the EverNDC dataset|\_my\_vdw\_lib.everndc|
| |`&_vdw_utilization`|" for the Utilization (encounters) dataset|\_my\_vdw\_lib.ute|
| |`&_vdw_dx`|" for the Diagnoses dataset|\_my\_vdw\_lib.dx|
| |`&_vdw_px`|" for the Procedures dataset|\_my\_vdw\_lib.px|
| |`&_vdw_death`|" for the Death dataset|\_my\_vdw\_lib.death|
| |`&_vdw_cause_of_death`|" for the Cause of Death dataset|\_my\_vdw\_lib.cod|
| |`&_vdw_provider`|" for the Provider dataset|\_my\_vdw\_lib.prov|
| |`&_vdw_prov_taxonomy`|" for the Provider Taxonomy dataset|\_my\_vdw\_lib.prov_taxonomy|
| |`&_vdw_facility`|" for the Facility dataset|\_my\_vdw\_lib.prov_taxonomy|
| |`&_vdw_vitalsigns`|" for the Vital Signs dataset|\_my\_vdw\_lib.vitals|
| |`&_vdw_census_loc`|" for the Census: Patient Locations dataset|\_my\_vdw\_lib.census_location|
| |`&_vdw_census_demog_acs`|" for the Census: American Community Survey Geo-Demographics dataset|\_my\_vdw\_lib.census_demog|
| |`&_vdw_census_demog_dec`|" for the Census: Decennial Geo-Demographics dataset|\_my\_vdw\_lib.census_demog|
| |`&_vdw_lab`| " for the Lab Results dataset|\_my\_vdw\_lib.lab_results|
| |`&_vdw_lab_notes`|" for the Lab Result Notes dataset|\_my\_vdw\_lib.lab_long_char|
| |`&_vdw_social_hx`|" for the Social History dataset|\_my\_vdw\_lib.SocialHx|
| |`&_vdw_pro_types`|" for the Patient Reported Outcome Types dataset|\_my\_vdw\_lib.pro\_types|
| |`&_vdw_pro_surveys`|" for the Patient Reported Outcome Surveys dataset|\_my\_vdw\_lib.pro\_survs|
| |`&_vdw_pro_responses`|" for the Patient Reported Outcome Responses dataset|\_my\_vdw\_lib.pro\_resps|
|Site Identification|`&_SiteName`|Full name of the site|St Louis University/AHEAD Institute|
| |`&_SiteAbbr`|Initials of the site|SLU|
| |`&_SiteCode`|An arbitrary two-digit numeric code identifying each site (are we maintaining these? they've sort of fallen out of favor).|01|
|Utility|`&lowest_count`|Many sites /projects have something like blanket IRB approval for generating/sending frequency data, so long as cells with "low" counts are masked. This variable holds what is considered "low" at the site. The most commonly used value is 5. You can use this in your code to mask or redact counts that are higher than this number.|5|
| |`&mrn_regex`| A regular expression that matches the local MRN format. Used by the macro `%detect_phi` to work out whether a given value could possibly contain an MRN value.|d{8}|
| |`&locally_forbidden_varnames`|A pipe-delimited list of variable names that should trigger a warning in the ouput of the macro `%detect_phi`. Generally a list of the various synonyms for MRN and/or other variable names commonly found at the site that contain values that should not (lightly) travel across sites. Not case sensitive.|consumno\|chsid\|pat\_id

## Version-Specific Variables
From time to time we may define additional variables to refer to prior or future-versioned data formats (e.g., a `_vdw_demog_v2` var to refer to version-2 formatted demographic data). For details on these, see the current VDW implementation overview page , and any Roadmap pages linked from that page. The model StdVars.sas file should also have a complete list of the variables currently considered as current.

## No Standard SAS Libraries
Note that sites are free to store their VDW data in a variety of places--they can keep them all as flat SAS datasets in a single directory, or can put each file in its own directory, or they can put some or all of their VDW data in tables on a server database. Wherever the data ultimately lie, sites define /whatever libraries are necessary to populate the dataset reference variables, but there is no standard library that users should count on existing. ***VDW users should not assume that any particular library is defined in their code unless they define it themselves.***

## For Site Data Managers
One other crucial bit of environment for VDW users is access to the VDW standard macros. These are achieved by way of the FILENAME vdw_macs blah blah blah statement in StdVars.sas. Depending on your local firewall and other policies, you may have a choice for how your local users get at these macros (and, in cases where macros load data (e.g., %vdw_formats() how they get at that data)). Those choices are:

1. Point your vdw_macs reference directly at the files on kpwhri.github.io.
2. Maintain local copies of those files, and point your users at those instead.

### Why you might want to read the code/data directly off kpwhri.github.io

1. Your sas environment, network configuration, and security policy does not prevent it.
2. You want your users to have the benefit of any updates (e.g., fixes, new macros) as soon as they are released, without having to wait for you to update your local copy.
3. You don't have the time or inclination to scrutinize updates to ensure their safety.
4. You trust that those empowered to change this code (currently [the KPWHRI organization owners](https://github.com/orgs/kpwhri/people?query=role%3Aowner)) will not do nefarious things to it.
5. You trust that the people w/edit access to this repo will keep their passwords/ssh keys & second-factor authorization means private, so that no one else will get this access.
6. You trust that github.com will not be compromised (or if it is, that this code is not disturbed).

### Why you might want to maintain your own copies of this code/data

1. Your sas environment, network configuration, or security policy makes reading off the central server impossible.
1. You want to review changes to code/data before allowing them to be inflicted on your users.
1. You don't mind paying attention to notices of changes (which Roy will send to the hcsrn-vdw-implementation listserv) and updating your local files as necessary.
1. You don't trust all of 4, 5 & 6 above.

In order to support this flexibility, we have two additional macro variables defined

|Variable|Description|Comments|
|--------|-----------|--------|
|`&_vdw_asset_engine`|Which FILENAME engine to use to read the various asset files|Should be one of two values: URL for direct reads off of kpwhri.github.io, or blank for reading out of a local directory|
|`&_vdw_asset_loc`|Location from which to read the various asset files|Should be set to either `http://kpwhri.github.io/public_vdw_assets` for direct reads off kpwhri.github.io or a local directory spec (e.g., `//my_server/my_share`).|

## The macro 'assets'

|File|Description|
|----|-----------|
|[standard_macros.sas](http://kpwhri.github.io/public_vdw_assets/standard_macros.sas)|The main macros file|
|[cdc-child-bmi-code.sas](http://kpwhri.github.io/public_vdw_assets/cdc-child-bmi-code.sas)|CDC code for assigning percentile scores to pediatric Body Mass Index measurements. Called by the wrapper macro %GetKidBMIPercentiles.|
|[CDCref_d.xpt](http://kpwhri.github.io/public_vdw_assets/CDCref_d.xpt)|A reference dataset containing the normative data needed by the above CDC code.|
|[formats.xpt](http://kpwhri.github.io/public_vdw_assets/formats.xpt)|A dataset containing format data for the various formats defined by %vdw_formats.|
|[pregnancy_codes.xpt](http://kpwhri.github.io/public_vdw_assets/pregnancy_codes.xpt)|A dataset of diagnosis & procedure codes used by the %pregnancy_periods macro.|

# A Sample StdVars.sas

```sas
** This keeps SAS from dumping raw records into the log. ;
options errors = 0 ;

* Note that this is a sas/access for teradata type
  libname--it could easily be oracle, or a plain file
  system directory--whatever works for your site. ;

* The name is intentionally wacky in order to keep from
  accidentally redefining something in user code. ;

libname __grond teradata
  user              = "&username"
  password          = "%superq(pwd)"
  server            = "my_teradata_server"
  schema            = "vdw_schema"
  multi_datasrc_opt = in_clause
  connection        = global
  access            = readonly
;

  ** libname locations specs ;

  ** 'Standard' VDW DATASET VARIABLES ;

  ** Consider un-commenting this in order to keep off-site-written code from accessing ;
  ** data other than what is in your VDW libs. ;
  ** libname _all_ clear ;

  ** In the same vein, if youve got significant off-spec site-specific-enhancement vars in your VDW,    ;
  ** consider putting up very simple sql views that just select the official VDW vars, and point your   ;
  ** dset vars at *those* rather than the raw dsets.  That way if user code does a "select * from blah" ;
  ** they wont get extra stuff they wont be expecting (and you may not want to give!).                  ;

  ** You are also free to define any number of different libnames, if your VDW dsets are stored in different locations. ;
  ** Note also that this is not a "standard" libname--there are no standard libnames.  Please see the above-referenced URL ;
  ** for details. ;

   %let _vdw_tumor                = __grond.tumor             ;
   %let _vdw_tumor_main           = __grond.tumor_eav_main    ;
   %let _vdw_tumor_supp           = __grond.tumor_supp        ;
   %let _vdw_enroll               = __grond.enroll            ;
   %let _vdw_demographic          = __grond.demog             ;
   %let _vdw_language             = __grond.person_languages  ;
   %let _vdw_rx                   = __grond.cesr_rx           ;
   %let _vdw_everndc              = __grond.everndc           ;
   %let _vdw_utilization          = __grond.utilization       ;
   %let _vdw_dx                   = __grond.dx                ;
   %let _vdw_px                   = __grond.px                ;
   %let _vdw_provider             = __grond.provider          ;
   %let _vdw_prov_taxonomy        = __grond.prov_taxonomy     ;

   %let _vdw_vitalsigns           = __grond.vitalsigns        ;
   %let _vdw_census_loc           = __grond.census_location   ;
   %let _vdw_census_demog_acs     = __grond.census_demog_acs  ;
   %let _vdw_census_demog_dec     = __grond.census_demog_dec_v;
   %let _vdw_lab                  = __grond.lab_results       ;
   %let _vdw_lab_notes            = __grond.lab_results_notes ;
   %let _vdw_death                = __grond.death             ;
   %let _vdw_cause_of_death       = __grond.cod               ;
   %let _vdw_social_hx            = __grond.social_hx         ;
   %let _vdw_facility             = __grond.vdw_facilities    ;
   %let _vdw_pro_survey_responses = __grond.cesr_pro_srvyrspn ;
   %let _vdw_pro_surveys          = __grond.cesr_pro_surveys  ;
   %let _vdw_pro_types            = __grond.cesr_pro_types    ;


  ** Reference to the location of VDW standard 'assets'. ;
  /*
    'Assets' here means standard macros, programs and in some
    cases supporting data (e.g., the pregnancy macro, which
    reads in a separate dset of dx/px codes).

    These things can be read directly off the HCSRN web site
    (in which case you should use the values below) or from a
    locally maintained repository, for sites that are not able
    to or comfortable with dynamically reading code off an
    external host.

    The HCSRN files should be considered the
    authoritative/canonical source of these assets.  If you
    cannot or do not want to use those directly, it will be
    your responsibility to keep your local versions
    up-to-date.

    To keep abreast of changes, send e-mail to
    chr_vdw_voc@kpchr.org and ask to be added to the 'VDW
    Implementation Group' e-mail list.
  */

  * Sample values to read code/supporting data off of github ;
  %let _vdw_asset_loc = http://kpwhri.github.io/public_vdw_assets ; * <-- root location for standard code/data assets. ;
  %let _vdw_asset_engine = URL ;               * <-- which filename engine to use for assets (leave blank if local dir) ;

  * Sample values to read code/supporting data out of a local directory. ;
  %let _vdw_asset_loc = //my_server/vdw_public_assets ;
  %let _vdw_asset_engine = ;

  * define a filename so these macros can easily by %include'ed into user code if desired. ;
  filename vdw_macs &_vdw_asset_engine "&_vdw_asset_loc/standard_macros.sas" ;

  * Site identifier vars ;
  %let _SiteCode = 01 ; * <-- dont use this ;
  %let _SiteAbbr = KPWA;
  %let _SiteName = KP Washington ;

  /*
    Many sites/projects have something like blanket IRB approval
    for generating/sending frequency data, so long as cells
    with "low" counts are masked. This variable should hold
    what is considered "low" at your site. The most commonly
    used value is 5.
  */

  %let lowest_count = 5 ;

  /*
    Variables used by the detect_phi macro.

    A regular expression giving the pattern that your MRN values
    follow. Used to check character vars for possibly holding
    MRNs.

    The pattern given below will match any 10 consecutive
    uppercase alpha or numeric characters.
  */
  %let mrn_regex = ([A-Z0-9]{10}) ;

  /*
    OPTIONAL: A pipe-delimited list of variable names that
    should trigger a warning in the ouput of the macro
    detect_phi.
    Not case-sensitive. Do not include spaces between the pipes. ;
  */

  %let locally_forbidden_varnames = consumno|chsid|hrn|identity_id|pat_mrn_id|pat_id|intptlat|intptlon|arealand|areawater ;

** End of file. ;

```
