# The VDW Standard Variables File: StdVars.sas

## Purpose
Strictly speaking, StdVars.sas defines a set of standard macro variables that VDW programmers can use to refer to the datasets that make up the VDW implementation at a site. More generally, StdVars.sas sets up the VDW environment at a given site. This environment includes:

* dataset variables
* site information variables
* filenames (including especially vdw_macs which you can %include to bring [the VDW standard macros](readme.md) into your session)
* system options

## The Standard Variables
The below table holds a complete and accurate list of the core VDW standard variables available to application programs.

|Category|Variable|Description|Sample Value|
|---|---|---|---|
|Dataset Reference|`&_vdw_tumor`|A two-part identifier for the local Tumor dataset|\_my\_vdw\_lib.my\_tumor\_dset|
| |`&_vdw_enroll`|" for the Enrollment dataset|\_my\_vdw\_lib.enroll2|
| |`&_vdw_demographic`|" for the Demographics dataset|\_my\_vdw\_lib.demos|
| |`&_vdw_language`|" for the Person Languages dataset|\_my\_vdw\_lib.languages|
| |`&_vdw_rx`|" for the Pharmacy dataset|\_my\_vdw\_lib.rx|
| |`&_vdw_everndc`|" for the EverNDC dataset|\_my\_vdw\_lib.everndc|
| |`&_vdw_utilization`|" for the Utilization dataset|\_my\_vdw\_lib.ute|
| |`&_vdw_death`|" for the Death dataset|\_my\_vdw\_lib.death|
| |`&_vdw_cause_of_death`|" for the Cause of Death dataset|\_my\_vdw\_lib.cod|
| |`&_vdw_dx`|" for the Diagnoses dataset|\_my\_vdw\_lib.dx|
| |`&_vdw_px`|" for the Procedures dataset|\_my\_vdw\_lib.px|
| |`&_vdw_provider_specialty`|" for the Provider Specialty dataset|\_my\_vdw\_lib.prov_spec|
| |`&_vdw_vitalsigns`|" for the Vital Signs dataset|\_my\_vdw\_lib.vitals|
| |`&_vdw_census_loc`|" for the Census: Patient Locations dataset|\_my\_vdw\_lib.census_location|
| |`&_vdw_census_demog`|" for the Census: Geo-Demographics dataset|\_my\_vdw\_lib.census_demog|
| |`&_vdw_lab`| " for the Lab Results dataset|\_my\_vdw\_lib.lab_results|
| |`&_vdw_lab_notes`|" for the Lab Result Notes dataset|\_my\_vdw\_lib.lab_long_char|
| |`&_vdw_social_hx`|" for the Socialy History dataset|\_my\_vdw\_lib.SocialHx|
|Site Identification|`&_SiteName`|Full name of the site|St Louis University/AHEAD Institute|
| |`&_SiteAbbr`|Initials of the site|SLU|
| |`&_SiteCode`|An arbitrary two-digit numeric code identifying each site (are we maintaining these? they've sort of fallen out of favor).|01|
|Utility|`&lowest_count`|Many sites /projects have something like blanket IRB approval for generating/sending frequency data, so long as cells with "low" counts are masked. This variable holds what is considered "low" at the site. The most commonly used value is 5. You can use this in your code to mask or redact counts that are higher than this number.|5|
| |`&mrn_regex`| A regular expression that matches the local MRN format. Used by the macro %detect\_phi to work out whether a given value could possibly contain an MRN value.|d{8}|
| |`&locally_forbidden_varnames`|A pipe-delimited list of variable names that should trigger a warning in the ouput of the macro `%detect_phi`. Generally a list of the various synonyms for MRN and/or other variable names commonly found at the site that contain values that should not (lightly) travel across sites. Not case sensitive.|consumno\|chsid\|pat\_id

## Version-Specific Variables
From time to time we may define additional variables to refer to prior or future-versioned data formats (e.g., a `_vdw_demog_v2` var to refer to version-2 formatted demographic data). For details on these, see the current VDW implementation overview page , and any Roadmap pages linked from that page. The model StdVars.sas file should also have a complete list of the variables currently considered as current.

## No Standard SAS Libraries
Note that sites are free to store their VDW data in a variety of places--they can keep them all as flat SAS datasets in a single directory, or can put each file in its own directory, or they can put some or all of their VDW data in tables on a server database. Wherever the data ultimately lie, sites define whatever libraries are necessary to populate the dataset reference variables, but there is no standard library that users should count on existing. VDW users should not assume that any particular library is defined in their code unless they define it themselves.

## For Site Data Managers
One other crucial bit of environment for VDW users is access to the VDW standard macros. These are acheived by way of the FILENAME vdw_macs blah blah blah statement in StdVars.sas. Depending on your local firewall and other policies, you may have a choice for how your local users get at these macros (and, in cases where macros load data (e.g., %vdw_formats() how they get at that data)). Those choices are:

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
