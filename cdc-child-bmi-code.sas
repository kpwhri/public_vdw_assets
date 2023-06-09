/*
  pardee.r@ghc.org
  Downloaded 16-jan-2015 from:
  http://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm

  Reformatted to add humane indentation; "run" statements, etc.
  TODO: change %getkidbmipercentiles to use this w/vdw.vitalsigns data.
*/

*************************************************************
* This sas program calculates CDC percentiles and z-scores
* based on the 2000 cdc growth charts
* (http://www.cdc.gov/growthcharts/cdc_charts.htm).
* The reference population is children examined in NCHS studies
* from 1963-65 to 1988-94. Biologically implausible values are flagged.

* Unless you have a very good reason to alter this file (and this is unlikely),
* please do NOT make changes.  This file is meant to be called with a %include
* statement from your SAS program.
**************************************************************;

*******  macros for calculations ******;
  %macro _zscore(var,l, m, s, z, p, f);
    if &var >0 then do;
      if abs(&L) ge 0.01 then &z=((&var / &M)**&L-1)/(&L * &S);
        else if .z < abs(&L) < 0.01 then &z=log(&var / &M)/ &S;
      &p=probnorm(&z)*100;

      sdl=((&M - &M*(1 - 2 * &L * &S)**(1 / &L)) / 2); sdh=((&M * (1 + 2 * &L * &S)**(1 / &L) - &M) / 2);
        if &var lt &M then &f= (&var - &M) / sdl; else &f= (&var - &M) / sdh;
    end;
  %mend _zscore;
  %macro _cuts(var,out,l,u);
    if &L <= &var <= &u then &out=0; else if &var> &u then &out=1; else if .< &var < &L then &out= -1;
  %mend _cuts;
************ End of Macros ************;

data
  _orig
  _mydata
  _old
;
  set mydata;
  _id=_n_;
  if agemos gt 240 then output _old;
  output _orig;
  if 0 <= agemos <24 then do;
    length=height;
    height=.;
  end;
  if 24<= agemos then stature=height;
  lenhei=height;
  if agemos lt 240 then output _mydata;
run ;

data
  _cinage
  _cinlen
  _cinht
;
  set _mydata;

  if agemos ge 0 and agemos lt 0.5 then _agecat=0;
  else _agecat=int(agemos+0.5)-0.5;
  if bmi < 0 & ( weight>0 & lenhei >0 & agemos >=24) then bmi=weight/(lenhei/100)**2;

  output _cinage;

  if length > . then do;
    if length >= 45 then _htcat=int(length+0.5)-0.5;
    if 45 <= length < 45.5 then _htcat=45;
    output _cinlen;
  end;

  if stature > . then do;
    if stature ge 77.5 then _htcat=int(stature+0.5)-0.5;
    else if 77<= stature < 77.5 then _htcat=77;
    output _cinht;
  end;
run ;

***********************************************************************;
*** begin the for-age calcs - note that this calls up the refdir libname;
data crefage;
  set &refdata ;
  where denom='age'; *contains all merged LMS data - use 'denom' variable;
run ;
proc sort;
  by sex _agecat;
run ;

proc sort data=_cinage;
  by sex _agecat;
run ;

data finfage;
  merge
    _cinage (in=a)
    crefage (in=b)
  ;
  by sex _agecat;
  if a;
  ageint = _agemos2-_agemos1;
  dage = agemos- _agemos1;

  array l0  _llg  _mlg  _slg  _lht  _mht  _sht  _lwt  _mwt  _swt _lhc  _mhc  _shc  _lbmi  _mbmi  _sbmi;
  array l1 _llg1 _mlg1 _slg1 _lht1 _mht1 _sht1 _lwt1 _mwt1 _swt1 _lhc1 _mhc1 _shc1 _lbmi1 _mbmi1 _sbmi1;
  array l2 _llg2 _mlg2 _slg2 _lht2 _mht2 _sht2 _lwt2 _mwt2 _swt2 _lhc2 _mhc2 _shc2 _lbmi2 _mbmi2 _sbmi2;

  do over l0; l0= l1 + (dage * (l2 - l1)) / ageint; end;

  if agemos < 24 then _mbmi=.; *theres a valid value for 23.5 months! ;

  %_zscore(length, _llg, _mlg, _slg, lgz, lgpct, _Flenz);
  %_cuts(_flenz, _bivlg, -5, 3);
  %_zscore(height, _lht, _mht, _sht, stz, stpct, _Fstatz);
  %_cuts(_fstatz, _bivst, -5, 3);
  %_zscore(weight, _lwt, _mwt, _swt, waz, wapct, _Fwaz);
  %_cuts(_fwaz, _bivwt, -5, 5);
  %_zscore(headcir, _lhc, _mhc, _shc, headcz, headcpct, _Fheadcz);
  %_cuts(_fheadcz, _bivhc, -5, 5);
  %_zscore(bmi, _lbmi, _mbmi, _sbmi, bmiz, bmipct, _Fbmiz);
  %_cuts(_fbmiz, _bivbmi, -4, 5);

  bmi95 = _mbmi * ((1 + _lbmi*_sbmi*probit(0.95))**(1/_lbmi));
  bmipct95=100 * (bmi/bmi95);  **% of 95th percentile: m * ((1+l*s*z)^(1/l));

  drop _llg1 _mlg1 _slg1 _lht1 _mht1 _sht1 _lwt1 _mwt1 _swt1 _lhc1 _mhc1 _shc1 _lbmi1 _mbmi1 _sbmi1
       _llg2 _mlg2 _slg2 _lht2 _mht2 _sht2 _lwt2 _mwt2 _swt2 _lhc2 _mhc2 _shc2 _lbmi2 _mbmi2 _sbmi2
       _lwht1  _mwht1  _swht1  _lwht2  _mwht2  _swht2  _lwlg1  _mwlg1  _swlg1 _lwlg2  _mwlg2  _swlg2;
run ;

******************************************;
*** begin for-length and for-stature calcs;
proc sort data=_cinlen;
  by sex _htcat;
run ;

data creflg;
  set &refdata  (keep=denom sex _lg1--_swlg2);
  where denom='length';
  _htcat=_lg1;
run ;

proc sort data=creflg;
  by sex _htcat;
run ;

data finflg;
  merge
    _cinlen (in=a)
    creflg
  ;
  by sex _htcat;
  if a & (43 < length <104);
  lenint = _lg2- _lg1;
  dlen=length - _lg1;
  array l  _lwl  _mwl  _swl;
  array l1  _lwlg1  _mwlg1  _swlg1;
  array l2  _lwlg2  _mwlg2  _swlg2;
  do over l; l = l1 + (dlen * (l2 - l1)) / lenint; end;

  %_zscore(weight, _lwl, _mwl, _swl, wlz, wlpct, _Fwlz);
  %_cuts(_Fwlz,_bivwlg, -4, 5);
  keep _id sex _agecat agemos weight _Fwlz _bivwlg wlz wlpct;
run ;

*** begin for-stature calcs;
proc sort data=_cinht;
  by sex _htcat;
run ;

data crefht;
  set &refdata  (keep=denom sex _ht1 _ht2 _lwht1 _lwht2 _mwht1 _mwht2 _swht1 _swht2);
  where denom='height';
  _htcat=_ht1;
run ;

proc sort data=crefht;
  by sex _htcat;
run ;

data finfht;
  merge
    _cinht (in=a)
    crefht
  ;
  by sex _htcat;
  if a & (77 < height <122);
  htint = _ht2- _ht1;
  dht=height - _ht1;
  array l   _lwh     _mwh   _swh;
  array l1  _lwht1  _mwht1  _swht1;
  array l2  _lwht2  _mwht2  _swht2;
  do over l; l = l1 + (dht * (l2 - l1)) / htint; end;
  %_zscore(weight, _lwh, _mwh, _swh, wstz, wstpct, _Fwstz);
  %_cuts(_Fwstz,_bivwst,-4,5);
  keep _id sex _agecat agemos weight _Fwstz _bivwst wstz wstpct;
run ;

*** combine the for-age, for-length, and for-height calcs;
proc sort data=finflg;
  by _id;
run ;

proc sort data=finfht;
  by _id;
run ;
data lenht;
  merge
    finflg
    finfht
  ;
  by _id;
run ;

proc sort data=finfage;
  by _id;
run ;
data _outdata;
  merge
    finfage
    lenht
  ;
  by _id;
  array a stature stz  stpct _bivst _fstatz wstz wstpct _bivwst _fwstz ;
  array b length  lgz  lgpct _bivlg _flenz  wlz  wlpct  _bivwlg _fwlz;
  array c height  haz  hapct _bivht _Fhaz   whz  whpct  _bivwh _Fwhz;
  do over c; c=max(a,b); end; **define height vars as max of stat and len vars;
    /*
  array d waz haz bmiz whz headcz; *recode **VERY** extreme z-scores to .;
    do over d; if abs(d) >10 then d=.; end;
    */

  if .z < weight  < 0.01 then do; waz=.; wapct=.; bmiz=.; bmipct=.; whz=.; whpct=.; end;
  if .z < height  < 0.01 then do; haz=.; hapct=.; bmiz=.; bmipct=.; whz=.; whpct=.; end;
  if .z < headcir < 0.01 then do; headcz=.; headcpct=.; end;

  min= min(of _bivht _bivwt _bivbmi _bivhc _bivwh);
  if min>=0 then _bivlow=0; else if min= -1 then _bivlow=1;
  max= max(of _bivht _bivwt _bivbmi _bivhc _bivwh);
  if max=0 or max= -1 then _bivhigh=0; else if max= 1 then _bivhigh=1;

  keep _Fbmiz _Fhaz _Fheadcz _Flenz _Fstatz _Fwaz _Fwhz _fwstz _Fwlz _bivbmi _bivhc _bivhigh _bivht _bivlg _bivlow _bivst
      _bivwh _bivwlg _bivwst _bivwt agemos bmi bmipct bmipct95 bmiz haz headcir headcpct headcz
      _id lgpct lgz sex stpct stz wapct waz weight whpct whz wlpct wlz hapct wstz wstpct _fstatz;
run ;

data _outdata;
  ** combine with older, excluded people;
  set
    _outdata
    _old
  ;

  label
    waz       = 'weight-for-age Z'
    wapct     = 'weight-for-age percentile'
    bmiz      = 'BMI-for-age Z'
    bmipct    = 'BMI-for-age percentile'
    haz       = 'height-for-age Z'
    hapct     = 'height-for-age percentile'
    whz       = 'weight-for-height Z'
    _fwaz     = 'modified weight-for-height Z'
    _bivwt    = 'BIV weight-for-age'
    headcz    = 'head_circ-for-age Z'
    headcpct  = 'head_circ-for age perc'
    _Fheadcz  = 'modified head_circ Z'
    _bivhc    = 'BIV head_circ'
    _fbmiz    = 'modified BMI-for-age Z'
    _bivbmi   = 'BIV BMI-for-age'
    whpct     = 'weight-for-height percentile'
    bmipct95  = '% of 95th BMI percentile'
    _bivht    = 'BIV height-for-age'
    _fhaz     = 'modified height-for-age Z'
    _bivwh    = 'BIV weight-for-height'
    _fwhz     = 'modified weight-for-height Z'
    _bivlow   = 'any low BIV'
    _bivhigh  = 'any high BIV'
  ;
run ;

proc sort data= _outdata;
  by _id;
run ;
proc sort data=_orig;
  by _id;
run ;

data _cdcdata;
  update
    _outdata
    _orig
  ;
  by _id;  *have variables in _orig dataset overwrite any changes that were made;
  drop _id stz stpct _bivst _fstatz wstz wstpct _fwstz  lgz  lgpct _bivlg _flenz  wlz  wlpct  _fwlz
      lenhei length stature _bivwst _bivwlg;
run ;



