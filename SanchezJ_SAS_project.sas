/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 3:31:40 PM
PROJECT: SanchezJ_SAS_project_Jan122015
PROJECT PATH: P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp
---------------------------------------- */

/* Library assignment for Local.QACDATA */
Libname QACDATA BASE 'P:\QAC\qac200\students\jrsanchez' ;
/* Library assignment for Local.QACDATA */
Libname QACDATA BASE 'P:\QAC\qac200\students\jrsanchez' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (QACDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (QACDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME QACDATA BASE "P:\QAC\qac200\students\jrsanchez" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data subset   */
%LET _CLIENTTASKLABEL='Data subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7);

PROC SQL;
   CREATE TABLE QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7(label="ASSING1_MEPS_FULLYR_2012_SAS7") AS 
   SELECT t1.DUPERSID, 
          t1.FAMID12, 
          t1.RUSIZE12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.MSA12, 
          t1.ENDRFM12, 
          t1.INSC1231, 
          t1.ELGRND12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.RFREL12X, 
          t1.HIBPDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADAPPT42, 
          t1.ADRTWW42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.EVRWRK, 
          t1.INS12X, 
          t1.CHDDX, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.MCREV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.PRVDRL12, 
          t1.PMNCNP12, 
          t1.PRDRNP12
      FROM EC100002.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Data Set Attributes1   */
%LET SYSLAST=QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7;
%LET _CLIENTTASKLABEL='Code For Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 07, 2015 at 3:26:41 PM
   By task: Data Set Attributes1

   Input Data: Local:QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForASSING1_MEPS_FULL);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7 ;

RUN;





GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 MEPS Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 MEPS Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:28 PM
   By task: One-Way Frequencies for 2012 MEPS Subset

   Input Data: Local:QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.FAMID12, T.RUSIZE12, T.REGION12, T.RESP12, T.MSA12, T.ENDRFM12, T.INSC1231, T.ELGRND12, T.AGE12X, T.SEX, T.RACETHX, T.MARRY12X, T.EDRECODE, T.RFREL12X, T.HIBPDX, T.ANGIDX, T.MIDX, T.OHRTDX, T.STRKDX, T.EMPHDX, T.CHOLDX
		     , T.CANCERDX, T.DIABDX, T.ARTHDX, T.ASTHDX, T.ADHDADDX, T.SAQELIG, T.ADPRX42, T.ADILCR42, T.ADILWW42, T.ADRTCR42, T.ADAPPT42, T.ADRTWW42, T.ADNDCR42, T.ADEGMC42, T.ADLIST42, T.ADEXPL42, T.ADRESP42, T.ADPRTM42, T.ADINST42, T.ADEZUN42
		     , T.ADTLHW42, T.ADFFRM42, T.ADFHLP42, T.ADHECR42, T.ADSMOK42, T.ADNSMK42, T.ADDRBP42, T.ADSPEC42, T.ADSPRF42, T.ADGENH42, T.ADDAYA42, T.ADCLIM42, T.ADPALS42, T.ADPWLM42, T.ADMALS42, T.ADMWLM42, T.ADPAIN42, T.ADCAPE42
		     , T.ADNRGY42, T.ADDOWN42, T.ADSOCA42, T.PCS42, T.MCS42, T.SFFLAG42, T.ADNERV42, T.ADHOPE42, T.ADREST42, T.ADSAD42, T.ADEFRT42, T.ADWRTH42, T.K6SUM42, T.ADINTR42, T.ADDPRS42, T.PHQ242, T.ADINSA42, T.ADINSB42, T.ADRISK42, T.ADOVER42
		     , T.ADCMPM42, T.ADCMPD42, T.ADCMPY42, T.ADLANG42, T.EVRWRK, T.INS12X, T.CHDDX, T.TRIEV12, T.PRVEV12, T.MCREV12, T.MCDEV12, T.OPAEV12, T.OPBEV12, T.UNINS12, T.INSCOV12, T.PRVDRL12, T.PMNCNP12, T.PRDRNP12
	FROM QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 MEPS Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Justin Sanchez";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES FAMID12 /  SCORES=TABLE;
	TABLES RUSIZE12 /  SCORES=TABLE;
	TABLES REGION12 /  SCORES=TABLE;
	TABLES RESP12 /  SCORES=TABLE;
	TABLES MSA12 /  SCORES=TABLE;
	TABLES ENDRFM12 /  SCORES=TABLE;
	TABLES INSC1231 /  SCORES=TABLE;
	TABLES ELGRND12 /  SCORES=TABLE;
	TABLES AGE12X /  SCORES=TABLE;
	TABLES SEX /  SCORES=TABLE;
	TABLES RACETHX /  SCORES=TABLE;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES EDRECODE /  SCORES=TABLE;
	TABLES RFREL12X /  SCORES=TABLE;
	TABLES HIBPDX /  SCORES=TABLE;
	TABLES ANGIDX /  SCORES=TABLE;
	TABLES MIDX /  SCORES=TABLE;
	TABLES OHRTDX /  SCORES=TABLE;
	TABLES STRKDX /  SCORES=TABLE;
	TABLES EMPHDX /  SCORES=TABLE;
	TABLES CHOLDX /  SCORES=TABLE;
	TABLES CANCERDX /  SCORES=TABLE;
	TABLES DIABDX /  SCORES=TABLE;
	TABLES ARTHDX /  SCORES=TABLE;
	TABLES ASTHDX /  SCORES=TABLE;
	TABLES ADHDADDX /  SCORES=TABLE;
	TABLES SAQELIG /  SCORES=TABLE;
	TABLES ADPRX42 /  SCORES=TABLE;
	TABLES ADILCR42 /  SCORES=TABLE;
	TABLES ADILWW42 /  SCORES=TABLE;
	TABLES ADRTCR42 /  SCORES=TABLE;
	TABLES ADAPPT42 /  SCORES=TABLE;
	TABLES ADRTWW42 /  SCORES=TABLE;
	TABLES ADNDCR42 /  SCORES=TABLE;
	TABLES ADEGMC42 /  SCORES=TABLE;
	TABLES ADLIST42 /  SCORES=TABLE;
	TABLES ADEXPL42 /  SCORES=TABLE;
	TABLES ADRESP42 /  SCORES=TABLE;
	TABLES ADPRTM42 /  SCORES=TABLE;
	TABLES ADINST42 /  SCORES=TABLE;
	TABLES ADEZUN42 /  SCORES=TABLE;
	TABLES ADTLHW42 /  SCORES=TABLE;
	TABLES ADFFRM42 /  SCORES=TABLE;
	TABLES ADFHLP42 /  SCORES=TABLE;
	TABLES ADHECR42 /  SCORES=TABLE;
	TABLES ADSMOK42 /  SCORES=TABLE;
	TABLES ADNSMK42 /  SCORES=TABLE;
	TABLES ADDRBP42 /  SCORES=TABLE;
	TABLES ADSPEC42 /  SCORES=TABLE;
	TABLES ADSPRF42 /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADDAYA42 /  SCORES=TABLE;
	TABLES ADCLIM42 /  SCORES=TABLE;
	TABLES ADPALS42 /  SCORES=TABLE;
	TABLES ADPWLM42 /  SCORES=TABLE;
	TABLES ADMALS42 /  SCORES=TABLE;
	TABLES ADMWLM42 /  SCORES=TABLE;
	TABLES ADPAIN42 /  SCORES=TABLE;
	TABLES ADCAPE42 /  SCORES=TABLE;
	TABLES ADNRGY42 /  SCORES=TABLE;
	TABLES ADDOWN42 /  SCORES=TABLE;
	TABLES ADSOCA42 /  SCORES=TABLE;
	TABLES PCS42 /  SCORES=TABLE;
	TABLES MCS42 /  SCORES=TABLE;
	TABLES SFFLAG42 /  SCORES=TABLE;
	TABLES ADNERV42 /  SCORES=TABLE;
	TABLES ADHOPE42 /  SCORES=TABLE;
	TABLES ADREST42 /  SCORES=TABLE;
	TABLES ADSAD42 /  SCORES=TABLE;
	TABLES ADEFRT42 /  SCORES=TABLE;
	TABLES ADWRTH42 /  SCORES=TABLE;
	TABLES K6SUM42 /  SCORES=TABLE;
	TABLES ADINTR42 /  SCORES=TABLE;
	TABLES ADDPRS42 /  SCORES=TABLE;
	TABLES PHQ242 /  SCORES=TABLE;
	TABLES ADINSA42 /  SCORES=TABLE;
	TABLES ADINSB42 /  SCORES=TABLE;
	TABLES ADRISK42 /  SCORES=TABLE;
	TABLES ADOVER42 /  SCORES=TABLE;
	TABLES ADCMPM42 /  SCORES=TABLE;
	TABLES ADCMPD42 /  SCORES=TABLE;
	TABLES ADCMPY42 /  SCORES=TABLE;
	TABLES ADLANG42 /  SCORES=TABLE;
	TABLES EVRWRK /  SCORES=TABLE;
	TABLES INS12X /  SCORES=TABLE;
	TABLES CHDDX /  SCORES=TABLE;
	TABLES TRIEV12 /  SCORES=TABLE;
	TABLES PRVEV12 /  SCORES=TABLE;
	TABLES MCREV12 /  SCORES=TABLE;
	TABLES MCDEV12 /  SCORES=TABLE;
	TABLES OPAEV12 /  SCORES=TABLE;
	TABLES OPBEV12 /  SCORES=TABLE;
	TABLES UNINS12 /  SCORES=TABLE;
	TABLES INSCOV12 /  SCORES=TABLE;
	TABLES PRVDRL12 /  SCORES=TABLE;
	TABLES PMNCNP12 /  SCORES=TABLE;
	TABLES PRDRNP12 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recoded Variables   */
%LET _CLIENTTASKLABEL='Recoded Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.SUBSET_MEPS_2012_MANAGED);

PROC SQL;
   CREATE TABLE WORK."SUBSET_MEPS_2012_MANAGED"n AS 
   SELECT t1.DUPERSID, 
          t1.FAMID12, 
          t1.RUSIZE12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.MSA12, 
          t1.ENDRFM12, 
          t1.INSC1231, 
          t1.ELGRND12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.RFREL12X, 
          t1.HIBPDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADAPPT42, 
          t1.ADRTWW42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.EVRWRK, 
          t1.INS12X, 
          t1.CHDDX, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.MCREV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.PRVDRL12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          /* GEN_HLTH */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="General health (recoded missing)" AS GEN_HLTH, 
          /* HLTH_LMTS */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="How Health Limits modify activities (recoded missing)" AS HLTH_LMTS, 
          /* HLTH_LMTS_CLIM */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health limits when climbing stairs (recoded missing)" AS HLTH_LMTS_CLIM, 
          /* PHY_PRBS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accomplish less due to physical problems within last 4 weeks (recoded missing)" AS PHY_PRBS, 
          /* PHY_PRB_WRK_LMT */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Limit work due to physical problems within last 4 weeks (recoded missing)" AS PHY_PRB_WRK_LMT, 
          /* MNT_PRBS */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accomplish less due to mental problems within last 4 weeks (recoded missing)" AS MNT_PRBS, 
          /* MNT_PRB_WRK_LMT */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work limitation due to mental problems within last 4 weeks (recoded missing)" AS MNT_PRB_WRK_LMT, 
          /* PAIN_LMTWRK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="How much has pain limited normal work within last 4 weeks (recoded missing)" AS PAIN_LMTWRK, 
          /* FELT_CLM_PCEFUL */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="How often felt calm/peaceful within last 4 weeks (recoded missing)" AS FELT_CLM_PCEFUL, 
          /* LOTS_NRGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had a lot of energy within last 4 weeks (recoded missing) " AS LOTS_NRGY, 
          /* FELT_DWN_DEPR */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt downhearted/depressed within last 4 weeks (recoded missing)" AS FELT_DWN_DEPR, 
          /* HLT_SOC_ACT */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health stopped social activity within last 4 weeks (recoded missing)" AS HLT_SOC_ACT, 
          /* MARRY */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Mariatal Status as of 12/31/12 (recoded missing)" AS MARRY, 
          /* EDUCAT */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education recode (recoded missing)" AS EDUCAT, 
          /* WRK_PAY */
            (CASE 
               WHEN -1 = t1.EVRWRK THEN .
               WHEN -7 = t1.EVRWRK THEN .
               WHEN -8 = t1.EVRWRK THEN .
               WHEN -9 = t1.EVRWRK THEN .
               ELSE t1.EVRWRK
            END) LABEL="Ever work for pay in life as of 12/31/12 (recoded missing)" AS WRK_PAY, 
          /* VST_MED_OFF */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="The number of visits to medical office for care within last 12 months (recoded missing)" AS 
            VST_MED_OFF, 
          /* MED_APPT_WW */
            (CASE 
               WHEN -1 = t1.ADRTWW42 THEN .
               WHEN -9 = t1.ADRTWW42 THEN .
               ELSE t1.ADRTWW42
            END) LABEL="Got medical appointment when wanted winthin last 12 months (recoded missing)" AS MED_APPT_WW, 
          /* ND_CRE_TRT */
            (CASE 
               WHEN -1 = t1.ADNDCR42 THEN .
               WHEN -8 = t1.ADNDCR42 THEN .
               WHEN -9 = t1.ADNDCR42 THEN .
               ELSE t1.ADNDCR42
            END) LABEL="Need any care, test or treatment within last 12 months (recoded missing)" AS ND_CRE_TRT, 
          /* ESY_GET_MED_CRE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy getting needed medical care within last 12 months (recoded missing)" AS ESY_GET_MED_CRE, 
          /* DR_LISTND */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="Doctor listened to you within last 12 months (recoded missing)" AS DR_LISTND, 
          /* DR_EXPL_UNDRSTD */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="Doctor explained so understood within last 12 months (recoded missing)" AS DR_EXPL_UNDRSTD, 
          /* DR_SHW_RSPCT */
            (CASE 
               WHEN -1 = t1.ADRESP42 THEN .
               WHEN -9 = t1.ADRESP42 THEN .
               ELSE t1.ADRESP42
            END) LABEL="Doctor showed respect within last 12 months (recoded missing)" AS DR_SHW_RSPCT, 
          /* DR_SPNT_TME */
            (CASE 
               WHEN -1 = t1.ADPRTM42 THEN .
               WHEN -9 = t1.ADPRTM42 THEN .
               ELSE t1.ADPRTM42
            END) LABEL="Doctor spent time enough time with you within last 12 months (recoded missing)" AS DR_SPNT_TME, 
          /* DR_SPCIFC_INST */
            (CASE 
               WHEN -1 = t1.ADINST42 THEN .
               WHEN -8 = t1.ADINST42 THEN .
               WHEN -9 = t1.ADINST42 THEN .
               ELSE t1.ADINST42
            END) LABEL="Doctor gave specific instructions within last 12 months (recoded missing)" AS DR_SPCIFC_INST, 
          /* DR_INSTR_UNDRSTD */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="Doctor given insturctions easily understood within last 12 months (recoded missing)" AS 
            DR_INSTR_UNDRSTD, 
          /* ASTH_DIAG */
            (CASE 
               WHEN -7 = t1.ASTHDX THEN .
               WHEN -8 = t1.ASTHDX THEN .
               WHEN -9 = t1.ASTHDX THEN .
               ELSE t1.ASTHDX
            END) LABEL="Asthma diagnosis (recoded missing)" AS ASTH_DIAG, 
          /* CNCR_DIAG */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="Cancer diagnosis (recoded missing)" AS CNCR_DIAG, 
          /* STRK_DIAG */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="Stroke diagnosis (recoded missing)" AS STRK_DIAG, 
          /* COV_PRV_PLAN */
            (CASE 
               WHEN -9 = t1.PRVDRL12 THEN .
               ELSE t1.PRVDRL12
            END) LABEL="Covered by private plan with doctor list as of 12/31/12 (recoded missing)" AS COV_PRV_PLAN, 
          /* HRT_ATCK_DIAG */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="Heart attack diagnosis (recoded missing)" AS HRT_ATCK_DIAG, 
          /* HI_BLD_DIAG */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="High blood pressure diagnosis (recoded missing)" AS HI_BLD_DIAG, 
          /* OTH_HRT_DIAG */
            (CASE 
               WHEN -7 = t1.OHRTDX THEN .
               WHEN -8 = t1.OHRTDX THEN .
               WHEN -9 = t1.OHRTDX THEN .
               ELSE t1.OHRTDX
            END) LABEL="Other heart disease diagnosis (recoded missing)" AS OTH_HRT_DIAG, 
          /* HI_CHOL_DIAG */
            (CASE 
               WHEN -7 = t1.CHOLDX THEN .
               WHEN -8 = t1.CHOLDX THEN .
               WHEN -9 = t1.CHOLDX THEN .
               ELSE t1.CHOLDX
            END) LABEL="High cholesterol diagnosis (recoded missing)" AS HI_CHOL_DIAG, 
          /* DIAB_DIAG */
            (CASE 
               WHEN -7 = t1.DIABDX THEN .
               WHEN -8 = t1.DIABDX THEN .
               WHEN -9 = t1.DIABDX THEN .
               ELSE t1.DIABDX
            END) LABEL="Diabetes diagnosis (recoded missing)" AS DIAB_DIAG, 
          /* ARTH_DIAG */
            (CASE 
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="Arthritis diagnosis (recoded missing)" AS ARTH_DIAG
      FROM QACDATA.ASSING1_MEPS_FULLYR_2012_SAS7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:29 PM
   By task: Table Analysis

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGENH42, T.GEN_HLTH
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42 * GEN_HLTH /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:29 PM
   By task: Table Analysis1

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDAYA42, T.HLTH_LMTS
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDAYA42 * HLTH_LMTS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:29 PM
   By task: Table Analysis2

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADCLIM42, T.HLTH_LMTS_CLIM
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCLIM42 * HLTH_LMTS_CLIM /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis3   */
%LET _CLIENTTASKLABEL='Table Analysis3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:30 PM
   By task: Table Analysis3

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADPALS42, T.PHY_PRBS
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPALS42 * PHY_PRBS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis4   */
%LET _CLIENTTASKLABEL='Table Analysis4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:30 PM
   By task: Table Analysis4

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PHY_PRB_WRK_LMT, T.ADPWLM42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPWLM42 * PHY_PRB_WRK_LMT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis5   */
%LET _CLIENTTASKLABEL='Table Analysis5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:30 PM
   By task: Table Analysis5

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MNT_PRBS, T.ADMALS42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMALS42 * MNT_PRBS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis6   */
%LET _CLIENTTASKLABEL='Table Analysis6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:31 PM
   By task: Table Analysis6

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MNT_PRB_WRK_LMT, T.ADMWLM42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMWLM42 * MNT_PRB_WRK_LMT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis7   */
%LET _CLIENTTASKLABEL='Table Analysis7';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:31 PM
   By task: Table Analysis7

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PAIN_LMTWRK, T.ADPAIN42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPAIN42 * PAIN_LMTWRK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis8   */
%LET _CLIENTTASKLABEL='Table Analysis8';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:31 PM
   By task: Table Analysis8

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.FELT_CLM_PCEFUL, T.ADCAPE42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCAPE42 * FELT_CLM_PCEFUL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis9   */
%LET _CLIENTTASKLABEL='Table Analysis9';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:32 PM
   By task: Table Analysis9

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.LOTS_NRGY, T.ADNRGY42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNRGY42 * LOTS_NRGY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis10   */
%LET _CLIENTTASKLABEL='Table Analysis10';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:32 PM
   By task: Table Analysis10

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.FELT_DWN_DEPR, T.ADDOWN42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDOWN42 * FELT_DWN_DEPR /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis11   */
%LET _CLIENTTASKLABEL='Table Analysis11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:32 PM
   By task: Table Analysis11

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HLT_SOC_ACT, T.ADSOCA42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSOCA42 * HLT_SOC_ACT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis12   */
%LET _CLIENTTASKLABEL='Table Analysis12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:32 PM
   By task: Table Analysis12

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X, T.MARRY
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X * MARRY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis13   */
%LET _CLIENTTASKLABEL='Table Analysis13';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:33 PM
   By task: Table Analysis13

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDRECODE, T.EDUCAT
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDRECODE * EDUCAT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis14   */
%LET _CLIENTTASKLABEL='Table Analysis14';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:33 PM
   By task: Table Analysis14

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EVRWRK, T.WRK_PAY
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EVRWRK * WRK_PAY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis15   */
%LET _CLIENTTASKLABEL='Table Analysis15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:33 PM
   By task: Table Analysis15

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADAPPT42, T.VST_MED_OFF
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADAPPT42 * VST_MED_OFF /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis16   */
%LET _CLIENTTASKLABEL='Table Analysis16';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:34 PM
   By task: Table Analysis16

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MED_APPT_WW, T.ADRTWW42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADRTWW42 * MED_APPT_WW /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis17   */
%LET _CLIENTTASKLABEL='Table Analysis17';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:34 PM
   By task: Table Analysis17

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ND_CRE_TRT, T.ADNDCR42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNDCR42 * ND_CRE_TRT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis18   */
%LET _CLIENTTASKLABEL='Table Analysis18';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:35 PM
   By task: Table Analysis18

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ESY_GET_MED_CRE, T.ADEGMC42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEGMC42 * ESY_GET_MED_CRE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis19   */
%LET _CLIENTTASKLABEL='Table Analysis19';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:35 PM
   By task: Table Analysis19

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_LISTND, T.ADLIST42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADLIST42 * DR_LISTND /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis20   */
%LET _CLIENTTASKLABEL='Table Analysis20';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:36 PM
   By task: Table Analysis20

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_EXPL_UNDRSTD, T.ADEXPL42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEXPL42 * DR_EXPL_UNDRSTD /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis21   */
%LET _CLIENTTASKLABEL='Table Analysis21';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:36 PM
   By task: Table Analysis21

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_SHW_RSPCT, T.ADRESP42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADRESP42 * DR_SHW_RSPCT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis22   */
%LET _CLIENTTASKLABEL='Table Analysis22';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:37 PM
   By task: Table Analysis22

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_SPNT_TME, T.ADPRTM42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPRTM42 * DR_SPNT_TME /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis23   */
%LET _CLIENTTASKLABEL='Table Analysis23';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:37 PM
   By task: Table Analysis23

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_SPCIFC_INST, T.ADINST42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADINST42 * DR_SPCIFC_INST /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis24   */
%LET _CLIENTTASKLABEL='Table Analysis24';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:37 PM
   By task: Table Analysis24

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DR_INSTR_UNDRSTD, T.ADEZUN42
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEZUN42 * DR_INSTR_UNDRSTD /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis25   */
%LET _CLIENTTASKLABEL='Table Analysis25';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:38 PM
   By task: Table Analysis25

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ASTHDX, T.ASTH_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ASTHDX * ASTH_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis26   */
%LET _CLIENTTASKLABEL='Table Analysis26';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:38 PM
   By task: Table Analysis26

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CANCERDX, T.CNCR_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES CANCERDX * CNCR_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis27   */
%LET _CLIENTTASKLABEL='Table Analysis27';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:38 PM
   By task: Table Analysis27

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.STRKDX, T.STRK_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES STRKDX * STRK_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis28   */
%LET _CLIENTTASKLABEL='Table Analysis28';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:39 PM
   By task: Table Analysis28

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PRVDRL12, T.COV_PRV_PLAN
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES PRVDRL12 * COV_PRV_PLAN /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis29   */
%LET _CLIENTTASKLABEL='Table Analysis29';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:39 PM
   By task: Table Analysis29

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MIDX, T.HRT_ATCK_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MIDX * HRT_ATCK_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis30   */
%LET _CLIENTTASKLABEL='Table Analysis30';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:39 PM
   By task: Table Analysis30

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HIBPDX, T.HI_BLD_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES HIBPDX * HI_BLD_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis31   */
%LET _CLIENTTASKLABEL='Table Analysis31';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:39 PM
   By task: Table Analysis31

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.OHRTDX, T.OTH_HRT_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES OHRTDX * OTH_HRT_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis32   */
%LET _CLIENTTASKLABEL='Table Analysis32';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:39 PM
   By task: Table Analysis32

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CHOLDX, T.HI_CHOL_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES CHOLDX * HI_CHOL_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis33   */
%LET _CLIENTTASKLABEL='Table Analysis33';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:40 PM
   By task: Table Analysis33

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DIABDX, T.DIAB_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES DIABDX * DIAB_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis34   */
%LET _CLIENTTASKLABEL='Table Analysis34';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:40 PM
   By task: Table Analysis34

   Input Data: Local:WORK.SUBSET_MEPS_2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ARTHDX, T.ARTH_DIAG
	FROM WORK.SUBSET_MEPS_2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ARTHDX * ARTH_DIAG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Coded SF-12 Variables   */
%LET _CLIENTTASKLABEL='Reverse Coded SF-12 Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SF12_REVCODE);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SF12_REVCODE"n AS 
   SELECT t1.DUPERSID, 
          t1.FAMID12, 
          t1.RUSIZE12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.MSA12, 
          t1.ENDRFM12, 
          t1.INSC1231, 
          t1.ELGRND12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.RFREL12X, 
          t1.HIBPDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADAPPT42, 
          t1.ADRTWW42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.EVRWRK, 
          t1.INS12X, 
          t1.CHDDX, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.MCREV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.PRVDRL12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.GEN_HLTH, 
          t1.HLTH_LMTS, 
          t1.HLTH_LMTS_CLIM, 
          t1.PHY_PRBS, 
          t1.PHY_PRB_WRK_LMT, 
          t1.MNT_PRBS, 
          t1.MNT_PRB_WRK_LMT, 
          t1.PAIN_LMTWRK, 
          t1.FELT_CLM_PCEFUL, 
          t1.LOTS_NRGY, 
          t1.FELT_DWN_DEPR, 
          t1.HLT_SOC_ACT, 
          t1.MARRY, 
          t1.EDUCAT, 
          t1.WRK_PAY, 
          t1.VST_MED_OFF, 
          t1.MED_APPT_WW, 
          t1.ND_CRE_TRT, 
          t1.ESY_GET_MED_CRE, 
          t1.DR_LISTND, 
          t1.DR_EXPL_UNDRSTD, 
          t1.DR_SHW_RSPCT, 
          t1.DR_SPNT_TME, 
          t1.DR_SPCIFC_INST, 
          t1.DR_INSTR_UNDRSTD, 
          t1.ASTH_DIAG, 
          t1.CNCR_DIAG, 
          t1.STRK_DIAG, 
          t1.COV_PRV_PLAN, 
          t1.HRT_ATCK_DIAG, 
          t1.HI_BLD_DIAG, 
          t1.OTH_HRT_DIAG, 
          t1.HI_CHOL_DIAG, 
          t1.DIAB_DIAG, 
          t1.ARTH_DIAG, 
          /* GEN_HLTH_R */
            (6- t1.GEN_HLTH) LABEL="SF-12 General health (reverse coded)" AS GEN_HLTH_R, 
          /* PAIN_LMTWRK_R */
            (6- t1.PAIN_LMTWRK) LABEL="SF-12 Pain limits normal work (reverse coded)" AS PAIN_LMTWRK_R, 
          /* FELT_CLM_PCEFUL_R */
            (6- t1.FELT_CLM_PCEFUL) LABEL="SF-12 Felt calm/peaceful (reverse coded)" AS FELT_CLM_PCEFUL_R, 
          /* LOTS_NRGY_R */
            (6- t1.LOTS_NRGY) LABEL="SF-12 Had a lot of energy (reverse coded)" AS LOTS_NRGY_R
      FROM WORK.SUBSET_MEPS_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis35   */
%LET _CLIENTTASKLABEL='Table Analysis35';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:41 PM
   By task: Table Analysis35

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.GEN_HLTH, T.GEN_HLTH_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES GEN_HLTH_R * GEN_HLTH /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis36   */
%LET _CLIENTTASKLABEL='Table Analysis36';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:41 PM
   By task: Table Analysis36

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PAIN_LMTWRK, T.PAIN_LMTWRK_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES PAIN_LMTWRK_R * PAIN_LMTWRK /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis37   */
%LET _CLIENTTASKLABEL='Table Analysis37';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:41 PM
   By task: Table Analysis37

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.FELT_CLM_PCEFUL, T.FELT_CLM_PCEFUL_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES FELT_CLM_PCEFUL_R * FELT_CLM_PCEFUL /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis38   */
%LET _CLIENTTASKLABEL='Table Analysis38';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:42 PM
   By task: Table Analysis38

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.LOTS_NRGY, T.LOTS_NRGY_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES LOTS_NRGY_R * LOTS_NRGY /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:42 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.GEN_HLTH_R, T.HLTH_LMTS, T.HLTH_LMTS_CLIM, T.PHY_PRBS, T.PHY_PRB_WRK_LMT, T.MNT_PRBS, T.MNT_PRB_WRK_LMT, T.PAIN_LMTWRK_R, T.FELT_CLM_PCEFUL_R, T.LOTS_NRGY_R, T.FELT_DWN_DEPR, T.HLT_SOC_ACT
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES GEN_HLTH_R /  SCORES=TABLE;
	TABLES HLTH_LMTS /  SCORES=TABLE;
	TABLES HLTH_LMTS_CLIM /  SCORES=TABLE;
	TABLES PHY_PRBS /  SCORES=TABLE;
	TABLES PHY_PRB_WRK_LMT /  SCORES=TABLE;
	TABLES MNT_PRBS /  SCORES=TABLE;
	TABLES MNT_PRB_WRK_LMT /  SCORES=TABLE;
	TABLES PAIN_LMTWRK_R /  SCORES=TABLE;
	TABLES FELT_CLM_PCEFUL_R /  SCORES=TABLE;
	TABLES LOTS_NRGY_R /  SCORES=TABLE;
	TABLES FELT_DWN_DEPR /  SCORES=TABLE;
	TABLES HLT_SOC_ACT /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SF12_AGG);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SF12_AGG"n AS 
   SELECT t1.DUPERSID, 
          t1.FAMID12, 
          t1.RUSIZE12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.MSA12, 
          t1.ENDRFM12, 
          t1.INSC1231, 
          t1.ELGRND12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.RFREL12X, 
          t1.HIBPDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADAPPT42, 
          t1.ADRTWW42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.EVRWRK, 
          t1.INS12X, 
          t1.CHDDX, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.MCREV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.PRVDRL12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.GEN_HLTH, 
          t1.HLTH_LMTS, 
          t1.HLTH_LMTS_CLIM, 
          t1.PHY_PRBS, 
          t1.PHY_PRB_WRK_LMT, 
          t1.MNT_PRBS, 
          t1.MNT_PRB_WRK_LMT, 
          t1.PAIN_LMTWRK, 
          t1.FELT_CLM_PCEFUL, 
          t1.LOTS_NRGY, 
          t1.FELT_DWN_DEPR, 
          t1.HLT_SOC_ACT, 
          t1.MARRY, 
          t1.EDUCAT, 
          t1.WRK_PAY, 
          t1.VST_MED_OFF, 
          t1.MED_APPT_WW, 
          t1.ND_CRE_TRT, 
          t1.ESY_GET_MED_CRE, 
          t1.DR_LISTND, 
          t1.DR_EXPL_UNDRSTD, 
          t1.DR_SHW_RSPCT, 
          t1.DR_SPNT_TME, 
          t1.DR_SPCIFC_INST, 
          t1.DR_INSTR_UNDRSTD, 
          t1.ASTH_DIAG, 
          t1.CNCR_DIAG, 
          t1.STRK_DIAG, 
          t1.COV_PRV_PLAN, 
          t1.HRT_ATCK_DIAG, 
          t1.HI_BLD_DIAG, 
          t1.OTH_HRT_DIAG, 
          t1.HI_CHOL_DIAG, 
          t1.DIAB_DIAG, 
          t1.ARTH_DIAG, 
          t1.GEN_HLTH_R, 
          t1.PAIN_LMTWRK_R, 
          t1.FELT_CLM_PCEFUL_R, 
          t1.LOTS_NRGY_R, 
          /* Sum_SF12 */
            
            (SUM(t1.LOTS_NRGY_R,t1.FELT_CLM_PCEFUL_R,t1.PAIN_LMTWRK_R,t1.GEN_HLTH_R,t1.HLT_SOC_ACT,t1.FELT_DWN_DEPR,t1.MNT_PRB_WRK_LMT,t1.MNT_PRBS,t1.PHY_PRB_WRK_LMT,t1.PHY_PRBS,t1.HLTH_LMTS_CLIM,t1.HLTH_LMTS)) 
            LABEL="Sum of SF-12 variables" AS Sum_SF12
      FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:42 PM
   By task: List Data

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.GEN_HLTH_R, T.PAIN_LMTWRK_R, T.FELT_CLM_PCEFUL_R, T.LOTS_NRGY_R, T.HLTH_LMTS, T.MNT_PRB_WRK_LMT, T.HLTH_LMTS_CLIM, T.PHY_PRBS, T.PHY_PRB_WRK_LMT, T.MNT_PRBS, T.HLT_SOC_ACT, T.FELT_DWN_DEPR, T.Sum_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_AGG as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregrate Variable Coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR GEN_HLTH_R PAIN_LMTWRK_R FELT_CLM_PCEFUL_R LOTS_NRGY_R HLTH_LMTS MNT_PRB_WRK_LMT HLTH_LMTS_CLIM PHY_PRBS PHY_PRB_WRK_LMT MNT_PRBS HLT_SOC_ACT FELT_DWN_DEPR Sum_SF12;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:42 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_AGG as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_SF12 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Aggregate Overall Mental Health Variable   */
%LET _CLIENTTASKLABEL='Summary Statistics for Aggregate Overall Mental Health Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:43 PM
   By task: Summary Statistics for Aggregate Overall Mental Health Variable

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_AGG(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 MEPS AGG Overall Health Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))  by Justin Sanchez";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_SF12;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Aggregate Overall Health Variable   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate Overall Health Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:43 PM
   By task: Distribution Analysis for Aggregate Overall Health Variable

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_AGG(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Sum_SF12 moderate and vigoruos exercise MEPS 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Justin Sanchez";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_SF12;
	HISTOGRAM   Sum_SF12 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:43 PM
   By task: Distribution Analysis

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12
	FROM WORK.MEPS_FULLYR_2012_SF12_AGG as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Sum_SF12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_SF12;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ AS 
   SELECT t1.DUPERSID, 
          t1.FAMID12, 
          t1.RUSIZE12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.MSA12, 
          t1.ENDRFM12, 
          t1.INSC1231, 
          t1.ELGRND12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.RFREL12X, 
          t1.HIBPDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADAPPT42, 
          t1.ADRTWW42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.EVRWRK, 
          t1.INS12X, 
          t1.CHDDX, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.MCREV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.PRVDRL12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.GEN_HLTH, 
          t1.HLTH_LMTS, 
          t1.HLTH_LMTS_CLIM, 
          t1.PHY_PRBS, 
          t1.PHY_PRB_WRK_LMT, 
          t1.MNT_PRBS, 
          t1.MNT_PRB_WRK_LMT, 
          t1.PAIN_LMTWRK, 
          t1.FELT_CLM_PCEFUL, 
          t1.LOTS_NRGY, 
          t1.FELT_DWN_DEPR, 
          t1.HLT_SOC_ACT, 
          t1.MARRY, 
          t1.EDUCAT, 
          t1.WRK_PAY, 
          t1.VST_MED_OFF, 
          t1.MED_APPT_WW, 
          t1.ND_CRE_TRT, 
          t1.ESY_GET_MED_CRE, 
          t1.DR_LISTND, 
          t1.DR_EXPL_UNDRSTD, 
          t1.DR_SHW_RSPCT, 
          t1.DR_SPNT_TME, 
          t1.DR_SPCIFC_INST, 
          t1.DR_INSTR_UNDRSTD, 
          t1.ASTH_DIAG, 
          t1.CNCR_DIAG, 
          t1.STRK_DIAG, 
          t1.COV_PRV_PLAN, 
          t1.HRT_ATCK_DIAG, 
          t1.HI_BLD_DIAG, 
          t1.OTH_HRT_DIAG, 
          t1.HI_CHOL_DIAG, 
          t1.DIAB_DIAG, 
          t1.ARTH_DIAG, 
          t1.GEN_HLTH_R, 
          t1.PAIN_LMTWRK_R, 
          t1.FELT_CLM_PCEFUL_R, 
          t1.LOTS_NRGY_R, 
          t1.Sum_SF12, 
          /* AGG_categorical */
            (CASE  
               WHEN t1.Sum_SF12 >= 2 and t1.Sum_SF12 <41
               THEN 1
              WHEN t1.Sum_SF12 >= 41 and t1.Sum_SF12 <=48
               THEN 2
            WHEN t1.Sum_SF12 >= 48 and t1.Sum_SF12 <=52
               THEN 3
            WHEN t1.Sum_SF12 >= 52
               THEN 4
            END) LABEL="CDC AGG categories" AS AGG_categorical
      FROM WORK.MEPS_FULLYR_2012_SF12_AGG t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:44 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_SF12, T.AGG_categorical
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_SF12 /  SCORES=TABLE;
	TABLES AGG_categorical /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_NEW_AGG);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_NEW_AGG"n AS 
   SELECT t1.DUPERSID, 
          t1.FAMID12, 
          t1.RUSIZE12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.MSA12, 
          t1.ENDRFM12, 
          t1.INSC1231, 
          t1.ELGRND12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.RFREL12X, 
          t1.HIBPDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADAPPT42, 
          t1.ADRTWW42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.EVRWRK, 
          t1.INS12X, 
          t1.CHDDX, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.MCREV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.PRVDRL12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.GEN_HLTH, 
          t1.HLTH_LMTS, 
          t1.HLTH_LMTS_CLIM, 
          t1.PHY_PRBS, 
          t1.PHY_PRB_WRK_LMT, 
          t1.MNT_PRBS, 
          t1.MNT_PRB_WRK_LMT, 
          t1.PAIN_LMTWRK, 
          t1.FELT_CLM_PCEFUL, 
          t1.LOTS_NRGY, 
          t1.FELT_DWN_DEPR, 
          t1.HLT_SOC_ACT, 
          t1.MARRY, 
          t1.EDUCAT, 
          t1.WRK_PAY, 
          t1.VST_MED_OFF, 
          t1.MED_APPT_WW, 
          t1.ND_CRE_TRT, 
          t1.ESY_GET_MED_CRE, 
          t1.DR_LISTND, 
          t1.DR_EXPL_UNDRSTD, 
          t1.DR_SHW_RSPCT, 
          t1.DR_SPNT_TME, 
          t1.DR_SPCIFC_INST, 
          t1.DR_INSTR_UNDRSTD, 
          t1.ASTH_DIAG, 
          t1.CNCR_DIAG, 
          t1.STRK_DIAG, 
          t1.COV_PRV_PLAN, 
          t1.HRT_ATCK_DIAG, 
          t1.HI_BLD_DIAG, 
          t1.OTH_HRT_DIAG, 
          t1.HI_CHOL_DIAG, 
          t1.DIAB_DIAG, 
          t1.ARTH_DIAG, 
          t1.GEN_HLTH_R, 
          t1.PAIN_LMTWRK_R, 
          t1.FELT_CLM_PCEFUL_R, 
          t1.LOTS_NRGY_R, 
          /* Sum_New */
            
            (SUM(t1.ASTH_DIAG,t1.CNCR_DIAG,t1.STRK_DIAG,t1.HRT_ATCK_DIAG,t1.HI_BLD_DIAG,t1.HI_CHOL_DIAG,t1.DIAB_DIAG,t1.ARTH_DIAG,t1.OTH_HRT_DIAG)) 
            LABEL="Sum of new variables" AS Sum_New
      FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data1   */
%LET _CLIENTTASKLABEL='List Data1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:44 PM
   By task: List Data1

   Input Data: Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ASTH_DIAG, T.CNCR_DIAG, T.STRK_DIAG, T.HRT_ATCK_DIAG, T.HI_BLD_DIAG, T.OTH_HRT_DIAG, T.HI_CHOL_DIAG, T.DIAB_DIAG, T.ARTH_DIAG, T.Sum_New
	FROM WORK.MEPS_FULLYR_2012_NEW_AGG as T
;
QUIT;
TITLE;
TITLE1 "Check New Aggregate Variable Coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ASTH_DIAG CNCR_DIAG STRK_DIAG HRT_ATCK_DIAG HI_BLD_DIAG OTH_HRT_DIAG HI_CHOL_DIAG DIAB_DIAG ARTH_DIAG Sum_New;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:44 PM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_New
	FROM WORK.MEPS_FULLYR_2012_NEW_AGG as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_New /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for New Aggregate Variable   */
%LET _CLIENTTASKLABEL='Summary Statistics for New Aggregate Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:44 PM
   By task: Summary Statistics for New Aggregate Variable

   Input Data: Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_New
	FROM WORK.MEPS_FULLYR_2012_NEW_AGG(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 MEPS FULLYR NEW AGG";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Justin Sanchez";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_New;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for new aggregate variable   */
%LET _CLIENTTASKLABEL='Distribution Analysis for new aggregate variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:45 PM
   By task: Distribution Analysis for new aggregate variable

   Input Data: Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_NEW_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_New
	FROM WORK.MEPS_FULLYR_2012_NEW_AGG(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Sum_New the new aggregate variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Justin Sanchez";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_New;
	HISTOGRAM   Sum_New / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder3   */
%LET _CLIENTTASKLABEL='Query Builder3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A AS 
   SELECT /* NEW_AGG_categorical */
            (CASE  
               WHEN t1.Sum_New >= 1 and t1.Sum_New <14
               THEN 1
            WHEN t1.Sum_New >= 14 and t1.Sum_New <=16
               THEN 2
            WHEN t1.Sum_New >=17  and t1.Sum_New <=18
               THEN 3
            END) LABEL="CDC New Variables categories" AS NEW_AGG_categorical, 
          t1.DUPERSID, 
          t1.FAMID12, 
          t1.RUSIZE12, 
          t1.REGION12, 
          t1.RESP12, 
          t1.MSA12, 
          t1.ENDRFM12, 
          t1.INSC1231, 
          t1.ELGRND12, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.RFREL12X, 
          t1.HIBPDX, 
          t1.ANGIDX, 
          t1.MIDX, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CHOLDX, 
          t1.CANCERDX, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.ADHDADDX, 
          t1.SAQELIG, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADAPPT42, 
          t1.ADRTWW42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADSMOK42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADGENH42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.EVRWRK, 
          t1.INS12X, 
          t1.CHDDX, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.MCREV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.UNINS12, 
          t1.INSCOV12, 
          t1.PRVDRL12, 
          t1.PMNCNP12, 
          t1.PRDRNP12, 
          t1.GEN_HLTH, 
          t1.HLTH_LMTS, 
          t1.HLTH_LMTS_CLIM, 
          t1.PHY_PRBS, 
          t1.PHY_PRB_WRK_LMT, 
          t1.MNT_PRBS, 
          t1.MNT_PRB_WRK_LMT, 
          t1.PAIN_LMTWRK, 
          t1.FELT_CLM_PCEFUL, 
          t1.LOTS_NRGY, 
          t1.FELT_DWN_DEPR, 
          t1.HLT_SOC_ACT, 
          t1.MARRY, 
          t1.EDUCAT, 
          t1.WRK_PAY, 
          t1.VST_MED_OFF, 
          t1.MED_APPT_WW, 
          t1.ND_CRE_TRT, 
          t1.ESY_GET_MED_CRE, 
          t1.DR_LISTND, 
          t1.DR_EXPL_UNDRSTD, 
          t1.DR_SHW_RSPCT, 
          t1.DR_SPNT_TME, 
          t1.DR_SPCIFC_INST, 
          t1.DR_INSTR_UNDRSTD, 
          t1.ASTH_DIAG, 
          t1.CNCR_DIAG, 
          t1.STRK_DIAG, 
          t1.COV_PRV_PLAN, 
          t1.HRT_ATCK_DIAG, 
          t1.HI_BLD_DIAG, 
          t1.OTH_HRT_DIAG, 
          t1.HI_CHOL_DIAG, 
          t1.DIAB_DIAG, 
          t1.ARTH_DIAG, 
          t1.GEN_HLTH_R, 
          t1.PAIN_LMTWRK_R, 
          t1.FELT_CLM_PCEFUL_R, 
          t1.LOTS_NRGY_R, 
          t1.Sum_New, 
          /* MARRY_CATG */
            (CASE  
               WHEN t1.MARRY =1 
               THEN 1
            WHEN t1.MARRY >1 and t1.MARRY <5
               THEN 2
               WHEN t1.MARRY =5 
               THEN 3
            END) LABEL="CDC MARRY categories" AS MARRY_CATG, 
          /* EDUCAT_CATG */
            (CASE  
               WHEN t1.EDUCAT >=0 and t1.EDUCAT <9
               THEN 1
               WHEN t1.EDUCAT >=9 and t1.EDUCAT <=13
               THEN 2
            WHEN t1.EDUCAT >=14 
               THEN 3
            END) LABEL="CDC EDUCAT categories" AS EDUCAT_CATG
      FROM WORK.MEPS_FULLYR_2012_NEW_AGG t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies4   */
%LET _CLIENTTASKLABEL='One-Way Frequencies4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:45 PM
   By task: One-Way Frequencies4

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.NEW_AGG_categorical, T.Sum_New
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES NEW_AGG_categorical /  SCORES=TABLE;
	TABLES Sum_New /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies5   */
%LET _CLIENTTASKLABEL='One-Way Frequencies5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:45 PM
   By task: One-Way Frequencies5

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MARRY_CATG, T.MARRY
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MARRY_CATG /  SCORES=TABLE;
	TABLES MARRY /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies6   */
%LET _CLIENTTASKLABEL='One-Way Frequencies6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:46 PM
   By task: One-Way Frequencies6

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.EDUCAT_CATG, T.EDUCAT
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_NEW_A as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES EDUCAT_CATG /  SCORES=TABLE;
	TABLES EDUCAT /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jrsanchez\SanchezJ_SAS_project_Jan122015.egp';
%LET _CLIENTPROJECTNAME='SanchezJ_SAS_project_Jan122015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 3:28:52 PM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
