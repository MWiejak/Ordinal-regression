/* Research question: What are the factors shaping the negative or neutral attitude towards homsexuals in France?*/
/* Hypothesis: The religiosity of a person stimulates his/her negative or neutral attitude towards homosexuals.*/

libname data '/home/u59653456/data';

/*1. Creating a copy of the original dataset*/

data ess;
 set data.ess9e03_1;
run;

/*2. Selecting only rows where value of the cntry variable is equal to 'FR'. As the country of our interest is France. */

data france;
	set ess;
	if ^missing(cntry) then do; 
		if cntry = 'FR' then output;
 	end;
run;

proc contents data = france;
run;

/*3. Checking distribution of target variable and main explanatory variable*/

proc freq data = france;
 tables freehms /out = freehms_distribution;
 tables rlgdgr / out = rlgdgr_distribution;
 tables freehms*rlgdgr / out = freehms_rlg_distribution;
 run;
 
/*4. Reducing the number of dimensions in freehms and rlgdgr and deleting missing values */

data france_reduced;
 	set france;
 	if (freehms ^= 7 & freehms ^= 8 & freehms ^= 9) & (rlgdgr ^=77 & rlgdgr ^= 88 & rlgdgr ^= 99) then do;
 	
  		if freehms = 3 | freehms = 4 | freehms = 5 then freehms = 3; 
  		if rlgdgr  = 2 |rlgdgr  = 3 |rlgdgr  = 4 then rlgdgr = 2;
  		else if rlgdgr = 5 then rlgdgr = 3;
  		else if rlgdgr = 6 then rlgdgr = 4;
  		else if rlgdgr = 7 then rlgdgr = 5;
  		else if rlgdgr = 8 then rlgdgr = 6;
  		else if rlgdgr = 9 then rlgdgr = 7;
  		else if rlgdgr = 10 then rlgdgr = 8;
  		output;
  		
 	end;
run;

/*5. Checking distribution of new dataset with elimanted missing values, and reduced number of dimensions.*/

proc means data=france_reduced NMISS;
	var FREEHMS RLGDGR;
run;

proc freq data = france_reduced;
 tables freehms*rlgdgr / out = freehms_rlg_reduced_distribution;
run;

proc univariate data = france_reduced;
	var freehms;
	var rlgdgr;
	histogram freehms;
	histogram rlgdgr;
run;

/*6. Discriminatory performace analysis of freehms and rlgdgr*/

ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=WORK.FRANCE_REDUCED;
	vbar rlgdgr / group=freehms groupdisplay=cluster;
	yaxis grid;
run;

ods graphics / reset;


ods noproctitle;

proc freq data=WORK.FRANCE_REDUCED;
	ods select MosaicPlot;
	tables freehms*rlgdgr / plots=mosaicplot out = freehms_rlgdgr_reduced_mosaic; 
run;

/*7. Checking association between FREEHMS and RLGDGR*/

proc corr spearman data = france_reduced;
	var freehms rlgdgr;
run;


/*8. Checking distribution of remaining explanatory variables*/

%macro check_distribution(Var=/*Variable*/);
 title "&Var.";
    proc means data=france_reduced NMISS;
    	var &Var.;
    run;
    
	proc freq data=france_reduced;
	 tables &Var. / out=f01;
	run;
	
	proc sgplot data=france_reduced;
	 vbar &Var.;
	run;
	
	title;
%mend check_distribution;


%check_distribution(Var = GNDR);
%check_distribution(Var = NETUSOFT);
%check_distribution(Var = PPLFAIR);


%check_distribution(Var = AGEA);
proc univariate data=france_reduced;
	var agea;
run;

ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=WORK.FRANCE_REDUCED;
	vbox agea /;
	yaxis grid;
run;

ods graphics / reset;



%check_distribution(Var = EDUYRS);

proc univariate data=france_reduced;
	var EDUYRS;
run;

ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=WORK.FRANCE_REDUCED;
	vbox eduyrs /;
	yaxis grid;
run;

ods graphics /  reset;


/*9. Further reduction of missing data and reduction of categories in pplfair*/

data france_reduced_2;
 	set france_reduced;
 	if (pplfair < 77) & (eduyrs >= 4 & eduyrs <= 22) then do;
 	
  		if pplfair <= 3 then pplfair = 0; 
  		else if pplfair = 4 then pplfair = 1;
  		else if pplfair = 5 then pplfair = 2;
  		else if pplfair = 6 then pplfair = 3;
  		else if pplfair = 7 then pplfair = 4;
  		else if pplfair = 8 then pplfair = 5;
  		else if (pplfair = 10 | pplfair = 9) then pplfair = 6;
  		output;
  		
 	end;
run;

/*10. Checking data summary after reduction*/

proc contents data = france_reduced_2;
run;

/*11. Checking data distribution after reduction*/

%macro check_distribution2(Var=/*Variable*/);
 title "&Var.";
    proc means data=france_reduced_2 NMISS;
    	var &Var.;
    run;
    
	proc freq data=france_reduced_2;
	 tables &Var. / out=f01;
	run;
	
	proc sgplot data=france_reduced_2;
	 vbar &Var.;
	run;
	
	title;
%mend check_distribution2;

%check_distribution2(Var = FREEHMS);
%check_distribution2(Var = RLGDGR);
%check_distribution2(Var = GNDR);
%check_distribution2(Var = NETUSOFT);
%check_distribution2(Var = PPLFAIR);


%check_distribution2(Var = AGEA);
proc univariate data=france_reduced;
	var agea;
run;

ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=WORK.FRANCE_REDUCED;
	vbox agea /;
	yaxis grid;
run;

ods graphics / reset;

%check_distribution2(Var = EDUYRS);
proc univariate data=france_reduced;
	var EDUYRS;
run;

ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=WORK.FRANCE_REDUCED;
	vbox eduyrs /;
	yaxis grid;
run;

ods graphics /  reset;

/*12 collinearity analysis*/

proc corr SPEARMAN data = france_reduced_2 ;
	var RLGDGR GNDR NETUSOFT PPLFAIR AGEA EDUYRS;
run;

proc corr data= france_reduced_2;
	var AGEA EDUYRS;
run;

proc reg data=france_reduced_2 plots=none;
model freehms = AGEA EDUYRS / vif tol collin;
run;
quit;

/*13. Discriminatory performance analysis */

%macro disc_perf(Var=);
	title "&Var.";
	ods graphics / reset width=6.4in height=4.8in imagemap;
	
	proc sgplot data=WORK.FRANCE_REDUCED_2;
		vbar &Var. / group=freehms groupdisplay=cluster;
		yaxis grid;
	run;
	
	ods graphics / reset;
	
	
	ods noproctitle;
	
	proc freq data=WORK.FRANCE_REDUCED_2;
		ods select MosaicPlot;
		tables freehms*&Var. / plots=mosaicplot out = disc_perf_table; 
	run;
%mend disc_perf;

%disc_perf(Var = GNDR);
%disc_perf(Var = PPLFAIR);

proc means data=france_reduced_2;
	var AGEA EDUYRS;
	class FREEHMS;
run;
proc sgpanel data=france_reduced_2;
	panelby FREEHMS / columns=1;
	histogram AGEA;
run;
proc sgpanel data=france_reduced_2;
	panelby FREEHMS / columns=1;
	histogram EDUYRS;
run;

/*14. Building model*/

proc logistic data=france_reduced_2 plots(only) = oddsratio alpha=0.05 ;
	class rlgdgr (param = ref ref = '0') gndr (param = ref ref = '1') pplfair (param = ref ref = '0') ;
	model freehms = RLGDGR GNDR PPLFAIR AGEA EDUYRS / lackfit rsq;
	units AGEA = 5 EDUYRS = 5;
	output out = final_model;
	
run;

/*15. Building model without PPLFAIR variable for comparison*/

proc logistic data=france_reduced_2 plots(only) = oddsratio alpha=0.05 ;
	class rlgdgr (param = ref ref = '0') gndr (param = ref ref = '1');
	model freehms = RLGDGR GNDR AGEA EDUYRS / lackfit rsq;
	units AGEA = 5 EDUYRS = 5;
	output out = final_model;
	
run;