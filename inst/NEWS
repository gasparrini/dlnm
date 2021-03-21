-----------------------------------

### Changes in dlnm 2.4.4 (21 Mar 2021)

#### Minor changes

  * Fixed issues with hyperlinks.

-----------------------------------

### Changes in dlnm 2.4.3 (19 Mar 2021)

#### Minor changes

  * Remove warning about old knot default placement in 'arglag' argument of
    crossbasis().

-----------------------------------

### Changes in dlnm 2.4.2 (22 May 2020)

#### Minor changes

  * smooth.construct.cb.smooth.spec() and Predict.matrix.cb.smooth() now defined
    as method functions and registered as such in namespace.
    
  * Changes in crosspred() to fix issues with penalized version 
    
  * Documentation amended accordingly.

-----------------------------------

### Changes in dlnm 2.4.1 (13 May 2020)

#### Minor changes

  * Fixes for CRAN checks.

-----------------------------------

### Changes in dlnm 2.4.0 (01 May 2020)

#### Minor changes

  * Allowed incremental cumulative predictions for any lag interval.
  
  * Small fixes in exphist(), getcoef(), and getvcov().

  * Documentation amended.

-----------------------------------

### Changes in dlnm 2.3.9 (11 Mar 2019)

#### Minor changes

  * Fixed bug in exphist() with negative lags.
  
  * Added colnames in object returned by exphist().

  * Documentation amended.

-----------------------------------

### Changes in dlnm 2.3.8 (04 Feb 2019)

#### Minor changes

  * Fixed bug in if/stop statements with && and !!.

-----------------------------------

### Changes in dlnm 2.3.7 (01 Feb 2019)

#### Minor changes

  * Changes in the function exphist() to allow negative times and lags. This has required a complete recoding of the internal steps which has also made the code simpler and the function faster.

  * Minor correction to documentation.

-----------------------------------

### Changes in dlnm 2.3.6 (12 Aug 2018)

#### Minor changes

  * Some changes in crosspred(), where now model.link can be set directly independently from the model class, making the use of the function more flexible and general. This is implemented with a minor change in getlink(). In addition, the inclusion of the coxme objects (see below) has been removed.

  * Minor correction to documentation.

-----------------------------------

### Changes in dlnm 2.3.5 (02 Aug 2018)

#### Minor changes

  * Fixed a bug in getlink(), now with correct link extraction for coxph objects. In addition, added coxme objects. Thanks to Chau-Ren Jung.

  * Minor correction to documentation.

-----------------------------------

### Changes in dlnm 2.3.4 (05 Dec 2017)

#### Minor changes

  * Replaced spline.des() with splineDesign() in ps(), and changed namespace.
  
  * Fixed a bug in crosspred() and crossreduce() when extracting vcov without accounting for the fact that missing parameters were excluded.
  
  * Minor correction to documentation.

-----------------------------------

### Changes in dlnm 2.3.2 (16 Jan 2017)

#### Major changes

  * Extension to penalized DLMs and DLNMs implemented with the inclusion of new functions and modification of existing ones.
  
  * Revision of the package vignettes, with improved overview of the various developments, and addition of the new vignette dlnmPenalized.
  
  * The package has now a development webpage in GitHub (https://github.com/gasparrini/dlnm).
  
  * Addition of a NEWS file that replaces the changeLog, also with HTML version included in the new GitHub page.
  
  * Extension and improvements of Examples section of help files, in particular for main functions.
  
  * Argument 'fill=0' added to exphist() to allow exposure histories with undefined values to be padded with different values that 0 (the default), in particuar NA.
  
  * Former internal function exphistint() now included within exphist().
  
  * mkcen() now centers at only approximately the mid-value, and return a message with info on the centering value if this is not defined.
  
#### Minor changes

  * Improved Description file, with links to GitHub page.
  
  * Fixed links, in particular to article in www.ag-myresearch.com.

-----------------------------------

### Changes in dlnm 2.2.6 (11 Apr 2016)

#### Minor changes

  * Bug fixed in crossreduce when ncol of cb is 1.

-----------------------------------

### Changes in dlnm 2.2.5 (11 Apr 2016)

#### Major changes

  * The order of transformation in the tensor for cross-basis transformation is changed and now consistent with tensor.prod.model.matrix() in mgcv. This change affects crossbasis(), mkXpred() and crossreduce(), where the order between var-lag has been reversed. The development only concerns internal code and no change should be seen in the usage of dlnm.

#### Minor changes

  * Revised centering procedure to fix minor issues with centering selection with function 'strata', 'thr', 'integer'. Fixed in checkonebasis() and mkcen().

-----------------------------------

### Changes in dlnm 2.2.3 (15 Mar 2016)

#### Major changes

  * Centering procedure moved from onebasis() to crosspred()-crossreduce(). This change simplifies the steps of obtaining predictions with different references, as it avoids refitting the model. At the moment the inclusion  of 'ce' in onebasis()-crossbasis() generates a warning, but the info is kept internally and the old code should still work.

  * Consistently withe the change above, crosspred()-crossreduce() now have a 'cen' argument to specify the reference value.

  * Major changes in crosspred()-crossreduce(), with several auxiliary functions build to simplify the internal code. Specifically: mkat(), mkcen(), mklag(), mkXpred(). See the related help page.

  * New import of the function tensor.prod.model.matrix() from mgcv, to simplify the computation of the tensor product in crosspred().

  * Published code has been updated accordingly, and included in the related web pages at http://www.ag-myresearch.com/. The function attrdl() (not included in the package but used in published code) has been changed as
well.

#### Minor changes

  * Changed default for colour (now black) and frame (now bty=”l”) in plotting functions.
  
  * Namespace changed accordingly, with imports from mgcv.

  * Lag2() removed, back to Lag() imported from package tsModel.

  * Documentation changed accordingly, references updated.

-----------------------------------

### Changes in dlnm 2.1.4 (15 Jan 2015)

#### Minor changes

  * Changes in function strata(), now with possibility of setting 'ref=0', thus obtaining a different dummy parameterization.

  * Changes in summary.crossbasis() to print only a subset of long arguments

-----------------------------------

### Changes in dlnm 2.1.4 (14 Aug 2014)

#### Minor changes

  * Bug fixed in plot()-points()-lines() methods when model.link was not provided.

-----------------------------------

### Changes in dlnm 2.1.3 (05 Aug 2014)

#### Minor changes

  * Bug fixed in function integer() when int=FALSE for lag-response.

-----------------------------------

### Changes in dlnm 2.1.2 (24 Jul 2014)

#### Minor changes

  * Negative lag can now be specified in crossbasis(), although presumably rarely needed.

-----------------------------------

### Changes in dlnm 2.1.0 (21 May 2014)

#### Minor changes

  * Added attribute 'df' in crossbasis objects, with df for each dimension.

  * Adjustment to the example on seasonal analysis.

  * References corrected.

-----------------------------------

### Changes in dlnm 2.0.9 (01 Apr 2014)

#### Major changes

  * Function strata() now accepts an additional argument 'ref' to select the reference category. Also, the intercept is now defined as a vector of 1's.

  * Old functions crossplot(), mkbasis() and mklagbasis() removed.

#### Minor changes

  * Functions now defined with unquoted names (problem with RStudio).

  * Internal functions now defined with names without initial dots.

  * checkgroup modified: now number of obs in each groups can also be less than df in argvar. Required in previous versions, not anymore.

  * Documentation changed accordingly, references updated.

-----------------------------------

### Changes in dlnm 2.0.6 (17 Oct 2013)

#### Minor changes

  * Included function equalknots() to place knots at equally-spaced values.

  * Changes in .get-() functions to allow changes in lme4 objects.

  * Error fixed in examples of vignette dlnmExtended.

-----------------------------------

### Changes in dlnm 2.0.3 (10 Sept 2013)

See also: file.show(system.file("Changesince200",package="dlnm"))

#### Major changes

  * The usage of functions crossbasis() and onebasis() has been extensively revised, as detailed below. The old usage is maintained with a warning for compatibility reasons. However, the old usage can return now different  results if compared to previous versions. See the file Changesince200 using the link above.

  * The functions in dlnm can now be used also for data other than time series. The only difference is the inclusion of a matrix of lagged occurrences as the 'x' argument of crossbasis, instead than the usual vector of time series data. The extension is detailed in the article Gasparrini Stat Med 2013 and in the new vignette 'dlnmExtended'.

  * The functions onebasis() now calls existing functions for generating the basis matrix. The functions poly(), strata(), thr(), integer() and lin() have been added, although not exported in the namespace. User-defined functions with attributes reporting the arguments exactly defining the transformation can be added as well.

  * Given the change above, the arguments of onebasis() and the list of arguments composing the arguments 'argvar' and 'arglag' in crossbasis() are now those of the called functions. In particular, the old usage is not guaranteed to work any more, and if it does a warning is reported.

  * The default knots placements for ns and bs functions for the lag dimension in crossbasis() is not at equally-spaced log-values as before, but at equally-spaced percentiles, consistently with the predictor space. The old default can be reproduced with the new function logknots().

  * Two new data sets included in the package, 'nested' and 'drug'. They are composed of simulated data on nested case-control and randomized controlled trial designs respectively.

  * The new function exphist() can be used to build matrices of exposure histories to be used in crosspred() to obtain specific predictions given an exposure profile.

  * The documentation of the package has beed extensively revised. The vignette 'dlnmOverview now provides a general introduction to the methodology and functions, while the two new vignettes 'dlnmTS' and 'dlnmExtended' describes specific applications in time series design and in other study designs, respectively.

#### Minor changes

  * Basis matrices now keep names from original vector/matrix.

  * Argument 'lag' in crossbasis() now given default value depending on the format of the argument 'x'.

  * Summary method functions have been changed accordingly.

  * Internal functions .getcoef(), .getvcov() and .getlink() added within crosspred(0 tro extract coef, vcov and model link.

  * Internal functions .checkgroup(), .oldonebasis and .oldcrossbasis() for checks in onebasis() and crossbasis().

  * Print of summary.crossreduce() fixed.

-----------------------------------

### Changes in dlnm 1.6.8 (16 May 2013)

#### Minor changes

  * Resolved the dependency with package NMMAPSlite, now archived.

  * Bug fixed in crosspred() when specifying the 'lag' argument (thanks to Joseph Ogutu).

-----------------------------------

### Changes in dlnm 1.6.6 (05 Apr 2013)

#### Minor changes

  * Import from tsModel removed. An internal function .Lag has been created, using the code from the same function in tsModel written by Roger Peng.

-----------------------------------

### Changes in dlnm 1.6.5 (20 Jan 2013)

#### Minor changes

  * URL link added in Description file. Copyright statement added in each function.

  * Updated citations.

  * Documentation changed accordingly.

-----------------------------------

### Changes in dlnm 1.6.4 (22 Aug 2012)

#### Minor changes

  * Bug fixed in Description file.

-----------------------------------

### Changes in dlnm 1.6.3 (19 June 2012)

#### Major changes

  * Argument 'lag' included in crosspred() and crossreduce(), to define sub-periods of lag on which predict. Useful for computing overall cumulative effects on different portions of the lag period.

  * Second example in vignette about seasonal analysis has been corrected. Now the model includes also a term for trend, forgotten so far.

  * Terminology in documentation largely revised.

#### Minor changes

  * In onebasis(), now strata defined by 'bound', not range, and reported.

  * Bug fixed in names assignment to objects in crossreduce().

  * Bug fixed in crosspred() for argument 'bylag'.

-----------------------------------

### Changes in dlnm 1.6.2 (17 Apr 2012)

#### Major changes

  * Bug fixed in onebasis() regarding the argument 'cen'. Now cen=0 and  cen=FALSE do not return the same value, and in the former the centering is acknowledged by summary(). Thanks to Yasushi Honda.

#### Minor changes

  * Documentation changed accordingly.

-----------------------------------

### Changes in dlnm 1.6.1 (13 Apr 2012)

#### Major changes

  * Argument 'bylag' included in crosspred() and crossreduce() to define the sequence of lag values used for prediction. It does not apply to cumulative effects.

#### Minor changes

  * Documentation changed accordingly.

-----------------------------------

### Changes in dlnm 1.6.0 (26 Mar 2012)

#### Major changes

  * crossbasis() and crosspred() now accept as 'x' argument also a matrix of exposure histories. The package now works also with non-time series data, for example cohort or case-control data. The documentation will be changed accordingly as soon as the methodological paper will be published.

#### Minor changes

  * onebasis() now accepts vectors/matrices for less than df+int observations. crosspred() and crossreduce() changed accordingly.

-----------------------------------

### Changes in dlnm 1.5.3 (21 Feb 2012)

#### Minor changes

  * Added as.vector(x) to constraint the type of first argument in onebasis().

  * Change in colnames(crossbasis).

  * Fixed bug in lines.crosspred() and points.crosspred()

-----------------------------------

### Changes in dlnm 1.5.2 (05 Jan 2012)

See also: file.show(system.file("Changesince151",package="dlnm"))

#### Major changes

  * New function onebasis() to create the 1-dimensional basis matrices. The function is made available and documented. This replace mkbasis() and mklagbasis(), which are kept only for compatibility. The functions returns a matrix with attributes.

  * The usage of crossbasis() has been extensively revised. Now the function has 2 arguments var and lag with a list of arguments for each space. Also, maxlag has been replaced by lag, defining the lag range (min and max). Old usage has been kept (with a warning) for compatibility reasons. 

  * New function crossreduce() to reduce the fit to one of the 2 dimensions. Methods functions (summary, plot, points, lines) are also included.

  *	crosspred() changed accordingly: now prediction also from simple basis, and computation based on faster new method in crossreduce().

  * Argument cenvalue eliminated from basis()-crossbasis(): centering now entirily determined by cen, which can be logical or numeric.
	
#### Minor changes

  * Bug fixed in onebasis() (previously mkbasis()) for type hthr-lthr.

  * Internal functions included: .onAttach() and .fci().

  * coef and vcov methods for crosspred() and crossreduce().

  *	In onebasis()-crossbasis(): bound and cen argument now with no default. degree=3 returned now for type='ns', similarly to the original function.

  * In plot functions: changed default of ptype, added default titles.

  * In plots and documentation: effects changed to outcome-association.

  * Documentation and examples changed accondingly with changes above.

-----------------------------------

### Changes in dlnm 1.4.2

#### Minor changes

  * Partial matching fixed in mkbasis() and plot.crosspred().

-----------------------------------

### Changes in dlnm 1.4.1

#### Minor changes

  *	Reference added in vignette, citation and help pages.

-----------------------------------

### Changes in dlnm 1.4.0

#### Major changes

  *	Argument 'ci.arg' added to plotting functions. Now it is possible to (almost) completely specify the confidence intervals display.

  *	onAttach function added, to display a message when the package is attached.

  *	Dependencies revised: splines excluded from Depends, and accordingly library(splines) included in examples and vignette. Also excluded graphics and stats, already imported.
	
#### Minor changes

  *	Documentation and examples changed accondingly.

-----------------------------------

### Changes in dlnm 1.3.2

#### Minor changes

  *	Argument 'group' of crossbasis() forced to define consecutive series. Documentation changed accordingly.

-----------------------------------

### Changes in dlnm 1.3.1

#### Major changes

  *	Function crossplot() replaced by method functions plot(), lines() and points() for class ?crosspred?. The user can now flexibly choose all the arguments through '...' to modify axes, colours, labels etc.

  *	Arguments 'coef', 'vcov' and 'model.link' added to crosspred(), to provide the user a way to manually include the parameters for models without methods for coef() and vcov().

  *	Improved routine for automatic selection of rounded values used for prediction in crosspred() with 'at'-'from'-'to'-'by'.
	
#### Minor changes

  *	Included 'ci.level' in crosspred() and plot() method and 'ci="n"' in plot() method for no confidence intervals.

  *	Improved model parameters extraction method in crosspred() through regular expressions.

  *	Added class "matrix" to crossbasis objects.

  *	Included summary.crosspred().

  *	Added lazyData to description file.

  *	Improved coherence checks in functions and centering in mkbasis().

  *	A detailed description of the functions added in the vignette.

  *	Help pages, vignette and citation changes accordingly.

-----------------------------------

### Changes in dlnm 1.2.4

#### Major changes

  *	mkbasis substantially revised: now 'int' unrelated to df, then relation 'type'-'df'-'int' more logical.

-----------------------------------

### Changes in dlnm 1.2.3

#### Minor changes

  *	Updated reference.

-----------------------------------

### Changes in dlnm 1.2.2

#### Minor changes

  *	Bug in crosspred() for vcov-coef: fixed.

-----------------------------------

### Changes in dlnm 1.2.1

#### Minor changes

  *	Bug in crosspred() with cumul=T: fixed.

  *	Error in regression commands for crosspred(): fixed.

  *	Help pages and citation changed accordingly.

-----------------------------------

### Changes in dlnm 1.2.0

#### Major changes

  *	New argument 'group' in crossbasis() to generate functions for each sub-group defined by the factor.

  *	crosspred() now works also with lm, gam (mgcv), gee (geepack), clogit and coxph (survival).

  *	change in mkbasis: now knots also outside range, but for types "strata" and "-thr" there's the chance to generate collinear variables: included a warning in help for crossbasis().

  *	Vignette 'dlnmOverview" rearranged with new examples, one of them on seasonal analysis. Reference to new paper Gasparrini 2010 Stat Med.

#### Minor changes

  *	Help pages and citation changed accordingly.

-----------------------------------

### Changes in dlnm 1.1.1

#### Minor changes

  *	Bug in mkbasis() with 'type="strata"': fixed.

-----------------------------------

### Changes in dlnm 1.1.0

#### Major changes

  *	The estimate of the cumulative effects along lags has been included in crosspred() and can be plotted with crossplot() setting 'cumul=T'.

  *	The confidence intervals for 'type' equal to "slices" or "overall" can be now represented both by "area" (default), "bars" or "lines", by the argument 'ci' in crossplot().

#### Minor changes

  *	Help pages, examples and vignette changed accordingly.

-----------------------------------

### Changes in dlnm 1.0.2

#### Minor changes

  *	Some html links giving warnings have been fixed.

-----------------------------------

### Changes in dlnm 1.0.1

#### Major changes

  *	The dataset chicagoNMMAPS has been added, relaxing the dependence on the package NMMAPSlite. Now the examples can be run using these data and without an internet connection.

  *	The function crosspred() has been improved: now there is no constraint on the range of the predicted values. Moreover, the values 'model.class' and 'model.link' have been added. The command now works also when the estimation is carried out by 'clogit()'.

#### Minor changes

  *	The section 'Functions' in the vignette 'dlnmOverview' has been reduced: a reference to the functions help pages has been added.

  *	Help pages, examples and vignette changed accordingly.

-----------------------------------

### Changes in dlnm 0.4.1

#### Major changes

  *	The argument 'degree' included in mkbasis()-crossbasis(), defining the degree of the unconstrained B-spline ('type' equal  to 'bs') or the maximum power of the polynomial ('type' equal to 'poly').

  *	Linear piecewise function above a threshold included for type='hthr'/'lthr'; added an example in help pages and vignette.

#### Improvements

  *	Automatic knot selection also for thr-type functions

  *	New example related to the changes above in the help pages and in the vignette

  *	Improved error messages in mkbasis() and mklagbasis()

#### Fixed bugs

  *	Choices and error messages in mkbasis() and mklagbasis()

#### Minor changes

  *	Double threshold type changed from 'thr' to 'dthr'; added an error message in mkbasis() to explain the change.

  *	For type='dthr', knots choice when more than 2 changed from first two to first and last ones. 

  *	Basis choices changed in example3 in order to show a quadratic spline.

  *	Examples for mkbasis()-mklagbasis() expanded in help pages and vignette.

  *	Help pages, examples and vignette changed accordingly.

-----------------------------------

### Changes in dlnm 0.3.0

#### Major changes

  *	A contour/level plot has been added to crossplot().

  *	The option 'bs' has been added to the argument 'type' of mkbasis()-crossbasis(), to specify a cubic spline without natural constraints.

#### Improvements

  *	Argument 'ylim' added for 'type="3d"'.

#### Minor changes

  *	Minor bugs fixed in mkbasis() and mklagbasis().

  *	Typing errors fixed in vignette 'dlnmOverview'.

  *	Help pages, examples and vignette changed accordingly.

#### Corrections

  *	Original series of pollutants in NNMAPS data given by tmean and mtrend, not mean and trend. Corrected

-----------------------------------

### Changes in dlnm 0.2.1

#### Fixed bugs

  *	par options in crossplot()

  *	titles for 'slices' in crossplot()

-----------------------------------

### Changes in dlnm 0.2.0

#### Major changes

  *	The package vignette 'dlnmOverview' has been added.

#### Improvements

  *	Intercept added to type equal to 'thr', 'hthr', 'lhr', and 'lin'

  *	The vector 'predvar' of predictor values for which the prediction must be performed, included in crosspred(), is now automatically ordered and made unique.

#### Fixed bugs

  *	Two links to mkbasis() and mklagbasis() added to the help of crossbasis() in order to let the internal functions help page
    	available

  *	par(par.old) included in 'type="slices"' for crossplot() in order to redifine a single plot after the command

  *	Error messages added to mkbasis() if argument types are wrong

#### Minor changes

  *	Default value for argument 'lagbound' of crossbasis() changed from	c(-1,maxlag) to c(0,maxlag).

  *	Help pages changed accordingly with the new features.

  *	The example 2 (in crosspred()) the threshold has been set to 40.3, in order to show the ordering functionality.

-----------------------------------

### First version on R CRAN: dlnm 0.1.0

-----------------------------------
