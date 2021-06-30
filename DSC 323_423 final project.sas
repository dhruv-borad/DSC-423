proc import datafile = "heart.csv" out = heart replace;
delimiter = ",";
getnames = yes;
run;
proc print;
run;

title "Descriptives";
proc means min p25 p50 p75 max std;
var age trtbps chol thalachh oldpeak;
run;

title "Histogram W/ normal curve";
proc univariate normal;
var age trtbps chol thalachh oldpeak;
histogram / normal (mu = est sigma = est);
run;

title "Boxplots vs IDVs";
proc sort;
by output;
run;
proc boxplot;
plot trtbps*output;
insetgroup min q1 q2 q3 max mean stddev;
plot chol*output;
insetgroup min q1 q2 q3 max mean stddev;
plot thalachh*output;
insetgroup min q1 q2 q3 max mean stddev;
plot oldpeak*output;
insetgroup min q1 q2 q3 max mean stddev;
plot age*output;
insetgroup min q1 q2 q3 max mean stddev;
plot sex*output;
insetgroup min q1 q2 q3 max mean stddev;
plot cp*output;
insetgroup min q1 q2 q3 max mean stddev;
plot fbs*output;
insetgroup min q1 q2 q3 max mean stddev;
plot restecg*output;
insetgroup min q1 q2 q3 max mean stddev;
plot thalachh*output;
insetgroup min q1 q2 q3 max mean stddev;
plot exng*output;
insetgroup min q1 q2 q3 max mean stddev;
plot oldpeak*output;
insetgroup min q1 q2 q3 max mean stddev;
plot caa*output;
insetgroup min q1 q2 q3 max mean stddev;
run;

* scatter plot;
proc sgscatter;
plot (age)*(sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall output)/pbspline;
run;

proc gplot;
plot age*(sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall output);
run;

* correlation table;
proc corr data = heart;
var output sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall age;
run;

proc reg data = heart;
model output =  sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall age/vif;
run;

proc reg data = heart;
model output = thalachh cp caa sex slp exng thall trtbps;
run;

proc logistic data = heart;
model output (event = '1') = sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall age/rsquare stb corrb; 
run;

proc reg data = heart;
model output = sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall age/influence R ;
run;

data heart;
set heart;
if _n_ = 282 then delete;
run;

* removed high p-values variables;
proc logistic data = heart;
model output (event = '1') = sex cp trtbps thalachh exng oldpeak caa thall/rsquare stb corrb; 
run;

* temporary;
* need to check with predicting variable again (age or output);
* check for insignificant variables;
* age dropped due to high p value;
proc reg data = heart;
model output =  sex cp trtbps thalachh exng oldpeak caa thall/vif;
run;

* new dataset;
data heart_new;

set heart;
drop chol fbs restecg slp age;
run;
proc print data=heart_new;
run;

* correlation table;
proc corr data = heart_new;
var output sex cp trtbps thalachh exng oldpeak caa thall;
run;

* check for outiers/influencial points;
proc reg data = heart_new;
model output = sex cp trtbps thalachh exng oldpeak caa thall/influence R ;
run;

* studentized plots;
title "Studentized plots"
proc reg data = heart_new;
model output =  sex cp trtbps thalachh exng oldpeak caa thall;
plot student.*(sex cp trtbps thalachh exng oldpeak caa thall);
plot student.*predicted.;
plot npp.*student.;
run;


* train and test set;
title"split data to train and test set";
proc surveyselect data = heart out = train seed=47274
samprate = 0.75 outall;
run;

proc freq data = train;
tables selected;
run;

* new y for train set;
data train;
set train;
if selected then train_y = output;
run;
proc print data = train;
run;

* model selection;
* stepwise selection method;
proc logistic data = train;
model train_y (event = '1') = sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall age/ selection = stepwise rsquare; 
run;

* forward selection;
proc logistic data = train;
model train_y (event = '1') = sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall age/ selection = forward rsquare; 
run;

* backward selection;
proc logistic data = train;
model train_y (event = '1') = sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall age/ selection = backward rsquare; 
run;

* final model;
title "Fianl model";
proc logistic data = train;
model train_y (event = '1') = thalachh cp caa sex slp exng thall trtbps/ stb rsquare iplots; 
run;

* classification table with prob 0.1 to 0.8 increase by 0.05;
proc logistic data= train;
model train_y (event = '1') = thalachh cp caa sex slp exng thall trtbps/ctable pprob=(0.1 to 0.8 by 0.05);
output out=pred(where= (train_y= .)) p= phat lower=lcl upper=ucl;
run;

* compute predicted y in testing set for pred_prob > 0.65;
data probs;
set pred;
pred_y = 0;
threshold = 0.65;
if phat > threshold then pred_y = 1;
run;

title 'Model 1';
Proc reg data=train;
model train_y = thalachh cp caa sex slp exng thall trtbps;
output out=outm1(where=(train_y=.)) p=yhat;
run;

* classification matrix;
proc freq data = probs;
tables output*pred_y / norow nocol noperccent;
run;

proc corr data=outm1;
var output yhat;
run;

proc sort data = train;
by output;
run;
proc boxplot;
plot age*output;
insetgroup min q1 q2 q3 max mean stddev;
plot cp*output;
insetgroup min q1 q2 q3 max mean stddev;
run;
