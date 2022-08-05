%this is the masterfile to do all our empirical applications
clear;
rng(1)
%create a folder "FiguresandTables" under this path to save figures and tables

%picking which application to run
% 1: Risk_difference
% 2: Odds Ratio


application=2;

% whether we want to produce figures, and/or estimates, and/or bias corrections
dofigures=0;
doestimates=1;
docorrections=1;

%simulations draws for p(z,theta) specifications
vcutoff=1.96;

fixcoeff=0; %By default, do not hold any coefficients fixed.

%%
%loading the data
if application == 1
    data=table2array(readtable('../Applications/water/RD_Estimates.csv'));
    Studynames=table2array(readtable('../Applications/water/RD_Labels.csv'))
    name='RD';
end
if application == 2
    data=table2array(readtable('../Applications/water/OR_Estimates.csv'));
    Studynames=table2array(readtable('../Applications/water/OR_Labels.csv'))
    name='OR'
end

if application == 3
    data=table2array(readtable('../Applications/water/rep-or-estimates.csv'));
    Studynames=table2array(readtable('../Applications/water/rep-or-labels.csv'))
    name='OR'
end

X=data(:,1);
sigma=data(:,2);
n=length(X);
cluster_ID=data(:,3);
C=ones(length(X),1);

includeinfigure=logical(ones(n,1));
includeinestimation=logical(ones(n,1));

% Identification approach = 1 => "replication method" (whatever that is)
% Identification approach = 2 => "metastudy method" (idk what this means either)
identificationapproach=2;
GMMapproach=0;
asymmetric_likelihood_spec=1; %Use a normal model for latent distribution of true effects
controls=0;
numerical_integration=0;
spec_test=0;

%Set cutoffs to use in step function: should be given in increasing order;
cutoffs=[-1.96 0 1.96];
%Use a step function symmetric around zero for p(z), but don't enforce
%symmetric distribution of true effects
symmetric=0;
symmetric_p=1;
%starting values for optimization
Psihat0=[0,1,1]; %[thetabar, tau, betap(1)]

%%
%producing figures
if dofigures==1
    if identificationapproach==1
        DescriptiveStats(Z,sigmaZ2, 1,name,symmetric,cluster_ID);
        DescriptiveStats(data(:,1),data(:,2), 2,[name 'Sanitycheck'],symmetric,cluster_ID);
        DescriptiveStatsCombined(Z,data(:,1),data(:,2),name,symmetric);
    else
        DescriptiveStats(X,sigma, 2,name,symmetric,cluster_ID);
    end
end

%%
%estimating the model

if doestimates==1
    EstimatingSelection
end


%%
%producing bias-corrected estimates and confidence sets

if docorrections ==1
    HorizontalBars
end