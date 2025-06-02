%% Main Modeling Script %%
% January, 2023, Maya & Nina
% last update: 30.04.20235

% Steps:
% 1. Simulation -> check algorithm
%   1.1 load traces
%   1.2 parameter recovery

% 2. Data fitting
%   2.1 parameter fit

clear
close all

%% Set dynamic base paths
% The script identifies and sets the base directory paths for various data and scripts.
% Ensure that 'data', 'material', and 'script' folders are present relative to the base path.
filePath = fileparts(matlab.desktop.editor.getActiveFilename); % Get the current script's path
cd(filePath)
cd(fullfile('..', '..', '..')) % Navigate up three directories to the base directory
basePath = pwd; % Store the base path

%% create table with relevant data
options = CBCsetOptions(); % options: no input
rng(11)

%% Simulation
% create TS for each subject
for iid = 1:length(options.simSubjectIDs)
    
    ID=options.simSubjectIDs{iid};
    
    options = CBCsetPaths(options, '', '', '1_prepare', 'D0');
    
    parms = {};
    
    trajNr = randi(16,1,1);
    
    D = CBCcreateD(options, trajNr, ID, parms); % createD: options, trajNr, ID
end

%% simulate for each model
%% simple RW
rng(11)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};

    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    % Parameters for simulation of simple RW and DDM
    ze_t = 0.3+rand*2.7; % non-decision time
    zStart = 0.5; % 0.5 = no bias towards one decision
    alpha1 = rand*1; % learning parameter of updating current pair
    startBelief = 0.5; %start Belief
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    
    parms.prc = [startBelief, alpha1];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};
    
    percModel = 'CBCsimpleRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%% transfer RW
rng(12)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    
    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    % Parameters for simulation of simple RW and DDM
    ze_t = 0.3+rand*2.7; % non-decision time
    zStart = 0.5; % 0.5 = no bias towards one decision
    alpha1 = rand*1; % learning parameter of updating chosen pair
    alpha2 = rand*1; % learning parameter of updating other pair
    startBelief = 0.5; %start Belief
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    
    parms.prc = [startBelief, alpha1, alpha2];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'alpha2', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};
    
    percModel = 'CBCbothPairsRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%% simple RW asymmetric updating
rng(13)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    
    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    % Parameters for simulation of simple RW and DDM
    ze_t = 0.3+rand*2.7; % non-decision time
    zStart = 0.5; % 0.5 = no bias towards one decision
    alpha1 = rand*1; % learning parameter of updating chosen pair after pos FB
    alpha12 = rand*1; % learning parameter of updating chosen pair after neg FB
    startBelief = 0.5; %start Belief
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    
    parms.prc = [startBelief, alpha1, alpha12];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'alpha12', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};
    
    percModel = 'CBCsimpleAsymRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%% transfer RW asymmetric updating
rng(14)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    
    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    % Parameters for simulation of simple RW and DDM
    ze_t = 0.3+rand*2.7; % non-decision time
    zStart = 0.5; % 0.5 = = no bias towards one decision
    alpha1 = rand*1; % learning parameter of updating chosen pair, pos FB
    alpha12 = rand*1; % learning parameter of updating chosen pair, neg FB
    alpha2 = rand*1; % learning parameter of updating other pair, pos FB
    alpha22 = rand*1; % learning parameter of updating other pair, neg FB
    startBelief = 0.5; %start Belief
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    
    parms.prc = [startBelief,alpha1, alpha12, alpha2, alpha22];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'alpha12', 'alpha2', 'alpha22', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};
    
    percModel = 'CBCbothPairsAsymRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ------------ Parameter Recovery -------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simple RW
rng(21)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    
    % ----------- simple RW - drift Diffusion L/R - pw beliefs -----------
    parms.names = {'startBelief', 'alpha1', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 3.0, 15, 5.0, 0.5];
    parms.nprc = 2;
    parms.n = 6;
    
    percModel = 'CBCsimpleRW'; %'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBCbothPairsRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid'
    
    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% transfer RW
rng(23)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    parms.names = {'startBelief', 'alpha1', 'alpha2', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 1.0, 3.0, 15, 5.0, 0.5];
    parms.nprc = 3;
    parms.n = 7;
    
    percModel = 'CBCbothPairsRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% simple RW asymmetric
rng(25)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    
    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    % Parameters for simulation of simple RW updating both pairs and DDM   
    parms.names = {'startBelief', 'alpha1', 'alpha12', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 1.0, 3.0, 15, 5.0, 0.5];
    parms.nprc = 3;
    parms.n = 7;
    
    percModel = 'CBCsimpleAsymRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% transfer RW asymmetric
rng(27)
for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    
    % ----------- Drift Diffusion, L/R, normalised beliefs ----------
    % Parameters for simulation of simple RW updating both pairs and DDM   
    parms.names = {'startBelief', 'alpha1', 'alpha12', 'alpha2', 'alpha22', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.0, 0.0, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 1.0, 1.0, 1.0, 3.0, 15, 5.0, 0.5];
    parms.nprc = 5;
    parms.n = 9;
    
    percModel = 'CBCbothPairsAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ------ Getting serious: Load and fit real datasets -----%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for iid = 1:length(options.subjectIDs)
    
    ID=options.subjectIDs{iid}
    
    options = CBCsetPaths(options, '', '', '1_prepare', 'D0');
    
    parms = {};
    
    D = CBCprepareLogs(options, parms, ID);
end

%% fitting on real data
%% simple RW
rng(31)
for iid = 1:length(options.subjectIDs)
    ID=options.subjectIDs{iid}    
    % ----------- simple RW - drift Diffusion L/R - pw beliefs -----------
    parms.names = {'startBelief', 'alpha1', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1, 3.0, 15, 5.0, 0.5];
    parms.nprc = 2;
    parms.n = 6;
    
    percModel = 'CBCsimpleRW'; %'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBCbothPairsRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% transfer RW 
rng(33)
for iid = 1:length(options.subjectIDs)
    ID=options.subjectIDs{iid};
    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    parms.names = {'startBelief', 'alpha1', 'alpha2', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 1.0, 3.0, 15, 5.0, 0.5];
    parms.nprc = 3;
    parms.n = 7;
    
    percModel = 'CBCbothPairsRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% simple RW asymmetric
rng(35)
for iid = 1:length(options.subjectIDs)
    ID=options.subjectIDs{iid};
    
    % ----------- Drift Diffusion, L/R, pw beliefs ----------
    % Parameters for simulation of simple RW updating both pairs and DDM   
    parms.names = {'startBelief', 'alpha1', 'alpha12', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 1.0, 3.0, 15, 5.0, 0.5];
    parms.nprc = 3;
    parms.n = 7;
    
    percModel = 'CBCsimpleAsymRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% transfer RW asymmetric
rng(37)
for iid = 1:length(options.subjectIDs)
    ID=options.subjectIDs{iid};
    
    % ----------- Drift Diffusion, L/R, normalised beliefs ----------
    % Parameters for simulation of simple RW updating both pairs and DDM   
    parms.names = {'startBelief', 'alpha1', 'alpha12', 'alpha2', 'alpha22', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.0, 0.0, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 1.0, 1.0, 1.0, 3.0, 15, 5.0, 0.5];
    parms.nprc = 5;
    parms.n = 9;
    
    percModel = 'CBCbothPairsAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR_pwBelief'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end


%% recalculate surprise
for iid = 1:length(options.subjectIDs)
    
    ID=options.subjectIDs{iid}
    
    parms = {};
    
    % -----------Simple Surprise----------
    % function can have more than 1 surprise model and calculates in the
    % order given. The variable name of the surprise will be the function
    % name without the CBC in the beginning. Needs to be within {}!!
    statModel = {'CBCsimpleSurprise', 'CBCnonMatchPairSurprise'}; % 'CBCsimpleSurprise', 'CBCnonMatchPairSurprise', 'CBCleftrightSurprise', 'CBCweightedSurprise';
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_', strjoin(statModel, '_')]);
    D = CBCsurprise(options, parms, statModel, ID);
    
end

