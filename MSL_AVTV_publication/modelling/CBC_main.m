%% Main Modeling Script %%
% January, 2023, Maya & Nina
% last update: 01.09.2023

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
for iid = 1:length(options.simSubjectIDs)                                                                                                                                                                                                                                                                                                                                   ength(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    % ------ simulate data ---------
    
    % ----------- Drift Diffusion, L/R, normalised beliefs ----------
    % Parameters for simulation of simple RW and DDM
    ze_t = 0.3+rand*0.8; % non-decision time
    zStart = 0.5; % 0.5 = in the middle between boundaries
    alpha1 = rand*1; % learning parameter of updating current pair
    startBelief = 0.5; %start Belief
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    
    parms.prc = [startBelief, alpha1];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};
    
    percModel = 'CBCsimpleRW'; % 'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%% Parameter Recovery

for iid = 1:length(options.simSubjectIDs)
    ID=options.simSubjectIDs{iid};
    
    % ----------- simple RW - drift Diffusion L/R - norm beliefs -----------
    parms.names = {'startBelief', 'alpha1', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0.0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1.0, 1.1, 15, 5.0, 0.5];
    parms.nprc = 2;
    parms.n = 6;
    
    percModel = 'CBCsimpleRW'; %'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBCbothPairsRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'
    
    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    options.gaTolerance = 10^(-3);
    options.maxGenerations = 500;
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
for iid = 1:length(options.subjectIDs)
    ID=options.subjectIDs{iid}
    % ----------- simple RW - drift Diffusion L/R - norm beliefs -----------
    parms.names = {'startBelief', 'alpha1', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0.3,  0, 1.0, 0.5];
    parms.UB = [0.5, 1, 1.1, 15, 5.0, 0.5];
    parms.nprc = 2;
    parms.n = 6;
    
    percModel = 'CBCsimpleRW'; %'CBCuniVsimpleRW', 'CBCuniATsimpleRW' 'CBCsimpleRW', 'CBCbothPairsRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end
