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

%% simulate each model
%% simpl RW
rng(11)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};

    % ----------- simple RW ----------
    % Parameters for simulation of simple RW and DDM
    startBelief = 0.5; %start Belief
    alpha1 = rand*1; % learning parameter of updating current pair
    ze_t = 0.3+rand*2.7; % non-decision time
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    zStart = 0.5; % 0.5 = in the middle between boundaries

    parms.prc = [startBelief, alpha1];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};

    percModel = 'CBCsimpleRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'

    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%% simple RW asymmetric
rng(12)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};

    % ----------- simple asymmetric RW ----------
    % Parameters for simulation of simple RW and DDM
    startBelief = 0.5; %start Belief
    alpha1 = rand*1; % learning parameter of updating current pair pos feedback
    alpha12 = rand*1;  % learning parameter of updating current pair neg feedback
    ze_t = 0.3+rand*2.7; % non-decision time
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    zStart = 0.5; % 0.5 = in the middle between boundaries

    parms.prc = [startBelief, alpha1, alpha12];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'alpha12', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};

    percModel = 'CBCsimpleAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'

    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%% transfer RW
rng(13)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};

    % -------- 2 Step RW --------
    % Parameters for simulation of 2StepRW and simple sigmoid
    startBelief = 0.5; %start Belief
    alpha1 = rand*1; % learning parameter of updating current pair
    alpha2 = rand*1; % learning parameter of all associated pairs with one similar stimulus
    ze_t = 0.3+rand*2.7; % non-decision time
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    zStart = 0.5; % 0.5 = in the middle between boundaries

    parms.prc = [startBelief, alpha1, alpha2];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'alpha2', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};

    percModel = 'CBC2StepRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'

    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%% transfer RW asymmetric
rng(14)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};

    % -------- 2 Step asymmetric RW --------
    % Parameters for simulation of 2StepRW and simple sigmoid
    startBelief = 0.5; %start Belief
    alpha1 = rand*1; % learning parameter of updating current pair after pos FB
    alpha12 = rand*1; % learning parameter of updating current pair after neg FB
    alpha2 = rand*1; % learning parameter of all associated pairs with one similar stimulus (only after pos FB
    ze_t = 0.3+rand*2.7; % non-decision time
    m = rand*15; % weight for drift rate
    a = 1+rand*4; % boudary separation
    zStart = 0.5; % 0.5 = in the middle between boundaries

    parms.prc = [startBelief, alpha1, alpha12, alpha2];
    parms.resp = [ze_t, m, a, zStart];
    parmsNames = {'startBelief', 'alpha1', 'alpha12', 'alpha2', 'nonDecisionTime','weight', 'startingBoundary', 'startingPoint'};

    percModel = 'CBC2StepAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'

    options = CBCsetPaths(options, '1_prepare', 'D0', '2_simulations', ['Dsim_',percModel,'_',respModel]);
    D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ------------ Parameter Recovery -------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simple RW
rng(20)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};
    % ---------- RW ---------
    % Start Parameters for fitting

    parms.names = {'startBelief', 'alpha1', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 2;
    parms.n = 6;

    percModel = 'CBCsimpleRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'

    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% simple RW asymmetric
rng(21)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};
    % ---------- RW Asym ---------
    % Start Parameters for fitting

    parms.names = {'startBelief', 'alpha1', 'alpha12', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 3;
    parms.n = 7;

    percModel = 'CBCsimpleAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'

    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% transfer RW
rng(22)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};
    % ---------- transfer RW ---------
    % Start Parameters for fitting

    parms.names = {'startBelief', 'alpha1', 'alpha2', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 3;
    parms.n = 7;

    percModel = 'CBC2StepRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'

    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end

%% transfer RW asymmetric
rng(23)
for iid = 1:length(options.simSubjectIDs)

    ID=options.simSubjectIDs{iid};
    % ---------- RW Transfer Asym ---------
    % Start Parameters for fitting

    parms.names = {'startBelief', 'alpha1', 'alpha12', 'alpha2', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 1, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 4;
    parms.n = 8;
    
    percModel = 'CBC2StepAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'

    options = CBCsetPaths(options, '2_simulations', ['Dsim_',percModel,'_',respModel], '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Getting serious: Load and fit real datasets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for iid = 1:length(options.subjectIDs)

    ID=options.subjectIDs{iid}

    options = CBCsetPaths(options, '', '', '1_prepare', 'D0');
    
    parms = {};

    D = CBCprepareLogs(options, ID, parms);

end
%% simple RW
rng(30)
for iid = 1:length(options.subjectIDs)

    ID=options.subjectIDs{iid}

    % -----------Simple RW----------
    % Parameters for simulation of simple RW and simple Sigmoid
    parms.names = {'startBelief', 'alpha1', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 2;
    parms.n = 6;

    percModel = 'CBCsimpleRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'

    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);

end
%% simple RW asymmetric
rng(31)
for iid = 1:length(options.subjectIDs)

    ID=options.subjectIDs{iid}

    % ---------- RW Asym ---------
    % Start Parameters for fitting
    parms.names = {'startBelief', 'alpha1', 'alpha12', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 3;
    parms.n = 7;

    percModel = 'CBCsimpleAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid' 'CBCsimpleSigmoid_pwBelief' 'CBCdriftDiffusion_LR' 'CBCdriftDiffusion_LR_pw'

    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);

end
%% transfer RW
rng(32)
for iid = 1:length(options.subjectIDs)

    ID=options.subjectIDs{iid}

    % ---------- 2Step RW ---------
    % Start Parameters for fitting
    parms.names = {'startBelief', 'alpha1', 'alpha2', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 3;
    parms.n = 7;

    percModel = 'CBC2StepRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'

    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_',percModel,'_',respModel]);
    D = CBCfit(options, percModel, respModel, parms, ID);
end
%% transfer RW asymmetric
rng(33)
for iid = 1:length(options.subjectIDs)

    ID=options.subjectIDs{iid}

    % ---------- 2Step Asym ---------
    % Start Parameters for fitting
    parms.names = {'startBelief', 'alpha1', 'alpha12', 'alpha2', 'nonDecisionTime', 'weight', 'startingBoundary', 'startingPoint'};
    parms.LB = [0.5, 0, 0, 0, 0.3,  0, 1, 0.5];
    parms.UB = [0.5, 1, 1, 1, 3.0, 15, 5, 0.5];
    parms.nprc = 4;
    parms.n = 8;
    
    percModel = 'CBC2StepAsymRW'; % 'CBCsimpleRW', 'CBC2StepRW', 'CBC2StepForgettingRW'
    respModel = 'CBCdriftDiffusionLR'; % 'CBCsimpleSigmoid'

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
    statModel = {'CBCsimpleSurprise'}; % 'CBCsimpleSurprise', 'CBCnonMatchPairSurprise', 'CBCleftrightSurprise', 'CBCweightedSurprise';
    
    options = CBCsetPaths(options, '1_prepare', 'D0', '3_fit', ['Dfit_', strjoin(statModel, '_')]);
    D = CBCsurprise(options, parms, statModel, ID);
    
end