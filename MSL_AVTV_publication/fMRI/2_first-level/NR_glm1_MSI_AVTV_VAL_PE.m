% script for 1st level analysis of MSI Task
% Stimulus and FB processing
% Surprise (SPE) and Prediction Error (PRE)
% CBC
% Nina Raduner, July 2023;

clear matlabbatch; % Clear any existing batch processing variables
close all; % Close any open figures
clear; % Clear the workspace

%% Set dynamic base paths
% The script identifies and sets the base directory paths for various data and scripts.
% Ensure that 'data', 'material', and 'script' folders are present relative to the base path.
filePath = fileparts(matlab.desktop.editor.getActiveFilename); % Get the current script's path
cd(filePath) 
cd(fullfile('..', '..', '..')) % Navigate up three directories to the base directory
basePath = pwd; % Store the base path

%% Set paths for data and output directories
path =          'path/to/preprocessed/data'; % Path where preprocessed data is stored for subjects
outputPath =    fullfile('.', 'data', 'outputs', filesep); % Define output path for results
modelingPath =  fullfile('.', 'scripts', 'modelling', 'analysed', filesep); % Define path for processed data
analysisPath =  fullfile('.', 'analyses', 'firstlevel', filesep); % Path to store first-level analysis results
firstlevelPath = '1st_02_glm1_VAL_RPE_oneMod/'; % Folder for first-level GLM analysis
name_batch = 'GLM_MSI_RPE.mat'; % Name of the batch file to be saved for each analyzed subject

%% Read subject list
% Read in the subject information from a CSV file
sublist = readtable([outputPath, 'outputfMRIv3AVeTV.csv'], ...
    'VariableNamingRule','preserve', 'Delimiter',',');
% Sort the subject list by ID and stimulus type
sublist = sortrows(sublist, {'ID', 'stimType'});

% Read log file with additional subject info
logFiles = readtable([outputPath, 'data_childrenMR.csv'], ...
    'Delimiter',';', 'VariableNamingRule','modify');

%% Define file filters and parameters
% Define patterns for filtering files
data_prefix =   '^s8wua.*'; % Filter for original preprocessed data files
name_rp =       '^rp.*.'; % Filter for realignment parameter files
flag =          'flagscans_allBad_v2.mat'; % Flag file for identifying bad scans

% data folder 
dataFolder = '';

% Define specific parameters for the analysis
TR_secs =       1.395; % Repetition time in seconds
MT_resolution = 44; % Microtime resolution (number of slices for slice-time correction)
MT_onset =      22; % Microtime onset (reference slice for slice-time correction)
mask_thresh =   0.2; % Threshold for mask definition (masking data based on intensity)

%% Define subject and model-related information
percModel =  'CBCsimpleRW'; % Define the perceptual model (e.g., "CBCsimpleRW")
respModel =  'driftDiffusionLR'; % Define the response model (e.g., "driftDiffusionLR")
model_path = [filesep '3_fit' filesep '*' percModel '*' respModel '*.mat']; % Path pattern for model files

% Extract unique subject IDs from the subject list
subjectAll = unique(sublist.ID);

% List files in the analysis directory that start with 'CBC_' (completed subjects)
d = dir(fullfile(basePath, analysisPath, firstlevelPath, 'CBC_*'));
subsDone = {d.name}; % Get names of the subjects were first-level is laready done
subject = setdiff(subjectAll, subsDone); % Identify subjects that are yet to be run

% List of subjects with old timings
subjectsOldTiming = {'CBC_1001', 'CBC_1002',  'CBC_1003', 'CBC_1004', 'CBC_1005',...
    'CBC_1006', 'CBC_1007', 'CBC_1008', 'CBC_1009'};

%% create matlab batch for each subject;
clear nRuns BadScans

% Loop through all subjects
for i=1:length(subject)
    % Initialize variables for the current subject
    clear matlabbatch D modelingTable currMods currList fileList sessions BS;
    subject{i}

    % Extract the data for the current subject from the subject list
    currList = sublist(ismember(sublist.ID, subject{i}),:);
    currList.session = (1:height(currList))';

    % Calculate the number of runs for each stimulus type (AV, TV)
    nRuns.AV(i) = sum(ismember(currList.stimType,'env'));
    nRuns.TV(i) = sum(ismember(currList.stimType,'vib'));

    % Define the paths to the preprocessed data and behavior logs
    prepro_path = fullfile('preprocessing', subject{i}, 'func', filesep);  
    logPath = fullfile('preprocessing', subject{i}, 'beh', filesep);

    % Load the model file for the current subject
    model_filename = dir([modelingPath subject{i} model_path]);
    load([model_filename.folder '/' model_filename.name]);

    % Loop through each session for the current subject
    for sess = 1:length(currList.session)
        clear logfilename logfile_table_all logfile_table idxFBneut
        % Current session number
        j = currList.session(sess);

        % Define the EPI file name for the current session
        name_data = [data_prefix currList.EPIfilename{sess}];

        % Load the onset file for the current session
        logfilename = currList.("logfile.x"){sess};

        % Special case handling for a specific log filename
        if contains(logfilename, '1o24')
            logfilename = 'CBC_1024_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv';
        end

        % Extract the outlier indices from the log files (e.g., neutral feedback, short/long RT)
        logfileOutliers = logFiles(strcmp(logFiles.logfile,logfilename),:);
        idxFBneut = find(logfileOutliers.trials_runs_feedback_given == 3); % trials with neutral feedback
        idxShortRT = find(logfileOutliers.outlier200ms == 1);
        idxLongRT = find(logfileOutliers.outlierSD == 1);

        % Read the logfile for the current session
        logfile_table_all = readtable([path logPath logfilename]);
        logfile_table = logfile_table_all(~isnan(logfile_table_all.trials_runs_thisRepN),:);

        % Load the model table for the current session
        if subject{i} == "CBC_1024" && j == 3
            modelingTable = D(strcmp(D.filename, 'CBC_1024_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv'),:);
        else
            modelingTable = D(strcmp(D.filename, logfilename),:);
        end
        idxOms = find(isnan(modelingTable.choiceLeft));

        % Check that omission indexes match with feedback neutral indexes
        if ~(idxOms == idxFBneut)
            warning(['different indexes for ommissions for subject ' subject{i} ' and run ' task{1}])
            return
        end

        % Get reward prediction error (RPE) and remove outliers (e.g., short/long RT)
        RPE = modelingTable.rewardPE;
        idx2remove = sort([idxOms; idxShortRT; idxLongRT]);
        RPE(idx2remove) = [];

        % Compute the belief value (VAL) from belief pairs
        V1 = modelingTable.beliefPair;
        V2 = modelingTable.beliefOtherPair;
        VAL = V1./(V1+V2);

        % Ensure there is variability in the belief value
        if std(VAL, 'omitnan')<0.0001
            warning(['no variation in VAL ' subject{i} ' and run ' num2str(sess)])
            VAL = VAL + 0.1*(rand(length(VAL),1)-0.5);
        end

        % Remove outliers and handle NaN values for VAL
        VAL(idx2remove) = [];
        VAL(isnan(VAL)) = 0;


        %% Path to the subject-specific folder
        pathSubject = fullfile(basePath, analysisPath, firstlevelPath, subject{i});

        % Create the folder if it doesn't exist
        if ~isfolder(pathSubject)
            mkdir(pathSubject);
        end

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%% SPECIFY 1ST LEVEL %%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        % Set up the first-level fMRI specification using SPM
        matlabbatch{1}.spm.stats.fmri_spec.dir = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.timing.units = 'secs';
        matlabbatch{1}.spm.stats.fmri_spec.timing.RT = TR_secs;
        matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t = MT_resolution;
        matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t0 = MT_onset;

        % Set up session-specific details for each scan
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).scans = '<UNDEFINED>';

        % Define feedback duration based on subject type
        if contains(subject{i}, subjectsOldTiming)
            FBdur = 2; % feedback duration for old subjects
        else
            FBdur = 1.8; % feedback duration for other subjects
        end

        % Define stimulus and feedback conditions
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).name = [currList.Task{sess} ' stimuli'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).duration = 2; % stimulus duration
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.name = [currList.Task{sess} ' Belief'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.param = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.poly = 1;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).orth = 1;

        % Feedback condition
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).name = [currList.Task{sess} ' feedback'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).duration = FBdur; % feedback duration
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.name = [currList.Task{sess} ' Prediction Error'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.param = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.poly = 1;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).orth = 1;

        % Regressor for stimulus onsets with no response
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).name = [currList.Task{sess} ' stim FB neutral'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).duration = 2;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).pmod = struct('name', {}, 'param', {}, 'poly', {});

        % Feedback-neutral condition
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).name = [currList.Task{sess} ' feedback neutral'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).duration = FBdur;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).pmod = struct('name', {}, 'param', {}, 'poly', {});

        % RT outliers for stimulus 
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).name = [currList.Task{sess} ' stim RT outlier'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).duration = 2;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).pmod = struct('name', {}, 'param', {}, 'poly', {});

        % RT outliers for feedback
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(6).name = [currList.Task{sess} ' feedback RT outlier'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(6).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(6).duration = FBdur;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(6).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(6).pmod = struct('name', {}, 'param', {}, 'poly', {});

        % Other details for session
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).multi = {''};
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).multi_reg = {''};
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).hpf = 128;
        matlabbatch{1}.spm.stats.fmri_spec.fact = struct('name', {}, 'levels', {});
        matlabbatch{1}.spm.stats.fmri_spec.bases.hrf.derivs = [0 0];
        matlabbatch{1}.spm.stats.fmri_spec.volt = 1;
        matlabbatch{1}.spm.stats.fmri_spec.global = 'None';
        matlabbatch{1}.spm.stats.fmri_spec.mthresh = mask_thresh;
        matlabbatch{1}.spm.stats.fmri_spec.mask = {''};


        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % ONSETS / PARAMETER VALUES
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        % Select the directory to save the SPM.mat file
        matlabbatch{1}.spm.stats.fmri_spec.dir = cellstr(pathSubject);

        % Define paths for preprocessed data and movement parameters
        data_path = fullfile(path, prepro_path, dataFolder, '/');
        rp_path = fullfile(path, prepro_path, '/');

        % Select smoothed scans and movement parameters
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).scans = cellstr(spm_select('ExtFPList', data_path, name_data ,Inf));
        nscans=numel(cellstr(spm_select('ExtFPList', data_path, name_data ,Inf)));
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).multi_reg = cellstr(spm_select('FPList', rp_path, [name_rp '_' currList.run{sess} '_.*.']));

        % Compute the first scan onset time
        index_scan=find(~isnan(logfile_table_all.('trigger_rt')));
        first_scan=logfile_table_all.('trigger_rt')(index_scan)+logfile_table_all.('trigger_started')(index_scan);

        % Identify valid trials for feedback and stimulus
        idxFB = find(~isnan(logfile_table.trials_runs_feedback_given));
        idxFB(idx2remove) = [];
        idxStim = find(~isnan(logfile_table.trials_runs_thisN));
        idxStim(idx2remove) = [];

        % Calculate onset times for stimulus events
        if contains(logfilename, "TV")
            speOnset = logfile_table.('tactile_started')(idxStim)-first_scan;
        else
            speOnset = logfile_table.('ImageLeft_4_started')(idxStim)-first_scan;
        end
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).onset = speOnset;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.param = VAL;

        % Calculate onset times for feedback events
        rpeOnset = logfile_table.('feedback_6_started')(idxFB)-first_scan;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).onset = rpeOnset;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.param = RPE;

        % Handle neutral feedback onset times
        if ~isempty(idxFBneut)
            % stimulus
            if contains(logfilename, "TV")
                matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).onset = logfile_table.('tactile_started')(idxFBneut)-first_scan;
            else
                matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).onset = logfile_table.('ImageLeft_4_started')(idxFBneut)-first_scan;
            end
            % feebdack
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).onset = logfile_table.('feedback_6_started')(idxFBneut)-first_scan;
        else
            % stimulus
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).onset = NaN;

            % feebdack
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).onset = NaN;
        end

        % Process onset times for short and long reaction times
        idxRT = sort([idxLongRT idxShortRT]);
        if ~isempty(idxRT)
            % stimulus
            if contains(logfilename, "TV")
                matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).onset = logfile_table.('tactile_started')(idxRT)-first_scan;
            else
                matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).onset = logfile_table.('ImageLeft_4_started')(idxRT)-first_scan;
            end
            % feebdack
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(6).onset = logfile_table.('feedback_6_started')(idxRT)-first_scan;
        else
            % stimulus
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(5).onset = NaN;

            % feebdack
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(6).onset = NaN;
        end

        % Check for flagged scans that need additional regressors
        dir_flag=dir([art_path '*' currList.run{sess}(1:8) '*' currList.run{sess}(end-3:end) '_' flag]);
        if ~isempty(dir_flag)
            disp('found bad scans to flag')
            file_flag= [dir_flag.folder '/' dir_flag.name];  %sets together name of flagfile
            load(file_flag);
        else
            Regr_badscans = zeros(nscans,1);
        end

        % Verify flagged scans match expected scan count
        if ~(size(Regr_badscans,1) == nscans)
            warning(['REG badscans not same length as nscans for ' subject{i} ' and run ' task{1}])
            return
        end

        % Create regressors for flagged scans
        idxBS = find(Regr_badscans);
        if sum(Regr_badscans)~=0
            for k = 1:sum(Regr_badscans)
                matlabbatch{1}.spm.stats.fmri_spec.sess(j).regress(k).name = ['bad scan ' num2str(k)];
                regrBS = zeros(nscans,1);
                regrBS(idxBS(k)) = 1;
                matlabbatch{1}.spm.stats.fmri_spec.sess(j).regress(k).val = regrBS;
            end
            BS(1) = sum(Regr_badscans);
        else
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).regress(1).name = 'bad scans';
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).regress(1).val = Regr_badscans;
            BS(1) = 1;
        end

        % Store flagged scan count
        BadScans(i,j) = BS;
    end
    % Save the batch processing structure
    save(fullfile(pathSubject, name_batch),'matlabbatch');
end
% Save bad scan and run count data
save([path analysisPath firstlevelPath '/badscans.mat'], 'BadScans')
save([path analysisPath firstlevelPath '/numberRuns.mat'], 'nRuns')


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Model Specification %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%can be used for opening the batch and controlling the inputs:
%spm_jobman('interactive', matlabbatch(1))
for i=1:length(subject)
    clear matlabbatch
    subject{i}
    load([path analysisPath firstlevelPath '/' subject{i} '/' name_batch])
    %run the batch
    spm_jobman('run', matlabbatch(1));
end


%%
%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% ESTIMATE %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
for i=1:length(subject)
    clear matlabbatch
    subject{i}
    pathSubject = fullfile(path, analysisPath, firstlevelPath, subject{i});
    %select SPM.mat file that contains the design specification
    matlabbatch{2}.spm.stats.fmri_est.spmmat = {[pathSubject,'/SPM.mat']};
    matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;

    %run the batch
    spm_jobman('run', matlabbatch(2));
end

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% CONTRASTS %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
% Load required data
load([path analysisPath firstlevelPath '/badscans.mat'])
load([path analysisPath firstlevelPath '/numberRuns.mat'])

for i=1:length(subject)
    % Clear variables for each iteration
    clear matlabbatch nBS nAV nTV weights wgths
    subject{i}  % Display current subject
    
    % Define the path to the SPM.mat file for contrast definition
    pathSubject = fullfile(path, analysisPath, firstlevelPath, subject{i});
    matlabbatch{3}.spm.stats.con.spmmat = {[pathSubject,'/SPM.mat']};

    % Extract the number of bad scans, AV runs, and TV runs for the current subject
    nBS = BadScans(i,:); 
    nAV = nRuns.AV(i); 
    nTV = nRuns.TV(i); 

    % Compute the total number of regressors (each AV/TV run has 14 regressors + bad scan regressors)
    weightsLength = nAV*14 + nTV*14 + sum(nBS); 
    weights = zeros(1, weightsLength); % Initialize contrast weights as zeros

    j=1;

    % Define contrasts for all stimulus conditions
    conditions = {'stim', 'VAL', 'FB', 'RPE'};
    contrast_weights = [1 0 0 0; 0 1 0 0; 0 0 1 0; 0 0 0 1];

    for k = 1:length(conditions)
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = conditions{k};
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = contrast_weights(k, :);
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'replsc';
        j=j+1;
    end

    % Check for number of AV and TV runs and define task-specific contrasts
    if nAV + nTV == 4
        % Define contrasts for AV and TV tasks
        % ---- AV ----
        task = 'AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
         
            wgths = weights;
            wgths(k) = 1;
            wgths(k+14+nBS(1)) = 1;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV ---
        task = 'TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];

            wgths = weights;
            wgths(k+14+nBS(1)+14+nBS(2)) = 1;
            wgths(k+14+nBS(1)+14+nBS(2)+14+nBS(3)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end


        % ---- AV vs TV ----
        task = 'AV vs TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = 1;
            wgths(k+14+nBS(1)) = 1;
            wgths(k+14+nBS(1)+14+nBS(2)) = -1;
            wgths(k+14+nBS(1)+14+nBS(2)+14+nBS(3)) = -1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV vs AV ----
        task = 'TV vs AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = -1;
            wgths(k+14+nBS(1)) = -1;
            wgths(k+14+nBS(1)+14+nBS(2)) = 1;
            wgths(k+14+nBS(1)+14+nBS(2)+14+nBS(3)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

    elseif nAV + nTV == 3 && nTV == 1
        % ---- AV ----
        task = 'AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = 1;
            wgths(k+14+nBS(1)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV ---
        task = 'TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];

            wgths = weights;
            wgths(k+14+nBS(1)+14+nBS(2)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end


        % ---- AV vs TV ----
        task = 'AV vs TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = 0.5;
            wgths(k+14+nBS(1)) = 0.5;
            wgths(k+14+nBS(1)+14+nBS(2)) = -1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV vs AV ----
        task = 'TV vs AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = -0.5;
            wgths(k+14+nBS(1)) = -0.5;
            wgths(k+14+nBS(1)+14+nBS(2)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end
        
    elseif nAV + nTV == 3 && nAV == 1
       % ---- AV ----
        task = 'AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV ---
        task = 'TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];

            wgths = weights;
            wgths(k+14+nBS(1)) = 1;
            wgths(k+14+nBS(1)+14+nBS(2)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end


        % ---- AV vs TV ----
        task = 'AV vs TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = 1;
            wgths(k+14+nBS(1)) = -0.5;
            wgths(k+14+nBS(1)+14+nBS(2)) = -0.5;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV vs AV ----
        task = 'TV vs AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = -1;
            wgths(k+14+nBS(1)) = 0.5;
            wgths(k+14+nBS(1)+14+nBS(2)) = 0.5;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

    elseif nAV + nTV == 2
         % ---- AV ----
        task = 'AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV ---
        task = 'TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];

            wgths = weights;
            wgths(k+14+nBS(1)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end


        % ---- AV vs TV ----
        task = 'AV vs TV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = 1;
            wgths(k+14+nBS(1)) = -1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

        % ---- TV vs AV ----
        task = 'TV vs AV';
        for k = 1:length(conditions)
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
            
            wgths = weights;
            wgths(k) = -1;
            wgths(k+14+nBS(1)) = 1;
            matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;

            matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
            j=j+1;
        end

    end

    % Delete previous contrasts before adding new ones
    matlabbatch{3}.spm.stats.con.delete = 1;

    % Run the contrast batch using SPM
    spm_jobman('run', matlabbatch(3));
end