% script for 1st level analysis of MSI Task
% Stimulus and FB processing
% Surprise (SPE) and Prediction Error (PRE)
% CBC
% Nina Raduner, October 2023;

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
firstlevelPath = '1st_02_glm1_SPE_RPE_adults_ICA/'; % Folder for first-level GLM analysis
name_batch = 'GLM_MSI_SPE_RPE.mat'; % Name of the batch file to be saved for each analyzed subject

%% Read subject list
% Read in the subject information from a CSV file

% match recognition task
sublistv1 = readtable([outputPath, 'outputfMRIAdultsAVeTV.csv'],...
    'VariableNamingRule','preserve', 'Delimiter',';');
sublistv1.ACC_4th = num2cell(sublistv1.ACC_4th);

% combine both groups
sublist = sublistv1;
% Sort the subject list by ID and stimulus type
sublist = sortrows(sublist, {'ID', 'stimType'});
sublist.Task = strrep(sublist.Task, 'av11', 'av1');
sublist.Task = strrep(sublist.Task, 'tv11', 'tv1');

% Read log file with additional subject info
logFiles = readtable([outputPath, 'MRdata_adultsMR.csv'], ...
    'Delimiter',';', 'VariableNamingRule','modify');

% check if all subjects are in the logfiles
if length(unique(logFiles.ID)) ~= 36
    warning('adults missing in logFiles')
end

% read modeling output data
modelingData = readtable([outputPath, 'modellingOuputfMRI_adults.csv'], ...
    'Delimiter',',', 'VariableNamingRule','modify');
modelingData.modality(contains(modelingData.mod2Type,'aud')) = {'av'};
modelingData.modality(contains(modelingData.mod2Type,'tac')) = {'tv'};

% check if all subjects are in the modelingdata
if length(unique(modelingData.ID)) ~= 28
    warning('adults missing in modelingData')
end

% read surprise output data
surpriseData =  readtable([outputPath, 'surpriseOuputfMRI_adults.csv'], ...
    'Delimiter',',', 'VariableNamingRule','modify');
surpriseData.modality(contains(surpriseData.mod2Type,'aud')) = {'av'};
surpriseData.modality(contains(surpriseData.mod2Type,'tac')) = {'tv'};

% check if all subjects are in the surprise
if length(unique(surpriseData.ID)) ~= 36
    warning('children missing in surpriseData')
end

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

% explicit mask from SPM without eyes
expliMask = fullfile('.', 'material', 'masks', 'mask_ICV.nii');

%% subjects
% Extract unique subject IDs from the modeling data
subjectAll = unique(modelingData.ID);

% List files in the analysis directory that start with 'CBC_' (completed subjects)
d = dir(fullfile(basePath, analysisPath, firstlevelPath, 'CBC_*'));
subsDone = {d.name}; % Get names of the subjects were first-level is laready done
subject = setdiff(subjectAll, subsDone); % Identify subjects that are yet to be run

if isempty(subject)
    subject = subjectAll;
end

%% create matlab batch for each subject;
clear nRuns BadScans

% Loop through all subjects
for i=1:length(subject)
    % Clear variables that vary per subject to avoid data carryover
    clear matlabbatch D modelingTable currMods currList fileList sessions BS
    subject{i}

     % Select the subset of runs corresponding to the current subject from the master list
    currList = sublist(ismember(sublist.ID, subject{i}),:);
    % Assign a sequential session index to each run
    currList.sess = (1:height(currList))';

    % Count the number of auditory-visual ('env') and tactile-visual ('vib') runs for this subject
    nRuns.AV(i) = sum(ismember(currList.stimType,'env'));
    nRuns.TV(i) = sum(ismember(currList.stimType,'vib'));

    % Define paths for preprocessed functional data and behavioral logs
    prepro_path = fullfile('preprocessing', subject{i}, 'func', filesep);  %path to preprocessed data
    logPath = fullfile('preprocessing', subject{i}, 'beh', filesep);

    %% Loop through each session/run for the current subject
    for sess = 1:length(currList.sess)
        % Clear intermediate variables for this run
        clear logfilename logfile_table_all logfile_table idxFBneut modelingTable surpriseTable 
        clear idxOmsSPE idxOms RPE SPE Regr_badscans
        
        % Identify the current session index
        j = currList.sess(sess);

         % Construct the EPI filename using the experiment-specific prefix
        name_data = [data_prefix currList.EPIfilename{sess}];

        % Load the onset log file name; handle special case for misnamed file
        logfilename = currList.("logfile"){sess};

        % Identify outlier trials based on precomputed logFiles table
        logfileOutliers = logFiles(strcmp(logFiles.logfile,logfilename),:);
        idxFBneut = find(logfileOutliers.trials_runs_feedback_given == 3); % neutral feedback trials
        idxShortRT = find(logfileOutliers.outlier200ms == 1);              % RT < 200 ms
        idxLongRT = find(logfileOutliers.outlierSD == 1);                  % RT > 3 SD
        
        % Ensure no trial is both short- and long-RT outlier
        if ~isempty(idxShortRT) && isequal(idxShortRT, idxLongRT)
            error('equal short and long RT outlier')
        end

        % Read the full and filtered log tables
        logfile_table_all = readtable([path logPath logfilename]);
        logfile_table = logfile_table_all(~isnan(logfile_table_all.trials_runs_thisRepN),:);

        % Load modeling data for the current subject and run, handling special filename exceptions
        if subject{i} == "CBC_1024" && j == 3
            modelingTable = modelingData(strcmp(modelingData.filename, 'CBC_1024_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv'),:);
        else
            modelingTable = modelingData(strcmp(modelingData.filename, logfilename),:);
        end
        idxOms = find(isnan(modelingTable.choiceAccurate));

        % Verify omission indices match neutral feedback trials
        if ~(idxOms == idxFBneut)
            warning(['different indexes for ommissions for subject ' subject{i} ' and run ' task{1}])
            return
        end

        % Extract and clean reward prediction errors (RPE)
        RPE = modelingTable.rewardPE;
        idx2remove = sort([idxOms; idxShortRT; idxLongRT]);
        RPE(idx2remove) = [];
        
        % Load surprise data for the current run
        if subject{i} == "CBC_1024" && j == 3
            surpriseTable = surpriseData(strcmp(surpriseData.filename, 'CBC_1024_MSI_TV_v3_Vset3_Tset1_2023-07-05_15h02.58.307.csv'),:);
        else
            surpriseTable = surpriseData(strcmp(surpriseData.filename, logfilename),:);
        end
        idxOmsSPE = find(isnan(surpriseTable.reactionTime));

        % Verify surprise omissions match neutral feedback
        if ~(idxOmsSPE == idxFBneut)
            warning(['different indexes for ommissions for subject ' subject{i} ' and run ' task{1}])
            return
        end

        SPE = surpriseTable.simpleSurprise;

        %% Create output directory for the subject if it does not exist
        pathSubject = fullfile(path, analysisPath, firstlevelPath, subject{i});

        if ~isfolder(pathSubject)
            mkdir(pathSubject);
        end

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%% SPECIFY 1ST LEVEL %%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        matlabbatch{1}.spm.stats.fmri_spec.dir = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.timing.units = 'secs';
        matlabbatch{1}.spm.stats.fmri_spec.timing.RT = TR_secs;
        matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t = MT_resolution;
        matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t0 = MT_onset;

        % Assign functional scans for this session
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).scans = '<UNDEFINED>';

        % Determine feedback duration based on subject timing group
        if contains(subject{i}, subjectsOldTiming)
            FBdur = 2;
        else
            FBdur = 1.8;
        end

        %%% Condition 1: Stimulus onsets and parametric modulation by surprise
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).name = [currList.Task{sess} ' stimuli'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).duration = 2; % can also be 0 for event related
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.name = [currList.Task{sess} ' Surprise'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.param = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.poly = 1;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).orth = 1;

        %%% Condition 2: Feedback onsets and parametric modulation by prediction error
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).name = [currList.Task{sess} ' feedback'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).duration = FBdur;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.name = [currList.Task{sess} ' Prediction Error'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.param = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.poly = 1;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).orth = 1;

        %%% Condition 3: Neutral feedback as separate regressor
        k = 3;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).name = [currList.Task{sess} ' feedback neutral'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).duration = FBdur;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).pmod = struct('name', {}, 'param', {}, 'poly', {});
        k = k+1;

        %%% Condition 4: Feedback RT outliers
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).name = [currList.Task{sess} ' feedback RT outlier'];
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).onset = '<UNDEFINED>';
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).duration = FBdur;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).tmod = 0;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(k).pmod = struct('name', {}, 'param', {}, 'poly', {});
        k = k+1;

        % Disable custom multi and regress settings; high-pass filter applied
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).multi = {''};
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).multi_reg = {''};
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).hpf = 128;

        % Set basis functions and masking parameters
        matlabbatch{1}.spm.stats.fmri_spec.fact = struct('name', {}, 'levels', {});
        matlabbatch{1}.spm.stats.fmri_spec.bases.hrf.derivs = [0 0];
        matlabbatch{1}.spm.stats.fmri_spec.volt = 1;
        matlabbatch{1}.spm.stats.fmri_spec.global = 'None';
        matlabbatch{1}.spm.stats.fmri_spec.mthresh = mask_thresh;
        matlabbatch{1}.spm.stats.fmri_spec.mask = {expliMask};


        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % ONSETS / PARAMETER VALUES
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %% Specify design directory and select scans/regressors
        matlabbatch{1}.spm.stats.fmri_spec.dir = cellstr(pathSubject);

        %Onsets and values:
        data_path = fullfile(path, prepro_path, dataFolder, '/');
        art_path = fullfile(path, prepro_path, '/ART/');
        rp_path = fullfile(path, prepro_path, '/');

        % Select smoothed EPI volumes for analysis
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).scans = cellstr(spm_select('ExtFPList', data_path, name_data ,Inf));
        nscans=numel(cellstr(spm_select('ExtFPList', data_path, name_data ,Inf)));

        % Specify motion and ICA regressors
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).multi_reg = {spm_select('FPList', rp_path, [name_rp '_' currList.run{sess} '_.*.']); ...
            spm_select('FPList', rp_path, ['ICAregressors_sess' num2str(currList.session(sess)) '.mat'])};

        %% Compute onsets relative to first functional volume
        index_scan=find(~isnan(logfile_table_all.('trigger_rt')));
        first_scan=logfile_table_all.('trigger_rt')(index_scan)+logfile_table_all.('trigger_started')(index_scan);

        % Identify valid feedback and stimulus trials, excluding outliers and omissions
        idxFB = find(~isnan(logfile_table.trials_runs_feedback_given));
        idxFB(idx2remove) = [];
        idxStim = find(~isnan(logfile_table.trials_runs_thisN));

        % Determine stimulus onsets based on modality
        if contains(logfilename, "TV")
            %speOnset = logfile_table.('image_stim_left_started')(allstim)-first_scan;
            speOnset = logfile_table.('image_3_started')(idxStim)-first_scan;
        else
            speOnset = logfile_table.('image_3_started')(idxStim)-first_scan;
        end

        % Assign onsets and parametric modulator for Condition 1
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).onset = speOnset;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(1).pmod.param = SPE;

        % Feedback onsets for Condition 2
        rpeOnset = logfile_table.('feedback_3_started')(idxFB)-first_scan;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).onset = rpeOnset;
        matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(2).pmod.param = RPE;

        % Neutral feedback onsets for Condition 3
        if ~isempty(idxFBneut)
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).onset = logfile_table.('feedback_3_started')(idxFBneut)-first_scan;
        else
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(3).onset = NaN;
        end

        % RT outlier feedback onsets for Condition 4
        idxRT = sort([idxLongRT idxShortRT]);
        if ~isempty(idxRT)
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).onset = logfile_table.('feedback_3_started')(idxRT)-first_scan;
        else
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).cond(4).onset = NaN;
        end

        %% Identify and model bad scans as regressors
        dir_flag=dir([art_path '*' currList.run{sess}(1:8) '*' currList.run{sess}(end-3:end) '_' flag]);
        if ~isempty(dir_flag)
            disp('found bad scans to flag')
            file_flag= [dir_flag.folder '/' dir_flag.name];  
            load(file_flag);            % loads Regr_badscans vector
        else
            Regr_badscans = zeros(nscans,1);
        end

        if ~(size(Regr_badscans,1) == nscans)
            warning(['REG badscans not same length as nscans for ' subject{i} ' and run ' task{1}])
            return
        end

        % Add a separate regressor for each bad scan
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
            % If no bad scans, include a zero regressor for consistency
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).regress(1).name = 'bad scans';
            matlabbatch{1}.spm.stats.fmri_spec.sess(j).regress(1).val = Regr_badscans;
            BS(1) = 1;
        end
        BadScans(i,j) = BS;
    end
    % Save the batch processing structure
    save(fullfile(pathSubject, name_batch),'matlabbatch');
end

% After looping all subjects, save summary metrics for bad scans and run counts
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
% Load summary metrics for bad scans and run counts
load([path analysisPath firstlevelPath '/badscans.mat'])
load([path analysisPath firstlevelPath '/numberRuns.mat'])

% Loop through each subject to define and run contrasts
for i=1:length(subject)
    clear matlabbatch nBS nAV nTV weights wgths
    subject{i} 

    % Prepare contrast batch structure
    pathSubject = fullfile(path, analysisPath, firstlevelPath, subject{i});
    matlabbatch{3}.spm.stats.con.spmmat = {[pathSubject,'/SPM.mat']};

    % Retrieve subject-specific metrics
    nBS = BadScans(i,:);    % vector of bad-scan counts per run
    nAV = nRuns.AV(i);      % number of AV runs
    nTV = nRuns.TV(i);      % number of TV runs
    nReg = 12+10;           % regressors per run (12 task + 10 ICA regressors)

    % Total number of regressors: all runs + bad-scan regressors
    weightsLength = nAV*nReg + nTV*nReg + sum(nBS); 
    weights = zeros(1, weightsLength); % initialize weight vector

    % Define names of conditions of interest
    conditions = {'stim', 'SPE', 'FB', 'RPE'};
    j = 1;  % contrast counter

    %%% 1. All-run contrasts (across sessions, replicated)
    for c = 1:4
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.name    = ['ALL ' conditions{c}];
        w = zeros(1,4); w(c) = 1;
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = w;
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep  = 'replsc';
        j = j + 1;
    end

    % ---- Task-specific contrasts for the AV condition ----
    task = 'AV';
    for k = 1:length(conditions)
        % Name this contrast as “AV <condition>”
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
        % Start from a zero vector and set the k-th regressor weight to 1
        wgths = weights;
        wgths(k) = 1;
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;
         % Do not replicate this contrast across sessions
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
        j=j+1;
    end

    % ---- Task-specific contrasts for the TV condition ----
    task = 'TV';
    for k = 1:length(conditions)
        % Name this contrast as “TV <condition>”
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
        % Offset by (nReg + nBS(1)) to index TV regressors, then set that weight to 1
        wgths = weights;
        wgths(k+nReg+nBS(1)) = 1;
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;
        % Do not replicate this contrast across sessions
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
        j=j+1;
    end

    % ---- Contrast of AV versus TV for each condition ----
    task = 'AV vs TV';
    for k = 1:length(conditions)
        % Name this contrast as “AV vs TV <condition>”
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
        % Positive weight on the AV regressor, negative weight on the corresponding TV regressor
        wgths = weights;
        wgths(k) = 1;
        wgths(k+nReg+nBS(1)) = -1;
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;
        % Do not replicate this contrast across sessions
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
        j=j+1;
    end

    % ---- Contrast of TV versus AV for each condition ----
    task = 'TV vs AV';
    for k = 1:length(conditions)
        % Name this contrast as “TV vs AV <condition>”
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.name = [task ' ' conditions{k}];
        % Negative weight on the AV regressor, positive weight on the corresponding TV regressor
        wgths = weights;
        wgths(k) = -1;
        wgths(k+nReg+nBS(1)) = 1;
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.weights = wgths;
        % Do not replicate this contrast across sessions
        matlabbatch{3}.spm.stats.con.consess{j}.tcon.sessrep = 'none';
        j=j+1;
    end

    % Remove any existing contrasts before running the new set
    matlabbatch{3}.spm.stats.con.delete = 1;

    % Execute the contrast batch for this subject
    spm_jobman('run', matlabbatch(3));
end