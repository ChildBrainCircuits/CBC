% Main Script for Preprocessing 
% Combines all the sub-scripts
% NR, September 2024
clear
clc

%% Set dynamic base paths
% The script identifies and sets the base directory paths for various data and scripts.
% Ensure that 'data', 'material', and 'script' folders are present relative to the base path.
filePath = fileparts(matlab.desktop.editor.getActiveFilename); % Get the current script's path
cd(filePath) 
cd(fullfile('..', '..', '..')) % Navigate up three directories to the base directory
basePath = pwd; % Store the base path

%% Subjects
% Paper 1 -----------------------------------------------------------------
% Read the list of final subjects from a CSV file for Paper 1 (AV vs TV).
subsP1 = readtable(fullfile(basePath, 'scripts', 'statistical analyses', 'paper1_AV vs TV', 'output', 'finalSubjects.csv'));

% Extract unique subject IDs from Paper 1, excluding 'NA' entries
subsAll = unique(subsP1.ID);
subsAll = subsAll(~ismember(subsAll, 'NA'));

% Assign the subject list for preprocessing
subjects = subsAll;

%% Paths
% Set various paths for backup data, output data, and local preprocessing.
% These paths should point to the relevant directories on the system.
backupPath = 'path/to/raw/data'; % (Raw data is not shared due to privacy concerns)
outputPath = 'path/to/output/data'; % (Preprocessed data is not shared)
localPreproPath = 'local/path/for/preprocessing/'; % (Local preprocessing)
preproPath = 'path/were/preprocessed/data/should/be/stored'; % preprocessed data is not shared 

% Path to the preprocessing script folder.
scriptPath = fullfile('.', 'scripts', 'fMRI', '1_preprocessing');

% Change to the folder with preprocessing functions.
cd(fullfile(scriptPath, 'fMRI_preprocessing_functions'))

%% create overview
% Generate an overview of all subjects, e.g., to check the number of subjects available for preprocessing.
savePath = fullfile(basePath, 'data', 'outputs', filesep);

% Calls function to generate overview for children subjects, saving results to 'savePath'.
overview_subs(backupPath, savePath, 'children')

%% copy files for preproc
% Copies necessary files from backup to a local folder for preprocessing.
copy_files_for_preproc(subjects, backupPath, localPreproPath, 'children')

%% Preprocessing
% Set up the anatomical template for preprocessing.
anatTemplate = fullfile(basePath, 'material', 'structural template', 'TPM_Age10.699-7.434.nii');

% Add paths for preprocessing functions and the script directory.
addpath(localPreproPath);
addpath(fullfile(scriptPath, 'fMRI_preprocessing_functions'));

% List of tasks for preprocessing (in this case, 'msi' task).
tasklist =  {'msi'};

% Set overwrite flag (whether to overwrite existing preprocessing files).
% overwrite = 1; % overwrite existing files -> reanalyses everything
overwrite = 0; % don't overwrite -> skip this files

% Call the preprocessing function for the 'msi' task.
AS_preproc_par_msi(subjects, tasklist, localPreproPath, anatTemplate, overwrite)

% Remove paths after preprocessing is done.
rmpath(localPreproPath);

%% Copy files after preprocessing
% Copy the preprocessed files from the local directory to the output directory.
cd(fullfile(scriptPath, 'fMRI_preprocessing_functions'))

copy_files_for_preproc(subjects, localPreproPath, preproPath, 'children')

%% Add log files to B0 table
% Add log file names to the B0 table for each subject.
cd(fullfile(scriptPath, 'fMRI_preprocessing_functions'))

logfilesB0(subjects, preproPath, 'children');

%% Run ARTRepair
% Perform artifact repair on the data using the ARTRepair tool.
AS_run_artrepair(subjects, preproPath)

%% Create regressors
% Create regressors for the bad scan data, based on the available bad scans file.
cd(fullfile(scriptPath, 'fMRI_preprocessing_functions'))

AS_new_create_regressor_badscans_allBad(subjects, preproPath)

%% Create a table with all bad scans for all subjects
% For all bad scan data, call the function to aggregate and save a table with bad scan information.
files = 'allBad_v2.txt'; 
AS_create_tables_badScans(preproPath, outputPath, files)

% Remove paths after preprocessing and analysis steps.
rmpath(fullfile(scriptPath, 'fMRI_preprocessing_functions'))
