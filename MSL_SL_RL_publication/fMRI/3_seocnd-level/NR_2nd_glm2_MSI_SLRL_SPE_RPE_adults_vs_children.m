% Script for 2nd level analysis of MSI Task
% two-sample t-tests
% Author: NR, January 2025

clear matlabbatch;
clear all;

% Option to analyze all subjects or exclude those with a diagnosis
wholeSample = true; % Set to false to exclude diagnosed subjects

%% Set dynamic base paths
% The script identifies and sets the base directory paths for various data and scripts.
% Ensure that 'data', 'material', and 'script' folders are present relative to the base path.
filePath = fileparts(matlab.desktop.editor.getActiveFilename); % Get the current script's path
cd(filePath)
cd(fullfile('..', '..', '..')) % Navigate up three directories to the base directory
basePath = pwd; % Store the base path

%% Define paths
% Path to first-level analysis results
dataPath1 = fullfile('.', 'analyses', 'firstlevel', '1st_02_glm1_SPE_RPE_children_ICA', filesep);
dataPath2 = fullfile('.', 'analyses', 'firstlevel', '1st_02_glm1_SPE_RPE_adults_ICA', filesep);

% Path to store second-level analysis results
stats = fullfile('.', 'analyses', 'secondlevel', 'twosample_t-tests', '1st_02_glm1_SPE_RPE_adults_vs_children', filesep);

path_mask='';

%% Define subjects
% Read subject demographic data from CSV file
sublistv1 = readtable(fullfile('.', 'data', 'outputs', 'finalSubjects.csv'), ...
    'VariableNamingRule','preserve', 'Delimiter',',');
sublistv1 = sublistv1(~ismember(sublistv1.ID, 'CBC_1089'),:);
sublistv1 = sublistv1(ismember(sublistv1.version,"v1"),:);

% sublist for adults
sublistv1Ad = readtable(fullfile('.', 'data', 'outputs', 'finalSubjectsAdults.csv'), ...
    'VariableNamingRule','preserve', 'Delimiter',',');
sublistv1Ad = sublistv1Ad(~contains(sublistv1Ad.ID, {'P008', 'P025', 'P032', 'P012', 'P011', 'P028', 'P021', 'P003'}),:);

% combine both lists
sublist = [sublistv1Ad; sublistv1(:,sublistv1Ad.Properties.VariableNames)];
sublist = sortrows(sublist, {'version', 'ID'});

% Convert handedness to numeric format
sublist.handedness(sublist.ID == "CBC_P006") = {'right'};
sublist.handedness(sublist.ID == "CBC_P016") = {'right'};
sublist.handedness2(ismember(sublist.handedness, 'right')) = 1;
sublist.handedness2(ismember(sublist.handedness, 'left')) = -1;
sublist.handedness2(ismember(sublist.handedness, 'ambidexterous')) = 0;
sublist.handedness2(ismember(sublist.handedness, 'NA')) = 1;

% Extract covariate information
coVariables = unique(sublist(:,{'ID', 'gender', 'age', 'handedness2'}));
coVariables.gender(ismember(coVariables.gender, 'f')) = {'0'};
coVariables.gender(ismember(coVariables.gender, 'm')) = {'1'};
coVariables.gender = str2double(coVariables.gender);

% Ensure subject list matches covariate data
if ~isequal(coVariables.ID, subject')
    error('coVariables and subject are not in the same order')
end

% Assign covariate values
ageValues = coVariables.age; % age in years
genderValues = coVariables.gender; % 0 = f, 1 = m
handednessValues = coVariables.handedness2; % r = 1, a = 0, l = -1

%% split the subjects into the two groups
% children group
sub1 = unique(sublistv1.ID);
sub1 = sub1(~ismember(sub1, 'CBC_1089'));
% adults group
sub2 = unique(sublistv1Ad.ID);

%% Define number of contrasts
% Identify contrast files from first-level analysis
d=dir([dataPath subject{1} '/*_*.nii']);
d={d.name};

% Filter and sort contrast files
con_names = {'con', 'ess'}; % define desired names
index = find(contains(d, con_names));
Cons=d(index);
nums = extractBetween(Cons,5,8); % extract the numbers from the names
[~,sortOrder] = sort(nums); %get the order in which to sort the files
Cons = Cons(sortOrder); % sort the files
nCons = numel(Cons);

%define type of contrast files
nii = '.nii';

%define path where results from 2nd level analysis will be stored
ConDir = [studyPath stats];

%define folder names where contrasts from 2nd level analysis will be stored
ConFolder = {
    ['01 All Stim']
    ['02 All SPE']
    ['03 All FB']
    ['04 All RPE']
    ['05 AV Stim']
    ['06 AV SPE']
    ['07 AV FB']
    ['08 AV RPE']
    ['09 TV Stim']
    ['10 TV SPE']
    ['11 TV FB']
    ['12 TV RPE']
    ['13 AV vs TV Stim']
    ['14 AV vs TV SPE']
    ['15 AV vs TV FB']
    ['16 AV vs TV RPE']
    ['17 TV vs AV Stim']
    ['18 TV vs AV SPE']
    ['19 TV vs AV FB']
    ['20 TV vs AV RPE']
    };

%% Specify second-level design for each contrast
for j=1:nCons
    % clear variables
    clear scans_up scans_down
    
    % Collect contrast files for each group
    % children group
    for i = 1 : length(sub1)
        subjDir = [dataPath filesep sub1{i} filesep];
        scans_up(i,:) = spm_select('FPList', subjDir, Cons{j});
    end

    % adult group
    for i = 1 : length(sub2)
        subjDir = [dataPath filesep sub2{i} filesep];
        scans_down(i,:) = spm_select('FPList', subjDir,Cons{j});
    end

    % Define output directory
    statsDir = [ConDir ConFolder{j}];
    if ~isfolder(statsDir)
        mkdir(statsDir);
    end

     % Specify factorial design in SPM batch
    matlabbatch{j}.spm.stats.factorial_design.dir = {statsDir}; %directory
    matlabbatch{1}.spm.stats.factorial_design.des.t2.scans1 = cellstr(scans_up);
    matlabbatch{1}.spm.stats.factorial_design.des.t2.scans2 = cellstr(scans_down);
    matlabbatch{1}.spm.stats.factorial_design.des.t2.dept = 0;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.variance = 1;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.gmsca = 0;
    matlabbatch{1}.spm.stats.factorial_design.des.t2.ancova = 0;

    %add co-variables
    k = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(k).c = handednessValues;
    matlabbatch{1}.spm.stats.factorial_design.cov(k).cname = 'handedness';
    matlabbatch{1}.spm.stats.factorial_design.cov(k).iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(k).iCC = 1;
    k = k+1;
    
    matlabbatch{1}.spm.stats.factorial_design.cov(k).c = genderValues;
    matlabbatch{1}.spm.stats.factorial_design.cov(k).cname = 'gender';
    matlabbatch{1}.spm.stats.factorial_design.cov(k).iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(k).iCC = 1; 
    k = k+1;

    matlabbatch{1}.spm.stats.factorial_design.multi_cov = struct('files', {}, 'iCFI', {}, 'iCC', {});
    matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.em = {path_mask};
    matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;
    
    %% model estimation
    matlabbatch{2}.spm.stats.fmri_est.spmmat(1) = cfg_dep('Factorial design specification: SPM.mat File', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
    matlabbatch{2}.spm.stats.fmri_est.write_residuals = 0;
    matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;
    
    %% define contrasts
    matlabbatch{3}.spm.stats.con.spmmat(1) = cfg_dep('Model estimation: SPM.mat File', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
    
    % first contrast
    c = 1; % contrast counter

    % both groups combined
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = [ConFolder{j} ' children + adults'];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [1 1];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';
    c = c + 1;
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = ['-' ConFolder{j} ' children + adults'];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [-1 -1];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';
    c = c + 1;

    % children vs. adults
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = [ConFolder{j} ' children vs adults' ];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [1 -1];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';
    c = c + 1;

    % adults vs. children
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = [ConFolder{j} ' adults vs children'];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [-1 1];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';
    c = c + 1;

    % children
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = [ConFolder{j} ' children'];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [1 0];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';
    c = c + 1;
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = ['-' ConFolder{j} ' children'];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [-1 0];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';
    c = c + 1;

    % adults
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = [ConFolder{j} ' adults'];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [0 1];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';
    c = c + 1;
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.name = ['-' ConFolder{j} ' adults'];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.weights = [0 -1];
    matlabbatch{3}.spm.stats.con.consess{c}.tcon.sessrep = 'none';

    matlabbatch{3}.spm.stats.con.delete = 0;

    % run matlab batch
    jobdir = [dataPath filesep 'BatchJob'];
    save(jobdir, 'matlabbatch');
    spm_jobman('run',matlabbatch);
    clear matlabbatch

end
