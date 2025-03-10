% Script for 2nd level analysis of MSI Task
% one-sample t-tests
% Author: NR, September 2024

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
dataPath = fullfile('.', 'analyses', 'firstlevel', '1st_02_glm1_VAL_RPE_oneMod', filesep);

% Path to store second-level analysis results
if wholeSample
    stats = fullfile('.', 'analyses', 'secondlevel', 'onesample_t-tests', '02_glm1_VAL_RPE_oneMod_fullSample_learningRate', filesep);
else ~wholeSample
    stats = fullfile('.', 'analyses', 'secondlevel', 'onesample_t-tests', '02_glm1_VAL_RPE_oneMod_fullSample_learningRate_diagExcl', filesep);
end

path_mask='';

%% Define subjects
d=dir([dataPath 'CBC*']);
subject_directory=d([d().isdir]==1);
subjectALL={subject_directory.name};

% Exclude subjects with a diagnosis if wholeSample is false
if ~wholeSample
    subsDiagnosed = {'CBC_1051', 'CBC_1108', 'CBC_1003', 'CBC_1014', ...
        'CBC_1019', 'CBC_1072', 'CBC_1090', 'CBC_1093', 'CBC_1119', 'CBC_1146'};
    subject = setdiff(subjectALL, subsDiagnosed);
else
    subject = subjectALL;
end

% Read subject demographic data from CSV file
sublist = readtable(fullfile('.', 'data', 'outputs', 'outputfMRIv3AVeTV.csv'), ...
    'VariableNamingRule','preserve', 'Delimiter',',');

% Convert handedness to numeric format
sublist.handedness = ones(height(sublist),1);
sublist.handedness(sublist.EHI_handedness == "left") = -1;
sublist.handedness(sublist.EHI_handedness == "ambidexterous") = 0;
sublist = sublist(ismember(sublist.ID, subject),:);

% Extract covariate information
coVariables = unique(sublist(:,{'ID', 'geschlecht', 'age', 'handedness'}));
coVariables.gender(ismember(coVariables.geschlecht, 'f')) = 0;
coVariables.gender(ismember(coVariables.geschlecht, 'm')) = 1;

% Ensure subject list matches covariate data
if ~isequal(coVariables.ID, subject')
    error('coVariables and subject are not in the same order')
end

% Assign covariate values
ageValues = coVariables.age; % age in years
genderValues = coVariables.gender; % 0 = f, 1 = m
handednessValues = coVariables.handedness; % r = 1, a = 0, l = -1

%% Extract learning rates from modeling summary
% Read modeling summary and extract learning rate (alpha) values
modelingSummary = readtable(fullfile('.', 'data', 'outputs', 'modelingSummary.csv'));
modelingSummary = modelingSummary(ismember(modelingSummary.ID, subject),:);
modelingSummary = sortrows(modelingSummary, {'ID', 'session'});

if ~isequal(unique(modelingSummary.ID), subject')
    error('coVariables and subject are not in the same order')
end

% Extract learning rates for AV and TV conditions
alphasAV = [];
alphasTV = [];

for i=1:length(subject)
    clear temp
    temp = table2array(modelingSummary(strcmp(modelingSummary.ID,subject{i}) & strcmp(modelingSummary.mod2Type,"aud"),'fit_alpha1'));
    for MSI = 1:height(temp)
        alphasAV(MSI,i) = temp(MSI);
    end

    temp = table2array(modelingSummary(strcmp(modelingSummary.ID,subject{i}) & strcmp(modelingSummary.mod2Type,"tac"),'fit_alpha1'));
    for MSI = 1:height(temp)
        alphasTV(MSI,i) = temp(MSI);
    end
end

minAV = min(modelingSummary.fit_alpha1(strcmp(modelingSummary.mod2Type,"aud")));
minTV = min(modelingSummary.fit_alpha1(strcmp(modelingSummary.mod2Type,"tac")));
alphasAV(alphasAV < minAV) = NaN; % change all 0 to NaN below minimum of alpha in AV
alphasTV(alphasTV < minTV) = NaN; % change all 0 to NaN below minimum of alpha in TV

%['01 All Stim'] ['02 All VAL'] ['03 All FB'] ['04 All RPE']
%['05 AV Stim'] ['06 AV VAL'] ['07 AV FB'] ['08 AV RPE']
%['09 TV Stim'] ['10 TV VAL'] ['11 TV FB'] ['12 TV RPE']
%['13 AV vs TV Stim'] ['14 AV vs TV VAL'] ['15 AV vs TV FB'] ['16 AV vs TV RPE']
%['17 TV vs AV Stim'] ['18 TV vs AV VAL'] ['19 TV vs AV FB'] ['20 TV vs AV RPE']
alphas = double(nan(length(subject),20));
alphas(:,1) = mean([alphasAV; alphasTV],1, 'omitnan')'; % mean over all runs for All stimulus
alphas(:,3) = mean([alphasAV; alphasTV],1, 'omitnan')'; % mean over all runs for All FB
alphas(:,5) = mean(alphasAV,1, 'omitnan')'; % mean from AV for AV stim
alphas(:,7) = mean(alphasAV,1, 'omitnan')'; % mean from AV for AV FB
alphas(:,9) = mean(alphasTV,1, 'omitnan')'; % mean from AV for AV stim
alphas(:,11) = mean(alphasTV,1, 'omitnan')'; % mean from AV for AV FB


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
    ['02 All VAL']
    ['03 All FB']
    ['04 All RPE']
    ['05 AV Stim']
    ['06 AV VAL']
    ['07 AV FB']
    ['08 AV RPE']
    ['09 TV Stim']
    ['10 TV VAL']
    ['11 TV FB']
    ['12 TV RPE']
    ['13 AV vs TV Stim']
    ['14 AV vs TV VAL']
    ['15 AV vs TV FB']
    ['16 AV vs TV RPE']
    ['17 TV vs AV Stim']
    ['18 TV vs AV VAL']
    ['19 TV vs AV FB']
    ['20 TV vs AV RPE']
    };

%% Specify second-level design for each contrast
for j=1:nCons
    % Collect contrast files for each subject
    for i=1:length(subject)
        file_con = [dataPath subject{i} '/' Cons{j}];
        con(i,1)={file_con};
    end

    % Define output directory
    statsDir = [ConDir ConFolder{j}];
    if ~isfolder(statsDir)
        mkdir(statsDir);
    end

     % Specify factorial design in SPM batch
    matlabbatch{j}.spm.stats.factorial_design.dir = {statsDir}; %directory
    matlabbatch{j}.spm.stats.factorial_design.des.t1.scans = con(:,1); %all con file found above

    %add co-variables
    k = 1;
    if includeAge
        disp('hey')
        matlabbatch{j}.spm.stats.factorial_design.cov(k).c = ageValues;
        matlabbatch{j}.spm.stats.factorial_design.cov(k).cname = 'age';
        matlabbatch{j}.spm.stats.factorial_design.cov(k).iCFI = 1;
        matlabbatch{j}.spm.stats.factorial_design.cov(k).iCC = 1;
        k = k+1;
    end

    if ~isnan(mean(alphas(:,j)))
        disp('Ho')
        matlabbatch{j}.spm.stats.factorial_design.cov(k).c = alphas(:,j);
        matlabbatch{j}.spm.stats.factorial_design.cov(k).cname = 'learningRate';
        matlabbatch{j}.spm.stats.factorial_design.cov(k).iCFI = 1;
        matlabbatch{j}.spm.stats.factorial_design.cov(k).iCC = 1;
        k = k+1;
    end

    matlabbatch{j}.spm.stats.factorial_design.cov(k).c = handednessValues;
    matlabbatch{j}.spm.stats.factorial_design.cov(k).cname = 'handedness';
    matlabbatch{j}.spm.stats.factorial_design.cov(k).iCFI = 1;
    matlabbatch{j}.spm.stats.factorial_design.cov(k).iCC = 1;
    k = k+1;

    matlabbatch{j}.spm.stats.factorial_design.cov(k).c = genderValues;
    matlabbatch{j}.spm.stats.factorial_design.cov(k).cname = 'gender';
    matlabbatch{j}.spm.stats.factorial_design.cov(k).iCFI = 1;
    matlabbatch{j}.spm.stats.factorial_design.cov(k).iCC = 1;
    k = k+1;

    matlabbatch{j}.spm.stats.factorial_design.masking.tm.tm_none = 1;
    matlabbatch{j}.spm.stats.factorial_design.masking.im = 1;
    matlabbatch{j}.spm.stats.factorial_design.masking.em = {path_mask};
    matlabbatch{j}.spm.stats.factorial_design.globalc.g_omit = 1;
    matlabbatch{j}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
    matlabbatch{j}.spm.stats.factorial_design.globalm.glonorm = 1;

    %run the batch
    spm_jobman('run', matlabbatch(j));

end



%% SecondLevel-Estimation

clear matlabbatch;
%find content of directory
ContDir = dir(ConDir);

%select names of directories
%position 1 and 2 are left out, because they do not contain folder names
Folder = {ContDir(3:end,1).name};

%count selected folders
nFolder =numel(Folder);


for j=1:nFolder
    %define directory where SPM file is stored
    statsDir = [ConDir Folder{j}];

    matlabbatch{j}.spm.stats.fmri_est.spmmat = {[statsDir,'/SPM.mat']}; %Directory
    matlabbatch{j}.spm.stats.fmri_est.write_residuals = 0;
    matlabbatch{j}.spm.stats.fmri_est.method.Classical = 1;

    %run the batch
    spm_jobman('run', matlabbatch(j));
end


%% Specify 2nd level - Calculation
clear matlabbatch;
for j=1:nFolder
    %define directory where estimated SPM file is stored
    statsDir = [ConDir Folder{j}];

    %define name of contrast
    %in this case the contrast is named after the directory
    name = Folder{j};

    matlabbatch{j}.spm.stats.con.spmmat = {[statsDir,'/SPM.mat']};  %select estimated SPM file

    %contrasts
    k = 1;
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.name = name;
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.weights = [1 0];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.sessrep = 'none';
    k = k + 1;

    matlabbatch{j}.spm.stats.con.consess{k}.tcon.name = ['-' name];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.weights = [-1 0];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.sessrep = 'none';
    k = k + 1;

    matlabbatch{j}.spm.stats.con.consess{k}.tcon.name = [name ' age'];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.weights = [0 1];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.sessrep = 'none';
    k = k + 1;

    matlabbatch{j}.spm.stats.con.consess{k}.tcon.name = [name ' -age'];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.weights = [0 -1];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.sessrep = 'none';
    k = k + 1;

    matlabbatch{j}.spm.stats.con.consess{k}.tcon.name = [name ' + age'];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.weights = [1 1];
    matlabbatch{j}.spm.stats.con.consess{k}.tcon.sessrep = 'none';
    k = k + 1;

    if ismember(j, [1, 3, 5, 7, 9, 11])
        matlabbatch{j}.spm.stats.con.consess{k}.tcon.name = [name ' learningRate'];
        matlabbatch{j}.spm.stats.con.consess{k}.tcon.weights = [0 0 1];
        matlabbatch{j}.spm.stats.con.consess{k}.tcon.sessrep = 'none';
        k = k + 1;

        matlabbatch{j}.spm.stats.con.consess{k}.tcon.name = [name ' -learningRate'];
        matlabbatch{j}.spm.stats.con.consess{k}.tcon.weights = [0 0 -1];
        matlabbatch{j}.spm.stats.con.consess{k}.tcon.sessrep = 'none';
        k = k + 1;
    end

    matlabbatch{j}.spm.stats.con.delete = 1;

    %run the batch
    spm_jobman('run', matlabbatch(j));
end

