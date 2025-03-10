% script for 2nd level analysis of MSI Task
% ANCOVA  to investigate main effects of age and interaction between age
% and modality ond stimulus and feedback processing
% NR, September 2024

clear
clc

% Option to analyze all subjects or exclude those with a diagnosis
wholeSample = true; % Set to false to exclude diagnosed subjects

%% Set dynamic base paths
% The script identifies and sets the base directory paths for various data and scripts.
% Ensure that 'data', 'material', and 'script' folders are present relative to the base path.
filePath = fileparts(matlab.desktop.editor.getActiveFilename); % Get the current script's path
cd(filePath) 
cd(fullfile('..', '..', '..')) % Navigate up three directories to the base directory
basePath = pwd; % Store the base path


%% define paths
dataPath = fullfile('.', 'analyses', 'firstlevel', '1st_02_glm1_VAL_RPE_oneMod', filesep);

% Path to store second-level analysis results
if wholeSample
    savePath = fullfile('.', 'analyses', 'secondlevel', 'ancova', 'anova_no_colin_fullSample_MainIntEFF', filesep);
elseif ~wholeSample
    savePath = fullfile('.', 'analyses', 'secondlevel', 'ancova', 'anova_no_colin_fullSample_MainIntEFF_diagExcl', filesep);
end

pathMask='';

%% Define subjects
% Retrieve subjects from firstlevel folder
d = dir([dataPath 'CBC_*']);
subjectsALL = {d.name};

% Exclude subjects with a diagnosis if wholeSample is false
if ~wholeSample
    subsDiagnosed = {'CBC_1051', 'CBC_1108', 'CBC_1003', 'CBC_1014', ...
        'CBC_1019', 'CBC_1072', 'CBC_1090', 'CBC_1093', 'CBC_1119', 'CBC_1146'};
    subjects = setdiff(subjectsALL, subsDiagnosed);
else
    subjects = subjectALL;
end

% Read subject demographic data from CSV file
sublist = readtable(fullfile('.', 'data', 'outputs', 'outputfMRIv3AVeTV.csv'), ...
    'VariableNamingRule','preserve', 'Delimiter',',');

% Convert handedness to numeric format
sublist.handedness = ones(height(sublist),1);
sublist.handedness(sublist.EHI_handedness == "left") = -1;
sublist.handedness(sublist.EHI_handedness == "ambidexterous") = 0;
sublist = sublist(ismember(sublist.ID, subjects),:);

% Extract covariate information
coVariables = unique(sublist(:,{'ID', 'geschlecht', 'age', 'handedness'}));
coVariables.gender(ismember(coVariables.geschlecht, 'f')) = 0;
coVariables.gender(ismember(coVariables.geschlecht, 'm')) = 1;

% Ensure subject list matches covariate data
if ~isequal(coVariables.ID, subjects')
    error('Covariables and subjects not identical')
end

% Assign covariate values
ageValues = coVariables.age; % age in years
genderValues = coVariables.gender; % 0 = f, 1 = m
handednessValues = coVariables.handedness; % r = 1, a = 0, l = -1

effects = {'Stim', 'FB', 'PE', 'VAL'};
cons.AV = {'con_0005.nii', 'con_0007.nii', 'con_0008.nii', 'con_0006.nii',};
cons.TV = {'con_0009.nii', 'con_0011.nii', 'con_0012.nii', 'con_0010.nii',};

%% Loop through effects and set up factorial design in SPM
for j = 1:length(effects)
    effects{j}
    clear matlabbatch
    
    % Create result directory if it does not exist
    if ~isfolder([savePath effects{j}])
        mkdir([savePath effects{j}])
    end
    
    % Define SPM batch for factorial design
    matlabbatch{1}.spm.stats.factorial_design.dir = {[savePath effects{j}]};
    
    % Collect subject contrast files
    scans = {};
    for i = 1:length(subjects)
        currScans = {
            fullfile(dataPath, subjects{i}, cons.AV{j})
            fullfile(dataPath, subjects{i}, cons.TV{j})
            };
        scans = [scans; currScans];
    end
        
    matlabbatch{1}.spm.stats.factorial_design.des.anova.icell.scans = scans;
    matlabbatch{1}.spm.stats.factorial_design.des.anova.dept = 0;
    matlabbatch{1}.spm.stats.factorial_design.des.anova.variance = 1;
    matlabbatch{1}.spm.stats.factorial_design.des.anova.gmsca = 0;
    matlabbatch{1}.spm.stats.factorial_design.des.anova.ancova = 0;
    
    % Define covariates (condition, age, interaction, gender, handedness)
    matlabbatch{1}.spm.stats.factorial_design.cov(1).c = repmat([1 -1], 1, length(subjects)); % 1: AV, -1: TV
    matlabbatch{1}.spm.stats.factorial_design.cov(1).cname = 'Cond';
    matlabbatch{1}.spm.stats.factorial_design.cov(1).iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(1).iCC = 1;
    
    matlabbatch{1}.spm.stats.factorial_design.cov(2).c = repelem(ageValues, 2);
    matlabbatch{1}.spm.stats.factorial_design.cov(2).cname = 'Age';
    matlabbatch{1}.spm.stats.factorial_design.cov(2).iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(2).iCC = 5;
    
    % add interaction with age and modality: AV*Condition
    matlabbatch{1}.spm.stats.factorial_design.cov(3).c = repelem(ageValues, 2) .* repmat([1 -1], 1, length(subjects))';
    matlabbatch{1}.spm.stats.factorial_design.cov(3).cname = 'Cond x Age';
    matlabbatch{1}.spm.stats.factorial_design.cov(3).iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(3).iCC = 1;
    
    matlabbatch{1}.spm.stats.factorial_design.cov(4).c = repelem(genderValues,2);
    matlabbatch{1}.spm.stats.factorial_design.cov(4).cname = 'gender';
    matlabbatch{1}.spm.stats.factorial_design.cov(4).iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(4).iCC = 5;
    
    matlabbatch{1}.spm.stats.factorial_design.cov(5).c = repelem(handednessValues,2);
    matlabbatch{1}.spm.stats.factorial_design.cov(5).cname = 'handedness';
    matlabbatch{1}.spm.stats.factorial_design.cov(5).iCFI = 1;
    matlabbatch{1}.spm.stats.factorial_design.cov(5).iCC = 5;
    
    for i = 1:length(subjects)-2
        covVector = zeros(2*length(subjects), 1);
        covVector(2*i-1:2*i) = 1;
        matlabbatch{1}.spm.stats.factorial_design.cov(i+5).c = covVector;
        matlabbatch{1}.spm.stats.factorial_design.cov(i+5).cname = ['subj' num2str(i)];
        matlabbatch{1}.spm.stats.factorial_design.cov(i+5).iCFI = 1;
        matlabbatch{1}.spm.stats.factorial_design.cov(i+5).iCC = 1;
    end
    
    matlabbatch{1}.spm.stats.factorial_design.multi_cov = struct('files', {}, 'iCFI', {}, 'iCC', {});
    matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
    matlabbatch{1}.spm.stats.factorial_design.masking.em = {pathMask};
    matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
    matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;
    
    matlabbatch{2}.spm.stats.fmri_est.spmmat(1) = cfg_dep('Factorial design specification: SPM.mat File', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
    matlabbatch{2}.spm.stats.fmri_est.write_residuals = 0;
    matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;
    
    % define contrasts
    matlabbatch{3}.spm.stats.con.spmmat(1) = cfg_dep('Model estimation: SPM.mat File', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','spmmat'));
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'main effect of condition';
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.weights = [0 1];
    matlabbatch{3}.spm.stats.con.consess{1}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{2}.tcon.name = '-main effect of condition';
    matlabbatch{3}.spm.stats.con.consess{2}.tcon.weights = [0 -1];
    matlabbatch{3}.spm.stats.con.consess{2}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{3}.tcon.name = 'main effect of age';
    matlabbatch{3}.spm.stats.con.consess{3}.tcon.weights = [0 0 1];
    matlabbatch{3}.spm.stats.con.consess{3}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{4}.tcon.name = '-main effect of age';
    matlabbatch{3}.spm.stats.con.consess{4}.tcon.weights = [0 0 -1];
    matlabbatch{3}.spm.stats.con.consess{4}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{5}.tcon.name = 'interaction AV > TV';
    matlabbatch{3}.spm.stats.con.consess{5}.tcon.weights = [0 0 0 1];
    matlabbatch{3}.spm.stats.con.consess{5}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.consess{6}.tcon.name = 'interaction TV > AV';
    matlabbatch{3}.spm.stats.con.consess{6}.tcon.weights = [0 0 0 -1];
    matlabbatch{3}.spm.stats.con.consess{6}.tcon.sessrep = 'none';
    matlabbatch{3}.spm.stats.con.delete = 0;
    
    % Run SPM job manager
    spm_jobman('run', matlabbatch);
end
    