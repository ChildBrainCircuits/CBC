%% extract beta values using marsbar
% Nina Raduner, September 2024

clear
clc

%% set dynamic base paths
% make sure that the 'data', 'material', and 'script' folder are present in
% the base path
filePath = fileparts(matlab.desktop.editor.getActiveFilename);
cd(filePath)
cd(fullfile('..', '..', '..'))
basePath = pwd;

%% add spm path
% set to your local marsbar folder
addpath(genpath('C:/Users/nradun/Documents/MATLAB/marsbar-0.45/'))

%%
dataPath = fullfile('.', 'data', 'firstlevel', '1st_02_glm1_VAL_RPE_oneMod', filesep);
roiPath = fullfile('.', 'material', 'ROI masks', filesep);
savePath = fullfile('.', 'analyses', 'ROI_results', filesep);

cd(basePath)

if ~isfolder(savePath)
    mkdir(savePath)
end

d = dir([dataPath 'CBC*']);
sub = {d.name};

d = dir([roiPath '*mask*']);
maskFolders = {d.name};


%% MS_uniform
i = 1;

currentMask = maskFolders{i}; 
currentFolder = fullfile('.', roiPath, currentMask, filesep);
d = dir([currentFolder '*.csv']);
currentTable = readtable([currentFolder d.name]);
currentTable = currentTable(currentTable.size >= 25,:);
currentTable = sortrows(currentTable, 'number');

for j = 1:max(currentTable.number)
    currentTable.file{j}
    roi_file = fullfile('.', roiPath, currentMask, 'subcluster', [currentTable.file{j}, '_roi.mat']);
    
    % Make marsbar ROI object
    R  = maroi(roi_file);
    
    meanBetas = table();
    AVtable = table();
    TVtable = table();
    
    for k = 1:length(sub)
        sub{k}
        spm_name = fullfile(filesep, dataPath, sub{k}, filesep, 'SPM.mat');
        
        % Make marsbar design object
        D  = mardo(spm_name);
        % Fetch data into marsbar data object
        Y  = get_marsy(R, D, 'mean');
        % Get contrasts from original design
        xCon = get_contrasts(D);
        %stimAV = find(xCon(5).c);
        %stimTV = find(xCon(9).c);
        % Estimate design on ROI data
        E = estimate(D, Y);
        % Put contrasts from original design back into design object
        E = set_contrasts(E, xCon);
        % get design betas
        %b = betas(E);
        %get stats and stuff for all contrasts into statistics structure
        marsS = compute_contrasts(E, 1:length(xCon));
        
        AVtable.ID(k) = sub(k);
        AVtable.mask(k) = {currentMask};
        AVtable.file(k) = currentTable.file(currentTable.number == j);
        AVtable.task(k) = {'AV'};
        AVtable.hemisphere(k) = currentTable.hemisphere(currentTable.number == j);
        AVtable.label(k) = currentTable.label(currentTable.number == j);
        AVtable.number(k) = currentTable.number(currentTable.number == j);
        
        AVtable.beta(k) = marsS.con(5); % 5th contrast is AV stimulus
        
        TVtable.ID(k) = sub(k);
        TVtable.mask(k) = {currentMask};
        TVtable.file(k) = currentTable.file(currentTable.number == j);
        TVtable.task(k) = {'TV'};
        TVtable.hemisphere(k) = currentTable.hemisphere(currentTable.number == j);
        TVtable.label(k) = currentTable.label(currentTable.number == j);
        TVtable.number(k) = currentTable.number(currentTable.number == j);
        
        TVtable.beta(k) = marsS.con(9); % 9th contrast is TV stimulus

    end
    
    meanBetas = [AVtable; TVtable];
    
    currentCluster = strrep([num2str(j) '_' currentTable.label{currentTable.number == j} '_' currentTable.hemisphere{currentTable.number == j}],' ', '_');
    writetable(meanBetas, fullfile(savePath, ['MSTACT_uniform_' currentCluster '.csv']));
end

%% repeat for the whole MSI network mask
overallMean = table();
AVtable = table();
TVtable = table();

roi_file = fullfile('.', roiPath, currentMask, 'MS_TACT_bin_1_roi.mat');

% Make marsbar ROI object
R  = maroi(roi_file);

for k = 1:length(sub)
    disp('MSwholebrain')
    sub{k}
    spm_name = fullfile(filesep, dataPath, sub{k}, filesep, 'SPM.mat');

    % Make marsbar design object
    D  = mardo(spm_name);
    % Fetch data into marsbar data object
    Y  = get_marsy(R, D, 'mean');
    % Get contrasts from original design
    xCon = get_contrasts(D);
    %stimAV = find(xCon(5).c);
    %stimTV = find(xCon(9).c);
    % Estimate design on ROI data
    E = estimate(D, Y);
    % Put contrasts from original design back into design object
    E = set_contrasts(E, xCon);
    % get design betas
    %b = betas(E);
    %get stats and stuff for all contrasts into statistics structure
    marsS = compute_contrasts(E, 1:length(xCon));
    
    AVtable.ID(k) = sub(k);
    AVtable.mask(k) = maskFolders(i);
    AVtable.task(k) = {'AV'};
    AVtable.hemisphere(k) = {'wholeBrain'};
    
    AVtable.beta(k) = marsS.con(5); % 5th contrast is AV stimulus
    
    TVtable.ID(k) = sub(k);
    TVtable.mask(k) = maskFolders(i);
    TVtable.task(k) = {'TV'};
    TVtable.hemisphere(k) = {'wholeBrain'};
    
    TVtable.beta(k) = marsS.con(9); % 9th contrast is TV stimulus
    
end

overallMean = [AVtable; TVtable];

writetable(overallMean, fullfile(savePath, 'MSTACT_uniform_meanBeta.csv'));

%% PE_uniform
i = 2;

currentMask = maskFolders{i}; 
currentFolder = fullfile('.', roiPath, currentMask, filesep);
d = dir([currentFolder '*.csv']);
currentTable = readtable([currentFolder d.name]);
currentTable = currentTable(currentTable.size >= 25,:);
currentTable = sortrows(currentTable, 'number');

for j = 1:max(currentTable.number)
    currentTable.file{j}
    roi_file = fullfile('.', roiPath, currentMask, 'subcluster', currentTable.file{j});
    
    % Make marsbar ROI object
    R  = maroi(roi_file);
    
    meanBetas = table();
    AVtable = table();
    TVtable = table();
    
    for k = 1:length(sub)
        sub{k}
        spm_name = fullfile(filesep, dataPath, sub{k}, filesep, 'SPM.mat');
        
        % Make marsbar design object
        D  = mardo(spm_name);
        % Fetch data into marsbar data object
        Y  = get_marsy(R, D, 'mean');
        % Get contrasts from original design
        xCon = get_contrasts(D);
        %rpeAV = find(xCon(8).c);
        %rpeTV = find(xCon(12).c);
        % Estimate design on ROI data
        E = estimate(D, Y);
        % Put contrasts from original design back into design object
        E = set_contrasts(E, xCon);
        % get design betas
        %b = betas(E);
        %get stats and stuff for all contrasts into statistics structure
        marsS = compute_contrasts(E, 1:length(xCon));
        
        AVtable.ID(k) = sub(k);
        AVtable.mask(k) = {currentMask};
        AVtable.file(k) = currentTable.file(currentTable.number == j);
        AVtable.task(k) = {'AV'};
        AVtable.hemisphere(k) = currentTable.hemisphere(currentTable.number == j);
        AVtable.label(k) = currentTable.label(currentTable.number == j);
        AVtable.number(k) = currentTable.number(currentTable.number == j);
        
        AVtable.beta(k) = marsS.con(8); % 8th contrast is AV RPE
        
        TVtable.ID(k) = sub(k);
        TVtable.mask(k) = {currentMask};
        TVtable.file(k) = currentTable.file(currentTable.number == j);
        TVtable.task(k) = {'TV'};
        TVtable.hemisphere(k) = currentTable.hemisphere(currentTable.number == j);
        TVtable.label(k) = currentTable.label(currentTable.number == j);
        TVtable.number(k) = currentTable.number(currentTable.number == j);
        
        TVtable.beta(k) = marsS.con(12); % 12th contrast is TV RPE

    end
    
    meanBetas = [AVtable; TVtable];
    
    currentCluster = strrep([num2str(j) '_' currentTable.label{currentTable.number == j} '_' currentTable.hemisphere{currentTable.number == j}],' ', '_')
    writetable(meanBetas, fullfile(savePath, ['PE_uniform_' currentCluster '.csv']));
end

%% repeat for the whole RPE network mask
overallMean = table();
AVtable = table();
TVtable = table();

roi_file = fullfile('.', roiPath, currentMask, 'PE_bin_1_roi.mat');

% Make marsbar ROI object
R  = maroi(roi_file);

for k = 1:length(sub)
    disp('PEwholebrain')
    sub{k}
    spm_name = fullfile(filesep, dataPath, sub{k}, filesep, 'SPM.mat');
    
    % Make marsbar design object
    D  = mardo(spm_name);
    % Fetch data into marsbar data object
    Y  = get_marsy(R, D, 'mean');
    % Get contrasts from original design
    xCon = get_contrasts(D);
    %rpeAV = find(xCon(8).c);
    %rpeTV = find(xCon(12).c);
    % Estimate design on ROI data
    E = estimate(D, Y);
    % Put contrasts from original design back into design object
    E = set_contrasts(E, xCon);
    % get design betas
    %b = betas(E);
    %get stats and stuff for all contrasts into statistics structure
    marsS = compute_contrasts(E, 1:length(xCon));
    
    AVtable.ID(k) = sub(k);
    AVtable.mask(k) = maskFolders(i);
    AVtable.task(k) = {'AV'};
    AVtable.hemisphere(k) = {'wholeBrain'};
    
    AVtable.beta(k) = marsS.con(8); % 8th contrast is AV RPE
    
    TVtable.ID(k) = sub(k);
    TVtable.mask(k) = maskFolders(i);
    TVtable.task(k) = {'TV'};
    TVtable.hemisphere(k) = {'wholeBrain'};
    
    TVtable.beta(k) = marsS.con(12); % 12th contrast is TV RPE
    
end

overallMean = [AVtable; TVtable];

writetable(overallMean, fullfile(savePath, 'PE_uniform_meanBeta.csv'));

%% VAL_uniform
i = 3;

currentMask = maskFolders{i}; 
currentFolder = fullfile('.', roiPath, currentMask, filesep);
d = dir([currentFolder '*.csv']);
currentTable = readtable([currentFolder d.name]);
currentTable = currentTable(currentTable.size >= 25,:);
currentTable = sortrows(currentTable, 'number');

for j = 1:max(currentTable.number)
    currentTable.file{j}
    roi_file = fullfile('.', roiPath, currentMask, 'subcluster', currentTable.file{j});
    
    % Make marsbar ROI object
    R  = maroi(roi_file);
    
    meanBetas = table();
    AVtable = table();
    TVtable = table();
    
    for k = 1:length(sub)
        sub{k}
        spm_name = fullfile(filesep, dataPath, sub{k}, filesep, 'SPM.mat');
        
        % Make marsbar design object
        D  = mardo(spm_name);
        % Fetch data into marsbar data object
        Y  = get_marsy(R, D, 'mean');
        % Get contrasts from original design
        xCon = get_contrasts(D);
        %valAV = find(xCon(6).c);
        %valTV = find(xCon(10).c);
        % Estimate design on ROI data
        E = estimate(D, Y);
        % Put contrasts from original design back into design object
        E = set_contrasts(E, xCon);
        % get design betas
        %b = betas(E);
        %get stats and stuff for all contrasts into statistics structure
        marsS = compute_contrasts(E, 1:length(xCon));
        
        AVtable.ID(k) = sub(k);
        AVtable.mask(k) = {currentMask};
        AVtable.file(k) = currentTable.file(currentTable.number == j);
        AVtable.task(k) = {'AV'};
        AVtable.hemisphere(k) = currentTable.hemisphere(currentTable.number == j);
        AVtable.label(k) = currentTable.label(currentTable.number == j);
        AVtable.number(k) = currentTable.number(currentTable.number == j);
        
        AVtable.beta(k) = marsS.con(6); % 6th contrast is AV value
        
        TVtable.ID(k) = sub(k);
        TVtable.mask(k) = {currentMask};
        TVtable.file(k) = currentTable.file(currentTable.number == j);
        TVtable.task(k) = {'TV'};
        TVtable.hemisphere(k) = currentTable.hemisphere(currentTable.number == j);
        TVtable.label(k) = currentTable.label(currentTable.number == j);
        TVtable.number(k) = currentTable.number(currentTable.number == j);
        
        TVtable.beta(k) = marsS.con(10); % 10th contrast is TV value

    end
    
    meanBetas = [AVtable; TVtable];
    
    currentCluster = strrep([num2str(j) '_' currentTable.label{currentTable.number == j} '_' currentTable.hemisphere{currentTable.number == j}],' ', '_');
    writetable(meanBetas, fullfile(savePath, ['VAL_uniform_' currentCluster '.csv']));
end

%% repeat for whole value network mask
overallMean = table();
AVtable = table();
TVtable = table();

roi_file = fullfile('.', roiPath, currentMask, 'VAL_bin_1_roi.mat');

% Make marsbar ROI object
R  = maroi(roi_file);

for k = 1:length(sub)
    disp('VALwholebrain')
    sub{k}
    spm_name = fullfile(filesep, dataPath, sub{k}, filesep, 'SPM.mat');
    
    % Make marsbar design object
    D  = mardo(spm_name);
    % Fetch data into marsbar data object
    Y  = get_marsy(R, D, 'mean');
    % Get contrasts from original design
    xCon = get_contrasts(D);
    %valAV = find(xCon(6).c);
    %valTV = find(xCon(10).c);
    % Estimate design on ROI data
    E = estimate(D, Y);
    % Put contrasts from original design back into design object
    E = set_contrasts(E, xCon);
    % get design betas
    %b = betas(E);
    %get stats and stuff for all contrasts into statistics structure
    marsS = compute_contrasts(E, 1:length(xCon));
    
    AVtable.ID(k) = sub(k);
    AVtable.mask(k) = maskFolders(i);
    AVtable.task(k) = {'AV'};
    AVtable.hemisphere(k) = {'wholeBrain'};
    
    AVtable.beta(k) = marsS.con(6); % 6th contrast is AV value
    
    TVtable.ID(k) = sub(k);
    TVtable.mask(k) = maskFolders(i);
    TVtable.task(k) = {'TV'};
    TVtable.hemisphere(k) = {'wholeBrain'};
    
    TVtable.beta(k) = marsS.con(10); % 10th contrast is TV value
    
end

overallMean = [AVtable; TVtable];

writetable(overallMean, fullfile(savePath, 'VAL_uniform_meanBeta.csv'));

