function options = CBCsetOptions()
% input:    nothing
% output:   options with paths, subject IDs, filenames, trialNr, ...

% set general paths
options.path = fullfile('.', 'scripts', 'modelling', 'match recognition - adults', filesep);
cd(options.path)
rmpath(genpath(options.path)); % removes old paths
addpath(genpath(options.path));
options.designPath = fullfile(options.path, 'design');

% set paths to trial structure files
options.designFileVis = fullfile(options.designPath, 'counterbalancedvisual.mat'); 
options.designFileMod2 = fullfile(options.designPath, 'counterbalancedauditory.mat');
options.designFile = fullfile(options.designPath, 'trialStructures.mat');

% set general information
options.simSubjectIDs = cellstr(num2str((1001:1010)'));
options.subjectIDs = {'P003', 'P004', 'P005', 'P006', 'P007', 'P008', 'P009', ...
    'P010', 'P011', 'P012', 'P013', 'P014', 'P015', 'P016', 'P017', 'P018', ...
    'P019', 'P020', 'P021', 'P022', 'P025', 'P026', 'P027', 'P028', 'P029', ...
    'P030', 'P031', 'P032', 'P034', 'P035', 'P036', 'P037', 'P038', 'P039', ...
    'P040', 'P041'};

options.trialNr = 42;

files = dir(fullfile('.', 'scripts', 'modelling', 'match recognition - adults', 'data', '*.csv'));
options.files = {files.name};

options.delimiter = ',';
options.decimalSeparator = '.';

rng(1)
end