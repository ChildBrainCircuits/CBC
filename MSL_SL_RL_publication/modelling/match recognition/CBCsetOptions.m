function options = CBCsetOptions()
% input:    nothing
% output:   options with paths, subject IDs, filenames, trialNr, ...

% set general paths
options.path = fullfile('.', 'scripts', 'modelling', 'match recognition', filesep);
cd(options.path)
rmpath(genpath(options.path)); % removes old paths
addpath(genpath(options.path));
options.designPath = fullfile(options.path, 'design');

% set paths to trial structure files
options.designFileVis = fullfile(options.designPath, 'counterbalancedvisual.mat'); 
options.designFileMod2 = fullfile(options.designPath, 'counterbalancedauditory.mat');
options.designFile = fullfile(options.designPath, 'trialStructures.mat');

% set general information
options.simSubjectIDs = cellstr(num2str((100:200)'));
options.subjectIDs = {'1032', '1039', '1046', '1047', '1049', '1055', '1056', ...
    '1057', '1062', '1063', '1065', '1069', '1074', '1077', '1080', '1083', ...
    '1084', '1086', '1089', '1095', '1098', '1101', '1104', '1105', '1106', ...
    '1107', '1116', '1133', '1149'};

options.trialNr = 42;

files = dir(fullfile('.', 'scripts', 'modelling', 'match recognition', 'data', '*.csv'));
options.files = {files.name};

options.delimiter = ',';
options.decimalSeparator = '.';

rng(1)
end