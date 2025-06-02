function options = CBCsetOptions()
% input:    nothing
% output:   options with paths, subject IDs, filenames, trialNr, ...

% set general paths
options.path = fullfile('.', 'scripts', 'modelling', 'discriminative choice', filesep);
cd(options.path)
rmpath(genpath(options.path)); % removes old paths
addpath(genpath(options.path));
options.designPath = fullfile(options.path, 'design');

% set paths to trial structure files
options.designFile = fullfile(options.designPath, 'trialStructures.mat'); 

% set general information
options.simSubjectIDs = cellstr(num2str((101:200)'));
options.subjectIDs = {'1001', '1002', '1003', '1005', '1006', '1007', '1013', ...
    '1014', '1015', '1016', '1017', '1019', '1020', '1024', '1027', '1029', ...
    '1033', '1036', '1037', '1040', '1051', '1058', '1072', '1075', '1102', ...
    '1109', '1142', '1143', '1144'};

options.trialNr = 44;

files = dir(fullfile('.', 'scripts', 'modelling', 'discriminative choice', 'data', '*.csv'));
options.files = {files.name};

options.delimiter = ',';
options.decimalSeparator = '.';

rng(11)
end