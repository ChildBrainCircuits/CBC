function options = CBCsetOptions()
% input:    nothing
% output:   options with paths, subject IDs, filenames, trialNr, ...

% set general paths

options.path = fullfile('.', 'scripts', 'modelling', filesep);
cd(options.path)
rmpath(genpath(options.path)); % removes old paths
addpath(genpath(options.path));
options.designPath = fullfile(options.path, 'design');

% set paths to trial structure files
options.designFile = fullfile(options.designPath, 'trialStructures.mat'); 

% set general information
options.simSubjectIDs = cellstr(num2str((101:200)'));
options.subjectIDs = {'1001', '1002', '1003', '1005', ...
    '1006', '1007', '1010', '1011', '1013', '1014', ...
    '1015', '1016', '1017', '1018', '1019', '1020', ...
    '1021', '1022', '1024', '1026', '1027', '1028', ...
    '1029', '1030', '1031', '1033', '1034', '1036', ...
    '1037', '1038', '1040', '1044', '1045', '1048', ...
    '1051', '1054', '1058', '1059', '1064', '1071', ...
    '1072', '1075', '1076', '1087', '1090', '1093', ...
    '1102', '1108', '1109', '1111', '1117', '1119', ...
    '1127', '1139', '1035', '1041', '1042', '1110', ...
    '1112', '1114', '1115', '1122', '1129', '1136', ...
    '1138', '1143', '1146'};

options.trialNr = 44;

files = dir(fullfile('.', 'scripts', 'modelling', 'data', '*.csv'));
options.files = {files.name};

options.delimiter = ',';
options.decimalSeparator = '.';

rng(1)
end