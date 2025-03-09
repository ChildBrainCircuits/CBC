function copy_files_for_preproc(subjects, sourcePath, savePath, group)
% copy_files_for_preproc copies the required MSI data for preprocessing from the source
% directory to the save directory for each subject.
% Input:
% - sourcePath: The path to the directory containing the source data
% - savePath: The path to save the copied data
% - subjects: A cell array containing subject IDs
% - group: The group of subjects ('children' or 'adults')
%
% Example usage:
% subjects = {'CBC_1001', 'CBC_1002'};
% copy_files_for_preproc('path/to/raw/data/', 'local/path/for/preprocessing/', subjects, 'children');

%% Define file paths
% The following variables specify the file paths for different types of data
epiFile = fullfile('func', '*_epi_msi_*'); % fMRI MSI data files
b0File = fullfile('fmap', '*b0_msi*'); % B0 fieldmap data
if isequal(group, 'children')
    logFile = fullfile('beh', '*_MSI_*'); % Behavior log files for children
elseif isequal(group, 'adults')
    logFile = fullfile('beh', '*_prob*'); % Behavior log files for adults
end
anatFile = fullfile('anat', '*t1*'); % Anatomical T1-weighted images
b0TableFile = fullfile('epis*b0.xlsx'); % B0 table (for fieldmap analysis)


%% Loop through each subject
for i=1:length(subjects)
    % Check if the subject directory already exists in the savePath
    % If it does not exist, create the directory for the subject
    if isfolder(fullfile(savePath, subjects{i}))
        continue % Skip this subject if the folder already exists
    else
        mkdir(fullfile(savePath, subjects{i})) % Create the folder for the subject
    end
    disp(subjects{i}) % Display the current subject being processed

    % Copy the EPI MSI files to the functional folder
    copyfile(fullfile(sourcePath, subjects{i}, epiFile), fullfile(savePath, subjects{i}, 'func', filesep));

    % Copy the B0 MSI files to the fmap folder
    copyfile(fullfile(sourcePath, subjects{i}, b0File), fullfile(savePath, subjects{i}, 'fmap', filesep));

    % Copy the behavior log files based on the group type (children/adults)
    copyfile(fullfile(sourcePath, subjects{i}, logFile), fullfile(savePath, subjects{i}, 'beh', filesep));

    % Copy the T1-weighted anatomical files to the anat folder
    % Check if more than one T1 image is found for the subject, and issue a warning
    if length(dir(fullfile(sourcePath, subjects{i}, anatFile)))/3 > 1
        warning(['More than 1 T1 for ' subjects{i}]) % Warning if multiple T1 images are found
    end
    copyfile(fullfile(sourcePath, subjects{i}, anatFile), fullfile(savePath, subjects{i}, 'anat', filesep));

    % Copy the B0 table if it exists
    if ~isempty(dir(fullfile(sourcePath, subjects{i}, b0TableFile)))
        copyfile(fullfile(sourcePath, subjects{i}, b0TableFile), fullfile(savePath, subjects{i}));
    else
         warning(['No B0 Table for ' subjects{i}]) % Warning if multiple T1 images are found
    end

end

