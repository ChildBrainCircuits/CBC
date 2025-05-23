function badscans = AS_create_tables_badScans(dataPath, savePath, files)
% GATHERBADSCANS: Aggregates bad scan information for all subjects.
% NR, Sep 2024
%   badscans = AS_create_tables_badScans(dataPath, savePath, files) reads bad scan data
%   from specified directories and combines the results into a single table.
%   This function looks for bad scan files, reads them, and updates the subject's B0 table accordingly.
%
%   INPUTS:
%   - dataPath: String, the path to the data directory containing subject folders.
%   - savePath: String, the path to the directory where results will be saved.
%   - files: String, the filename pattern used for bad scan files (e.g., 'allBad_v2.txt' or 'inBlock_v2.txt').
%
%   OUTPUTS:
%   - badscans: Table containing aggregated bad scan information for all subjects.

%% Initialize an empty table for storing bad scan data for all subjects
badscans = table();

% Get the list of subjects (directories starting with 'CBC_')
subjects = dir([dataPath 'CBC_*']);
subjects = {subjects.name}; % Extract only the names of the subject directories

%% Loop over each subject to process their data
for i=1:length(subjects)
    % Load the B0 table (which contains the EPI filenames) for the current subject
    B0_table = readtable(fullfile(dataPath, subjects{i}, 'matching_epis_b0.xlsx'));
    epis = table2cell(B0_table(:, ['EPIfilename'])); % Extract the EPI filenames
    B0_table.badScans_pr = zeros(length(epis), 1); % Initialize a column for bad scan counts

    % Find the bad scan files in the 'ART' folder for the current subject
    filename = dir(fullfile(dataPath, subjects{i}, 'func', 'ART', ['*', files]));
    filename = {filename.name}; % Extract the filenames of bad scan files

    %% Loop over each EPI file to check for bad scans and update the B0 table
    for e=1:length(epis)
        split = strsplit(epis{e, 1}, '_');
        ID = [subjects{i} '_' split{6} '_' split{7}]; % Create the subject ID
        IDdate = [subjects{i} '_' split{4} '_' split{6} '_' split{7}]; % Create an ID with date

        % Check if any bad scan file matches the current ID
        if any(startsWith(filename, ID))
            % If a matching file is found, read the file and update the bad scan count
            t = readtable(fullfile(dataPath, subjects{i}, 'func', 'ART', filename{startsWith(filename, ID)}));
            B0_table.badScans_pr(e) = t.ARTrepaired_pr + t.gapScansFromFlagged_pr;
        elseif any(startsWith(filename, IDdate))
            % If a matching file is found with the date in the filename, read the file and update the bad scan count
            t = readtable(fullfile(dataPath, subjects{i}, 'func', 'ART', filename{startsWith(filename, IDdate)}));
            B0_table.badScans_pr(e) = t.ARTrepaired_pr + t.gapScansFromFlagged_pr;
        else
            % If no bad scan file is found, set the bad scan count to 0 for this scan
            B0_table.badScans_pr(e) = 0;
        end
    end
    % Save the updated B0 table back into the subject's directory
    writetable(B0_table, fullfile(dataPath, subjects{i}, 'matching_epis_b0.xlsx'))

    % Append the updated B0 table for the current subject to the main badscans table
    badscans = [badscans; B0_table];
end

%% Save the aggregated bad scan data for all subjects into an Excel file
writetable(badscans, fullfile(savePath, ['badscans_' files(1:end-4) '.xlsx']))
end