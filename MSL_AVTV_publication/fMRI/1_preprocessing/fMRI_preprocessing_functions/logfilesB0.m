function logfilesB0(subjects, preproPath, group)
% logfilesB0 synchronizes log files with EPI files for each subject.
% The function reads the matching EPI and B0 table, aligns it with log files
% based on date and time, and updates the B0_table with the correct log files.
%
% Input:
% - subjects: A cell array containing subject IDs
% - preproPath: A string specifying the path to the preprocessing directory
% - group: A string ('children' or 'adults') indicating the subject group

%% Begin loop for each subject
for i = 1:length(subjects)
    % Define subject-specific directory path
    subjectDir = fullfile(preproPath, subjects{i});
    
    % Read the existing table that contains EPI and B0 matching information
    B0_table = readtable(fullfile(subjectDir, 'matching_epis_b0.xlsx'));
    
    % Find all log files in the subject's behavioral data folder
    logpath = dir(fullfile(subjectDir, 'beh', '*.csv'));
    
    % Check if the number of log files matches the number of entries in B0_table
    if ~(length(logpath) == height(B0_table))
        warning(['not the same amount of logfiles and epi files for ' subjects{i}])
        return % Exit the function early if they do not match
    end
    
    % Initialize an empty table to store log file metadata
    d = table();
    d.date = datetime.empty(length(logpath), 0); % Initialize an empty datetime array
    d.time = zeros(length(logpath), 1); % Initialize an array to store extracted time values
    d.logfile = cell(length(logpath), 1); % Initialize a cell array to store log file names
    
    % Process each log file to extract date and time
    for j = 1:length(logpath)
        % Split filename using underscore to extract relevant information
        time = strsplit(logpath(j).name, '_');

        if isequal(group, 'chilren')
            % Extract date and time for children (date is at position 8)
            d.date(j) = datetime(time{8}, 'InputFormat', 'yyyy-MM-dd');
            time = time{9}; % Time is at position 9
        elseif isequal(group, 'adults')
            % Extract date and time for adults
            if ~contains(logpath(j).name, 'viwo')
                % Standard case for adults (date at position 7)
                d.date(j) = datetime(time{7}, 'InputFormat', 'yyyy-MM-dd');
                time = time{8};
            else
                % Special case for 'viwo' subjects (date at position 8)
                d.date(j) = datetime(time{8}, 'InputFormat', 'yyyy-MM-dd');
                time = time{9};
            end
        end
        % Further split the time string to extract hour and minute
        time = strsplit(time, '.'); % Remove file extension
        time = time{1};
        time = strsplit(time, 'h');  % Split hours and minutes
        time = str2double(time{1}) + str2double(time{2})/60; % Convert to decimal hours
        
        % Store extracted information in the table
        d.time(j) = time;
        d.logfile{j} = logpath(j).name;
    end

    % Sort the table based on date and time to ensure proper alignment
    d = sortrows(d);

    % Assign sorted log file names to the B0_table
    B0_table.logfile = d.logfile;

    % Save the updated table back to the Excel file
    writetable(B0_table, fullfile(subjectDir, 'matching_epis_b0.xlsx'))
end
