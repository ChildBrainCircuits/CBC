function overview_subs(backupPath, savePath, group)
% overview_subs generates an overview of task data and writes to an Excel file
% Input:
% - backupPath: The path to the directory containing the subject data
% - savePath: The path to save the output Excel file
% - group: The group of subjects ('children' or 'adults')
%
% Example usage:
% overview_subs('path/to/raw/data/', 'path/where/outputs/are/stored/', 'children');

%% Get the list of subjects
% Based on the group (children or adults), this part gets all subject IDs
% from the raw folder starting with a specific prefix for each group.
if isequal(group, 'children')
    d = dir([backupPath '\CBC_1*']); % For children, subject IDs start with 'CBC_1'
    subs = {d.name};
elseif isequal(group, 'adults')
    d = dir([backupPath '\CBC_P*']); % For adults, subject IDs start with 'CBC_P'
    subs = {d.name};
end

%% Initialize a table to store the overview data
A = table(); % Create an empty table for storing task data
A.ID = char(subs'); % Store subject IDs in the first column of the table
A.task1(:) = ""; % Initialize empty columns for each task
A.task2(:) = "";
A.task3(:) = "";
A.task4(:) = "";
A.task5(:) = "";
A.task6(:) = "";

%% Loop through each subject
for i = 1:length(subs)
    task = ""; % Initialize task variable to empty string
    tasks = ""; % Initialize tasks list as empty

    % Get the list of files matching the pattern for the current subject
    % (looking for specific fMRI data files)
    d = dir([backupPath subs{i} '\func\mr*epi_msi*.nii']);

    %% Remove small files based on group (children/adults)
    if isequal(group, 'children')
        % For children, remove files that are too small (less than 100MB)
        for j = length(d):-1:1
            if d(j).bytes < 100000000 % Check file size in bytes
                d(j) = []; % Remove the file from the list
                j = j-1; % Decrease index to recheck the next file
            end
        end
    elseif isequal(group, 'adults')
        % For adults, remove files that are too small (less than 95MB)
        for j = length(d):-1:1
            if d(j).bytes < 95000000
                d(j) = [];
                j = j-1;
            end
        end
    end

    % Get the names of the remaining files after filtering by size
    filenames = {d.name};

    %% Extract task identifiers from filenames
    for j = 1:length(filenames)
        task = split(filenames{j}, '_');  % Split the filename by underscore
        tasks(j) = task(10); % Extract the 10th part of the filename as the task identifier
    end

    % Initialize counters for AV and TV task types
    avCounter = 1;
    tvCounter = 1;

    %% Loop through each extracted task and update with counters
    for k = 1:length(tasks)
        currentElement = tasks{k}; % Get the current task identifier

        % Check if the element is 'av'
        if strcmp(currentElement, 'av')
            % If 'av', append the counter value to the task identifier
            tasks{k} = strcat('av', num2str(avCounter));
            avCounter = avCounter + 1;  % Increment the AV counter
        elseif strcmp(currentElement, 'tv')
            % If 'tv', append the counter value to the task identifier
            tasks{k} = strcat('tv', num2str(tvCounter));
            tvCounter = tvCounter + 1; % Increment the TV counter
        end

        % Store the updated task information in the table
        A(i,k+1) = {tasks(k)};
    end

end

%% Remove rows where task1 column is empty (i.e., no task data)
A(A.task1 == "",:) = [];

%% Write the overview data to an Excel file
writetable(A, [savePath 'data_overview.xlsx']) % Save the table to an Excel file at the specified path

end