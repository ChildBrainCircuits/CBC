function [B0_table,numbB0,b0s] = AS_create_b0table_msi(paths, currsubject)
% Creates a table matching b0 images to EPI files for a given subject.
%-----------------------------------------------------------------------
% Inputs:
%   paths - Structure or string containing the preprocessing directory path.
%   currsubject - String specifying the current subject ID.
%
% Outputs:
%   B0_table - Table mapping EPI files to their corresponding b0 images.
%   numbB0 - Number of unique b0 files found.
%   b0s - Unique b0 filenames used.
%
% (c) Agustina Steuer, Nina Raduner (Oktober 2023)

% Initialize an empty table to store b0-EPI mappings
B0_table = table();
clear numB0 b0_num

% Find EPI filenames in the subject's functional directory
epi_filenames = dir(fullfile(paths, currsubject, 'func', 'mr*_msi_*.nii'));

% Loop through EPI files and discard files that are too small (likely corrupted)
if contains(currsubject, 'CBC_1')
    for j = length(epi_filenames):-1:1
        if epi_filenames(j).bytes < 100000000
            epi_filenames(j) = [];
        end
    end
elseif contains(currsubject, 'CBC_P')
    for j = length(epi_filenames):-1:1
        if epi_filenames(j).bytes < 95000000
            epi_filenames(j) = [];
        end
    end
end

% Extract filenames as a cell array
epi_filenames = {epi_filenames.name};

% Find b0 filenames in the subject's fieldmap directory
b0_filenames = dir(fullfile(paths, currsubject, 'fmap', 'mr*_msi_*.nii'));
b0_filenames = {b0_filenames.name};

% Determine the number of b0 images by assuming groups of 6 b0 files
numbB0 = length(b0_filenames)/6;

% Extract numeric identifiers from b0 filenames
for j = 1:numbB0
    currB0 = b0_filenames{1+(j-1)*6};
    split = strsplit(currB0, '_');
    b0_num(j,1) = str2double(split{1, 6});
    b0_num(j,2) = str2double(split{1, 4});
end

% Initialize B0_table structure
B0_table.ID = repmat(currsubject,length(epi_filenames),1);
B0_table.Task = cell(length(epi_filenames), 1);
B0_table.B0 = zeros(length(epi_filenames), 1);
B0_table.EPIfilename = cell(length(epi_filenames), 1);
B0_table.B0filename = cell(length(epi_filenames), 1);
B0_table_empty = B0_table;

% Identify unique session dates for b0 images
dates = unique(b0_num(:,2));

% Loop over each session date to match EPI files with corresponding b0 images
for t = 1:length(dates)
    epis = epi_filenames(contains(epi_filenames,num2str(dates(t))));
    b0s = b0_num(b0_num(:,2)==dates(t),:);
    b0_files = b0_filenames(contains(b0_filenames,num2str(dates(t))));
    B0_table_temp = B0_table_empty;

    % Loop through EPI files and find matching b0 files
    for r = 1:length(epis)
        epi_file = epis{r};
        split = strsplit(epi_file, '_');
        epi_num = str2double(split{1, 6});
        epi_date = str2double(split{1, 4});

        % Assign the most recent b0 image to each EPI file
        for s = 1:length(b0s)
            if s < length(b0s) && ((b0s(s) < epi_num) && (b0s(s+1) > epi_num))
                B0_table_temp.B0(r) = b0s(s);
                B0_table_temp.B0filename{r} = ['^' b0_files{1+(s-1)*6}(1:end-12) '.*.nii$'];
                B0_table_temp.EPIfilename{r} = epi_file;
            elseif s == length(b0s) && (b0s(s) < epi_num)
                B0_table_temp.B0(r) = b0s(s);
                B0_table_temp.B0filename{r} = ['^' b0_files{1+(s-1)*6}(1:end-12) '.*.nii$'];
                B0_table_temp.EPIfilename{r} = epi_file;
            end
        end

    end

    % Remove empty rows from temporary table
    B0_table_temp = B0_table_temp(~cellfun('isempty', B0_table_temp.EPIfilename),:);

    % Append results to final B0_table
    B0_table = [B0_table;B0_table_temp];
end

% Remove empty rows from final table
B0_table = B0_table(~cellfun('isempty', B0_table.EPIfilename),:);

% Load task overview table to assign tasks to each EPI file
if contains(currsubject, 'CBC_1')
    table_overview = readtable(fullfile(basePath, 'data', 'outputs', 'data_overview.xlsx'));
elseif contains(currsubject, 'CBC_P')
    table_overview = readtable(fullfile('path', 'to', 'overview', 'table.xlsx'));
end

% Find the current subject in the task overview table
ind_currsubject = find(strcmp(table_overview.ID, currsubject));

% Assign tasks from the overview table to B0_table
for i = 1:length(B0_table.Task) %starting with 2 because 1 is the ID
    B0_table.Task{i} = char(table_overview{ind_currsubject, i+1});
end

% Extract unique b0 filenames
b0s = unique(B0_table.B0filename);
numbB0 = length(b0s);

% Save B0_table as an Excel file for future reference
writetable(B0_table, fullfiele(char(paths), currsubject, 'matching_epis_b0.xlsx'))