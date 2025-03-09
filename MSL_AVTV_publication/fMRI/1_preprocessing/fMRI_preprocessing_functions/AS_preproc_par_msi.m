function AS_preproc_par_msi(subjects, tasklist, preproPath, anatTemplate, overwrite)
% A wrapper script for preprocessing using MATLAB parallel toolbox.
% The script preprocesses MRI data using SPM (Statistical Parametric Mapping) for
% multiple subjects and tasks. It works with field maps, T1 segmentation, and other preprocessing steps.
% The preprocessing is done in parallel for faster execution.

% INPUTS:
%   - subjects: A cell array containing subject IDs.
%   - tasklist: A cell array of task names (recommended to take one task at a time).
%   - preproPath: Path to the folder containing subject folders and other files.
%   - anatTemplate: Path to the anatomical template file (e.g., T1w).
%   - overwrite: A flag to overwrite existing processed files (1 for yes, 0 for no).

% OUTPUTS:
%   - Preprocessed files (SPM style) for multiple subjects.
%   - A log file containing the operations performed.

% Initialize SPM configuration
clear matlabbatch batch;
spm_jobman('initcfg');

%% BEGIN TASK LOOP
cd(preproPath);

% Loop through all tasks
for t=1:length(tasklist)
    currTask = tasklist{t}; % Get current task name
    currPath = [preproPath]; % Define the path for preprocessing files

    % Prepare empty cell arrays for storing batch jobs for each subject
    batch = cell(1,length(subjects));
    t1batch = cell(1,length(subjects)); % Store T1 segmentation jobs
    fmapbatch = cell(1,length(subjects)); % Store fieldmap creation jobs

    % Loop through all subjects
    for i=1:length(subjects)
        currsubject = subjects{i};% Get current subject ID

        % Check if the subject folder exists in the specified path
        if  isempty(dir([currPath, '**', filesep, currsubject]))
            disp(['Cannot find ',currsubject,' folder in ',currPath,' \n']);
        else

            % Call CREATE B0 table to generate a table of B0 images for the subject
            [B0_table,numbB0,b0_num] = AS_create_b0table_msi(currPath,currsubject);

            for a = 1:numbB0 % Loop through all B0 files
                % Find corresponding B0 files in the table
                ind = find(contains(B0_table.B0filename, b0_num(a)));

                % Call CREATE FIELD MAP to preprocess the B0 files and generate field maps
                [fmapbatch{1,i}{a}] = AS_create_fieldmap_msi(currsubject,preproPath,anatTemplate, B0_table.EPIfilename(ind), B0_table.B0filename{ind(1)}, overwrite);

                % Update the B0 table with processed filenames
                for b = 1:length(ind)
                    if length(ind) > 1
                        B0_table.B0filename_processed{ind(b)} = ['vdm5_sc' B0_table.B0filename{ind(1)}(2:end-7) 'ec1_typ3_session' num2str(b) '.nii'];
                    else
                        B0_table.B0filename_processed{ind(b)} = ['vdm5_sc' B0_table.B0filename{ind(1)}(2:end-7) 'ec1_typ3.nii'];
                    end
                end
            end

            % Save the B0 table for the current subject
            writetable(B0_table, fullfile(currPath, currsubject, 'matching_epis_b0.xlsx'));

            % Create T1 segmentation matlabbatch for the current subject
            [t1batch{1,i}] = AS_create_matlabbatch_t1seg(preproPath,currsubject,anatTemplate);
        end

    end


    %% RUN batch in parallel
    % Check if a parallel pool exists; if not, create one
    if ~isempty(gcp('nocreate'))
        delete(gcp('nocreate'));
    end
    parpool(12); % Start a parallel pool with 12 workers

    % Run Fieldmap batch jobs in parallel
    for i = 1:length(subjects)
        for a = 1:length(fmapbatch{1,i})
            if ~isempty(fmapbatch{1,i}{a}) % Check if fieldmap batch exists
                fprintf ('========================================================================\n');
                fprintf ('Generating fieldmap...');
                fprintf ('Subject directory is %s\n',subjects{i});
                fprintf ('========================================================================\n');

                % Run the fieldmap creation job for the current subject
                spm_jobman('run', fmapbatch{1,i}{a});
                fprintf(['>> Created Fieldmap for ' subjects{i} '.\n'])
            else
                disp(['Fieldmaps for ' currsubject 'already processed.'])
            end
        end
        disp(['Done with fieldmap for ' currsubject])
    end

    % Run T1 segmentation in parallel for all subjects
    seg_out = cell(1,length(subjects)); % Store segmentation outputs
    parfor i=1:length(subjects)
        if ~isempty(t1batch{1,i}) % Check if the T1 batch exists
            seg_out{i} =spm_jobman('run',t1batch{1,i}); % Run T1 segmentation job
        end
    end
    disp(['Done with T1 segmentation'])

    %% Create matlabbatch for each EPI file
    for i=1:length(subjects)
        clear B0_table
        currsubject = subjects{i}; % Get current subject ID
        if  isempty(dir([currPath,'**', filesep,currsubject]))
            disp(['Cannot find ',currsubject,' folder in ',currPath,' \n']);
        else
            % Read the B0 table to get processed B0 filenames
            B0_table = readtable(fullfile(currPath, currsubject, 'matching_epis_b0.xlsx'));

            % Loop through the table to create matlabbatch for each EPI file
            for c = 1:height(B0_table)
                b0 = B0_table.B0filename_processed{c}; % Get processed B0 filename
                epifile = B0_table.EPIfilename{c}; % Get EPI filename

                % Create preprocessing batch for the current EPI file
                [batch{1,i}{c}] = AS_create_matlabbatch_msi(preproPath,currsubject,anatTemplate, b0, epifile);
            end
        end
    end


    %% RUN batch sequentially
    %         for i=1:length(subjects)
    %             for j = 1:length(batch{1,i})
    %                 spm_jobman('run',batch{1,i}{j});
    %                 % spm_jobman('interactive',batch{1,i}{j});
    %             end
    %         end

    %% RUN batch in parallel for preprocessing
    if ~isempty(gcp('nocreate'))
        delete(gcp('nocreate'));% Delete any existing parallel pool
    end
    parpool(12); % Start a new parallel pool with 12 workers

    % Run preprocessing batch jobs in parallel for all subjects and EPIs
    data_out = cell(1,length(subjects)); % Store preprocessing outputs
    for i=1:length(subjects)
        currbatch = batch{1,i};
        parfor j = 1:length(currbatch)
            temp_data_out{j} = spm_jobman('run',currbatch{j}); % Run preprocessing job
            data_out{i,j} = temp_data_out{j}; % Store the output
        end
        disp(['Done with preprocessing for ' subjects{i}])
    end

    % Save the batch jobs to a .mat file for future reference
    save(fullfile(preproPath,['Batch_',currTask,'_',datestr(now,'mmddyyyy-HHMM'),'.mat']),'batch');
end
