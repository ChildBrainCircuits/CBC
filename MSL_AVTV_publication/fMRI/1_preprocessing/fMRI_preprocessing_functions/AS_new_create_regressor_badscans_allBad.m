function AS_new_create_regressor_badscans_allBad(subjects, path)
% CREATE A REGRESSOR WITH BAD SCANS
%-----------------------------------------------------
% Source: Georgette Pleisch & Iliana Karipidis März 15
% last version March 2020, SD, GFG, September 2024: NR
% - Reads the output txt file from ARTREPAIR with the index of repaired/detected
% - Artrepair txt files are empty if no scan exceeded thresholds set in artrepair scripts
% - Good scans flagged with 0 and bad scans with 1. Scans surrounded by bad scans (with 2 or 1 good scans gap) are also marked as 1.
%  e.g., the sequence [0 0 0 1 0 0 1 0 0 0 1] were 1 = bad scans will be coded:  [0 0 0 1 1 1 1 0 0 0 1]
% - Output mat file can be used for GLM (1st level ) *_flagscans.mat
% - Another output text file shows percent and count of flagged scans

% Loop through each subject
for i=1:length(subjects)
    % Get the list of 'art_repaired.txt' files in the subject's functional directory
    art_repaired = dir([path,subjects{i},filesep,'func',filesep, 'ART',filesep, '*art_repaired.txt']);

    % Get the number of scans for the subject
    B0_table = B0_table_nscans(path, subjects{i});

    % Loop through each art_repaired file (if there are multiple files)
    for f = 1:length(art_repaired)
        % Determine the number of scans in the current run
        if sum(B0_table.nscans) == height(B0_table)*B0_table.nscans(1)
            N_scans = B0_table.nscans(1); % If all scans are the same, use this value
        else
            id = split(art_repaired(f).name, '_'); % Split the filename to get the run identifier
            N_scans = B0_table.nscans(contains(B0_table.run, id{3, 1}) & contains(B0_table.run, ['_' id{4, 1} '_1']));
        end

        % Construct the full file path for the current art_repaired file
        file_repair = fullfile(path, subjects{i}, 'func', 'ART', art_repaired(f).name);

        % Open the text file containing bad scans and read it
        fid = fopen(file_repair, 'r'); % open txt file
        badscans = textscan(fid,'%n', N_scans, 'delimiter','\n'); % Read the list of bad scans
        fclose(fid); % close the file

        % Get the difference between consecutive bad scan positions
        differences=diff(badscans{1,1});

        % Identify the scans that are surrounded by bad scans (gap of 1 or 2 good scans)
        gaps=[];
        for s=1:length(differences)
            if differences(s,1) == 2
                % Add the scan after a gap of 2 to the list
                gaps=[gaps badscans{1,1}(s,1)+1];
            elseif differences(s,1) == 3
                % Add the next two scans after a gap of 3
                gaps=[gaps badscans{1,1}(s,1)+1 badscans{1,1}(s,1)+2];
            else
                continue
            end
        end

        % Combine the positions of flagged bad scans and those surrounded by bad scans
        all_bad=sort([badscans{1,1}' gaps]);

        % Create a regressor vector initialized with zeros
        flag=[];
        flag = all_bad;

        Regr_badscans = zeros(1,N_scans);
        Regr_badscans (1,flag)=1; % Mark the bad scan positions with 1
        Regr_badscans = Regr_badscans.'; % Transpose the regressor to a column vector

        % If there are bad scans, count and calculate percentages for flagged and repaired scans
        if ~isempty(badscans{1})
            nRepNotFlagged =  length(badscans{1}) - (length(flag)-length(gaps)); % Number of repaired scans not flagged as bad
            percentRepNotFlagged = sprintf('%.2f',(100*(nRepNotFlagged/N_scans)));  % Percentage of repaired scans not flagged
            ngaps = length(gaps); % Number of gap scans
            percentgaps =  sprintf('%.2f',(100*(ngaps/N_scans))); % Percentage of gap scans

            % Create a table to store the count and percentage of bad scans and repaired scans
            countBad = [{sprintf('%.2f',(100*length(badscans{1})/N_scans))},{sprintf('%.2f',(100*length(flag)/N_scans))},percentRepNotFlagged, percentgaps,length(badscans{1}),length(flag),nRepNotFlagged,ngaps];

            % Save the count information to a text file
            countBadTable = cell2table(countBad,'VariableNames',{'ARTrepaired_pr','Flagged_pr','RepairedInAnalysis_pr','gapScansFromFlagged_pr','ARTrepaired_n','Flagged_n','RepairedInAnalysis_n','gapScansFromFlagged_n'});
            countbadfile =  strrep(file_repair,'art_repaired.txt','countBadScans_allBad_v2.txt');
            writetable(countBadTable, countbadfile,'delimiter','\t')
        end

        % Save the regressor to a .mat file if there are any flagged bad scans
        if any(Regr_badscans)
            regressorfile =  strrep(file_repair,'art_repaired.txt','flagscans_allBad_v2.mat'); % Save the regressor file
            save(fullfile(regressorfile), 'Regr_badscans'); % Save as .mat file
            disp (['Flagged bad scans for subject ',subjects{i}])
        else
            disp (['No scans were flagged for subject ',subjects{i}])
        end
    end
end
end
