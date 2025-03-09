function AS_run_artrepair(subjects, preproPath)
%% Correct movement outliers using ArtRepair Toolbox
%---------------------------------------------------------------------
% This function applies the ArtRepair toolbox to correct movement outliers
% in EPI files for each subject.
%
% Prerequisites:
% The ArtRepair toolbox should be installed in SPM12.
% Before running the script define the following in art_global function:
% - line 108: Percent_thresh = 4 (as Özlem et al. 2014, Bertelettia et al.2014, Prado et al. 2014; for 1.5% read Hong et al. 2014)
% - line 110: z_thresh = 9 (very liberal, beacause it is never reported in the literature)
% - line 116: mv_thresh = 1.5; (half of voxel size)
% - line 136 set repair1_flag = 0 to only correct first scan if necessary
% Change also threshold for total movement in art_clipmvmt function:
% -  line 27: MVMTTHRESHOLD = 5 (default=3)
%-------------------------------------------------------------------------

cd(preproPath); % Change directory to preprocessing path

%% Loop through each subject
for ss = 1:numel(subjects)
    subjectDir = fullfile(preproPath, subjects{ss});

    % Read the EPI file paths from 'matching_epis_b0.xlsx'
    B0_table = readtable(fullfile(subjectDir, 'matching_epis_b0.xlsx'));
    epis = table2cell(B0_table(:, ['EPIfilename']));

    % Create output directory for ART-processed files
    newDir = fullfile(subjectDir,'func', 'ART', filesep];
    if ~isfolder(newDir)
        mkdir(newDir);
    end

    if~isempty(epis)
        for e = 1:length(epis)
            % Extract relevant information from EPI filename
            split = strsplit(epis{e, 1}, '_');

            % Select all EPI volumes matching the pattern
            Images = spm_select('ExtFPList', fullfile(subjectDir, 'func', filesep),  ['^s.*.wuamr.*._' split{1,4} '_.*._' split{1,6} '_' split{1, 7} '_.*.nii$'], Inf);

            % Select the realignment parameter file
            RealignmentFile = spm_select('FPList', fullfile(subjectDir, 'func', filesep], ['^rp_am.*._' split{1,4} '_.*._' split{1,6} '_' split{1, 7} '_.*.txt$']); % common file with realignment parameters

            % ArtRepair settings
            HeadMaskType = 4; %auto mask
            RepairType = 1; % ArtifactRepair alone (movement and add margin)
            % if you use RepairType = 2 or 0 movement threshold has to be adjusted in
            % line 147 or 150 of art_global function

            % Run ArtRepair
            art_global(Images, RealignmentFile, HeadMaskType, RepairType)

            %% Process and move output files
            fileID_split = strsplit(['^s.*.wuamr.*._' split{1,4} '_.*._' split{1,6} '_' split{1, 7} '_.*.nii$'],'_');
            fileDate = [fileID_split{2}];
            fileID = [fileID_split{4} '_' fileID_split{5}];

            % Locate output files
            funcDir = [subjectDir filesep 'func' filesep];
            newfiles = [dir([funcDir 'vs*wuamr*' fileDate '*' fileID '*.nii']);
                dir([funcDir 'ArtifactMask.nii']);
                dir([funcDir 'art_deweighted.txt']);
                dir([funcDir,'art_repaired.txt'])];

            B0_table.ARTfilename(e) = {newfiles(1).name};

            % Move and rename output files
            for j = 1:length(newfiles)
                if contains(newfiles(j).name,'art_deweighted.txt') || contains(newfiles(j).name,'art_repaired.txt')  || contains(newfiles(j).name,'ArtifactMask.nii')
                    movefile([funcDir,newfiles(j).name],[newDir,subjects{ss},'_',fileDate,'_',fileID,'_',newfiles(j).name]);
                else
                    movefile(fullfile(newfiles(j).folder, newfiles(j).name),[newDir,newfiles(j).name]);
                end
            end

            % Move and rename the output figure
            picfile = dir(fullfile(subjectDir, ['artglobal' subjects{ss} 'func.jpg'])) ;
            if ~isempty(picfile)
                movefile(fullfile(picfile.folder, picfile.name),[newDir,subjects{ss},'_',fileDate,'_',fileID,'_',picfile.name]);

            else
                disp([picfile.name,' NOT FOUND!'])
            end

            % Save updated B0 table
            writetable(B0_table, fullfile(subjectDir, 'matching_epis_b0.xlsx'))
        end
    else
        disp('Epis not found')
    end
end
