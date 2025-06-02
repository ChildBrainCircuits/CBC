function [matlabbatch] = AS_create_fieldmap_msi(currsubject,preproPath,anatTemplate, epi_files, b0, overwrite)
% AS_create_fieldmap_msi - Creates the voxel displacement map (vdm5*.nii) for a fieldmap file and an EPI image.
% This function sets up and executes an SPM batch to process fieldmaps for distortion correction.
%
% INPUTS:
%   currsubject   - Subject ID
%   preproPath    - Path to preprocessing directory
%   anatTemplate  - Anatomical template file
%   epi_files     - Cell array of EPI filenames
%   b0            - Base name of fieldmap file
%   overwrite     - Flag to determine whether to overwrite existing files (0 = no overwrite)
%
% Adapted by Nina Raduner, October 2024, Gorka Fraga Gonzalez, March 2020. Original: (c) David Willinger 2018/08/02.


%% Define directories for fieldmap and EPI images
clear matlabbatch
b0Dir = fullfile(preproPath,currsubject,'fmap', filesep);
epiDir = fullfile(preproPath,currsubject,'func', filesep);

%% Check if vdm file already exists and matches expected number of EPI files
if overwrite==0 && (length(dir([ b0Dir 'vdm5*' b0(2:end-7) '*'])) == 1 && length(epi_files) == 1) || ...
        (length(dir([ b0Dir 'vdm5*' b0(2:end-7) '*'])) > 1 && length(dir([ b0Dir 'vdm5*' b0(2:end-7) '*'])) == length(epi_files) + 1 )
    matlabbatch = [];
    return;
end

%% Retrieve fieldmap files
fullfilesb0 = cellstr(spm_select('ExtFPList',b0Dir,b0,1));

% Ensure exactly one fieldmap set (6 files expected: 2 magnitudes, 2 phases, etc.)
n_fieldmaps = length(fullfilesb0)/6;
if n_fieldmaps ~= 1
    fprintf ('Wrong number of field map files found in B0 directory \n', n_fieldmaps);
end

%% Retrieve subject-specific echo times from parameter file
parfile = dir([b0Dir b0(2:end-8) '*.par']);
if length(parfile)~= 1
    fprintf ('Check your par files ABORT!!!')
else
    fid = fopen(fullfile([parfile.folder,filesep,parfile.name]),'rt');
    textFromPar = textscan(fid, '%f ', 'delimiter', 'Whitespace','collectoutput',true,'HeaderLines',100);
    format shortg
    echoes = [];
    
    % Extract echo times from parameter filele
    shortecho=textFromPar{1}(31);
    longecho=textFromPar{1}(80);
    if (longecho == shortecho)
        longecho=textFromPar{1}(227);
    end
    echoes = [echoes; shortecho longecho];
    fclose(fid);
end
current_echo = echoes;

%% Setup SPM batch for fieldmap processing
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.data.phasemag.shortphase = cellstr(spm_select('FPList', b0Dir, [b0(1:end-8) '.*.ec1_typ3.*.nii$']));
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.data.phasemag.shortmag = cellstr(spm_select('FPList', b0Dir, [b0(1:end-8) '.*.ec1_typ0.*.nii$']));
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.data.phasemag.longphase = cellstr(spm_select('FPList', b0Dir, [b0(1:end-8) '.*.ec2_typ3.*.nii$']));
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.data.phasemag.longmag = cellstr(spm_select('FPList', b0Dir, [b0(1:end-8) '.*.ec2_typ0.*.nii$']));
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.et = current_echo;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.maskbrain = 0;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.blipdir = -1;

%% Compute EPI readout time
% Formulas from: https://support.brainvoyager.com/brainvoyager/functional-analysis-preparation/29-pre-processing/78-epi-distortion-correction-echo-spacing-and-bandwidth
% MRI scanner and acquisition parameters
epifactor  = 31; % EPI factor
matrixsize_phase_enc_dir = 62; % Matrix size in phase encoding direction in create_matlabbatch: "scanresolution"
water_fat_shift_pixel = 12.48;
resonance_freq_mhz_tesla    = 42.576; % gyromagnetic ratio for proton (1H)
fieldstrength_tesla         = 3.0;  % magnetic field strength (T)
water_fat_diff_ppm          = 3.35; % Haacke et al: 3.35ppm. Bernstein et al (pg. 960): Chemical shifts (ppm, using protons in tetramethyl silane Si(CH3)4 as a reference). Protons in lipids ~1.3, protons in water 4.7, difference: 4.7 - 1.3 = 3.4.
sensefactor                 = 2;

% Compute readout time
water_fat_shift_hz          = fieldstrength_tesla * water_fat_diff_ppm * resonance_freq_mhz_tesla; % water_fat_shift_hz 3T = 427.8888 Hz
echo_train_length           = epifactor + 1;
effective_echo_spacing_msec = ((1000 * water_fat_shift_pixel)/(water_fat_shift_hz * echo_train_length))/sensefactor;
total_epi_readout_time      = effective_echo_spacing_msec * matrixsize_phase_enc_dir;

%% Finalize SPM batch settings
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.tert = total_epi_readout_time;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.epifm = 0;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.ajm = 0;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.uflags.method = 'Mark3D';
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.uflags.fwhm = 10;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.uflags.pad = 0;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.uflags.ws = 1;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.mflags.template = {anatTemplate};
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.mflags.fwhm = 5;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.mflags.nerode = 2;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.mflags.ndilate = 4;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.mflags.thresh = 0.5;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.defaults.defaultsval.mflags.reg = 0.02;

% Assign EPI files to batch
for i = 1:length(epi_files)
    matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.session(i).epi = cellstr(spm_select('ExtFPList', epiDir, epi_files{i},1)); %first volume of epi file.
end

matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.matchvdm = 1;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.sessname = 'session';
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.writeunwarped = 0;
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.anat = '';
matlabbatch{1}.spm.tools.fieldmap.calculatevdm.subj.matchanat = 0;

end