function [matlabbatch] = AS_create_matlabbatch_msi(preproPath,currsubject,anatTemplate, b0, epifile)
% AS_CREATE_MATLABBATCH_MSI
%
% This function creates an SPM matlabbatch configuration for an MR 
% preprocessing pipeline. The pipeline includes:
%   1. Slice timing correction
%   2. Realignment and unwarping (using field maps)
%   3. Coregistration of the functional images to the anatomical image
%   4. Normalization of functional and anatomical images to MNI space
%   5. Smoothing of normalized functional images
%
% INPUTS:
%   preproPath   - Base directory containing the subject folders.
%   currsubject  - The current subject's folder name.
%   anatTemplate - Anatomical template file (currently not used directly).
%   b0           - A string (or pattern) used to select the field map (b0) file.
%   epifile      - A string (file name or pattern) used to select the EPI file.
%
% OUTPUT:
%   matlabbatch  - A cell array with SPM batch configuration for the pipeline.
%
% (c) David Willinger. Gorka Fraga Gonzalez (March 2020), Nina Raduner (October 2024)

%% Check for required input arguments
if nargin < 1
    sprintf('No paths provided!');
    return;
end

%% Define directories for the different image types
% Field map (b0) directory
b0Dir = fullfile(preproPath, currsubject, 'fmap', filesep); 
% EPI (functional) directory
epiDir = fullfile(preproPath, currsubject, 'func', filesep);  
% Anatomical (T1) directory
t1Dir = fullfile(preproPath, currsubject, 'anat', filesep); 

%% Task-specific parameters for file selection and acquisition parameters
b0current = cellstr(spm_select('FPList', b0Dir, b0));

% Select the EPI scans:
% Initialize an empty cell array; note that this should include only raw data.
scansList = []; 
% For a specific epi file, limit the selection to 278 volumes;
% otherwise, select all volumes (Inf).
if isequal(epifile, 'mr5393_cbc_1046_20240127_120744_13_1_epi_msi_av_44slices_v3.nii')
    scansList = {cellstr(spm_select('ExtFPList',epiDir, ['^',epifile], 1:278))};
else 
    scansList = {cellstr(spm_select('ExtFPList',epiDir, ['^',epifile], Inf))};
end

% Acquisition parameters
nslices     = 44; % Number of slices per volume
tr          = 1.395; % Repetition time (in seconds)
% Define slice timing 
timings     = [fliplr(round([0:31.7045*2:1331.6])),fliplr(round([0:31.7045*2:1331.6]))];
voxelSize   = 3; % Voxel size for normalization (in mm)
smoothSize  = 8; % moothing kernel size (FWHM in mm)

%% Select the deformation field for normalization from the anatomical folder
% The deformation field file usually starts with 'y_'
normFile = dir([t1Dir 'y_*']);
normFile = {[t1Dir, normFile.name]};

%% 1. PARALLEL SLICE TIMING CORRECTION
% ------------------------------------
% Correct for differences in slice acquisition timing.
matlabbatch{1}.spm.temporal.st.scans = scansList;
matlabbatch{1}.spm.temporal.st.nslices = nslices;
matlabbatch{1}.spm.temporal.st.so = timings;
% Set the reference slice to one in the middle of the acquisition (dependent on multiband factor)
matlabbatch{1}.spm.temporal.st.refslice = matlabbatch{1}.spm.temporal.st.so(ceil(length(matlabbatch{1}.spm.temporal.st.so)/4)); % find middle slice timing. Depends on the multiband factor.
matlabbatch{1}.spm.temporal.st.tr = tr;
matlabbatch{1}.spm.temporal.st.ta = 0; % ta= tr - tr/nslices.(time of acquisition of one slice). Not used when explicit slice timing (so) is provided
matlabbatch{1}.spm.temporal.st.prefix = 'a'; % Prefix for the output files

%% 2. SPATIAL REALIGNMENT AND UNWARPING
% --------------------------------------
% Correct for head motion and susceptibility-induced distortions using field maps.
% The scans to be unwarped are the slice-timing corrected images from step 1.
matlabbatch{2}.spm.spatial.realignunwarp.data(1).scans(1) = cfg_dep('Slice Timing: Slice Timing Corr. Images (Sess 1)', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{1}, '.','files'));
% Provide the field map file(s)
matlabbatch{2}.spm.spatial.realignunwarp.data(1).pmscan = b0current;
% Options for the realignment/unwarp procedure
matlabbatch{2}.spm.spatial.realignunwarp.eoptions.quality = 1;
matlabbatch{2}.spm.spatial.realignunwarp.eoptions.sep = 4;
matlabbatch{2}.spm.spatial.realignunwarp.eoptions.fwhm = 5;
matlabbatch{2}.spm.spatial.realignunwarp.eoptions.rtm = 1;
matlabbatch{2}.spm.spatial.realignunwarp.eoptions.einterp = 7;
matlabbatch{2}.spm.spatial.realignunwarp.eoptions.ewrap = [0 1 0];
matlabbatch{2}.spm.spatial.realignunwarp.eoptions.weight = '';
% Options for the unwarping part (estimation of the deformation field)
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.basfcn = [12 12];
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.regorder = 1;
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.lambda = 100000;
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.jm = 0;
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.fot = [4 5];
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.sot = [];
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.uwfwhm = 4;
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.rem = 1;
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.noi = 2;
matlabbatch{2}.spm.spatial.realignunwarp.uweoptions.expround = 'Average';
% Options for writing the unwarped images
matlabbatch{2}.spm.spatial.realignunwarp.uwroptions.uwwhich = [2 1];
matlabbatch{2}.spm.spatial.realignunwarp.uwroptions.rinterp = 7;
matlabbatch{2}.spm.spatial.realignunwarp.uwroptions.wrap = [0 1 0];
matlabbatch{2}.spm.spatial.realignunwarp.uwroptions.mask = 1;
matlabbatch{2}.spm.spatial.realignunwarp.uwroptions.prefix = 'u';

%% 3. COREGISTRATION
% -------------------
% Align the mean functional image (from realign & unwarp) with the subject's T1-weighted image.
% The T1 image is selected using a pattern ('^im*.') in the anatomical folder.
matlabbatch{3}.spm.spatial.coreg.estimate.ref(1) = cellstr(spm_select('ExtFPList',t1Dir, '^im*.', Inf)); % t1 image as reference
% The source image is the unwarped mean functional image (from step 2)
matlabbatch{3}.spm.spatial.coreg.estimate.source(1) = cfg_dep('Realign & Unwarp: Unwarped Mean Image', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','meanuwr')); %mean functional image as source. Transformation to bring to T1 space.
% All individual functional volumes will be coregistered to T1 space
matlabbatch{3}.spm.spatial.coreg.estimate.other(1) = cfg_dep('Realign & Unwarp: Unwarped Images (Sess 1)', substruct('.','val', '{}',{2}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','sess', '()',{1}, '.','uwrfiles')); % all individual volumes to be transformed to t1 spaces
% Coregistration options
matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.cost_fun = 'nmi';
matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.sep = [4 2];
matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.tol = [0.02 0.02 0.02 0.001 0.001 0.001 0.01 0.01 0.01 0.001 0.001 0.001];
matlabbatch{3}.spm.spatial.coreg.estimate.eoptions.fwhm = [7 7];

%% 4. NORMALIZATION and SMOOTHING of FUNCTIONAL IMAGES
% ------------------------------------------------------
% Step 4 normalizes the coregistered functional images to MNI space using the deformation field.
matlabbatch{4}.spm.spatial.normalise.write.subj.def = normFile;
% The images to be normalized are the coregistered functional images from step 3.
matlabbatch{4}.spm.spatial.normalise.write.subj.resample(1) = cfg_dep('Coregister: Estimate: Coregistered Images', substruct('.','val', '{}',{3}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','cfiles'));
% Define the bounding box for the normalized images (in MNI space)
matlabbatch{4}.spm.spatial.normalise.write.woptions.bb = [-90 -126 -72
                                                           91 91 109];  %keep this 'box'  in 2 lines!
% Voxel size for normalized images (functional resolution)
matlabbatch{4}.spm.spatial.normalise.write.woptions.vox = [voxelSize voxelSize voxelSize]; 
matlabbatch{4}.spm.spatial.normalise.write.woptions.interp = 7;
matlabbatch{4}.spm.spatial.normalise.write.woptions.prefix = 'w';

% Smooth the normalized functional images.
matlabbatch{5}.spm.spatial.smooth.data(1) = cfg_dep('Normalise: Write: Normalised Images (Subj 1)', substruct('.','val', '{}',{4}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('()',{1}, '.','files'));
matlabbatch{5}.spm.spatial.smooth.fwhm = [smoothSize smoothSize smoothSize];
matlabbatch{5}.spm.spatial.smooth.dtype = 0;
matlabbatch{5}.spm.spatial.smooth.im = 0;
matlabbatch{5}.spm.spatial.smooth.prefix = ['s',num2str(smoothSize)];

%% 5. NORMALIZATION of THE ANATOMICAL (T1) IMAGE
% ------------------------------------------------
% Here we normalize the T1-weighted anatomical image to MNI space with a finer voxel resolution.
matlabbatch{6}.spm.spatial.normalise.write.subj.def = normFile;
% Select the T1 image(s) using the same pattern as earlier.
matlabbatch{6}.spm.spatial.normalise.write.subj.resample(1) = cellstr(spm_select('ExtFPList',t1Dir, '^im*.', Inf));
matlabbatch{6}.spm.spatial.normalise.write.woptions.bb = [-90 -126 -72
                                                           91 91 109]; %keep this in 2 lines!
% Use a 1x1x1 mm voxel size for the anatomical normalization.
matlabbatch{6}.spm.spatial.normalise.write.woptions.vox = [1 1 1];
matlabbatch{6}.spm.spatial.normalise.write.woptions.interp = 7;
matlabbatch{6}.spm.spatial.normalise.write.woptions.prefix = 'w';

end