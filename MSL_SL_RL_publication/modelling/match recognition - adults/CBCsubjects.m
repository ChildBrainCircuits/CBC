function [details] = CBCsubjects(ID, parms, options)
%INPUT:% id: ID of subject or subjectgroup
% OPTIONS.analysisdir: preprocessing, psychoFit, modelFit
% OPTIONS.analysis: name of analysis or processing step
% OPTIONS.startdir: preprocessing, (psychoFit, modelFit)
% OPTIONS.start: name of data which enters the analysis step:
%                   Draw, Dcorrected.. 

%OUTPUT% Starting file for analysis
% details.startfile 
% Analysed file (dependent on OPTIONS.task, OPTIONS.analysisdir and OPTIONS.analysis):
% details.analysisfile
% details.analysisfig    
% Author: Maya Schneebeli, Nina Raduner
% Reviewer:


%% Name
details.subjectname = ['CBC_' ID];
disp(['Create paths for subject ' details.subjectname]);

%% Data file
details.dataroot = fullfile(options.path, 'data');
if isequal(options.analysisdir, '3_fixed')
    details.dataroot = fullfile(options.path, 'data/fixed/');
end




%% Analysis file
analysisroot = fullfile(options.path, 'analysed', details.subjectname);

if isfield(options,'analysis')
    analysisname = [details.subjectname,'_', options.analysis];
    %for export in R   
    details.analysisfile_R = fullfile(analysisroot, options.analysisdir, [analysisname, '.csv']);
    details.analysisfile_R_parms = fullfile(analysisroot, options.analysisdir, [analysisname, '_parms.csv']);
    details.pathfilename_R = fullfile(options.path, 'rcode');
    %name of analysis
    % file and analysisfigure   
%     if isequal(options.analysisdir, '1_prepare') || isequal(options.analysis, 'Dfit_CBCsurprise') ...
%             || isequal(options.analysisdir, '2_simulations') || isequal(options.analysisdir, '3_fixed')
    details.analysisfile = fullfile(analysisroot, options.analysisdir, [analysisname, '.mat']);
%     else
%         details.analysisfile = fullfile(analysisroot, options.analysisdir, [analysisname, '_a' num2str(parms.LB(2)) num2str(parms.UB(2)) '_b'  num2str(parms.LB(3)) num2str(parms.UB(3)) '.mat']);
%     end
    details.analysisfig = fullfile(analysisroot, options.analysisdir, [analysisname, '.fig']);
    if ~exist(fullfile(analysisroot, options.analysisdir),'dir')
        mkdir(fullfile(analysisroot, options.analysisdir))
    end
    disp(['Outputfile: ' details.analysisfile])
end

%% Inputfile (startfile) 
if isfield(options, 'start')
    startname = [details.subjectname,'_', options.start];
    details.startfile = fullfile(analysisroot, options.startdir, [startname, '.mat']);
    disp(['Inputfile: ' details.startfile]);
end
