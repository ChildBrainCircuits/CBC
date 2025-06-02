function [allD, D, P, R, rpath] = CBCcombineData(OPTIONS, parms, whichIDs)
%This function combines dataframes from multiple analyses over multiple
%subjects into one dataframe. This dataframe contains all variables of the
%defined analyses in the columns and all subjects and trials in the rows.

%whichIDs a field of OPTIONS
%'ASDIDs'         ASD group
%'CIDs'           control group
%'subjectIDs'     all subjects without excluded
%'subjectIDsAll'  all subjects with excluded

% Author: Maya Schneebeli
% Reviewer: Annia Rüesch

% Copyright (C) 2020 TNU, Institute for Biomedical Engineering, University of Zurich and ETH Zurich.

% This file is released under the terms of the GNU General Public
% Licence (GPL), version 3. You can redistribute it and/or modify it under the terms of the GPL
% (either version 3 or, at your option, any later version).
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.



disp(['Combine data for id group ' whichIDs '...']);


allD = table();
icount = 0;

%combine over all IDs in OPTIONS.(whichIDs)
for iid = 1:length(OPTIONS.(whichIDs))

    id = [OPTIONS.(whichIDs){iid}];

    if ~isfield(OPTIONS, 'analysisstepsdir')

        analysisdir = {OPTIONS.startdir};
        analysisfiles = {OPTIONS.start};
    else
        analysisdir = OPTIONS.analysisstepsdir(1:end);
        analysisfiles = OPTIONS.analysisstepsfile(1:end);
    end

    %open per subject all analysisfiles
    for a = 1:length(analysisdir)
        OPTIONS.startdir = analysisdir{a};
        OPTIONS.start = analysisfiles{a};

        details = CBCsubjects(id, parms, OPTIONS);

        load(details.analysisfile);

        %replace temporarly all NaN values with -99
        %otherwise error in joining
        for i=1:size(D,2)
            x = D{:,i};
            try
                x(isnan(x)) = -99;
            catch
            end
            D{:,i} = x;
        end

        %G contains all Dataframes D for one participant
        G{a} = D;

    end

    if a > 1
        %combine the dataframes of one subject
        for d = 1:length(G)-1
            G{d+1}=join(G{d}, G{d+1});

        end
        D = G{end};
    else
        D = G{1};
    end

    %replace -99 values with NaN
    for i=1:size(D,2)
        try
            x = D{:,i};
            x = double(x);
            x(x==-99) = NaN;
            D{:,i} = x;
        catch
        end
    end

    %attach all subjects
    allD = [allD; D];




    disp([details.startfile, ' successfully added'])

end

%P is a summary of allD with one mean row (over all trials per session)
% for each participant
if ismember('session', allD.Properties.VariableNames)
    allDnew=allD;
    allDnew(:,[2,12:25]) = [];
    P = varfun(@nanmean, allDnew, 'GroupingVariables', {'session', 'ID'});
    for i = 1:length(P.Properties.VariableNames)
        if ~isempty(cell2mat((strfind(P.Properties.VariableNames(i), 'nanmean'))))
            s = char(P.Properties.VariableNames(i));
            try
                P.Properties.VariableNames(i) = {s(9:end)};
            catch
            end
        end
    end



    % D is a summary of allD with one mean row (over participants) for each
    % trial
    D = varfun(@nanmean, allDnew, 'GroupingVariables', {'session', 'trial'});
    for i = 1:length(D.Properties.VariableNames)
        if ~isempty(cell2mat((strfind(D.Properties.VariableNames(i), 'nanmean'))))
            s = char(D.Properties.VariableNames(i));
            try
                D.Properties.VariableNames(i) = {s(9:end)};
            catch
            end
        end
    end


    D.ID = repmat(id, height(D), 1);

else
    P = NaN;
    D = NaN;
end

details = CBCsubjects(whichIDs, OPTIONS);
save(details.analysisfile, 'allD', 'D','P');
% Exportfiles for R
writetable(allD, details.analysisfile_R);
if isfield(allD, 'session')
    writetable(P, details.analysisfile_R_parms);
end

disp(['Combine data for id group ' whichIDs ': completed']);


end



