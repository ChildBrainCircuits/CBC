function D = CBCcreateD(options, trajNr, ID, parms)
% input:    options - struct created by CBCsetOptions
%           trajNr - number of trajectory (1-4)
%           ID - current subject
% output:   D - table containing relevant information about task structure
%           save D to folder

D = table();
details = CBCsubjects(ID, parms, options);
% add subject ID to D
D.ID = repmat(ID, options.trialNr,1);
D.session = ones(height(D),1);
D.trialNr = (1:options.trialNr)';

% load stimulus structure and save in D
ts = load(options.designFile).ts;
currTS = ts{trajNr};

probFBcol = randi([6,width(currTS)]);

D = [D currTS(:,1:5) currTS(:,probFBcol)];
D.Properties.VariableNames = {'ID', 'session', 'trialNr', 'visStimLeft', 'mod2Stim', 'match', 'correctKey', 'frequency', 'rewardAccurate'};
D.correctKey = repelem(D.correctKey(1),options.trialNr,1);
D.stimPair = append(string(D.visStimLeft), string(D.mod2Stim));
D.probFB(:) = round(mean(D.rewardAccurate),2);

save(details.analysisfile, 'D')

end