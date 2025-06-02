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
D.Properties.VariableNames = {'ID', 'session', 'trialNr','mod2Stim', 'visStimLeft', 'visStimRight', 'correctKey', 'frequency', 'rewardAccurate'};

D.probFB(:) = round(mean(D.rewardAccurate),2);

D.stimPairLeft = string(D.visStimLeft) + string(D.mod2Stim);
D.stimPairRight = string(D.visStimRight) + string(D.mod2Stim);

D.leftCorrect = char(D.correctKey) == 'a';
D.rightCorrect = char(D.correctKey) == 'l';

D.pCorrectLeft = abs(D.leftCorrect-(1-D.rewardAccurate));
D.pCorrectRight = 1- D.pCorrectLeft;

D.triplet = [char(D.mod2Stim) num2str(D.visStimLeft) num2str(D.visStimRight)];

%D.reactionTime = rand(options.trialNr,1)*3.5;

save(details.analysisfile, 'D')

end