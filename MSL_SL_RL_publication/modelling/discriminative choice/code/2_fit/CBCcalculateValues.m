function D = CBCcalculateValues(options, percModel, respModel, ID)

details = CBCsubjects(ID, options);
load(details.startfile) % D

sessions = unique(D.session);

Ds = table();
for s = 1:length(sessions)
    Dtemp = D(D.session == sessions(s), :);

    parms = [Dtemp.fitCBCsimpleRW_CBCsimpleSigmoid_startBelief(1) ...
        Dtemp.fitCBCsimpleRW_CBCsimpleSigmoid_alpha1(1) ...
        Dtemp.fitCBCsimpleRW_CBCsimpleSigmoid_beta(1)];
    nprc = 2;

    %add dummy row
    Dtemp = [Dtemp; Dtemp(end,:)];

    for i = 1:height(Dtemp)

        Dtemp.surprise(i) = -log((sum(ismember(Dtemp.stimPair(1:(i-1)),Dtemp.stimPair(i)))+1)/(9+i-1));

        Dtemp = eval([percModel '(Dtemp, parms(1:nprc), i)']);

        Dtemp = eval([respModel 'Lik(Dtemp, parms((nprc+1):end), i)']);

    end

    Ds = [Ds;Dtemp(1:options.trialNr,:)];
end

D = Ds;

D.rewardPE(isnan(D.choice)) = NaN;
D.choicePE(isnan(D.choice)) = NaN;

save(details.analysisfile, 'D')

end