function D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID)

details = CBCsubjects(ID, parms, options);
load(details.startfile)

%add dummy row
D = [D; D(end,:)];

for i = 1:height(D)
    % calculate surprise for current trial i
    % prior probability = 1/9 (fixed)

        D.surprise(i) = -log((sum(ismember(D.stimPair(1:(i-1)),D.stimPair(i)))+1)/(9+i-1));

        D = eval([percModel '(D, parms.prc, i)']);

        D = eval([respModel '(D, parms.resp, i)']);

end

%delete dummy row
D = [D(1:(end-1), :)];

simparms = [parms.prc, parms.resp];

D.('sim_percModel') = repmat(percModel, height(D), 1);
D.('sim_respModel') = repmat(respModel, height(D), 1);

for i = 1:length(simparms)
    D.(['sim_', parmsNames{i}]) = repmat(simparms(i), height(D), 1);
end

save(details.analysisfile, 'D')
end