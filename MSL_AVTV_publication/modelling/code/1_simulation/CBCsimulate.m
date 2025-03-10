function D = CBCsimulate(options, percModel, respModel, parms, parmsNames, ID)

details = CBCsubjects(ID, parms, options);
load(details.startfile)

%add dummy row
D = [D; D(end,:)];

for i = 1:height(D)
    % calculate surprise for current trial i
    % prior probability = 1/16 (fixed)

    D.surprise(i) = -log((sum(ismember(string(D.triplet(1:(i-1),:)),D.triplet(i,:)))+1)/(16+i-1));

    D = eval([percModel '(D, parms.prc, i)']);

    D = eval([respModel '(D, parms.resp, i)']);

end

%delete dummy row
D = [D(1:(end-1), :)];

simparms = [parms.prc, parms.resp];

for i = 1:length(simparms)
    D.(['sim_', percModel, '_', respModel, '_', parmsNames{i}]) = ...
        repmat(simparms(i), height(D), 1);
end


save(details.analysisfile, 'D')
end