function D = CBCsurprise(options, parms, percModel, ID)

details = CBCsubjects(ID, parms, options);
load(details.startfile) % D

sessions = unique(D.session);

Ds = table();

for s = 1:length(sessions)
    Dtemp = D(D.session == sessions(s), :);

    for i = 1:height(Dtemp)
        Dtemp.surprise(i) = -log((sum(ismember(string(Dtemp.triplet(1:(i-1),:)),Dtemp.triplet(i,:)))+1)/(16+i-1));
    end
    
    Ds = [Ds;Dtemp];
end

D = Ds;
save(details.analysisfile, 'D')
end