function D = CBCsurprise(options, parms, statModel, ID)

details = CBCsubjects(ID, parms, options);
load(details.startfile) % D

sessions = unique(D.session);

Ds = table();

for s = 1:length(sessions)
    Dtemp = D(D.session == sessions(s), :);
    
    for j = 1:length(statModel)
        for i = 1:height(Dtemp)
            Dtemp = eval([statModel{j} '(Dtemp, i)']);
        end
        
        Dtemp = renamevars(Dtemp, 'surprise', statModel{j}(4:end));
    end
        
    % combine in one table
    Ds = [Ds;Dtemp];
end

D = Ds;
save(details.analysisfile, 'D')
end