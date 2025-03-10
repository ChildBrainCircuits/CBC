function D = CBCaddVariable(options,ID)
details = CBCsubjects(ID, options);
load(details.startfile) % D


D.trial = (1:height(D))';

save(details.analysisfile, 'D')



end