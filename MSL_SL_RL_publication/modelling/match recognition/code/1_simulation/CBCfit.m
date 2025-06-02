function [D, fittedParms, NLL] = CBCfit(options, percModel, respModel, parms, ID)

%This function fits a perceptual Model and a response model to the dataset
%specified in options.
%Input: - options: specifies paths etc
%       - percModel: specifies perceptual Model as char e.g. 'CBCsimpleRW'
%       - respModel: specifies response Model e.g. 'CBCsimpleSigmoidLik'
%       - parms: parms.LB, parms.UB, parms.n, parms.names;

details = CBCsubjects(ID, parms, options);
load(details.startfile) % D

sessions = unique(D.session);

Ds = table();
for s = 1:length(sessions)
    tic;
    Dtemp = D(D.session == sessions(s), :);
    
    getD = 0;
    
    % change upper boundary of non-decision time to min(RT)
    if min(Dtemp.reactionTime, [],'omitnan') > 0.200
        parms.UB(contains(parms.names, 'nonDecisionTime')) = min(Dtemp.reactionTime, [],'omitnan');
    end
    
    % function giving out negative loglikelihood
    NLLfun = @(xparms) CBCwrapperForFit(Dtemp, percModel, respModel, parms.nprc, xparms, getD);
    
    %function finding minimum
    %[fittedParms,NLL] = fminsearch(NLLfun,startparms);
    
    options = optimoptions('ga','PlotFcn', @gaplotbestf, ...
        'FunctionTolerance', 10^(-5), 'MaxGenerations', 500, 'UseParallel', true);
    
    [fittedParms,NLL,exitflag,output] = ga(NLLfun,parms.n,[],[],[],[],parms.LB,parms.UB, [],[], options);
    
    getD = 1;
    Dtemp = CBCwrapperForFit(Dtemp, percModel, respModel, parms.nprc, fittedParms,getD);
    
    % add fitted parameters to the table
    Dtemp.('fit_percModel') = repmat(percModel, height(Dtemp), 1);
    Dtemp.('fit_respModel') = repmat(respModel, height(Dtemp), 1);
    
    for i = 1:parms.n
        Dtemp.(['fit_', parms.names{i}]) = repmat(fittedParms(i), height(Dtemp), 1);
    end
    
    Dtemp.('fit_NLL') = repmat(NLL, height(Dtemp), 1);
    
    % combine in one table
    Ds = [Ds;Dtemp];
    
    toc;
end

D = Ds;
save(details.analysisfile, 'D')


end