function output = CBCwrapperForFit(D, percModel, respModel, nprc, parms, getD)

%add dummy row
D = [D; D(end,:)];

for i = 1:height(D)

    D.surprise(i) = -log((sum(ismember(string(D.triplet(1:(i-1),:)),D.triplet(i,:)))+1)/(16+i-1));

    D = eval([percModel '(D, parms(1:nprc), i)']);

    D = eval([respModel 'Lik(D, parms((nprc+1):end), i)']);

end

D = D(1:end-1,:);

D.lik(isnan(D.choiceLeft)) = NaN;

% remove outliers from real subjects
if length(D.ID{1}) == 8
    idx = find(D.reactionTime < (mean(D.reactionTime, 'omitnan') + 3*std(D.reactionTime, 'omitnan')) & ...
        D.reactionTime > 0.200);
    NLL = -sum(log(D.lik(idx)), 'omitnan');    
else
    NLL = -sum(log(D.lik), 'omitnan');
end

if getD
    output = D;
else
    output = NLL;
end

end