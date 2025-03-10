function D = CBCsimpleRW(D, parms, i)
startBelief = parms(1);
alpha1 = parms(2);

%beliefs start as agnostic priors
if i == 1

    for au = {'A','B','C','D'}
        for vis = {'0', '1', '2','3'}
            D.([ char(vis),char(au),' belief'])(i) = startBelief;
        end
    end

else

    %read belief from D
    for au = {'A','B','C','D'}
        for vis = {'0', '1', '2','3'}
            D.([char(vis),char(au),' belief'])(i) = D.([char(vis),char(au),' belief'])(i-1);
        end
    end

    if isnan(D.choiceLeft(i-1)) %better D.timeout == 1 if available
        return
    end

    %determine reward for current trial
    %D.reward(i-1) = (D.match(i-1) == D.choice(i-1));
    %get current belief
    V1 = D.beliefPair(i-1);
    %V1 = D.beliefPairNorm(i-1);

    % calculate reward PE and update based on reward PE
    % important if we have probabilistic feedback
    if D.reward(i-1)
        D.rewardPE(i-1) = D.reward(i-1) - V1;
        D.update2(i-1) = alpha1*D.rewardPE(i-1);
    elseif ~(D.reward(i-1))
        D.rewardPE(i-1) = D.reward(i-1) - (V1); %% IS TIHS CORRECT OR SCHOULD IT BE V1??
        D.update2(i-1) = alpha1*D.rewardPE(i-1);
    end

    % calculate choice PE & update based on choice PE
    % ATM: only works for deterministic feedback!
    %     D.choicePE(i-1) = D.match(i-1) - V1;
    %     D.update1(i-1) = alpha1*(D.match(i-1) - V1);

    % match  choice reward belief0  rewardPE  choicePE
    % 0        0      1      0.3      0.3       -0.3
    % 0        1      0      0.3     -0.3       -0.3
    % 1        0      0      0.3     -0.7        0.7
    % 1        1      1      0.3      0.7        0.7


    % calculate updated belief for stimPair in current trial i
    %currVis = num2str(D.visStim(i-1));
    %currMod2 = char(D.mod2Stim(i-1));
    chosenPair = char(D.chosenPair(i-1,:));
    D.([chosenPair ' belief'])(i) = V1 + D.update2(i-1);
end

end