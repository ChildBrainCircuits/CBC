function D = CBCbothPairsRW(D, parms, i)
startBelief = parms(1);
alpha1 = parms(2);
alpha2 = parms(3);


for au = {'A','B','C', 'D'}
    for vis = {'0', '1', '2', '3'}
        if i == 1
            %beliefs start as agnostic priors
            D.([char(vis),char(au),' belief'])(i) = startBelief;
        else
            %read belief from previous trial
            D.([char(vis),char(au),' belief'])(i) =D.([char(vis),char(au),' belief'])(i-1);
        end
    end
end

if i > 1 && ~isnan(D.choiceLeft(i-1))
    %determine reward for current trial
    %D.reward(i-1) = (D.match(i-1) == D.choice(i-1));
    %get current belief
    V1 = D.beliefPair(i-1);
    
    % calculate reward PE and update based on reward PE
    % important if we have probabilistic feedback
    if D.reward(i-1)
        D.rewardPE(i-1) = D.reward(i-1) - V1;
        D.update2(i-1) = alpha1*D.rewardPE(i-1);
    elseif ~(D.reward(i-1))
        D.rewardPE(i-1) = D.reward(i-1) - (V1);
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
    
    % calculate belief for stimPair in current trial i
    chosenPair = char(D.chosenPair(i-1,:));
    D.([chosenPair ' belief'])(i) = V1 + D.update2(i-1);
    
    % repeat everything for other pair
    V2 = D.beliefOtherPair(i-1);
    
    % calculate reward PE and update based on reward PE
    % important if we have probabilistic feedback
    if D.reward(i-1)
        D.rewardPEOP(i-1) = D.reward(i-1) - (1-V2);
        D.update2OP(i-1) = -(alpha2*D.rewardPEOP(i-1));
    elseif ~(D.reward(i-1))
        D.rewardPEOP(i-1) = D.reward(i-1) - (1-V2);
        D.update2OP(i-1) = -(alpha2*D.rewardPEOP(i-1));
    end
    
    % calculate belief for stimPair in current trial i
    otherPair = char(D.otherPair(i-1,:));
    D.([otherPair ' belief'])(i) = V2 + D.update2OP(i-1);
    
end
end