function D = CBC2StepAsymRW(D, parms, i)
startBelief = parms(1);
alpha1 = parms(2);
alpha12 = parms(3);
alpha2 = parms(3);
%alpha22 = parms(4); % not used since we only update other pairs if pos reward

for au = {'A','B','C'}
    for vis = {'0', '1', '2'}
        if i == 1
            %beliefs start as agnostic priors
            D.([char(vis),char(au),' belief'])(i) = startBelief;
        else
            %read belief from previous trial
            D.([char(vis),char(au),' belief'])(i) =D.([char(vis),char(au),' belief'])(i-1);
        end
    end
end

if i > 1
    
    if isnan(D.choice(i-1)) 
        return
    end
    
    %determine reward for current trial
    %D.reward(i-1) = (D.match(i-1) == D.choice(i-1));
    %get current belief
    V1 = D.beliefPair(i-1);
    
    % calculate reward PE and update based on reward PE
    % important if we have probabilistic feedback
    if D.choice(i-1)
        D.rewardPE(i-1) = D.reward(i-1) - V1;
        if D.reward(i-1) == 1
            D.update2(i-1) = alpha1*D.rewardPE(i-1);
        elseif D.reward(i-1) == 0
            D.update2(i-1) = alpha12*D.rewardPE(i-1);
        end
    elseif ~(D.choice(i-1))
        D.rewardPE(i-1) = D.reward(i-1) - (1-V1);
        if D.reward(i-1) == 1
            D.update2(i-1) = -(alpha1*D.rewardPE(i-1));
        elseif D.reward(i-1) == 0
            D.update2(i-1) = -(alpha12*D.rewardPE(i-1));
        end
    end
    
    % calculate choice PE & update based on choice PE
    % ATM: only works for deterministic feedback!
    D.choicePE(i-1) = D.match(i-1) - V1;
    D.update1(i-1) = alpha1*(D.match(i-1) - V1);
    
    % match  choice reward belief0  rewardPE  choicePE
    % 0        0      1      0.3      0.3       -0.3
    % 0        1      0      0.3     -0.3       -0.3
    % 1        0      0      0.3     -0.7        0.7
    % 1        1      1      0.3      0.7        0.7
    
    % calculate belief for stimPair in current trial i
    % currVis = num2str(D.visStimLeft(i-1));
    % currMod2 = char(D.mod2Stim(i-1));
    currPair = char(D.stimPair(i-1));
    D.([currPair ' belief'])(i) = V1 + D.update2(i-1);
       
    %if pair is "correct": update belief of row and column of current pair
    %if pair is "incorrect", the remaining pairs could be incorrect or
    %correct, therefore no updating takes place
    
    if D.choice(i-1) && D.reward(i-1)% ATM: probablisitic feedback not considered!!
        currVis = num2str(D.visStimLeft(i-1));
        currMod2 = char(D.mod2Stim(i-1));
        for au = 'A':'C'
            for vis = 0:2
                testPair = [num2str(vis), au];

                if strcmp(testPair, currPair)
                    continue
                elseif contains(testPair, currVis) || contains(testPair, currMod2)
                     V2 = D.([testPair ' belief'])(i-1);
                     D.([testPair ' belief'])(i) = V2 + alpha2*(0-V2);
                end

            end
        end
    end
    
end
end