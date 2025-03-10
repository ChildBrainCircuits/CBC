function D = CBCsimpleSigmoid(D, parms, i)

beta = parms(1);

% 
leftPair = char(D.stimPairLeft(i));
V1 = D.([leftPair ' belief'])(i);

rightPair = char(D.stimPairRight(i));
V2 = D.([rightPair ' belief'])(i);
%V2 = 1 -V1;

% noramlising beliefs
V1norm = V1/(V1+V2);
V2norm = V2/(V1+V2);

% probability chosing left pair
D.prob(i) = exp(beta*V1norm)/(exp(beta*V1norm)+exp(beta*(1-V1norm)));

D.choice(i) = rand < D.prob(i); % when randon number < current probability -> 1 correct answer, if > -> 0 wrong answert --> select choice based on probability

if D.choice(i)
    D.choiceLeft(i) = 1;
    D.choiceRight(i) = 0;
    D.chosenPair(i,:) = leftPair;
    D.otherPair(i,:) = rightPair;
    D.beliefPair(i) = V1;
    D.beliefOtherPair(i) = V2;
    D.beliefPairNorm(i) = V1norm;
    D.beliefOtherPairNorm(i) = V2norm;

    % calculate reward
    D.choiceAccurate(i) = D.choiceLeft(i) == D.leftCorrect(i);
    D.reward(i) = (D.choiceLeft(i) == D.leftCorrect(i)) == D.rewardAccurate(i);

elseif D.choice(i) == 0
    D.choiceLeft(i) = 0;
    D.choiceRight(i) = 1;
    D.chosenPair(i,:) = rightPair;
    D.otherPair(i,:) = leftPair;
    D.beliefPair(i) = V2;
    D.beliefOtherPair(i) = V1;
    D.beliefPairNorm(i) = V2norm;
    D.beliefOtherPairNorm(i) = V1norm;

    % calculate reward
    D.choiceAccurate(i) = D.choiceRight(i) == D.rightCorrect(i);
    D.reward(i) = (D.choiceRight(i) == D.rightCorrect(i)) == D.rewardAccurate(i);
end
end







