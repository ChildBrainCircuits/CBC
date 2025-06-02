function D = CBCdriftDiffusionLR(D, parms, i)

% This script fits data using the Wiener first passage time
% distribution (WFPT) from Navarro et al 2009. The upper and lower boundary
% was modeled to be left vs. right choice

% Author: Yifei Cao, IDB Master internship

ze_t = parms(1);% non-decision time
m = parms(2);%drift weight
a = parms(3);%fixed decision boundary
zStart  = parms(4);

% if you want to add a increasing or collapsing decision boundary, please
% uncomment the bb and bp parameters and comment the a parameter. See this
% paper for reference: Pedersen et al., (2017) on Psyconomic Bulletin &
% Review
% bb = parms(5); %decision boundary at first trial
% bp = parms(6); %controls the increase or decrease of the boundary


%% Define the perceptual model
% development of starting point z over time

leftPair = char(D.stimPairLeft(i));
V1 = D.([leftPair ' belief'])(i);
rightPair = char(D.stimPairRight(i));
V2 = D.([rightPair ' belief'])(i);

V1norm = V1/(V1+V2);
V2norm = V2/(V1+V2);
belief = V1norm - V2norm;%please decide whether you need to use V1 or V1norm

l = length(belief);

sim.wfpt_v1_rt_sim = nan(length(belief),1);
sim.wfpt_v1_response_sim = nan(length(belief),1);

dt = 0.001;
t = [10^-80 dt:dt:10];
t= t';

v = belief * m;
a = a;
err = 10^(-29); % err: error threshold
s = 1; % VarianceNoise of random walk

z = zStart * a;

rtsample = zeros(1,l);
rtmean = zeros(1,l);
responsemean = zeros(1,l);

ii = l;
pt_upper = wfpt_wrapper(t,-repmat(v(ii),length(t),1),...
    repmat(a(ii), length(t),1),a(ii)-repmat(z(ii),length(t),1),err);

pt_lower = wfpt_wrapper(t,repmat(v(ii),length(t),1),...
    repmat(a(ii), length(t),1),repmat(z(ii),length(t),1),err);

% flipping and plottng lower and upper distribution next to each other
x = [-flip(t);t];
x1 =[flip(t);t];
pt = [flip(pt_lower); pt_upper];
pt_norm = pt./sum(pt);

% calculating cummulative probability 
rn = rand;
pt_cum = cumsum(pt_norm);
[~, min_ind] = min(abs(pt_cum-rn));
rtsample(ii) = x(min_ind);
% rtsample(ii) = randsample(x,1,true,pt); %sample randomly from distribution

%disp(['accumulated probability:',num2str(sum(pt)*dt)])

rtmean(ii) = sum(x1.*pt_norm)+ze_t; % mean reaction time from distribution
responsemean(ii) = sum(pt_upper/sum(pt)); % mean choice from distribution

response = (1+sign(rtsample))./2;
rt = abs(rtsample)+ze_t; 

% sim.wfpt_v1_rt_sim = rtmean';
% sim.wfpt_v1_response_sim = responsemean';
% 
% sim.wfpt_v1_correct_sim = [responsemean, ...
%         (1-flip(responsemean)+responsemean)./2];

D.reactionTime(i) = rt;
% D.prob(i) = sim.wfpt_v1_correct_sim;

D.choice(i) = response;

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

