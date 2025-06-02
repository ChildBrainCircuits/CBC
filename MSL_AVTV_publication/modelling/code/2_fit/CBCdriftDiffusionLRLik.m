function D = CBCdriftDiffusionLRLik(D, parms, i)
%Yifei: this code is for modeling DDM for left vs right choice with
%fixed boundaries


% This script fits data using the Wiener first passage time
% distribution (WFPT) from Navarro et al 2009.

% Author: Maya Schneebeli
% Reviewer: Annia R�esch

% Copyright (C) 2020 TNU, Institute for Biomedical Engineering, University of Zurich and ETH Zurich.

% This file is released under the terms of the GNU General Public
% Licence (GPL), version 3. You can redistribute it and/or modify it under the terms of the GPL
% (either version 3 or, at your option, any later version).
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

ze_t = parms(1);% non-decision time
m = parms(2);%drift weight
a = parms(3);%fixed decision boundary
zStart  = parms(4);

%% Define the perceptual model
% development of starting point z over time
if ismissing(D.chosenPair(i,:)) 
    return
else
    %l = height(D);
    
    leftPair = char(D.stimPairLeft(i));
    V1 = D.([leftPair ' belief'])(i);
    rightPair = char(D.stimPairRight(i));
    V2 = D.([rightPair ' belief'])(i);
    
    % Here we modified, and classified the choice to "correct" vs "incorrect"
    % instead of "left" and "right"
    % Vc denotes belief for correct choice, Vi denotes belief for incorrect
    % choice
%     if D.leftCorrect(i) == 1
%         Vc = D.([leftPair ' belief'])(i);
%         Vi = D.([rightPair ' belief'])(i);
%     else
%         Vc = D.([rightPair ' belief'])(i);
%         Vi = D.([leftPair ' belief'])(i);
%     end
    
    V1norm = V1/(V1+V2);
    V2norm = V2/(V1+V2);
    belief = V1norm - V2norm;
      
    if D.choiceLeft(i) == 1
        pressed = 1;
        D.beliefPairNorm(i) = V1norm;
        D.beliefOtherPairNorm(i) = V2norm;
        D.beliefPair(i) = V1;
        D.beliefOtherPair(i) = V2;
    else
        pressed = 0;
        D.beliefPairNorm(i) = V2norm;
        D.beliefOtherPairNorm(i) = V1norm;
        D.beliefPair(i) = V2;
        D.beliefOtherPair(i) = V1;
    end
    
    vf = belief*m + randn*0.001;
    af = a;
    
    tf = (D.reactionTime(i)-ze_t);
    
    err = 10^(-29); % err: error threshold
    s = 1; % Variance/Noise of random walk
    
    zf = af*zStart;
    
    
%     py_upperf =(exp(2.*af.*vf./s.^2)-exp(2*(af-zf).*vf./s.^2))./...
%         (exp(2.*af.*vf./s.^2)-1);
    
    %probability of response time upper boundary
    pt_upperf = wfpt_wrapper(tf,-vf,...
        af,(af-zf),err);

    %probability of response time lower boundary
    pt_lowerf = wfpt_wrapper(tf,vf,...
        af,zf,err);
    
    %probability of response time given the boundary hit (upper or lower)
    ptf = pressed.*pt_upperf+...
        (1-pressed).*pt_lowerf;
    %pt = [flip(pt_lowerf); pt_upperf];
    %responsemean = sum(pt_upperf/sum(pt));
    %log likelihood
    %logp = -nansum(log(ptf)) ;
    
    ptf_1 = pressed.* (pt_upperf/(pt_upperf + pt_lowerf))+ ...
       (1-pressed).* (pt_lowerf/(pt_upperf + pt_lowerf));
    
    %correct = [responsemean(ceil(end/2)), ...
    %    (1-flip(responsemean(1:floor(end/2)))+responsemean(ceil(end/2)+1:end))./2];
    %D.reactionTime(i) = sim.wfpt_v1_rt_sim;
    %D.prob(i) = correct;
    %D.logp(i) = logp;
    D.lik(i) = ptf; % density
    D.corrLik(i) = ptf_1; % probability
    
end

