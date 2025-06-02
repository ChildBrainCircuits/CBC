function D = CBCnonMatchPairSurprise(D, i)

D.surprise(i) = -log((sum(ismember(string(D.nonMatchingPair(1:(i-1),:)),D.nonMatchingPair(i,:)))+1)/(4+i-1));

% if the Left Stimulus Pair is non matching
% if D.leftCorrect(i,:) == 0
%     D.surpriseOld(i) = -log((sum(ismember(string(D.stimPairLeft(1:(i-1),:)),D.stimPairLeft(i,:)))+1)/(4+i-1));
% end
% 
% % if the Right Stimulus Pair is non matching
% if D.rightCorrect(i,:) == 0
%     D.surpriseOld(i) = -log((sum(ismember(string(D.stimPairRight(1:(i-1),:)),D.stimPairRight(i,:)))+1)/(4+i-1));
% end

end