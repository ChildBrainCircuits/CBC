function D = CBCsimpleSurprise(D, i)

D.surprise(i) = -log((sum(ismember(string(D.triplet(1:(i-1),:)),D.triplet(i,:)))+1)/(16+i-1));
% D.surpriseOld(i) = -log((sum(ismember(string(D.tripletUnsorted(1:(i-1),:)),D.tripletUnsorted(i,:)))+1)/(16+i-1));

end