function D = CBCsimpleSurprise(D, i)

    D.surprise(i) = -log((sum(ismember(string(D.stimPair(1:(i-1),:)),D.stimPair(i,:)))+1)/(9+i-1));
    
end