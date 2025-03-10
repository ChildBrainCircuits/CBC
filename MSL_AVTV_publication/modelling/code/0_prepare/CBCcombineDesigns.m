function [] = CBCcombineDesigns(options)

d = dir([options.designPath '\*\']);
d = d(~ismember({d.name}, {'.', '..'}));

for i = 1:length(d)
    tempts = readtable([d(i).folder '\' d(i).name], 'ReadVariableNames',true);

    ts{i} = tempts;
end

save([options.designPath '\trialStructures.mat'], 'ts')

end

end