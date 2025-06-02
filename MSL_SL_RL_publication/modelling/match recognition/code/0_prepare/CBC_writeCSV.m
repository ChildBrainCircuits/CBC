%% save modeling output as csv
cd('A:/Projects/04-03-ChildBrainCircuits/Analyses/Modeling/CBC_Modeling_Paper2/CBC_Modeling_v1_P2_Pre/')

dataPath = [pwd '/analysed/'];

files = dir(fullfile(dataPath, 'CBC_*'));
subjects = {files(cellfun(@(x) length(x) == 7, {files.name})).name};
%subjects = {'CBC_1031'};
% subjects = {'CBC_1010', 'CBC_1011', 'CBC_1081', 'CBC_1087', 'CBC_1088'};
%%
pause('on')

for i = 1:length(subjects)
    filenames = dir([dataPath subjects{i} '/3_fit/*.mat']);
    filenames = {filenames.name};

    if isempty(filenames)
        warning(['zero files for ' subjects{i}])
        return
    elseif length(filenames) ~= 5
        warning(['different amount of files for ' subjects{i}])
        return
    end

    if ~isfolder([dataPath subjects{i} '/4_csv/'])
        mkdir([dataPath subjects{i} '/4_csv/'])
    end

    for j = 1:length(filenames)
        temp = load([dataPath subjects{i} '/3_fit/' filenames{j}]).D;
        writetable(temp, [dataPath subjects{i} '/4_csv/' filenames{j}(1:end-3) 'csv'])
    end

    disp(['Done with ' subjects{i} '!'])

    if mod(i, 10) == 0
        pause(120)
    end
end

