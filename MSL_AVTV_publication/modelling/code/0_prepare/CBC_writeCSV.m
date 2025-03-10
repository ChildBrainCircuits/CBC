%% save modeling output as csv
clear
clc
%%
cd('A:/Projects/04-03-ChildBrainCircuits/Analyses/Modeling/CBC_Modeling_v3_Paper1/')

dataPath = ['A:/Projects/04-03-ChildBrainCircuits/Analyses/Modeling/CBC_Modeling_v3_Paper1/' 'analysed/'];

files = dir(fullfile(dataPath, 'CBC_*'));
%subjects = {files(cellfun(@(x) length(x) == 8, {files.name})).name};
subjects = {'CBC_1035', 'CBC_1041', 'CBC_1042', 'CBC_1110', ...
    'CBC_1112', 'CBC_1114', 'CBC_1115', 'CBC_1122', 'CBC_1129', 'CBC_1136', ...
    'CBC_1138', 'CBC_1143', 'CBC_1146'};
%% delete files
% for i = 1:length(subjects)
%     filenames = dir([dataPath subjects{i} '/2_simulations/*CBCdriftDiffusion_LR*']);
%     if ~isempty(filenames)
%         for j = 1:length(filenames)
%             delete([filenames(j).folder '\' filenames(j).name])
%         end
%     end
%     disp(['Done with ' subjects{i} '!'])
% end

%% save as csv
for i = 1:length(subjects)
    filenames = dir([dataPath subjects{i} '/3_fit/*.mat']);
    filenames = {filenames.name};

    if length(filenames) ~= 1
        warning(['more files for ' subjects{i}])
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
end



