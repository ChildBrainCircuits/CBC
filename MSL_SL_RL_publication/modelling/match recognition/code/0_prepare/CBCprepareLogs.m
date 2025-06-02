function D = CBCprepareLogs(options, ID, parms)
% function that reads in log files and saves them in the desired way
% input =   options with paths to the logfiles and where to save
%           ID of current subject
% output =  D with data

details = CBCsubjects(ID, parms, options);

d = dir(fullfile(details.dataroot, [details.subjectname '*.csv']));
filenames = {d.name};

sessInfo = readtable('data/sessInfo.csv', 'ReadVariableNames',true, 'Delimiter', ',');

if isempty(filenames)
    warning(['No files found for subject ' ID]);
    D=table();
else
    disp([num2str(length(filenames)) ' files found for subject ' ID]);

    % get correct order of filenames
    dates = strings(length(filenames),1);
    for j = 1:length(filenames)
        all = strsplit(filenames{j},'_');
        dates(j,:) = [all{end-1} '_' all{end}];
    end

    [~, order] = sort(dates);

    % get session numbers based on filenames
    currSessInfo = sessInfo(contains(sessInfo.logfile, filenames),{'ID', 'logfile', 'session'});

    D = table();

    % load file and save
    for i = 1:length(filenames)
        filename = filenames{order(i)};
        logfile = readtable(filename, 'Delimiter',options.delimiter, 'DecimalSeparator',options.decimalSeparator, 'VariableNamingRule', 'modify');
        idxTrials = find(~isnan(logfile.trials_runs_thisRepN));
        logfile_trials = logfile(idxTrials,:); %um Format anzupassen, ab jetzt nur noch logfile_trials nehmen
        
        newlogTV = table();
        
        %check if correct subject-name is logged, otherwise change
        if isequal(logfile_trials.participant{1}, details.subjectname)
            newlogTV.ID = logfile_trials.participant;
        else
            newlogTV.ID = repelem({details.subjectname}, options.trialNr)';
            warning(['wrong ID in ' filename])
        end
        
        newlogTV.visStimLeft = logfile_trials.trials_runs_visual_stim;
        if contains(filename, 'TV')
            newlogTV.secondStim = logfile_trials.trials_runs_tactile_stim; 
            newlogTV.mod2Type = repmat('tac', height(newlogTV),1);
        elseif contains(filename, 'AV')
            newlogTV.secondStim = logfile_trials.trials_runs_auditory_stim; % trials_runs_auditory_stim for children
            newlogTV.mod2Type = repmat('aud', height(newlogTV),1);
        end
        
        newlogTV.frequency = logfile_trials.trials_runs_presentationFrequncy/100;
        newlogTV.mod2Stim(newlogTV.secondStim == newlogTV.secondStim(find(newlogTV.visStimLeft == 0 & newlogTV.frequency == 0.5, 1))) = "A";
        newlogTV.mod2Stim(newlogTV.secondStim == newlogTV.secondStim(find(newlogTV.visStimLeft == 1 & newlogTV.frequency == 0.5, 1))) = "B";
        newlogTV.mod2Stim(newlogTV.secondStim == newlogTV.secondStim(find(newlogTV.visStimLeft == 2 & newlogTV.frequency == 0.5, 1))) = "C";
        newlogTV.rewardAccurate = logfile_trials.trials_runs_FBaccuracy;
        
        newlogTV.stimPair = string(newlogTV.visStimLeft) + string(newlogTV.mod2Stim);
        newlogTV.match = logfile_trials.trials_runs_correctPair;
        
        newlogTV.key = logfile_trials.response_runs_keys;
        newlogTV.reactionTime = logfile_trials.response_runs_rt;

        if ismember('yesKey', logfile_trials.Properties.VariableNames) %checks whether yesKey is logged
            yesKey = logfile.yesKey(end);
        else
            warning(['yesKey not logged in ' filename])
            idx = find(logfile_trials.trials_runs_FBaccuracy == 1 & logfile_trials.trials_runs_correctPair == 1, 1); % finds first trial with correct combination and accurate reward
            if logfile_trials.trials_runs_correct_answer(idx) == 1
                yesKey = logfile_trials.response_runs_keys{idx};
            elseif logfile_trials.trials_runs_correct_answer(idx) == 0 && logfile_trials.response_runs_keys{idx} == 'a'
                yesKey = 'l';
            elseif logfile_trials.trials_runs_correct_answer(idx) == 0 && logfile_trials.response_runs_keys{idx} == 'l'
                yesKey = 'a';
            end            
        end
        newlogTV.choice(ismember(newlogTV.key, yesKey)) = 1;
        newlogTV.yesKey = repelem({yesKey},options.trialNr,1);
        newlogTV.choiceAccurate = logfile_trials.trials_runs_correct_answer;
       
        newlogTV.reward = logfile_trials.trials_runs_feedback_given;
        
        newlogTV.choiceAccurate(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
        newlogTV.choice(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
        newlogTV.reward(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;

        % Define expected session based on filename patterns
        currSessNumb = currSessInfo.session(contains(currSessInfo.logfile, filename));
        if length(currSessNumb) ~= 1
            error(['More than 1 session number for: ' filename])
        end
        newlogTV.session = repelem(currSessNumb, height(newlogTV))';

        newlogTV.trial = logfile_trials.trials_runs_thisRepN+1; %logfile_trials.xy %links: macht Spalte mit Name ID, rechts: man definiert, was in diese Spalte kommt
%         newlogTV.probMatch = newlogTV.match;
%         newlogTV.probMatch(~newlogTV.rewardAccurate) = 1-newlogTV.match(~newlogTV.rewardAccurate);
%         newlogTV.probMatch(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;

        newlogTV.filename = repelem({filename}, height(newlogTV))';
        newlogTV.Properties.VariableNames;

        newlogTV = newlogTV(:, {'ID', 'filename', 'session', 'trial', 'visStimLeft', 'secondStim', 'mod2Type', ...
            'frequency', 'rewardAccurate', 'yesKey', 'mod2Stim', 'stimPair', 'match', 'key', 'reactionTime', ...
            'choice', 'choiceAccurate', 'reward'});

        D = [D;newlogTV];
        
        D = sortrows(D, 'session');

        % SAVE D
        save(details.analysisfile, 'D')
    end
end
