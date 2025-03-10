function D = CBCprepareLogs(options, parms, ID)
% function that reads in log files and saves them in the desired way
% input =   options with paths to the logfiles and where to save
%           ID of current subject
% output =  D with data

details = CBCsubjects(ID, parms, options);

d = dir(fullfile(details.dataroot, [details.subjectname '*.csv']));
filenames = {d.name};

if isempty(filenames)
    warning(['No files found for subject ' ID]);
    D=table();
else
    disp([num2str(length(filenames)) ' files found for subject ' ID]);

    % get correct order of filenames
    dates = strings(length(filenames),1);
    for j = 1:length(filenames)
        all = strsplit(filenames{j},'_');
        dates(j,:) = all{end};
    end

    [~, order] = sort(dates);

    D = table();

    % load file and save
    for i = 1:length(filenames)
        filename = filenames{order(i)};
        logfile = readtable(['data/', filename]);%, 'Delimiter',options.delimiter, 'DecimalSeparator',options.decimalSeparator, 'VariableNamingRule', 'modify');
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
        
        newlogTV.visStimLeft = logfile_trials.trials_runs_visual_stim_left;
        newlogTV.visStimRight = logfile_trials.trials_runs_visual_stim_right;
        newlogTV.correctKey = char(logfile_trials.trials_runs_correctKey);
        newlogTV.leftCorrect = newlogTV.correctKey == logfile.trials_runs_leftKey{end};
        newlogTV.rightCorrect = newlogTV.correctKey == logfile.trials_runs_rightKey{end};

        if contains(filename, 'TV')
            newlogTV.secondStim = logfile_trials.trials_runs_tactile_stim; %evtl. noch oder einsetzen, also wenn auditorisch und nicht taktil
            newlogTV.mod2Type = repmat('tac', height(newlogTV),1);
            pairing = str2num(logfile.tactilePair{end});
        else %if contains(filename, 'AV')
            newlogTV.secondStim = logfile_trials.trials_runs_auditory_stim;
            newlogTV.mod2Type = repmat('aud', height(newlogTV),1);
            pairing = str2num(logfile.audioPair{end});
        end
        
        newlogTV.mod2Stim(newlogTV.secondStim == pairing(1)) = "A";
        newlogTV.mod2Stim(newlogTV.secondStim == pairing(2)) = "B";
        newlogTV.mod2Stim(newlogTV.secondStim == pairing(3)) = "C";
        newlogTV.mod2Stim(newlogTV.secondStim == pairing(4)) = "D";
        newlogTV.frequency = logfile_trials.trials_runs_presentationFrequncy;
        newlogTV.rewardAccurate = logfile_trials.trials_runs_FBaccuracy;

        newlogTV.stimPairLeft = string(newlogTV.visStimLeft) + string(newlogTV.mod2Stim);
        newlogTV.stimPairRight = string(newlogTV.visStimRight) + string(newlogTV.mod2Stim);
        %newlogTV.stimPairCorrect = 
        newlogTV.pCorrectLeft = abs(newlogTV.leftCorrect-(1-newlogTV.rewardAccurate));
        newlogTV.pCorrectRight = 1- newlogTV.pCorrectLeft;

        newlogTV.key = string(logfile_trials.response_runs_keys);
        newlogTV.reactionTime = logfile_trials.response_runs_rt;

        newlogTV.choiceLeft = double(newlogTV.key(:,1) == logfile.trials_runs_leftKey{end});
        newlogTV.choiceRight = double(newlogTV.key(:,1) == logfile.trials_runs_rightKey{end});
        newlogTV.choiceAccurate = double(newlogTV.key(:,1) == string(newlogTV.correctKey));

        newlogTV.reward = logfile_trials.trials_runs_feedback_given;
      
        newlogTV.chosenPair(logical(newlogTV.choiceLeft)) = newlogTV.stimPairLeft(logical(newlogTV.choiceLeft));
        newlogTV.chosenPair(logical(newlogTV.choiceRight)) = newlogTV.stimPairRight(logical(newlogTV.choiceRight));
        newlogTV.otherPair(logical(newlogTV.choiceLeft)) = newlogTV.stimPairRight(logical(newlogTV.choiceLeft));
        newlogTV.otherPair(logical(newlogTV.choiceRight)) = newlogTV.stimPairLeft(logical(newlogTV.choiceRight));
        
        if any(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None'))
            newlogTV.choiceLeft(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
            newlogTV.choiceRight(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
            newlogTV.choiceAccurate(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
            newlogTV.reward(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
            newlogTV.pCorrectLeft(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
            newlogTV.pCorrectRight(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
            newlogTV.chosenPair(ismember(logfile_trials.response_runs_keys, '[]') | ismember(logfile_trials.response_runs_keys, 'None')) = NaN;
        end

        if ismember('session', logfile_trials.Properties.VariableNames) && logfile_trials.session(1) == i%checks whether session is logged and identical to order number
            newlogTV.session = logfile_trials.session;
        else
            warning(['wrong session number in ' filename])
            newlogTV.session = repelem(i, height(newlogTV))';
        end

        newlogTV.trial = logfile_trials.trials_runs_thisN+1; %logfile_trials.xy %links: macht Spalte mit Name ID, rechts: man definiert, was in diese Spalte kommt
        newlogTV.filename = repelem({filename}, height(newlogTV))';
        
        newlogTV = [newlogTV(:,1) newlogTV(:,26) newlogTV(:,24) newlogTV(:,25) newlogTV(:,2:23)];

        newlogTV.triplet = [char(newlogTV.mod2Stim) num2str(newlogTV.visStimLeft) num2str(newlogTV.visStimRight)];

        D = [D;newlogTV];

        % SAVE D
        save(details.analysisfile, 'D')
    end
end
