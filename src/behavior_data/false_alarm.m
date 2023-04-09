%import data
clear;

this_dir = fileparts(mfilename('fullpath'));
root_dir = fullfile(this_dir, '..', '..', '..','..','..');
% opt.dir.raw = fullfile(root_dir, 'Dropbox', 'behavioraldata','sub-006','ses-001','func');
opt.dir.raw = fullfile(root_dir, 'DATA', 'numMVPA_analysis','inputs', 'raw','sub-006','ses-02','func');

% cd(opt.dir.raw);
% addpath('/Users/yiyang/DATA/resting_state_eyemovement/code');


taskNames = {'wordLocalizer','numMVPA'};
% create a pattern to look for in the folder
filePattern = ['*',taskNames{2},'*_events.tsv'];

tsvFiles = dir(fullfile(opt.dir.raw,filePattern));

nbrun = length(tsvFiles); %nbrun 18 15 10

A = cell(nbrun,1);


for n = 1:nbrun
    
    tsvFileName = tsvFiles(n).name;
    tsvFileFolder = tsvFiles(n).folder;
    tsvFile = fullfile(tsvFileFolder, tsvFileName);
    A{n} = tdfread(tsvFile);
    
end

%% any repetition?

for i = 1:nbrun
        n = 1:length(A{i,1}.keyName);
        resp{i,1} = setdiff(n,find(A{i,1}.keyName == 'n'));%response
        isTarget{i,1} = find(str2double(cellstr(A{i,1}.target)) == 1)';%target should be response-1
        nbResp(i,:) = length(resp{i,1});
        trialType = cellstr(A{i,1}.trial_type);
        type{i,1} = trialType(resp{i,1}-1);
        target = cellstr(A{i,1}.target);
        ind_fafal{i,1} = find(str2double(target(resp{i,1}-1)) ~= 1);
        type_falal{i,1} = type{i,1}(ind_fafal{i,1});
        uni_type = unique(type_falal{i,1});
    if length(uni_type) == length(type_falal{i,1})
        fprintf('no repetition\n')
    else
        fprintf('repetition detected\n')
    end
    
end

%% check if difference betweem two successive stimuli matters false alarm
for i = 1 : nbrun
    
    false_al = setdiff(resp{i,1}-1,isTarget{i,1});
    bef_false_al = false_al-1; % one trial before false alarm trial
    
    trialType = cellstr(A{i,1}.trial_type);
    numInfo = regexprep(trialType,'\D','');
    numInfo = str2double(numInfo);
    
    fal_diff{i,1} = abs(numInfo(bef_false_al) - numInfo(false_al));
    
end

numInfo_fal_diff{1} = cat(1,fal_diff{:}); %concatenate them into double for further stats
tabulate(numInfo_fal_diff{1});

%% accuracy rate

for i=1:nbrun
    
    target = str2double(cellstr(A{i,1}.target));
    targetTrial = find(target == 1);
    trialType = cellstr(A{i,1}.trial_type);
    answer = trialType(targetTrial+1); %should be {'response'} if correct
    rightHit = length(find(strcmp(answer,'response')));
    accuracyRate(i,1) = rightHit/length(targetTrial);
    
end

total_accu = mean(accuracyRate);
sprintf('%0.3g',total_accu)
%% frequency of false alarm on different pairs

for i=1:nbrun

    false_al = setdiff(resp{i,1}-1,isTarget{i,1});
    bef_false_al = false_al-1; % one trial before false alarm trial
    
    trialType = cellstr(A{i,1}.trial_type);
    pair1_fafal{i,1} = extractBetween(trialType(false_al),2,8);
    pair2_fafal{i,1} =  extractBetween(trialType(bef_false_al),2,8);
    pair_type_fafal{i,1} = strcat(pair1_fafal{i,1},'_',pair2_fafal{i,1});
end

total_pair_type_fafal = cat(1,pair_type_fafal{:});
tabulate(total_pair_type_fafal);

%             Value    Count   Percent
%   vis_num_vis_sim        1      7.14%
%   aud_seq_vis_sim        2     14.29%
%   vis_seq_vis_seq        1      7.14%
%   aud_num_vis_seq        1      7.14%
%   vis_seq_vis_num        2     14.29%
%   aud_num_aud_num        1      7.14%
%   vis_num_aud_seq        1      7.14%
%   vis_seq_vis_sim        2     14.29%
%   vis_num_vis_seq        1      7.14%
%   vis_seq_aud_num        1      7.14%
%   aud_seq_vis_seq        1      7.14%

