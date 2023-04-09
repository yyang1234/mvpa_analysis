% This script takes one of the TSV of the dataset
% and uses a Replace transformer to replace all values to one condition;

clear;
clc;

addpath(fullfile(pwd, '..','..', 'lib', 'bidspm'));
bidspm init;

filePattern = ['*numMVPA', '*_events.tsv'];
tsvFiles = dir(fullfile(pwd,filePattern));

% create the transformer
replace = struct('key',   {'2aud_num', '2aud_seq', '2vis_num', '2vis_seq', '2vis_sim'...
                            '3aud_num', '3aud_seq', '3vis_num', '3vis_seq', '3vis_sim'...
                            '4aud_num', '4aud_seq', '4vis_num', '4vis_seq', '4vis_sim'...
                            '5aud_num', '5aud_seq', '5vis_num', '5vis_seq', '5vis_sim'}, ...
                 'value', 'allin1');
transformer = struct('Name',  'Replace', ...
                     'Input', 'trial_type', ...
                     'Attribute', 'value', ...
                     'Replace', replace);

for i = 1: length(tsvFiles)
    
    file = tsvFiles(i).name;
    
    data = bids.util.tsvread(fullfile(pwd, file));


% apply the transformer
[new_content, json] = bids.transformers(transformer, data);

bids.util.tsvwrite(fullfile(pwd, file), new_content);

end

% write the new TSV file so we can compare it with the original
% % to make sure the transformer worked as expected
% bids.util.tsvwrite(fullfile(pwd, 'new.tsv'), new_content);
% 
% % write the JSON 'snippet' that corresponds to the transformer
% % so we can copy paste in our bids stats model
% bids.util.jsonwrite(fullfile(pwd, 'new.json'), json);
% 
% %% update the BIDS stats model
% 
% model_file = fullfile(pwd, 'models', 'model-VisuoTact_smdl.json');
% 
% bm = BidsModel('file', model_file);
% bm.Nodes{1}.Transformations.Instructions = transformers;
% bm.write(model_file);
