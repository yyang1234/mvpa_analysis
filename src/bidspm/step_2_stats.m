% (C) Copyright 2019 bidspm developers

clear;
clc;

addpath(fullfile(pwd, '..', 'lib', 'bidspm'));
bidspm();

this_dir = fileparts(mfilename('fullpath'));
% model_file = fullfile(this_dir,'..', 'code','model', 'model-taskName_num.json');
% model_file = '/Users/yiyang/DATA/numMVPA_analysis/code/models/model-numMVPA_smdl.json';
model_file = fullfile(pwd,'..', 'models', 'model-wordLocalizer_smdl.json');
root_dir = fullfile(this_dir, '..', '..');

bids_dir = fullfile(root_dir, 'inputs', 'raw');
output_dir = fullfile(root_dir, 'outputs', 'derivatives');
preproc_dir = fullfile(root_dir, 'outputs', 'derivatives', 'bidspm-preproc');
subject_label = '006';
% addpath(fullfile(pwd, '..', '..'));
% bidspm();
% 
% this_dir = fileparts(mfilename('fullpath'));
% model_file = fullfile(this_dir, 'models', 'model-visMotionLoc_smdl.json');
% root_dir = fullfile(this_dir, '..', '..', '..', '..');
% bids_dir = fullfile(root_dir, 'inputs', 'raw');
% output_dir = fullfile(root_dir, 'outputs', 'derivatives');
% preproc_dir = fullfile(root_dir, 'outputs', 'derivatives', 'bidspm-preproc');

% TODO via BIDS api
% bidsRFX('meananatandmask', opt);

%% subject level

results.nodeName = 'run_level';
results.name = {'number_gt_word','word_gt_number'};
results.png = true();
results.csv = true();
results.p = 0.05;
results.MC = 'FWE';
results.binary = true();
results.montage.do = true();
results.montage.slices = -4:2:16;
results.montage.orientation = 'axial';
results.montage.background = struct('suffix', 'T1w', ...
                                    'desc', 'preproc', ...
                                    'modality', 'anat');
results.nidm = true();

opt.results = results;

bidspm(bids_dir, output_dir, 'subject', ...
       'participant_label', {subject_label}, ...
       'action', 'stats', ...
       'task', {'wordLocalizer'},...
       'preproc_dir', preproc_dir, ...
       'model_file', model_file, ...
       'options', opt,...
       'fwhm',6);

%% dataset level

% opt.results = struct('nodeName',  'dataset_level', ...
%                      'name', {{'VisMot_gt_VisStat'}}, ...
%                      'Mask', false, ...
%                      'MC', 'none', ...
%                      'p', 0.05, ...
%                      'k', 10, ...
%                      'NIDM', true);
% 
% bidspm(bids_dir, output_dir, 'dataset', ...
%        'action', 'stats', ...
%        'preproc_dir', preproc_dir, ...
%        'model_file', model_file, ...
%        'options', opt);

%% preprocessing for MVPA

% opt = getOptionMvpa();
% bidsConcatBetaTmaps(opt);
