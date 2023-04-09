% (C) Copyright 2019 bidspm developers

clear;
clc;

addpath(fullfile(pwd, '..', 'lib', 'bidspm'));
bidspm();

this_dir = fileparts(mfilename('fullpath'));
root_dir = fullfile(this_dir, '..', '..','..');

bids_dir = fullfile(root_dir, 'inputs', 'raw');
output_dir = fullfile(root_dir, 'outputs', 'derivatives');
subject_label = '006';

opt.query.ses = '02';
% opt.query.run = {'02', '05'}

bidspm(bids_dir, output_dir, 'subject', ...
       'participant_label', {subject_label}, ...
       'action', 'preprocess', ...
       'ignore', {'unwarp'},...
       'task', {'numMVPA'}, ...%,'numMVPA','aliceLocalizer',wordLocalizer
       'options', opt,...
       'space', {'IXI549Space'},...
       'fwhm',0); %try 0, 2 ,4
   %        'options', opt,...