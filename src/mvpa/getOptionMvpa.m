% (C) Copyright 2019 CPP BIDS SPM-pipeline developpers

function opt = getOptionMvpa()
  % returns a structure that contains the options chosen by the user to run
  % decoding with cosmo-mvpa.

  if nargin < 1
    opt = [];
  end

  % suject to run in each group
  opt.subjects = {'003','005','006'}; %{'003','005','006'}
  
  % mask choose to use
  opt.mask = 'activationMap'; %neurosynth  'jubrain'

  % Uncomment the lines below to run preprocessing
  % - don't use realign and unwarp
  opt.realign.useUnwarp = true;

  % we stay in native space (that of the T1)
  opt.space = 'IXI549Space'; % 'individual', 'MNI'
  opt.desc = 'MVPA';

  % The directory where the data are located
  this_dir = fileparts(mfilename('fullpath'));
  root_dir = fullfile(this_dir, '..', '..','..');
  opt.dir.raw = fullfile(root_dir, 'inputs', 'raw');
  opt.dir.derivatives = fullfile(root_dir, 'outputs', 'derivatives');
  opt.dir.preproc = fullfile(root_dir, 'outputs', 'derivatives','bidspm-preproc');
  opt.dir.input = opt.dir.preproc;
  opt.dir.rois = fullfile(root_dir, 'outputs', 'derivatives','bidspm-rois');
  opt.dir.stats = fullfile(root_dir, 'outputs', 'derivatives','bidspm-stats');
  opt.dir.cosmo =  fullfile(root_dir, 'outputs', 'derivatives','CoSMoMVPA');
  % multivariate
  opt.model.file = fullfile(root_dir,'code', 'model', 'model-defaultNumMVPA_smdl.json');

  % task to analyze
  opt.taskName = 'numMVPA';
  opt.pipeline.type = 'stats';

  opt.parallelize.do = false;
  opt.parallelize.nbWorkers = 1;
  opt.parallelize.killOnExit = true;

  %% DO NOT TOUCH
  opt = checkOptions(opt);
  saveOptions(opt);
  % we cannot save opt with opt.mvpa, it crashes

  %% mvpa options

  opt.decodingCondition = {'within_format'};
  
  % define the 4D maps to be used
  opt.fwhm.func = 0;

%   % take the most responsive xx nb of voxels
   opt.mvpa.ratioToKeep = [1000]; % 220  210  0.5

  % set which type of ffx results you want to use
  opt.mvpa.map4D = {'beta', 'tmap'};

  
  % design info
  opt.mvpa.nbRun = [22, 26, 22]; %14(13) 15  10  sub001:16  sub003:22  sub005:26   [16,22,26,22]
  opt.mvpa.nbTrialRepetition = 1;%???1:one beta for each condition per run 2: two beta (I want 1 beta each condition per run though I have repetition)

  % cosmo options
  opt.mvpa.tool = 'cosmo';
  opt.mvpa.normalization = 'zscore';
  opt.mvpa.child_classifier = @cosmo_classify_libsvm;
  opt.mvpa.feature_selector = @cosmo_anova_feature_selector;

  % permute the accuracies ?
  opt.mvpa.permutate = 1;

end