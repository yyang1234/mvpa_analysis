%% MVPA batch script for numMVPA experiment
%
%

clear;
clc;

warning('off');
addpath(fullfile(pwd, '..', 'lib', 'bidspm'));
bidspm();

% cosmo
cosmo = '/Applications/CoSMoMVPA';
addpath(genpath(cosmo));
cosmo_warning('once');

% libsvm
libsvm = '/Applications/libsvm';
addpath(genpath(libsvm));
% verify it worked
cosmo_check_external('libsvm');

%might helpful in the future
% addpath(genpath(fullfile(pwd, 'subfun')));

opt = getOptionMvpa();

% bidsConcatBetaTmaps(opt);

%% GO GET THAT ACCURACY!

% Within modality

% with in model
mvpaWithin = mvpa_calculateMVPA(opt);
% 
% % within modal pairwised decoding
%  pairwisedMVPA = mvpa_calculatepairwisedMVPA(opt);

% crossmodal
%   mvpaCross = mvpa_CrossModal(opt);