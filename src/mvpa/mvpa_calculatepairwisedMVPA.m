function accu = mvpa_calculatepairwisedMVPA(opt)

% main function which loops through masks and subjects to calculate the
% decoding accuracies for given conditions.
% dependant on SPM + CPP_SPM and CosMoMvpa toolboxes
% the output is compatible for R visualisation, it gives .csv file as well
% as .mat file

funcFWHM = opt.fwhm.func;
opt.decodingCondition = {'pairwise_decoding'};

% choose masks to be used
opt = mvpa_chooseMask(opt);
%get opt.maskname & opt.masklabel

% set output folder/name
savefileMat = fullfile(opt.dir.cosmo, ...
    [opt.taskName{1}, ... 
    opt.maskLabel{1}, ...
    'pairwiseDecoding_', ...
    num2str(funcFWHM), ...
    '_voxNb', num2str(opt.mvpa.ratioToKeep), ...
    '_', datestr(now, 'yyyymmddHHMM'), '.mat']);

savefileCsv = fullfile(opt.dir.cosmo, ...
    [opt.taskName{1}, ...
    'pairwiseDecoding_', ...
    num2str(funcFWHM), ...
    '_voxNb', num2str(opt.mvpa.ratioToKeep ), ...
    '_', datestr(now, 'yyyymmddHHMM'), '.csv']);

%% let's get going!

% set structure array for keeping the results
accu = struct( ...
    'subID', [], ...
    'mask', [], ...
    'accuracy', [], ...
    'decodingCondition', [], ...
    'maskVoxNb', [], ...
    'choosenVoxNb', [], ...
    'image', [], ...
    'ffxSmooth', [], ...
    'roiSource', [], ...
    'permutation', [], ...
    'prediction', [], ...
    'imagePath', []);
% decodingCondition = {'aud_num', 'aud_seq', 'vis_num','vis_seq','vis_sim'};
modalityLabelName = {'aud_num', 'aud_seq', 'vis_num', 'vis_seq', 'vis_sim'};
modalityLabelNb = [1 2 3 4 5];
count = 1;

for iSub = 1:numel(opt.subjects)

    % get FFX path
    subID = opt.subjects{iSub};
    ffxDir = getFFXdir(subID, opt);

    % get subject folder name
    subFolder = ['sub-', subID];

    for iImage = 1:length(opt.mvpa.map4D)

        subMasks = opt.maskName(startsWith(opt.maskName,...
                   strcat('sub-', subID)));
        %how many mask does the current subj have

        for iMask = 1:length(subMasks)

            % choose the mask
            mask = fullfile(opt.dir.rois, subFolder, 'neurosynth', subMasks{iMask});

            % display the used mask
            disp(subMasks{iMask});

            % 4D image
            imageName = ['sub-',num2str(subID),...
                        '_task-',char(opt.taskName),...
                        '_space-',char(opt.space),'_desc-4D_', ...
                        opt.mvpa.map4D{iImage},'.nii'];
            image = fullfile(ffxDir, imageName);
            
            [condLabelName, condLabelNb, decodingPairs] = mvpa_assignDecodingConditions(opt);
            
            for mod = 1:length(modalityLabelNb)
                for pair = 1:size(decodingPairs,1)

                    % load cosmo input
                    ds = cosmo_fmri_dataset(image, 'mask', mask);

                    % Getting rid off zeros
                    zeroMask = all(ds.samples == 0, 1);
                    ds = cosmo_slice(ds, ~zeroMask, 2);

                    % set cosmo structure
                    ds = c_setCosmoStructure(opt, ds, condLabelNb, condLabelName, modalityLabelNb, modalityLabelName);

                    % slice the ds according to your targers (choose your
                    % train-test conditions
                    
                    ds = cosmo_slice(ds,ds.sa.modality == modalityLabelNb(mod)) ;
                    decodingCondition = [num2str(decodingPairs(pair,1)),'_vs_',...
                                        num2str(decodingPairs(pair,2))];

                    ds = cosmo_slice(ds, ds.sa.targets == decodingPairs(pair,1) |...
                                     ds.sa.targets == decodingPairs(pair,2)); 

                                 % can't find a smarter way for now

                    % remove constant features
                    ds = cosmo_remove_useless_data(ds);

                    % calculate the mask size
                    maskVoxel = size(ds.samples, 2);

                    % partitioning, for test and training : cross validation
                    partitions = cosmo_nfold_partitioner(ds);

                    % use the ratios, instead of the voxel number:
                    opt.mvpa.feature_selection_ratio_to_keep = opt.mvpa.ratioToKeep;

                    % ROI mvpa analysis
                    [pred, accuracy] = cosmo_crossvalidate(ds, ...
                        @cosmo_classify_meta_feature_selection, ...
                        partitions, opt.mvpa);

                    %%

                    %         ratios_to_keep = .05:.05:.95;
                    %         nratios = numel(ratios_to_keep);
                    %
                    %         accs = zeros(nratios, 1);
                    %
                    %         for k = 1:nratios
                    %           opt.mvpa.feature_selection_ratio_to_keep = ratios_to_keep(k);
                    %
                    %           [pred, acc] = cosmo_crossvalidate(ds, ...
                    %                                             @cosmo_meta_feature_selection_classifier, ...
                    %                                             partitions, opt.mvpa);
                    %           accs(k) = acc;
                    %         end
                    %
                    %         plot(ratios_to_keep, accs);
                    %         xlabel('ratio of selected feaures');
                    %         ylabel('classification accuracy');
                    %
                    %         accuracy = max(accs);
                    %         maxRatio = ratios_to_keep(accs == max(accs));

                    %% store output
                    accu(count).subID = subID;
                    accu(count).mask = opt.maskLabel{iMask};
                    accu(count).maskVoxNb = maskVoxel;
                    accu(count).choosenVoxNb = opt.mvpa.feature_selection_ratio_to_keep;
                    % accu(count).choosenVoxNb = round(maskVoxel * maxRatio);
                    % accu(count).maxRatio = maxRatio;
                    accu(count).image = opt.mvpa.map4D{iImage};
                    accu(count).ffxSmooth = opt.fwhm.func;
                    accu(count).accuracy = accuracy;
                    accu(count).prediction = pred;
                    accu(count).imagePath = image;
                    %         accu(count).roiSource = roiSource;
                    accu(count).decodingCondition = decodingCondition;


                    %% PERMUTATION PART
                    if opt.mvpa.permutate  == 1
                        % number of iterations
                        nbIter = 100;

                        % allocate space for permuted accuracies
                        acc0 = zeros(nbIter, 1);

                        % make a copy of the dataset
                        ds0 = ds;

                        % for _niter_ iterations, reshuffle the labels and compute accuracy
                        % Use the helper function cosmo_randomize_targets
                        for k = 1:nbIter
                            ds0.sa.targets = cosmo_randomize_targets(ds);
                            [~, acc0(k)] = cosmo_crossvalidate(ds0, ...
                                @cosmo_classify_meta_feature_selection, ...
                                partitions, opt.mvpa);
                        end

                        p = sum(accuracy < acc0) / nbIter;
                        fprintf('%d permutations: accuracy=%.3f, p=%.4f\n', nbIter, accuracy, p);

                        % save permuted accuracies
                        accu(count).permutation = acc0';
                    end

                    % increase the counter and allons y!
                    count = count + 1;

                    fprintf(['Sub'  subID ': ' decodingCondition 'in' modalityLabelName{mod} ...
                        ' in ' opt.maskLabel{iMask} ' - accuracy: ' num2str(accuracy) '\n\n\n']);
                end
            end
        end
    end
end
%% save output

% mat file
save(savefileMat, 'accu');

% csv but with important info for plotting
csvAccu = rmfield(accu, 'permutation');
csvAccu = rmfield(csvAccu, 'prediction');
csvAccu = rmfield(csvAccu, 'imagePath');
writetable(struct2table(csvAccu), savefileCsv);

end

function ds = setCosmoStructure(opt, ds, condLabelNb, condLabelName)
  % sets up the target, chunk, labels by stimuli condition labels, runs,
  % number labels.

  % design info from opt
  nbRun = opt.mvpa.nbRun;
  betasPerCondition = opt.mvpa.nbTrialRepetition;

  % chunk (runs), target (condition), labels (condition names)
  conditionPerRun = length(condLabelNb);
  betasPerRun = betasPerCondition * conditionPerRun;

  chunks = repmat((1:nbRun)', 1, betasPerRun);
  chunks = chunks(:);

  targets = repmat(condLabelNb', 1, nbRun)';
  targets = targets(:);
  targets = repmat(targets, betasPerCondition, 1);

  condLabelName = repmat(condLabelName', 1, nbRun)';
  condLabelName = condLabelName(:);
  condLabelName = repmat(condLabelName, betasPerCondition, 1);

% assign our 4D image design into cosmo ds git
ds.sa.targets = targets;
ds.sa.chunks = chunks;
ds.sa.labels = condLabelName;

% figure; imagesc(ds.sa.chunks);

end
