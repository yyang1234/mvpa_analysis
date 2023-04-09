function accu = mvpa_CrossModal(opt)

    % main function which loops through masks and subjects to calculate the
    % decoding accuracies for given conditions.
    % dependant on SPM + CPP_SPM and CosMoMvpa toolboxes
    % the output is compatible for R visualisation, it gives .csv file as well
    % as .mat file

    % get the smoothing parameter for 4D map
    funcFWHM = opt.fwhm.func;

    % choose masks to be used
    opt = mvpa_chooseMask(opt);

    % set output folder/name
    savefileMat = fullfile(opt.dir.cosmo, ...
    [opt.taskName{1}, ... 
    opt.maskLabel{1}, ...
    'Decoding_s', ...
    num2str(funcFWHM), ...
    '_ratio', num2str(opt.mvpa.ratioToKeep), ...
    '_', datestr(now, 'yyyymmddHHMM'), '.mat']);

    savefileCsv = fullfile(opt.dir.cosmo, ...
    [opt.taskName{1}, ...
    'Decoding_s', ...
    num2str(funcFWHM), ...
    '_ratio', num2str(opt.mvpa.ratioToKeep ), ...
    '_', datestr(now, 'yyyymmddHHMM'), '.csv']);

    %% MVPA options

    % set cosmo mvpa structure
    condLabelNb = [1 2 3 4];
    condLabelName = {'2', '3', '4', '5'};
    modalityLabelName = {'aud_num', 'aud_seq', 'vis_num', 'vis_seq', 'vis_sim'};
    modalityLabelNb = [1 2 3 4 5];
%     decodingConditionList = {'trainanum_testaseq', ...
%         'trainanum_testvnum', 'trainanum_testvseq', ...
%         'trainanum_testvsim', 'trainaseq_testvnum', ...
%         'trainaseq_testvseq', 'trainaseq_testvsim', ...
%         'trainvnum_testvseq', 'trainvnum_testvsim', ...
%         'trainvseq_testvsim'};
    stim = 1:5;
    modalityPairs = sort(nchoosek(stim,2), 2, 'ascend');
    labels= {'aud_num', 'aud_seq', 'vis_num', 'vis-seq',...
             'vis_sim'};
%     modalityPairs = [1 2; 1 3; 1 4; 1 5; 2 3; 2 4; 2 5; 3 4; 3 5; 4 5];
%     decodingConditionList = {'trainanum_testaseq', 'trainanum_testvnum', ...
%         'trainanum_testvseq','trainanum_testvsim', 'trainaseq_testvnum', ...
%         'trainaseq_testvseq','trainaseq_testvsim', 'trainvnum_testvseq', ...
%         'trainvnum_testvsim','trainvseq_testvsim'};

    %% let's get going!

    % set structure array for keeping the results
    accu = struct( ...
                  'subID', [], ...
                  'mask', [], ...
                  'accuracy', [], ...
                  'prediction', [], ...
                  'maskVoxNb', [], ...
                  'choosenVoxNb', [], ...
                  'image', [], ...
                  'ffxSmooth', [], ...
                  'roiSource', [], ...
                  'decodingCondition', [], ...
                  'permutation', [], ...
                  'imagePath', []);

    count = 1;

    for iSub = 1:numel(opt.subjects)

        % get FFX path
        subID = opt.subjects{iSub};
        ffxDir = getFFXdir(subID, opt);

        % get subject folder name
        subFolder = ['sub-', subID];

        for iImage = 1:length(opt.mvpa.map4D)

            for iMask = 1:length(opt.maskName)

                % choose the mask
                mask = fullfile(opt.dir.rois, subFolder, opt.maskName{iMask});
                %         if strcmp(roiSource, 'freesurfer') || strcmp(roiSource, 'hmat')
                %           mask = fullfile(opt.maskPath, subFolder, [opt.maskBaseName, opt.maskName{iMask}]);
                %         end

                % display the used mask
                disp(opt.maskName{iMask});

                % 4D image
                imageName = ['sub-',num2str(subID),...
                        '_task-',char(opt.taskName),...
                        '_space-',char(opt.space),'_desc-4D_', ...
                        opt.mvpa.map4D{iImage},'.nii'];
                image = fullfile(ffxDir, imageName);

                for iModality = 1:length(modalityPairs) % see the types in decoding conditionlist
                
%                     decodingCondition = decodingConditionList(iModality);
                
                    if iModality == 1
                        test = 2;
                    elseif iModality == 2
                        test = 3;
                    elseif iModality == 3
                        test = 4;
                    elseif iModality == 4
                        test = 5;
                    elseif iModality == 5
                        test = 3;
                    elseif iModality == 6
                        test = 4;
                    elseif iModality == 7
                        test = 5;
                    elseif iModality == 8
                        test = 4;
                    elseif iModality == 9
                        test = 5;
                    elseif iModality == 10
                        test = 5;
                    end
                    
%                     if iModality == 1
%                         test = 1;
%                     elseif iModality == 2
%                         test = 1;
%                     elseif iModality == 3
%                         test = 1;
%                     elseif iModality == 4
%                         test = 1;
%                     elseif iModality == 5
%                         test = 2;
%                     elseif iModality == 6
%                         test = 2;
%                     elseif iModality == 7
%                         test = 2;
%                     elseif iModality == 8
%                         test = 3;
%                     elseif iModality == 9
%                         test = 3;
%                     elseif iModality == 10
%                         test = 4;
%                     end

                    % load cosmo input
                    ds = cosmo_fmri_dataset(image, 'mask', mask);

                    % Getting rid off zeros
                    zeroMask = all(ds.samples == 0, 1);
                    ds = cosmo_slice(ds, ~zeroMask, 2);

                    % set cosmo structure
                    ds = c_setCosmoStructure(opt, ds, condLabelNb, condLabelName, modalityLabelNb, modalityLabelName);
                    
                    % Demean  every pattern to remove univariate effect differences
                    meanPattern = nanmean(ds.samples,2);  % get the mean for every pattern
                    meanPattern = repmat(meanPattern,1,size(ds.samples,2)); % make a matrix with repmat
                    ds.samples  = ds.samples - meanPattern; % remove the mean from every every point in each pattern

                    % % %         % slice the ds according to your targers (choose your
                    % % %         % train-test conditions
                    % % %         ds = cosmo_slice(ds, ds.sa.targets == 3 | ds.sa.targets == 4);%%

                    % Slice the dataset accroding to modality
                    ds = cosmo_slice(ds,ds.sa.modality == modalityPairs(iModality,1) | ds.sa.modality == modalityPairs(iModality,2)) ;
                    decodingCondition = [labels{modalityPairs(iModality,1)},'_vs_',...
                                        labels{modalityPairs(iModality,2)}];

%                     modIdx = (ds.sa.modality == modalityPairs(iModality,1)) ...
%                             | (ds.sa.modality == modalityPairs(iModality,2));
%                     ds = cosmo_slice(ds,modIdx);

                    %         idx = strcmp(ds.sa.labels,'vertical') | strcmp(ds.sa.labels,'horizontal');

                    ds = cosmo_slice(ds, strcmp(ds.sa.labels, '2') | strcmp(ds.sa.labels, '3')...
                        | strcmp(ds.sa.labels, '4') | strcmp(ds.sa.labels, '5'));

                    % remove constant features
                    ds = cosmo_remove_useless_data(ds);

                    % calculate the mask size
                    maskVoxel = size(ds.samples, 2);

                    % partitioning, for test and training : cross validation
                    % partitions = cosmo_nfold_partitioner(ds);
                    partitions = cosmo_nchoosek_partitioner(ds, 1, 'modality', test);
                    % partitions = cosmo_nfold_partitioner(ds);
                    %what's the difference between cosmo_nfold and cosmo_nchoosek

                    % define the voxel number for feature selection
                    % set ratio to keep depending on the ROI dimension
                    % if SMA, double the voxel number
                    %         if strcmpi(maskLabel{iMask}, 'sma')
                    %            opt.mvpa.feature_selection_ratio_to_keep = 2 * opt.mvpa.ratioToKeep;
                    %         else
                    %            opt.mvpa.feature_selection_ratio_to_keep = opt.mvpa.ratioToKeep;
                    %         end

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
                    accu(count).ffxSmooth = funcFWHM;
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
                        % manaully randomize the targets (because of cross-modal error)
                        % In every modality seperatly and in every chunk,
                        % randomize the directions
                        
                        for iChunk=1:length(unique(ds.sa.chunks))
                            for iTestModality = 1:length(unique(ds.sa.modality))
                                indMod = unique(ds.sa.modality);
                                ds0.sa.targets(ds.sa.chunks==iChunk & ...
                                    ds.sa.modality==indMod(iTestModality)) = ...
                                    Shuffle(ds.sa.targets(ds.sa.chunks==iChunk & ...
                                    ds.sa.modality==indMod(iTestModality)));
                            end
                        end
                        
%                             ds0.sa.targets = cosmo_randomize_targets(ds);
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

                    fprintf(['Sub'  subID ' - area: ' opt.maskLabel{iMask} ...
                             ', accuracy: ' num2str(accuracy) '\n\n\n']);

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

function ds = setCosmoStructure(opt, ds, condLabelNb, condLabelName, modalityLabelNb, modalityLabelName)
    % sets up the target, chunk, labels by stimuli condition labels, runs,
    % number labels.

    % design info from opt
    nbRun = opt.mvpa.nbRun;
    betasPerCondition = opt.mvpa.nbTrialRepetition;

    % chunk (runs), target (condition), labels (condition names)
    conditionPerRun = length(condLabelNb);
    betasPerRun = betasPerCondition * conditionPerRun;

    chunks = repmat((1:nbRun)', 1, betasPerRun * conditionPerRun);
    chunks = chunks(:);

    targets = repmat(condLabelNb', 1, nbRun)';
    targets = targets(:);
    targets = repmat(targets, betasPerCondition, 2);
    targets = targets(:); % IQRA

    condLabelName = repmat(condLabelName', 1, nbRun)';
    condLabelName = condLabelName(:);
    condLabelName = repmat(condLabelName, betasPerCondition, 2);
    condLabelName = condLabelName(:);

    modalityLabelName = repmat(modalityLabelName', 1, nbRun * conditionPerRun)';
    modalityLabelName = modalityLabelName(:);
    modalityLabelName = repmat(modalityLabelName, betasPerCondition, 1);

    modalityLabelNb = repmat(modalityLabelNb', 1, nbRun * conditionPerRun)';
    modalityLabelNb = modalityLabelNb(:);
    modalityLabelNb = repmat(modalityLabelNb, betasPerCondition, 1);

    %   modality = repmat(modalityLabelNb',1,nbRun*conditionPerRun)';
    %   modality = modality(:) ;

    % assign our 4D image design into cosmo ds git
    ds.sa.targets = targets;
    ds.sa.chunks = chunks;
    ds.sa.labels = condLabelName;
    ds.sa.modality = modalityLabelNb;

    % figure; imagesc(ds.sa.chunks);

end
