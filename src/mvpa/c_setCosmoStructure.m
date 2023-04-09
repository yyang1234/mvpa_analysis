function ds = c_setCosmoStructure(opt, ds, condLabelNb, condLabelName, modalityLabelNb, modalityLabelName)
    % sets up the target, chunk, labels by stimuli condition labels, runs,
    % number labels.

    % design info from opt
    nbRun = opt.mvpa.nbRun;
    betasPerCondition = opt.mvpa.nbTrialRepetition;

    % chunk (runs), target (condition), labels (condition names)
    nbModality = length(modalityLabelNb);
    conditionPerRun = length(condLabelNb);
    betasPerRun = betasPerCondition * conditionPerRun;

    chunks = repmat((1:nbRun)', 1, 5 * conditionPerRun);
    chunks = chunks(:);

    targets = repmat(condLabelNb', 1, nbRun * nbModality)';%5: 5 modalities
    targets = targets(:);
%     targets = repmat(targets, betasPerCondition, 5);
%     targets = targets(:); % IQRA

    condLabelName = repmat(condLabelName', 1, nbRun * nbModality)';
    condLabelName = condLabelName(:);
%     condLabelName = repmat(condLabelName, betasPerCondition, 5);
%     condLabelName = condLabelName(:);

    modalityLabelName = repmat(modalityLabelName', 1, nbRun)';
    modalityLabelName = modalityLabelName(:);
    modalityLabelName = repmat(modalityLabelName, betasPerCondition, conditionPerRun);
    modalityLabelName = modalityLabelName(:);
    
    modalityLabelNb = repmat(modalityLabelNb', 1, nbRun)';
    modalityLabelNb = modalityLabelNb(:);
    modalityLabelNb = repmat(modalityLabelNb, betasPerCondition, conditionPerRun);
    modalityLabelNb = modalityLabelNb(:);

    %   modality = repmat(modalityLabelNb',1,nbRun*conditionPerRun)';
    %   modality = modality(:) ;

    % assign our 4D image design into cosmo ds git
    ds.sa.targets = targets;
    ds.sa.chunks = chunks;
    ds.sa.labels = condLabelName;
    ds.sa.modality = modalityLabelNb;

    % figure; imagesc(ds.sa.chunks);

end