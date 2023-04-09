%binarise and threshold the maps
clear

run ../../../../'CPP_ROI'/initCppRoi;

%use for referecing to an image
opt.subjects={'sub-001'}; 

%lit of all the neurosynth masks 
neurosynthMaskName={'space-MNI_atlas-neurosynth_label-nsLeftHips_desc-p00pt00_mask.nii',...
    'space-MNI_atlas-neurosynth_label-nsRightHips_desc-p00pt00_mask.nii'};

%for each mask, you binarise them ny thresholding them
for iNeurosynthMaskName = 1: length(neurosynthMaskName)
    zMap = fullfile(pwd, 'inputs', char(neurosynthMaskName(iNeurosynthMaskName)));
    % dataImage = fullfile(pwd, 'inputs', 'TStatistic.nii');
    dataImage = fullfile('/Users/yiyang/DATA/numMVPA_analysis/outputs/derivatives/bidspm-stats/',char(opt.subjects), '/task-numMVPA_space-IXI549Space_FWHM-2', 'beta_0001.nii');
   zMap = fullfile('/Users/yiyang/DATA/numMVPA_analysis/outputs/derivatives/bidspm-rois/sub-001/tsub-001_space-MNI_atlas-neurosynth_label-nsLeftHipsExpandVox230_desc-p00pt00p00pt00_mask.nii');
    opt.unzip.do = false;
    opt.save.roi = true;
    opt.outputDir = pwd; % if this is empty new masks are saved in the current directory.
    if opt.save.roi
      opt.reslice.do = true;
    else
      opt.reslice.do = false;
    end
    [roiName, zMap] = prepareDataAndROI(opt, dataImage, zMap);
end


%% Functions
function [roiName, zMap] = prepareDataAndROI(opt, dataImage, zMap)

  if opt.unzip.do
    gunzip(fullfile('inputs', '*.gz'));
  end

  % give the neurosynth map a name that is more bids friendly
  %
  % space-MNI_label-neurosynthKeyWordsUsed_probseg.nii
  %
  zMap = renameNeuroSynth(zMap);

  if opt.reslice.do
    % If needed reslice probability map to have same resolution as the data image
    %
    % resliceImg won't do anything if the 2 images have the same resolution
    %
    % if you read the data with spm_summarise,
    % then the 2 images do not need the same resolution.
    zMap = resliceRoiImages(dataImage, zMap);
  end

  % Threshold probability map into a binary mask
  % to keep only values above a certain threshold
  threshold = 0; 
  roiName = thresholdToMask(zMap, threshold);
  roiName = removePrefix(roiName, spm_get_defaults('realign.write.prefix'));

end
