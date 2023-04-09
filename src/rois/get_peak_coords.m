% (C) Copyright 2020 CPP ROI developers

% Shows how to get the peak coordinate within a ROI
% roiImage = extractRoiFromAtlas(pwd, 'wang', 'V1v', 'L');
roiImage = fullfile(pwd, '..','..','roi_mask', 'rsemantic_association-test_z_FDR_0.01.nii');
% Data to read the maximum from
% gunzip(fullfile(pwd, 'inputs', '*.gz'));
dataImage = fullfile(pwd, '..','..','..','outputs','derivatives','bidspm-stats',...
    'sub-001', 'task-wordLocalizer_space-IXI549Space_FWHM-6',...
    'spmT_0003.nii');
%/Users/yiyang/DATA/numMVPA_analysis
%/outputs/derivatives/bidspm-stats/sub-001/
%task-wordLocalizer_space-IXI549Space_FWHM-6
%/spmT_0003.nii
% If there is no value above a certain threshold the function will return NaN
threshold = 1;

% The image and the ROI must have the same dimension if we want to use the threshold option
reslicedImages = resliceRoiImages(dataImage, roiImage);

% Get to work.
[worldCoord, voxelCoord, maxVal] = getPeakCoordinates(dataImage, reslicedImages, threshold);
