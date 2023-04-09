%scripts from Iqra
%use binarised and threshold the maps
%create ROIs
%change the subjects and their peak coordinates
clear
cd('/Users/yiyang/DATA/numMVPA_analysis/code/src/rois');
% run '../lib/CPP_SPM/initCppSpm.m'

%better to create ROIs for one subject at a time because the loop on the
%number of subject and not the id of the subject, so it can misread the opt.sphere.location
opt.subjects={'003'};

opt.roi = {'lhips','rhips'};

opt.dir.this = fileparts(mfilename('fullpath'));
opt.dir.root = fullfile(opt.dir.this, '..', '..', '..');
opt.dir.atlas = fullfile(opt.dir.root, 'atlas');
%list of all the binarised/thresholded neurosynth masks 
opt.maskName = {fullfile(opt.dir.atlas,'leftIPS.nii'),...
                fullfile(opt.dir.atlas,'rightIPS.nii')};

%peak coordinates of the 7 rois for each subject observed manually in SPM
%by applying the mask
% opt.sphere.location = {[-49.4,-34,52.5],[46.8,-39.2,49.6]}; %sub2
opt.sphere.location = {[-44,-44,50],[26,-60,39]}; %sub3: leftIPS;rightIPS
% starting radius
opt.sphere.radius = 10;
% number of voxels in the new masks
opt.sphere.maxNbVoxels = 500;

%creating names of the created ROIs
opt.roiName={strcat(opt.subject,'_hemi-L_space-MNI_label-hips_jumask.nii'),...
            strcat(opt.subject,'_hemi-R_space-MNI_label-hips_jumask.nii'),};
opt.save.roi = true;

% if this is empty new masks are saved in the current directory.
% also create the folders corresponding to  the voxelNb and subject
iSub = 1;
subID = opt.subjects{iSub};
subFolder = ['sub-', subID];
opt.dir.output = fullfile(opt.dir.root, 'outputs','derivatives',...
                'bidspm-rois',subFolder);
outputMaskDir=opt.outputDir;

% 4-D image used for used for referencing 
dataImage = fullfile(opt.dir.root, 'outputs','derivatives','bidspm-stats',...
                subFolder,'task-numMVPA_space-IXI549Space_FWHM-0',...
                'beta_0001.nii');
% dataImage = fullfile('/Users/yiyang/DATA/numMVPA_analysis/outputs/derivatives/bidspm-stats/',...
%     char(opt.subject), '/task-wordLocalizer_space-IXI549Space_FWHM-6', 'spmT_0003.nii'); %


for iSub = 1:length(opt.subject)
    for iRoi = 1:length(opt.roi)
        
        % to create new names for rois for the renameMyRoi function
        subName=char(opt.subject(iSub));
        roiName = char(opt.roi(iRoi));
        voxelNb= num2str(opt.sphere.maxNbVoxels);
        
        %this creates the new Rois by expansion. expansion srarts from the
        %given peak coordinate
        maskName=char(opt.maskName(iRoi));
        sphere.location=cell2mat(opt.sphere.location(iRoi));
        sphere.radius = opt.sphere.radius;
        sphere.maxNbVoxels=opt.sphere.maxNbVoxels;
        
        specification  = struct( ...
                          'mask1', maskName, ...
                          'mask2', sphere);

        mask = createRoi('expand', specification, dataImage, opt.outputDir, opt.save.roi);
        
        %this function renames the saved rois
        renameMyRoi(subName, roiName,'.nii',voxelNb)
        renameMyRoi(subName, roiName,'.json',voxelNb)


        data_expand = spm_summarise(dataImage, mask.roi.XYZmm);

    end
end

%% function

function renameMyRoi(subName, roiName,fileFormat,voxelNb)

    switch roiName
        case 'lhMT'
            hemi='L';
            label='lhMT';
        case 'rhMT'
            hemi='R';
            label='rhMT';
        case'lS1'
            hemi='L';
            label='lS1';
        case'lPC'
            hemi='L';
            label='lPC';
        case 'rPC'
            hemi='R';
            label='rPC';
        case'lMTt'
            hemi='L';
            label='lMTt';
        case 'rMTt'
            hemi='R';
            label='rMTt';
    end 
    
    switch fileFormat
        case '.nii'
            ext='*mask.nii';
        case '.json'
            ext='*mask.json';
    end
    
    subName=char(subName);
    fileInfo = dir(fullfile(fileparts(mfilename('fullpath')),'..', '..','..','derivatives','cpp_roi_glasser',strcat('voxelNb-',num2str(voxelNb)), subName, ext) );
    oldName = fileInfo.name;
    newName = strcat(subName,'_','hemi-',hemi,'_','space-MNI', '_','label-',label,'_', 'vox-',voxelNb,fileFormat);
    movefile( fullfile(fileparts(mfilename('fullpath')),'..', '..','..','derivatives','cpp_roi_glasser',strcat('voxelNb-',num2str(voxelNb)), subName, oldName),...
        char(fullfile(fileparts(mfilename('fullpath')),'..', '..','..','derivatives','cpp_roi_glasser',strcat('voxelNb-',num2str(voxelNb)), subName, newName) ));
    disp(hemi)
    disp(label)
    disp(oldName)
    disp(newName)
    
end