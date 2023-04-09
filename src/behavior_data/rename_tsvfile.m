clear;

this_dir = fileparts(mfilename('fullpath'));
root_dir = fullfile(this_dir, '..', '..', '..','..','..');
opt.dir.destination = fullfile(root_dir, 'DATA', 'numMVPA_analysis','inputs', 'raw','sub-006','ses-02','func');
opt.dir.raw = fullfile(root_dir, 'Dropbox', 'behavioraldata','sub-006','ses-002','func');
cd(opt.dir.raw);
% addpath('/Users/yiyang/DATA/resting_state_eyemovement/code');


taskNames = {'numerosityLocalizer','visualEventRelated'};%visualEventRelated
% create a pattern to look for in the folder
filePattern = ['*',taskNames{2},'*.tsv'];
jfilePattern = ['*',taskNames{2},'*events','*.json'];

tsvFiles = dir(fullfile(opt.dir.raw,filePattern));
jsonFiles = dir(fullfile(opt.dir.raw,jfilePattern));

runNum = {'09','10','11','12','13','14','15','16','17','18','19',...
            '20','21','22','23','24','25','26'};

for iFile = 1:length(tsvFiles)

        tsvFileName = tsvFiles(iFile).name;
        newtsvFileName = ['sub-006_ses-02_task-numMVPA_run-'...
                        char(runNum{iFile}) '_events.tsv'];
        tsvFileFolder = tsvFiles(iFile).folder;

        movefile(tsvFileName,newtsvFileName);% rename file
        copyfile(newtsvFileName,opt.dir.destination)% move file

end


for iFile = 1:length(jsonFiles)

        jsonFileName = jsonFiles(iFile).name;
        newjsonFileName = ['sub-006_ses-02_task-numMVPA_run-'...
                        char(runNum{iFile}) '_events.json'];

        movefile(jsonFileName,newjsonFileName); % rename file
        copyfile(newjsonFileName,opt.dir.destination)% move file

end

%% rename runs

clear;

this_dir = fileparts(mfilename('fullpath'));
root_dir = fullfile(this_dir, '..', '..', '..','..','..');
opt.dir.destination = fullfile(root_dir, 'DATA', 'numMVPA_analysis','inputs', 'raw','sub-006','ses-02','func');
opt.dir.raw = fullfile(root_dir, 'Dropbox', 'behavioraldata','sub-006','ses-002','func');
cd(opt.dir.destination);
% addpath('/Users/yiyang/DATA/resting_state_eyemovement/code');


taskNames = {'numerosityLocalizer','numMVPA'};%visualEventRelated
% create a pattern to look for in the folder
filePattern = ['*',taskNames{2},'*.tsv'];
jfilePattern = ['*',taskNames{2},'*events','*.json'];
boldPattern = ['*',taskNames{2},'*.nii.gz'];
jboldPattern = ['*',taskNames{2},'*bold','*.json'];

tsvFiles = dir(fullfile(pwd,filePattern));
jsonFiles = dir(fullfile(pwd,jfilePattern));
boldFiles = dir(fullfile(pwd,boldPattern));
jboldFiles = dir(fullfile(pwd,jboldPattern));

runNum = {'09','10','11','12','13','14','15','16','17','18','19',...
            '20','21','22','23','24','25','26'};

for iFile = 1:length(tsvFiles)

        tsvFileName = tsvFiles(iFile).name;
        newtsvFileName = ['sub-006_ses-02_task-numMVPA_run-'...
                        num2str(runNum(iFile)) '_events.tsv'];
        tsvFileFolder = tsvFiles(iFile).folder;

        movefile(tsvFileName,newtsvFileName);% rename file
%         copyfile(newtsvFileName,opt.dir.destination)% move file

end


for iFile = 1:length(jsonFiles)

        jsonFileName = jsonFiles(iFile).name;
        newjsonFileName = ['sub-006_ses-02_task-numMVPA_run-'...
                        num2str(runNum(iFile)) '_events.json'];

        movefile(jsonFileName,newjsonFileName); % rename file
%         copyfile(newjsonFileName,opt.dir.destination)% move file

end

for iFile = 1:length(boldFiles)

        boldFilesName = boldFiles(iFile).name;
        newboldFilesName = ['rsub-006_ses-02_task-numMVPA_run-'...
                        char(runNum{iFile}) '_bold.nii.gz'];

        movefile(boldFilesName,newboldFilesName); % rename file
%         copyfile(newjsonFileName,opt.dir.destination)% move file

end

for iFile = 1:length(jboldFiles)

        jboldFilesName = jboldFiles(iFile).name;
        newjboldFilesName = ['rsub-006_ses-02_task-numMVPA_run-'...
                        char(runNum{iFile}) '_bold.json'];

        movefile(jboldFilesName,newjboldFilesName); % rename file
%         copyfile(newjsonFileName,opt.dir.destination)% move file

end

rboldPattern = ['r','*',taskNames{2},'*.nii.gz'];
rjboldPattern = ['r','*',taskNames{2},'*bold','*.json'];
rboldFiles = dir(fullfile(pwd,rboldPattern));
rjboldFiles = dir(fullfile(pwd,rjboldPattern));

for iFile = 1:length(rboldFiles)

        rboldFilesName = rboldFiles(iFile).name;
        rnewboldFilesName = extractAfter(rboldFilesName,'r');

        movefile(rboldFilesName,rnewboldFilesName); % rename file
%         copyfile(newjsonFileName,opt.dir.destination)% move file

end

for iFile = 1:length(rjboldFiles)

        rjboldFilesName = rjboldFiles(iFile).name;
        rnewjboldFilesName = extractAfter(rjboldFilesName,'r');

        movefile(rjboldFilesName,rnewjboldFilesName); % rename file
%         copyfile(newjsonFileName,opt.dir.destination)% move file

end

