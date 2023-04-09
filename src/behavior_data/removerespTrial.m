%% import data
clear;

this_dir = fileparts(mfilename('fullpath'));
root_dir = fullfile(this_dir, '..', '..', '..');
opt.dir.raw = fullfile(root_dir, 'inputs', 'raw','sub-006','ses-02','func');

taskNames = {'wordLocalizer','numMVPA'};
% create a pattern to look for in the folder
filePattern = ['*',taskNames{2},'*_events.tsv'];
% find all the .tsv files
tsvFiles = dir(fullfile(opt.dir.raw,filePattern));

for iFile = 1:length(tsvFiles)
    
    tsvFileName = tsvFiles(iFile).name;
    tsvFileFolder = tsvFiles(iFile).folder;
    tsvFile = fullfile(tsvFileFolder, tsvFileName);
    %open file
    fid = fopen(tsvFile,'r');
    %read first line - header
    tline = fgetl(fid);
    %extract the headers
    variableNames = strsplit(tline,'\t');
    nVars = length(variableNames);
    
    data = {};
    c = 1;
    % read line by line
    while ~feof(fid)
        
        tline = fgetl(fid);
        tmp = strsplit(tline,'\t');
        
        if all(tmp{3} == 'response')
            data = data(1:end-1,:);
            c = c-1;
            % if trialtype for current trial is 'response'
            % delete this trial and one trial before
        else
            data(c,:) = tmp;
            c = c+1;
        end

    end
    
    fclose(fid);
    
    tbl = cell2table(data);
    %write table title
    tbl.Properties.VariableNames = variableNames;
    
%     save with suffixes
%     outputTag = '_removeRESP.tsv';
%     outputFileName = strrep(tsvFileName, '.tsv', outputTag);
%     bids.util.tsvwrite(fullfile(tsvFileFolder, outputFileName), tbl);

%   save without suffixes
    bids.util.tsvwrite(fullfile(tsvFileFolder, tsvFileName), tbl);
end
