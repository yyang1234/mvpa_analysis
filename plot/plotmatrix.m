%% Decoding data vis
%
% From huge cosmomvpa output table to nice RDM-like matrices
% Valid for different sizes
% 1. dividi table in struct

clear

% warning('off')
% 
% addpath '/Users/cerpelloni/Desktop/GitHub/VisualBraille_data/code'
% 
% mvpa_results = struct;
% 
opt = getOptionMvpa();

% for iSub = 1:numel(opt.subjects)
%     % create structs, otherwise they get confused with tables
%     % fixed for the 5 ROIs of visbra_expertise
%      eval(['mvpa_results.raw.s' char(opt.subjects{iSub}) '.IPS_Left = struct;']);
%      eval(['mvpa_results.raw.s' char(opt.subjects{iSub}) '.IPS_Right = struct;']);
% %     eval(['mvpa_results.raw.s' char(opt.subjects{iSub}) '.PFS_Left = struct;']);
% %     eval(['mvpa_results.raw.s' char(opt.subjects{iSub}) '.LOC_Right = struct;']);
% %     eval(['mvpa_results.raw.s' char(opt.subjects{iSub}) '.PFS_Right = struct;']);
% end

load(fullfile(opt.dir.cosmo, 'numMVPAjumask_lhipspairwiseDecoding_0_voxNb200_202303041103.mat'));

% loop through the matrix and save each chunk in the corresponding file
% Order: sub, image, mask
% Get some numbers

% how many rows are for each subject
singleSub = size(accu,2)/numel(opt.subjects);

% two types of data: bets and tMaps
singleMap = singleSub / 2;

% again, fixed for visbra_expertise
singleROI = singleMap / 2;

%% CASE 'within_script'
%  only within language decoding 
%
% Example:
%       FRW FPW FNW FFS BRW BPW BNW BFS
%   FRW nan  3   3   3  nan nan nan nan
%   FPW  3  nan  3   3  nan nan nan nan
%   FNW  3   3  nan  3  nan nan nan nan
%   FFS  3   3   3  nan nan nan nan nan
%   BRW nan nan nan nan nan  3   3   3
%   BPW nan nan nan nan  3  nan  3   3
%   BNW nan nan nan nan  3   3  nan  3 
%   BFS nan nan nan nan  3   3   3  nan
% 
% Make it symmetrical, make a lot of NaN where there is no decoding
% (3 is a placeholder, if you actually get it as results, it's a problem)

nCond = 4;
condName = {'lhips_beta','lhips_tmap','rhips_beta','rhips_tmap'};
for i = 1:singleROI:size(accu,2)
    row = 1;
    % get chunk %for one single mask and one single map (beta vs t-map)
%     thisChunk = accu(i:i+singleROI-1);
    
    % save original in corresponding variable
%     pathString = "mvpa_results.raw.s" + accu(i).subID + "." + accu(i).mask + "." + accu(i).image;
% %     if strcmp(pathString{1}(27), "-")
% %         pathString{1}(27) = "_";
% %     else
% %         pathString{1}(26) = "_";
% %     end
%     
%     %save chunk in the right struct place
%     eval([char(pathString) ' = thisChunk;']);
%     
%     % Modify pathString: from 'raw' to 'mat'
%     pathString{1}(14:16) = 'mat';
%     
    % Place every accuracy from struct to NxN matrix
    % N is number of conditions we decoded from/to
    % It's only 12 data points, no need to loop 
%     aud_num_positions = [2 21; 3 41; 4 61; 23 42; 24 62; 44 63]; %aud_num
    positions = [2 5; 3 9; 4 13; 7 10; 8 14; 12 15];
    
    %the position of num2-3 2-4 2-5 3-4 3-5 4-5
    decode_cond = {'aud_num','aud_seq','vis_num','vis_seq','vis_sim'};
    NBcond = (1:length(decode_cond));
    
    for iMod = 1:length(decode_cond)
        thisChunk = accu(iMod:iMod+5);
        eval(['virName = [''' char(decode_cond{NBcond(iMod)}) '_mat''];']);
        eval([char(virName) '=nan(nCond)']);
        
        for iMat = 1:size(positions,1) % for each one of our positions in the matrix
            
            % get this accuracy and put it in the matrix place
            thisAccu = thisChunk(iMat).accuracy;
            eval([char(virName) '(positions(' int2str(iMat) ',:)) = thisAccu']);
            
        end
        
            eval(['overall_accu(:,:,' int2str(iMod) ') = aud_num_mat']);
            
            eval([char(condName{row}) '_ov_accu = mean(overall_accu,3);']); %should have four matrix
            % left beta; right beta; left t-map; right t-map;
            row = row + 1;
    
    end
    
    % show figure as heatmap
%     lab_mvpa = {'2', '3', '4', '5'};
% %     mvpa.aud_num = heatmap(lab_mvpa, lab_mvpa, aud_num_mat);
% %     mvpa.aud_seq = heatmap(lab_mvpa, lab_mvpa, aud_seq_mat);
% %     mvpa.vis_num = heatmap(lab_mvpa, lab_mvpa, vis_num_mat);
% %     mvpa.vis_seq = heatmap(lab_mvpa, lab_mvpa, vis_seq_mat);
% %     mvpa.vis_sim = heatmap(lab_mvpa, lab_mvpa, vis_sim_mat);
%     
%     %try to look at the overall mean accu
%     %should have 4 (lhips/rhips+beta/tmap) * 5(modalities) matrix
%     
%     f = figure(i);
%     f = heatmap(lab_mvpa,lab_mvpa,ov_accu{i});
    
end
%     eval([char(pathString) ' = mvpaMat;']);
    
    % show figure as heatmap
    % labels
%     lab_mvpa = {'2', '3', '4', '5'};
%     f = figure;
%     f.Position = [300 300 740 700];
%     
%     % e.g. s002-PFS_Le-beta
%     name = pathString{1}(18:21) + "_" + pathString{1}(23:28) + "_" + accu(i).image(1:4) + "_htmp";
%     
%     eval(['mvpa.' char(name) ' = heatmap(lab_mvpa, lab_mvpa, mvpaMat,' ...
%           '''CellLabelColor'',''none'',''Colormap'',parula,' ...
%           '''GridVisible'',''off'',''FontSize'',20,' ...
%           '''ColorLimits'',[0.2 1],''Units'',''pixels'',''Position'',[70 40 600 600]);']);
%     title = pathString{1}(18:21) + "-" + pathString{1}(23:28) + "-" + accu(i).image(1:4);
%     eval(['mvpa.' char(name) '.Title = ''' char(title) ''';']);
%     
%     %save as .fig and .png
%     savefig(f, name);
%     name = name + ".png";
%     saveas(f,name);
%     
%     % get bar graphs too (why?)
%     nameBar = pathString{1}(18:21) + "_" + pathString{1}(23:28) + "_" + accu(i).image(1:4) + "_bar";
%     
%     barLab = [];
%     barVal = zeros(1,24);
%     % from matrix, make set
%     x = 1;
%     for r = 1:size(mvpaMat,1)
%         for c = 1:size(mvpaMat,2)
%             if not(isnan(mvpaMat(r,c)))
%                 barVal(x) = mvpaMat(r,c);
%                 x = x+1;
%                 thisLab = lab_mvpa(r) + " - " + lab_mvpa(c);
%                 barLab = horzcat(barLab, thisLab);
%             end
%         end
%     end
%     % values
%     barVal([4,7,8,10,11,12,16,19,20,22,23,24]) = [];
%     % labels
%     barLab([4,7,8,10,11,12,16,19,20,22,23,24]) = [];
%     % title
%     barTitle = pathString{1}(18:21) + " " + pathString{1}(23:28) + " " + accu(i).image(1:4);
%     
%     % show barplot
%     f = figure;
%     f.Position = [170,116,793,620];
%     b = bar(barVal,'FaceColor','flat');
%     % cpp colors to distinguish between FR and BR
%     b.CData([1 2 3 4 5 6],:) = [1 0.62 0.29; 1 0.62 0.29; 1 0.62 0.29; 1 0.62 0.29; 1 0.62 0.29; 1 0.62 0.29];
%     b.CData([7 8 9 10 11 12],:) = [0.41 0.67 0.6; 0.41 0.67 0.6; 0.41 0.67 0.6; ...
%                                    0.41 0.67 0.6; 0.41 0.67 0.6; 0.41 0.67 0.6];
%     % graphics part
%     ax = gca;
%     ax.FontSize = 20;
%     ax.Units = 'pixels';
%     ax.Position = [74, 134, 706, 480];
%     xlabel('Areas')
%     ylabel('decoding accuracy')
%     ylim([0 1.1])
%     xticklabels(barLab)
%     yticks([0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1])
%     
%     % save graph
%     
%     % as variable 
%     eval(['mvpa.' char(nameBar) ' = b;']);
%     % as .fig
%     savefig(f, nameBar);
%     % as .png
%     nameBar = nameBar + ".png";
%     saveas(f,nameBar);
% 
% end

%% SAVE SET
save('visbra_expertise-decoding-s5variant.mat','accu','mvpa_results');
