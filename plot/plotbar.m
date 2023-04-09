% extractMeanAccuracies
clear

load("visbra_expertise-decodingWithinScript.mat");

accuFR = accu;
accuBR = accu;

% work on the accu struct
% only get those decodings where fr is involved
% toDelFR = [];
% toDelBR = [];
% for i =1:size(accu,2)
%     if startsWith(accu(i).decodingCondition,"b")
%         toDelFR = vertcat(toDelFR,i);
%     end
%     if startsWith(accu(i).decodingCondition,"f")
%         toDelBR = vertcat(toDelBR,i);
%     end
% end
% accuFR(toDelFR) = [];
% accuBR(toDelBR) = [];

%% FOR FRENCH

AccTable = table('Size',[12 3],'VariableTypes',{'string','string','cell'},'VariableNames',{'sub','label','accuracy'});
accInd = 1;

for j = 1:5:size(accuFR,2)

    AccTable.sub(accInd) = accuFR(j).subID;
    
    % get the label: subject + mask + image
%     if startsWith(accu(j).mask,"l")
        AccTable.label(accInd) = accuFR(j).mask(12:15) + "_" + accuFR(j).image(1:4);
%     else
%         thisAccFR.label(accInd) = accuFR(j).mask([1 2 3 5]) + "_" + accuFR(j).image(1:4);
%     end

    % get the accuracy
    AccTable.accuracy{accInd} = [accuFR(j:j+4).accuracy].';
    
    % store everything
    accInd = accInd + 1;
end

% %% FOR BRAILLE
% 
% thisAccBR = table('Size',[30 3],'VariableTypes',{'string','string','cell'},'VariableNames',{'sub','label','accuracy'});
% accInd = 1;
% 
% for j = 1:6:size(accuBR,2)
% 
%     thisAccBR.sub(accInd) = accuBR(j).subID;
%     
%     % get the label: subject + mask + image
%     if startsWith(accu(j).mask,"V")
%         thisAccBR.label(accInd) = accuBR(j).mask(1:4) + "_" + accuBR(j).image(1:4);
%     else
%         thisAccBR.label(accInd) = accuBR(j).mask([1 2 3 5]) + "_" + accuBR(j).image(1:4);
%     end
% 
%     % get the accuracy
%     thisAccBR.accuracy{accInd} = [accuBR(j:j+5).accuracy].';
%     
%     % store everything
%     accInd = accInd + 1;
% end
%% Plot accuracies for each subject, rudimental is fine


%
s2Lab = AccTable.label(1:4);
s2accInMatrix = zeros(5,4);
% sub-001
for a = 1:4
    s2accInMatrix(:,a) = AccTable.accuracy{a};
end

beta = s2accInMatrix(:,1:2);
t_map = s2accInMatrix(:,3:4);

% sub-003
k=1;
for a = 5:8
    s3accInMatrix(:,k) = AccTable.accuracy{a};
    k=k+1;
end
beta = s3accInMatrix(:,1:2);
t_map = s3accInMatrix(:,3:4);

% sub-005
k=1;
for a = 9:12
    s5accInMatrix(:,k) = AccTable.accuracy{a};
    k=k+1;
end
beta = s5accInMatrix(:,1:2);
t_map = s5accInMatrix(:,3:4);

f = figure;
f.Position = [100 100 1200 800];
boxplot(aa);
ax = gca;
ax.FontSize = 20;
xlabel('rois')
ylabel('decoding accuracy')
ylim([0 0.5]);
title('sub-001 - mean accuracy in aud-num')
xticklabels(s2Lab);
yticks([0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1]);
yline(0.25);
% save images
savefig(f, "s1accuAud-num");
saveas(f,"s1accuAud-num.png");

%%
% bar plot for sub1
decodingCondition = {'aud-num', 'aud-seq', 'vis-num','vis-seq','vis-sim'};
ax = gca(f);
bar(beta);
yline(0.25);
ylim([0 0.4]);
xticklabels(decodingCondition);
ax.FontSize = 20;
f = title('beta');
%%
% sub-002 BRAILLE
s2Lab = thisAccBR.label(1:10);
s2accInMatrix = zeros(6,10);
for a = 1:10
    s2accInMatrix(:,a) = thisAccBR.accuracy{a};
end
f = figure;
f.Position = [100 100 1200 800];
boxplot(s2accInMatrix)
ax = gca;
ax.FontSize = 20;
xlabel('Areas')
ylabel('decoding accuracy')
ylim([0 1.1])
title('sub-002 - mean accuracy in BR')
xticklabels(s2Lab)
% yticks([0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1])

% save images 
savefig(f, "s2accuBR");
saveas(f,"s2accuBR.png");

% sub-003 FRENCH
s3Lab = thisAccFR.label(1:10);
s3accInMatrix = zeros(6,10);
for a = 11:20
    s3accInMatrix(:,a-10) = thisAccFR.accuracy{a};
end
f = figure;
f.Position = [100 100 1200 800];
boxplot(s3accInMatrix)
ax = gca;
ax.FontSize = 20;
xlabel('Areas')
ylabel('decoding accuracy')
ylim([0 1.1])
title('sub-003 - mean accuracy in FR')
xticklabels(s3Lab)
yticks([0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1])

% save images 
savefig(f, "s3accuFR");
saveas(f,"s3accuFR.png");


% sub-003 BRAILLE
s3Lab = thisAccBR.label(1:10);
s3accInMatrix = zeros(6,10);
for a = 11:20
    s3accInMatrix(:,a-10) = thisAccBR.accuracy{a};
end
f = figure;
f.Position = [100 100 1200 800];
boxplot(s3accInMatrix)
ax = gca;
ax.FontSize = 20;
xlabel('Areas')
ylabel('decoding accuracy')
ylim([0 1.1])
title('sub-003 - mean accuracy in BR')
xticklabels(s3Lab)
yticks([0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1])

% save images 
savefig(f, "s3accuBR");
saveas(f,"s3accuBR.png");


% sub-005 FRENCH
s5Lab = thisAccFR.label(1:10);
s5accInMatrix = zeros(6,10);
for a = 21:30
    s5accInMatrix(:,a-20) = thisAccFR.accuracy{a};
end
f = figure;
f.Position = [100 100 1200 800];
boxplot(s5accInMatrix)
ax = gca;
ax.FontSize = 20;
xlabel('Areas')
ylabel('decoding accuracy')
ylim([0 1.1])
title('sub-005 - mean accuracy in FR')
xticklabels(s5Lab)
yticks([0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1])

% save images 
savefig(f, "s5accuFR");
saveas(f,"s5accuFR.png");


% sub-005 BRAILLE
s5Lab = thisAccBR.label(1:10);
s5accInMatrix = zeros(6,10);
for a = 21:30
    s5accInMatrix(:,a-20) = thisAccBR.accuracy{a};
end
f = figure;
f.Position = [100 100 1200 800];
boxplot(s5accInMatrix)
ax = gca;
ax.FontSize = 20;
xlabel('Areas')
ylabel('decoding accuracy')
ylim([0 1.1])
title('sub-005 - mean accuracy in BR')
xticklabels(s5Lab)
yticks([0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1])

% save images 
savefig(f, "s5accuBR");
saveas(f,"s5accuBR.png");
