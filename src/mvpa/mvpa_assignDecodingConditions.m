function [condLabelName, condNb, decoNbs] = mvpa_assignDecodingConditions(opt)

    %1,2,3,4: vis_sim2-5
    %5,6,7,8: aud_num2-5
    %9,10,11,12: aud_seq2-5
    %13,14 15,16: vis_seq2-5
    %17,18,19,20: vis-num2-5
    
    switch opt.decodingCondition{1}

        case 'within_format'
           condLabelName = {'2aud_num', '2aud_seq', '2vis_num', '2vis_seq', '2vis_sim'...
                            '3aud_num', '3aud_seq', '3vis_num', '3vis_seq', '3vis_sim'...
                            '4aud_num', '4aud_seq', '4vis_num', '4vis_seq', '4vis_sim'...
                            '5aud_num', '5aud_seq', '5vis_num', '5vis_seq', '5vis_sim'};
                        % order the same as labelfold.tsv
            condNb = [1 2 3 4 5 ...
                      6 7 8 9 10 ...
                      11 12 13 14 15 ...
                      16 17 18 19 20];
            decoNbs = [1 6 11 16; 2 7 12 17; 3 8 13 18; ...
                      4 9 14 19; 5 10 15 20];

        case 'pairwise_decoding'
            condNb = [1 2 3 4];
            condLabelName = {'2', '3', '4', '5'};
            stim = 1:4;
            decoNbs = sort(nchoosek(stim,2), 2, 'ascend');
            revdecoNbs = [decoNbs(:,2),decoNbs(:,1)];
            nColumns = size(decoNbs,2);
            decoNbs = [decoNbs,revdecoNbs]';
            decoNbs = reshape(decoNbs(:),nColumns,[])';

%         case 'all'
%             deco = {'frw_v_fpw', 'frw_v_fnw', 'frw_v_ffs', 'frw_v_brw', 'frw_v_bpw', 'frw_v_bnw', 'frw_v_bfs', ...
%                     'fpw_v_fnw', 'fpw_v_ffs', 'fpw_v_brw', 'fpw_v_bpw', 'fpw_v_bnw', 'fpw_v_bfs', ...
%                     'fnw_v_ffs', 'fnw_v_brw', 'fnw_v_bpw', 'fnw_v_bnw', 'fnw_v_bfs', ...
%                     'ffs_v_brw', 'frw_v_bpw', 'frw_v_bnw', 'frw_v_bfs', ...
%                     'brw_v_bpw', 'brw_v_bnw', 'brw_v_bfs', ...
%                     'bpw_v_bnw', 'bpw_v_bfs', ...
%                     'bnw_v_bfs'};
%             condNb = [1 2 3 4 5 6 7 8];
%             decoNbs = [1 2; 1 3; 1 4; 1 5; 1 6; 1 7; 1 8; 2 3; 2 4; 2 5; 2 6; 2 7; 2 8; ...
%                        3 4; 3 5; 3 6; 3 7; 3 8; 4 5; 4 6; 4 7; 4 8; 5 6; 5 7; 5 8; 6 7; 6 8; 7 8];
    end
end