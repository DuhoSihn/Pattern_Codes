% Pattern_Codes_Motor
clear all; close all; clc
% addpath(genpath('C:\DATA\Util\matlab_codes'));



%% Get, Pattern Atypicality
% 
% flist = dir( 'data_*.mat' );
% 
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% 
%         load( fname )
% 
%         tic
%         % -----------------------------------------------------------------
% 
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
% 
%         G = bsxfun( @minus, F, mean( F, 1 ) );
%         G = bsxfun( @rdivide, G, std( G, 0, 1 ) );
%         H = bsxfun( @ge, G, median( G, 2 ) );
% 
%         F = double( F );
%         G = double( G );
%         H = double( H );
% 
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
% 
%         X_F = [];
%         X_G = [];
%         X_H = [];
%         tr_idx = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1 || tr_state == 2 || tr_state == 3 || tr_state == 4 || tr_state == 5 || tr_state == 6
%                     ct = ct + 1;
%                     tr_idx( ct, 1 ) = tr_state;
%                     X_F( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                     X_G( :, :, ct ) = G( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                     X_H( :, :, ct ) = H( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
%         clear val_time st val_trialTypeMat tloc idx
% 
%         Y_G = nan( size( X_G, 1 ), size( X_G, 2 ), 6 );
%         for tr_state = 1 : 6
%             if ~isempty( find( tr_idx == tr_state ) )
%                 Y_G( :, :, tr_state) = mean( X_G( :, :, tr_state ), 3 );
%             end
%         end; clear tr_state
%         Y_G = bsxfun( @ge, Y_G, median( Y_G, 2 ) );
% 
%         D_conds = getPatternAtypicalityConds( X_H, Y_G, tr_idx );
% 
%         tr_idx_reduced = tr_idx( tr_idx == 1 | tr_idx == 2 | tr_idx == 3 | tr_idx == 4 );
%         tr_idx_reduced( tr_idx_reduced == 1 | tr_idx_reduced == 2 ) = 1;
%         tr_idx_reduced( tr_idx_reduced == 3 | tr_idx_reduced == 4 ) = 2;
%         X_G_reduced = X_G( :, :, tr_idx == 1 | tr_idx == 2 | tr_idx == 3 | tr_idx == 4 );
%         X_H_reduced = X_H( :, :, tr_idx == 1 | tr_idx == 2 | tr_idx == 3 | tr_idx == 4 );
%         Y_G_reduced = nan( size( X_G, 1 ), size( X_G, 2 ), 2 );
%         for tr_state = 1 : 2
%             if ~isempty( find( tr_idx_reduced == tr_state ) )
%                 Y_G_reduced( :, :, tr_state) = mean( X_G_reduced( :, :, tr_state ), 3 );
%             end
%         end; clear tr_state
%         Y_G_reduced = bsxfun( @ge, Y_G_reduced, median( Y_G_reduced, 2 ) );
% 
%         D_conds_reduced = getPatternAtypicalityConds( X_H_reduced, Y_G_reduced, tr_idx_reduced );
% 
%         if sum( tr_idx_reduced == 2, 1 ) >= 2
%             ridx = find( tr_idx_reduced == 1 );
%             ridx = ridx( randperm( sum( tr_idx_reduced == 1, 1 ), sum( tr_idx_reduced == 2, 1 ) ) );
%             ridx = sort( [ ridx; find( tr_idx_reduced == 2 ) ], 1 );
%             tr_idx_reduced_same = tr_idx_reduced( ridx );
%             X_G_reduced_same = X_G_reduced( :, :, ridx );
%             X_H_reduced_same = X_H_reduced( :, :, ridx );
%             Y_G_reduced_same = nan( size( X_G, 1 ), size( X_G, 2 ), 2 );
%             for tr_state = 1 : 2
%                 if ~isempty( find( tr_idx_reduced_same == tr_state ) )
%                     Y_G_reduced_same( :, :, tr_state) = mean( X_G_reduced_same( :, :, tr_state ), 3 );
%                 end
%             end; clear tr_state
%             Y_G_reduced_same = bsxfun( @ge, Y_G_reduced_same, median( Y_G_reduced_same, 2 ) );
%             
%             D_conds_reduced_same = getPatternAtypicalityConds( X_H_reduced_same, Y_G_reduced_same, tr_idx_reduced_same );
%         else
%             D_conds_reduced_same = cell( 2, 2 );
%         end
% 
%         clear F G H X_F X_G X_H tr_idx D Y_G tr_idx_reduced X_G_reduced X_H_reduced Y_G_reduced ridx tr_idx_reduced_same X_G_reduced_same X_H_reduced_same Y_G_reduced_same
% 
%         % -----------------------------------------------------------------
%         toc
% 
%         save( [ 'PAty', fname( 5 : end ) ], 'timeDomain', 'D_conds', 'D_conds_reduced', 'D_conds_reduced_same' )
%         clear obj timeDomain D_conds D_conds_reduced D_conds_reduced_same
% 
%     end
% 
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
% 
% end; clear fn
% 

%% Get, Prediction
%
% flist = dir( 'data_*.mat' );
%
% for fn = 1 : size( flist, 1 )
%
%     fname = flist( fn, 1 ).name;
%
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
%
%         load( fname )
%
%         tic
%         % -----------------------------------------------------------------
%
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
%
%         F = bsxfun( @minus, F, mean( F, 1 ) );
%         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         F = bsxfun( @ge, F, median( F, 2 ) );
%
%         F = double( F );
%
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
%
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1 || tr_state == 2
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
%         clear val_time st val_trialTypeMat tloc idx
%
%         A = getPrediction( X );
%         A_Prediction = A;
%         clear F X A
%
%         % -----------------------------------------------------------------
%         toc
%
%         save( [ 'Pred', fname( 5 : end ) ], 'timeDomain', 'A_Prediction' )
%         clear obj timeDomain A_Prediction
%
%     end
%
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
%
% end; clear fn
%

%% Get, Prediction (MLP)
% 
% flist = dir( 'data_*.mat' );
% 
% for fn = 1% : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% 
%         load( fname )
% 
%         tic
%         % -----------------------------------------------------------------
% 
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
% 
%         F = bsxfun( @minus, F, mean( F, 1 ) );
%         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         F = bsxfun( @ge, F, median( F, 2 ) );
% 
%         F = double( F );
% 
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1 || tr_state == 2
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
%         clear val_time st val_trialTypeMat tloc idx
% 
%         % A = getPredictionMLP( X );
%         % -----------------------------------------------------------------
%         %         T = size( X, 1 );
%         %
%         %         CH = size( X, 2 );
%         %         CH_thr = 100;% reduced the number of channels
%         %         if CH < CH_thr
%         %             CH_thr = CH;
%         %         end
%         %
%         %         TR = size( X, 3 );
%         %         if TR >= 10
%         %             N_folds = 10;% 10-fold cross-validation
%         %         else
%         %             N_folds = 2;% 2-fold cross-validation
%         %         end
%         %         disp( [ num2str( N_folds ), '-fold cross-validation' ] )
%         %         P_folds = round( linspace( 1, TR, N_folds + 1 ) );
%         %         L_folds = [];
%         %         for p = 1 : N_folds
%         %             if p == 1
%         %                 L_folds = [ L_folds; p * ones( P_folds( p + 1 ) - 0, 1 ) ];
%         %             elseif p > 1
%         %                 L_folds = [ L_folds; p * ones( P_folds( p + 1 ) - P_folds( p ), 1 ) ];
%         %             end
%         %         end; clear p
%         %         N_iter = 1;% # of iteration of k-fold cross-validation
%         %
%         %
%         %         A = NaN( T, T );
%         %
%         %         % -----------------------------------------------------------------
%         %         %         for t1 = [ 1 : 18 ]%[ 1 : 18 ];[ 19 : 42 ];[ 43 : T ]% 1 : T
%         %         %             for t2 = t1 : T
%         %         %
%         %         %                 tA_1 = NaN( N_iter, 1 );
%         %         %                 tA_2 = NaN( N_iter, 1 );
%         %         %                 for iter = 1 : N_iter
%         %         %                     tL_folds = L_folds( randperm( TR ), 1 );% random partition
%         %         %                     tX_1 = [];
%         %         %                     tX_2 = [];
%         %         %                     tout_X_1 = [];
%         %         %                     tout_X_2 = [];
%         %         %                     for n = 1 : N_folds
%         %         %                         try
%         %         %                             X_training_1 = permute( X( t1, :, tL_folds ~= n ), [ 3, 2, 1 ] );
%         %         %                             X_training_2 = permute( X( t2, :, tL_folds ~= n ), [ 3, 2, 1 ] );
%         %         %                             X_test_1 = permute( X( t1, :, tL_folds == n ), [ 3, 2, 1 ] );
%         %         %                             X_test_2 = permute( X( t2, :, tL_folds == n ), [ 3, 2, 1 ] );
%         %         %
%         %         %                             ch_idx = randperm( CH, CH_thr );
%         %         %                             X_training_1 = X_training_1( :, ch_idx );
%         %         %                             X_training_2 = X_training_2( :, ch_idx );
%         %         %                             X_test_1 = X_test_1( :, ch_idx );
%         %         %                             X_test_2 = X_test_2( :, ch_idx );
%         %         %
%         %         %                             net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
%         %         %                             net = train( net, transpose( X_training_1 ), transpose( X_training_2 ) );
%         %         %                             results = net( transpose( X_test_1 ) );
%         %         %                             results = transpose( results );
%         %         %                             results( results >= 0.5 ) = 1;
%         %         %                             results( results < 0.5 ) = 0;
%         %         %                             out_X_1 = results;
%         %         %
%         %         %                             net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
%         %         %                             net = train( net, transpose( X_training_2 ), transpose( X_training_1 ) );
%         %         %                             results = net( transpose( X_test_2 ) );
%         %         %                             results = transpose( results );
%         %         %                             results( results >= 0.5 ) = 1;
%         %         %                             results( results < 0.5 ) = 0;
%         %         %                             out_X_2 = results;
%         %         %
%         %         %                             tX_1 = [ tX_1; X_test_2 ];
%         %         %                             tX_2 = [ tX_2; X_test_1 ];
%         %         %                             tout_X_1 = [ tout_X_1; out_X_1 ];
%         %         %                             tout_X_2 = [ tout_X_2; out_X_2 ];
%         %         %                         catch
%         %         %                         end
%         %         %                     end
%         %         %                     if ~isempty( tout_X_1 ) && ~isempty( tout_X_2 )
%         %         %                         tA_1( iter, 1 ) = mean( mean( tX_1 == tout_X_1, 2 ), 1 );% Prediction accuracy
%         %         %                         tA_2( iter, 1 ) = mean( mean( tX_2 == tout_X_2, 2 ), 1 );% Prediction accuracy
%         %         %                     end
%         %         %                 end
%         %         %
%         %         %                 if ~all( isnan( tA_1 ), 'all' ) && ~all( isnan( tA_2 ), 'all' )
%         %         %                     A( t1, t2 ) = mean( tA_1, 1, 'omitnan' );
%         %         %                     A( t2, t1 ) = mean( tA_2, 1, 'omitnan' );
%         %         %                 end
%         %         %
%         %         %                 save( 'temp_MLP_A1.mat', 'A' )
%         %         %                 % save( 'temp_MLP_A2.mat', 'A' )
%         %         %                 % save( 'temp_MLP_A3.mat', 'A' )
%         %         %                 disp( [ '( ', num2str( t1 ), ', ', num2str( t2 ), ' ) / ', num2str( T ) ] )
%         %         %             end; clear t2
%         %         %         end; clear t1
%         %         % -----------------------------------------------------------------
%         %         %         for t1 = [ 19 : 42 ]%[ 1 : 18 ];[ 19 : 42 ];[ 43 : T ]% 1 : T
%         %         %             for t2 = t1 : T
%         %         %
%         %         %                 tA_1 = NaN( N_iter, 1 );
%         %         %                 tA_2 = NaN( N_iter, 1 );
%         %         %                 for iter = 1 : N_iter
%         %         %                     tL_folds = L_folds( randperm( TR ), 1 );% random partition
%         %         %                     tX_1 = [];
%         %         %                     tX_2 = [];
%         %         %                     tout_X_1 = [];
%         %         %                     tout_X_2 = [];
%         %         %                     for n = 1 : N_folds
%         %         %                         try
%         %         %                             X_training_1 = permute( X( t1, :, tL_folds ~= n ), [ 3, 2, 1 ] );
%         %         %                             X_training_2 = permute( X( t2, :, tL_folds ~= n ), [ 3, 2, 1 ] );
%         %         %                             X_test_1 = permute( X( t1, :, tL_folds == n ), [ 3, 2, 1 ] );
%         %         %                             X_test_2 = permute( X( t2, :, tL_folds == n ), [ 3, 2, 1 ] );
%         %         %
%         %         %                             ch_idx = randperm( CH, CH_thr );
%         %         %                             X_training_1 = X_training_1( :, ch_idx );
%         %         %                             X_training_2 = X_training_2( :, ch_idx );
%         %         %                             X_test_1 = X_test_1( :, ch_idx );
%         %         %                             X_test_2 = X_test_2( :, ch_idx );
%         %         %
%         %         %                             net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
%         %         %                             net = train( net, transpose( X_training_1 ), transpose( X_training_2 ) );
%         %         %                             results = net( transpose( X_test_1 ) );
%         %         %                             results = transpose( results );
%         %         %                             results( results >= 0.5 ) = 1;
%         %         %                             results( results < 0.5 ) = 0;
%         %         %                             out_X_1 = results;
%         %         %
%         %         %                             net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
%         %         %                             net = train( net, transpose( X_training_2 ), transpose( X_training_1 ) );
%         %         %                             results = net( transpose( X_test_2 ) );
%         %         %                             results = transpose( results );
%         %         %                             results( results >= 0.5 ) = 1;
%         %         %                             results( results < 0.5 ) = 0;
%         %         %                             out_X_2 = results;
%         %         %
%         %         %                             tX_1 = [ tX_1; X_test_2 ];
%         %         %                             tX_2 = [ tX_2; X_test_1 ];
%         %         %                             tout_X_1 = [ tout_X_1; out_X_1 ];
%         %         %                             tout_X_2 = [ tout_X_2; out_X_2 ];
%         %         %                         catch
%         %         %                         end
%         %         %                     end
%         %         %                     if ~isempty( tout_X_1 ) && ~isempty( tout_X_2 )
%         %         %                         tA_1( iter, 1 ) = mean( mean( tX_1 == tout_X_1, 2 ), 1 );% Prediction accuracy
%         %         %                         tA_2( iter, 1 ) = mean( mean( tX_2 == tout_X_2, 2 ), 1 );% Prediction accuracy
%         %         %                     end
%         %         %                 end
%         %         %
%         %         %                 if ~all( isnan( tA_1 ), 'all' ) && ~all( isnan( tA_2 ), 'all' )
%         %         %                     A( t1, t2 ) = mean( tA_1, 1, 'omitnan' );
%         %         %                     A( t2, t1 ) = mean( tA_2, 1, 'omitnan' );
%         %         %                 end
%         %         %
%         %         %                 % save( 'temp_MLP_A1.mat', 'A' )
%         %         %                 save( 'temp_MLP_A2.mat', 'A' )
%         %         %                 % save( 'temp_MLP_A3.mat', 'A' )
%         %         %                 disp( [ '( ', num2str( t1 ), ', ', num2str( t2 ), ' ) / ', num2str( T ) ] )
%         %         %             end; clear t2
%         %         %         end; clear t1
%         %         % -----------------------------------------------------------------
%         %         %         for t1 = [ 43 : T ]%[ 1 : 18 ];[ 19 : 42 ];[ 43 : T ]% 1 : T
%         %         %             for t2 = t1 : T
%         %         %
%         %         %                 tA_1 = NaN( N_iter, 1 );
%         %         %                 tA_2 = NaN( N_iter, 1 );
%         %         %                 for iter = 1 : N_iter
%         %         %                     tL_folds = L_folds( randperm( TR ), 1 );% random partition
%         %         %                     tX_1 = [];
%         %         %                     tX_2 = [];
%         %         %                     tout_X_1 = [];
%         %         %                     tout_X_2 = [];
%         %         %                     for n = 1 : N_folds
%         %         %                         try
%         %         %                             X_training_1 = permute( X( t1, :, tL_folds ~= n ), [ 3, 2, 1 ] );
%         %         %                             X_training_2 = permute( X( t2, :, tL_folds ~= n ), [ 3, 2, 1 ] );
%         %         %                             X_test_1 = permute( X( t1, :, tL_folds == n ), [ 3, 2, 1 ] );
%         %         %                             X_test_2 = permute( X( t2, :, tL_folds == n ), [ 3, 2, 1 ] );
%         %         %
%         %         %                             ch_idx = randperm( CH, CH_thr );
%         %         %                             X_training_1 = X_training_1( :, ch_idx );
%         %         %                             X_training_2 = X_training_2( :, ch_idx );
%         %         %                             X_test_1 = X_test_1( :, ch_idx );
%         %         %                             X_test_2 = X_test_2( :, ch_idx );
%         %         %
%         %         %                             net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
%         %         %                             net = train( net, transpose( X_training_1 ), transpose( X_training_2 ) );
%         %         %                             results = net( transpose( X_test_1 ) );
%         %         %                             results = transpose( results );
%         %         %                             results( results >= 0.5 ) = 1;
%         %         %                             results( results < 0.5 ) = 0;
%         %         %                             out_X_1 = results;
%         %         %
%         %         %                             net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
%         %         %                             net = train( net, transpose( X_training_2 ), transpose( X_training_1 ) );
%         %         %                             results = net( transpose( X_test_2 ) );
%         %         %                             results = transpose( results );
%         %         %                             results( results >= 0.5 ) = 1;
%         %         %                             results( results < 0.5 ) = 0;
%         %         %                             out_X_2 = results;
%         %         %
%         %         %                             tX_1 = [ tX_1; X_test_2 ];
%         %         %                             tX_2 = [ tX_2; X_test_1 ];
%         %         %                             tout_X_1 = [ tout_X_1; out_X_1 ];
%         %         %                             tout_X_2 = [ tout_X_2; out_X_2 ];
%         %         %                         catch
%         %         %                         end
%         %         %                     end
%         %         %                     if ~isempty( tout_X_1 ) && ~isempty( tout_X_2 )
%         %         %                         tA_1( iter, 1 ) = mean( mean( tX_1 == tout_X_1, 2 ), 1 );% Prediction accuracy
%         %         %                         tA_2( iter, 1 ) = mean( mean( tX_2 == tout_X_2, 2 ), 1 );% Prediction accuracy
%         %         %                     end
%         %         %                 end
%         %         %
%         %         %                 if ~all( isnan( tA_1 ), 'all' ) && ~all( isnan( tA_2 ), 'all' )
%         %         %                     A( t1, t2 ) = mean( tA_1, 1, 'omitnan' );
%         %         %                     A( t2, t1 ) = mean( tA_2, 1, 'omitnan' );
%         %         %                 end
%         %         %
%         %         %                 % save( 'temp_MLP_A1.mat', 'A' )
%         %         %                 % save( 'temp_MLP_A2.mat', 'A' )
%         %         %                 save( 'temp_MLP_A3.mat', 'A' )
%         %         %                 disp( [ '( ', num2str( t1 ), ', ', num2str( t2 ), ' ) / ', num2str( T ) ] )
%         %         %             end; clear t2
%         %         %         end; clear t1
%         % -----------------------------------------------------------------
%         temp_A = NaN( 98, 98 );
%         load( 'temp_MLP_A1.mat' )
%         temp_A( ~isnan( A ) ) = A( ~isnan( A ) );
%         clear A
%         load( 'temp_MLP_A2.mat' )
%         temp_A( ~isnan( A ) ) = A( ~isnan( A ) );
%         clear A
%         load( 'temp_MLP_A3.mat' )
%         temp_A( ~isnan( A ) ) = A( ~isnan( A ) );
%         clear A
%         A = temp_A; clear temp_A
%         A_Prediction = A;
%         clear F X A
% 
%         % -----------------------------------------------------------------
%         toc
% 
%         save( [ 'MLP_Pred', fname( 5 : end ) ], 'timeDomain', 'A_Prediction' )
%         clear obj timeDomain A_Prediction
% 
%     end
% 
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
% 
% end; clear fn
% 

%% Get, Prediction ( Miss )
% 
% flist = dir( 'data_*.mat' );
% 
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% 
%         load( fname )
% 
%         tic
%         % -----------------------------------------------------------------
% 
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
% 
%         F = bsxfun( @minus, F, mean( F, 1 ) );
%         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         F = bsxfun( @ge, F, median( F, 2 ) );
% 
%         F = double( F );
% 
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 3 || tr_state == 4
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
%         clear val_time st val_trialTypeMat tloc idx
% 
%         A = getPrediction( X );
%         A_Prediction = A;
%         clear F X A
% 
%         % -----------------------------------------------------------------
%         toc
% 
%         save( [ 'MPred', fname( 5 : end ) ], 'timeDomain', 'A_Prediction' )
%         clear obj timeDomain A_Prediction
% 
%     end
% 
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
% 
% end; clear fn
% 

%% Get, Prediction ( Left/Right )
% 
% flist = dir( 'data_*.mat' );
% 
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% 
%         load( fname )
% 
%         tic
%         % -----------------------------------------------------------------
% 
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
% 
%         F = bsxfun( @minus, F, mean( F, 1 ) );
%         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         F = bsxfun( @ge, F, median( F, 2 ) );
% 
%         F = double( F );
% 
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
% 
%         A = getPrediction( X );
%         A_Left = A;
%         clear X A
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 2
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
% 
%         A = getPrediction( X );
%         A_Right = A;
%         clear X A
% 
%         clear val_time st val_trialTypeMat tloc idx F
% 
%         % -----------------------------------------------------------------
%         toc
% 
%         save( [ 'APred', fname( 5 : end ) ], 'timeDomain', 'A_Left', 'A_Right' )
%         clear obj timeDomain A_Prediction
% 
%     end
% 
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
% 
% end; clear fn
% 

%% Get, Prediction ( Left/Right ) Diagonal
% 
% flist = dir( 'data_*.mat' );
% 
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% 
%         load( fname )
% 
%         tic
%         % -----------------------------------------------------------------
% 
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
% 
%         F = bsxfun( @minus, F, mean( F, 1 ) );
%         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         F = bsxfun( @ge, F, median( F, 2 ) );
% 
%         F = double( F );
% 
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
% 
%         [ A, H ] = getPredictionD( X );
%         A_Left = A;
%         H_Left = H;
%         clear X A H
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 2
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
% 
%         [ A, H ] = getPredictionD( X );
%         A_Right = A;
%         H_Right = H;
%         clear X A H
% 
%         clear val_time st val_trialTypeMat tloc idx F
% 
%         % -----------------------------------------------------------------
%         toc
% 
%         save( [ 'A2Pred', fname( 5 : end ) ], 'timeDomain', 'A_Left', 'A_Right', 'H_Left', 'H_Right' )
%         clear obj timeDomain A_Prediction
% 
%     end
% 
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
% 
% end; clear fn
% 

%% Get, Decoding
%
% flist = dir( 'data_*.mat' );
%
% for fn = 1 : size( flist, 1 )
%
%     fname = flist( fn, 1 ).name;
%
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
%
%         load( fname )
%
%         tic
%         % -----------------------------------------------------------------
%
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
%
%         F = bsxfun( @minus, F, mean( F, 1 ) );
%         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         % F = bsxfun( @ge, F, median( F, 2 ) );
%
%         F = double( F );
%
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
%
%         X = [];
%         C = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1 || tr_state == 2
%                     ct = ct + 1;
%                     C( ct, 1 ) = tr_state;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
%         clear val_time st val_trialTypeMat tloc idx
%
%         A = getDecoding( X, C );
%         A_Activity = A;
%         clear F X C A
%
%         % -----------------------------------------------------------------
%
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
%
%         F = bsxfun( @minus, F, mean( F, 1 ) );
%         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         F = bsxfun( @ge, F, median( F, 2 ) );
%
%         F = double( F );
%
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
%
%         X = [];
%         C = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1 || tr_state == 2
%                     ct = ct + 1;
%                     C( ct, 1 ) = tr_state;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
%         clear val_time st val_trialTypeMat tloc idx
%
%         A = getDecoding( X, C );
%         A_Pattern = A;
%         clear F X C A
%
%         % -----------------------------------------------------------------
%         toc
%
%         save( [ 'Deco', fname( 5 : end ) ], 'timeDomain', 'A_Activity', 'A_Pattern' )
%         clear obj timeDomain A_Activity A_Pattern
%
%     end
%
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
%
% end; clear fn
%

%% Get, Cosine Similarity
% 
% flist = dir( 'data_*.mat' );
% 
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% 
%         load( fname )
% 
%         tic
%         % -----------------------------------------------------------------
% 
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
% 
%         %         F = bsxfun( @minus, F, mean( F, 1 ) );
%         %         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         %         F = bsxfun( @ge, F, median( F, 2 ) );
% 
%         F = double( F );
% 
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1 || tr_state == 2
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
%         clear val_time st val_trialTypeMat tloc idx
% 
%         S = getSimilarity( X );
%         S_Similarity = S;
%         clear F X S
% 
%         % -----------------------------------------------------------------
%         toc
% 
%         save( [ 'CSim', fname( 5 : end ) ], 'timeDomain', 'S_Similarity' )
%         clear obj timeDomain S_Similarity
% 
%     end
% 
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
% 
% end; clear fn
% 

%% Get, Cosine Similarity ( Left/Right )
% 
% flist = dir( 'data_*.mat' );
% 
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% 
%         load( fname )
% 
%         tic
%         % -----------------------------------------------------------------
% 
%         F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
%         F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
%         F = transpose( F );
% 
%         %         F = bsxfun( @minus, F, mean( F, 1 ) );
%         %         F = bsxfun( @rdivide, F, std( F, 0, 1 ) );
%         %         F = bsxfun( @ge, F, median( F, 2 ) );
% 
%         F = double( F );
% 
%         val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
%         st = 1 / mean( diff( val_time ) );
%         val_trialTypeMat = obj.trialTypeMat;
%         [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
%         idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
%         tloc = tloc( 1, idx );
%         val_trialTypeMat = val_trialTypeMat( :, idx );
%         timeDomain = val_time( tloc( 1 ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ] ) - val_time( tloc( 1 ) );
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 1
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
% 
%         S = getSimilarity( X );
%         S_Left = S;
%         clear X S
% 
%         X = [];
%         ct = 0;
%         for tr = 1 : length( tloc )
%             tr_state = find( val_trialTypeMat( :, tr ) == 1 );
%             if ~isempty( tr_state )
%                 if tr_state == 2
%                     ct = ct + 1;
%                     X( :, :, ct ) = F( tloc( tr ) + [ - fix( 2 * st ) : 1 : fix( ( 2 * 1.3 + 2 ) * st ) ], : );
%                 end
%             end
%             clear tr_state
%         end; clear tr ct
% 
%         S = getSimilarity( X );
%         S_Right = S;
%         clear X S
% 
%         clear val_time st val_trialTypeMat tloc idx F
% 
%         % -----------------------------------------------------------------
%         toc
% 
%         save( [ 'ACSim', fname( 5 : end ) ], 'timeDomain', 'S_Left', 'S_Right' )
%         clear obj timeDomain S_Left S_Right
% 
%     end
% 
%     clear fname
%     disp( [ num2str( fn ), ' / ', num2str( size( flist, 1 ) ) ] )
% 
% end; clear fn
% 

%% Results calculation for Pattern Atypicality
% 
% flist = dir( 'PAty_*.mat' );
% 
% PAty_conds = cell( 6, 2 );
% PAty_conds_reduced = cell( 2, 2 );
% PAty_conds_reduced_same = cell( 2, 2 );
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     load( fname )
% 
% 
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
% 
%     for tr_state = 1 : 6
%         if ~isempty( D_conds{ tr_state, 1 } )
%             PAty_conds{ tr_state, 1 } = [ PAty_conds{ tr_state, 1 }, D_conds{ tr_state, 1 } ];
%         else
%             PAty_conds{ tr_state, 1 } = [ PAty_conds{ tr_state, 1 }, nan( length( timeDomain ), 1 ) ];
%         end
%         if ~isempty( D_conds{ tr_state, 2 } )
%             PAty_conds{ tr_state, 2 } = [ PAty_conds{ tr_state, 2 }, D_conds{ tr_state, 2 } ];
%         else
%             PAty_conds{ tr_state, 2 } = [ PAty_conds{ tr_state, 2 }, nan( length( timeDomain ), 1 ) ];
%         end
%     end; clear tr_state
% 
%     for tr_state = 1 : 2
%         if ~isempty( D_conds_reduced{ tr_state, 1 } )
%             PAty_conds_reduced{ tr_state, 1 } = [ PAty_conds_reduced{ tr_state, 1 }, D_conds_reduced{ tr_state, 1 } ];
%         else
%             PAty_conds_reduced{ tr_state, 1 } = [ PAty_conds_reduced{ tr_state, 1 }, nan( length( timeDomain ), 1 ) ];
%         end
%         if ~isempty( D_conds_reduced{ tr_state, 2 } )
%             PAty_conds_reduced{ tr_state, 2 } = [ PAty_conds_reduced{ tr_state, 2 }, D_conds_reduced{ tr_state, 2 } ];
%         else
%             PAty_conds_reduced{ tr_state, 2 } = [ PAty_conds_reduced{ tr_state, 2 }, nan( length( timeDomain ), 1 ) ];
%         end
%     end; clear tr_state
% 
%     for tr_state = 1 : 2
%         if ~isempty( D_conds_reduced_same{ tr_state, 1 } )
%             PAty_conds_reduced_same{ tr_state, 1 } = [ PAty_conds_reduced_same{ tr_state, 1 }, D_conds_reduced_same{ tr_state, 1 } ];
%         else
%             PAty_conds_reduced_same{ tr_state, 1 } = [ PAty_conds_reduced_same{ tr_state, 1 }, nan( length( timeDomain ), 1 ) ];
%         end
%         if ~isempty( D_conds_reduced_same{ tr_state, 2 } )
%             PAty_conds_reduced_same{ tr_state, 2 } = [ PAty_conds_reduced_same{ tr_state, 2 }, D_conds_reduced_same{ tr_state, 2 } ];
%         else
%             PAty_conds_reduced_same{ tr_state, 2 } = [ PAty_conds_reduced_same{ tr_state, 2 }, nan( length( timeDomain ), 1 ) ];
%         end
%     end; clear tr_state
% 
%     clear D_conds D_conds_reduced D_conds_reduced_same
% 
% end; clear fn
% 
% 
% save( 'Results_PAty.mat', 'depth', 'timeDomain', 'PAty_conds', 'PAty_conds_reduced', 'PAty_conds_reduced_same' )
% 

%% Results calculation for Prediction
%
% flist = dir( 'Pred_*.mat' );
%
% Acc_Prediction = [];
% for fn = 1 : size( flist, 1 )
%
%     fname = flist( fn, 1 ).name;
%
%     load( fname )
%
%
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
%
%     Acc_Prediction( :, :, fn ) = A_Prediction;
%
%     clear A_Prediction
%
% end; clear fn
%
%
% save( 'Results_Prediction.mat', 'depth', 'timeDomain', 'Acc_Prediction' )
%

%% Results calculation for Prediction (Miss)
%
% flist = dir( 'MPred_*.mat' );
% 
% Acc_MPrediction = [];
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     load( fname )
% 
% 
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
% 
%     Acc_MPrediction( :, :, fn ) = A_Prediction;
% 
%     clear A_Prediction
% 
% end; clear fn
% 
% 
% save( 'Results_MPrediction.mat', 'depth', 'timeDomain', 'Acc_MPrediction' )
%

%% Results calculation for Prediction
% 
% flist = dir( 'APred_*.mat' );
% 
% Acc_Left = [];
% Acc_Right = [];
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     load( fname )
% 
% 
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
% 
%     Acc_Left( :, :, fn ) = A_Left;
%     Acc_Right( :, :, fn ) = A_Right;
% 
%     clear A_Left A_Right
% 
% end; clear fn
% 
% 
% save( 'Results_APrediction.mat', 'depth', 'timeDomain', 'Acc_Left', 'Acc_Right' )
% 

%% Results calculation for Prediction 2
% 
% flist = dir( 'A2Pred_*.mat' );
% 
% Acc_Left = [];
% Acc_Right = [];
% Ent_Left = [];
% Ent_Right = [];
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     load( fname )
% 
% 
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
% 
%     Acc_Left( :, fn ) = A_Left;
%     Acc_Right( :, fn ) = A_Right;
%     Ent_Left( :, fn ) = H_Left;
%     Ent_Right( :, fn ) = H_Right;
% 
%     clear A_Left A_Right H_Left H_Right
% 
% end; clear fn
% 
% 
% save( 'Results_A2Prediction.mat', 'depth', 'timeDomain', 'Acc_Left', 'Acc_Right', 'Ent_Left', 'Ent_Right' )
% 

%% Results calculation for Decoding
% 
% flist = dir( 'Deco_*.mat' );
% 
% Acc_Decoding = [];
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     load( fname )
% 
% 
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
% 
%     Acc_Decoding( :, :, fn ) = A_Pattern;
% 
%     clear A_Activity A_Pattern
% 
% end; clear fn
% 
% 
% save( 'Results_Decoding.mat', 'depth', 'timeDomain', 'Acc_Decoding' )
% 

%% Results calculation for Similarity
% 
% flist = dir( 'CSim_*.mat' );
% 
% Result_Similarity = [];
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     load( fname )
% 
% 
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
% 
%     Result_Similarity( :, :, fn ) = S_Similarity;
% 
%     clear S_Similarity
% 
% end; clear fn
% 
% 
% save( 'Results_Similarity.mat', 'depth', 'timeDomain', 'Result_Similarity' )
% 

%% Results calculation for Similarity
% 
% flist = dir( 'ACSim_*.mat' );
% 
% Result_Left = [];
% Result_Right = [];
% for fn = 1 : size( flist, 1 )
% 
%     fname = flist( fn, 1 ).name;
% 
%     load( fname )
% 
% 
%     depth( fn, 1 ) = str2num( fname( end- 6 : end - 4) );
% 
%     Result_Left( :, :, fn ) = S_Left;
%     Result_Right( :, :, fn ) = S_Right;
% 
%     clear S_Left S_Right
% 
% end; clear fn
% 
% 
% save( 'Results_ASimilarity.mat', 'depth', 'timeDomain', 'Result_Left', 'Result_Right' )
% 

%% info. data (Table 1)
% %
% % depth = cell( 1, 4 );
% % nCells = cell( 1, 4 );
% % nTrials = cell( 1, 4 );
% % successRate = cell( 1, 4 );
% %
% % for an = 1 : 4
% %
% %     if an == 1
% %         flist = dir( 'data_an019*.mat' );
% %     elseif an == 2
% %         flist = dir( 'data_an022*.mat' );
% %     elseif an == 3
% %         flist = dir( 'data_an023*.mat' );
% %     elseif an == 4
% %         flist = dir( 'data_an026*.mat' );
% %     end
% %
% %     for fn = 1 : size( flist, 1 )
% %
% %         fname = flist( fn, 1 ).name;
% %
% %         if ~isempty( str2num( fname( end - 6 : end - 4 ) ) )
% %
% %             load( fname )
% %
% %             depth{ 1, an } = [ depth{ 1, an }; str2num( fname( end - 6 : end - 4 ) ) ];
% %
% %             cell_idx = obj.timeSeriesArrayHash.value{ 1, 1 }.cellType;
% %             cell_idx_P = false( 1, length( cell_idx ) );
% %             cell_idx_I = false( 1, length( cell_idx ) );
% %             for k = 1 : length( cell_idx )
% %                 if ~isempty( cell_idx{ 1, k } )
% %                     str = cell_idx{ 1, k };
% %                     if strcmpi( str( 1 ), 'p' ) || strcmpi( str( 1 ), 'P' )
% %                         cell_idx_P( k ) = true;
% %                     elseif strcmpi( str( 1 ), 'i' ) || strcmpi( str( 1 ), 'I' )
% %                         cell_idx_I( k ) = true;
% %                     end
% %                     clear str
% %                 end
% %             end; clear k
% %
% %             nCells{ 1, an } = [ nCells{ 1, an }; [ size( cell_idx, 2 ), sum( cell_idx_P, 2 ), sum( cell_idx_I, 2 ) ] ];
% %             clear cell_idx cell_idx_P cell_idx_I
% %
% %             val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
% %             st = 1 / mean( diff( val_time ) );
% %             val_trialTypeMat = obj.trialTypeMat;
% %             [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
% %             idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
% %             tloc = tloc( 1, idx );
% %             val_trialTypeMat = val_trialTypeMat( :, idx );
% %             val_trialTypeMat = sum( val_trialTypeMat, 2 );
% %
% %             nTrials{ 1, an } = [ nTrials{ 1, an }; sum( val_trialTypeMat, 1 ) ];
% %             successRate{ 1, an } = [ successRate{ 1, an }; 100 * sum( val_trialTypeMat( 1 : 2, 1 ), 1 ) / sum( val_trialTypeMat, 1 ) ];
% %             clear val_time val_trialTypeMat tloc idx
% %
% %             clear obj
% %
% %         end
% %
% %         clear fname
% %
% %     end; clear fn
% %
% %     clear flist
% %
% % end; clear an
% %
% %
% % save( 'info_data.mat', 'depth', 'nCells', 'nTrials', 'successRate', 'st' )
% %
% %
% load( 'info_data.mat' )
% 
% 
% nSession = NaN( 4, 4 );
% nTrial = NaN( 3, 4 );
% successRates = NaN( 1, 4 );
% for an = 1 : 4
%     idx = depth{ 1, an } <= 350;
%     nSession( 1, an ) = sum( idx, 1 );
%     nTrial( 1, an ) = mean( nTrials{ 1, an }( idx, 1 ), 1 );
%     idx = depth{ 1, an } > 350 & depth{ 1, an } <= 550;
%     nSession( 2, an ) = sum( idx, 1 );
%     nTrial( 2, an ) = mean( nTrials{ 1, an }( idx, 1 ), 1 );
%     idx = depth{ 1, an } > 550;
%     nSession( 3, an ) = sum( idx, 1 );
%     nTrial( 3, an ) = mean( nTrials{ 1, an }( idx, 1 ), 1 );
%     clear idx
%     nSession( 4, an ) = length( depth{ 1, an } );
%     nTrial( 4, an ) = mean( nTrials{ 1, an }( :, 1 ), 1 );
%     successRates( 1, an ) = mean( successRate{ 1, an }, 1 );
% end; clear an
% nSession = [ nSession, sum( nSession, 2 ) ];
% nTrial = round( 100 * nTrial ) / 100;
% successRates = round( 100 * successRates ) / 100;
% 
% 
% 
% % { 'an019', 'an022', 'an023', 'an026', 'Sum' }
% % { 'Superficial layer (150-350 \mum)', 'Middle layer (350-550 \mum)', 'Deep layer (550-750 \mum)', 'All' }
% disp( nSession )
% % { 'an019', 'an022', 'an023', 'an026' }
% % { 'Superficial layer (150-350 \mum)', 'Middle layer (350-550 \mum)', 'Deep layer (550-750 \mum)', 'All' }
% disp( nTrial )
% % { 'an019', 'an022', 'an023', 'an026' }
% % { 'Superficial layer (150-350 \mum)', 'Middle layer (350-550 \mum)', 'Deep layer (550-750 \mum)', 'All' }
% disp( successRates )
% 
% 
% nCell = cell( 4, 3 );
% for an = 1 : 4
%     idx = depth{ 1, an } <= 350;
%     nCell{ 1, 1 } = [ nCell{ 1, 1 }; nCells{ 1, an }( idx, 1 ) ];
%     idx = depth{ 1, an } > 350 & depth{ 1, an } <= 550;
%     nCell{ 2, 1 } = [ nCell{ 2, 1 }; nCells{ 1, an }( idx, 1 ) ];
%     idx = depth{ 1, an } > 550;
%     nCell{ 3, 1 } = [ nCell{ 3, 1 }; nCells{ 1, an }( idx, 1 ) ];
%     clear idx
%     nCell{ 4, 1 } = [ nCell{ 4, 1 }; nCells{ 1, an }( :, 1 ) ];
% end; clear an
% for d = 1 : 4
%     for k = 1
%         nCell{ d, k } = nCell{ d, k }( nCell{ d, k } ~= 0 );
%         if ~isempty( nCell{ d, k } )
%             nCell{ d, k } = mean( nCell{ d, k }, 1 );
%         else
%             nCell{ d, k } = NaN;
%         end
%     end; clear k
% end; clear d
% nCell = cell2mat( nCell );
% nCell = round( 100 * nCell ) / 100;
% nCell = [ nCell( :, 1 ) ];
% 
% % { 'All' }
% % { 'Superficial layer (150-350 \mum)', 'Middle layer (350-550 \mum)', 'Deep layer (550-750 \mum)', 'All' }
% disp( nCell )
% 

%% Figure #7
% 
% flist = dir( 'data_*.mat' );
% fn = 6;
% fname = flist( fn, 1 ).name;
% load( fname )
% 
% 
% val_time = obj.timeSeriesArrayHash.value{ 1, 1 }.time;
% st = 1 / mean( diff( val_time ) );
% val_trialTypeMat = obj.trialTypeMat;
% [ ~, tloc ] = min( abs( bsxfun( @minus, obj.trialStartTimes, transpose( val_time ) ) ), [], 1 );
% idx = val_time( tloc ) - 2 >= 0 & val_time( tloc ) + 2 * 1.3 + 2 <= val_time( end );
% tloc = tloc( 1, idx );
% tloc = tloc / st;
% 
% 
% F = obj.timeSeriesArrayHash.value{ 1, 1 }.valueMatrix - 0.7 * obj.timeSeriesArrayHash.value{ 1, 2 }.valueMatrix;
% F = bsxfun( @rdivide, bsxfun( @minus, F, mean( F, 2 ) ), mean( F, 2 ) );% dF/F_0
% F = transpose( F );
% 
% 
% figure( 'position', [ 100, 100, 600, 150 ] )
% imagesc( [ 1 : size( F, 1 ) ] / st, [ 1 : size( F, 2 ) ], transpose( F ), 5 * [ -1, 1 ] )
% colormap( gca, turbo )
% axis xy
% hold on
% for tr = 1 : length( tloc )
%     plot( tloc( tr ) * [ 1, 1 ], [ 0.5, 20 + 0.5 ], '-m', 'linewidth', 1 )
% end; clear tr
% set( gca, 'xlim', [ 0.5 / st, size( F, 1 ) / st ], 'ylim', [ 0.5, size( F, 2 ) + 0.5 ] )
% ylabel( 'Neuron' )
% 
% figure( 'position', [ 100, 100, 600, 150 ] )
% imagesc( NaN, 5 * [ -1, 1 ] )
% colormap( gca, turbo )
% colorbar
% 
% 
% X = F;
% X = bsxfun( @minus, X, mean( X, 1 ) );
% X = bsxfun( @rdivide, X, std( X, 0, 1 ) );
% X = bsxfun( @ge, X, median( X, 2 ) );
% 
% colors = [ 0, 0, 0; 1, 1, 1 ];
% figure( 'position', [ 100, 100, 600, 150 ] )
% imagesc( [ 1 : size( X, 1 ) ] / st, [ 1 : size( X, 2 ) ], transpose( X ), [ 0, 1 ] )
% colormap( gca, colors )
% axis xy
% hold on
% for tr = 1 : length( tloc )
%     plot( tloc( tr ) * [ 1, 1 ], [ 0.5, 20 + 0.5 ], '-m', 'linewidth', 1 )
% end; clear tr
% set( gca, 'xlim', [ 0.5 / st, size( X, 1 ) / st ], 'ylim', [ 0.5, size( X, 2 ) + 0.5 ] )
% ylabel( 'Neuron' )
% 
% figure( 'position', [ 100, 100, 600, 150 ] )
% imagesc( NaN, [ 0, 1 ] )
% colormap( gca, colors )
% colorbar
% 

%% Figure #2
% 
% flist = dir( 'data_*.mat' );
% 
% fn = 1;
% 
% fname = flist( fn, 1 ).name;
% 
% 
% load( [ 'Pred', fname( 5 : end ) ] );
% A_Prediction_GEP = A_Prediction;
% 
% load( [ 'MLP_Pred', fname( 5 : end ) ] );
% A_Prediction_MLP = A_Prediction;
% 
% 
% A_Prediction_GEP( logical( eye( length( timeDomain ) ) ) ) = NaN;
% A_Prediction_MLP( logical( eye( length( timeDomain ) ) ) ) = NaN;
% 
% 
% lw = 1;
% c_range = [ 0.5, 0.6 ];
% 
% figure( 'position', [ 100, 100, 200, 400 ] )
% 
% subplot( 2, 1, 1 )
% imagesc( timeDomain, timeDomain, A_Prediction_GEP, 'alphadata', ~isnan( A_Prediction_GEP ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], '-k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], '-k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], '-k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% title( 'General event prediction' )
% 
% subplot( 2, 1, 2 )
% imagesc( timeDomain, timeDomain, A_Prediction_MLP, 'alphadata', ~isnan( A_Prediction_MLP ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], '-k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], '-k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], '-k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% title( 'Multilayer Perceptron' )
% 

%% Figure #3
% 
% flist = dir( 'data_*.mat' );
% 
% fn = 2;
% 
% fname = flist( fn, 1 ).name;
% 
% 
% load( [ 'Pred', fname( 5 : end ) ] );
% 
% A_Prediction( logical( eye( size( A_Prediction, 1 ) ) ) ) = nan;
% 
% [ ~, k_Prediction, I_Prediction, S_Prediction ] = getInterval( A_Prediction, [ 2 : 10 ], 100, 30 );
% 
% 
% lw = 1;
% c_range = [ 0.5, 0.6 ];
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, A_Prediction, 'AlphaData', ~isnan( A_Prediction ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], '-k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], '-k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], '-k', 'linewidth', lw )
% % plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% % plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% % plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [], 'ytick', [] )
% ylabel( 'Observation' )
% xlabel( 'Prediction' )
% % set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% % ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, S_Prediction, 'AlphaData', ~isnan( S_Prediction ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], '-k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], '-k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], '-k', 'linewidth', lw )
% % plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% % plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% % plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [], 'ytick', [] )
% ylabel( 'Observation' )
% xlabel( 'Prediction' )
% % set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% % ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% 
% lw = 1;
% c_range = [ 1, k_Prediction ];
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, I_Prediction, 'AlphaData', ~isnan( I_Prediction ), c_range )
% colormap( gca, cool )
% axis image
% axis xy
% hold on
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], '-k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], '-k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], '-k', 'linewidth', lw )
% % plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% % plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% % plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], '-k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [], 'ytick', [] )
% ylabel( 'Observation' )
% xlabel( 'Prediction' )
% % set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% % ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% 
% 
% % figure
% % colors = [ 0, 1, 1; 1, 0, 1 ];
% % imagesc( nan, [ 0.5, 2.5 ] )
% % colormap( colors )
% % colorbar( 'ticks', [ 1, 2 ] )
% 

%% Figure #8
% 
% load( 'Results_PAty.mat' )
% load( 'Results_A2Prediction.mat' )
% 
% 
% lw1 = 2;
% lw2 = 1;
% lw3 = 0.5;
% ptsLevel = 0.52;
% yRange = [ 0.42, 0.53 ];
% 
% 
% % -------------------------------------------------------------------------
% 
% pop1 = PAty_conds{ 1, 1 };
% 
% colors1 = [ 1, 0, 0 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% end; clear t
% pVals = nan( 1, length( timeDomain ) );
% for t = 31 : length( timeDomain )
%     pVals( 1, t ) = ranksum( pop1( 30, : ), pop1( t, : ), 'tail', 'right' );
% end; clear t
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% 
% % -------------------------------------------------------------------------
% 
% pop1 = PAty_conds{ 2, 1 };
% 
% colors1 = [ 0, 0, 1 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% end; clear t
% pVals = nan( 1, length( timeDomain ) );
% for t = 31 : length( timeDomain )
%     pVals( 1, t ) = ranksum( pop1( 30, : ), pop1( t, : ), 'tail', 'right' );
% end; clear t
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% 
% % -------------------------------------------------------------------------
% 
% pop1 = PAty_conds{ 1, 1 };
% pop2 = PAty_conds{ 1, 2 };
% 
% colors1 = [ 1, 0, 0; 1, 0.7, 0 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% stats2 = [];
% pVals = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
%     stats2( :, t ) = fct_extractBox( transpose( pop2( t, : ) ) );
%     pVals( 1, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'right' );
%     pVals( 2, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'left' );
% end; clear t
% pVals = min( pVals, [], 1 );
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats2( 1, : ), fliplr( stats2( 3, : ) ) ], colors2( 2, : ), 'edgecolor', colors2( 2, : ) );
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats2( 1, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% plot( timeDomain, stats2( 3, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% legend( 'Within', 'Between', 'Location', 'southwest' )
% 
% % -------------------------------------------------------------------------
% 
% pop1 = PAty_conds{ 2, 1 };
% pop2 = PAty_conds{ 2, 2 };
% 
% colors1 = [ 0, 0, 1; 0, 0.7, 1 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% stats2 = [];
% pVals = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
%     stats2( :, t ) = fct_extractBox( transpose( pop2( t, : ) ) );
%     pVals( 1, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'right' );
%     pVals( 2, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'left' );
% end; clear t
% pVals = min( pVals, [], 1 );
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats2( 1, : ), fliplr( stats2( 3, : ) ) ], colors2( 2, : ), 'edgecolor', colors2( 2, : ) );
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats2( 1, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% plot( timeDomain, stats2( 3, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% legend( 'Within', 'Between', 'Location', 'southwest' )
% 
% % -------------------------------------------------------------------------
% % 
% % pop1 = PAty_conds{ 1, 1 };
% % pop2 = PAty_conds{ 2, 1 };
% % 
% % colors1 = [ 1, 0, 0; 0, 0, 1 ];
% % colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% % 
% % stats1 = [];
% % stats2 = [];
% % pVals = [];
% % for t = 1 : length( timeDomain )
% %     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% %     stats2( :, t ) = fct_extractBox( transpose( pop2( t, : ) ) );
% %     pVals( 1, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'right' );
% %     pVals( 2, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'left' );
% % end; clear t
% % pVals = min( pVals, [], 1 );
% % sig = FDR_correction( pVals, 0.05 );
% % 
% % figure( 'position', [ 100, 100, 300, 200 ] )
% % hold on
% % plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% % plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% % patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% % patch( [ timeDomain, fliplr( timeDomain ) ], [ stats2( 1, : ), fliplr( stats2( 3, : ) ) ], colors2( 2, : ), 'edgecolor', colors2( 2, : ) );
% % plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% % plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% % plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% % plot( timeDomain, stats2( 1, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% % plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% % plot( timeDomain, stats2( 3, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% % plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% % plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% % plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% % plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% % set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% % legend( 'Left', 'Right', 'Location', 'southwest' )
% % 
% % % -------------------------------------------------------------------------
% % 
% % pop1 = PAty_conds_reduced_same{ 1, 1 };
% % pop2 = PAty_conds_reduced_same{ 2, 1 };
% % 
% % colors1 = [ 1, 0, 1; 0, 1, 0 ];
% % colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% % 
% % stats1 = [];
% % stats2 = [];
% % pVals = [];
% % for t = 1 : length( timeDomain )
% %     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% %     stats2( :, t ) = fct_extractBox( transpose( pop2( t, : ) ) );
% %     pVals( 1, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'right' );
% %     pVals( 2, t ) = ranksum( pop1( t, : ), pop2( t, : ), 'tail', 'left' );
% % end; clear t
% % pVals = min( pVals, [], 1 );
% % sig = FDR_correction( pVals, 0.05 );
% % 
% % figure( 'position', [ 100, 100, 300, 200 ] )
% % hold on
% % plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% % plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% % patch( [ timeDomain, fliplr( timeDomain ) ], [ stats2( 1, : ), fliplr( stats2( 3, : ) ) ], colors2( 2, : ), 'edgecolor', colors2( 2, : ) );
% % patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% % plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% % plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% % plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% % plot( timeDomain, stats2( 1, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% % plot( timeDomain, stats2( 2, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw1 )
% % plot( timeDomain, stats2( 3, : ), '-', 'color', colors1( 2, : ), 'linewidth', lw2 )
% % plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% % plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% % plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% % plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% % set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% % legend( 'Hit', 'Miss', 'Location', 'southwest' )
% % 
% % -------------------------------------------------------------------------
% 
% ptsLevel = 0.59;
% yRange = [ 0.58, 0.68 ];
% 
% pop1 = Acc_Left;
% 
% colors1 = [ 1, 0, 0 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% end; clear t
% pVals = nan( 1, length( timeDomain ) );
% for t = 31 : length( timeDomain )
%     pVals( 1, t ) = ranksum( pop1( 30, : ), pop1( t, : ), 'tail', 'left' );
% end; clear t
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% 
% % -------------------------------------------------------------------------
% 
% ptsLevel = 0.59;
% yRange = [ 0.58, 0.68 ];
% 
% pop1 = Acc_Right;
% 
% colors1 = [ 0, 0, 1 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% end; clear t
% pVals = nan( 1, length( timeDomain ) );
% for t = 31 : length( timeDomain )
%     pVals( 1, t ) = ranksum( pop1( 30, : ), pop1( t, : ), 'tail', 'left' );
% end; clear t
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% 
% % -------------------------------------------------------------------------
% 
% ptsLevel = 0.59;
% yRange = [ 0.58, 0.68 ];
% 
% pop1 = Acc_Left;
% 
% colors1 = [ 1, 0, 0 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% end; clear t
% pVals = nan( 1, length( timeDomain ) );
% for t = 1 : length( timeDomain )
%     pVals( 1, t ) = signrank( 0.5 * ones( 1, size( pop1, 2 ) ), pop1( t, : ), 'tail', 'left' );
% end; clear t
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% 
% % -------------------------------------------------------------------------
% 
% ptsLevel = 0.59;
% yRange = [ 0.58, 0.68 ];
% 
% pop1 = Acc_Right;
% 
% colors1 = [ 0, 0, 1 ];
% colors2 = 0.2 * colors1 + 0.8 * ones( size( colors1 ) );
% 
% stats1 = [];
% for t = 1 : length( timeDomain )
%     stats1( :, t ) = fct_extractBox( transpose( pop1( t, : ) ) );
% end; clear t
% pVals = nan( 1, length( timeDomain ) );
% for t = 1 : length( timeDomain )
%     pVals( 1, t ) = signrank( 0.5 * ones( 1, size( pop1, 2 ) ), pop1( t, : ), 'tail', 'left' );
% end; clear t
% sig = FDR_correction( pVals, 0.05 );
% 
% figure( 'position', [ 100, 100, 300, 200 ] )
% hold on
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% patch( [ timeDomain, fliplr( timeDomain ) ], [ stats1( 1, : ), fliplr( stats1( 3, : ) ) ], colors2( 1, : ), 'edgecolor', colors2( 1, : ) );
% plot( timeDomain, stats1( 1, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain, stats1( 2, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw1 )
% plot( timeDomain, stats1( 3, : ), '-', 'color', colors1( 1, : ), 'linewidth', lw2 )
% plot( timeDomain( sig ), ptsLevel * sig( sig ), '.k' )
% plot( 0 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 1.3 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% plot( 2.6 * [ 1, 1 ], yRange, ':k', 'linewidth', lw3 )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', yRange )
% 

%% Figure #9
% 
% load( 'Results_Prediction.mat' )
% load( 'Results_MPrediction.mat' )
% load( 'Results_APrediction.mat' )
% load( 'Results_Decoding.mat' )
% 
% 
% % Acc_Prediction = Acc_Prediction( timeDomain >= 0, timeDomain >= 0, : );
% % Acc_MPrediction = Acc_MPrediction( timeDomain >= 0, timeDomain >= 0, : );
% % Acc_Left = Acc_Left( timeDomain >= 0, timeDomain >= 0, : );
% % Acc_Right = Acc_Right( timeDomain >= 0, timeDomain >= 0, : );
% % Acc_Decoding = Acc_Decoding( timeDomain >= 0, timeDomain >= 0, : );
% % timeDomain = timeDomain( timeDomain >= 0 );
% 
% % Acc_Prediction = Acc_Prediction( timeDomain >= 1.3, timeDomain >= 1.3, : );
% % Acc_MPrediction = Acc_MPrediction( timeDomain >= 1.3, timeDomain >= 1.3, : );
% % Acc_Left = Acc_Left( timeDomain >= 1.3, timeDomain >= 1.3, : );
% % Acc_Right = Acc_Right( timeDomain >= 1.3, timeDomain >= 1.3, : );
% % Acc_Decoding = Acc_Decoding( timeDomain >= 1.3, timeDomain >= 1.3, : );
% % timeDomain = timeDomain( timeDomain >= 1.3 );
% 
% % A_Prediction = median( Acc_Prediction, 3, 'omitnan' );
% % A_MPrediction = median( Acc_MPrediction, 3, 'omitnan' );
% A_Left = median( Acc_Left, 3, 'omitnan' );
% A_Right = median( Acc_Right, 3, 'omitnan' );
% A_Decoding = median( Acc_Decoding, 3, 'omitnan' );
% 
% 
% % A_Prediction( logical( eye( size( A_Prediction, 1 ) ) ) ) = nan;
% % A_MPrediction( logical( eye( size( A_MPrediction, 1 ) ) ) ) = nan;
% A_Left( logical( eye( size( A_Left, 1 ) ) ) ) = nan;
% A_Right( logical( eye( size( A_Right, 1 ) ) ) ) = nan;
% % A_Decoding( logical( eye( size( A_Decoding, 1 ) ) ) ) = nan;
% 
% 
% % [ ~, k_Prediction, I_Prediction, ~ ] = getInterval( A_Prediction, [ 2 : 10 ], 100, 30 );
% % [ ~, k_MPrediction, I_MPrediction, ~ ] = getInterval( A_MPrediction, [ 2 : 10 ], 100, 30 );
% [ ~, k_Left, I_Left, S_Left ] = getInterval( A_Left, [ 2 : 10 ], 100, 30 );
% [ ~, k_Right, I_Right, S_Right ] = getInterval( A_Right, [ 2 : 10 ], 100, 30 );
% 
% 
% % idx_Prediction_pool = [];
% % idx_MPrediction_pool = [];
% idx_Left_pool = [];
% idx_Right_pool = [];
% for session = 1 : size( Acc_Prediction, 3 )
%     % [ idx_Prediction_pool( :, session ), ~, ~, ~ ] = getInterval( Acc_Prediction( :, :, session ), [ 2 ], 100, 30 );
%     % [ idx_MPrediction_pool( :, session ), ~, ~, ~ ] = getInterval( Acc_MPrediction( :, :, session ), [ 2 ], 100, 30 );
%     [ idx_Left_pool( :, session ), ~, ~, ~ ] = getInterval( Acc_Left( :, :, session ), [ 2 ], 100, 30 );
%     [ idx_Right_pool( :, session ), ~, ~, ~ ] = getInterval( Acc_Right( :, :, session ), [ 2 ], 100, 30 );
% end; clear session
% 
% % idx_Prediction = median( idx_Prediction_pool, 2, 'omitnan' );
% % idx_MPrediction = median( idx_MPrediction_pool, 2, 'omitnan' );
% idx_Left = median( idx_Left_pool, 2, 'omitnan' );
% idx_Right = median( idx_Right_pool, 2, 'omitnan' );
% % pVals_Prediction = [];
% % pVals_MPrediction = [];
% pVals_Left = [];
% pVals_Right = [];
% for t = 1 : length( timeDomain )
%     %     pVal1 = signrank( transpose( idx_Prediction_pool( t, : ) ), 1.5, 'tail', 'left' );
%     %     pVal2 = signrank( transpose( idx_Prediction_pool( t, : ) ), 1.5, 'tail', 'right' );
%     %     pVals_Prediction( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
%     %     pVal1 = signrank( transpose( idx_MPrediction_pool( t, : ) ), 1.5, 'tail', 'left' );
%     %     pVal2 = signrank( transpose( idx_MPrediction_pool( t, : ) ), 1.5, 'tail', 'right' );
%     %     pVals_MPrediction( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
%     pVal1 = signrank( transpose( idx_Left_pool( t, : ) ), 1.5, 'tail', 'left' );
%     pVal2 = signrank( transpose( idx_Left_pool( t, : ) ), 1.5, 'tail', 'right' );
%     pVals_Left( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
%     pVal1 = signrank( transpose( idx_Right_pool( t, : ) ), 1.5, 'tail', 'left' );
%     pVal2 = signrank( transpose( idx_Right_pool( t, : ) ), 1.5, 'tail', 'right' );
%     pVals_Right( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
%     clear pVal1 pVal2
% end; clear t
% % sig_Prediction = FDR_correction( pVals_Prediction, 0.05 );
% % sig_MPrediction = FDR_correction( pVals_MPrediction, 0.05 );
% sig_Left = FDR_correction( pVals_Left, 0.05 );
% sig_Right = FDR_correction( pVals_Right, 0.05 );
% % idx_Prediction( ~sig_Prediction ) = nan;
% % idx_MPrediction( ~sig_MPrediction ) = nan;
% idx_Left( ~sig_Left ) = nan;
% idx_Right( ~sig_Right ) = nan;
% 
% % idx = idx_Prediction;
% % I = nan( length( timeDomain ), length( timeDomain ) );
% % for kk = 1 : 2
% %     idx_kk = find( idx == kk );
% %     for t1 = 1 : length( idx_kk )
% %         for t2 = t1 : length( idx_kk )
% %             I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
% %             I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
% %         end; clear t2
% %     end; clear t1
% % end
% % I_Prediction_pool = I;
% % clear idx I
% 
% % idx = idx_MPrediction;
% % I = nan( length( timeDomain ), length( timeDomain ) );
% % for kk = 1 : 2
% %     idx_kk = find( idx == kk );
% %     for t1 = 1 : length( idx_kk )
% %         for t2 = t1 : length( idx_kk )
% %             I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
% %             I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
% %         end; clear t2
% %     end; clear t1
% % end
% % I_MPrediction_pool = I;
% % clear idx I
% 
% idx = idx_Left;
% I = nan( length( timeDomain ), length( timeDomain ) );
% for kk = 1 : 2
%     idx_kk = find( idx == kk );
%     for t1 = 1 : length( idx_kk )
%         for t2 = t1 : length( idx_kk )
%             I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
%             I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
%         end; clear t2
%     end; clear t1
% end
% I_Left_pool = I;
% clear idx I
% 
% idx = idx_Right;
% I = nan( length( timeDomain ), length( timeDomain ) );
% for kk = 1 : 2
%     idx_kk = find( idx == kk );
%     for t1 = 1 : length( idx_kk )
%         for t2 = t1 : length( idx_kk )
%             I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
%             I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
%         end; clear t2
%     end; clear t1
% end
% I_Right_pool = I;
% clear idx I
% 
% 
% pVals_Decoding = [];
% for t1 = 1 : length( timeDomain )
%     for t2 = 1 : length( timeDomain )
%         pVals_Decoding( t1, t2 ) = signrank( squeeze( Acc_Decoding( t1, t2, : ) ), 0.5, 'tail', 'right' );
%     end; clear t2
% end; clear t1
% sig_Decoding = FDR_correction( pVals_Decoding( : ), 0.05 );
% sig_Decoding = reshape( sig_Decoding, length( timeDomain ), length( timeDomain ) );
% A_Decoding( ~sig_Decoding ) = nan;
% 
% 
% lw = 1;
% c_range = [ 0.5, 0.6 ];
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, A_Left, 'AlphaData', ~isnan( A_Left ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, A_Right, 'AlphaData', ~isnan( A_Right ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, S_Left, 'AlphaData', ~isnan( S_Left ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, S_Right, 'AlphaData', ~isnan( S_Right ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% 
% lw = 1;
% c_range = [ 1, k_Left ];
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, I_Left, 'AlphaData', ~isnan( I_Left ), c_range )
% colormap( gca, cool )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% lw = 1;
% c_range = [ 1, k_Right ];
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, I_Right, 'AlphaData', ~isnan( I_Right ), c_range )
% colormap( gca, cool )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% 
% lw = 1;
% c_range = [ 1, 2 ];
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, I_Left_pool, 'AlphaData', ~isnan( I_Left_pool ), c_range )
% colormap( gca, cool )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, I_Right_pool, 'AlphaData', ~isnan( I_Right_pool ), c_range )
% colormap( gca, cool )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% 
% lw = 1;
% c_range = [ 0.5, 0.9 ];
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, A_Decoding, 'AlphaData', ~isnan( A_Decoding ), c_range )
% colormap( gca, parula )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 

% %% Figure #9
% % 
% % load( 'Results_Prediction.mat' )
% % load( 'Results_MPrediction.mat' )
% % load( 'Results_APrediction.mat' )
% % 
% % 
% % A_Prediction_depth = [];
% % A_Left_depth = [];
% % A_Right_depth = [];
% % idx = depth <= 350;
% % A_depth = median( Acc_Prediction( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Prediction_depth( :, :, 1 ) = A_depth; clear A_depth
% % A_depth = median( Acc_Left( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Left_depth( :, :, 1 ) = A_depth; clear A_depth
% % A_depth = median( Acc_Right( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Right_depth( :, :, 1 ) = A_depth; clear A_depth
% % idx = depth > 350 & depth <= 550;
% % A_depth = median( Acc_Prediction( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Prediction_depth( :, :, 2 ) = A_depth; clear A_depth
% % A_depth = median( Acc_Left( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Left_depth( :, :, 2 ) = A_depth; clear A_depth
% % A_depth = median( Acc_Right( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Right_depth( :, :, 2 ) = A_depth; clear A_depth
% % idx = depth > 550;
% % A_depth = median( Acc_Prediction( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Prediction_depth( :, :, 3 ) = A_depth; clear A_depth
% % A_depth = median( Acc_Left( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Left_depth( :, :, 3 ) = A_depth; clear A_depth
% % A_depth = median( Acc_Right( :, :, idx ), 3, 'omitnan' );
% % A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% % A_Right_depth( :, :, 3 ) = A_depth; clear A_depth
% % 
% % 
% % % I_Prediction_depth = [];
% % I_Left_depth = [];
% % I_Right_depth = [];
% % % k_Prediction_depth = [];
% % k_Left_depth = [];
% % k_Right_depth = [];
% % for d = 1 : 3
% %     % [ ~, k_Prediction_depth( d, 1 ), I_Prediction_depth( :, :, d ), ~ ] = getInterval( A_Prediction_depth( :, :, d ), [ 2 : 10 ], 100, 30 );
% %     [ ~, k_Left_depth( d, 1 ), I_Left_depth( :, :, d ), ~ ] = getInterval( A_Left_depth( :, :, d ), [ 2 : 10 ], 100, 30 );
% %     [ ~, k_Right_depth( d, 1 ), I_Right_depth( :, :, d ), ~ ] = getInterval( A_Right_depth( :, :, d ), [ 2 : 10 ], 100, 30 );
% % end; clear d
% % 
% % 
% % % idx_MPrediction_pool = [];
% % idx_Left_pool = [];
% % idx_Right_pool = [];
% % for session = 1 : size( Acc_Prediction, 3 )
% %     % [ idx_MPrediction_pool( :, session ), ~, ~, ~ ] = getInterval( Acc_MPrediction( :, :, session ), [ 2 ], 100, 30 );
% %     [ idx_Left_pool( :, session ), ~, ~, ~ ] = getInterval( Acc_Left( :, :, session ), [ 2 ], 100, 30 );
% %     [ idx_Right_pool( :, session ), ~, ~, ~ ] = getInterval( Acc_Right( :, :, session ), [ 2 ], 100, 30 );
% % end; clear session
% % 
% % idx_Left_depth = [];
% % idx_Right_depth = [];
% % idx = depth <= 350;
% % idx_Left_depth( :, 1 ) = median( idx_Left_pool( :, idx ), 2, 'omitnan' );
% % idx_Right_depth( :, 1 ) = median( idx_Right_pool( :, idx ), 2, 'omitnan' );
% % idx = depth > 350 & depth <= 550;
% % idx_Left_depth( :, 2 ) = median( idx_Left_pool( :, idx ), 2, 'omitnan' );
% % idx_Right_depth( :, 2 ) = median( idx_Right_pool( :, idx ), 2, 'omitnan' );
% % idx = depth > 550;
% % idx_Left_depth( :, 3 ) = median( idx_Left_pool( :, idx ), 2, 'omitnan' );
% % idx_Right_depth( :, 3 ) = median( idx_Right_pool( :, idx ), 2, 'omitnan' );
% % clear idx
% % pVals_Left_depth = [];
% % pVals_Right_depth = [];
% % for t = 1 : length( timeDomain )
% %     idx = depth <= 350;
% %     pVal1 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Left_depth( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Right_depth( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     idx = depth > 350 & depth <= 550;
% %     pVal1 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Left_depth( t, 2 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Right_depth( t, 2 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     idx = depth > 550;
% %     pVal1 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Left_depth( t, 3 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Right_depth( t, 3 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     clear idx pVal1 pVal2
% % end; clear t
% % sig_Left_depth = [];
% % sig_Left_depth( :, 1 ) = FDR_correction( pVals_Left_depth( :, 1 ), 0.05 );
% % sig_Left_depth( :, 2 ) = FDR_correction( pVals_Left_depth( :, 2 ), 0.05 );
% % sig_Left_depth( :, 3 ) = FDR_correction( pVals_Left_depth( :, 3 ), 0.05 );
% % sig_Right_depth = [];
% % sig_Right_depth( :, 1 ) = FDR_correction( pVals_Right_depth( :, 1 ), 0.05 );
% % sig_Right_depth( :, 2 ) = FDR_correction( pVals_Right_depth( :, 2 ), 0.05 );
% % sig_Right_depth( :, 3 ) = FDR_correction( pVals_Right_depth( :, 3 ), 0.05 );
% % idx_Left_depth( ~sig_Left_depth ) = nan;
% % idx_Right_depth( ~sig_Right_depth ) = nan;
% % 
% % I = nan( length( timeDomain ), length( timeDomain ), 3 );
% % for d = 1 : 3
% %     idx = idx_Left_depth( :, d );
% %     for kk = 1 : 2
% %         idx_kk = find( idx == kk );
% %         for t1 = 1 : length( idx_kk )
% %             for t2 = t1 : length( idx_kk )
% %                 I( idx_kk( t1 ), idx_kk( t2 ), d ) = kk;
% %                 I( idx_kk( t2 ), idx_kk( t1 ), d ) = kk;
% %             end; clear t2
% %         end; clear t1
% %     end
% % end; clear d
% % I_Left_pool_depth = I;
% % clear idx I
% % 
% % I = nan( length( timeDomain ), length( timeDomain ), 3 );
% % for d = 1 : 3
% %     idx = idx_Right_depth( :, d );
% %     for kk = 1 : 2
% %         idx_kk = find( idx == kk );
% %         for t1 = 1 : length( idx_kk )
% %             for t2 = t1 : length( idx_kk )
% %                 I( idx_kk( t1 ), idx_kk( t2 ), d ) = kk;
% %                 I( idx_kk( t2 ), idx_kk( t1 ), d ) = kk;
% %             end; clear t2
% %         end; clear t1
% %     end
% % end; clear d
% % I_Right_pool_depth = I;
% % clear idx I
% % 
% % 
% % lw = 1;
% % c_range = [ 0.5, 0.6 ];
% % 
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, A_Left_depth( :, :, d ), 'AlphaData', ~isnan( A_Left_depth( :, :, d ) ), c_range )
% %     colormap( gca, turbo )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, A_Right_depth( :, :, d ), 'AlphaData', ~isnan( A_Right_depth( :, :, d ) ), c_range )
% %     colormap( gca, turbo )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     lw = 1;
% %     c_range = [ 1, k_Left_depth( d, 1 ) ];
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Left_depth( :, :, d ), 'AlphaData', ~isnan( I_Left_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     lw = 1;
% %     c_range = [ 1, k_Right_depth( d, 1 ) ];
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Right_depth( :, :, d ), 'AlphaData', ~isnan( I_Right_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % 
% % lw = 1;
% % c_range = [ 1, 2 ];
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Left_pool_depth( :, :, d ), 'AlphaData', ~isnan( I_Left_pool_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Right_pool_depth( :, :, d ), 'AlphaData', ~isnan( I_Right_pool_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 

%% Figure #10
% 
% load( 'Results_Similarity.mat' )
% load( 'Results_ASimilarity.mat' )
% 
% 
% S_Similarity = median( Result_Similarity, 3, 'omitnan' );
% S_Left = median( Result_Left, 3, 'omitnan' );
% S_Right = median( Result_Right, 3, 'omitnan' );
% 
% 
% S_Similarity( logical( eye( size( S_Similarity, 1 ) ) ) ) = nan;
% S_Left( logical( eye( size( S_Left, 1 ) ) ) ) = nan;
% S_Right( logical( eye( size( S_Right, 1 ) ) ) ) = nan;
% 
% 
% % % [ ~, k_Similarity, I_Similarity, ~ ] = getInterval( S_Similarity, [ 2 : 10 ], 100, 30 );
% % [ ~, k_Left, I_Left, ~ ] = getInterval( S_Left, [ 2 : 10 ], 100, 30 );
% % [ ~, k_Right, I_Right, ~ ] = getInterval( S_Right, [ 2 : 10 ], 100, 30 );
% % 
% % 
% % idx_Similarity_pool = [];
% % idx_Left_pool = [];
% % idx_Right_pool = [];
% % for session = 1 : size( Result_Similarity, 3 )
% %     [ idx_Similarity_pool( :, session ), ~, ~, ~ ] = getInterval( Result_Similarity( :, :, session ), [ 2 ], 100, 30 );
% %     [ idx_Left_pool( :, session ), ~, ~, ~ ] = getInterval( Result_Left( :, :, session ), [ 2 ], 100, 30 );
% %     [ idx_Right_pool( :, session ), ~, ~, ~ ] = getInterval( Result_Right( :, :, session ), [ 2 ], 100, 30 );
% % end; clear session
% % 
% % idx_Similarity = median( idx_Similarity_pool, 2, 'omitnan' );
% % idx_Left = median( idx_Left_pool, 2, 'omitnan' );
% % idx_Right = median( idx_Right_pool, 2, 'omitnan' );
% % pVals_Similarity = [];
% % pVals_Left = [];
% % pVals_Right = [];
% % for t = 1 : length( timeDomain )
% %     pVal1 = signrank( transpose( idx_Similarity_pool( t, : ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Similarity_pool( t, : ) ), 1.5, 'tail', 'right' );
% %     pVals_Similarity( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Left_pool( t, : ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Left_pool( t, : ) ), 1.5, 'tail', 'right' );
% %     pVals_Left( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Right_pool( t, : ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Right_pool( t, : ) ), 1.5, 'tail', 'right' );
% %     pVals_Right( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     clear pVal1 pVal2
% % end; clear t
% % sig_Similarity = FDR_correction( pVals_Similarity, 0.05 );
% % sig_Left = FDR_correction( pVals_Left, 0.05 );
% % sig_Right = FDR_correction( pVals_Right, 0.05 );
% % idx_Similarity( ~sig_Similarity ) = nan;
% % idx_Left( ~sig_Left ) = nan;
% % idx_Right( ~sig_Right ) = nan;
% % 
% % % idx = idx_Similarity;
% % % I = nan( length( timeDomain ), length( timeDomain ) );
% % % for kk = 1 : 2
% % %     idx_kk = find( idx == kk );
% % %     for t1 = 1 : length( idx_kk )
% % %         for t2 = t1 : length( idx_kk )
% % %             I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
% % %             I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
% % %         end; clear t2
% % %     end; clear t1
% % % end
% % % I_Similarity_pool = I;
% % % clear idx I
% % 
% % idx = idx_Left;
% % I = nan( length( timeDomain ), length( timeDomain ) );
% % for kk = 1 : 2
% %     idx_kk = find( idx == kk );
% %     for t1 = 1 : length( idx_kk )
% %         for t2 = t1 : length( idx_kk )
% %             I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
% %             I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
% %         end; clear t2
% %     end; clear t1
% % end
% % I_Left_pool = I;
% % clear idx I
% % 
% % idx = idx_Right;
% % I = nan( length( timeDomain ), length( timeDomain ) );
% % for kk = 1 : 2
% %     idx_kk = find( idx == kk );
% %     for t1 = 1 : length( idx_kk )
% %         for t2 = t1 : length( idx_kk )
% %             I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
% %             I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
% %         end; clear t2
% %     end; clear t1
% % end
% % I_Right_pool = I;
% % clear idx I
% 
% 
% S_Similarity_depth = [];
% S_Left_depth = [];
% S_Right_depth = [];
% idx = depth <= 350;
% A_depth = median( Result_Similarity( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Similarity_depth( :, :, 1 ) = A_depth; clear A_depth
% A_depth = median( Result_Left( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Left_depth( :, :, 1 ) = A_depth; clear A_depth
% A_depth = median( Result_Right( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Right_depth( :, :, 1 ) = A_depth; clear A_depth
% idx = depth > 350 & depth <= 550;
% A_depth = median( Result_Similarity( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Similarity_depth( :, :, 2 ) = A_depth; clear A_depth
% A_depth = median( Result_Left( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Left_depth( :, :, 2 ) = A_depth; clear A_depth
% A_depth = median( Result_Right( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Right_depth( :, :, 2 ) = A_depth; clear A_depth
% idx = depth > 550;
% A_depth = median( Result_Similarity( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Similarity_depth( :, :, 3 ) = A_depth; clear A_depth
% A_depth = median( Result_Left( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Left_depth( :, :, 3 ) = A_depth; clear A_depth
% A_depth = median( Result_Right( :, :, idx ), 3, 'omitnan' );
% A_depth( logical( eye( size( A_depth, 1 ) ) ) ) = nan;
% S_Right_depth( :, :, 3 ) = A_depth; clear A_depth
% 
% 
% % % I_Similarity_depth = [];
% % I_Left_depth = [];
% % I_Right_depth = [];
% % % k_Similarity_depth = [];
% % k_Left_depth = [];
% % k_Right_depth = [];
% % for d = 1 : 3
% %     % [ ~, k_Similarity_depth( d, 1 ), I_Similarity_depth( :, :, d ), ~ ] = getInterval( S_Similarity_depth( :, :, d ), [ 2 : 10 ], 100, 30 );
% %     [ ~, k_Left_depth( d, 1 ), I_Left_depth( :, :, d ), ~ ] = getInterval( S_Left_depth( :, :, d ), [ 2 : 10 ], 100, 30 );
% %     [ ~, k_Right_depth( d, 1 ), I_Right_depth( :, :, d ), ~ ] = getInterval( S_Right_depth( :, :, d ), [ 2 : 10 ], 100, 30 );
% % end; clear d
% % 
% % 
% % idx_Left_pool = [];
% % idx_Right_pool = [];
% % for session = 1 : size( Result_Similarity, 3 )
% %     [ idx_Left_pool( :, session ), ~, ~, ~ ] = getInterval( Result_Left( :, :, session ), [ 2 ], 100, 30 );
% %     [ idx_Right_pool( :, session ), ~, ~, ~ ] = getInterval( Result_Right( :, :, session ), [ 2 ], 100, 30 );
% % end; clear session
% % 
% % idx_Left_depth = [];
% % idx_Right_depth = [];
% % idx = depth <= 350;
% % idx_Left_depth( :, 1 ) = median( idx_Left_pool( :, idx ), 2, 'omitnan' );
% % idx_Right_depth( :, 1 ) = median( idx_Right_pool( :, idx ), 2, 'omitnan' );
% % idx = depth > 350 & depth <= 550;
% % idx_Left_depth( :, 2 ) = median( idx_Left_pool( :, idx ), 2, 'omitnan' );
% % idx_Right_depth( :, 2 ) = median( idx_Right_pool( :, idx ), 2, 'omitnan' );
% % idx = depth > 550;
% % idx_Left_depth( :, 3 ) = median( idx_Left_pool( :, idx ), 2, 'omitnan' );
% % idx_Right_depth( :, 3 ) = median( idx_Right_pool( :, idx ), 2, 'omitnan' );
% % clear idx
% % pVals_Left_depth = [];
% % pVals_Right_depth = [];
% % for t = 1 : length( timeDomain )
% %     idx = depth <= 350;
% %     pVal1 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Left_depth( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Right_depth( t, 1 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     idx = depth > 350 & depth <= 550;
% %     pVal1 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Left_depth( t, 2 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Right_depth( t, 2 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     idx = depth > 550;
% %     pVal1 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Left_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Left_depth( t, 3 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     pVal1 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'left' );
% %     pVal2 = signrank( transpose( idx_Right_pool( t, idx ) ), 1.5, 'tail', 'right' );
% %     pVals_Right_depth( t, 3 ) = min( [ pVal1, pVal2 ], [], 2 );
% %     clear idx pVal1 pVal2
% % end; clear t
% % sig_Left_depth = [];
% % sig_Left_depth( :, 1 ) = FDR_correction( pVals_Left_depth( :, 1 ), 0.05 );
% % sig_Left_depth( :, 2 ) = FDR_correction( pVals_Left_depth( :, 2 ), 0.05 );
% % sig_Left_depth( :, 3 ) = FDR_correction( pVals_Left_depth( :, 3 ), 0.05 );
% % sig_Right_depth = [];
% % sig_Right_depth( :, 1 ) = FDR_correction( pVals_Right_depth( :, 1 ), 0.05 );
% % sig_Right_depth( :, 2 ) = FDR_correction( pVals_Right_depth( :, 2 ), 0.05 );
% % sig_Right_depth( :, 3 ) = FDR_correction( pVals_Right_depth( :, 3 ), 0.05 );
% % idx_Left_depth( ~sig_Left_depth ) = nan;
% % idx_Right_depth( ~sig_Right_depth ) = nan;
% % 
% % I = nan( length( timeDomain ), length( timeDomain ), 3 );
% % for d = 1 : 3
% %     idx = idx_Left_depth( :, d );
% %     for kk = 1 : 2
% %         idx_kk = find( idx == kk );
% %         for t1 = 1 : length( idx_kk )
% %             for t2 = t1 : length( idx_kk )
% %                 I( idx_kk( t1 ), idx_kk( t2 ), d ) = kk;
% %                 I( idx_kk( t2 ), idx_kk( t1 ), d ) = kk;
% %             end; clear t2
% %         end; clear t1
% %     end
% % end; clear d
% % I_Left_pool_depth = I;
% % clear idx I
% % 
% % I = nan( length( timeDomain ), length( timeDomain ), 3 );
% % for d = 1 : 3
% %     idx = idx_Right_depth( :, d );
% %     for kk = 1 : 2
% %         idx_kk = find( idx == kk );
% %         for t1 = 1 : length( idx_kk )
% %             for t2 = t1 : length( idx_kk )
% %                 I( idx_kk( t1 ), idx_kk( t2 ), d ) = kk;
% %                 I( idx_kk( t2 ), idx_kk( t1 ), d ) = kk;
% %             end; clear t2
% %         end; clear t1
% %     end
% % end; clear d
% % I_Right_pool_depth = I;
% % clear idx I
% 
% 
% lw = 1;
% c_range = [ -0.3, 1 ];
% 
% 
% % figure
% % imagesc( nan, c_range )
% % colormap( gca, turbo )
% % colorbar
% 
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, S_Left, 'AlphaData', ~isnan( S_Left ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% figure( 'position', [ 100, 100, 200, 200 ] )
% imagesc( timeDomain, timeDomain, S_Right, 'AlphaData', ~isnan( S_Right ), c_range )
% colormap( gca, turbo )
% axis image
% axis xy
% hold on
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% ylabel( 'Observation time (s)' )
% xlabel( 'Prediction time (s)' )
% 
% 
% figure( 'position', [ 100, 100, 200, 600 ] )
% for d = 1 : 3
%     subplot( 3, 1, d )
%     imagesc( timeDomain, timeDomain, S_Left_depth( :, :, d ), 'AlphaData', ~isnan( S_Left_depth( :, :, d ) ), c_range )
%     colormap( gca, turbo )
%     axis image
%     axis xy
%     hold on
%     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
%     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
%     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
%     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
%     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
%     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
%     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
%     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
%     ylabel( 'Observation time (s)' )
%     if d == 1
%         title( 'Superficial layer (150-350 \mum)' )
%     elseif d == 2
%         title( 'Middle layer (350-550 \mum)' )
%     elseif d == 3
%         xlabel( 'Prediction time (s)' )
%         title( 'Deep layer (550-750 \mum)' )
%     end
% end; clear d
% 
% figure( 'position', [ 100, 100, 200, 600 ] )
% for d = 1 : 3
%     subplot( 3, 1, d )
%     imagesc( timeDomain, timeDomain, S_Right_depth( :, :, d ), 'AlphaData', ~isnan( S_Right_depth( :, :, d ) ), c_range )
%     colormap( gca, turbo )
%     axis image
%     axis xy
%     hold on
%     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
%     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
%     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
%     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
%     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
%     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
%     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
%     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
%     ylabel( 'Observation time (s)' )
%     if d == 1
%         title( 'Superficial layer (150-350 \mum)' )
%     elseif d == 2
%         title( 'Middle layer (350-550 \mum)' )
%     elseif d == 3
%         xlabel( 'Prediction time (s)' )
%         title( 'Deep layer (550-750 \mum)' )
%     end
% end; clear d
% 
% 
% % lw = 1;
% % c_range = [ 1, k_Left ];
% % 
% % figure( 'position', [ 100, 100, 200, 200 ] )
% % imagesc( timeDomain, timeDomain, I_Left, 'AlphaData', ~isnan( I_Left ), c_range )
% % colormap( gca, cool )
% % axis image
% % axis xy
% % hold on
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% % plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% % set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% % ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% % 
% % lw = 1;
% % c_range = [ 1, k_Right ];
% % 
% % figure( 'position', [ 100, 100, 200, 200 ] )
% % imagesc( timeDomain, timeDomain, I_Right, 'AlphaData', ~isnan( I_Right ), c_range )
% % colormap( gca, cool )
% % axis image
% % axis xy
% % hold on
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% % plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% % set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% % ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% % 
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     lw = 1;
% %     c_range = [ 1, k_Left_depth( d, 1 ) ];
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Left_depth( :, :, d ), 'AlphaData', ~isnan( I_Left_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     lw = 1;
% %     c_range = [ 1, k_Right_depth( d, 1 ) ];
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Right_depth( :, :, d ), 'AlphaData', ~isnan( I_Right_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % 
% % lw = 1;
% % c_range = [ 1, 2 ];
% % 
% % figure( 'position', [ 100, 100, 200, 200 ] )
% % imagesc( timeDomain, timeDomain, I_Left_pool, 'AlphaData', ~isnan( I_Left_pool ), c_range )
% % colormap( gca, cool )
% % axis image
% % axis xy
% % hold on
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% % plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% % set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% % ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% % 
% % figure( 'position', [ 100, 100, 200, 200 ] )
% % imagesc( timeDomain, timeDomain, I_Right_pool, 'AlphaData', ~isnan( I_Right_pool ), c_range )
% % colormap( gca, cool )
% % axis image
% % axis xy
% % hold on
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% % plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% % plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% % set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% % set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% % ylabel( 'Observation time (s)' )
% % xlabel( 'Prediction time (s)' )
% % 
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Left_pool_depth( :, :, d ), 'AlphaData', ~isnan( I_Left_pool_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% % 
% % figure( 'position', [ 100, 100, 200, 600 ] )
% % for d = 1 : 3
% %     subplot( 3, 1, d )
% %     imagesc( timeDomain, timeDomain, I_Right_pool_depth( :, :, d ), 'AlphaData', ~isnan( I_Right_pool_depth( :, :, d ) ), c_range )
% %     colormap( gca, cool )
% %     axis image
% %     axis xy
% %     hold on
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 0, 0 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 1.3, 1.3 ], ':k', 'linewidth', lw )
% %     plot( [ timeDomain( 1 ), timeDomain( end ) ], [ 2.6, 2.6 ], ':k', 'linewidth', lw )
% %     plot( [ 0, 0 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 1.3, 1.3 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     plot( [ 2.6, 2.6 ], [ timeDomain( 1 ), timeDomain( end ) ], ':k', 'linewidth', lw )
% %     set( gca, 'xlim', [ timeDomain( 1 ), timeDomain( end ) ], 'ylim', [ timeDomain( 1 ), timeDomain( end ) ] )
% %     set( gca, 'xtick', [ 0, 1.3, 2.6 ], 'ytick', [ 0, 1.3, 2.6 ] )
% %     ylabel( 'Observation time (s)' )
% %     if d == 1
% %         title( 'Superficial layer (150-350 \mum)' )
% %     elseif d == 2
% %         title( 'Middle layer (350-550 \mum)' )
% %     elseif d == 3
% %         xlabel( 'Prediction time (s)' )
% %         title( 'Deep layer (550-750 \mum)' )
% %     end
% % end; clear d
% 
