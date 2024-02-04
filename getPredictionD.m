function [ A, H ] = getPredictionD( X )
% Get, Prediction Accuracy from multivariate time series with trials
% Diagonal components of Cross-temporal generalization of prediction
% X : Binary-valued (0, 1) multivariate time series with trials, (time) X (channel) X (trial).
% A : Prediction accuracy, (Observation/Prediction time) X (1).
% H : Shannon entropy for co-activation rule, (Observation/Prediction time) X (1).


T = size( X, 1 );

CH = size( X, 2 );
CH_thr = 100;% reduced the number of channels
if CH < CH_thr
    CH_thr = CH;
end
candY_1 = zeros( 1, 2 * CH_thr );
candY_2 = zeros( 1, 2 * CH_thr );
candY_1( CH_thr + 1 : end ) = 1;
candY_2( 1 : CH_thr ) = 1;

TR = size( X, 3 );
if TR >= 10
    N_folds = 10;% 10-fold cross-validation
elseif TR < 10 && TR >= 5
    N_folds = 5;% 2-fold cross-validation
else
    N_folds = 2;% 2-fold cross-validation
end
disp( [ num2str( N_folds ), '-fold cross-validation' ] )
P_folds = round( linspace( 1, TR, N_folds + 1 ) );
L_folds = [];
for p = 1 : N_folds
    if p == 1
        L_folds = [ L_folds; p * ones( P_folds( p + 1 ) - 0, 1 ) ];
    elseif p > 1
        L_folds = [ L_folds; p * ones( P_folds( p + 1 ) - P_folds( p ), 1 ) ];
    end
end; clear p
N_iter = 1;% # of iteration of k-fold cross-validation


A = NaN( T, 1 );
H = NaN( T, 1 );

for t1 = 1 : T
    t2 = t1;

    tA_1 = NaN( N_iter, 1 );
    tA_2 = NaN( N_iter, 1 );
    tH = NaN( N_iter, 1 );
    for iter = 1 : N_iter
        tL_folds = L_folds( randperm( TR ), 1 );% random partition
        tX_1 = [];
        tX_2 = [];
        tout_X_1 = [];
        tout_X_2 = [];
        tH_t = [];
        for n = 1 : N_folds
            try
                X_training_1 = permute( X( t1, :, tL_folds ~= n ), [ 3, 2, 1 ] );
                X_training_2 = permute( X( t2, :, tL_folds ~= n ), [ 3, 2, 1 ] );
                X_test_1 = permute( X( t1, :, tL_folds == n ), [ 3, 2, 1 ] );
                X_test_2 = permute( X( t2, :, tL_folds == n ), [ 3, 2, 1 ] );

                ch_idx = randperm( CH, CH_thr );
                X_training_1 = X_training_1( :, ch_idx );
                X_training_2 = X_training_2( :, ch_idx );
                X_test_1 = X_test_1( :, ch_idx );
                X_test_2 = X_test_2( :, ch_idx );

                % [w_ri, w_r] = GEP_findW( [ X_training_1, X_training_2 ], ceil( CH / 2 ) );% Learning via general event prediction
                [w_ri, w_r] = GEP_findW( [ X_training_1, X_training_2 ], ceil( CH / 2 ), 'nMaxIter', 1000 );% Learning via general event prediction
                out_X_1 = [];
                out_X_2 = [];
                for trial = 1 : size( X_test_1, 1 )
                    X_test_1_trial = NaN( 1, 2 * CH_thr );
                    X_test_2_trial = NaN( 1, 2 * CH_thr );
                    X_test_1_trial( 1 : CH_thr ) = X_test_1( trial, : );
                    X_test_2_trial( CH_thr + 1 : end ) = X_test_2( trial, : );
                    out_X_1( trial, : ) = GEP_findY( candY_1, X_test_1_trial, w_ri, w_r );% Prediction via general event prediction
                    out_X_2( trial, : ) = GEP_findY( candY_2, X_test_2_trial, w_ri, w_r );% Prediction via general event prediction
                end

                tX_1 = [ tX_1; X_test_2 ];
                tX_2 = [ tX_2; X_test_1 ];
                tout_X_1 = [ tout_X_1; out_X_1 ];
                tout_X_2 = [ tout_X_2; out_X_2 ];

                tH_tt = log2( w_r );
                tH_tt( w_r == 0 ) = 0;
                tH_t = [ tH_t; -sum( w_r .* tH_tt, 1 ) ];
            catch
            end
        end
        if ~isempty( tout_X_1 ) && ~isempty( tout_X_2 )
            tA_1( iter, 1 ) = mean( mean( tX_1 == tout_X_1, 2 ), 1 );% Prediction accuracy
            tA_2( iter, 1 ) = mean( mean( tX_2 == tout_X_2, 2 ), 1 );% Prediction accuracy
            tH( iter, 1 ) = mean( tH_t, 1, 'omitnan' );
        end
    end

    if ~all( isnan( tA_1 ), 'all' ) && ~all( isnan( tA_2 ), 'all' )
        A( t1, 1 ) = mean( [ mean( tA_1, 1, 'omitnan' ), mean( tA_2, 1, 'omitnan' ) ], 2, 'omitnan' );
        H( t1, 1 ) = mean( tH, 1, 'omitnan' );
    end

    clear t2
end; clear t1
