function A = getPredictionMLP( X )
% Get, Prediction Accuracy from multivariate time series with trials
% Cross-temporal generalization of prediction
% X : Binary-valued (0, 1) multivariate time series with trials, (time) X (channel) X (trial).
% A : Prediction accuracy, (Observation time) X (Prediction time).


T = size( X, 1 );

CH = size( X, 2 );
CH_thr = 100;% reduced the number of channels
if CH < CH_thr
    CH_thr = CH;
end

TR = size( X, 3 );
if TR >= 10
    N_folds = 10;% 10-fold cross-validation
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


A = NaN( T, T );

for t1 = 1 : T
    for t2 = t1 : T

        tA_1 = NaN( N_iter, 1 );
        tA_2 = NaN( N_iter, 1 );
        for iter = 1 : N_iter
            tL_folds = L_folds( randperm( TR ), 1 );% random partition
            tX_1 = [];
            tX_2 = [];
            tout_X_1 = [];
            tout_X_2 = [];
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

                    net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
                    net = train( net, transpose( X_training_1 ), transpose( X_training_2 ) );
                    results = net( transpose( X_test_1 ) );
                    results = transpose( results );
                    results( results >= 0.5 ) = 1;
                    results( results < 0.5 ) = 0;
                    out_X_1 = results;

                    net = fitnet( [ ceil( CH / 2 ), ceil( CH / 2 ) ], 'trainlm' );
                    net = train( net, transpose( X_training_2 ), transpose( X_training_1 ) );
                    results = net( transpose( X_test_2 ) );
                    results = transpose( results );
                    results( results >= 0.5 ) = 1;
                    results( results < 0.5 ) = 0;
                    out_X_2 = results;

                    tX_1 = [ tX_1; X_test_2 ];
                    tX_2 = [ tX_2; X_test_1 ];
                    tout_X_1 = [ tout_X_1; out_X_1 ];
                    tout_X_2 = [ tout_X_2; out_X_2 ];
                catch
                end
            end
            if ~isempty( tout_X_1 ) && ~isempty( tout_X_2 )
                tA_1( iter, 1 ) = mean( mean( tX_1 == tout_X_1, 2 ), 1 );% Prediction accuracy
                tA_2( iter, 1 ) = mean( mean( tX_2 == tout_X_2, 2 ), 1 );% Prediction accuracy
            end
        end

        if ~all( isnan( tA_1 ), 'all' ) && ~all( isnan( tA_2 ), 'all' )
            A( t1, t2 ) = mean( tA_1, 1, 'omitnan' );
            A( t2, t1 ) = mean( tA_2, 1, 'omitnan' );
        end

        save( 'temp_MLP_A.mat', 'A' )
        disp( [ '( ', num2str( t1 ), ', ', num2str( t2 ), ' ) / ', num2str( T ) ] )
    end; clear t2
end; clear t1
