function A = getDecoding( X, C )
% Get, Decoding Accuracy from multivariate time series with trials
% Cross-temporal generalization of decoding
% X : Multivariate time series with trials, (time) X (channel) X (trial).
% C : Class, (trial) X 1.
% A : Decoding accuracy, (Training time) X (Test time).


T = size( X, 1 );

TR = size( X, 3 );
if TR >= 10
    N_folds = 10;% 10-fold cross-validation
else
    N_folds = 2;% 2-fold cross-validation
end
disp( [ num2str( N_folds ), '-folds cross-validation' ] )
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
    for t2 = 1 : T

        tA = NaN( N_iter, 1 );
        for iter = 1 : N_iter
            tL_folds = L_folds( randperm( TR ), 1 );% random partition
            tC = [];
            tout_C = [];
            for n = 1 : N_folds
                try
                    X_training = permute( X( t1, :, tL_folds ~= n ), [ 3, 2, 1 ] );
                    X_test = permute( X( t2, :, tL_folds == n ), [ 3, 2, 1 ] );
                    Mdl = fitcdiscr( X_training, C( tL_folds ~= n ) );% Decoding via linear discriminant analysis
                    out_C = predict( Mdl, X_test );
                    tC = [ tC; C( tL_folds == n ) ];
                    tout_C = [ tout_C; out_C ];
                catch
                end
            end
            if ~isempty( tout_C )
                tA( iter, 1 ) = mean( tC == tout_C, 1 );% Decoding accuracy
            end
        end
        if ~all( isnan( tA ), 'all' )
            A( t1, t2 ) = mean( tA, 1, 'omitnan' );
        end

    end; clear t2
end; clear t1
