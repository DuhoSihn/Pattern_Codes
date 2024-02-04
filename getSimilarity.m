function S = getSimilarity( X )
% Get, Cosine similarity from multivariate time series with trials
% Cross-temporal generalization of prediction
% X : Binary-valued (0, 1) multivariate time series with trials, (time) X (channel) X (trial).
% S : Cosine similarity, (Observation time) X (Prediction time).


T = size( X, 1 );

S = NaN( T, T );

for t1 = 1 : T
    for t2 = t1 : T
        M1 = permute( X( t1, :, : ), [ 3, 2, 1 ] );
        M2 = permute( X( t2, :, : ), [ 3, 2, 1 ] );
        M1 = mean( M1, 1, 'omitnan' );
        M2 = mean( M2, 1, 'omitnan' );
        S( t1, t2 ) = cosineSimilarity( M1, M2 );
        S( t2, t1 ) = S( t1, t2 );
    end; clear t2
end; clear t1
