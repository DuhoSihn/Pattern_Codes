function D_conds = getPatternAtypicalityConds( X, Y, tr_idx )
% Get, Pattern Atypicality from multivariate time series
% X : multivariate time series data, (time) X (channel) X (trial).
% D : Atypicality as time series, (time) X (trial).

D_conds = cell( size( Y, 3 ), 2 );
for tr_state = 1 : size( Y, 3 )
    if ~isempty( find( tr_idx == tr_state ) ) && size( find( tr_idx == tr_state ), 1 ) > 1
        % distance between signals at two trials
        X_dist = mean( abs( permute( X( :, :, tr_idx == tr_state ), [ 1, 3, 4, 2 ] ) - permute( X( :, :, tr_idx == tr_state ), [ 1, 4, 3, 2 ] ) ), 4 );
        % Atypicality
        D = sum( X_dist, 3 ) / ( sum( tr_idx == tr_state, 1 ) - 1 );
        D_conds{ tr_state, 1 } = mean( D, 2 );

        % distance between signals at two trials
        X_dist = permute( mean( abs( X( :, :, tr_idx == tr_state ) - Y( :, :, tr_state ) ), 2 ), [ 1, 3, 2 ] );
        D_conds{ tr_state, 2 } = mean( X_dist, 2 );
    end
end
