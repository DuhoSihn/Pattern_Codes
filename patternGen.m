function [ X, Y ] = patternGen( nUnits, nTrials, rateScale, perts, parts, ncLevel )
% nUnits = 80;
% nTrials = 100;
% rateScale = 5;
% perts = [ 0 * [ 1, 1, 1, 1 ], 0.3* [ 1, 1, 1, 1 ] ];
% parts = [ 1 * [ 1, 1, 1, 1 ], 2 * [ 1, 1, 1, 1 ] ];
% ncLevel = 0.5;

idxs = cell( length( perts ), 3 );
for t = 1 : length( perts )
    idx1 = randperm( nUnits, round( nUnits / 2 ) );
    idx2 = setdiff( 1 : nUnits,  idx1 );
    idxs{ t, 1 } = idx1;
    idxs{ t, 2 } = idx2;
    idx5 = randperm( nUnits, round( nUnits / 2 ) );
    idx6 = setdiff( 1 : nUnits,  idx5 );
    idxs{ t, 5 } = idx5;
    idxs{ t, 6 } = idx6;
end
parts_idx = unique( parts );
for cidx = 1 : length( parts_idx )
    idx3 = randperm( nTrials, round( nTrials * ncLevel ) );
    idx4 = setdiff( 1 : nTrials,  idx3 );
    for t = 1 : length( parts )
        if parts( t ) == parts_idx( cidx )
            idxs{ t, 3 } = idx3;
            idxs{ t, 4 } = idx4;
        end
    end
end

Y = poissrnd( rateScale, length( perts ), nUnits, nTrials );% firing rates (time X unit X trial)
for t = 1 : length( perts )
    idx1 = idxs{ t, 1 };
    idx2 = idxs{ t, 2 };
    idx3 = idxs{ t, 3 };
    Y( t, idx1, idx3 ) = Y( t, idx1, idx3 ) + perts( t ) * rateScale * ones( 1, length( idx1 ), length( idx3 ) );
    Y( t, idx2, idx3 ) = Y( t, idx2, idx3 ) - perts( t ) * rateScale * ones( 1, length( idx2 ), length( idx3 ) );
    idx5 = idxs{ t, 5 };
    idx6 = idxs{ t, 6 };
    idx4 = idxs{ t, 4 };
    Y( t, idx5, idx4 ) = Y( t, idx5, idx4 ) + perts( t ) * rateScale * 0.5 * ( ones( 1, length( idx5 ), length( idx4 ) ) .* rand( 1, length( idx5 ), length( idx4 ) ) );
    Y( t, idx6, idx4 ) = Y( t, idx6, idx4 ) - perts( t ) * rateScale * 0.5 * ( ones( 1, length( idx6 ), length( idx4 ) ) .* rand( 1, length( idx6 ), length( idx4 ) ) );
end

X = Y;% co-activation patterns
% X = X - mean( X, 1 );
% X = X ./ std( X, 0, 1 );
X = X >= median( X, 2 );
X = double( X );
