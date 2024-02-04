function sig = FDR_correction( pVals, pValSig, ctype )
% False Discorvery Rate correation
% 
% pVals : p-values, column vector, possibly contain NaN values.
% pValSig : significant level, e.g.) 0.05.
% ctype : type of FDR correction.
%
% sig : significance after FDR correction
% sig == 1, if pass the FDR correction
% sig == 0, o.w.

if isrow( pVals ), pVals = transpose( pVals ); end

if nargin < 3
    ctype = 'independent';
end

pValues = pVals( ~isnan( pVals ) );
pValues = sort( pValues );

if strcmpi( ctype, 'independent' )
    % Benjamini–Hochberg procedure
    cr = pValSig * transpose( 1 : length( pValues ) ) / length( pValues );
elseif strcmpi( ctype, 'general' )
    % Benjamini–Yekutieli procedure
    cr = pValSig * transpose( 1 : length( pValues ) ) / length( pValues );
    h = 1 ./ repmat( 1 : length( pValues ), [ length( pValues ), 1 ] ) ;
    h( logical( triu( ones( length( pValues ), length( pValues ) ), 1 ) ) ) = nan;
    h = sum( h, 2, 'omitnan' );
    cr = cr ./ h;
end

idx_pass = find( ( pValues <= cr ) == 1 );
if ~isempty( idx_pass )
sig = pVals <= pValues( max( idx_pass ) );
else
    sig = false( length( pVals ), 1 );
end
