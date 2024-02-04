function stat = fct_extractBox( pts )
% pts: points of data, N X 1 vector.
% 
% Example
% pts = squeeze( corr_A_RT( 3, 3, : ) );

pts = sort( pts );
pts = pts( ~isnan( pts ) );
levels = linspace( 1, length( pts ), 5 );
levels = levels( 2 : 4 );
stat = [];
for s = 1 : 3
    if mod( levels( s ), 1 ) == 0
        stat( s, 1 ) = pts( levels( s ) );
    else
        level_d = floor( levels( s ) );
        level_i = levels( s ) - level_d;
        level_u = level_d + 1;
        stat( s, 1 ) = ( 1 - level_i ) * pts( level_d ) + ( level_i ) * pts( level_u );
    end
end
