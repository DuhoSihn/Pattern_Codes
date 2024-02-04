function h = fct_boxplot( pts, xRange, markersize_pts, linewidth_line, color_pts, color_line )
% pts: points of data, N X 1 vector.
% xRange: x-axis range of plot, 1 X 2 vector.
% markersize_pts: marker size of points, scalar.
% linewidth_line: line width if line, scalar.
% color_pts: color of points, 1 X 3 vector.
% color_line: color of line, 1 X 3 vector.

% Example
% pts = squeeze( corr_A_RT( 3, 3, : ) );
% xRange = [ 0.8, 1.2 ];
% markersize_pts = 10;
% linewidth_line = 2;
% color_pts = [ 0, 0, 0 ];
% color_line = [ 1, 0, 0 ];

stat = fct_extractBox( pts );

hold on
plot( ( xRange( 2 ) - xRange( 1 ) ) * rand( length( pts ), 1 )  + xRange( 1 ), pts, '.', 'markersize', markersize_pts, 'color', color_pts )
for s = 1 : 3
    plot( xRange, stat( s ) * [ 1, 1 ], '-', 'linewidth', linewidth_line, 'color', color_line )
end
plot( xRange( 1 ) * [ 1, 1 ], [ stat( 1 ), stat( 3 ) ], '-', 'linewidth', linewidth_line, 'color', color_line )
plot( xRange( 2 ) * [ 1, 1 ], [ stat( 1 ), stat( 3 ) ], '-', 'linewidth', linewidth_line, 'color', color_line )
