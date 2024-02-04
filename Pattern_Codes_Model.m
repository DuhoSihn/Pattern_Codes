% Pattern_Codes_Model
clear all; close all; clc
% addpath(genpath('C:\DATA\Util\matlab_codes'));



%% Simulations (simple)
% 
% N_iter = 30;
% nUnits = 80;
% nTrials = 100;
% rateScale = 5;
% 
% for k1 = 1 : 4
%         for k2 = 1 : 4
% 
%             perts = [ k1 * 0.1 * [ 1, 1, 1, 1 ], k1 * 0.1 * [ 1, 1, 1, 1 ], k1 * 0.1 * [ 1, 1, 1, 1 ] ];
%             parts = [ 1 * [ 1, 1, 1, 1 ], 2 * [ 1, 1, 1, 1 ], 3 * [ 1, 1, 1, 1 ] ];
%             ncLevel = k2 * 0.1;
% 
%             A_pool = [];
%             for iter = 1 : N_iter
%                 X = patternGen( nUnits, nTrials, rateScale, perts, parts, ncLevel );
%                 A = getPrediction( X );
%                 A_pool( :, :, iter ) = A;
%             end; clear iter
% 
%             save( [ 'Model_simple_k1_', num2str( k1 ), '_k2_', num2str( k2 ), '.mat' ], 'nUnits', 'nTrials', 'rateScale', 'perts', 'parts', 'ncLevel', 'N_iter', 'A_pool' )
%             disp( [ num2str( k1 ), ', ', num2str( k2 ) ] )
%         end; clear k2
% end; clear k1
% 
% 
% for k1 = 1 : 4
%         for k2 = 1 : 4
% 
%             load( [ 'Model_simple_k1_', num2str( k1 ), '_k2_', num2str( k2 ), '.mat' ] )
% 
%             [ ~, k_pool, I_pool, ~ ] = getInterval( mean( A_pool, 3 ), [ 3 ], 100, 30 );
% 
%             save( [ 'Model_simple_k1_', num2str( k1 ), '_k2_', num2str( k2 ), '.mat' ], 'nUnits', 'nTrials', 'rateScale', 'perts', 'parts', 'ncLevel', 'N_iter', 'A_pool', 'I_pool', 'k_pool' )
% 
%         end; clear k2
% end; clear k1
% 

%% Figure #1
% 
% nUnits = 80;
% nTrials1 = 20;
% rateScale = 5;
% perts = [ 0.0 * [ 1 ] ];
% parts = [ 1 * [ 1 ] ];
% ncLevel = 1;
% X1 = patternGen( nUnits, nTrials1, rateScale, perts, parts, ncLevel );
% 
% nUnits = 80;
% nTrials2 = 10;
% rateScale = 5;
% perts = [ 0.95 * [ 1 ] ];
% parts = [ 1 * [ 1 ] ];
% ncLevel = 1;
% X2 = patternGen( nUnits, nTrials2, rateScale, perts, parts, ncLevel );
% 
% Y1 = [];
% Y1( 1, 1 : nUnits, 1 : nTrials1 ) = X1;
% Y1( 1, 1 : nUnits, nTrials1 + [ 1 : nTrials2 ] ) = X2;
% 
% 
% nUnits = 80;
% nTrials1 = 20;
% rateScale = 5;
% perts = [ 0.0 * [ 1 ] ];
% parts = [ 1 * [ 1 ] ];
% ncLevel = 1;
% X1 = patternGen( nUnits, nTrials1, rateScale, perts, parts, ncLevel );
% 
% nUnits = 80;
% nTrials2 = 10;
% rateScale = 5;
% perts = [ 0.95 * [ 1 ] ];
% parts = [ 1 * [ 1 ] ];
% ncLevel = 1;
% X2 = patternGen( nUnits, nTrials2, rateScale, perts, parts, ncLevel );
% 
% Y2 = [];
% Y2( 1, 1 : nUnits, 1 : nTrials1 ) = X1;
% Y2( 1, 1 : nUnits, nTrials1 + [ 1 : nTrials2 ] ) = X2;
% 
% 
% nUnits = 80;
% nTrials1 = 20;
% rateScale = 5;
% perts = [ 0.0 * [ 1 ] ];
% parts = [ 1 * [ 1 ] ];
% ncLevel = 1;
% X1 = patternGen( nUnits, nTrials1, rateScale, perts, parts, ncLevel );
% 
% nUnits = 80;
% nTrials2 = 10;
% rateScale = 5;
% perts = [ 0.95 * [ 1 ] ];
% parts = [ 1 * [ 1 ] ];
% ncLevel = 1;
% X2 = patternGen( nUnits, nTrials2, rateScale, perts, parts, ncLevel );
% 
% Y3 = [];
% Y3( 1, 1 : nUnits, 1 : nTrials1 ) = X1;
% Y3( 1, 1 : nUnits, nTrials1 + [ 1 : nTrials2 ] ) = X2;
% 
% 
% idx_Trials1 = randperm( nTrials1 + nTrials2 );
% idx_Trials2 = randperm( nTrials1 + nTrials2 );
% 
% colors1 = [ 1, 0.8, 0.8; 1, 1, 1 ];
% colors2 = [ 1, 0, 0; 1, 1, 1 ];
% 
% 
% figure( 'position', [ 100, 100, 400, 400 ] )
% hold on
% for tr = 1 : nTrials1 + nTrials2
%     subplot( nTrials1 + nTrials2, 1, tr )
%     imagesc( Y1( 1, :, idx_Trials1( tr ) ) )
%     if idx_Trials1( tr ) <= nTrials1
%         colormap( gca, colors1 )
%     else
%         colormap( gca, colors2 )
%     end
%     axis off
% end; clear tr
% 
% 
% figure( 'position', [ 100, 100, 400, 400 ] )
% hold on
% for tr = 1 : nTrials1 + nTrials2
%     subplot( nTrials1 + nTrials2, 1, tr )
%     imagesc( Y2( 1, :, idx_Trials1( tr ) ) )
%     if idx_Trials1( tr ) <= nTrials1
%         colormap( gca, colors1 )
%     else
%         colormap( gca, colors2 )
%     end
%     axis off
% end; clear tr
% 
% 
% figure( 'position', [ 100, 100, 400, 400 ] )
% hold on
% for tr = 1 : nTrials1 + nTrials2
%     subplot( nTrials1 + nTrials2, 1, tr )
%     imagesc( Y3( 1, :, idx_Trials2( tr ) ) )
%     if idx_Trials2( tr ) <= nTrials1
%         colormap( gca, colors1 )
%     else
%         colormap( gca, colors2 )
%     end
%     axis off
% end; clear tr
% 

%% Figure #4
% 
% nUnits = 80;
% nTrials = 20;
% rateScale = 5;
% parts = 1;
% ncLevel = 1;
% 
% for perts = [ 0.1 : 0.1 : 0.4 ]
% 
%     X = patternGen( nUnits, nTrials, rateScale, perts, parts, ncLevel );
% 
%     X = permute( X, [ 3, 2, 1 ] );
% 
%     colors = [ 0, 0, 1; 1, 1, 1 ];
% 
%     figure( 'position', [ 100, 100, 200, 130 ] )
%     imagesc( X )
%     colormap( colors )
%     xlabel( 'Neuron' )
%     ylabel( 'Trial' )
%     title( [ '\kappa = ', num2str( 5 * perts ), ' Hz, \pi = ', num2str( 100 ), '%' ] )
% 
% end
% 

%% Figure #4, #5 simple
% 
% lw = 1;
% c_range = [ 0.5, 0.6 ];
% 
% figure( 'position', [ 100, 100, 600, 600 ] )
% 
% ct_k1 = 0;
% for k1 = 1 : 4
%     ct_k1 = ct_k1 + 1;
%     ct_k2 = 0;
%     for k2 = 1 : 4
%         ct_k2 = ct_k2 + 1;
% 
%         load( [ 'Model_simple_k1_', num2str( k1 ), '_k2_', num2str( k2 ), '.mat' ] )
% 
%         A = mean( A_pool, 3 );
%         A( logical( eye( size( A, 1 ) ) ) ) = nan;
% 
%         subplot( 4, 4, ( ct_k1 - 1 ) * 4 + ct_k2 )
%         imagesc( A, 'alphadata', ~isnan( A ), c_range )
%         % colorbar
%         colormap( gca, turbo )
%         axis image
%         axis xy
%         hold on
%         plot( [ 4.5, 4.5 ], [ 0.5, 12.5 ], ':k', 'linewidth', lw )
%         plot( [ 8.5, 8.5 ], [ 0.5, 12.5 ], ':k', 'linewidth', lw )
%         plot( [ 0.5, 12.5 ], [ 4.5, 4.5 ], ':k', 'linewidth', lw )
%         plot( [ 0.5, 12.5 ], [ 8.5, 8.5 ], ':k', 'linewidth', lw )
%         set( gca, 'xlim', [ 0.5, 12.5 ], 'ylim', [ 0.5, 12.5 ] )
%         set( gca, 'xtick', [ 1 : 12 ], 'ytick', [ 1 : 12 ] )
%         set( gca, 'xticklabel', { '1', '1', '1', '1', '2', '2', '2', '2', '3', '3', '3', '3' } )
%         set( gca, 'yticklabel', { '1', '1', '1', '1', '2', '2', '2', '2', '3', '3', '3', '3' } )
%         ylabel( 'Observation' )
%         xlabel( 'Prediction' )
%         title( [ '\kappa = ', num2str( 5 * 0.1 * k1 ), ' Hz, \pi = ', num2str( 0.1 * k2 * 100 ), '%' ] )
% 
%     end; clear k2 ct_k2
% end; clear k1 ct_k1
% 
% 
% % -------------------------------------------------------------------------
% 
% figure( 'position', [ 100, 100, 600, 600 ] )
% 
% ct_k1 = 0;
% for k1 = 1 : 4
%     ct_k1 = ct_k1 + 1;
%     ct_k2 = 0;
%     for k2 = 1 : 4
%         ct_k2 = ct_k2 + 1;
% 
%         load( [ 'Model_simple_k1_', num2str( k1 ), '_k2_', num2str( k2 ), '.mat' ] )
% 
%         lw = 1;
%         c_range = [ 1, k_pool ];
% 
%         subplot( 4, 4, ( ct_k1 - 1 ) * 4 + ct_k2 )
%         imagesc( I_pool, 'alphadata', ~isnan( I_pool ), c_range )
%         % colorbar
%         colormap( gca, cool )
%         axis image
%         axis xy
%         hold on
%         plot( [ 4.5, 4.5 ], [ 0.5, 12.5 ], ':k', 'linewidth', lw )
%         plot( [ 8.5, 8.5 ], [ 0.5, 12.5 ], ':k', 'linewidth', lw )
%         plot( [ 0.5, 12.5 ], [ 4.5, 4.5 ], ':k', 'linewidth', lw )
%         plot( [ 0.5, 12.5 ], [ 8.5, 8.5 ], ':k', 'linewidth', lw )
%         set( gca, 'xlim', [ 0.5, 12.5 ], 'ylim', [ 0.5, 12.5 ] )
%         set( gca, 'xtick', [ 1 : 12 ], 'ytick', [ 1 : 12 ] )
%         set( gca, 'xticklabel', { '1', '1', '1', '1', '2', '2', '2', '2', '3', '3', '3', '3' } )
%         set( gca, 'yticklabel', { '1', '1', '1', '1', '2', '2', '2', '2', '3', '3', '3', '3' } )
%         ylabel( 'Observation' )
%         xlabel( 'Prediction' )
%         title( [ '\kappa = ', num2str( 5 * 0.1 * k1 ), ' Hz, \pi = ', num2str( 0.1 * k2 * 100 ), '%' ] )
% 
%     end; clear k2 ct_k2
% end; clear k1 ct_k1
% 
