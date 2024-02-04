function [ idx, k, I, S ] = getInterval( A, k_pool, N_iter, N_rep )

% Symmetrization ----------------------------------------------------------
S = A;
for t1 = 1 : size( A, 1 ) - 1
    for t2 = t1 + 1 : size( A, 2 )
        % mS = min( [ S( t1, t2 ), S( t2, t1 ) ], [], 2 );
        mS = mean( [ S( t1, t2 ), S( t2, t1 ) ], 2 );
        S( t1, t2 ) = mS;
        S( t2, t1 ) = mS;
    end; clear t2
end; clear t1

S( logical( eye( size( S, 1 ) ) ) ) = 1;

% k-means clustering ------------------------------------------------------

x = [ 1 : size( S, 1 ) ];
idx_pool = nan( size( S, 1 ), length( k_pool ) );

ct_k = 0;
for k = k_pool

    idx_rep = nan( size( S, 1 ), N_rep );

    for rep = 1 : N_rep

        m = randperm( size( S, 1 ), k );
        idx = nan( size( S, 1 ), 1 );

        for iter = 1 : N_iter

            % Assignment step
            for kk = 1 : k
                idx( S( :, m( kk ) ) >= max( S( :, setdiff( m, m( kk ) ) ), [], 2 ) ) = kk;
            end

            % Update step
            for kk = 1 : k
                m( kk ) = mean( x( idx == kk ), 2 );
                m( kk ) = round( m( kk ) );
                if isnan( m( kk ) )
                    cand_m = setdiff( x, m );
                    m( kk ) = cand_m( randperm( length( cand_m ), 1 ) );
                end
            end

        end

        % idx re-ordering
        ridx = nan( size( idx ) );
        [ ~, order_m ] = sort( m, 2, 'ascend' );
        for kk = 1 : k
            ridx( idx == order_m( kk ) ) = kk;
        end
        idx_rep( :, rep ) = ridx;

    end

    idx = round( mean( idx_rep, 2 ) );
    % idx re-ordering
    m = randperm( size( S, 1 ), k );
    for kk = 1 : k
        m( kk ) = mean( x( idx == kk ), 2 );
        m( kk ) = round( m( kk ) );
        if isnan( m( kk ) )
            cand_m = setdiff( x, m );
            m( kk ) = cand_m( randperm( length( cand_m ), 1 ) );
        end
    end
    [ ~, order_m ] = sort( m, 2, 'ascend' );
    for kk = 1 : k
        ridx( idx == order_m( kk ) ) = kk;
    end
    idx = ridx;

    ct_k = ct_k + 1;
    idx_pool( :, ct_k ) = idx;

end

% silhouette method -------------------------------------------------------

S( logical( eye( size( S, 1 ) ) ) ) = nan;

sc = nan( 1, length( k_pool ) );
ct_k = 0;
for k = k_pool

    ct_k = ct_k + 1;
    idx = idx_pool( :, ct_k );

    a = nan( size( S, 1 ), 1 );
    b = nan( size( S, 1 ), 1 );
    for xx = 1 : size( S, 1 )
        a( xx, 1 ) = sum( S( xx, idx == idx( xx ) ), 2, 'omitnan' ) / ( sum( idx == idx( xx ), 1 ) - 1 );
        tb = nan( 1, k );
        for kk = 1 : k
            if kk ~= idx( xx )
               tb( 1, kk ) = sum( S( xx, idx == kk ), 2, 'omitnan' ) / sum( idx == kk, 1 );
            end
        end
        b( xx, 1 ) = min( tb, [], 2, 'omitnan' );
    end
    ts = ( b - a ) ./ max( [ a, b ], [], 2 );
    for xx = 1 : size( S, 1 )
        if sum( idx == idx( xx ), 1 ) == 1
            ts( xx ) = 0;
        end
    end
    sc( 1, ct_k ) = mean( ts, 1 );

end

[ ~, sc ] = max( sc, [], 2 );

idx = idx_pool( :, sc );
k = k_pool( sc );

I = nan( size( S, 1 ), size( S, 2 ) );
for kk = 1 : k
    idx_kk = find( idx == kk );
    for t1 = 1 : length( idx_kk )
        for t2 = t1 : length( idx_kk )
            I( idx_kk( t1 ), idx_kk( t2 ) ) = kk;
            I( idx_kk( t2 ), idx_kk( t1 ) ) = kk;
        end
    end
end


