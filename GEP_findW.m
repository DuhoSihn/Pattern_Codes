function [ w_ri, w_r ] = GEP_findW( X, nR, varargin )
% Finding maximum likelihood parameters W = {w_ri, w_r}.
%
% X : the training set, 1 (event occur) or 0 (not) if observed & NaN otherwise, (the number of trials) X (the number of events) matrix.
% nR : the number of rules, scalar.
%
% w_ri : maximum likelihood parameters.
% w_r : maximum likelihood parameters.
%
% nMaxIter : the number of maximum allowable iterations of the EM algorithm, scalar.
% e.g.) [w_ri, w_r] = GEP_findW( X, nR, 'nMaxIter', nMaxIter )


type_nMaxIter = find( strcmpi(varargin, 'nMaxIter') == 1 );
if ~isempty( type_nMaxIter )
    nMaxIter = varargin( type_nMaxIter + 1);
    nMaxIter = nMaxIter{1,1};
end


[ ~, nE ] = size( X );% (the number of trials) X (the number of events)


% initial W --------------------
w_ri = randn( [nR, nE] ) * 10^(-2) + 0.5;
w_ri( w_ri > 1 ) = 1;
w_ri( w_ri < 0 ) = 0;
w_r = ones( [nR, 1] ) / nR;
% ------------------------------


stopCriterion = 0;
if ~isempty( type_nMaxIter )
    nIter = 0;
end
while stopCriterion == 0
    
    % Expectation step --------------------
    % Equation 5
    P_X = bsxfun( @power, w_ri, permute( X, [3, 2, 1] ) ) .* bsxfun( @power, 1 - w_ri, permute( 1 - X, [3, 2, 1] ) );
    P_X = prod( P_X, 2, 'omitnan' );
    
    % Equation 7
    P_r = bsxfun( @times, P_X, w_r );
    P_r = bsxfun( @rdivide, P_r, sum( P_r, 1 ) );
    % -------------------------------------
    
    
    old_w_ri = w_ri;
    old_w_r = w_r;
    
    
    % Maximization step --------------------
    % Equation 9
    w_ri = bsxfun( @rdivide, ...
        sum( repmat( P_r, [1, nE, 1] ) .* repmat( permute( X, [3, 2, 1] ), [nR, 1, 1] ), 3, 'omitnan' ), ...
        sum( repmat( P_r, [1, nE, 1] ) .* repmat( permute( ~isnan(X), [3, 2, 1] ), [nR, 1, 1] ), 3 ) );
    % Equation 10
    w_r = mean( P_r, 3 );
    % --------------------------------------
    
    
    % Stop criterion --------------------
    if max( max( abs( w_ri - old_w_ri ), [], 1 ), [], 2 ) < 10^(-3) ...
            && max( abs( w_r - old_w_r ), [], 1 ) < 10^(-3)
        stopCriterion = 1;
    end
    % -----------------------------------
    
    
    % Algorithm stop when reaching the user's allowable iterations --------------------
    if ~isempty( type_nMaxIter )
        nIter = nIter + 1;
        if nIter > nMaxIter
            stopCriterion = 1;
        end
    end
    % ---------------------------------------------------------------------------------
    
end


% Make sure, there is no NaN in the parameters W.
if ~isempty( find( isnan( w_ri ), 1 ) )
    m_w_ri = repmat( mean( w_ri, 2, 'omitnan' ), [ 1, nE ] );
    w_ri( isnan( w_ri ) ) = m_w_ri( isnan( w_ri ) );
    if ~isempty( find( isnan( w_ri ), 1 ) )
        w_ri( isnan( w_ri ) ) = 0.5;
    end
end
if ~isempty( find( isnan( w_r ), 1 ) )
    w_r( isnan( w_r ) ) = 1 / nR;
    w_r = w_r / sum( w_r, 1 );
end
