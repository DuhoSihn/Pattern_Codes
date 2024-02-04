function Y = GEP_findY( candY, X, w_ri, w_r, varargin )
% Finding predicted events Y (Equation 1).
%
% candY : the candidate events to be predicted, 1 := candidate & 0 := non-candidate, (1) X (the number of events) vector.
% X : the observation events, integer if observed & NaN otherwise, (1) X (the number of events) vector.
% w_ri : maximum likelihood parameters which are found by 'eventsPrediction_findW.m'.
% w_r : maximum likelihood parameters which are found by 'eventsPrediction_findW.m'.
%
% Y : the predicted events that be indicated by candY.
%
% constraint : constraint of solution space, it indicates the subspace satisfying one-hot encoding.
% constarint : the candidate events of subspace in the candidate events to be predicted, 1 := candidate & 0 := non-candidate.
% constraint : ( the number of constraints ) X ( the number of candidate events to be predicted ).
% e.g.) Y = GEP_findY( candY, X, w_ri, w_r, 'constraint', constraint )


type_constraint = find( strcmpi(varargin, 'constraint') == 1 );
if ~isempty( type_constraint )
    constraint = varargin( type_constraint + 1);
    constraint = constraint{1,1};
    
    if ~isempty( find( sum( constraint, 2 ) < 2, 1 ) )
        error('Each constraint should have the number of candidate events of subspace with >= 2.')
    end
    
    if ~isempty( find( sum( constraint, 1 ) > 1, 1 ) )
        error('Each constraint should be non-overlapped with others.')
    end
    
    [ nConstraints, nConstraintEvents ] = size( constraint );
    if nConstraintEvents ~= sum( candY, 2 )
        error('The number of events in constraint should be same as the number of candidate events to be predicted')
    end
    
    % projection matrix in order to project to subspace.
    list_constraint = sum( constraint, 2 );
    list_proj = unique( list_constraint );
    proj = cell( size( list_proj, 1 ), 1 );
    for k = 1 : size( list_proj, 1 )
        proj{ k, 1 } = eye( list_proj( k, 1) );
        proj{ k, 1 } = proj{ k, 1 }( :,  1 : end - 1 ) - proj{ k, 1 }( :, 2 : end );
        proj{ k, 1 } = proj{ k, 1 } * ( ( transpose( proj{ k, 1 } ) * proj{ k, 1 } ) \ transpose( proj{ k, 1 } ) );
        proj{ k, 1 } = transpose( proj{ k, 1 } );
    end
    
    idx_non_oneHot = sum( constraint, 1 ) == 0;
end


if ~isempty( find( ~isnan( X( candY == 1 ) ), 1 ) )
    error('The candidate events Y should contain no observation events X.')
end


nR = size(w_ri, 1);
if length(w_r) ~= nR, error('The number of rules should be identical.'), end


if ~isempty( find( isnan( w_ri ), 1 ) )
    error('w_ri contains NaN.')
end
if ~isempty( find( isnan( w_r ), 1 ) )
    error('w_r contains NaN.')
end

% In order to make a ordinal gradient (there is no neither inf or -inf), editing w_ri little bit.
w_ri( w_ri > 1 - 10^(-6) ) = 1 - 10^(-6);
w_ri( w_ri < 10^(-6) ) = 10^(-6);


% The activities of the rules --------------------
% Equation 11
P_X = bsxfun( @power, w_ri, X ) .* bsxfun( @power, 1 - w_ri, 1 - X );
P_X = prod( P_X, 2, 'omitnan' );
if all( bsxfun( @eq, P_X, 0 ) ), error('In case of P_X = 0 for every X, we cannot continue calculate procedure.'), end

% Equation 12
P_r = bsxfun( @times, P_X, w_r );
P_r = bsxfun( @rdivide, P_r, sum( P_r, 1 ) );
% ------------------------------------------------


% initial Y --------------------
% Equation 17
Y = sum( P_r .* w_ri, 1 );
Y = Y( candY == 1 );
if ~isempty( type_constraint )
    for k = 1 : nConstraints
        ini_Y_constraint = Y( constraint( k, : ) == 1 );
        [ ~, loc_ini_Y_max ] = max( ini_Y_constraint );
        ini_Y_constraint( loc_ini_Y_max ) = 1;
        ini_Y_constraint( ini_Y_constraint < 1 ) = 0;
        Y( constraint( k, : ) == 1 ) = ini_Y_constraint;
    end
end
Y = round( Y );
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disp(['Y = ', num2str(Y)])
% P_Y = bsxfun( @power, w_ri( :, candY == 1 ), Y ) .* bsxfun( @power, 1 - w_ri( :, candY == 1 ), 1 - Y );
% P_Y = prod( P_Y, 2 );
% disp(['P(Y|X,W) = ', num2str(sum(P_Y.*P_r,1))])
% disp([' '])
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ------------------------------


stopCriterion = 0;
while stopCriterion == 0
    
    % Equation 15 --------------------
    P_Y = bsxfun( @power, w_ri( :, candY == 1 ), Y ) .* bsxfun( @power, 1 - w_ri( :, candY == 1 ), 1 - Y );
    P_Y = prod( P_Y, 2 );
    % --------------------------------
    
    
    old_Y = Y;
    
    
    % gradient of Y --------------------
    % Equation 16
    dY = sum( bsxfun( @times, P_Y .* P_r, log( w_ri( :, candY == 1 ) ./ ( 1 - w_ri( :, candY == 1 ) ) ) ), 1 );
    % ----------------------------------
    
    
    % fast gradient ascent update Y --------------------
    if isempty( type_constraint )
        Y = Y + sign( dY );
        Y( Y > 1 ) = 1;
        Y( Y < 0 ) = 0;
    elseif ~isempty( type_constraint )
        for k = 1 : nConstraints
            dY_constraint = dY( constraint( k, : ) == 1 ) * proj{ list_constraint( k, 1 ) == list_proj, 1 };
            Y_constraint = Y( constraint( k, : ) == 1 );
            loc_Y_one = find( Y_constraint );
            if dY_constraint( 1, loc_Y_one ) >= 0
            elseif dY_constraint( 1, loc_Y_one ) < 0
                [ ~, loc_Y_max ] = max( dY_constraint, [], 2 );
                Y_constraint( 1, loc_Y_one ) = 0;
                Y_constraint( 1, loc_Y_max ) = 1;
                Y( constraint( k, : ) == 1 ) = Y_constraint;
            end
        end
        Y( 1, idx_non_oneHot ) = Y( 1, idx_non_oneHot ) + sign( dY( 1, idx_non_oneHot ) );
        Y( Y > 1 ) = 1;
        Y( Y < 0 ) = 0;
    end
    % --------------------------------------------------
    
    
    % Stop criterion --------------------
    if all( Y == old_Y )
        stopCriterion = 1;
    end
    % -----------------------------------
    
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %     disp(['Y = ', num2str(Y)])
    %     P_Y = bsxfun( @power, w_ri( :, candY == 1 ), Y ) .* bsxfun( @power, 1 - w_ri( :, candY == 1 ), 1 - Y );
    %     P_Y = prod( P_Y, 2 );
    %     disp(['P(Y|X,W) = ', num2str(sum(P_Y.*P_r,1))])
    %     disp([' '])
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end
