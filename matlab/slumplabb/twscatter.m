function f = twscatter(F, varargin)
    % Takes a value-generating function [t, o] = F() and returns a function
    % [t, o] = f() which is equal in value, but different in side-effect.
    % f() will, in addition to its initial effects, also discreetly scatter
    % each value on a canvas (axes_handle).
    % This function takes a variable number of arguments; the first of
    % which is an axis_handle, and the rest is passed to scatter.
    
    
    % BROKEN, FIX!
    if (nargin > 1)
        axes_handle = varargin(1);
    
    function [t, o] = func()
        [t, o] = F();
        scatter(axes_handle, t, o, varargin{1:});
    end
    f = @func;
end
