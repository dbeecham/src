function f = twplot2d(F, axes_handle, varargin)
    % Takes a value-generating function [t, o] = F() and returns a function
    % [t, o] = f() which is equal in value, but different in side-effect.
    % f() will, in addition to its initial effects, also discreetly scatter
    % each value on a canvas (axes_handle).
    % This function takes a variable number of arguments, all of which are
    % passed to 'scatter'.
    
    function [t, o] = func()
        [t, o] = F();
        scatter(axes_handle, t, o, varargin{:});
    end
    f = @func;
end
