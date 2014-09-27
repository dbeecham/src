function f = wplot2d(F, axes_handle, varargin)
    % Takes a value-generating function F and returns the same function,
    % but any value generated is also plotted on the canvas handle
    % axes_handle.
    x = 0;
    
    function o = func()
        o = F();
        scatter(axes_handle, x, o, varargin{:});
        x = x + 1;
    end
    f = @func;
end
