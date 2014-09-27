function f = timewalker(F, varargin)
    % This function takes a walker and adds a time dimension to it.
    % It takes 2 optional arguments, an initial time [0] and a time delta
    % [1].
    
    dt = 1;
    time = 0;
    if (nargin > 2)
        dt = varargin(2);
    end
    if (nargin > 1)
        time = varargin(1);
    end
    
    function [t, o] = func()
        t = time;
        time = time + 1;
        
        o = F();
    end
    f = @func;
end

