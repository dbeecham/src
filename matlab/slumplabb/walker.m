function F = walker(varargin)
% walker takes an initial position and a stepper function and returns a
% function which takes a step and returns it's new position. If initial
% position is not specified, an inital position of 0 is assumed.
    
    if nargin > 1
        p = varargin{1};
        f = varargin{2};
    else
        p = 0;
        f = varargin{1};
    end
        
    function P = func()
        p = f(p);
        P = p;
    end
    F = @func;
end
