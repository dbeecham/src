function F = linstep(varargin)
% linstep returns a linear stepper function which takes a value v and
% returns k+v. If k is not specified, a default value of 1 is assumed.

    if nargin > 0
        k = varargin{1};
    else
        k = 1;
    end
    
    function o = func(v)
        o = v + k;
    end
    F = @func;
end

