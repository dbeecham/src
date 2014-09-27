function f = partial(F, varargin)
    % Partial takes a function f and a variable number of arguments a,
    % and returns a function which takes a variable number of
    % arguments b, which calls f(a b). Note that |a| + |b| = |f|.
    
    outerargs = varargin;
    function o = func(varargin)
        o = F(outerargs{:}, varargin{:});
    end
    f = @func;
end
