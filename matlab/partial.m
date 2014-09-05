function f = partial(F, varargin)
    outerargs = varargin;
    function o = func(varargin)
        o = F(outerargs{:}, varargin{:});
    end
    f = @func;
end