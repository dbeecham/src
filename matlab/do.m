function o = do(varargin)
    for i = 1:length(varargin)
        o = varargin{i}();
    end
end