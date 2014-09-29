function f = amalgamate(dim, varargin)
% amalgamate takes a variable number of functions (at least two) and
% returns a function which is the union of all of them. The function will
% call each function and return a matrix array of the results. The results
% is concatenated along the 'dim' dimension.
    function o = func()
        for i=1:length(varargin)
            tmp{i} = varargin{i}();
            o = cat(dim, tmp{:});
        end
    end
    f = @func;
end

