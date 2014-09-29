function f = wscatter(F, varargin)
    % wscatter takes a waker which returns a 2 dimensional walker and
    % scatters it on a plot as a side effect.
    % w2scatter also takes a variable number of arguments, the first of
    % which is a boolean value which decides if w2scatter should create
    % a new figure (0) or use a handle (1). If this value is 1, the next
    % value must be an axes_handle.
    % Every value after is passed directly to scatter.

    % Set up figure.
    if nargin > 1 && varargin{1} == 1
        axes_handle = varargin{2};
        varargin(1:2) = [];
    else
        figure();
        axes_handle = subplot(1, 1, [1]);
        hold(axes_handle);
    end


    function o = func()
        o = F();
        scatter(axes_handle, o(1), o(2), varargin{:});
    end
    f = @func;
end
