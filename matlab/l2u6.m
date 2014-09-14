function [] = l2u6()

    % Draw a circle.
    function circle(x, y, r)
        hold on
        xs = (r * cos(linspace(0, 2*pi))) + x;
        ys = (r * sin(linspace(0, 2*pi))) + y;
        plot(xs, ys);
        hold off
    end

    % An attempt at abstracting trail behaviour.
    % Apparently cellfun cannot return a list of functions.
    function f = remtail(size, order, preserve_order, F)
        if nargin < 3
            preserve_order = 0;
        end
        
        values = arrayfun(@(x) F(), 1:size);
        cur_index = 1;
        
        function P = func()
            if preserve_order
            else
                values(cur_index:(cur_index+order)) = F();
                cur_index = mod(cur_index + 1 + order, size * order) + 1;
            end
        end
        f = @func;
    end

    % A cell structure with balls.
    balls = {
        bouncy_ball([0.1, 0.1], [0.01, -0.01], [0, 1])
        bouncy_ball([0.1, 0.2], [-0.03, -0.02], [0, 1])
    };

    % Number of positions to store.
    num_positions = 50;
    
    % Initialize a vector with ball positions.
    positions = zeros(length(balls), num_positions, 2);
    for i=1:length(balls)
        for j=1:num_positions
            [x, y] = balls{i}();
            positions(i, j, :) = [x, y];
        end
    end
    cur_pos = 1;
    
    % Main loop
    while true
        
        % Clear canvas
        clf
        
        for i=1:length(balls)
            
            % Step ball
            [x, y] = balls{i}();
            
            % Update tail positions
            positions(i, cur_pos, :) = [x, y];
            
            % Draw circle and tail.
            circle(x, y, 0.05);
            hold on
            scatter(positions(i, :, 1), positions(i, :, 2));
            hold off
        end
        
        % Increment index mod size.
        cur_pos = mod(cur_pos, num_positions) + 1;
        
        % For some reason, axis is automatically updated at plot?
        axis([0, 1, 0, 1]);
        
        pause(1/10);
    end
end