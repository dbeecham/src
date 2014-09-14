function [] = uppgift3()
    function o = evennumber(x)
        % evennnumber :: Int -> Bool
        o = (rem(x, 2) == 0);
    end
    
    % Loop over 1:100, if number is even then 1, else -1.
    mask = arrayfun(@(x) iff(evennumber(x), 1, -1), 1:100);
    
    % Multiply the mask elementwise onto 1:100.
    l = (1:100) .* mask;
    
    % Map the display function on the resulting list.
    arrayfun(@disp, l);
end


% for i = 1:100
% if rem(i, 2) == 0 then mask = 1 else mask = -1 end
% disp(i * mask)
% end