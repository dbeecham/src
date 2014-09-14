function o = l2u3()
    tal = input('Skriv in ett tal: ');
    if tal == 1
        fprintf('One.\n');
    elseif tal == 2
        fprintf('Two.\n');
    elseif tal == 3
        fprintf('Three.\n');
    else
        error('Not 1, 2 or 3!');
        return
    end
end