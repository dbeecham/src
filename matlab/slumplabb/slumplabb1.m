function [] = slumplabb1()

    % Första uppgiften:
    % Skriv ett program som genererar 1-dimensionella slumpvandringar
    % med ett givet antal steg, N.

    N = 100;
    w = walker(0, randDiscreteStepper({-1, 1}));
    xs = replicate(w, N)
        
end
