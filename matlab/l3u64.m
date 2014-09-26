function [] = l3u64()
    function o = extractDigits(x, n)
        o = floor(x * 10^(n-1));
    end

    piappr = piapprox();

    fst_found = 0;
    snd_found = 0;
    thr_found = 0;
    i = 0;

    %p = gcp();
    %parfeval(p, @parplot, 0);

    while true
        p = piappr();

        % I dont care about divide by 0 errors.
        try

            if extractDigits(p, 2) == 31
                if fst_found == 0
                    fprintf('Found first digit at N = %i.\n', i);
                    fst_found = 1;
                end
            else
                if fst_found == 1
                    fprintf('Lost first digit at N = %i.\n', i);
                    fst_found = 0;
                end
            end

            if extractDigits(p, 3) == 314
                if snd_found == 0
                    fprintf('Found second digit at N = %i.\n', i);
                    snd_found = 1;
                end
            else
                if snd_found == 1
                    fprintf('Lost second digit at N = %i.\n', i);
                    snd_found = 0;
                end
            end

            if extractDigits(p, 4) == 3141
                if thr_found == 0
                    fprintf('Found third digit at N = %i.\n', i);
                    thr_found = 1;
                end
            else
                if thr_found == 1
                    fprintf('Lost third digit at N = %i.\n', i);
                    thr_found = 0;
                end
            end



        catch err
            if (strcmp(err.identifier, 'MATLAB:badsubscript'))
                continue
            else
                disp(err);
                break
            end
        end

        i = i + 1;
    end
end
