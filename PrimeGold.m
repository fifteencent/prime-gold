prompt = "Enter your three numbers as a 1x3 (rxc) matrix: ";
A = input(prompt);
while size(A,2) ~= 3
    disp("Hey! You're not playing Prime Gold! (or your matrix has too many columns!)")
    A = input(prompt);
end
sort = howToSort();
B = possibleResults(A)';
C = unique(B(:,1));
D = primePairs(C)';
disp(englishEquation(B))
disp(englishPairs(D, sort))

function Z = englishPairs(pairs, sort)
    
    switch sort
        case "c"
        case "ph"
            pairs = [pairs(:,2) pairs(:,3) pairs(:,1)];
        otherwise
            pairs = [pairs(:,3) pairs(:,2) pairs(:,1)];
    end

    pairs = unique(pairs, "rows");
    
    [m,~] = size(pairs);
    
    Z = zeros(m,1);
    Z = string(Z);
    for i = 1:m
        a = 1;
        while Z(a,1) ~= "0"
            a = a + 1;
        end
        if sort == "ph" || sort == "pl"
            Z(a,1) = pairs(i,1) + " + " + pairs(i,2) + " = " + pairs(i,3);
        else
            Z(a,1) = pairs(i,1) + " = " + pairs(i,2) + " + " + pairs(i,3);
        end
    end
    
    i = m;
    while Z(i,1) == "0"
        i = i - 1;
    end
    
    Z = char(Z(1:i,:));
end

function Z = englishEquation(eqns)
    [m,n] = size(eqns);
    
    z = (n + 1)/4; %n = 4*z-1 (where z is amount of numbers in input)
    results = eqns(:,1);
    inputs = eqns(:,2:z+1); %2 + z -1
    factorials = eqns(:,z+2:3*z); %z+2+2z-1-1
    operations = eqns(:,3*z+1:4*z-1);%3*z+1+z-1-1
        
    Z = zeros(m,1);
    Z = string(Z);
    for a = 1:m
        %puts exclamation mark after factorialed numbers
        N = zeros(1,z);
        N = string(N);
        for i = 1:z
            if factorials(a,i) == 2
                N(i) = inputs(a,i) + "!";
            else
                N(i) = inputs(a,i);
            end
        end
        
        %adds operations and extra factorials
        x = N(1);
        for i = 1:z-1
            next = N(i+1);
            bank = [x next];
            b = factorials(a,2*z-i);%2*z-1+1-i
            c = operations(a,i);
            switch c
                case 1
                    x = bank(1) + " - " + bank(2);
                case 2
                    x = bank(1) + " + " + bank(2);
                case 3
                    x = bank(1) + " / " + bank(2);
                case 4
                    x = bank(1) + " * " + bank(2);
                otherwise
                    x = bank(1) + " ^ " + bank(2);
            end
            
            evenCase = i < z-1 && mod(c,2) == 0 && operations(a,i+1) > c;
            oddCase = i < z-1 && mod(c,2) ~= 0 && operations(a,i+1) > c + 1;
            if b == 2 || evenCase || oddCase
                x = "(" + x + ")";
            end
            if b == 2
                x = x + "!";
            end
        end
        
        i = 1;
        while Z(i,1) ~= "0"
            i = i + 1;
        end
        
        if a > 1 && results(a,1) ~= results(a-1,1) || a == 1
            Z(i,1) = results(a,1) + " = " + x;
        else 
            Z(i-1,1) = Z(i-1,1) + " = " + x;
        end
    end
    
    i = m;
    while Z(i,1) == "0"
        i = i - 1;
    end
    
    Z = char(Z(1:i,:));
    
end

function Z = primePairs(composites)
    %A is a column matrix of possible results
    composites = findEvenComposites(composites);
    [~,n] = size(composites);
    primes = primesToNumber(121);
    [~,o] = size(primes);
    primes = primes(:, 2:o); % Gets rid of 2
    
    Z = zeros(3,n);
    for a = 1:n
        b = 1;
        while primes(b) < composites(a)/2 && b < o - 1
            x = composites(a) - primes(b);
            A = ismember(x, primes);
            if A*A' == 1
                X = [composites(a);x;primes(b)];
                if Z(1,n) == 0
                    z = 1;
                    while Z(1,z) ~= 0 && z < n
                        z = z + 1;
                    end
                    Z(:,z) = X;
                else
                    Z = [Z X];
                end
            end
            b = b + 1;
        end
    end
end

function Z = findEvenComposites(A)
    [m, ~] = size(A);
    Z = zeros(1,m);
    for i = 1:m
        if mod(A(i,1), 2) == 0 && A(i,1) > 7
            a = 1;
            while Z(a) ~= 0 && a < m
                a = a + 1;
            end
            Z(a) = A(i,1);
        end 
    end
    
    i = m;
    while Z(i) == 0
        i = i - 1;
    end
    
    Z = Z(:,1:i);
end

function Z = primesToNumber(n)
    %a = number (primes up to a)
    
    N = 2:n;
    
    for i = 3:2:n-1
        N(i) = 0;
    end

    p = 3;

    while p^2 <= n 
        for i = p^2 : 2*p : n %remvove multiples of prime. Start at p * p
            N(i - 1) = 0;
        end
        
        p = p + 2;
        
        while N(p - 1) == 0 
            p = p + 2;
        end
    end

    N = unique(N','rows')';
    
    N(i) = 2;
    Z = N;
end

function B = possibleResults(A)
    [~,m] = size(A);
    A = perms(A);
    order = permn([1 2], 2*m-1);%first n amount is for factorial or not, and last n-1 are factorial or not after each operation
    operations = permn([1 2 3 4 5], m-1);
    
    [n,~] = size(A);
    [o,~] = size(order);
    [p,~] = size(operations);
    B = zeros(4*m-1,n*o*p);
    
    i = 1;
    while i <= n
        X = orderLayer(A(i,:), order, operations);
        
        [~,p] = size(X);
        a = 1;
        while B(1,a) ~= 0 && a < n*o*p
            a = a + 1;
        end
        B(:,a:a+p-1) = X;
        
        i = i + 1;
    end
    B = unique(B.', "rows").';
    [~,o] = size(B);
    B = B(:,2:o);
end

function Z = orderLayer(A, B, C)
    %A = input + factorials
    %B = order matrix
    %C = operation matrix (entire thing)
    %Z = [number matrix]
    A = [A' factorial(A')];
    [m,~] = size(A);
    [n,~] = size(B);
    [o,~] = size(C);
    Z = zeros(4*m-1,n*o);
    
    i = 1;
    while i <= n
        X = operationLayer(A, B(i, :), C);
        
        [~,p] = size(X);
        a = 1;
        while Z(1,a) ~= 0 && a < n*o
            a = a + 1;
        end
        Z(:,a:a+p-1) = X;
        
        i = i + 1;
    end
    Z = unique(Z.', "rows").';
    [~,o] = size(Z);
    Z = Z(:,2:o);
end

function Z = operationLayer(A, B, C)
    %A = input + factorials
    %B = order (one row) eg [1 2 1]
    %C = operation matrix (entire thing)
    %Z = [number matrix]
    i = 1;
    [m, ~] = size(C);
    
    [n, ~] = size(A);
 
    Z = zeros(4*n-1,m);
    
    Y = realInput(A, B);
    while i <= m
        x = operateRow(Y, B, C(i,:));
        if x < 230 && x > 0
            X = [x; A(:,1); B'; C(i,:)'];
            Z(:,i) = X;
        end
        i = i + 1;
    end
    Z = unique(Z.', "rows").';
    [~,o] = size(Z);
    Z = Z(:,2:o);
end

function z = operateRow(A, B, C)
    %A = input according to matrix B
    %B = order (one row) eg [1 2 1]
    %C = operation matrix (one row)
    %Z = final number
    i = 1;
    [~, n] = size(C);
    x = A(1,1);
    [~,m] = size(A);
    while i <= n && i < m
        if i > n
            break
        end
        next = A(1,i+1);
        bank = [x next];
        [~,n] = size(B);
        x = operate(bank, B(1,n+1-i), C(1, i));
        i = i + 1;
        if x < 0
            break
        end
    end
    z = x;
end

function z = operate(A, b, c)
    %A = bank 
    %b = whether final number is factorialized or not
    %c = operation (one number) eg 3
    %Z = [number, number!]
    switch c
        case 1
            x = A(1,1) - A(1,2);
        case 2
            x = A(1,1) + A(1,2);
        case 3
            x = A(1,1) / A(1,2);
        case 4
            x = A(1,1) * A(1,2);
        otherwise
            x = A(1,1) ^ A(1,2);
    end
    x = abs(x);
    if mod(x,1) ~= 0
        z = -1;
    else
        if b == 1
            z = x;
        else
            z = factorial(x);
        end
    end
end

function Z = realInput(A, B)
    %A = input + factorials
    %B = order (one row) eg [1 2 1]
    [m,~] = size(A);
    i = 1;
    Z = [1 1];
    while i <= m
        factorial = B(1, i);
        Z(i) = A(i, factorial);
        i = i + 1;
    end
end

function keys = howToSort
    prompt = 'How would you like to sort pairs? ';
    str = input(prompt, 's');
    keywords = ["composite", "prime high", "prime low"];
    shorthand = ["c", "ph", "pl"];
    switch str
        case keywords(1)
            keys = shorthand(1);
        case shorthand(1)
            keys = shorthand(1);
        case keywords(2)
            keys = shorthand(2);
        case shorthand(2)
            keys = shorthand(2);
        otherwise
            keys = shorthand(3);
    end
    
    if contains(str, [keywords shorthand]) == 0
        disp('Please use one of the keywords: "composite", "prime high", or "prime low"!')
        keys = howToSort;
    end
end

function [M, I] = permn(V, N, K)
% PERMN - permutations with repetition
%   Using two input variables V and N, M = PERMN(V,N) returns all
%   permutations of N elements taken from the vector V, with repetitions.
%   V can be any type of array (numbers, cells etc.) and M will be of the
%   same type as V.  If V is empty or N is 0, M will be empty.  M has the
%   size numel(V).^N-by-N. 
%
%   When only a subset of these permutations is needed, you can call PERMN
%   with 3 input variables: M = PERMN(V,N,K) returns only the K-ths
%   permutations.  The output is the same as M = PERMN(V,N) ; M = M(K,:),
%   but it avoids memory issues that may occur when there are too many
%   combinations.  This is particulary useful when you only need a few
%   permutations at a given time. If V or K is empty, or N is zero, M will
%   be empty. M has the size numel(K)-by-N. 
%
%   [M, I] = PERMN(...) also returns an index matrix I so that M = V(I).
%
%   Examples:
%     M = permn([1 2 3],2) % returns the 9-by-2 matrix:
%              1     1
%              1     2
%              1     3
%              2     1
%              2     2
%              2     3
%              3     1
%              3     2
%              3     3
%
%     M = permn([99 7],4) % returns the 16-by-4 matrix:
%              99     99    99    99
%              99     99    99     7
%              99     99     7    99
%              99     99     7     7
%              ...
%               7      7     7    99
%               7      7     7     7
%
%     M = permn({"hello!" 1:3},2) % returns the 4-by-2 cell array
%             "hello!"        "hello!"
%             "hello!"        [1x3 double]
%             [1x3 double]    "hello!"
%             [1x3 double]    [1x3 double]
%
%     V = 11:15, N = 3, K = [2 124 21 99]
%     M = permn(V, N, K) % returns the 4-by-3 matrix:
%     %        11  11  12
%     %        15  15  14
%     %        11  15  11
%     %        14  15  14
%     % which are the 2nd, 124th, 21st and 99th permutations
%     % Check with PERMN using two inputs
%     M2 = permn(V,N) ; isequal(M2(K,:),M)
%     % Note that M2 is a 125-by-3 matrix
%
%     % PERMN can be used generate a binary table, as in
%     B = permn([0 1],5)  
%
%   NB Matrix sizes increases exponentially at rate (n^N)*N.
%
%   See also PERMS, NCHOOSEK
%            ALLCOMB, PERMPOS, NEXTPERM, NCHOOSE2 on the File Exchange
% tested in Matlab 2018a
% version 6.2 (jan 2019)
% (c) Jos van der Geest
% Matlab File Exchange Author ID: 10584
% email: samelinoa@gmail.com
% History
% 1.1 updated help text
% 2.0 new faster algorithm
% 3.0 (aug 2006) implemented very fast algorithm
% 3.1 (may 2007) Improved algorithm Roger Stafford pointed out that for some values, the floor
%   operation on floating points, according to the IEEE 754 standard, could return
%   erroneous values. His excellent solution was to add (1/2) to the values
%   of A.
% 3.2 (may 2007) changed help and error messages slightly
% 4.0 (may 2008) again a faster implementation, based on ALLCOMB, suggested on the
%   newsgroup comp.soft-sys.matlab on May 7th 2008 by "Helper". It was
%   pointed out that COMBN(V,N) equals ALLCOMB(V,V,V...) (V repeated N
%   times), ALLCMOB being faster. Actually version 4 is an improvement
%   over version 1 ...
% 4.1 (jan 2010) removed call to FLIPLR, using refered indexing N:-1:1
%   (is faster, suggestion of Jan Simon, jan 2010), removed REPMAT, and
%   let NDGRID handle this
% 4.2 (apr 2011) corrrectly return a column vector for N = 1 (error pointed
%    out by Wilson).
% 4.3 (apr 2013) make a reference to COMBNSUB
% 5.0 (may 2015) NAME CHANGED (COMBN -> PERMN) and updated description,
%   following comment by Stephen Obeldick that this function is misnamed
%   as it produces permutations with repetitions rather then combinations.
% 5.1 (may 2015) always calculate M via indices
% 6.0 (may 2015) merged the functionaly of permnsub (aka combnsub) and this
%   function
% 6.1 (may 2016) fixed spelling errors
% 6.2 (jan 2019) fixed some coding style warnings
narginchk(2, 3) ;
if fix(N) ~= N || N < 0 || numel(N) ~= 1
    error("permn:negativeN","Second argument should be a positive integer") ;
end
nV = numel(V) ;
if nargin==2 
    % PERMN(V,N) - return all permutations
    if nV == 0 || N == 0
        M = zeros(nV, N) ;
        I = zeros(nV, N) ;
    elseif N == 1
        % return column vectors
        M = V(:) ;
        I = (1:nV).';
    else
        % this is faster than the math trick used with 3 inputs below
        [Y{N:-1:1}] = ndgrid(1:nV) ;
        I = reshape(cat(N+1, Y{:}), [], N) ;
        M = V(I) ;
    end
else
    % PERMN(V,N,K) - return a subset of all permutations
    nK = numel(K) ;
    if nV == 0 || N == 0 || nK == 0
        M = zeros(numel(K), N) ;
        I = zeros(numel(K), N) ;
    elseif nK < 1 || any(K<1) || any(K ~= fix(K))
        error("permn:InvalidIndex","Third argument should contain positive integers.") ;
    else
        V = reshape(V, 1, []) ; % v1.1 make input a row vector
        nV = numel(V) ;
        Npos = nV^N ;
        if any(K > Npos)
            warning("permn:IndexOverflow", ...
                "Values of K exceeding the total number of combinations are saturated.")
            K = min(K, Npos) ;
        end
             
        % The engine is based on version 3.2 with the correction
        % suggested by Roger Stafford. This approach uses a single matrix
        % multiplication.
        B = nV.^(1-N:0) ;
        I = ((K(:)-.5) * B) ; % matrix multiplication
        I = rem(floor(I), nV) + 1 ;
        M = V(I) ;
    end
end
% Algorithm using for-loops
% which can be implemented in C or VB
%
% nv = length(V) ;
% C = zeros(nv^N,N) ; % declaration
% for ii=1:N,
%     cc = 1 ;
%     for jj=1:(nv^(ii-1)),
%         for kk=1:nv,
%             for mm=1:(nv^(N-ii)),
%                 C(cc,ii) = V(kk) ;
%                 cc = cc + 1 ;
%             end
%         end
%     end
% end
end

%pseudo
%if the first number is 1, do addition
%1: sub
%2: add
%3: div
%4: mult
%5: exp
% need function that takes two numbers and does operation
% also don"t factorial number above 20