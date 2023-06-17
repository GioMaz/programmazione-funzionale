% ricorsione
fact(0, 1).
fact(X, N) :- X > 0,
              X1 is X-1,
              fact(X1, N1),
              N is X*N1.

% grafi
edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).
edge(b, e).
edge(d, f).

path(X, X).
path(X, Y) :- edge(X, Z), path(Z, Y).

% cut
member(X, [X|_]) :- !.
member(X, [H|T]) :- member(X, T).
