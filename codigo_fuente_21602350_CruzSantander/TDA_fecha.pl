tdaFechaCrear(D,M, fecha(D,M)).

tdaFechaEsFecha(fecha(D,M)):-
    integer(D),
    integer(M),
    D >= 1, D =< 30,
    M >= 1, M =< 12.

tdaFechaGetDia(fecha(D,_),D).
tdaFechaGetMes(fecha(_,M),M).





