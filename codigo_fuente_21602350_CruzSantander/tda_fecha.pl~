check(S, S2) :-
    (   string_length(S, 1)
    ->  string_concat("0", S, S2)
    ;   S2 = S
    ).

tdaFechaCrear(D, M, Fecha) :-
    integer(D), integer(M),
    D >= 1, D =< 30,
    M >= 1, M =< 12,

    % Convertir números a strings
    number_string(D, SD0),
    number_string(M, SM0),

    % Asegurar que tengan dos dígitos
    check(SD0, SD),
    check(SM0, SM),

    % Construir DD/MM
    string_concat(SD, "/", T),
    string_concat(T, SM, Fecha).


tdaFechaEsFecha(Fecha):-
    string(Fecha),
    split_string(Fecha,"/","",[SD,SM]),
    number_string(M,SM),
    number_string(D,SD),
    D >= 1, D =< 30,
    M >= 1, M =< 12.

tdaFechaGetDia(Fecha, D) :-
    tdaFechaEsFecha(Fecha),
    split_string(Fecha, "/", "", [SD, _]),
    number_string(D, SD).

tdaFechaGetMes(Fecha, M) :-
    tdaFechaEsFecha(Fecha),
    split_string(Fecha, "/", "", [_, SM]),
    number_string(M, SM).

tdaFechaPasar(Fecha,NuevaFecha):-
    tdaFechaGetDia(Fecha,Dia),
    tdaFechaGetMes(Fecha,Mes),
    (   Dia < 30 ->
    Dia2 is Dia + 1,
        Mes2 is Mes
    ;   Dia2 is 1,
        (Mes< 12 -> Mes2 is Mes+1 ; Mes2 is 1 )
    ),
    tdaFechaCrear(Dia2, Mes2, NuevaFecha).



