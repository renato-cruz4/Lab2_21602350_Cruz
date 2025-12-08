%funcion auxiliar que checkea si el numero es de dos digitos,
%añade un 0 al inicio si no.
check2D(S, S2) :-
    (   string_length(S, 1)
    ->  string_concat("0", S, S2)
    ;   S2 = S
    ).



/* TDA Fecha Crear
 Descripción: funcion que crea una fecha.
Dom: dia(int) X mes(int)
Rec: fecha(string)
*/
fecha(A, M,D ,Fecha) :-
    integer(D), integer(M), integer(A),
    D >= 1, D =< 30,
    M >= 1, M =< 12,

    % Convertir números a strings
    number_string(D, SD0),
    number_string(M, SM0),
    number_string(A, SA),

    % Asegurar que tengan dos dígitos
    check2D(SD0, SD),
    check2D(SM0, SM),
            % Construir DD/MM/AAAA
    string_concat(SD, "/", P1),
    string_concat(P1, SM, P2),
    string_concat(P2, "/", P3),
    string_concat(P3, SA, Fecha).





/* TDA Fecha Es Fecha
 Descripción: funcion que corrobora si un string es una fecha valida.
Dom: fecha(string)
Rec: boolean
*/

tdaFechaEsFecha(Fecha):-
    string(Fecha),
    split_string(Fecha,"/","",[SD,SM,SA]),
    number_string(M,SM),
    number_string(D,SD),
    number_string(A,SA),
    D >= 1, D =< 30,
    M >= 1, M =< 12,
    integer(A).

/* TDA Fecha Get Dia
 Descripción: funcion que obtiene el dia de una fecha.
Dom: fecha(string)
Rec: dia(int)
*/

tdaFechaGetDia(Fecha, D) :-
    tdaFechaEsFecha(Fecha),
    split_string(Fecha, "/", "", [SD, _,_]),
    number_string(D, SD).


/* TDA Fecha Get Mes
 Descripción: funcion que obtiene el mes de una fecha.
Dom: fecha(string)
Rec: mes(int)
*/

tdaFechaGetMes(Fecha, M) :-
    tdaFechaEsFecha(Fecha),
    split_string(Fecha, "/", "", [_, SM,_]),
    number_string(M, SM).

/* TDA Fecha Get Mes
 Descripción: funcion que obtiene el mes de una fecha.
Dom: fecha(string)
Rec: mes(int)
*/
tdaFechaGetAno(Fecha, A) :-
    tdaFechaEsFecha(Fecha),
    split_string(Fecha, "/", "", [_, _, SA]),
    number_string(A, SA).

/* TDA Fecha Pasar
 Descripción: funcion que aumenta un dia a una fecha, si el dia pasa de
 30 cambia de mes.
Dom: fecha(string)
Rec: fecha(string)
*/
tdaFechaPasar(Fecha, NuevaFecha):-
    tdaFechaGetDia(Fecha, D),
    tdaFechaGetMes(Fecha, M),
    tdaFechaGetAno(Fecha, A),

    ( D < 30 ->
        D2 is D + 1,
        M2 is M,
        A2 is A
    ;   % Si es fin de mes (30)
        D2 is 1,
        ( M < 12 ->
            M2 is M + 1,
            A2 is A
        ;   % Si es fin de año (Mes 12, Dia 30) -> 01/01/A+1
            M2 is 1,
            A2 is A + 1
        )
    ),
    % Llamamos al nuevo constructor fecha/4
    fecha(A2, M2, D2, NuevaFecha).

/* sumarDias
 Descripción: funcion que suma una cantidad de dias determinados a una
 fecha.
Dom: fecha(string) X dias sumados(int)
Rec: fecha(string)
*/

sumarDias(Fecha, 0, Fecha) :- !.

sumarDias(FechaIn, DiasRestantes, FechaOut) :-
    DiasRestantes > 0,
    tdaFechaPasar(FechaIn, FechaSiguiente),
    NuevosDias is DiasRestantes - 1,
    sumarDias(FechaSiguiente, NuevosDias, FechaOut).

%funcion auxiliar que cuenta los dias totales de una fecha

diasTotales(Fecha, TotalDias) :-
    tdaFechaGetDia(Fecha, D),
    tdaFechaGetMes(Fecha, M),
    tdaFechaGetAno(Fecha, A),
    TotalDias is (A * 360) + ((M - 1) * 30) + D.

