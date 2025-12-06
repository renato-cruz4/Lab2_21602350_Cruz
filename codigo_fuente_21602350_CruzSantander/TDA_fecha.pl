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

tdaFechaCrear(D, M, Fecha) :-
    integer(D), integer(M),
    D >= 1, D =< 30,
    M >= 1, M =< 12,

    % Convertir números a strings
    number_string(D, SD0),
    number_string(M, SM0),

    % Asegurar que tengan dos dígitos
    check2D(SD0, SD),
    check2D(SM0, SM),

    % Construir DD/MM
    string_concat(SD, "/", T),
    string_concat(T, SM, Fecha).




/* TDA Fecha Es Fecha
 Descripción: funcion que corrobora si un string es una fecha valida.
Dom: fecha(string)
Rec: boolean
*/

tdaFechaEsFecha(Fecha):-
    string(Fecha),
    split_string(Fecha,"/","",[SD,SM]),
    number_string(M,SM),
    number_string(D,SD),
    D >= 1, D =< 30,
    M >= 1, M =< 12.



/* TDA Fecha Get Dia
 Descripción: funcion que obtiene el dia de una fecha.
Dom: fecha(string)
Rec: dia(int)
*/

tdaFechaGetDia(Fecha, D) :-
    tdaFechaEsFecha(Fecha),
    split_string(Fecha, "/", "", [SD, _]),
    number_string(D, SD).


/* TDA Fecha Get Mes
 Descripción: funcion que obtiene el mes de una fecha.
Dom: fecha(string)
Rec: mes(int)
*/

tdaFechaGetMes(Fecha, M) :-
    tdaFechaEsFecha(Fecha),
    split_string(Fecha, "/", "", [_, SM]),
    number_string(M, SM).



/* TDA Fecha Pasar
 Descripción: funcion que aumenta un dia a una fecha, si el dia pasa de
 30 cambia de mes.
Dom: fecha(string)
Rec: fecha(string)
*/
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
    tdaFechaGetDia(Fecha, Dia),
    tdaFechaGetMes(Fecha, Mes),
    TotalDias is ((Mes - 1) * 30) + Dia.
