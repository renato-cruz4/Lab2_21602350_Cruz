:-consult('TDA_fecha.pl').

/* TDA Prestamo Crear
 Descripción: Constructor principal del TDA Prestamo.
 Dom: IDPrestamo(int) X IDUsuario(int) X IDLibro(int) X FechaPrestamo(string) X DiasSolicitados(int)
 Rec: Prestamo(P)
*/

tdaPrestamoCrear(IDPrestamo, IDUsuario, IDLibro, FechaPrestamo, DiasSolicitados, P) :-
    P = [idPrestamo(IDPrestamo),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados),
         estadoPrestamo(activo)
        ].

%auxiliares de estado
estadoPPermitido(activo).
estadoPPermitido(completado).


/* TDA Prestamo Es Prestamo
 Descripción: Verifica la estructura y tipos de datos de un prestamo.
 Dom: Prestamo(P)
 Rec: boolean
*/
tdaPrestamoEsPrestamo(Prestamo):-
    Prestamo=[idPrestamo(IDPrestamo),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados),
         estadoPrestamo(Estado)
        ],
    integer(IDPrestamo),
    integer(IDUsuario),
    integer(IDLibro),
    tdaFechaEsFecha(FechaPrestamo),
    integer(DiasSolicitados),
    estadoPPermitido(Estado).

%///////////////selectores///////////////


/* TDA Prestamo Get ID
 Descripción: Selector para obtener el ID unico del prestamo.
 Dom: Prestamo(P)
 Rec: IDPrestamo(int)
*/
tdaPrestamoGetID(Prestamo, IDPrestamo):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [idPrestamo(IDPrestamo)|_].


/* TDA Prestamo Get ID Usuario
 Descripción: Selector para obtener el ID del usuario asociado al prestamo.
 Dom: Prestamo(P)
 Rec: IDUsuario(int)
*/
tdaPrestamoGetIDUsuario(Prestamo, IDUsuario):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo=[_,idUsuario(IDUsuario)|_].


/* TDA Prestamo Get ID Libro
 Descripción: Selector para obtener el ID del libro prestado.
 Dom: Prestamo(P)
 Rec: IDLibro(int)
*/
tdaPrestamoGetIdLibro(Prestamo,IDLibro):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo=[_, _, idLibro(IDLibro)|_].


/* TDA Prestamo Get Fecha
 Descripción: Selector para obtener la fecha de inicio del prestamo.
 Dom: Prestamo(P)
 Rec: FechaPrestamo(string)
*/
tdaPrestamoGetFecha(Prestamo,FechaPrestamo):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [_, _, _, fechaPrestamo(FechaPrestamo)|_].


/* TDA Prestamo Get Dias
 Descripción: Selector para obtener la cantidad de dias solicitados.
 Dom: Prestamo(P)
 Rec: DiasSolicitados(int)
*/
tdaPrestamoGetDias(Prestamo,DiasSolicitados):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [_, _, _, _, diasSolicitados(DiasSolicitados)|_].


/* TDA Prestamo Get estado
 Descripción: Selector para obtener el estado del prestamo.
 Dom: Prestamo(P)
 Rec: estado(atom)
*/
tdaPrestamoGetEstado(Prestamo,EstadoPrestamo):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [_, _, _, _,_,estadoPrestamo(EstadoPrestamo)|_ ].




/* TDA Prestamo Buscar
 Descripción: Busca un prestamo especifico en la lista dado un usuario y un libro.
 Dom: ListaPrestamos(list) X IdUser(int) X IdLibro(int)
 Rec: PrestamoEncontrado(P)
 Tipo de recursion: De cola (Tail Recursion).
*/
tdaPrestamoBuscar([P|_], IdUser, IdLibro, P) :-
    tdaPrestamoGetIDUsuario(P, U),
    tdaPrestamoGetIdLibro(P, L),
    U =:= IdUser,
    L =:= IdLibro, !.
tdaPrestamoBuscar([_|Resto], IdUser, IdLibro, P) :-
    tdaPrestamoBuscar(Resto, IdUser, IdLibro, P).



% auxiliar busca un prestamo por su ID
% devuelve un prestamo
% usa recursion de cola
buscarPrestamoPorID([Prestamo|_], IdBusca, Prestamo) :-
    tdaPrestamoGetID(Prestamo, Id),
    Id =:= IdBusca, !.
buscarPrestamoPorID([_|Resto], IdBusca, P) :-
    buscarPrestamoPorID(Resto, IdBusca, P).


/* tdaPrestamoActualizar
 Descripción: Recorre la lista de prestamos y reemplaza el prestamo con el ID coincidente.
 Dom: Prestamos(list) X PrestamoNuevo(P)
 Rec: Prestamos(list)
 Tipo de recursion: Natural (Reconstruye la lista al volver de la recursion).
*/
tdaPrestamoActualizar([], _, []).
tdaPrestamoActualizar([Prestamo|Resto], PrestamoNuevo, [PrestamoNuevo|Resto]) :-
    tdaPrestamoGetID(Prestamo, IdOld),
    tdaPrestamoGetID(PrestamoNuevo, IdNew),
    IdOld =:= IdNew, !.
tdaPrestamoActualizar([Prestamo|Resto], PrestamoNuevo, [Prestamo|RestoMod]) :-
    tdaPrestamoActualizar(Resto, PrestamoNuevo, RestoMod).
