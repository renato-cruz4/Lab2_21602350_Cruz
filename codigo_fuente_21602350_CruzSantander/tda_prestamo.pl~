:-consult('TDA_fecha.pl').


tdaPrestamoCrear(IDPrestamo, IDUsuario, IDLibro, FechaPrestamo, DiasSolicitados, P) :-
    P = [idPrestamo(IDPrestamo),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados)
        ].


tdaPrestamoEsPrestamo(Prestamo):-
    Prestamo=[idPrestamo(IDPrestamo),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados)
        ],
    integer(IDPrestamo),
    integer(IDUsuario),
    integer(IDLibro),
    tdaFechaEsFecha(FechaPrestamo),
    integer(DiasSolicitados).

tdaPrestamoGetID(Prestamo, IDPrestamo):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [idPrestamo(IDPrestamo)|_].

tdaPrestamoGetIDUsuario(Prestamo, IDUsuario):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo=[_,idUsuario(IDUsuario)|_].

tdaPrestamoGetIdLibro(Prestamo,IDLibro):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo=[_, _, idLibro(IDLibro)|_].

tdaPrestamoGetFecha(Prestamo,FechaPrestamo):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [_, _, _, fechaPrestamo(FechaPrestamo)].

tdaPrestamoGetDias(Prestamo,DiasSolicitados):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [_, _, _, _, diasSolicitados(DiasSolicitados)].



