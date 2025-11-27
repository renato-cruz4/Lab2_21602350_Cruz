tdaPrestamoCrear(ID, IDUsuario, IDLibro, FechaPrestamo, DiasSolicitados, P) :-
    P = [id(ID),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados)
        ].


tdaPrestamoEsPrestamo([id(ID),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados)
        ]):-
    integer(ID),
    integer(IDUsuario),
    integer(IDLibro),
    string(FechaPrestamo),
    integer(DiasSolicitados).

tdaPrestamoGetID(Prestamo, ID):-
    tdaPrestamoEsPrestamo(Prestamo),
    Prestamo= [idPrestamo(ID)|_].

tdaLibroGetIDUsuario(Prestamo, IDUsuario):-
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



