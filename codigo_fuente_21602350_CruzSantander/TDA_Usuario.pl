tdaUsuarioCrear(ID, Nombre, U):-
    U= [idUsuario(ID),
        nombreUsuario(Nombre),
       deudaUsuario(0),
       librosUsuario([]),
       estadoUsuario(activo)].

estadoUPermitido(activo).
estadoUPermitido(suspendido).

tdaUsuarioEsUsuario(Usuario):-

    Usuario=        [idUsuario(IDUsuario),
                     nombreUsuario(Nombre),
                     deudaUsuario(Deuda),
                     librosUsuario(LibrosU),
                     estadoUsuario(EstadoUsuario)],
    integer(IDUsuario),
    string(Nombre),
    integer(Deuda),
    is_list(LibrosU),
    estadoUPermitido(EstadoUsuario).

tdaUsuarioGetID(Usuario,IDUsuario):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [idUsuario(IDUsuario)|_].

tdaUsuarioGetNombre(Usuario,Nombre):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_, nombreUsuario(Nombre)|_].

tdaUsuarioGetDeuda(Usuario,Deuda):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_,_,deudaUsuario(Deuda)|_].

tdaUsuarioGetLibros(Usuario,LibrosU):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_,_,_,librosUsuario(LibrosU)|_].

tdaUsuarioGetEstado(Usuario,Estado):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_,_,_,_,estadoUsuario(Estado)|_].

tdaUsuarioSetDeuda(Usuario, NuevaDeuda, UsuarioNuevo):-
    integer(NuevaDeuda),
    NuevaDeuda>=0,
    tdaUsuarioGetID(Usuario,ID),
    tdaUsuarioGetNombre(Usuario,Nombre),
    tdaUsuarioGetLibros(Usuario,LibrosU),
    tdaUsuarioGetEstado(Usuario,Estado),

    UsuarioNuevo=[idUsuario(ID),
               nombreUsuario(Nombre),
               deudaUsuario(NuevaDeuda),
               librosUsuario(LibrosU),
                  estadoUsuario(Estado)
                 ].

tdaUsuarioSetEstado(Usuario, NuevoEstado, UsuarioNuevo):-
    estadoUPermitido(NuevoEstado),
    tdaUsuarioGetID(Usuario,ID),
    tdaUsuarioGetNombre(Usuario,Nombre),
    tdaUsuarioGetDeuda(Usuario,Deuda),
    tdaUsuarioGetLibros(Usuario,LibrosU),

    UsuarioNuevo=[idUsuario(ID),
               nombreUsuario(Nombre),
               deudaUsuario(Deuda),
               librosUsuario(LibrosU),
               estadoUsuario(NuevoEstado)
                 ].


tdaUsuarioBuscarUsuario(IDEsperado, [Usuario|_],Usuario):-
    tdaUsuarioGetID(Usuario,IDU),
    IDEsperado =:= IDU,!.

tdaUsuarioBuscarUsuario(IDBusca,[_|Resto],UsuarioEncontrado):-
    tdaUsuarioBuscarUsuario(IDBusca,Resto,UsuarioEncontrado).




tdaUsuarioAgregarLibro(UsuarioIn, IdLibro, UsuarioOut) :-
    tdaUsuarioGetID(UsuarioIn, ID),
    tdaUsuarioGetNombre(UsuarioIn, Nom),
    tdaUsuarioGetDeuda(UsuarioIn, Deuda),
    tdaUsuarioGetLibros(UsuarioIn, LibrosActuales),
    tdaUsuarioGetEstado(UsuarioIn, Est),

    append(LibrosActuales, [IdLibro], LibrosNuevos),

    UsuarioOut = [idUsuario(ID), nombreUsuario(Nom), deudaUsuario(Deuda), librosUsuario(LibrosNuevos), estadoUsuario(Est)].


tdaUsuarioActualizarL([], _, []).
tdaUsuarioActualizarL([Usuario|Resto], UsuarioNuevo, [UsuarioNuevo|Resto]) :-
    tdaUsuarioGetID(Usuario, IDViejo),
    tdaUsuarioGetID(UsuarioNuevo, IDNuevo),
    IDViejo =:= IDNuevo, !.
tdaUsuarioActualizarL([Usuario|Resto], UsuarioNuevo, [Usuario|RestoMod]) :-
    tdaUsuarioActualizarL(Resto, UsuarioNuevo, RestoMod).



eliminarElemento([], _, []).
eliminarElemento([X|Resto], X, Resto) :- !.
eliminarElemento([Y|Resto], X, [Y|RestoMod]) :-
    eliminarElemento(Resto, X, RestoMod).




tdaUsuarioEliminarLibro(UsuarioIn, IdLibro, UsuarioOut) :-
    tdaUsuarioGetLibros(UsuarioIn, LibrosActuales),
    eliminarElemento(LibrosActuales, IdLibro, LibrosNuevos),


    tdaUsuarioGetID(UsuarioIn, ID),
    tdaUsuarioGetNombre(UsuarioIn, Nom),
    tdaUsuarioGetDeuda(UsuarioIn, Deuda),
    tdaUsuarioGetEstado(UsuarioIn, Est),

    UsuarioOut = [idUsuario(ID), nombreUsuario(Nom), deudaUsuario(Deuda), librosUsuario(LibrosNuevos), estadoUsuario(Est)].




tienePrestamoAtrasado([Prestamo|_], IdUsuario, FechaActual, MaxDias) :-
    tdaPrestamoGetIDUsuario(Prestamo, IdU),
    IdU =:= IdUsuario,
    obtenerFechaVencimiento(Prestamo, FechaVenc),
    calcularDiasRetraso(FechaVenc, FechaActual, DiasRetraso),
    DiasRetraso > MaxDias.


tienePrestamoAtrasado([_|Resto], IdUsuario, FechaActual, MaxDias) :-
    tienePrestamoAtrasado(Resto, IdUsuario, FechaActual, MaxDias).
