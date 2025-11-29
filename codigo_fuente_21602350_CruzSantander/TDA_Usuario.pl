tdaUsuarioCrear(ID, Nombre, U):-
    U= [idUsuario(ID),
        nombreUsuario(Nombre),
       deudaUsuario(0),
       librosUsuario([]),
       estadoUsuario(activo)].

estadoUPermitido(activo).
estadoUPermitido(suspendido).

tdaUsuarioEsUsuario([idUsuario(IDUsuario),
                     nombreUsuario(Nombre),
                     deudaUsuario(Deuda),
                     librosUsuario(LibrosU),
                     estadoUsuario(EstadoUsuario)]):-
    integer(IDUsuario),
    (atom(Nombre);  string(Nombre)),
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
