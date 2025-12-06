/* TDA Usuario Crear
 Descripción: Constructor que inicializa un usuario con estado activo y deuda 0.
 Dom: ID(int) X Nombre(string)
 Rec: Usuario(U)
*/
tdaUsuarioCrear(ID, Nombre, U):-
    U= [idUsuario(ID),
        nombreUsuario(Nombre),
       deudaUsuario(0),
       librosUsuario([]),
       estadoUsuario(activo)].


%estados permitidos de usuarios
estadoUPermitido(activo).
estadoUPermitido(suspendido).


/* TDA Usuario Es Usuario
 Descripción: Predicado de validacion de tipos para el TDA Usuario.
 Dom: Usuario(U)
 Rec: boolean
*/
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



%/////////selectores//////////


/* TDA Usuario Get ID
 Descripción: Obtiene el ID del usuario.
 Dom: Usuario(U)
 Rec: IDUsuario(int)
*/
tdaUsuarioGetID(Usuario,IDUsuario):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [idUsuario(IDUsuario)|_].


/* TDA Usuario Get Nombre
 Descripción: Obtiene el nombre del usuario.
 Dom: Usuario(U)
 Rec: Nombre(string)
*/
tdaUsuarioGetNombre(Usuario,Nombre):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_, nombreUsuario(Nombre)|_].


/* TDA Usuario Get Deuda
 Descripción: Obtiene la deuda actual del usuario.
 Dom: Usuario(U)
 Rec: Deuda(int)
*/
tdaUsuarioGetDeuda(Usuario,Deuda):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_,_,deudaUsuario(Deuda)|_].


/* TDA Usuario Get Libros
 Descripción: Obtiene la lista de IDs de libros que el usuario tiene en su poder.
 Dom: Usuario(U)
 Rec: LibrosU(list)
*/
tdaUsuarioGetLibros(Usuario,LibrosU):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_,_,_,librosUsuario(LibrosU)|_].


/* TDA Usuario Get Estado
 Descripción: Obtiene el estado del usuario (activo/suspendido).
 Dom: Usuario(U)
 Rec: Estado(atom)
*/
tdaUsuarioGetEstado(Usuario,Estado):-
    tdaUsuarioEsUsuario(Usuario),
    Usuario= [_,_,_,_,estadoUsuario(Estado)|_].


%/////////// modificadores//////////////




/* TDA Usuario Set Deuda
 Descripción: Actualiza la deuda del usuario retornando un nuevo usuario.
 Dom: Usuario(U) X NuevaDeuda(int)
 Rec: UsuarioNuevo(U)
*/
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


/* TDA Usuario Set Estado
 Descripción: Modifica el estado del usuario (ej: de activo a suspendido).
 Dom: Usuario(U) X NuevoEstado(atom)
 Rec: UsuarioNuevo(U)
*/
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



%///////////Otros/////////


/* TDA Usuario Buscar Usuario
 Descripción: Busca un usuario en una lista por su ID.
 Dom: IDEsperado(int) X ListaUsuarios(list)
 Rec: UsuarioEncontrado(U)
 Tipo de recursion: De cola
*/
tdaUsuarioBuscarUsuario(IDEsperado, [Usuario|_],Usuario):-
    tdaUsuarioGetID(Usuario,IDU),
    IDEsperado =:= IDU,!.

tdaUsuarioBuscarUsuario(IDBusca,[_|Resto],UsuarioEncontrado):-
    tdaUsuarioBuscarUsuario(IDBusca,Resto,UsuarioEncontrado).



/* TDA Usuario Agregar Libro
 Descripción: Agrega un ID de libro a la lista de libros del usuario.
 Dom: UsuarioIn(U) X IdLibro(int)
 Rec: UsuarioOut(U)
*/
tdaUsuarioAgregarLibro(UsuarioIn, IdLibro, UsuarioOut) :-
    tdaUsuarioGetID(UsuarioIn, ID),
    tdaUsuarioGetNombre(UsuarioIn, Nom),
    tdaUsuarioGetDeuda(UsuarioIn, Deuda),
    tdaUsuarioGetLibros(UsuarioIn, LibrosActuales),
    tdaUsuarioGetEstado(UsuarioIn, Est),

    append(LibrosActuales, [IdLibro], LibrosNuevos),

    UsuarioOut = [idUsuario(ID), nombreUsuario(Nom), deudaUsuario(Deuda), librosUsuario(LibrosNuevos), estadoUsuario(Est)].



/* TDA Usuario Actualizar L (Lista)
 Descripción: Actualiza la informacion de un usuario dentro de la lista general de usuarios.
 Dom: ListaUsuarios(list) X UsuarioNuevo(U)
 Rec: ListaActualizada(list)
 Tipo de recursion: Natural (Reconstruye la lista al volver de la recursión)
*/
tdaUsuarioActualizarL([], _, []).
tdaUsuarioActualizarL([Usuario|Resto], UsuarioNuevo, [UsuarioNuevo|Resto]) :-
    tdaUsuarioGetID(Usuario, IDViejo),
    tdaUsuarioGetID(UsuarioNuevo, IDNuevo),
    IDViejo =:= IDNuevo, !.
tdaUsuarioActualizarL([Usuario|Resto], UsuarioNuevo, [Usuario|RestoMod]) :-
    tdaUsuarioActualizarL(Resto, UsuarioNuevo, RestoMod).




/* Eliminar Elemento (Auxiliar)
 Descripción: Predicado generico para eliminar la primera ocurrencia de
 un elemento en una lista.
Dom: Lista(list) X Elemento(any)
Rec:ListaSinElemento(list)
Tipo de recursion: Natural.
*/
eliminarElemento([], _, []).
eliminarElemento([X|Resto], X, Resto) :- !.
eliminarElemento([Y|Resto], X, [Y|RestoMod]) :-
    eliminarElemento(Resto, X, RestoMod).




/* TDA Usuario Eliminar Libro
 Descripción: Elimina un libro de un usuario.
 Dom: UsuarioIn(U) X IdLibro(int)
 Rec: UsuarioOut(U)
*/
tdaUsuarioEliminarLibro(UsuarioIn, IdLibro, UsuarioOut) :-
    tdaUsuarioGetLibros(UsuarioIn, LibrosActuales),
    eliminarElemento(LibrosActuales, IdLibro, LibrosNuevos),


    tdaUsuarioGetID(UsuarioIn, ID),
    tdaUsuarioGetNombre(UsuarioIn, Nom),
    tdaUsuarioGetDeuda(UsuarioIn, Deuda),
    tdaUsuarioGetEstado(UsuarioIn, Est),

    UsuarioOut = [idUsuario(ID), nombreUsuario(Nom), deudaUsuario(Deuda), librosUsuario(LibrosNuevos), estadoUsuario(Est)].



/* Tiene Prestamo Atrasado
 Descripción: Verifica si un usuario tiene algun prestamo que exceda los dias permitidos de retraso.
 Dom: ListaPrestamos(list) X IdUsuario(int) X FechaActual(string) X MaxDias(int)
 Rec: boolean
 Tipo de recursion: De cola
*/
tienePrestamoAtrasado([Prestamo|_], IdUsuario, FechaActual, MaxDias) :-
    tdaPrestamoGetIDUsuario(Prestamo, IdU),
    IdU =:= IdUsuario,
    obtenerFechaVencimiento(Prestamo, FechaVenc),
    calcularDiasRetraso(FechaVenc, FechaActual, DiasRetraso),
    DiasRetraso > MaxDias.


tienePrestamoAtrasado([_|Resto], IdUsuario, FechaActual, MaxDias) :-
    tienePrestamoAtrasado(Resto, IdUsuario, FechaActual, MaxDias).
