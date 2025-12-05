:- consult('TDA_fecha.pl').
:- consult('TDA_usuario.pl').
:- consult('TDA_libro.pl').
:- consult('TDA_prestamo.pl').
:- consult('TDA_biblioteca.pl').


crearUsuario(IDUsuario, Nombre, U) :-
    U = [ idUsuario(IDUsuario),
                nombreUsuario(Nombre),
                deudaUsuario(0),
                librosUsuario([]),
                estadoUsuario(activo)
             ].


crearLibro(IDLibro, Titulo, Autor,L ) :-
    string_lower(Titulo, TituloLower),
    string_lower(Autor, AutorLower),
    L = [ idLibro(IDLibro),
                tituloLibro(TituloLower),
                autorLibro(AutorLower),
                estadoLibro(disponible)
             ].


crearPrestamo(IDPrestamo, IDUsuario, IDLibro, FechaPrestamo, DiasSolicitados, P):-
    P = [idPrestamo(IDPrestamo),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados)
        ].


crearBiblioteca(Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, FechaBiblioteca, B) :-
    B= [libros(Libros),
        usuarios(Usuarios),
        prestamos(Prestamos),
        maxLibros(MaxLibros),
        diasMax(DiasMax),
        tasaMulta(TasaMulta),
        limiteDeuda(LimiteDeuda),
        diasRetraso(DiasRetraso),
        fechaBiblioteca(FechaBiblioteca)
       ].


agregarLibro(BibliotecaIn, Libro1, BibliotecaOut) :-

    tdaLibroGetID(Libro1, IDNuevo),

    tdaBibliotecaGetLibros(BibliotecaIn, LibrosActuales),

    ( checkIDLibro(IDNuevo, LibrosActuales) ->
        BibliotecaOut = BibliotecaIn
    ;
        append(LibrosActuales, [Libro1], LibrosNuevos),
        tdaBibliotecaSetLibros(BibliotecaIn, LibrosNuevos, BibliotecaOut)
    ).


registrarUsuario(BibliotecaIn, Usuario, BibliotecaOut):-

    tdaUsuarioGetID(Usuario,IDNuevo),
    tdaBibliotecaGetUsuarios(BibliotecaIn,UsuariosActuales),

    (checkIDUsuario(IDNuevo,UsuariosActuales)->
    BibliotecaOut=BibliotecaIn
    ;
    append(UsuariosActuales,[Usuario],UsuariosNuevos),
    tdaBibliotecaSetUsuarios(BibliotecaIn,UsuariosNuevos,BibliotecaOut)
    ).


obtenerUsuario(BibliotecaIn,IDBusca,UsuarioEncontrado):-

    tdaBibliotecaGetUsuarios(BibliotecaIn,ListaUsuarios),

    tdaUsuarioBuscarUsuario(IDBusca,ListaUsuarios,UsuarioEncontrado).


buscarLibro(BibliotecaIn, Criterio, Valor, LibroOut) :-

    tdaBibliotecaGetLibros(BibliotecaIn, ListaLibros),

    tdaLibroBuscarLibro(ListaLibros, Criterio, Valor, LibroOut).


getFecha(Biblioteca, Fecha) :-
    tdaBibliotecaGetFecha(Biblioteca, Fecha).


isLibroDisponible(BibliotecaIn,IDLibro):-

    tdaBibliotecaGetLibros(BibliotecaIn,ListaLibros),
    tdaBibliotecaCheckLibro(IDLibro,ListaLibros).


isUsuarioSuspendido(Usuario):-
    tdaUsuarioGetEstado(Usuario,Estado),
    Estado== suspendido.


obtenerDeuda(Usuario, Deuda):-
    tdaUsuarioGetDeuda(Usuario, Deuda).


obtenerFechaVencimiento(Prestamo, FechaVencimiento) :-
    tdaPrestamoGetFecha(Prestamo, FechaInicio),
    tdaPrestamoGetDias(Prestamo, DiasSolicitados),
    sumarDias(FechaInicio, DiasSolicitados, FechaVencimiento).


calcularDiasRetraso(FechaVenc, FechaActual, DiasRetraso) :-

    diasTotales(FechaVenc, TotalVenc),
    diasTotales(FechaActual, TotalActual),

    Diferencia is TotalActual - TotalVenc,

    ( Diferencia > 0 ->
        DiasRetraso is Diferencia
    ;
        DiasRetraso is 0
    ).


calcularMulta(PrestamoIn, FechaActual, Tasa, Multa) :-

    obtenerFechaVencimiento(PrestamoIn, FechaVencimiento),

    calcularDiasRetraso(FechaVencimiento, FechaActual, DiasRetraso),

    Multa is DiasRetraso * Tasa.



tomarPrestamo(BibIn, IdUsuario, IdLibro, Dias, FechaActual, BibOut) :-

    tdaBibliotecaGetMaxLibros(BibIn, MaxLibros),
    tdaBibliotecaGetDiasMax(BibIn, DiasMax),
    tdaBibliotecaGetLimiteDeuda(BibIn, LimiteDeuda),

    Dias =< DiasMax,

    isLibroDisponible(BibIn, IdLibro),

    obtenerUsuario(BibIn, IdUsuario, Usuario),

    \+ isUsuarioSuspendido(Usuario),

    obtenerDeuda(Usuario, Deuda),
    Deuda =< LimiteDeuda,

    tdaUsuarioGetLibros(Usuario, LibrosUser),
    length(LibrosUser, CantidadActual),
    CantidadActual < MaxLibros,



    tdaBibliotecaGetPrestamos(BibIn, Prestamos),
    length(Prestamos, CantidadP),
    IDNuevoPrestamo is CantidadP + 1,
    crearPrestamo(IDNuevoPrestamo, IdUsuario, IdLibro, FechaActual, Dias, NuevoPrestamo),
    append(Prestamos, [NuevoPrestamo], PrestamosActualizados),

    tdaBibliotecaGetLibros(BibIn, Libros),
    tdaLibroActualizarE(Libros, IdLibro, no_disponible, LibrosActualizados),

    tdaUsuarioAgregarLibro(Usuario, IdLibro, UsuarioActualizado),

    tdaBibliotecaGetUsuarios(BibIn, Usuarios),
    tdaUsuarioActualizarL(Usuarios, UsuarioActualizado, UsuariosActualizados),


    tdaBibliotecaSetLibros(BibIn, LibrosActualizados, B_Tmp1),
    tdaBibliotecaSetUsuarios(B_Tmp1, UsuariosActualizados, B_Tmp2),
    tdaBibliotecaSetPrestamos(B_Tmp2, PrestamosActualizados, BibOut).




devolverLibro(BibIn, IdUser, IdLibro, FechaActual, BibOut) :-
    tdaBibliotecaGetPrestamos(BibIn, Prestamos),
    tdaBibliotecaGetUsuarios(BibIn, Usuarios),
    tdaBibliotecaGetLibros(BibIn, Libros),
    tdaBibliotecaGetTasaMulta(BibIn, Tasa),
    tdaBibliotecaGetLimiteDeuda(BibIn, LimiteDeuda),

    tdaPrestamoBuscar(Prestamos, IdUser, IdLibro, Prestamo),

    calcularMulta(Prestamo, FechaActual, Tasa, Multa),

    obtenerUsuario(BibIn, IdUser, Usuario),
    obtenerDeuda(Usuario, DeudaActual),
    NuevaDeuda is DeudaActual + Multa,


    (NuevaDeuda > LimiteDeuda ->
        NuevoEstado = suspendido
    ;
        tdaUsuarioGetEstado(Usuario, NuevoEstado)
    ),

    tdaUsuarioSetDeuda(Usuario, NuevaDeuda, U_Tmp1),
    tdaUsuarioSetEstado(U_Tmp1, NuevoEstado, U_Tmp2),
    tdaUsuarioEliminarLibro(U_Tmp2, IdLibro, UsuarioActualizado),

    tdaUsuarioActualizarL(Usuarios, UsuarioActualizado, UsuariosActualizados),

    tdaLibroActualizarE(Libros, IdLibro, disponible, LibrosActualizados),

    eliminarElemento(Prestamos, Prestamo, PrestamosActualizados),


    tdaBibliotecaSetLibros(BibIn, LibrosActualizados, B_Tmp1),
    tdaBibliotecaSetUsuarios(B_Tmp1, UsuariosActualizados, B_Tmp2),
    tdaBibliotecaSetPrestamos(B_Tmp2, PrestamosActualizados, BibOut).



debeSuspenderse(Biblioteca, IdUsuario, FechaActual) :-

    tdaBibliotecaGetLimiteDeuda(Biblioteca, LimiteDeuda),
    tdaBibliotecaGetDiasRetraso(Biblioteca, MaxDiasRetrasoPermitidos),


    obtenerUsuario(Biblioteca, IdUsuario, Usuario),

    (
        obtenerDeuda(Usuario, Deuda),
        Deuda > LimiteDeuda
    ;
        tdaBibliotecaGetPrestamos(Biblioteca, Prestamos),
        tienePrestamoAtrasado(Prestamos, IdUsuario, FechaActual, MaxDiasRetrasoPermitidos)
    ).


suspenderUsuario(BibliotecaIn, IdUsuario, BibliotecaOut) :-

    tdaBibliotecaGetUsuarios(BibliotecaIn, UsuariosActuales),

    tdaBibliotecaSusU(UsuariosActuales, IdUsuario, UsuariosNuevos),

    tdaBibliotecaSetUsuarios(BibliotecaIn, UsuariosNuevos, BibliotecaOut).
