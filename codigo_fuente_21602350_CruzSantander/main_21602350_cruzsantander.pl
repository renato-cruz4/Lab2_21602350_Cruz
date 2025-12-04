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
