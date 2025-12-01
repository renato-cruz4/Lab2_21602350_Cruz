:- consult('TDA_fecha.pl').
:- consult('TDA_usuario.pl').
:- consult('TDA_libro.pl').
:- consult('TDA_prestamo.pl').
:- consult('TDA_biblioteca.pl').


crearUsuario(IDUsuario, Nombre, U) :-
    U = [ id(IDUsuario),
                nombre(Nombre),
                deuda(0),
                libros([]),
                estado(activo)
             ].

crearLibro(IDLibro, Titulo, Autor,L ) :-
    string_lower(Titulo, TituloLower),
    string_lower(Autor, AutorLower),
    L = [ id(IDLibro),
                titulo(TituloLower),
                autor(AutorLower),
                estado(disponible)
             ].


crearPrestamo(IDPrestamo, IDUsuario, IDLibro, FechaPrestamo, DiasSolicitados, P):-
    P = [idPrestamo(IDPrestamo),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados)
        ].

crearBiblioteca(Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, FechaInicial, B) :-
    B= [libros(Libros),
        usuarios(Usuarios),
        prestamos(Prestamos),
        maxLibros(MaxLibros),
        diasMax(DiasMax),
        tasaMulta(TasaMulta),
        limiteDeuda(LimiteDeuda),
        diasRetraso(DiasRetraso),
        fechaInicial(FechaInicial)
       ].


agregarLibro(BibliotecaIn, Libro1, BibliotecaOut) :-

    % Extraer ID del nuevo libro
    tdaLibroGetID(Libro1, IDNuevo),

    % Extraer lista actual de libros
    tdaBibliotecaGetLibros(BibliotecaIn, LibrosActuales),

    % Revisar si ya existe libro con ese ID
    ( checkIDLibro(IDNuevo, LibrosActuales) ->
        BibliotecaOut = BibliotecaIn         % No agrega
    ;
        append(LibrosActuales, [Libro1], LibrosNuevos),
        tdaBibliotecaSetLibros(BibliotecaIn, LibrosNuevos, BibliotecaOut)
    ).


