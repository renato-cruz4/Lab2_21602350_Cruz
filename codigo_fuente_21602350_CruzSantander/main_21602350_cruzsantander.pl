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
    % 1. Obtener la lista de libros
    tdaBibliotecaGetLibros(BibliotecaIn, ListaLibros),
    
    % 2. Llamar al buscador recursivo
    buscarLibroRec(ListaLibros, Criterio, Valor, LibroOut).




buscarLibroRec([Libro|_], "id", ValorID, Libro) :-
    tdaLibroGetID(Libro, ID),
    ValorID =:= ID, !. % ! Detiene la búsqueda al encontrar el primero


buscarLibroRec([Libro|_], "titulo", ValorBusqueda, Libro) :-
    tdaLibroGetTitulo(Libro, TituloLibro), % El título ya está en minúsculas en el TDA
    string_lower(ValorBusqueda, ValorLower), % Convertimos la búsqueda a minúsculas
    sub_string(TituloLibro, _, _, _, ValorLower), !. % Verificamos si Valor está DENTRO de Titulo


buscarLibroRec([Libro|_], "autor", ValorBusqueda, Libro) :-
    tdaLibroGetAutor(Libro, AutorLibro),   % El autor ya está en minúsculas en el TDA
    string_lower(ValorBusqueda, ValorLower),
    sub_string(AutorLibro, _, _, _, ValorLower), !.


buscarLibroRec([_|Resto], Criterio, Valor, LibroEncontrado) :-
    buscarLibroRec(Resto, Criterio, Valor, LibroEncontrado).