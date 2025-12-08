/* TDA Libro Crear
 Descripción: Constructor que crea un libro convirtiendo titulo y autor a minusculas para facilitar busquedas.
 Dom: ID(int) X TituloI(string) X AutorI(string)
 Rec: Libro(L)
*/
tdaLibroCrear(ID, TituloI, AutorI,L) :-
    string_lower(TituloI, Titulo),
    string_lower(AutorI, Autor),
    L= [idLibro(ID),
        tituloLibro(Titulo),
        autorLibro(Autor),
        estadoLibro(disponible)       ].

%auxiliares de estados permitidos de un libro
estadoLPermitido(disponible).
estadoLPermitido(no_disponible).


/* TDA Libro Es Libro
 Descripción: Predicado de pertenencia que valida si una estructura cumple con el formato de libro.
 Dom: Libro(L)
 Rec: boolean
*/
tdaLibroEsLibro(Libro):-
      Libro=   [idLibro(IDLibro),
                tituloLibro(Titulo),
                autorLibro(Autor),
                estadoLibro(Estado)],
    integer(IDLibro),
    string(Titulo),
    string(Autor),
    estadoLPermitido(Estado).


/* TDA Libro Get ID
 Descripción: Obtiene el identificador numerico del libro.
 Dom: Libro(L)
 Rec: IDLibro(int)
*/

tdaLibroGetID(Libro, IDLibro):-
    tdaLibroEsLibro(Libro),
    Libro= [idLibro(IDLibro)|_].



/* TDA Libro Get Titulo
 Descripción: Obtiene el titulo del libro.
 Dom: Libro(L)
 Rec: Titulo(string)
*/
tdaLibroGetTitulo(Libro, Titulo):-
    tdaLibroEsLibro(Libro),
    Libro=[_,tituloLibro(Titulo)|_].



/* TDA Libro Get Autor
 Descripción: Obtiene el autor del libro.
 Dom: Libro(L)
 Rec: Autor(string)
*/
tdaLibroGetAutor(Libro,Autor):-
    tdaLibroEsLibro(Libro),
    Libro=[_, _, autorLibro(Autor)|_].




/* TDA Libro Get Estado
 Descripción: Obtiene el estado actual del libro (disponible/no_disponible).
 Dom: Libro(L)
 Rec: Estado(atom)
*/
tdaLibroGetEstado(Libro,Estado):-
    tdaLibroEsLibro(Libro),
    Libro= [_, _, _, estadoLibro(Estado)].




% --- Modificadores ---



/* TDA Libro Set Estado
 Descripción: Genera un nuevo libro con el estado actualizado.
 Dom: Libro(L) X NuevoEstado(atom)
 Rec: LibroNuevo(L)
*/
tdaLibroSetEstado(Libro, NuevoEstado, LibroNuevo):-
    estadoLPermitido(NuevoEstado),
    tdaLibroGetID(Libro,IDLibro),
    tdaLibroGetTitulo(Libro,Titulo),
    tdaLibroGetAutor(Libro,Autor),

    LibroNuevo=[idLibro(IDLibro),
               tituloLibro(Titulo),
               autorLibro(Autor),
               estadoLibro(NuevoEstado)].


/* TDA Libro Buscar Libro
 Descripción: Busca un libro en una lista segun un criterio ("id", "titulo", "autor").
 Dom: ListaLibros(list) X Criterio(string) X ValorBusqueda(any)
 Rec: LibroEncontrado(L)
 Tipo de recursion: De cola, busca hasta encontrar o vaciar la lista.
*/
tdaLibroBuscarLibro([Libro|_], "id", ValorID, Libro) :-
    tdaLibroGetID(Libro, ID),
    ValorID =:= ID, !.

tdaLibroBuscarLibro([Libro|_], "titulo", ValorBusqueda, Libro) :-
    tdaLibroGetTitulo(Libro, TituloLibro),
    string_lower(ValorBusqueda, ValorLower),
    sub_string(TituloLibro, _, _, _, ValorLower), !.

tdaLibroBuscarLibro([Libro|_], "autor", ValorBusqueda, Libro) :-
    tdaLibroGetAutor(Libro, AutorLibro),
    string_lower(ValorBusqueda, ValorLower),
    sub_string(AutorLibro, _, _, _, ValorLower), !.

tdaLibroBuscarLibro([_|Resto], Criterio, Valor, LibroEncontrado) :-
    tdaLibroBuscarLibro(Resto, Criterio, Valor, LibroEncontrado).







/* TDA Libro Actualizar E (Elemento)
 Descripción: Recorre una lista de libros y actualiza el estado de uno especifico segun su ID
 Dom: ListaLibros(list) X ID(int) X NuevoEstado(atom)
 Rec: ListaModificada(list)
 Tipo de recursion: Natural (Construye la lista de salida al retornar de la recursión).
*/
tdaLibroActualizarE([], _, _, []).

tdaLibroActualizarE([Libro|Resto], ID, NuevoEstado, [LibroMod|Resto]) :-
    tdaLibroGetID(Libro, IDL),
    IDL =:= ID,
    !,
    tdaLibroSetEstado(Libro, NuevoEstado, LibroMod).

tdaLibroActualizarE([Libro|Resto], ID, NuevoEstado, [Libro|RestoMod]) :-
    tdaLibroActualizarE(Resto, ID, NuevoEstado, RestoMod).







































































































