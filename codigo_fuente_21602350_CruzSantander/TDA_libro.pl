tdaLibroCrear(ID, TituloI, AutorI,L) :-
    string_lower(TituloI, Titulo),
    string_lower(AutorI, Autor),
    L= [idLibro(ID),
        tituloLibro(Titulo),
        autorLibro(Autor),
        estadoLibro(disponible)       ].


estadoLPermitido(disponible).
estadoLPermitido(no_disponible).

tdaLibroEsLibro(Libro):-
      Libro=   [idLibro(IDLibro),
                tituloLibro(Titulo),
                autorLibro(Autor),
                estadoLibro(Estado)],
    integer(IDLibro),
    string(Titulo),
    string(Autor),
    estadoLPermitido(Estado).

tdaLibroGetID(Libro, IDLibro):-
    tdaLibroEsLibro(Libro),
    Libro= [idLibro(IDLibro)|_].

tdaLibroGetTitulo(Libro, Titulo):-
    tdaLibroEsLibro(Libro),
    Libro=[_,tituloLibro(Titulo)|_].

tdaLibroGetAutor(Libro,Autor):-
    tdaLibroEsLibro(Libro),
    Libro=[_, _, autorLibro(Autor)|_].

tdaLibroGetEstado(Libro,Estado):-
    tdaLibroEsLibro(Libro),
    Libro= [_, _, _, estadoLibro(Estado)].



tdaLibroSetEstado(Libro, NuevoEstado, LibroNuevo):-
    estadoLPermitido(NuevoEstado),
    tdaLibroGetID(Libro,IDLibro),
    tdaLibroGetTitulo(Libro,Titulo),
    tdaLibroGetAutor(Libro,Autor),

    LibroNuevo=[idLibro(IDLibro),
               tituloLibro(Titulo),
               autorLibro(Autor),
               estadoLibro(NuevoEstado)].




















































































































