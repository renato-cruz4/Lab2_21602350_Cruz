tdaLibroCrear(ID, Titulo, Autor,L) :-
    string_lower(Titulo, TituloBn),
    string_lower(Autor, AutorBn),
    L= [idLibro(ID),
        tituloLibro(TituloBn),
        autorLibro(AutorBn),
        estadoLibro(disponible)       ].


estadoLPermitido(disponible).
estadoLPermitido(no_disponible).

tdaLibroEsLibro([idLibro(IDLibro),
                tituloLibro(Titulo),
                autorLibro(Autor),
                estadoLibro(Estado)]):-
    integer(IDLibro),
    atom(Titulo),
    atom(Autor),
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
    Libro= [_, _, _, estado(Estado)].



tdaLibroSetEstado(Libro, NuevoEstado, LibroNuevo):-
    estadoLPermitido(NuevoEstado),
    tdaLibroGetID(Libro,IDLibro),
    tdaLibroGetTitulo(Libro,Titulo),
    tdaLibroGetAutor(Libro,Autor),

    LibroNuevo=[idLibro(IDLibro),
               tituloLibro(Titulo),
               autorLibro(Autor),
               estado(NuevoEstado)].




















































































































