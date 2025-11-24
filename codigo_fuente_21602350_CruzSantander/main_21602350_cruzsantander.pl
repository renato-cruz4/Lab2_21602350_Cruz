crearUsuario(ID, Nombre, U) :-
    U = [ id(ID),
                nombre(Nombre),
                deuda(0),
                libros([]),
                estado(activo)
             ].

crearLibro(ID, Titulo, Autor,L ) :-
    string_lower(Titulo, TituloLower),
    string_lower(Autor, AutorLower),
    L = [ id(ID),
                titulo(TituloLower),
                autor(AutorLower),
                estado(disponible)
             ].


crearPrestamo(ID, IDUsuario, IDLibro, FechaPrestamo, DiasSolicitados, P) :-
    P = [id(ID),
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

