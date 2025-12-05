:- consult('TDA_fecha.pl').
:- consult('TDA_libro.pl').
:- consult('TDA_usuario.pl').
:- consult('TDA_prestamo.pl').



tdaBibliotecaCrear(Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, FechaBiblioteca, B) :-
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

tdaBibliotecaEsBiblioteca([libros(Libros),
        usuarios(Usuarios),
        prestamos(Prestamos),
        maxLibros(MaxLibros),
        diasMax(DiasMax),
        tasaMulta(TasaMulta),
        limiteDeuda(LimiteDeuda),
        diasRetraso(DiasRetraso),
        fechaBiblioteca(FechaBiblioteca)
       ]):-
       is_list(Libros),
       is_list(Usuarios),
       is_list(Prestamos),
       integer(MaxLibros),
       integer(DiasMax),
       integer(TasaMulta),
       integer(LimiteDeuda),
       integer(DiasRetraso),
       tdaFechaEsFecha(FechaBiblioteca).

tdaBibliotecaGetLibros(Biblioteca,Libros):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [libros(Libros)|_].


tdaBibliotecaGetUsuarios(Biblioteca,Usuarios):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,usuarios(Usuarios)|_].


tdaBibliotecaGetPrestamos(Biblioteca,Prestamos):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,_,prestamos(Prestamos)|_].


tdaBibliotecaGetMaxLibros(Biblioteca,MaxLibros):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,_,_,maxLibros(MaxLibros)|_].


tdaBibliotecaGetDiasMax(Biblioteca,DiasMax):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,_,_,_,diasMax(DiasMax)|_].


tdaBibliotecaGetTasaMulta(Biblioteca,TasaMulta):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,_,_,_,_,tasaMulta(TasaMulta)|_].


tdaBibliotecaGetLimiteDeuda(Biblioteca,LimiteDeuda):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,_,_,_,_,_,limiteDeuda(LimiteDeuda)|_].


tdaBibliotecaGetDiasRetraso(Biblioteca,DiasRetraso):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,_,_,_,_,_,_,diasRetraso(DiasRetraso)|_].


tdaBibliotecaGetFecha(Biblioteca,FechaBiblioteca):-
    tdaBibliotecaEsBiblioteca(Biblioteca),
    Biblioteca= [_,_,_,_,_,_,_,_,fechaBiblioteca(FechaBiblioteca)|_].

tdaBibliotecaSetLibros([libros(_),
        usuarios(Usuarios),
        prestamos(Prestamos),
        maxLibros(MaxLibros),
        diasMax(DiasMax),
        tasaMulta(TasaMulta),
        limiteDeuda(LimiteDeuda),
        diasRetraso(DiasRetraso),
        fechaBiblioteca(FechaBiblioteca)],

        LibrosNuevos,

       [libros(LibrosNuevos),
        usuarios(Usuarios),
        prestamos(Prestamos),
        maxLibros(MaxLibros),
        diasMax(DiasMax),
        tasaMulta(TasaMulta),
        limiteDeuda(LimiteDeuda),
        diasRetraso(DiasRetraso),
        fechaBiblioteca(FechaBiblioteca)
       ]).
tdaBibliotecaSetUsuarios([libros(Libros),
        usuarios(_),
        prestamos(Prestamos),
        maxLibros(MaxLibros),
        diasMax(DiasMax),
        tasaMulta(TasaMulta),
        limiteDeuda(LimiteDeuda),
        diasRetraso(DiasRetraso),
        fechaBiblioteca(FechaBiblioteca)],

        UsuariosNuevos,

       [libros(Libros),
        usuarios(UsuariosNuevos),
        prestamos(Prestamos),
        maxLibros(MaxLibros),
        diasMax(DiasMax),
        tasaMulta(TasaMulta),
        limiteDeuda(LimiteDeuda),
        diasRetraso(DiasRetraso),
        fechaBiblioteca(FechaBiblioteca)
       ]).


tdaBibliotecaSetPrestamos([libros(Libros),
                           usuarios(Usuarios),
                           prestamos(_),
                           maxLibros(MaxLibros),
                           diasMax(DiasMax),
                           tasaMulta(TasaMulta),
                           limiteDeuda(LimiteDeuda),
                           diasRetraso(DiasRetraso),
                           fechaBiblioteca(FechaBiblioteca)],

                          PrestamosNuevos,

                          [libros(Libros),
                           usuarios(Usuarios),
                           prestamos(PrestamosNuevos),
                           maxLibros(MaxLibros),
                           diasMax(DiasMax),
                           tasaMulta(TasaMulta),
                           limiteDeuda(LimiteDeuda),
                           diasRetraso(DiasRetraso),
                           fechaBiblioteca(FechaBiblioteca)]).






checkIDLibro(_,[]):-
    false.
checkIDLibro(ID,[Libro|Resto]):-
    tdaLibroGetID(Libro,IDL),
        (ID=:= IDL -> true ; checkIDLibro(ID,Resto)).

checkIDUsuario(_,[]):-
    false.
checkIDUsuario(ID,[Usuario|Resto]):-
    tdaUsuarioGetID(Usuario,IDU),
    (ID =:= IDU->
    true
    ;
    checkIDUsuario(ID,Resto)
    ).

tdaBibliotecaCheckLibro(IDEsperado, [Libro|_]):-
    tdaLibroGetID(Libro,IDL),
    IDEsperado =:= IDL,!,
    tdaLibroGetEstado(Libro,Estado),
    Estado== disponible.

tdaBibliotecaCheckLibro(IDEsperado, [_|Resto]):-
    tdaBibliotecaCheckLibro(IDEsperado,Resto).



tdaBibliotecaSusU([], _, []).

tdaBibliotecaSusU([Usuario|Resto], IdTarget, [UsuarioModificado|Resto]) :-
    tdaUsuarioGetID(Usuario, IdActual),
    IdActual =:= IdTarget, !,
    tdaUsuarioSetEstado(Usuario, suspendido, UsuarioModificado).
tdaBibliotecaSusU([Usuario|Resto], IdTarget, [Usuario|RestoModificado]) :-
    tdaBibliotecaSusU(Resto, IdTarget, RestoModificado).
