:- consult('TDA_fecha_21602350_CruzSantander.pl').
:- consult('TDA_usuario_21602350_CruzSantander.pl').
:- consult('TDA_libro_21602350_CruzSantander.pl').
:- consult('TDA_prestamo_21602350_CruzSantander.pl').
:- consult('TDA_biblioteca_21602350_CruzSantander.pl').



/*RF02>01
 crearUsuario
 Descripción: Crea un usuario con deuda inicial 0 y estado activo (no suspendido).
 Dom: id (int) X nombre (string) X usuario (Usuario)
 Rec: usuario(U)
*/
crearUsuario(IDUsuario, Nombre, U) :-
    U = [ idUsuario(IDUsuario),
                nombreUsuario(Nombre),
                deudaUsuario(0),
                librosUsuario([]),
                estadoUsuario(activo)
             ].



/* RF03<01
 crearLibro
 Descripción: Crea un libro con id, título y autor. Título y autor se convierten en minúsculas al crear.
 Dom: id (int) X titulo (string) X autor (string) X libro (Libro)
 Rec: libro(L)
*/
crearLibro(IDLibro, Titulo, Autor,L ) :-
    string_lower(Titulo, TituloLower),
    string_lower(Autor, AutorLower),
    L = [ idLibro(IDLibro),
                tituloLibro(TituloLower),
                autorLibro(AutorLower),
                estadoLibro(disponible)
             ].



/* RF04>01
crearPrestamo
 Descripción: Crea un préstamo.
Dom: id (int) X idUsuario (int) X idLibro (int) X fechaPrestamo
(string) X diasSolicitados (int) X prestamo (Prestamo)
Rec: prestamo(P)
*/
crearPrestamo(IDPrestamo, IDUsuario, IDLibro, FechaPrestamo, DiasSolicitados, P):-
    P = [idPrestamo(IDPrestamo),
         idUsuario(IDUsuario),
         idLibro(IDLibro),
         fechaPrestamo(FechaPrestamo),
         diasSolicitados(DiasSolicitados),
         estadoPrestamo(activo)
        ].



/* RF05>02,03,04
crearBiblioteca
 Descripción: Crea la biblioteca con parámetros de configuración dados
 Dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista
 Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
 tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso
 (int) X fecha-inicial (string) X biblioteca (Biblioteca)
 Rec:
 biblioteca(B)
*/
crearBiblioteca(Libros, Usuarios, Prestamos, MaxLibros, DiasMax, TasaMulta, LimiteDeuda, DiasRetraso, FechaBiblioteca, B) :-
    MaxLibros > 0,
    DiasMax > 0,
    TasaMulta >= 0,
    LimiteDeuda >= 0,
    DiasRetraso >= 0,

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





/* RF06>05
agregarLibro
 Descripción: Agrega un libro a la biblioteca. **Si el ID ya existe, no se agrega y retorna la biblioteca sin cambios**. Se permiten libros con mismo título/autor pero diferente ID. **La posición donde se agrega el libro queda a criterio del estudiante**.
 Dom: biblioteca (Biblioteca) X libro (Libro) X biblioteca (Biblioteca)
 Rec: BibliotecaOut(B)
*/
agregarLibro(BibliotecaIn, Libro1, BibliotecaOut) :-

    tdaLibroGetID(Libro1, IDNuevo),

    tdaBibliotecaGetLibros(BibliotecaIn, LibrosActuales),

    ( checkIDLibro(IDNuevo, LibrosActuales) ->
        BibliotecaOut = BibliotecaIn
    ;
        append(LibrosActuales, [Libro1], LibrosNuevos),
        tdaBibliotecaSetLibros(BibliotecaIn, LibrosNuevos, BibliotecaOut)
    ).



/* RF07>05
registrarUsuario
 Descripción: Registra usuario en el sistema.
Dom: biblioteca
 (Biblioteca) X usuario (Usuario) X biblioteca (Biblioteca)
 Rec:bibliotecaOut(B)
*/
registrarUsuario(BibliotecaIn, Usuario, BibliotecaOut):-

    tdaUsuarioGetID(Usuario,IDNuevo),
    tdaBibliotecaGetUsuarios(BibliotecaIn,UsuariosActuales),

    (checkIDUsuario(IDNuevo,UsuariosActuales)->
    BibliotecaOut=BibliotecaIn
    ;
    append(UsuariosActuales,[Usuario],UsuariosNuevos),
    tdaBibliotecaSetUsuarios(BibliotecaIn,UsuariosNuevos,BibliotecaOut)
    ).




/* RF08>07
obtenerUsuario
 Descripción: Busca usuario por ID. **Retorna false si no encuentra el usuario**.
 Dom: biblioteca (Biblioteca) X id (int) X usuario (Usuario)
 Rec: usuario(U)
*/
obtenerUsuario(BibliotecaIn,IDBusca,UsuarioEncontrado):-

    tdaBibliotecaGetUsuarios(BibliotecaIn,ListaUsuarios),

    tdaUsuarioBuscarUsuario(IDBusca,ListaUsuarios,UsuarioEncontrado).



/* RF09 >08
buscarLibro
 Descripción: Busca un libro por "id", "titulo" o "autor".
 Dom: biblioteca (Biblioteca) X criterio (string) X valor (string/int) X libro (Libro)
 Rec: LibroOut(L)
*/
buscarLibro(BibliotecaIn, Criterio, Valor, LibroOut) :-

    tdaBibliotecaGetLibros(BibliotecaIn, ListaLibros),

    tdaLibroBuscarLibro(ListaLibros, Criterio, Valor, LibroOut).



/* RF10 >09
getLibroId
 Descripción: obtiene el ID de un libro.
 Dom: libro(L) X ID(int)
 Rec: id(int)
*/

getLibroId(Libro,ID):-
    tdaLibroGetID(Libro,ID).




/* RF11>10
getFecha
 Descripción: Obtiene la fecha actual del sistema desde la biblioteca.
 Dom: biblioteca (Biblioteca) X fecha (String)
 Rec: Fecha(F)
*/
getFecha(Biblioteca, Fecha) :-
    tdaBibliotecaGetFecha(Biblioteca, Fecha).



/* RF12>11
isLibroDisponible
 Descripción: Verifica si un libro está disponible (no prestado)
 Dom: biblioteca (Biblioteca) X idLibro (int) Rec: boolean
*/
isLibroDisponible(BibliotecaIn,IDLibro):-

    tdaBibliotecaGetLibros(BibliotecaIn,ListaLibros),
    tdaBibliotecaCheckLibro(IDLibro,ListaLibros).



/* RF13> 12
isUsuarioSuspendido
 Descripción: Verifica estado de suspensión del usuario.
 Dom: usuario (Usuario)
 Rec: boolean
*/
isUsuarioSuspendido(Usuario):-
    tdaUsuarioGetEstado(Usuario,Estado),
    Estado== suspendido.



/* RF14>13
obtenerDeuda
 Descripción: Retorna la deuda acumulada del usuario.
 Dom: usuario (Usuario) X deuda (number)
 Rec: Deuda(D)
*/
obtenerDeuda(Usuario, Deuda):-
    tdaUsuarioGetDeuda(Usuario, Deuda).



/* RF15>14
obtenerFechaVencimiento
 Descripción: Calcula fecha de vencimiento sumando días solicitados a
 fecha de préstamo.
Dom: prestamo (Prestamo) X fecha (String)
 Rec:FechaVencimiento(F)
*/
%auxiliar en tdafecha
obtenerFechaVencimiento(Prestamo, FechaVencimiento) :-
    tdaPrestamoGetFecha(Prestamo, FechaInicio),
    tdaPrestamoGetDias(Prestamo, DiasSolicitados),
    sumarDias(FechaInicio, DiasSolicitados, FechaVencimiento).





/* RF16 >15
calcularDiasRetraso
 Descripción: Calcula días de retraso entre fecha de vencimiento y fecha
 actual.
Dom: fecha-vencimiento (string) X fecha-actual (string) X dias(int)
 Rec: DIAS(int)
*/
%aux tdafecha
calcularDiasRetraso(FechaVenc, FechaActual, Dias) :-

    diasTotales(FechaVenc, TotalVenc),
    diasTotales(FechaActual, TotalActual),

    Diferencia is TotalActual - TotalVenc,

    ( Diferencia > 0 ->
        Dias is Diferencia
    ;
        Dias is 0
    ).



/* RF17>16
calcularMulta
 Descripción: Calcula multa multiplicando días de retraso por tasa diaria.
 Dom: prestamo (Prestamo) X fecha-actual (string) X tasa-multa-diaria (int) X multa (number)
 Rec: Multa(M)
*/
calcularMulta(PrestamoIn, FechaActual, Tasa, Multa) :-

    obtenerFechaVencimiento(PrestamoIn, FechaVencimiento),

    calcularDiasRetraso(FechaVencimiento, FechaActual, DiasRetraso),

    Multa is DiasRetraso * Tasa.




/* RF18>17
tomarPrestamo
 Descripción: Usuario toma libro prestado, solo si cumple diversos
 requisitos.
Dom: biblioteca (Biblioteca) X id-usuario (int) X id-libro(int)
X dias-solicitados (int) X fecha-actual (string) X biblioteca(Biblioteca)
 Rec: BibliotecaOut(B)
*/
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



/* RF19>18
devolverLibro
 Descripción: Devuelve un libro y procesa multas, suspende
 automaticamente.
Dom: biblioteca (Biblioteca) X id-usuario (int) X id-libro (int) X
fecha-actual (string) X biblioteca (Biblioteca)
Rec: BibliotecaOut(B)
*/
devolverLibro(BibIn, IdUser, IdLibro, _, BibOut) :-

    tdaBibliotecaGetPrestamos(BibIn, Prestamos),
    tdaBibliotecaGetUsuarios(BibIn, Usuarios),
    tdaBibliotecaGetLibros(BibIn, Libros),

    tdaPrestamoBuscar(Prestamos, IdUser, IdLibro, Prestamo),

    eliminarElemento(Prestamos, Prestamo, PrestamosActualizados),

    tdaLibroActualizarE(Libros, IdLibro, disponible, LibrosActualizados),

    obtenerUsuario(BibIn, IdUser, Usuario),
    tdaUsuarioEliminarLibro(Usuario, IdLibro, UsuarioActualizado),

    tdaUsuarioActualizarL(Usuarios, UsuarioActualizado, UsuariosActualizados),

    tdaBibliotecaSetLibros(BibIn, LibrosActualizados, B_Tmp1),
    tdaBibliotecaSetUsuarios(B_Tmp1, UsuariosActualizados, B_Tmp2),
    tdaBibliotecaSetPrestamos(B_Tmp2, PrestamosActualizados, BibOut).




/* RF20>21
debeSuspenderse
 Descripción: Verifica si el usuario debe suspenderse automáticamente
 por ciertos criterios.
Dom: biblioteca (Biblioteca) X id-usuario (int) X fecha-actual (string)
Rec: boolean
*/
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




/* RF21>19
suspenderUsuario
 Descripción: Suspende al usuario manualmente cambiando su estado a true. Si el usuario se encuentra suspendido entonces el usuario se mantiene intacto en la biblioteca de salida.
 Dom: biblioteca (Biblioteca) X idUsuario (int) X biblioteca (Biblioteca)
 Rec: BibliotecaOut(B)
*/
suspenderUsuario(BibliotecaIn, IdUsuario, BibliotecaOut) :-

    tdaBibliotecaGetUsuarios(BibliotecaIn, UsuariosActuales),

    tdaBibliotecaSusU(UsuariosActuales, IdUsuario, UsuariosNuevos),

    tdaBibliotecaSetUsuarios(BibliotecaIn, UsuariosNuevos, BibliotecaOut).




/* RF22>20
renovarPrestamo
 Descripción: Renueva préstamo agregando días extra, verifica
 prestamo existente, usuario no suspendido, sin retrasos y que los
 dias totales no superen el maximo.
Dom: biblioteca (Biblioteca) X id-prestamo (int) X dias-extra (int) X
fecha-actual (string) X biblioteca (Biblioteca)
Rec: BibliotecaOut(B)
*/
renovarPrestamo(BibliotecaIn, IdPrestamo, DiasExtra, FechaActual, BibliotecaOut) :-

    tdaBibliotecaGetDiasMax(BibliotecaIn, DiasMaxPermitidos),
    tdaBibliotecaGetPrestamos(BibliotecaIn, ListaPrestamos),


    buscarPrestamoPorID(ListaPrestamos, IdPrestamo, PrestamoOriginal),

    tdaPrestamoGetIDUsuario(PrestamoOriginal, IdUsuario),
    obtenerUsuario(BibliotecaIn, IdUsuario, Usuario),
    \+ isUsuarioSuspendido(Usuario),


    obtenerFechaVencimiento(PrestamoOriginal, FechaVencimiento),
    calcularDiasRetraso(FechaVencimiento, FechaActual, DiasRetraso),
    DiasRetraso =:= 0,


    tdaPrestamoGetDias(PrestamoOriginal, DiasSolicitados),
    NuevosDiasTotales is DiasSolicitados + DiasExtra,
    NuevosDiasTotales =< DiasMaxPermitidos,


    tdaPrestamoGetIdLibro(PrestamoOriginal, IdLibro),
    tdaPrestamoGetFecha(PrestamoOriginal, FechaInicio),


    crearPrestamo(IdPrestamo, IdUsuario, IdLibro, FechaInicio, NuevosDiasTotales, PrestamoRenovado),


    tdaPrestamoActualizar(ListaPrestamos, PrestamoRenovado, PrestamosActualizados),
    tdaBibliotecaSetPrestamos(BibliotecaIn, PrestamosActualizados, BibliotecaOut).




/* RF23>22
pagarDeuda
 Descripción: Usuario paga monto para reducir deuda, actva usuario
 sipaga el total.
Dom: biblioteca (Biblioteca) X id-usuario (int) X monto(int)
X biblioteca (Biblioteca)
Rec: BibliotecaOut(B)
*/
pagarDeuda(BibliotecaIn, IdUsuario, Monto, BibliotecaOut) :-
    tdaBibliotecaGetFecha(BibliotecaIn, FechaActual),
    tdaBibliotecaGetDiasRetraso(BibliotecaIn, MaxDiasRetraso),
    tdaBibliotecaGetPrestamos(BibliotecaIn, Prestamos),

    obtenerUsuario(BibliotecaIn, IdUsuario, Usuario),
    obtenerDeuda(Usuario, DeudaActual),

    CalculoDeuda is DeudaActual - Monto,
    (CalculoDeuda < 0 -> DeudaFinal = 0 ; DeudaFinal = CalculoDeuda),

    tdaUsuarioSetDeuda(Usuario, DeudaFinal, UsuarioConDeuda),


    ( (DeudaFinal =:= 0, \+ tienePrestamoAtrasado(Prestamos, IdUsuario, FechaActual, MaxDiasRetraso)) ->
        NuevoEstado = activo
    ;
        tdaUsuarioGetEstado(Usuario, NuevoEstado)
    ),

    tdaUsuarioSetEstado(UsuarioConDeuda, NuevoEstado, UsuarioActualizado),

    tdaBibliotecaGetUsuarios(BibliotecaIn, ListaUsuarios),
    tdaUsuarioActualizarL(ListaUsuarios, UsuarioActualizado, UsuariosNuevos),
    tdaBibliotecaSetUsuarios(BibliotecaIn, UsuariosNuevos, BibliotecaOut).


/* RF24>23
 historialPrestamosUsuario
 Descripción: Obtiene préstamos de un usuario formateados como string.
 Retorna string vacío si el usuario no tiene préstamos.
 Incluye encabezado y cálculo de fecha de vencimiento.
 Dom: biblioteca (Biblioteca) X IdUsuario (int) X str (String)
 Rec: StringSalida
*/
historialPrestamosUsuario(BibliotecaIn, IdUsuario, StrSalida) :-
    tdaBibliotecaGetPrestamos(BibliotecaIn, ListaPrestamos),

    tdaBibliotecaEscribirUsuario(ListaPrestamos, IdUsuario, CuerpoStr),


    ( CuerpoStr = "" ->
        StrSalida = ""
    ;
        number_string(IdUsuario, IdStr),
        string_concat("Historial Usuario ", IdStr, S1),
        string_concat(S1, ":\n", Header),
        string_concat(Header, CuerpoStr, StrSalida)
    ).




/* RF25
 historialPrestamosSistema
 Descripción: Obtiene todos los préstamos del sistema formateados como string.
 Muestra: ID préstamo, ID usuario, ID libro, fecha, días y estado.
 Dom: biblioteca (Biblioteca) X str (String)
 Rec: StringSalida
*/
historialPrestamosSistema(BibliotecaIn, StrSalida) :-
    tdaBibliotecaGetPrestamos(BibliotecaIn, ListaPrestamos),
    tdaBibliotecaEscribir(ListaPrestamos, StrSalida).










/* cobrarMultasDelDia
   Recorre todos los préstamos activos. si un prestamo está vencido le
   suma la Tasa de multa, usa recursion de cola
*/
cobrarMultasDelDia([], Usuarios, _, _, Usuarios).

cobrarMultasDelDia([Prestamo|RestoPrestamos], UsuariosIn, FechaActual, Tasa, UsuariosOut) :-
    ( prestamoEstaVencido(Prestamo, FechaActual) ->
        tdaPrestamoGetIDUsuario(Prestamo, IdUser),
        aplicarMultaUsuario(UsuariosIn, IdUser, Tasa, UsuariosTmp)
    ;
        UsuariosTmp = UsuariosIn
    ),
    cobrarMultasDelDia(RestoPrestamos, UsuariosTmp, FechaActual, Tasa, UsuariosOut).



/* prestamoEstaVencido
   Retorna true si la FechaActual es mayor a la fecha de vencimiento.
*/
prestamoEstaVencido(Prestamo, FechaActual) :-
    tdaPrestamoGetFecha(Prestamo, FechaInicio),
    tdaPrestamoGetDias(Prestamo, DiasSolicitados),
    sumarDias(FechaInicio, DiasSolicitados, FechaVencimiento),

    diasTotales(FechaVencimiento, TotalVenc),
    diasTotales(FechaActual, TotalHoy),
    TotalHoy > TotalVenc.



/* aplicarMultaUsuario
   Busca un usuario por ID en la lista Usuario y aumenta su deuda
*/
aplicarMultaUsuario([User|Resto], IdTarget, Monto, [UserMod|Resto]) :-
    tdaUsuarioGetID(User, IdCurrent),
    IdCurrent =:= IdTarget, !,

    tdaUsuarioGetDeuda(User, DeudaActual),
    NuevaDeuda is DeudaActual + Monto,
    tdaUsuarioSetDeuda(User, NuevaDeuda, UserMod).

aplicarMultaUsuario([User|Resto], IdTarget, Monto, [User|RestoMod]) :-
    aplicarMultaUsuario(Resto, IdTarget, Monto, RestoMod).




/* actualizarEstadoUsuarios
   Descripción: Recorre la lista de usuarios y verifica si deben suspenderse
   con la nueva fecha
   Dom: ListaUsuarios(list) X BiblioRef(Biblioteca) X NuevaFecha(string)
   Rec: ListaUsuariosActualizada(list)
   Tipo de recursion:de cola
*/
actualizarEstadoUsuarios([], _, _, []).

actualizarEstadoUsuarios([Usuario|Resto], BiblioRef, NuevaFecha, [UsuarioOut|RestoOut]) :-
    tdaUsuarioGetID(Usuario, IdUser),

    ( debeSuspenderse(BiblioRef, IdUser, NuevaFecha) ->
        tdaUsuarioSetEstado(Usuario, suspendido, UsuarioOut)
    ;
        UsuarioOut = Usuario
    ),
    actualizarEstadoUsuarios(Resto, BiblioRef, NuevaFecha, RestoOut).


/* RF26>25
procesarDia
 Descripción: Cobra multas diarias a préstamos vencidos, avanza la fecha
 y actualiza suspensiones, el orden es importante porque debe cambiar
 la fecha antes de los neuvos cobro.
*/
procesarDia(BibliotecaIn, BibliotecaOut) :-
    tdaBibliotecaGetFecha(BibliotecaIn, FechaActual),
    tdaBibliotecaGetPrestamos(BibliotecaIn, Prestamos),
    tdaBibliotecaGetUsuarios(BibliotecaIn, UsuariosIn),
    tdaBibliotecaGetTasaMulta(BibliotecaIn, Tasa),

    tdaFechaPasar(FechaActual, NuevaFecha),

    tdaBibliotecaSetFecha(BibliotecaIn, NuevaFecha, BiblioConFecha),


    cobrarMultasDelDia(Prestamos, UsuariosIn, NuevaFecha, Tasa, UsuariosConDeuda),

    actualizarEstadoUsuarios(UsuariosConDeuda, BiblioConFecha, NuevaFecha, UsuariosFinales),

    tdaBibliotecaSetUsuarios(BiblioConFecha, UsuariosFinales, BibliotecaOut).
