cantidadDeAceitunas (Capa (Aceitunas 9)  
                          (Capa (Aceitunas 8) 
                                (Capa Queso Prepizza))) = cantidadDeAceitunas (conDescripcionMejorada 
                                                                                (Capa (Aceitunas 9) 
                                                                                (Capa (Aceitunas 8)
                                                                                (Capa Queso Prepizza)))) ?

Izq) 

cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
----------------------------------------------------------------------------------
=                                                                                     def de cantidadDeAceitunas
cuentaAceituna (Aceitunas 9) + cantidadDeAceitunas(Capa (Aceitunas 8) (Capa Queso Prepizza))
---------------------------
=                                                                                     def de cuentaAceituna
9 + cantidadDeAceitunas(Capa (Aceitunas 8) (Capa Queso Prepizza))
    -------------------------------------------------------------
=                                                                                     def de cantidadDeAceitunas
9 + cuentaAceituna (Aceitunas 8) + cantidadDeAceitunas(Capa Queso Prepizza)
    ----------------------------
=                                                                                     def de cuentaAceituna
9 + 8 + cantidadDeAceitunas(Capa Queso Prepizza)
        ----------------------------------------
=                                                                                     def de cantidadDeAceitunas
9 + 8 + cuentaAceituna Queso + cantidadDeAceitunas Prepizza
        --------------------
=                                                                                     def de cuentaAceituna
9 + 8 + 0 + cantidadDeAceitunas Prepizza
            ----------------------------
=                                                                                     def de cantidadDeAceitunas
9 + 8 + 0 + 0
-------------
=                                                                                     def de aritm
17

Der)

cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
                     ---------------------------------------------------------------------------------------
=                            --      i                                               p                      def de conDescripcionMejorada
cantidadDeAceitunas (juntarCapas (Aceitunas 9) (conDescripcionMejorada(Capa (Aceitunas 8) (Capa Queso Prepizza))))
                     ---------------------------------------------------------------------------------------------
=                                                                                                           def de juntarCapas
cantidadDeAceitunas (Capa (Aceitunas 9 + 8) (conDescripcionMejorada(Capa Queso Prepizza)))
                                             -------------------------------------------
=                                                                                                           def de conDescripcionMejorada
cantidadDeAceitunas (Capa (Aceitunas 9 + 8) (juntarCapas Queso (conDescripcionMejorada Prepizza)))
                                             ----------------------------------------------------
=                                                                                                           def juntarCapas
cantidadDeAceitunas (Capa (Aceitunas 9 + 8) (Capa Queso (conDescripcionMejorada Prepizza))
                                                        ---------------------------------
=                                                                                                           def de conDescripcionMejorada
cantidadDeAceitunas (Capa (Aceitunas 9 + 8) (Capa Queso Prepizza))
------------------------------------------------------------------
=                                                                                                           def de cantidadDeAceitunas
cuentaAceituna (Aceitunas 9 + 8) + cantidadDeAceitunas (Capa Queso Prepizza)
--------------------------------
=                                                                                                           def de cuentaAceituna
(9 + 8) + cantidadDeAceitunas (Capa Queso Prepizza)
-------
=                                                                                                           def de aritm
17 + cantidadDeAceitunas (Capa Queso Prepizza)
      ---------------------------------------
=                                                                                                           def de cantidadDeAceitunas
17 + cuentaAceituna Queso + cantidadDeAceitunas Prepizza
      ------------------
=                                                                                                           def de cuentaAceituna
17 + 0 + cantidadDeAceitunas Prepizza
         ----------------------------
=                                                                                                           def de cantidadDeAceitunas
17 + 0 + 0
-----------
=                                                                                                           def de aritm
17

