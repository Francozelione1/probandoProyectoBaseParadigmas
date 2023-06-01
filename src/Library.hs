module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Sabor = String

data Postre= UnPostre{
    sabores:: [Sabor],
    peso:: Number,
    temperatura:: Number
}deriving Show

type Hechizo = (Postre -> Postre)

modificarPeso:: Number -> Number -> Number
modificarPeso num1 num2= num1 + num2

modificarTemperatura:: Number -> Number -> Number
modificarTemperatura num1 num2= num1 + num2

agregarSabor:: [Sabor] -> Sabor -> [Sabor]
agregarSabor listaSabores sabor = listaSabores ++ [sabor]

incendio:: Hechizo
incendio postre = postre{temperatura= modificarTemperatura (temperatura postre) (1), peso= modificarPeso (peso postre) (-(peso postre *0.05))}

inmobulus:: Hechizo
inmobulus postre = postre{temperatura= modificarTemperatura (temperatura postre) (- temperatura postre)}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre{peso= modificarPeso (peso postre) (-(peso postre *0.1)), sabores= agregarSabor (sabores postre) ("concentrado")}

diffindo:: Number -> Hechizo
diffindo num postre = postre{peso= modificarPeso (peso postre) (- ((num * peso postre)/100))}

riddikulus:: Sabor -> Hechizo
riddikulus sabor postre = postre{sabores= reverse (agregarSabor (sabores postre) (sabor))}

avadaKedavra:: Hechizo
avadaKedavra postre = postre{temperatura= modificarTemperatura (temperatura postre) (- temperatura postre), sabores=[]}

estaListo:: Hechizo -> Postre -> Bool
estaListo hechizo postre = (length (sabores (hechizo postre))) > 0 && peso (hechizo postre) > 0 && temperatura (hechizo postre) > 0

estanListos:: [Postre] -> Hechizo -> Bool
estanListos listaPostres hechizo = all (estaListo hechizo) listaPostres

obtenerPeso:: Postre -> Number
obtenerPeso postre = peso postre

pesoPromedio:: [Postre] -> Number
pesoPromedio listaPostres = (sum . map peso ) listaPostres / length listaPostres

data Mago = UnMago{
    hechizos:: [Hechizo],
    horrocruxes:: Number
} deriving Show

practicarConHechizo:: Hechizo -> Mago -> [Hechizo]
practicarConHechizo hechizo mago = hechizos mago ++ [hechizo]

verificarAvada:: Eq Postre => Postre -> Hechizo -> Bool
verificarAvada postre hechizo = (hechizo postre) /= (avadaKedavra postre)

asistirClase:: Eq Postre => Mago -> Postre -> Hechizo -> Mago
asistirClase mago postre hechizo 
    | (verificarAvada postre hechizo) == True = mago{hechizos= practicarConHechizo hechizo mago}
    | otherwise = mago{hechizos= practicarConHechizo hechizo mago, horrocruxes= horrocruxes mago + 1}

--mejorHechizo:: Mago -> Postre -> [Postre]
--mejorHechizo mago postre = map postre (hechizos mago)

mejorHechizo:: Mago -> [Hechizo] -> Postre -> Hechizo
mejorHechizo mago [hechizo] postre = hechizo
mejorHechizo mago (primero:segundo:resto) postre
    | (length (sabores (primero postre))) >= ((length (sabores (segundo postre)))) = mejorHechizo mago (primero:resto) postre
    | otherwise = mejorHechizo mago (segundo:resto) postre


postre1= UnPostre{sabores=["menta", "chocolate"], peso= 5, temperatura=5}
mago1= UnMago{hechizos=[inmobulus, avadaKedavra, incendio, wingardiumLeviosa], horrocruxes=0}

listaInfinitaDeHechizos:: [Hechizo]
listaInfinitaDeHechizos = repeat avadaKedavra

magoConHechizosInfinitos:: Mago
magoConHechizosInfinitos = UnMago{ hechizos= repeat inmobulus ,horrocruxes=0}

{- Si existe dicha consulta y es la funcion "estanListos" que utiliza la funcion all, corroborando que todos los postres de la lista
esten listos al aplicarles cierto hechizo. La funcion all se detiene al encontrar un elemento falso, por lo tanto al aplicarle un hechizo
a una lista infinita de postres, se detendrá cuando encuentre uno que no lo cumpla -}

{- No se puede verificar cual es el mejor hechizo de una lista infinita de hechizos a pesar de la lazy evaluation con la que trabaja
Haskell ya que necesitaría verificar todos porque en este caso, estamos buscando comparar uno por uno. Lo que se podría obtener por ejemplo es,
el primer elemento de dicha lista o un elemento en una posicion particular de la misma -}



{- Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
Immobulus: congela el postre, llevando su temperatura a 0.
Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. Además, pierde 10% de su peso.
Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado. 
Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que tiene un postre, pero invertido.
Avada kedavra: Hace lo mismo que el immobulus pero además hace que el postre pierda todos sus sabores.  -}