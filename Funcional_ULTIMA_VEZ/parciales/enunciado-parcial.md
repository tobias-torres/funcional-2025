# Parcial de Haskell - Estructuras JSON con Índices

## Tipos de Datos Dados

```haskell
data Value = Vint Int | Vstring String | Vbool Bool | Vlist [Value]
type Field = String
data Svson i = Empty | Obj i (Field -> Maybe Value) (Svson i) (Svson i)
```

La estructura `Svson i` representa un árbol binario de búsqueda (BST) donde cada nodo contiene:
- Un índice de tipo `i` (que debe ser ordenable)
- Una función que mapea nombres de campos a valores opcionales
- Dos subárboles izquierdo y derecho

## Funciones Auxiliares Ya Implementadas

```haskell
valuesOf :: [Field] -> (Field -> Maybe Value) -> [Value]
-- Obtiene los valores de una lista de campos de un objeto

valuesWithFields :: [Field] -> (Field -> Maybe Value) -> [(Field, Value)]
-- Obtiene pares (campo, valor) para los campos que existen

only :: [Field] -> (Field -> Maybe Value) -> (Field -> Maybe Value)
-- Restringe un objeto a solo los campos especificados

update :: Field -> Value -> (Field -> Maybe Value) -> (Field -> Maybe Value)
-- Actualiza o agrega un campo con un valor en un objeto

singleton :: Field -> Value -> (Field -> Maybe Value)
-- Crea un objeto con un solo campo y valor
```

## Ejercicios a Resolver

### 1. indices (2 puntos)
Implementar la función:
```haskell
indices :: Svson i -> [i]
```
Que devuelve una lista con todos los índices presentes en la estructura, en cualquier orden.

### 2. belongs (2 puntos)
Implementar la función:
```haskell
belongs :: Ord i => i -> Svson i -> Bool
```
Que indica si un índice dado existe en la estructura. Debe aprovechar la propiedad de BST para ser eficiente.

### 3. lookupProjecting (3 puntos)
Implementar la función:
```haskell
lookupProjecting :: Ord i => i -> [Field] -> Svson i -> [Value]
```
Que busca un índice específico y devuelve los valores de los campos solicitados para ese índice. Si el índice no existe, devuelve una lista vacía.

### 4. upsert (3 puntos)
Implementar la función:
```haskell
upsert :: Ord i => i -> Field -> Value -> Svson i -> Svson i
```
Que actualiza o inserta un campo en el objeto con el índice dado. Si el índice no existe, crea un nuevo nodo en la posición correcta del BST.

### 5. mkObj (2 puntos)
Implementar la función:
```haskell
mkObj :: Svson i -> Svson i -> Svson i
```
Que realiza la intersección entre dos estructuras, combinando los objetos que tienen índices en común.

### 6. takeUpSatisfying (4 puntos)
Implementar la función:
```haskell
takeUpSatisfying :: Ord i => Int -> (i -> Bool) -> Svson i -> (Svson i, Int)
```
Que devuelve hasta `n` objetos que cumplan la condición dada, junto con el número real de objetos encontrados. Debe priorizar los índices menores (recorrido in-order del BST).

### 7. Esquemas de Recursión (4 puntos)
Se definen los siguientes esquemas de recursión:

```haskell
foldS :: a -> (i -> (Field -> Maybe Value) -> a -> a -> a) -> Svson i -> a
recS :: a -> (i -> (Field -> Maybe Value) -> Svson i -> a -> Svson i -> a -> a) -> Svson i -> a
```

Implementar estas dos funciones que permiten realizar recursión estructural sobre `Svson`.

### 8. Reimplementación con Esquemas (Bonus - 2 puntos)
Reimplementar las siguientes funciones utilizando `foldS` o `recS`:
- `indices'`
- `belongs'`
- `lookupProjecting'`
- `upsert'`
- `takeUpSatisfying'`

## Demostración Teórica (Bonus - 3 puntos)

Demostrar por inducción que para toda estructura `s` de tipo `Svson i` y todo índice `i`:

```haskell
belongs i s = elem i (indices s)
```

La demostración debe seguir el método de inducción estructural sobre `s`.

## Notas Importantes

- Todas las funciones deben mantener la propiedad de BST
- Se puede asumir que los índices son únicos en la estructura
- Las funciones deben ser eficientes aprovechando la estructura de árbol
- Para `takeUpSatisfying`, el recorrido debe ser in-order para priorizar índices menores