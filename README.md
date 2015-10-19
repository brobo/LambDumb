# LambDumb
Lambda calculus is stupid. We're here to make it even stupider.

## Compiling
Install ghc and run
```
ghc main.hs -o main
```
and then run the compiled executable.

## The DERPL
To run the Dumb Execute-Read-Print Loop, just run the LambDumb program without any arguments.

## Syntax
LambDumb follows traditional lambda calculus syntax as closely as possible, with the following changes:
* Variable names are multi-character and are separated by spaces
* The lambda symbol is replaced by the backquote "\"
* Aliasing is done with the "=" symbol

Examples:
````
    id = (\x . x)
    T = (\x y . x)
    F = (\x y . y)
    AND = (\x y . x y F)
````

To execute a single expression, wrap the expression in parenthesis, like such:
````(id (T a b))````
Any unbound variables are treated as literals.

## Making Lambdumb Less Dumb
If you want to make lambdumb a little less dumb, then sumbit a pull request. But I swear to everything Holy and True,
it would be in the best interest of your helath, safety, and existance to use the imperative tense.

## Bad Ideas
I would not recommend running the following commands:
````
  Y = (\f . (\x . f (x x)) (\x . f (x x)))
  (Y R)
````
