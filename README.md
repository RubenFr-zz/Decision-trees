# Decision-trees
First assignement in course Machine Learning 2021B

## Formulas

```
Entropy: H(X) = - Σx P(X=x) log2(P(X=x))

Conditional Entropy: H(Y, X) =Σx P(X = x) H(Y | X = x)

Information Gain: IG (Y, X) = H(Y) - H(Y | X)
```


## Remove Column from matrix
Remove 2nd column of random matrix of dimensions 6x4 -> result dimensions: 6x3 
```
(m = RandomInteger[9, {6, 4}]) // MatrixForm
Drop[m, None, {2}] // MatrixForm
```
