# Decision-trees
First assignement in course Machine Learning 2021B

## Formulas

* Entropy: H(X) = - Σx P(X=x) log2(P(X=x))
* Conditional Entropy: H(Y, X) = Σx P(X = x) H(Y | X = x)
* Information Gain: IG (Y, X) = H(Y) - H(Y | X)

For entropy, the smaller the better (big entropy -> big uncertainty). For IG, the higher the better. 
> H(x) = 0 -> no uncertainty

## Remove Column from matrix
Remove 2nd column of random matrix of dimensions 6x4 -> result dimensions: 6x3 
```mathematica
(m = RandomInteger[9, {6, 4}]) // MatrixForm
Drop[m, None, {2}] // MatrixForm
```

## Entropy with don't cares
If we have a don't care bit, we need to "duplicate" the rule to include both cases. 
> To calculate the entropy we take the probability with 2^(# of don't cares)
```mathematica
tot = 2^Count[-1] /@ rules;
prob = (tot/(Total@tot)) // N;
H = -Sum[prob[[i]] log2[prob[[i]]], {i, Length[c0]}] // N;
condEntropy = Table[EntropyWithDontCare[rules, i], {i, 2, Length[rules[[1]]]}];
IG = H - # &@ condEntropy;
```
```mathematica
EntropyWithDontCare[li_List, ind_Integer] :=
 Module[{list = li, i = ind, tot, tot0, tot1, c0, c1, prob0, prob1, H0, H1, H},
  c0 = Drop[#, None, {i}] &@Select[rules, #[[i]] != 1  &];
  tot0 = 2^Count[-1] /@ c0 ;
  prob0 = (tot0/(Total@tot0)) // N;
  H0 = -Sum[prob0[[i]] log2[prob0[[i]]], {i, Length[c0]}] // N;
  
  c1 = Drop[#, None, {i}] &@Select[rules, #[[i]] != 0 &];
  tot1 = 2^Count[-1] /@ c1;
  prob1 = (tot1/(Total@tot1)) // N;
  H1 = -Sum[prob1[[i]] log2[prob1[[i]]], {i, Length[c1]}] // N;
  
  H = ((Length[c0]/Length[li]) H0 + (Length[c1]/Length[li]) H1) // N
  ]
```

## Function timeIt[function_]
```mathematica
timeIt::usage = 
  "timeIt[expr] gives the time taken to execute expr,   repeating as \
many times as necessary to achieve a total time of 1s";

SetAttributes[timeIt, HoldAll]
timeIt[expr_] := 
 Module[{t = Timing[expr;][[1]], tries = 1}, 
  While[t < 1., tries *= 2; t = Timing[Do[expr, {tries}];][[1]];];
  t/tries]
```
