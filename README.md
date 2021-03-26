# Decision-trees
**First assignement in course Machine Learning 2021B**  :sparkles:

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

### MyEntropy[list]
Replace the native one in mathematica by taking don't care (**-1**) into account
```mathematica
MyEntropy[li_List] :=
 Module[{list = li, tot, prob, H},
  tot = 2^Count[-1] /@ list ;
  prob = (tot/(Total@tot)) // N;
  H = -Sum[prob[[i]] log2[prob[[i]]], {i, Length[list]}] // N
  ]
```

### CondEntropy[list, index]
Calculate the conditional entropy of the **_list_** for a specific bit (**_index_**)
```mathematica
CondEntropy[li_List, ind_Integer] :=
 Module[{list = li, i = ind, c0, c1, H},
  c0 = Drop[#, None, {i}] &@Select[rules, #[[i]] != 1  &];
  c1 = Drop[#, None, {i}] &@Select[rules, #[[i]] != 0 &];
  H = ((Length[c0]/Length[list]) MyEntropy[
        c0] + (Length[c1]/Length[list]) MyEntropy[c1]) // N
  ]
```

### FindBestChoice[list]
For a given **_list_** return the best index to pick top maximize the **IG**
```mathematica
FindBestChoice[li_List] :=
 Module[{list = li, tot, prob, H, condEntropy, IG, bestCond},
  H = MyEntropy[list];
  allEntropy = Table[CondEntropy[list, i], {i, 2, Length[list[[1]]]}];
  IG = H - # &@allEntropy;
  bestCond = Part[#, 1] &@Flatten@Position[IG, Max[IG]]
  ]
```

## Evaluate timing of a function
```Mathematica
timeIt::usage = 
  "timeIt[expr] gives the time taken to execute expr,   repeating as \
many times as necessary to achieve a total time of 1s";

SetAttributes[timeIt, HoldAll]
timeIt[expr_] := 
 Module[{t = Timing[expr;][[1]], tries = 1}, 
  While[t < 1., tries *= 2; t = Timing[Do[expr, {tries}];][[1]];];
  t/tries]
```

# Example

Rules| b1 | b2 | b3 | b4 
- | - | - | - | -
R1| 1 | 0 | * | * 
R2| 0 | 1 | * | * 
R3| 1 | 1 | 0 | * 
R4| 0 | 1 | 0 | 0 

> Every * (don't care) duplicate the rule -> R1 & R2 appear 4 times, R3 twice and R4 only once.
> In total it is like there are 11 rules

```Mathematica
H(Rule) = -(4/11 log2(4/11) + 4/11 log2(4/11) + 2/11 log2(2/11) + 1/11 log2(1/11)) = 
```

#### Conditional Entropy given b1
```
H(Rule | b1=0) = 4/5 log2(4/5) + 1/5 log2(1/5)) = 
H(Rule | b1=1) = 4/6 log2(4/6) + 2/6 log2(1/6)) =
H(Rule | b1) = 1/2 H(Rule | b1=0) + 1/2 H(Rule | b1=1) =
```
