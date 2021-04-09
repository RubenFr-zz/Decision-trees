# Decision Trees
**First assignement in course Machine Learning 2021B at [BGU](https://in.bgu.ac.il/en/Pages/default.aspx)**  :sparkles:  
All the code is in the Wolfram Mathematica language

## Formulas

> * Entropy: H(X) = - Σx P(X=x) log<sub>2</sub>(P(X=x))
> * Conditional Entropy: H(Y | X) = Σx P(X = x) H(Y | X = x)
> * Information Gain: IG (Y, X) = H(Y) - H(Y | X)

For entropy, the smaller the better (big entropy -> big uncertainty). For IG, the higher the better. 
> H(x) = 0 &rarr; no uncertainty

## Entropy & Information Gain (IG) with don't cares
If we have a don't care bit, we need to "duplicate" the rule to include both cases. 
> In practice, each rule appears 2^(# of don't cares)

Let's find the analitycal expression of IG(R<sub>i</sub>,b<sub>j</sub>):
In order to compute the Entropy and IG we let's define new variables:
* **N<sub>0</sub>: # of rules where b<sub>j</sub> = 0**
* **N<sub>1</sub>: # of rules where b<sub>j</sub> = 1**
* **N<sub>&Phi;</sub>: # of rules where b<sub>j</sub> = &Phi; (note this is half the # of rules with 0/1)**
* **N<sub>tot</sub> = N<sub>0</sub> + N<sub>0</sub> + 2 N<sub>&Phi;**

![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B100%7D%20%5Csmall%20%5Cbegin%7Baligned%7D%20H%28R%20%7C%20b_%7Bj%7D%3D0%29%20%26%3Dlog_2%28N_%7B0%7D&plus;N_%7B%5CPhi%7D%29%20-%5Cfrac%7B1%7D%7BN%7D%20%5C%5B%20%5Csum_%7Bb_%7Bj%7D%3D0%7D%20N_%7Br_%7Bi%7D%7D%20log_2%7BN_%7Br_%7Bi%7D%7D%7D%20&plus;%20%5Csum_%7Bb_%7Bj%7D%3D%5CPhi%7D%20%5Cfrac%7BN_%7Br_%7Bi%7D%7D%7D%7B2%7D%20log_2%7BN_%7Br_%7Bi%7D%7D%7D%20-%20%5Csum_%7Bb_%7Bj%7D%3D%5CPhi%7D%20%5Cfrac%7BN_%7Br_%7Bi%7D%7D%7D%7B2%7D%20%5C%5D%20%5C%5C%20IG%28R%2C%20b_%7Bj%7D%29%20%26%3D%20H%28R%29%20-%20%5Cfrac%7BN_0%20&plus;%20N_%5CPhi%7D%7BN_%7Btot%7D%7DH%28R%20%7C%20b_%7Bj%7D%3D0%29%20-%20%5Cfrac%7BN_1%20&plus;%20N_%5CPhi%7D%7BN_%7Btot%7D%7DH%28R%20%7C%20b_%7Bj%7D%3D1%29%20%5C%5C%20%26%3D%20log_2%20N_%7Btot%7D%20-%20%5Cfrac%7B1%7D%7BN_%7Btot%7D%7D%20%5C%5B%20%28N_0%20&plus;%20N_%5CPhi%29%20log_2%28N_%7B0%7D&plus;N_%7B%5CPhi%7D%29%20&plus;%20%28N_1%20&plus;%20N_%5CPhi%29%20log_2%28N_%7B1%7D&plus;N_%7B%5CPhi%7D%29%20&plus;%202%20N_%5CPhi%5C%5D%20%5Cend%7Baligned%7D)

<details>
<summary>See code</summary>
<br>LaTEX
<pre>
\begin{aligned} 
H(R | b_{j}=0) &= log_2(N_{0}+N_{\Phi}) -\frac{1}{N} \[ \sum_{b_{j}=0} N_{r_{i}} log_2{N_{r_{i}}} + \sum_{b_{j}=\Phi} \frac{N_{r_{i}}}{2} log_2{N_{r_{i}}} - \sum_{b_{j}=\Phi} \frac{N_{r_{i}}}{2} \] \\
IG(R, b_{j}) &= H(R) - \frac{N_0 + N_\Phi}{N_{tot}}H(R | b_{j}=0) - \frac{N_1 + N_\Phi}{N_{tot}}H(R | b_{j}=1) \\ 
&= log_2 N_{tot} - \frac{1}{N_{tot}} \[ (N_0 + N_\Phi) log_2(N_{0}+N_{\Phi}) +  (N_1 + N_\Phi) log_2(N_{1}+N_{\Phi}) + 2 N_\Phi\] 
\end{aligned}
</pre>
</details>

### IG[list, index]
```Mathematica
IG[list_,i_]:= With[{
  N0=Tr[2^Count[-1]/@Pick[list,Unitize@list[[All,i]],0]]//N,
  N1=Tr[2^Count[-1]/@Pick[list,Unitize@(1-list[[All,i]]),0]]//N,
  N2=((Tr[2^Count[-1]/@Pick[list,Unitize@(-1-list[[All,i]]),0]])//N)/2},
  log2[N0+N1+2. N2]-((N0+N2) log2[N0+N2]+(N1+N2) log2[N1+N2]+2. N2)/(N0+N1+2. N2)
]
```

## Build a decision tree with the best IG on each branch

### BuildTree1[rules, threshold]
```Mathematica
BuildTree1[rules_List,threshold_]:= With[{node=FindBestChoice1[rules,threshold]},AddNode1[rules,node,threshold]];
```
### AddNode[rules, node, threshold]
```Mathematica
AddNode1[rules_List,node_,_]/;node["leaf"]:= rules;
AddNode1[_,node_,threshold_]:=<|"choice"-> node["choosen"],"right"->BuildTree1[node["right"],threshold],"left"->BuildTree1[node["left"],threshold]|>;
```
### FindBestChoice[list]
```Mathematica
FindBestChoice1[li_List,threshold_]/; Length[li]<= threshold+1 || Length[li[[1]]]==1 := <|"leaf"->True|>;
FindBestChoice1[li_List,_]:= Module[{list=StripHeaders[li],ig,best,left0,right1},
  ig=Table[IG[list,i],{i,1.,Length[list[[1]]]}];
  best=Ordering[ig,-1][[1]]+1.;
  left0=RemoveColumn[#,best]&@Pick[li,Join[{1},Unitize@(1-li[[2;;-1,best]])],1];
  right1=RemoveColumn[#,best]&@Pick[li,Join[{1},Unitize@(li[[2;;-1,best]])],1];   
  <|"choosen"->li[[1,best]],"left"->left0,"right"-> right1,"leaf"->(Max[ig]<= 0||Length@left0<=1||Length@right1<=1)|>
]
```
The returned value is a tuple of:
* The best choice
* The sublist of rules for which b<sub>best choice</sub> = 0 or &Phi;
* The sublist of rules for which b<sub>best choice</sub> = 1 or &Phi;
* Is the current level a leaf ?
  * If <kbd>true</kbd> that means we cannot improve the sub-tree.
  * If <kbd>false</kbd> that means can continue

## Build a decision tree with the best IG on each level

### BuildTree2[rules, threshold]
```Mathematica
BuildTree2[DT_,choices_,level_,threshold_]:= With[{best=FindBestChoice2[Values[DT],threshold]},
AddNode2[DT,choices,best,level+1,threshold]];
```
### AddNode[DT, choices, best, level, threshold]
```Mathematica
AddNode2[DT_,choices_,-1,level_,_]:=<|"DT"->DT,"choices"->choices|>;
AddNode2[DT_,choices_,best_,level_,threshold_]:= BuildTree2[
  With[{keys=IntegerDigits[Range[0,(2^level)-1],2,level]},
    AssociationThread[keys,Table[RemoveColumn[#,best]&@Pick[DT[i[[;;-2]]],Join[{1},Unitize@(DT[i[[;;-2]]][[2;;-1,best]]-(1-i[[-1]]))],1],{i,keys}]]],
      Append[choices,DT[ConstantArray[0,level-1]][[1,best]]], level, threshold]
```
### FindBestChoice[list]
```Mathematica
FindBestChoice2[lst_List,threshold_]/; Mean[N[Length/@lst]]<=threshold+1|| Length[lst[[1,1]]]== 1:=-1
FindBestChoice2[lst_List,_]:= Module[{list=StripHeaders[#]&/@lst,ig,best},
  ig=Table[IG[#,i],{i,1.,Length[#[[1]]]}]&/@list;
  best=(With[{b=Ordering[#,-1][[1]]},{#[[b]],b+1}]&/@ig);
  best[[Ordering[best[[All,1]],-1][[1]],2]]
  ]
```
The returned value of this function is the index of the best choice to build the tree

## Build Tree

```Mathematica
BuildTree[rules_List,threshold_,"Branch"]:= BuildTree1[rules,threshold];
BuildTree[rules_List,threshold_,"Level"]:= With[{first=FindBestChoice1[rules,threshold]},
  BuildTree2[<|{0}->first["left"],{1}->first["right"]|>,{first["choosen"]},1,threshold]];
```

## Binary range with don't cares

### Example - [5:18]
> We need to divide in 5 ranges: {5, {6,7}, {8,15}, {16,17}, 18}


|         |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
|---------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 5       | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 1
| [6:7]   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 1 | __&Phi;__
| [8:15]  | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | __&Phi;__ | __&Phi;__ | __&Phi;__
| [16:17] | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | __&Phi;__
| 18      | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 1 | 0
|         |   |   |   |   |

### ProcessRange[{a,b}]
Return the list of the divisions with don't cares &Phi of the range [a,b]
```Mathematica 
ProcessRange[{0, 65535}] = GenerateRange[{0, 65535}];
ProcessRange[{a_Integer, b_Integer}] := GenerateRange /@ BinaryRange[a, b];
```

### GenerateRange[{a,b}]
Return a binary form of the range [a,b] with don't cares &Phi;
```Mathematica
GenerateRange[{x_Integer, y_Integer}] /; x == y := IntegerDigits[x, 2, 16];
GenerateRange[{x_Integer, y_Integer}] := ReplacePart[List /@ 
  Range[Position[IntegerDigits[BitXor[x, y], 2, 16], 1][[1, 1]], 16] ->"\[Phi]"][IntegerDigits[x, 2, 16]];
```

### DivideByPower[min,max]
Divide the interval according to the power of 2 -> [min,2<sup>n</sup>-1],[2<sup>n</sup>,2<sup>n+1</sup>-1]...[2<sup>m</sup>,max]
```Mathematica
DivideByPower[min_, max_] /; min > max := {};
DivideByPower[min_, max_] /; PrevPerfectPower2[max] < min := {{min, max}};
DivideByPower[min_, max_] := Prepend[DivideByPower[min, # - 1], {#, max}] &@ PrevPerfectPower2[max];
```

### DivideByEdge[min,max]
When the interval cannot be separate with power of 2 we need to separate it cleverer
```Mathematica
DivideEdge[{min_, max_}] /; min > max := {};
DivideEdge[{min_, max_}] /; EvenQ[max] := Prepend[DivideEdge[{min, max - 1}], {max, max}];
DivideEdge[{min_, max_}] /; min == max := {{min, max}};
DivideEdge[{min_, max_}] /; NextEdge[max] < min := DivideEdge[{min, max, 1}]
DivideEdge[{min_, max_}] := Prepend[DivideEdge[{min, # - 1}], {#, max}] &@NextEdge[max];

DivideEdge[{min_, max_, n_}] /; min > (max - n + 1) := Prepend[DivideEdge[{min, max - (n/2)}], {max - (n/2) + 1, max}];
DivideEdge[{min_, max_, n_}] := DivideEdge[{min, max, 2 n}];
```

### NextEdge[max]
Return the next valid {?,max} interval
```Mathematica
NextEdge[65535] = 0;
NextEdge[max_] := With[{num = IntegerDigits[max, 2, 16]}, 
   FromDigits[ReplacePart[List /@ Range[Position[num, 0][[-1, -1]] + 1, 16] -> 0][num], 2]];
```

### Helpful Functions
```Mathematica
PerfectPower2[int_Integer?Positive] := BitAnd[int, int - 1] == 0;
PerfectPower2[_] := False;
```
```Mathematica
PrevPerfectPower2[n_Integer?Positive] := 2^Floor[Log2[n]];
```

# Example

Rules| b<sub>1</sub> | b<sub>2</sub> | b<sub>3</sub> | b<sub>4</sub>
-- | - | - | -- | -
__R<sub>1</sub>__ | 1 | 0 | &Phi; | &Phi; 
__R<sub>2</sub>__ | 0 | 1 | &Phi; | &Phi; 
__R<sub>3</sub>__ | 1 | 1 | 0     | &Phi;
__R<sub>4</sub>__ | 0 | 1 | 0     | 0 

> Every * (don't care) duplicate the rule &rarr; R<sub>1</sub> & R<sub>2</sub> appear 4 times, R<sub>3</sub> twice and R<sub>4</sub> only once.
> In total it is like there are 11 rules

```Mathematica
H(Rule) = -(4/11 log2[4/11] + 4/11 log2[4/11] + 2/11 log2[2/11] + 1/11 log2[1/11]) = 1.82307
```

#### Conditional Entropy given b<sub>1</sub>
```Mathematica
H(Rule | b1=0) = - (4/5 log2[4/5] + 1/5 log2[1/5]) = 0.721928
H(Rule | b1=1) = - (4/6 log2[4/6] + 2/6 log2[2/6]) = 0.918296
H(Rule | b1) = 5/11 H(Rule | b1=0) + 6/11 H(Rule | b1=1) = 0.829038
IG(Rule, b1) = H(rule) - H(Rule | b1) = 0.994032
```

#### Conditional Entropy given b<sub>2</sub>
```Mathematica
H(Rule | b2=0) = - 4/4 log2[4/4] = 0
H(Rule | b2=1) = - (4/7 log2[4/7] + 2/7 log2[2/7] + 1/7 log2[1/7]) = 1.37878
H(Rule | b2) = 4/11 H(Rule | b2=0) + 7/11 H(Rule | b2=1) = 0.877408
IG(Rule, b2) = H(rule) - H(Rule | b2) = 0.945662
```

#### Conditional Entropy given b<sub>3</sub>
```Mathematica
H(Rule | b3=0) = - (2/7 log2[2/7] + 2/7 log2[2/7] + 2/7 log2[2/7] + 1/7 log2[1/7]) = 1.95021
H(Rule | b3=1) = - (2/4 log2[2/4] + 2/4 log2[2/4]) = 1
H(Rule | b3) = 7/11 H(Rule | b3=0) + 4/11 H(Rule | b3=1) = 1.60468
IG(Rule, b3) = H(rule) - H(Rule | b3) = 0.21839
```

#### Conditional Entropy given b<sub>4</sub>
```Mathematica
H(Rule | b4=0) = - (2/6 log2[2/6] + 2/6 log2[2/6] + 1/6 log2[1/6] + 1/6 log2[1/6]) = 1.9183
H(Rule | b4=1) = - (2/5 log2[2/5] + 2/5 log2[2/5] + 1/5 log2[1/5]) = 1.52193
H(Rule | b4) = 6/11 H(Rule | b1=0) + 5/11 H(Rule | b1=1) = 1.73813
IG(Rule, b4) = H(rule) - H(Rule | b1) = 0.0849413
```

> The best pick is the one with the highest Information Gain (IG) &rarr; b<sub>1</sub>

[![](https://mermaid.ink/img/eyJjb2RlIjoiZ3JhcGggVEQ7XG4gICAgQVtSMSxSMixSMyxSNF0tLT58YjE9MHxCW1IyLFI0XTtcbiAgICBBLS0-fGIxPTF8Q1tSMSxSMl07XG4iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ)](https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggVEQ7XG4gICAgQVtSMSxSMixSMyxSNF0tLT58YjE9MHxCW1IyLFI0XTtcbiAgICBBLS0-fGIxPTF8Q1tSMSxSMl07XG4iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ)

<details>
<summary>See code</summary>
<br>
<pre>
graph TD;
    A[R1,R2,R3,R4]-->|b1=0|B[R2,R4];
    A-->|b1=1|C[R1,R2];
</pre>
</details>

## Second Iteration 
### Left

Rules| b<sub>2</sub> | b<sub>3</sub> | b<sub>4</sub>
-- | - | -- | -
__R<sub>2</sub>__  | 1 | &Phi; | &Phi; 
__R<sub>4</sub>__  | 1 | 0  | 0 

```Mathematica
H(Rule) = -(4/5 log2[4/5] + 1/5 log2[1/5]) = 0.721928
```

#### Conditional Entropy given b<sub>2</sub>
```Mathematica
H(Rule | b2=0) = 0
H(Rule | b2=1) = - (4/5 log2[4/5] + 1/5 log2[1/5]) = 0.721928
H(Rule | b2) = 0.721928
IG(Rule, b2) = H(rule) - H(Rule | b2) = 0
```

#### Conditional Entropy given b<sub>3</sub> or b<sub>4</sub>
```Mathematica
H(Rule | b3=0) = - (2/3 log2[2/3] + 1/3 log2[1/3]) = 0.918296
H(Rule | b3=1) = 0
H(Rule | b3) = 3/5 H(Rule | b3=0) = 0.550978
IG(Rule, b3) = H(rule) - H(Rule | b3) = 0.170951
```

> The best pick is the one with the highest Information Gain (IG) &rarr; b<sub>3</sub>

### Rigth

Rules| b<sub>2</sub> | b<sub>3</sub> | b<sub>4</sub>
-- | - | -- | -
__R<sub>1</sub>__ | 0 | &Phi; | &Phi; 
__R<sub>3</sub>__ | 1 | 0  | &Phi;

```Mathematica
H(Rule) = -(4/6 log2[4/6] + 2/6 log2[2/6]) = 0.918296
```

#### Conditional Entropy given b<sub>2</sub>
```Mathematica
H(Rule | b2=0) = 0
H(Rule | b2=1) = 0
H(Rule | b2) = 0
IG(Rule, b2) = H(rule) - H(Rule | b2) = 0.918296
```

#### Conditional Entropy given b<sub>3</sub>
```Mathematica
H(Rule | b3=0) = - (2/4 log2[2/4] + 2/4 log2[2/4]) = 1
H(Rule | b3=1) = 0
H(Rule | b3) = 4/6 H(Rule | b3=0) = 0.666667
IG(Rule, b3) = H(rule) - H(Rule | b3) = 0.251629
```

#### Conditional Entropy given b<sub>4</sub>
```Mathematica
H(Rule | b3=0) = - (2/3 log2[2/3] + 1/3 log2[1/3]) = 0.918296
H(Rule | b3=1) = - (2/3 log2[2/3] + 1/3 log2[1/3]) = 0.918296
H(Rule | b3) = 0.918296
IG(Rule, b3) = H(rule) - H(Rule | b3) = 0
```
> The best pick is the one with the highest Information Gain (IG) &rarr; b<sub>2</sub>

[![](https://mermaid.ink/img/eyJjb2RlIjoiZ3JhcGggVEQ7XG4gICAgQVtSMSxSMixSMyxSNF0tLT58YjE9MHxCW1IyLFI0XTtcbiAgICBBLS0-fGIxPTF8Q1tSMSxSMl07XG4gICAgQy0tPnxiMj0wfERbUjFdXG4gICAgQy0tPnxiMj0xfEVbUjNdXG4gICAgQi0tPnxiMz0wfEZbUjIsUjRdXG4gICAgQi0tPnxiMz0xfEdbUjJdIiwibWVybWFpZCI6eyJ0aGVtZSI6ImRlZmF1bHQifSwidXBkYXRlRWRpdG9yIjpmYWxzZX0)](https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiZ3JhcGggVEQ7XG4gICAgQVtSMSxSMixSMyxSNF0tLT58YjE9MHxCW1IyLFI0XTtcbiAgICBBLS0-fGIxPTF8Q1tSMSxSMl07XG4gICAgQy0tPnxiMj0wfERbUjFdXG4gICAgQy0tPnxiMj0xfEVbUjNdXG4gICAgQi0tPnxiMz0wfEZbUjIsUjRdXG4gICAgQi0tPnxiMz0xfEdbUjJdIiwibWVybWFpZCI6eyJ0aGVtZSI6ImRlZmF1bHQifSwidXBkYXRlRWRpdG9yIjpmYWxzZX0)

<details>
<summary>See code</summary>
<br>
<pre>
graph TD;
    A[R1,R2,R3,R4]-->|b1=0|B[R2,R4];
    A-->|b1=1|C[R1,R2];
    C-->|b2=0|D[R1]
    C-->|b2=1|E[R3]
    B-->|b3=0|F[R2,R4]
    B-->|b3=1|G[R2]
</pre>
</details>

## Third Iteration
Because we get only one possible rule for the right side of the tree we know these are __leafs__!  
Same story for the right side of the left sub-tree. let's check for its left side:


Rules| b<sub>2</sub> |  b<sub>4</sub>
-- | -- | -
__R<sub>2</sub>__  | 1  | &Phi; 
__R<sub>4</sub>__  | 1  | 0 

> As before we get that the best pick is b<sub>4</sub> (check calculation yourself!)  
> Note that two rules are still there on the last level (R<sub>2</sub> and R<sub>4</sub>). However, because they have the same probabilty to be choosen we let them as they are __(to be decided later...)__

Finaly every nodes we reached are leafs and the decision tree looks as followed:

[![](https://mermaid.ink/img/eyJjb2RlIjoiXG5ncmFwaCBURDtcbiAgICBBW1IxLFIyLFIzLFI0XS0tPnxiMT0wfEJbUjIsUjRdO1xuICAgIEEtLT58YjE9MXxDW1IxLFIyXTtcbiAgICBCLS0-fGIzPTB8RltSMixSNF1cbiAgICBCLS0-fGIzPTF8R1tSMl1cbiAgICBGLS0-fGI0PTB8SFtSMixSNF1cbiAgICBGLS0-fGI0PTF8SVtSMl1cbiAgICBDLS0-fGIyPTB8RFtSMV1cbiAgICBDLS0-fGIyPTF8RVtSM10iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ)](https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiXG5ncmFwaCBURDtcbiAgICBBW1IxLFIyLFIzLFI0XS0tPnxiMT0wfEJbUjIsUjRdO1xuICAgIEEtLT58YjE9MXxDW1IxLFIyXTtcbiAgICBCLS0-fGIzPTB8RltSMixSNF1cbiAgICBCLS0-fGIzPTF8R1tSMl1cbiAgICBGLS0-fGI0PTB8SFtSMixSNF1cbiAgICBGLS0-fGI0PTF8SVtSMl1cbiAgICBDLS0-fGIyPTB8RFtSMV1cbiAgICBDLS0-fGIyPTF8RVtSM10iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ)

<details>
<summary>See code</summary>
<br>
<pre>
graph TD;
    A[R1,R2,R3,R4]-->|b1=0|B[R2,R4];
    A-->|b1=1|C[R1,R2];
    B-->|b3=0|F[R2,R4]
    B-->|b3=1|G[R2]
    F-->|b4=0|H[R2,R4]
    F-->|b4=1|I[R2]
    C-->|b2=0|D[R1]
    C-->|b2=1|E[R3]
</pre>
</details>

<br>

---

# Usefull Functions in Mathematica

## Add Column to a matrix
```Mathematica

```

## Remove Column from matrix
Remove 2nd column of random matrix of dimensions 6x4 &rarr; result dimensions: 6x3 
```Mathematica
(m = RandomInteger[9, {6, 4}]) // MatrixForm
Drop[m, None, {2}] // MatrixForm
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
<br>

---

# References
* BGU: [https://bgu.ac.il](https://in.bgu.ac.il/en/Pages/default.aspx)   
* Flowcharts: [Mermaid](https://mermaid-js.github.io/mermaid/#/flowchart?id=flowcharts-basic-syntax)

<!--https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiXG5ncmFwaCBURDtcbiAgICBBW1IxLFIyLFIzLFI0XS0tPnxiMT0wfEJbUjIsUjRdO1xuICAgIEEtLT58YjE9MXxDW1IxLFIyXTtcbiAgICBDLS0-fGIyPTB8RFtSMV1cbiAgICBDLS0-fGIyPTF8RVtSM11cbiAgICBCLS0-fGIzPTB8RltSMixSNF1cbiAgICBCLS0-fGIzPTF8R1tSMl0iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ-->

<!--https://www.codecogs.com/latex/eqneditor.php-->
