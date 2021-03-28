# Decision-trees
**First assignement in course Machine Learning 2021B at [BGU](https://in.bgu.ac.il/en/Pages/default.aspx)**  :sparkles:

## Formulas

> * Entropy: H(X) = - Σx P(X=x) log<sub>2</sub>(P(X=x))
> * Conditional Entropy: H(Y | X) = Σx P(X = x) H(Y | X = x)
> * Information Gain: IG (Y, X) = H(Y) - H(Y | X)

For entropy, the smaller the better (big entropy -> big uncertainty). For IG, the higher the better. 
> H(x) = 0 &rarr; no uncertainty

## Remove Column from matrix
Remove 2nd column of random matrix of dimensions 6x4 &rarr; result dimensions: 6x3 
```Mathematica
(m = RandomInteger[9, {6, 4}]) // MatrixForm
Drop[m, None, {2}] // MatrixForm
```

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

Rules| b<sub>1</sub> | b<sub>2</sub> | b<sub>3</sub> | b<sub>4</sub>
-- | - | - | -- | -
R<sub>1</sub> | 1 | 0 | &Phi; | &Phi; 
R<sub>2</sub> | 0 | 1 | &Phi; | &Phi; 
R<sub>3</sub> | 1 | 1 | 0  | &Phi;
R<sub>4</sub> | 0 | 1 | 0  | 0 

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

![graph1](https://mermaid.ink/img/eyJjb2RlIjoiXG5ncmFwaCBURDtcbiAgICBBW1IxLFIyLFIzLFI0XS0tPnxiMT0wfEJbUjIsUjRdO1xuICAgIEEtLT58YjE9MXxDW1IxLFIyXTtcbiIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In0sInVwZGF0ZUVkaXRvciI6ZmFsc2V9)

<details>
<summary>See code</summary>
<br>
<pre>
graph TD;
    A[R1,R2,R3,R4]-->|b1=0|B[R2,R4];
    A-->|b1=1|B[R1,R2];
</pre>
</details>

## Second Iteration (left)

Rules| b<sub>2</sub> | b<sub>3</sub> | b<sub>4</sub>
-- | - | -- | -
R<sub>2</sub>  | 1 | &Phi; | &Phi; 
R<sub>4</sub>  | 1 | 0  | 0 

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
H(Rule | b3) = 2/3 H(Rule | b3=0) = 0.612197
IG(Rule, b3) = H(rule) - H(Rule | b3) = 0.109731
```

## Second Iteration (rigth)

Rules| b<sub>2</sub> | b<sub>3</sub> | b<sub>4</sub>
-- | - | -- | -
R<sub>1</sub> | 0 | &Phi; | &Phi; 
R<sub>3</sub> | 1 | 0  | &Phi;

```Mathematica
H(Rule) = -(4/5 log2[4/5] + 1/5 log2[1/5]) = 0.721928
```

#### Conditional Entropy given b<sub>2</sub>
```Mathematica
H(Rule | b2=0) = 0
H(Rule | b2=1) = 0
H(Rule | b2) = 0
IG(Rule, b2) = H(rule) - H(Rule | b2) = 0.721928
```

#### Conditional Entropy given b<sub>3</sub>
```Mathematica
H(Rule | b3=0) = - (2/3 log2[2/3] + 1/3 log2[1/3]) = 0.918296
H(Rule | b3=1) = 0
H(Rule | b3) = 2/3 H(Rule | b3=0) = 0.612197
IG(Rule, b3) = H(rule) - H(Rule | b3) = 0.109731
```

#### Conditional Entropy given b<sub>4</sub>
```Mathematica
H(Rule | b3=0) = - (2/3 log2[2/3] + 1/3 log2[1/3]) = 0.918296
H(Rule | b3=1) = - (2/3 log2[2/3] + 1/3 log2[1/3]) = 0.918296
H(Rule | b3) = 0.918296
IG(Rule, b3) = H(rule) - H(Rule | b3) = -0.196368
```

> Finaly we get: 

![Graph2](https://mermaid.ink/img/eyJjb2RlIjoiXG5ncmFwaCBURDtcbiAgICBBW1IxLFIyLFIzLFI0XS0tPnxiMT0wfEJbUjIsUjRdO1xuICAgIEEtLT58YjE9MXxDW1IxLFIyXTtcbiAgICBDLS0-fGIyPTB8RFtSMV1cbiAgICBDLS0-fGIyPTF8RVtSM11cbiAgICBCLS0-fGIzPTB8RltSMixSNF1cbiAgICBCLS0-fGIzPTF8R1tSMl0iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ)

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

<br>

---

# References
* BGU: [https://bgu.ac.il](https://in.bgu.ac.il/en/Pages/default.aspx)   
* Graphs: [Mermaid](https://mermaid-js.github.io/mermaid/#/flowchart?id=flowcharts-basic-syntax)

<!--https://mermaid-js.github.io/mermaid-live-editor/#/edit/eyJjb2RlIjoiXG5ncmFwaCBURDtcbiAgICBBW1IxLFIyLFIzLFI0XS0tPnxiMT0wfEJbUjIsUjRdO1xuICAgIEEtLT58YjE9MXxDW1IxLFIyXTtcbiAgICBDLS0-fGIyPTB8RFtSMV1cbiAgICBDLS0-fGIyPTF8RVtSM11cbiAgICBCLS0-fGIzPTB8RltSMixSNF1cbiAgICBCLS0-fGIzPTF8R1tSMl0iLCJtZXJtYWlkIjp7InRoZW1lIjoiZGVmYXVsdCJ9LCJ1cGRhdGVFZGl0b3IiOmZhbHNlfQ-->
