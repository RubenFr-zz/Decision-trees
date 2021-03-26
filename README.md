# Decision-trees
First assignement in course Machine Learning 2021B

## Extracting rules in Rule.tsv

'''mathematica
data = StringSplit[
   StringRiffle[
    Import[Rule.tsv, "\n"];
data = StringSplit[#, {".", ":"}] & /@ 
     StringSplit[#, {"@", " 0x", "/0x", " ", "/"}] & /@ data;

sourceMask = ToExpression /@ Flatten@data[[All, 2]];
Flatten@IntegerDigits[#, 2, 8] & /@ ToExpression /@ data[[All, 1]];
sourceIP = 
  Join[Take[%[[#]], sourceMask[[#]]], 
     Table[-1, {32 - sourceMask[[#]]}]] & /@ Range[1, Length[%]];

destinationMask = ToExpression /@ Flatten@data[[All, 4]];
Flatten@IntegerDigits[#, 2, 8] & /@ ToExpression /@ data[[All, 3]];
destinationIP = 
  Join[Take[%[[#]], destinationMask[[#]]], 
     Table[-1, {k, 32 - destinationMask[[#]]}]] & /@ Range[Length[%]];

sourcePort = data[[All, 5]];
destinationPort = data[[All, 6]];
protocol1 = FromDigits[#, 16] & /@ Flatten@data[[All, 7]];
protocol2 = FromDigits[#, 16] & /@ Flatten@data[[All, 8]];

rules = Flatten /@ 
   Transpose@
    List[sourceIP, destinationIP, sourcePort, destinationPort, 
     protocol1, protocol2];
'''
