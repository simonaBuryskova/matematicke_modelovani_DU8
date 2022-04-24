(* ::Package:: *)

g1[x1_, x2_] := x1^2 + ((5/4)*(x2 - 3) - Sqrt[Abs[x1]])^2;
g2[y1_, y2_] := (2*(y1 - 3)^2 + 2*(y2 - 4)^2)^3 - 40*(y1 - 3)^2*(y2 - 4)^2;


(*Pomoc\[IAcute] vestav\[EHacek]n\[YAcute]ch funkc\[IAcute]*)
distance = (x1 - y1)^2 + (x2 - y2)^2;
min = FindMinimum[{distance, g1[x1, x2] <= 1 && g2[y1, y2] <= 1}, {x1, x2, y1,y2}]
delkaMostu = Sqrt[First[min]];
(* Graf ostrov\[URing] 2D, obrys*)
o1 = ContourPlot[g1[x1,x2]==1, {x1,-1,1},{x2,1,5}];
o2 = ContourPlot[g2[y1,y2]==1, {y1,2,4},{y2,3,5}];
most = Graphics[Line[{{0.9890878189073022,3.6777616483796556},{2.1115475185999135,3.483330619802231}}]];
mostNewton = Graphics[Line[{{0.75,-2.6},{0.75,-2.6}}], Green];
ostrovy = Show[o1,o2,most,PlotRange-> All]


(*Newton*)
f1 = ImplicitRegion[x1^2 + ((5/4)*(x2 - 3) - Sqrt[Sqrt[x1^2+0.000001^2]])^2<=1, {x1,x2}];
f2 = ImplicitRegion[(2*(y1 - 3)^2 + 2*(y2 - 4)^2)^3 - 40*(y1 - 3)^2*(y2 - 4)^2<=1, {y1,y2}];
Show[Region[f1],Region[f2],PlotRange-> All ]
vzdalenost[{x1_, y1_, x2_,y2_}] := (x1 - y1)^2 + (x2 - y2)^2  
grad[{x01_,x02_,y01_,y02_}]:=D[vzdalenost,{{x1,x2,y1,y2}}]/.{x1->x01,x2->x02,y1->y01,y2->y02}
J=D[grad[{x1,x2,y1,y2}],{{x1,x2,y1,y2}}];
Newton[{xI1_,xI2_} /; xI1\[Element] f1 && xI2\[Element]f2]:=NestWhile[LinearSolve[J,-grad[{#1,#2}]]+{#1,#2}&,{xI1,xI2},Norm[{#1,#2}]>0.000001&,1,10]


Newton[{0.7,3.5,2,3.5}]
