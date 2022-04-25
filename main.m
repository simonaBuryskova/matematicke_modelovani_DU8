(* ::Package:: *)

(*Pomoc\[IAcute] vestav\[EHacek]n\[YAcute]ch funkc\[IAcute]*)
gg1[x1_, x2_] := x1^2 + ((5/4)*(x2 - 3) - Sqrt[Sqrt[x1^2 + 0.0001^2]])^2;
gg2[y1_, y2_] := (2*(y1 - 3)^2 + 2*(y2 - 4)^2)^3 - 40*(y1 - 3)^2*(y2 - 4)^2;
distance = (x1 - y1)^2 + (x2 - y2)^2;
min = FindMinimum[{distance, gg1[x1, x2] <= 1 && gg2[y1, y2] <= 1}, {x1, x2, y1,y2}]
delkaMostu = Sqrt[First[min]];
souradniceMostu = {{Last[min[[2]][[1]]],Last[min[[2]][[2]]]},{Last[min[[2]][[3]]],Last[min[[2]][[4]]]}};
(* Graf ostrov\[URing] 2D, obrys*)
o1 = ImplicitRegion[x1^2 + ((5/4)*(x2 - 3) - Sqrt[Abs[x1]])^2<=1, {x1,x2}];
o2 = ImplicitRegion[(2*(y1 - 3)^2 + 2*(y2 - 4)^2)^3 - 40*(y1 - 3)^2*(y2 - 4)^2<=1, {y1,y2}];
mostFindMin = Graphics[{Line[souradniceMostu],Text[delkaMostu, {1.5, 3.8}]}];


(*Newton constrained*)
g1= x1^2 + ((5/4)*(x2 - 3) - Sqrt[Sqrt[x1^2+0.0001^2]])^2;
g2= (2*(y1 - 3)^2 + 2*(y2 - 4)^2)^3 - 40*(y1 - 3)^2*(y2 - 4)^2;
distance = (x1 - y1)^2 + (x2 - y2)^2;
Linequality=\[Piecewise]{
 {\[Lambda] p+\[Rho]/2 p^2, \[Lambda]+\[Rho] p<0},
 {-(1/(2\[Rho])) \[Lambda]^2, \[Lambda]+\[Rho] p<=\[Rho] && \[Lambda]+\[Rho] p>=0}, 
 {\[Lambda](p-1)+\[Rho]/2 (p-1)^2, \[Lambda]+\[Rho] p>\[Rho]}
};
L=distance+ReplaceAll[ReplaceAll[Linequality,p->g1],\[Lambda]->\[Lambda]1]+ReplaceAll[ReplaceAll[Linequality,p->g2],\[Lambda]->\[Lambda]2];


\[Rho]=0.5;
prvniDerivaceL = D[L,{{x1,x2,y1,y2,\[Lambda]1,\[Lambda]2}}];
druhaDerivaceL = D[L,{{x1,x2,y1,y2,\[Lambda]1,\[Lambda]2},2}];
x0={0.7,3.5,2,3.5,0.1,0.1};
\[Tau]a = 10^(-3);
\[Tau]r = 10^(-3);
totalTol = \[Tau]a + \[Tau]r * EuclideanDistance[prvniDerivaceL /. x1->x0[[1]] /. x2->x0[[2]] /. y1->x0[[3]] /. y2->x0[[4]] /. \[Lambda]1->x0[[5]] /. \[Lambda]2->x0[[6]],0];
newton3[xn_] := Module[{xnp1},
xnp1=-prvniDerivaceL . Inverse[druhaDerivaceL]+xn /. x1->xn[[1]] /. x2->xn[[2]] /. y1->xn[[3]] /. y2->xn[[4]] /. \[Lambda]1->xn[[5]] /. \[Lambda]2->xn[[6]]]


reseni = NestWhileList[newton3, x0, EuclideanDistance[prvniDerivaceL /. x1->#[[1]] /. x2->#[[2]] /. y1->#[[3]] /. y2->#[[4]] /. \[Lambda]1->#[[5]] /. \[Lambda]2->#[[6]] &,0] > totalTol,15]


res=Last[reseni]
mostRucne=Graphics[{Thick,Blue,{Line[{{res[[1]],res[[2]]},{res[[3]],res[[4]]}}],Text[Style[delkaMostRucne, Blue], {1.5, 3.4}]}}];
delkaMostRucne = EuclideanDistance[{res[[1]],res[[2]]},{res[[3]],res[[4]]}];
ostrovy = Show[Region[o1],Region[o2],mostRucne,mostFindMin,PlotRange-> All]



