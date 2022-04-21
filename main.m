(* ::Package:: *)

g1[x1_, x2_] := x1^2 + ((5/4)*(x2 - 3) - Sqrt[Abs[x1]])^2;
g2[y1_, y2_] := (2*(y1 - 3)^2 + 2*(y2 - 4)^2)^3 - 40*(y1 - 3)^2*(y2 - 4)^2;

g1Plot = Plot3D[g1[x1, x2], {x1, -1, 1}, {x2, 1, 5}, 
   RegionFunction -> 
    Function[{x1, x2, z}, 
     0 < x1^2 + ((5/4)*(x2 - 3) - Sqrt[Abs[x1]])^2 <= 1], Mesh -> None, 
   Filling -> Top, FillingStyle -> Orange];

g2Plot = Plot3D[g2[y1, y2], {y1, 2, 4}, {y2, 3, 5}, 
   RegionFunction -> 
    Function[{y1, y2, 
      z}, -5 < (2*(y1 - 3)^2 + 2*(y2 - 4)^2)^3 - 40*(y1 - 3)^2*(y2 - 4)^2 <= 
      1], Mesh -> None, Filling -> Top, FillingStyle -> Orange];

(*Graf ostrov\[URing] 3D*)
Show[g1Plot, g2Plot, PlotRange -> All];



(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(*Kontrola minima pomoc\[IAcute] vestav\[EHacek]n\[YAcute]ch funkc\[IAcute]*)

distance = (x1 - y1)^2 + (x2 - y2)^2;
min = FindMinimum[{distance, g1[x1, x2] <= 1 && g2[y1, y2] <= 1}, {x1, x2, y1,y2}]
delkaMostu = Sqrt[First[min]];

(* Graf ostrov\[URing] 2D*)

o1 = ContourPlot[g1[x1,x2]==1, {x1,-1,1},{x2,1,5}];
o2 = ContourPlot[g2[y1,y2]==1, {y1,2,4},{y2,3,5}];
most = Graphics[Line[{{0.9890878189073022,3.6777616483796556},{2.1115475185999135,3.483330619802231}}]];
ostrovy = Show[o1,o2,most, PlotRange-> All]


(* Define the equations *)
jacobian = D[distance,{{x1,x2,y1,y2}}]

xInitial={0.9890878189073022,3.6777616483796556,2.1115475185999135,3.483330619802231};
\[Tau]a = 10^(-3);
\[Tau]r = 10^(-3);
totalTol = \[Tau]a + \[Tau]r * EuclideanDistance[distance  /. x1->xInitial[[1]] /. x2->xInitial[[2]] /. y1->xInitial[[3]] /. y2->xInitial[[4]] ,0];

(* Newton: Dostane xn, vrati xn+1 *)
stepNewton[xn_] := Module[{xnp1,eq,sol},
	xnp1 = {a,b,c,d};
	eq =  (distance/jacobian)+xnp1-xn /. x1->xn[[1]] /. x2->xn[[2]] /. y1->xn[[3]] /. y2->xn[[4]];
	sol = NSolve[eq == 0,xnp1][[1]];
	xnp1 /. sol]

(* Ukazka Newtona *)
solNewton = NestWhileList[
	stepNewton,
	{0.9890878189073022,3.6777616483796556,2.1115475185999135,3.483330619802231},
	EuclideanDistance[distance  /. x1->#[[1]] /. x2->#[[2]] /. y1->#[[3]] /. y2->#[[4]] &,0] > totalTol, (* check the size of the solution *)
	10^4 (* Pokud jsi nedokonvergoval, zastav po tolika iteracich *)
];
Last[solNewton] (* Nejlepsi kam dokonvergoval *)





	xn = {-2.0416666666666665,-8.9,0.041666666666666664,19.9}
	xnp1 = {a,b,c,d};
	eq =  (distance/jacobian)+xnp1-xn /. x1->xn[[1]] /. x2->xn[[2]] /. y1->xn[[3]] /. y2->xn[[4]];
	sol = NSolve[eq == 0,xnp1][[1]]
	xnp1 /. sol
	(distance/jacobian)






grad[{x01_,x02_,y01_,y02_}]:=D[distance,{{x1,x2,y1,y2}}]/.{x1->x01,x2->x02,y1->y01,y2->y02}
J=D[grad[{x1,x2,y1,y2}],{{x1,x2,y1,y2}}]
Newton[x0_]:=NestWhile[LinearSolve[J,-grad[#]]+#&,x0,Norm[#]>0.000001&,1,10];


Newton[{1,0.8,0.5,-6}]
