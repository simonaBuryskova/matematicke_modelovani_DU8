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



(*Kontrola minima pomoc\[IAcute] vestav\[EHacek]n\[YAcute]ch funkc\[IAcute]*)

distance = (x1 - y1)^2 + (x2 - y2)^2;
min = FindMinimum[{distance, g1[x1, x2] <= 1 && g2[y1, y2] <= 1}, {x1, x2, y1,y2}];
delkaMostu = Sqrt[First[min]];

(* Graf ostrov\[URing] 2D*)

o1 = ContourPlot[g1[x1,x2]==1, {x1,-1,1},{x2,1,5}];
o2 = ContourPlot[g2[y1,y2]==1, {y1,2,4},{y2,3,5}];
most = Graphics[Line[{{0.9890878189073022,3.6777616483796556},{2.1115475185999135,3.483330619802231}}]];
ostrovy = Show[o1,o2,most, PlotRange-> All];


(* Newton *)
(* Define the equations *)
f = {g1,g2};

jacobian = D[f,{x1,x2,y1,y2}];

xInitial={1,1,1,1};
\[Tau]a = 10^(-3);
\[Tau]r = 10^(-3);
totalTol = \[Tau]a + \[Tau]r * EuclideanDistance[f  /. x1->xInitial[[1]] /. x2->xInitial[[2]] /. y1->xInitial[[3]] /. y2->xInitial[[4]] ,0];

(* Newton: Dostane xn, vrati xn+1 *)
stepNewton[xn_] := Module[{xnp1,deltanp1,eq,sol},
	xnp1 = {a,b,c,d};
	deltanp1 = xnp1 - xn;
	eq = jacobian . deltanp1 + f  /. x1->xn[[1]] /. x2->xn[[2]] /. y1->xn[[3]] /. y2->xn[[4]];
	sol = NSolve[eq == 0,xnp1][[1]];
	xnp1 /. sol]

(* Ukazka Newtona *)
solNewton = NestWhileList[
	stepNewton,
	xInitial,
	EuclideanDistance[f  /. x1->#[[1]] /. x2->#[[2]] /. y1->#[[3]] /. y2->#[[4]] &,0] > totalTol, (* check the size of the solution *)
	10^4 (* Pokud jsi nedokonvergoval, zastav po tolika iteracich *)
];
Last[solNewton] (* Nejlepsi kam dokonvergoval *)



