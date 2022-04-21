(* ::Package:: *)

(* ::Section:: *)
(*Most mezi ostrovy (\[CapitalUAcute]kol 1)*)


(* ::Input:: *)
(**)
(*g1 = x1^2+((5/4)*(x2-3)-Sqrt[Abs[x1]])^2;*)
(*g2 = (2*(y1-3)^2+2*(y2-4)^2)^3-40*(y1-3)^2*(y2-4)^2;*)
(**)
(**)
(*g1Plot = Plot3D[g1,{x1,-1,1},{x2,1,5},RegionFunction ->Function[{x1,x2,z},0< x1^2+((5/4)*(x2-3)-Sqrt[Abs[x1]])^2<=1], Mesh-> None, Filling->Top, FillingStyle->Orange];*)
(**)
(*g2Plot = Plot3D[g2,{y1,2,4},{y2,3,5},RegionFunction ->Function[{y1,y2,z},-5< (2*(y1-3)^2+2*(y2-4)^2)^3-40*(y1-3)^2*(y2-4)^2<=1], Mesh-> None, Filling->Top, FillingStyle->Orange];*)
(**)
(**)
(**)
(*(*Graf ostrov\[URing]*)*)
(**)
(*Show[g1Plot,g2Plot, PlotRange->All]*)


(* ::Input:: *)
(**)
(**)
(*(*Kontrola minima pomoc\[IAcute] vestav\[EHacek]n\[YAcute]ch funkc\[IAcute]*)*)


(* ::Input:: *)
(*distance =Sqrt[(x1-y1)^2+(x2-y2)^2] ;*)
(**)
(*FindMinimum[{distance && (x1-y1)>0 && (x2-y2)>0 && g1[x1,x2]<=1&& g2[y1,y2]<=1 },{x1,x2,y1,y2}]*)



