(* Author: Dalibor Djukanovic *)
(* Date: 14.10.15 *)
(* Version: 0.1 *) 


BeginPackage["qct`"]

Print["QuarkContractionTool (QCT) "]

(* Nice Formatting *)


Format[TForm[(H_)[CI[{a_,b_}],SI[{c_,d_}]]],TraditionalForm]:= 
 DisplayForm[
  RowBox[{SubsuperscriptBox[H, RowBox[{c, d}], 
     RowBox[{a, b}]]}]]

Format[TForm[(H_)[CI[{a_,b_}]]],
  TraditionalForm] := 
 DisplayForm[
  SuperscriptBox[H, RowBox[{a, b}]]]

Format[TForm[(H_)[SI[{c_,d_}]]],TraditionalForm]:= 
 DisplayForm[
  SubscriptBox[H, RowBox[{c, d}]]]


Format[DE[{ferm_, ferm_}, {x_ , y_} ] [CI[ {ci1_, ci2_}],SI[ {si1_, si2_}]], 
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[S, RowBox[{si1, si2}], 
     RowBox[{ferm, ",", RowBox[{ci1, ci2}]}]], "(", x, ",", y, ")"}]]

Format[DEInverse[{ferm_, ferm_}, {x_ , y_} ] [CI[ {ci1_, ci2_}],SI[ {si1_, si2_}]], 
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[S, RowBox[{si1, si2}], 
     RowBox[{"-1",ferm, ",", RowBox[{ci1, ci2}]}]], "(", x, ",", y, ")"}]]

Format[DE[{ferm_, ferm_}, {x_ , y_} ], 
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SuperscriptBox[S,  
     ferm], "(", x, ",", y, ")"}]]

MakeBoxes[(h_)[SI[{a_, b_}]], TraditionalForm] := 
 SubscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]

MakeBoxes[(h_)[CI[{a_, b_}]], TraditionalForm] := 
 SuperscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]


MakeBoxes[h_[CI[{a_,b_}],SI[{mu_,nu_}]], TraditionalForm] /; (h=!= DE || h =!=
DEInverse):= 
 SubsuperscriptBox[ToBoxes[HoldForm[h],TraditionalForm], RowBox[{ToString[mu], "",
ToString[nu]}],RowBox[{ToString[a],"", ToString[b]}]]

MakeBoxes[h_[SI[{mu_,nu_}],CI[{a_,b_}]], TraditionalForm] /; (h=!= DE || h =!=
DEInverse):= 
 SubsuperscriptBox[ToBoxes[HoldForm[h],TraditionalForm], RowBox[{ToString[mu], "",
ToString[nu]}],RowBox[{ToString[a]," ", ToString[b]}]]


Format[Field[u_, a_, \[Alpha]_, x_], TraditionalForm] := 
 DisplayForm[RowBox[{SubsuperscriptBox[u, \[Alpha], a], "(", x, ")"}]]

Format[FieldB[u_, a_, \[Alpha]_, x_], TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[OverBar[u], \[Alpha], a], "(", x, ")"}]]

Format[NM[a__], TraditionalForm] := DisplayForm[HoldForm[Times[a]]]



Unprotect[NonCommutativeMultiply];
Format[NonCommutativeMultiply[a_, b__], TeXForm] := 
  HoldForm[Times[a, b]];
Protect[NonCommutativeMultiply];

Unprotect[Dot]
Format[Dot[a___, b___], CForm] := HoldForm[Times[a*b]]
Protect[Dot]
Unprotect[Transpose];
Format[Transpose[a__], CForm] := transpose[a];
Protect[Transpose];
Format[quarkContract[{num1_, num2_}, A_, 
   B_], CForm] :=   ToExpression[
      "quarkContract" <> ToString[num1] <> ToString[num2]][A,B]

Format[DD[a_, b_], TraditionalForm] := 
 DisplayForm[SubscriptBox["\[Delta]", RowBox[{a, b}]]]

Format[quarkContract[{num1_, num2_}, A_, 
   B_][CI[{cind1_, cind2_}], SI[{lind1_, lind2_}]], TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[
     ToExpression[
      "quarkContract" <> ToString[num1] <> ToString[num2]], 
     RowBox[{lind1, lind2}], RowBox[{cind1, cind2}]], "(", A, ",", B, 
    ")"}]]

Format[quarkContract[{num1_, num2_}, A_, 
   B_] , TraditionalForm] := 
 DisplayForm[
  RowBox[{
     ToExpression[
      "quarkContract" <> ToString[num1] <> ToString[num2]], 
      "(", A, ",", B, 
    ")"}]]


Format[Eps[a_, b_, c_], TraditionalForm] := 
 DisplayForm[SuperscriptBox[\[Epsilon], RowBox[{a, b, c}]]]

Format[MyProd[
   f_, {u_, u_}, {cind1_, cind2_}, {lind1_, lind2_}, {x_, y_}], 
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[f, RowBox[{cind1, cind2}], 
     RowBox[{lind1, lind2}]], "(", x, ",", y, ")"}]]

(* Declaration of externally accessible functions *)

GraphWC::usage="Visualization of Wick contracted expressions."
adj::usage="Adjoint of the argument";
G5::usage="gamma_5"
ToTeXForm::usage="Write expressions in TeXForm"
DEProject::usage="DEProject[{type,type},{x,y}][CI[__],SI[__]] projects out the
propagator of type DE[{type,type},{x,y}]"
trace::usage="trace QDP trace function"
traceSpin::usage="traceSpin -  QDP trace function"
traceColor::usage="traceColor -  QDP trace function"
colorTranspose::usage="colorTranspose - QDP color transpose function"
transposeSpin::usage="transposeSpin - QDP color transpose function"
WickContract::usage="WickContract[x] computes Wick contractions of all quantum fields. At the moment the fields are assumed to be anticommuting."
Contract::usage="Contract[x] performs contractions over summed indices."
Uncontract::usage="Uncontract[x] is the inverse of Contract. Note that also
color independent objects will artificially pick up color indices. "
QuarkContract::usage="QuarkContract[x] computes quark contractions and returns
expressions suitable for QDP++."
ToQDP::usage="ToQDP[x,reps:{}, fn:\"stdout\" ] writes the CForm of x to file fn using replacements given in reps."
TForm::usage="Helper function to enable TeXForm of color and spin objects."
Canonicalize::usage="EXPERIMENTAL: Canonicalize[expr,y,dir] tries to generate canonical
expressions, especially rewriting shifts in direction +/- dir to QDP shifts."
shift::usage="QDP shift function."
FORWARD
BACKWARD
(*GenerateSource::usage="GenerateSource[x,reps:{}, fn:\"stdout\" ] writes the
CForm of x to file fn using replacements given in reps."*)

SetAttributes[NM, {OneIdentity, Flat}]

NMM[___, Field[__], Field[__], ___] := 0
NMM[___, FieldB[__], FieldB[__], __] := 0
NM[a___, -c__, b___] := -NM[a, c, b]


Begin[ "Private`"]

LegacyWickContract[expr_] := 
 Block[{}, 
  If[Head[Distribute[expr]] === Plus, LegacyWickContract2[#] & /@Distribute[expr], 
   LegacyWickContract2[expr]]]

QF[expr_] := (Head[expr] === Field) || (Head[expr] === FieldB)

LegacyWickContract2[expr_] := 
 Block[{res}, 
  res=expr/.NonCommutativeMultiply->NM;
  NM[NM @@ (DeleteCases[res, _?QF, \[Infinity]]), 
   LegacyWickContract1[Cases[res, _?QF, \[Infinity]]]]/.NM->Times]

LegacyWickContract1[expr_] := 
 Block[{tmpexpr}, tmpexpr = expr /. NM :> List; 
  sgn = Signature[tmpexpr]; 
  tmp1 = sgn*Signature[#]*NMM[#] & /@ Permutations[tmpexpr] //. 
    NMM[{a__}] :> NMM[a];
  tmp2 = tmp1 //. {NMM[xx___, Field[ferm1_, a_, b_, c_], 
        FieldB[ferm2_, d_, e_, f_], yy___] :> 
       DE[{ferm1, ferm2}, {f, c}][CI[{a,d}] , SI[{b, e}]]*NMM[xx, yy], 
       NMM[] -> 1, 
      DE[{ferm1_, ferm2_}, __][___] /; ferm1 =!= ferm2 :> 0} //. 
    NMM[FieldB[__], __] :> 0; Plus @@ DeleteCases[Union[tmp2], 0]]

WickContract[expr_] := 
Block[{expr1},
 expr1=expr//. NonCommutativeMultiply[arg1___, a_.*(b_ + Plus[c__]), arg2___]
:>
  NonCommutativeMultiply[arg1, a*b, arg2] +
   NonCommutativeMultiply[arg1, a*Plus[c], arg2];
  If[Head[Distribute[expr1]] === Plus,
   WickContract2[#] & /@ Distribute[expr1], WickContract2[expr1]]];


QF[expr_] := (Head[expr] === Field) || (Head[expr] === FieldB)

WickContract2[expr_] := 
 Block[{res}, res = expr /. NonCommutativeMultiply -> NM;
  NM[NM @@ (DeleteCases[res, _?QF, \[Infinity]]), 
    WickContract1[Cases[res, _?QF, \[Infinity]]]] /. NM -> Times]

WickContract1[expr_] := 
 Block[{tmpexpr}, tmpexpr = expr /. NM :> List;
  sgn = Signature[tmpexpr];
  creat = Union[Cases[tmpexpr, Field[__], \[Infinity]]];
  ann = Union[Cases[tmpexpr, FieldB[__], \[Infinity]]];
  tmp1 = ((sgn*Signature[#]*NMM2[#]) & /@ ((Join[creat, #]) & /@ 
        Permutations[ann])) //. NMM2[{a__}] :> NMM2[a];
  tmp2 = tmp1 //. {NMM2[xx___, Field[ferm1_, a_, b_, c_], 
        FieldB[ferm2_, d_, e_, f_], yy___] :> 
       DE[{ferm1, ferm2}, {f, c}][CI[{a, d}], SI[{b, e}]]*
        NMM2[xx, yy], 
      NMM2[] -> 1} //. {DE[{ferm1_, ferm2_}, __][___] /; 
       ferm1 =!= ferm2 :> 0}; Plus @@ DeleteCases[Union[tmp2], 0]]

ed = ({Black, {Arrowheads[{{1/2, 1/2, 
        Graphics[{Black, 
          Inset[Style[#3, Background -> White], {0, 0}, Automatic, 
           Automatic, None]}]}, {0.05032098685208049`, 0.8`}}], 
     Arrow[#1]}} &);
vd = ({Black, Text[
    Framed[#2, Background -> White, 
     RoundingRadius -> Scaled[10000]], #1]} &);

SetOptions[GraphPlot,{EdgeRenderingFunction -> ed,VertexRenderingFunction ->vd, SelfLoopStyle -> 1/4.,DirectedEdges->True,VertexLabeling->True}];

GraphWC[expr_,opts:OptionsPattern[]]:=Block[{t1,t2,t3},


t1 = Expand[expr /. {NM -> Times, Dot -> Times}];
t2 = Rule[#, 1] & /@ DeleteCases[Variables[t1], DE[__][__], \[Infinity]];
t3 = t1 /. t2 //. Times[b___, a_?NumberQ, c___] -> Times[b, c]/. Plus -> List;
 rul=FilterRules[{opts},Options[GraphPlot]];
If[Length[Dimensions[t3]]==1,
If[ Head[t3]=!=List,
Show[GraphPlot[#,rul] & /@ 
  Union[(Cases[#, DE[__][__], \[Infinity]] & /@ {{Expand[t3]}} //. 
     DE[{a_, a_}, {x_, y_}][__] -> {x -> y, a})]],
Show[{#}]&/@(GraphPlot[#,rul] & /@ 
  Union[(Cases[#, DE[__][__], \[Infinity]] & /@ Expand[t3] //. 
     DE[{a_, a_}, {x_, y_}][__] -> {x -> y, a})])
]
]
]


SetAttributes[DD, Orderless]

Contract[expr_]:=FixedPoint[Contract1,expr];

Contract1[expr_] := 
 Block[{}, 
       part1=expr//.
 {
         DE[{a__},{x_,y_}][CI[ {ind1_, ind2_}], SI[{lind1_, lind2_}]]*
         DEProject[{a__},{x_,y_}][CI[ {ind3_, ind4_}],SI[ {lind3_, lind4_}]]
           :> 
         DD[ind1,ind4] *DD[ind2,ind3] *DD[lind1,lind4]*DD[lind2, lind3] ,
         DE[{a__},{y_,x_}][CI[ {ind1_, ind2_}], SI[{lind1_, lind2_}]]*
         DEProject[{a__},{y_,x_}][CI[ {ind3_, ind4_}],SI[ {lind3_, lind4_}]]
           :> 
         DD[ind1,ind4] *DD[ind2,ind3] *DD[lind2,lind3]*DD[lind1, lind4] ,
         DE[{a__},{x_,y_}][CI[ {ind1_, ind3_}], SI[{lind1_, lind2_}]]*
         DEInverse[{a__},{y_,z_}][CI[ {ind3_, ind4_}],SI[ {lind2_, lind4_}]]
           :> 
         DD[ind1,ind4] *DD[lind1, lind4] * 
         DD[x,z] ,
 	DD[ind1_,ind1_]:>1,
Dot[a___,DD,b___]:>Dot[a,b],
Dot[a___,DE[c__,{x_,y_}],DEInverse[c__,{y_,z_}],b___]:>DD[x,z]Dot[a,b]
       };
       part1
         //. 
	{
	Eps[a___, b_, c___] DD[b_, e_] :> Eps[a, e, c], 
        DD[a1_, a2_]*(H_)[a2_, a3_] :> 
        H[a1, a3]
	} 
	//. 
	{
	DD[a1_, a2_]*(H__)[parms1___, {a2_, a3_}, 
        parms2__] :> H[parms1, {a1, a3}, parms2], 
     	DD[a1_, a2_]*(H_)[parms1___, {a3_, a2_}, parms2___] :> 
      	H[parms1, {a3, a1}, parms2],
	DD[a1_, a2_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b], 
	DD[a2_, a1_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b],
	DD[a1_, a2_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___][inds__] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b][inds], 
	DD[a2_, a1_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___][inds__] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b][inds],
	(H__)[CI[{a1_, a2_}], SI[{l1_, l2_}]]*(G__)[CI[{a2_, a3_}], 
   	SI[{l2_, l3_}]] :> (H.G)[CI[{a1, a3}], SI[{l1, l3}]],
	Dot[a___,DD,b___][inds___]:>Dot[a,b][inds],
	(H__)[CI[{a2_, a1_}], SI[{l2_, l1_}]]*(G__)[CI[{a2_, a3_}],
   	SI[{l2_, l3_}]] /;Length[H]<Length[G]:> (Transpose[H].G)[CI[{a1, a3}], SI[{l1, l3}]],
	(H__)[CI[{a1_,a2_}], SI[{ l1_, l2_}]]*(G__)[CI[{a3_,a2_}],
   	SI[{l3_,l2_}]] /;Length[H]<Length[G]:> (G.Transpose[H])[CI[{a3, a1}], SI[{l3, l1}]],
	(H__)[CI[{a2_, a1_}], SI[{l2_, l1_}]]*(G__)[CI[{a1_, a3_}],
   	SI[{l2_, l3_}]] :> (transposeSpin[H].G)[CI[{a2, a3}], SI[{l1, l3}]],
	(H__)[CI[{a2_, a3_}], SI[{l2_, l1_}]]*(G__)[CI[{a1_, a2_}],
   	SI[{l2_, l3_}]] :> (transposeSpin[G].H)[CI[{a1, a3}], SI[{l3, l1}]],
	(H__)[CI[{a1_,a2_}], SI[{ l1_, l2_}]]*(G__)[CI[{a3_,a2_}],
   	SI[{l3_,l2_}]] /;Length[H]<Length[G]:> (G.Transpose[H])[CI[{a3, a1}], SI[{l3, l1}]],
 	(H__)[CI[{a1_, a2_}], SI[{l1_, l2_}]]*(G__)[
    	CI[{a2_, a3_}]] :> (H.G)[CI[{a1, a3}], SI[{l1, l2}]],
 	(H__)[CI[{a1_, a2_}], SI[{l1_, l2_}]]*(G__)[
    	CI[{a3_, a2_}]] :> (H.colorTranspose[G])[CI[{a1, a3}], SI[{l1, l2}]]
	}
 	//. 
	{
	DD[a1_, a2_]*(H_)[a3_, a2_] /;
       	H =!= List :> H[a3, a1]}
   	//. 
	{
	DD[a1_, a2_]*(H_)[a2_, a3_] :> H[a1, a3], 
        DD[a1_, a2_]*(H_)[a3_, a2_] :> 
        H[a3, a1]} 
       	//. 
	{
	DD[a1_, a2_]*(H_)[parms1___, {a2_, a3_}, 
        parms2___] :> H[parms1, {a1, a3}, parms2], 
        DD[a1_, a2_]*(H_)[parms1___, {a3_, a2_}, parms2___] :> 
        H[parms1, {a3, a1}, parms2]}
        //. 
	{
	DD[a1_, a2_]*(H_)[parms1___,(f_)[{a2_, a3_}], 
             parms2___] :> H[parms1, f[{a1, a3}], parms2], 
        DD[a1_, a2_]*(H_)[parms1___, (f_)[{a3_, a2_}], parms2___] :> 
         H[parms1, f[{a3, a1}], parms2]}
       //. {(H_)[a1_, a2_]*(G_)[a2_, a3_] :> (H.G)[a1, 
           a3]}
       //. {
           (H_)[a1_, a2_]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (H.G)[a,f[{a1,a3}],b],
           (H_)[a2_, a1_]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (Transpose[H].G)[a,f[{a1,a3}],b],
           (H_)[a3_, a1_]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (G.H)[a,f[{a2,a1}],b],
           (H_)[a1_, a3_]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (G.Transpose[H])[a,f[{a2,a1}],b],
           (H_)[(f_)[{a1_, a2_}]]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (H.G)[a,f[{a1,a3}],b],
           (H_)[(f_)[{a2_, a1_}]]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (Transpose[H].G)[a,f[{a1,a3}],b],
           (H_)[(f_)[{a3_, a1_}]]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (G.H)[a,f[{a2,a1}],b],
           (H_)[(f_)[{a1_, a3_}]]*(G_)[a___,(f_)[{a2_, a3_}],b___] /;FreeQ[H,DD]:> (G.Transpose[H])[a,f[{a2,a1}],b]
	   }
     
]

ReorderIndex[f1_, 
  f2_, {alpha__}, {beta__}, {eps1__}, {eps2__}, {si1__}, {si2__}] := 
 Block[{},
  lind1 = #[[1]] & /@ {{si1}, {si2}};
  lind2 = #[[2]] & /@ {{si1}, {si2}};
  lover1 = Complement[{eps1}, lind1];
  lover2 = Complement[{eps2}, lind2];
  reind1 = Join[lind1, lover1];
  reind2 = Join[lind2, lover2];
  
    tmpvar=Select[Last@
       Reap[MapIndexed[Sow[#2, #1] &, Join[{alpha}, {beta}]]], 
      Length[#] > 1 &];
  If[tmpvar=={},Print[ReorderIndex1[f1,f2,{alpha},{beta},{eps1},{eps2},{si1},{si2}]]];
  remind =
   Delete[Join[{alpha}, {beta}], 
tmpvar[[1]]];
  name = Flatten[
    Select[Last@
       Reap[MapIndexed[Sow[#2, #1] &, Join[{alpha}, {beta}]]], 
      Length[#] > 1 &][[1]]];
  (*Print[reind1,{eps1},reind2,{eps2}];*)
  
  Signature[reind1]*Signature[{eps1}]*Signature[reind2]
   *Signature[{eps2}]
   quarkContract[name, f1, f2][CI[{lover2[[1]], lover1[[1]]}], SI[remind]]
  ]


QuarkContract[expr_]:=FixedPoint[QuarkContract1,expr]

QuarkContract1[expr_] := Block[{tmpexpr},
  (* Do canonical Contractions *)
  
res=  Contract[Expand[expr]] 
       /. 
       Eps[a___, b_, c___]*
         Eps[d___, e_, 
          f___]*(f1_)[CI[{b_, b2_}],SI[ {alpha1_, 
           alpha2_}]]*(f2_)[CI[{e1_, e_}],SI[ {alpha1_, 
           beta2_}]] /;Not[FreeQ[{d,f},b2]]&&Not[FreeQ[{a,c},e1]]:> 
        ReorderIndex[f1, 
         f2, {alpha1, alpha2}, {alpha1, beta2}, {a, b, c}, {d, e, 
          f}, {b, b2}, {e1, e}] /. 
      Eps[a___, b_, c___]*
        Eps[d___, e_, 
         f___]*(f1_)[CI[ {b_, b2___}],SI[ {alpha1_, 
          alpha2_}]]*(f2_)[CI[ {e1___, e_}],SI[ {beta2_, 
          alpha1_}]]  /;Not[FreeQ[{d,f},b2]]&&Not[FreeQ[{a,c},e1]] :> 
       ReorderIndex[f1, 
        f2, {alpha1, alpha2}, {beta2, alpha1}, {a, b, c}, {d, e, 
         f}, {b, b2}, {e1, e}] /. 
     Eps[a___, b_, c___]*
       Eps[d___, e_, 
        f___]*(f1_)[CI[{b_, b2___}],SI[ {alpha1_, 
         alpha2_}]]*(f2_)[CI[ {e1___, e_}],SI[ {alpha2_, 
         beta2_}]]   /;Not[FreeQ[{d,f},b2]]&&Not[FreeQ[{a,c},e1]] :> 
      ReorderIndex[f1, 
       f2, {alpha1, alpha2}, {alpha2, beta2}, {a, b, c}, {d, e, 
        f}, {b, b2}, {e1, e}]
    /. Eps[a___, b_, c___]*
      Eps[d___, e_, 
       f___]*(f1_)[CI[{b_, b2___}],SI[ {alpha1_, 
        alpha2_}]]*(f2_)[CI[ {e1___, e_}],SI[ {beta2_, 
        alpha2_}]]   /;Not[FreeQ[{d,f},b2]]&&Not[FreeQ[{a,c},e1]] :> 
     ReorderIndex[f1, 
      f2, {alpha1, alpha2}, {beta2, alpha2}, {a, b, c}, {d, e, f}, {b,
        b2}, {e1, e}]
   (*/. Eps[a___, b_, c___]*
     Eps[d___, e_, 
      f___]*(f1_)[CI[ {b_, b2___}], SI[{alpha1_, 
       alpha2_}]]*(f2_)[CI[ {e1___, e_}], SI[{beta1_, 
       beta2_}]]   /;Not[FreeQ[{d,f},b2]]&&Not[FreeQ[{a,c},e1]] :> 
    ReorderIndex[f1, 
     f2, {alpha1, alpha2}, {beta1, beta2}, {a, b, c}, {d, e, f}, {b, 
      b2}, {e1, e}]*)
    //.{
       (H_)[CI[{a1_,a2_}],SI[{l1_,l2_}]]*(G_)[CI[{a2_,a1_}], SI[{l2_,l1_}]] :> trace[H.G] ,
       (H_)[CI[{a1_,a2_}],SI[{l1_,l2_}]]*(G_)[CI[{a2_,a1_}], SI[{l1_,l2_}]] :> trace[H.transposeSpin[G]] ,
       (H_)[CI[{a1_,a2_}],SI[{l1_,l2_}]]*(G_)[CI[{a2_,a3_}], SI[{l1_,l3_}]] :> (transposeSpin[H].G )[CI[{a1,a3}],SI[{l2,l3}]],
       (H_)[CI[{a1_,a2_}],SI[{l1_,l2_}]]*(G_)[CI[{a1_,a2_}], SI[{l2_,l1_}]] :> trace[H.colorTranspose[G]] 
       } 
    //.{(H_)[CI[{a1_,a1_}],SI[{l1_,l2_}]]:> traceColor[H][SI[{l1,l2}]] } 
    //.{(H_)[CI[{a1_,a2_}],SI[{l1_,l1_}]]:> traceSpin[H][CI[{a1,a2}]]} 
    //.{
        (H_)[CI[{a1_,a2_}]] * (G_)[CI[{a2_,a1_}]]:> traceColor[H.G],
        (H_)[SI[{a1_,a2_}]] * (G_)[SI[{a2_,a1_}]]:> traceSpin[H.G],
        (H_)[SI[{a1_,a1_}]] :> traceSpin[H],
        (H_)[CI[{a1_,a1_}]] :> traceColor[H],
        traceSpin[traceColor[a__]]:>trace[a]}
        //.{traceSpin[traceColor[a__]]:>trace[a],traceColor[traceSpin[a__]]:>trace[a]};
  
Contract[res]
  ]

SimpleContract[expr_] := 
 Block[{}, 

       part1=expr//.
 {
         DE[{a__},{x_,y_}][CI[ {ind1_, ind2_}], SI[{lind1_, lind2_}]]*
         DEProject[{a__},{x_,y_}][CI[ {ind3_, ind4_}],SI[ {lind3_, lind4_}]]
           :> 
         DD[ind1,ind4] *DD[ind2,ind3] *DD[lind1,lind4]*DD[lind2, lind3] ,
         DE[{a__},{y_,x_}][CI[ {ind1_, ind2_}], SI[{lind1_, lind2_}]]*
         DEProject[{a__},{y_,x_}][CI[ {ind3_, ind4_}],SI[ {lind3_, lind4_}]]
           :> 
         DD[ind1,ind4] *DD[ind2,ind3] *DD[lind2,lind3]*DD[lind1, lind4] ,
         DE[{a__},{x_,y_}][CI[ {ind1_, ind3_}], SI[{lind1_, lind2_}]]*
         DEInverse[{a__},{y_,z_}][CI[ {ind3_, ind4_}],SI[ {lind2_, lind4_}]]
           :> 
         DD[ind1,ind4] *DD[lind1, lind4] * 
         DD[x,z] ,
 	DD[ind1_,ind1_]:>1,
Dot[a___,DD,b___]:>Dot[a,b],
Dot[a___,DE[c__,{x_,y_}],DEInverse[c__,{y_,z_}],b___]:>DD[x,z]Dot[a,b]
       };
       part1
         //. 
	{
	Eps[a___, b_, c___] DD[b_, e_] :> Eps[a, e, c], 
        DD[a1_, a2_]*(H_)[a2_, a3_] :> 
        H[a1, a3]
	} 
	//. 
	{
	DD[a1_, a2_]*(H__)[parms1___, {a2_, a3_}, 
        parms2__] :> H[parms1, {a1, a3}, parms2], 
     	DD[a1_, a2_]*(H_)[parms1___, {a3_, a2_}, parms2___] :> 
      	H[parms1, {a3, a1}, parms2],
	DD[a1_, a2_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b], 
	DD[a2_, a1_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b],
	DD[a1_, a2_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___][inds__] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b][inds], 
	DD[a2_, a1_]*
  	Dot[a___, (H__)[parms1___, {a2_, a3_}, parms2___], b___][inds__] :> 
 	Dot[a, H[parms1, {a1, a3}, parms2], b][inds]}
 	//. 
	{
	DD[a1_, a2_]*(H_)[a3_, a2_] /;
       	H =!= List :> H[a3, a1]}
   	//. 
	{
	DD[a1_, a2_]*(H_)[a2_, a3_] :> H[a1, a3], 
        DD[a1_, a2_]*(H_)[a3_, a2_] :> 
        H[a3, a1]} 
       	//. 
	{
	DD[a1_, a2_]*(H_)[parms1___, {a2_, a3_}, 
        parms2___] :> H[parms1, {a1, a3}, parms2], 
        DD[a1_, a2_]*(H_)[parms1___, {a3_, a2_}, parms2___] :> 
        H[parms1, {a3, a1}, parms2]}
        //. 
	{
	DD[a1_, a2_]*(H_)[parms1___,(f_)[{a2_, a3_}], 
             parms2___] :> H[parms1, f[{a1, a3}], parms2], 
        DD[a1_, a2_]*(H_)[parms1___, (f_)[{a3_, a2_}], parms2___] :> 
         H[parms1, f[{a3, a1}], parms2]}
]

ToQDP[expr_, reps_: {} , fn_:"stdout"] := 
 Block[{res, props, propreps, iter = 0}, res = expr //. reps; 
  props = Union[Cases[res, DE[__], \[Infinity],Heads->True]];
  propreps = 
   Flatten[(iter++; {Rule[#, 
         ToExpression["quarkProp" <> ToString[iter]]]}) & /@ props];

  (*Print[Union[Cases[List[res],(H__)[CI[a__],SI[b__]]:>{a,b},\[Infinity],Heads->True]]];*)
  spincolorm=Length[Union[Cases[List[res],(H__)[CI[a__],SI[b__]]:>{a,b},\[Infinity],Heads->True]]];
  spinm=Length[Union[Cases[List[res],(H__)[SI[b__]]:>{b},\[Infinity],Heads->True]]];
  colorm=Length[Union[Cases[List[res],(H__)[CI[a__]]:>{a},\[Infinity],Heads->True]]];
  If[Not[(spincolorm+spinm+colorm==1) || (spincolorm+spinm+colorm==0)],
  Print["Could not identify QDP structure!"];Return[]];
  
  WriteString[fn, "/* \n"];
  Which[spincolorm==1,
  WriteString[fn, "Result is of type SpinColorMatrix\n"],
  colorm==1,
  WriteString[fn, "Result is of type ColorMatrix\n"],
  spinm==1,
  WriteString[fn, "Result is of type SpinMatrix\n"],
  spincolorm+spinm+colorm==0,
  WriteString[fn, "Result is of type Scalar\n"]
];
  WriteString[fn, 
   ToString[
     propreps /. 
      DE[{a_, a_}, {x_, y_}] :> 
       "S^" <> ToString[a] <> "(" <> ToString[x] <> "," <> 
        ToString[y] <> ")"] <> "\n"];
  WriteString[fn, "*/ \n"];
  WriteString[fn, CForm[res/.{(H__)[CI[__],SI[__]]:>H,(H__)[CI[__]]:>H,(H__)[SI[__]]:>H} /. propreps]];]


Uncontract[expr_] := FixedPoint[Uncontract1, expr]

(* With this uncontract also object not carrying color indices artificially get  color indices *)
 
Uncontract1[expr_] := 
 Block[{}, 
  expr /. {Dot[a_, b_, c__][CI[{a1_, a2_}], 
      SI[{l1_, l2_}]] :> (CIU = Unique["ci"]; SIU = Unique["si"]; 
      a[CI[{a1, CIU}], SI[{l1, SIU}]]*
       Dot[b, c][CI[{CIU, a2}], SI[{SIU, l2}]]), 
    Dot[a_, b_][CI[{a1_, a2_}], 
      SI[{l1_, l2_}]] :> (CIU = Unique["ci"]; SIU = Unique["si"]; 
      a[CI[{a1, CIU}], SI[{l1, SIU}]]*
       b[CI[{CIU, a2}], SI[{SIU, l2}]])}]





ToTeXForm[expr_]:=Block[{},
part1=expr/.{
(H_)[CI[a__],SI[b__]]/;Head[H]=!=DE :>TForm[H[CI[a],SI[b]]],
(H_)[SI[b__]]/;H=!=DE :>TForm[H[SI[b]]],
(H_)[CI[a__]]/;H=!=DE :>TForm[H[CI[a]]]
};
WriteString["stdout",ToString[part1,TeXForm]]
]


Canonicalize[expr_, y_, si_] := 
 Block[{expr1}, 
  expr1 = Expand[
    QuarkContract[expr]]; (Canonicalize1[#, y, si] & /@ 
     Expand[expr1 + REMOVEINTHEND]) /. REMOVEINTHEND -> 0]

Canonicalize1[expr_, y_, si_] := 
 Block[{props, repl, expr1}, props = Cases[expr, DE[__], \[Infinity]];
   repl = Flatten[
    DeleteCases[
     If[FreeQ[(# /. DE[a__, {b__}] :> {b})[[1]], y], 
        Rule[#, G5.adj[
           ReplaceAll[#, 
            DE[{xx__}, {bx_, cx_}] -> DE[{xx}, {cx, bx}]]].G5], 
        NOMATCH] & /@ props, NOMATCH]];
  shiftargs = 
   Coefficient[#, 
      si] & /@ (Cases[
       Flatten[props /. 
         DE[a__, {xx_, yy_}] :> {xx, yy}], _?(Not[FreeQ[#, y]] &)] /. 
      y -> 0);
  shiftsep = -Min[Flatten[Join[shiftargs, {0}]]];
  
  expr1 = expr /. repl;
  doshift = 
   Rule[#, # + shiftsep*si] & /@ 
    Union[Flatten[
      Flatten[props /. DE[a__, {xx_, yy_}] :> {xx, yy}] /. si -> 0]];
  If[Length[doshift] > 0, expr1 /. doshift, expr1] /. 
    func_[arg1___, {y + cy_., xx_ + si*a_.}, arg2___] :> 
     makeshift[func[arg1, {y + cy, xx}, arg2], a, si] /. {func_[
      arg1___, {yy_., xx_ + si*a_.}, arg2___] :> 
     makeshift[func[arg1, {yy, xx}, arg2], a, si], 
    func_[xx_ + si*a_.] :> makeshift[func[xx], a, si]}
  ]

makeshift[func_, sign_, dir_] := 
 Block[{newfunc = func, $ri}, 
  If[sign > 0, mv = FORWARD, mv = BACKWARD]; 
  For[$ri = 0, $ri < Abs[sign], $ri++, 
   newfunc = shift[newfunc, mv, dir]]; newfunc]

  End[]

EndPackage[]
