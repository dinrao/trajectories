(* ::Package:: *)

BeginPackage["Trajectory3D`"];


InputUserValues3D::usage ="User specified values of location of csv files, frame rate, max y value and scale info location according to project";


GetData3D::usage = "Import csv files from a  given folder, assumes there's a header";


ProximityCut3D::usage = "cut trajectories to the closest point of approach";


CollettPlot3D::usage = "3D ball and pin plots";


TwinCollettPlot3D::usage = "TwinCollettPlots[data] generate 3d ball and pin plots of tracked position data of two animals.";


DistanceProfile3D::usage = "Calculates consecutive distances to an object that may also be moving; repeat coords in the dataset if it's a fixed object";


SpeedProfile3D::usage = "Calculates speed of animal at each frame";


SpeedCollettPlot3D::usage ="Plots a 3d collettplot with heads colour coded with speed values "


AngleofFlight::usage = "Angular change in path";


OrthogonalComponentsVelocity3D::usage ="Calculate Persistence velocity,Turning velocity and Inclination velocity at each frame "


Begin["`Private`"];


InputUserValues3D[]:=Module[{},
folder=Input["Enter path to folder with csv files, (with quotes): "];
fr=Input["Enter frame rate of video:"];
]


GetData3D[]:=Module[{fn,data},
fn=FileNames["*.csv",folder];
data=Import[#,"Data"]&/@fn;
data]


ProximityCut3D[file_]:=Module[{head,obj,cuts,fullDistProfile,cuttraj},
head=file[[All,{1,2,3}]];
obj=file[[All, {7,8,9}]];
fullDistProfile=Table[EuclideanDistance[head[[i]],obj[[i]]],{i,Length@head-1}];
cuts=First[Flatten[Position[fullDistProfile,Min[fullDistProfile]]]];
cuttraj=file[[;;cuts,All]];
cuttraj
]


CollettPlot3D[file_]:=Module[{head,abdomen},
((*frame1=p[[1, {5,6}]];
frame2  = p[[1,{7,8}]];
frame=Graphics[{Dashed,Red,Line[{frame1,frame2}]}]; use this to put other stuff such as ring or frame*)
head=file[[All, {1,2,3}]];
abdomen= file[[All,{4,5,6}]];
Show[Graphics3D[{Black,Arrowheads[{{.003,Automatic,Graphics3D[{Red,Opacity[0.3],Sphere[]}]}}],Arrow[#]}&/@Transpose[{abdomen,head}][[;;;;3]]]])]


TwinCollettPlot3D[data_]:=Module[{fhead,fabdomen,mhead,mabdomen, f, m},
(fhead=data[[All, {1,2,3}]];
fabdomen= data[[All,{4,5,6}]];
mhead=data[[All, {7,8,9}]];
mabdomen= data[[All,{10,11,12}]];
f = Graphics3D[{Black,Arrowheads[{{.003,Automatic,Graphics3D[{Red,Opacity[0.3],Sphere[]}]}}],Arrow[#]}&/@Transpose[{fabdomen,fhead}][[;;;;3]]];
m = Graphics3D[{Black,Arrowheads[{{.003,Automatic,Graphics3D[{LightBlue,Opacity[0.3],Sphere[]}]}}],Arrow[#]}&/@Transpose[{mabdomen,mhead}][[;;;;3]]];
Show[{f,m}]
)]


DistanceProfile3D[file_]:=Module[{head,obj, dist},
(obj=file[[All, {7,8,9}]];
head=file[[All, {1,2,3}]];
dist=Table[EuclideanDistance[head[[i]],obj[[i]]],{i,Length@head-1}]/10 (*data is in mm from pose3d, hence divide by 100*)
)]


SpeedProfile3D[file_]:=Module[{head,betnFramesDistances,betnFramesDistancesincm,speeds},
(
fps=1/500;
head = file[[All,{1,2,3}]];
betnFramesDistances=Table[EuclideanDistance[head[[i]],head[[i+1]]],{i,Length@head-1}];
speeds=(betnFramesDistances/10)/fps
)]


SpeedCollettPlot3D[file_] := Module[{head, abdomen, val, colors, plot},
  head = file[[All, {1, 2, 3}]];
  abdomen = file[[All, {4, 5, 6}]];
  
  (* Calculate speed profile *)
  val = SpeedProfile3D[file];
  
  (* Generate colors based on speed *)
  colors = ColorData["BlueGreenYellow"] /@ Rescale[val];
  
  (* Create the 3D plot *)
  plot = Graphics3D[{
    Table[{
      Directive[colors[[i]]],
      Sphere[head[[i]], 0.5],  (* Head sphere *)
      Cylinder[{abdomen[[i]], head[[i]]}, 0.1]  (* Stick connecting abdomen to head *)
    },
    {i, 1, Min[Length[head], Length[abdomen], Length[colors]],3}]  (* Use Min to avoid any potential length mismatches *)
  },
  Axes -> True,
  BoxRatios -> Automatic,
  Lighting -> "Neutral",
  ViewPoint -> {1.3, -2.4, 2}
  ];
  
  (* Add a color bar legend *)
  Legended[plot, 
    BarLegend[{"BlueGreenYellow", MinMax[val]}, 
      LegendLabel -> "Speed (cm/s)"]]
]



AngleofFlight[file_] := Module[{head, vectors, angles,angleOfFlight},
  head = file[[All, {1, 2, 3}]];
  vectors = Differences[head];
  angleOfFlight[v1_, v2_] := 
   ArcCos[(v1 . v2)/(Norm[v1] Norm[v2])]*(180/Pi);
  angles = 
   Table[angleOfFlight[vectors[[i]], vectors[[i + 1]]], {i, 
     Length[vectors] - 1}]
  ]


OrthogonalComponentsVelocity3D[data_]:=Module[{coordinates,fr,x,y,z,t,\[Rho],\[Theta],\[CurlyPhi],\[CapitalTheta],\[CapitalPhi],v,P,T,I},
(*Extract x,y,z coordinates*)
coordinates=data[[All,1;;3]];
x=coordinates[[All,1]];
y=coordinates[[All,2]];
z=coordinates[[All,3]];
fr=1/500;
(*Calculate time steps*)
t=Range[0,Length[coordinates]-1]*fr;
(*Helper functions*)\[Rho][i_]:=Sqrt[(x[[i+1]]-x[[i]])^2+(y[[i+1]]-y[[i]])^2+(z[[i+1]]-z[[i]])^2];
 \[Theta][i_]:=ArcTan[x[[i+1]]-x[[i]],y[[i+1]]-y[[i]]];
 \[CurlyPhi][i_]:=ArcCos[(z[[i+1]]-z[[i]])/\[Rho][i]];
(*Calculate changes in angles*) 
\[CapitalTheta]=Table[\[Theta][i+1]-\[Theta][i],{i,1,Length[coordinates]-2}];
\[CapitalPhi]=Table[\[CurlyPhi][i+1]-\[CurlyPhi][i],{i,1,Length[coordinates]-2}];
(*Calculate instantaneous velocity*)
v=Table[\[Rho][i]/(fr),{i,1,Length[coordinates]-1}];
(*Calculate orthogonal components*)
P=v[[1;;-2]]*Sin[\[CapitalPhi]]*Cos[\[CapitalTheta]];
(*Persistence velocity*)
T=v[[1;;-2]]*Sin[\[CapitalPhi]]*Sin[\[CapitalTheta]];
(*Turning velocity*)
I=v[[1;;-2]]*Cos[\[CapitalPhi]];
(*Inclination velocity*)(*Return results*)
<|"PersistenceVelocity"->P,"TurningVelocity"->T,"InclinationVelocity"->I|>]


End[];


EndPackage[];
