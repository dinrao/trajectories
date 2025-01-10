(* ::Package:: *)

(* ::Title:: *)
(*Trajectory*)


(* ::Text:: *)
(*Author: Dinesh Rao, dinrao@gmail.com, raospiderlab.org*)
(*With code input from Horacio Tapia-McClung and Claude.ai (angular velocity)*)
(**)
(*This is a package mainly to make ball and pin plots  to represent trajectories with 2 points tracked per animal; first used as far as I know by Tom Collett and Michael Land in the paper Collett, T. & Land, M. Visual control of flight behaviour in the hoverfly Syritta pipiens L. Journal of Comparative Physiology 99, 1\[Dash]66 (1975). Additional functions are also included. *)
(**)
(*Data must be in the following format. XY coordinates in columns in this order. Head, Abdomen, Otherhead, other abdomen, object or target XY, scale. *)
(**)
(*If there is only one animal then use Collettplot and place object of interest XY coordinates after the head/abdomen column for distance calculations.  *)
(*Step1: Run InputUserValues[] to set variables such as frame rate, location of data, scale info and y max to flip the data*)
(*Step 2: Run data = GetData[] to import a list of csv files from a folder specified earlier. Assumes headerlines*)
(*Step 3: Run processedData=PreProcFunc to flip the y axis data and convert to real world cm. *)
(**)
(*All further functions must be run on processed data*)
(*Proximity Cut is a function to only use data up to the closest approach to a target*)
(**)
(**)


BeginPackage["Trajectory`"];


InputUserValues::usage ="User specified values of location of csv files, frame rate, max y value and scale info location according to project"


GetData::usage = "Import csv files from a  given folder, assumes there's a header";


PreProcFunc::usage = "Function to flip y axis and convert data to cm";


ProximityCut::usage = "Cut trajectory file data upto the nearest point to target orbject"


CollettPlot::usage = "CollettPlots[data] generate ball and pin plots of tracked position data.";


TwinCollettPlot::usage = "TwinCollettPlots[data] generate ball and pin plots of tracked position data of two animals.";


BodyAxis::usage = "Angle between head and abdomen"


GazeDirection::usage ="Angle between head and target"


DistanceProfile::usage = "Calculates consecutive distances to an object that may also be moving; repeat coords in the dataset if it's a fixed object";


TrajectoryStraightness::usage = "Calculate how straight the path is; distance betn 1st and last point/ length of traj"


SpeedProfile::usage = "Calculates speed of animal at each frame";


SpeedCollettPlot::usage = "Ball and pin plots with speed data as colour of heads";


AngularVelocity::usage = "Calculates angular velocities of head coordinates at each time step (in radians)";


PersistenceVelocity::usage ="Calculates persistence velocity of head coordinates"


allfigs::usage ="plot of all figures";


InputUserValues[] := Module[{},
CreateDialog[Grid[{
{"Path to folder with csv files:",InputField[Dynamic[folder],String]},{"Maximum value of Y from video frame:",InputField[Dynamic[ymax],Number]},
{"Column number where scale is located:",InputField[Dynamic[sc],Number]},
{"Frame rate of video:",InputField[Dynamic[fr],Number]},
{"Number of points tracked in csv file:",InputField[Dynamic[pts],Number]},
{"Target Column numbers (separated by space; e.g., 2 3):",InputField[Dynamic[target],String]},
{CancelButton[],DefaultButton[DialogReturn[{folder,ymax,sc,fr,pts,target}]]}},Spacings->{1,Automatic},Alignment->Left],Modal->True]]


GetData[]:=Module[{fn,data},
fn=FileNames["*.csv",folder];
data=Import[#,"Data",  HeaderLines->1]&/@fn;
data]


Begin["`Private`"];


PreProcFunc[file_]:=Module[{flip,datacm,scale,cols},
cols=Range[2,2*pts,2]; (*specify what columns to flip*)
flip = MapAt[(ymax-#)&,file,{All,cols}];(*flip y axis*)
scale=file[[1,sc]];
datacm=N[(flip/scale)](*Convert to cm using scale value in csv file*)
]


ProximityCut[file_]:=Module[{head,obj,cuts,fullDistProfile,cuttraj},
head=file[[All,{1,2}]];
obj=file[[All, {5,6}]];
fullDistProfile=Table[EuclideanDistance[head[[i]],obj[[i]]],{i,Length@head-1}];
cuts=First[Flatten[Position[fullDistProfile,Min[fullDistProfile]]]];
cuttraj=file[[;;cuts,All]];
cuttraj
]


CollettPlot[data_]:=Module[{head,abdomen},
(
head=data[[All, {1,2}]];
abdomen= data[[All,{3,4}]];
Graphics[{Orange,Arrowheads[{{.003,Automatic,Graphics[{Black,Circle[]}]}}],Arrow[#]}&/@Transpose[{abdomen,head}][[;;;;1]], PlotRange->All])]


TwinCollettPlot[data_]:=Module[{fhead,fabdomen,mhead,mabdomen},
(fhead=data[[All, {1,2}]];
fabdomen= data[[All,{3,4}]];
mhead=data[[All, {5,6}]];
mabdomen= data[[All,{7,8}]];
Show[{Graphics[{Orange,Arrowheads[{{.003,Automatic,Graphics[{Black,Circle[]}]}}],Arrow[#]}&/@Transpose[{fabdomen,fhead}][[;;;;5]], PlotRange->All],Graphics[{Blue,Arrowheads[{{.003,Automatic,Graphics[{Black,Circle[]}]}}],Arrow[#]}&/@Transpose[{mabdomen,mhead}][[;;;;5]], PlotRange->All]}])]


BodyAxis[file_]:=Module[{head,abdomen,angListPos},
(head=file[[All, {1,2}]];
abdomen= file[[All,{3,4}]];
angListPos[p1_,p2_]:=N[Mod[ArcTan@@(p2-p1),2 Pi]/\[Degree]];
MapThread[angListPos,{head,abdomen}])]


GazeDirection[file_]:=Module[{head,targt, angListPos},
head=file[[All, {1,2}]];
targt=file[[All, ToExpression[StringSplit[target]]]];
angListPos[p1_,p2_]:=N[Mod[ArcTan@@(p2-p1),2 Pi]/\[Degree]];
MapThread[angListPos,{head,targt}]
]


DistanceProfile[file_]:=Module[{head,obj, dist},
(obj=file[[All, target]];
head=file[[All, {1,2}]];
dist=Table[EuclideanDistance[head[[i]],obj[[i]]],{i,Length@head-1}]
)]


TrajectoryStraightness[file_]:=Module[{head,totaldist, distStartFinish,straightness},
head=file[[All, {1,2}]];
totaldist=Total[Table[EuclideanDistance[head[[i]],head[[i+1]]],{i,Length@head-1}]];
distStartFinish = EuclideanDistance[head[[1]],head[[-1]]];
straightness = distStartFinish/totaldist
]


SpeedProfile[file_]:=Module[{head,betnFramesDistances,betnFramesDistancesincm,speeds,fps},
(
fps = 1/fr;
head = file[[All,{1,2}]];
betnFramesDistances=Table[EuclideanDistance[head[[i]],head[[i+1]]],{i,Length@head-1}];
speeds=betnFramesDistances/fps
)]


SpeedCollettPlot[file_]:=Module[{head,abdomen,frame3,val,colors,colourheads,plot,legend},head=file[[All,{1,2}]];
abdomen=file[[All,{3,4}]];
(*frame3 = Graphics[{Thick,Red,Line[{file[[1,{5,6}]],file[[1,{7,8}]]}]}];this is to place a reference object*)
(*use this to colour the heads according a particular value such as speed*)
val=Prepend[LowpassFilter[SpeedProfile[file],0.07],0];(*calls the Speed function and adds a zero to the list of speed values in order to match the number of points in the trajectory and then smooths the data*)
colors=ColorData["BlueGreenYellow"]/@Rescale@val;
colourheads=MapThread[{#1,PointSize[0.006],Point[#2]}&,{colors,head}];
plot=Graphics[{{Gray,Arrowheads[0],Arrow[#]}&/@Transpose[{abdomen,head}][[;;;;5]],{colourheads[[;;;;5]]}},PlotRange->All](*,frame3*);
legend=BarLegend[{"BlueGreenYellow",{Min[val],Max[val]}},LegendLabel->"Speed (cm/s)"];
Legended[plot,legend]]


AngularVelocity[coordinates_List]:=Module[{n,angles,timeStep,angularVelocities},
n=Length[coordinates];(*Number of points*)
angles=Table[ArcTan[coordinates[[i+1,1]]-coordinates[[i,1]],coordinates[[i+1,2]]-coordinates[[i,2]]],{i,1,n-1}];(*Calculate angles between consecutive points*)
timeStep=1/fr;
angularVelocities=Prepend[Table[(*Use ArcTan to handle angle wrapping*)ArcTan[Sin[angles[[i+1]]-angles[[i]]],Cos[angles[[i+1]]-angles[[i]]]](** (180/Pi) for values in degrees*)/timeStep,{i,1,n-2}],(*First point has no angular velocity*)0];
Append[angularVelocities,0]](*Add 0 for the last point as we can't calculate its angular velocity*)


PersistenceVelocity[file_]:=Module[{head,dt,speeds,orientations,turningAngles,PV},
head=file[[All, {1,2}]];
dt=Table[1/500,{Length[head]-1}];(*Calculate time differences*)
speeds=Table[EuclideanDistance[head[[i]],head[[i+1]]]/dt[[i]],{i,1,Length[file]-1}];
(*Calculate compass orientations*)
orientations=Table[ArcTan[head[[i+1,1]]-head[[i,1]],head[[i+1,2]]-head[[i,2]]],{i,1,Length[head]-1}];
(*Calculate turning angles*)
turningAngles=Table[angleDifference=orientations[[i+1]]-orientations[[i]];
(*Normalize to[-\[Pi],\[Pi]]*)Mod[angleDifference+\[Pi],2\[Pi]]-\[Pi],{i,1,Length[orientations]-1}];
PV=Table[speeds[[i]]*Cos[turningAngles[[i]]],{i,1,Length[turningAngles]}]
]


allfigs[file_]:=Module[{dp,cp,ba,ga,scp,spd,av,pv},
cp=CollettPlot[file];
scp=SpeedCollettPlot[file];
dp = ListLinePlot[DistanceProfile[file],Frame->True,FrameLabel->{{"Distance to web (cm)",None},{"Time (Frames)",None}},FrameTicks->{{All,None},{All,None}}, FrameStyle->Black, PlotLabel->"Distance Profile"];
ba=ListLinePlot[BodyAxis[file],Frame->True,FrameLabel->{{"Body axis",None},{"Time (Frames)",None}},FrameTicks->{{All,None},{All,None}}, FrameStyle->Black, PlotLabel->"Body Axis"];
ga=ListLinePlot[GazeDirection[file],Frame->True,FrameLabel->{{"Gaze Direction",None},{"Time (Frames)",None}},FrameTicks->{{All,None},{All,None}}, FrameStyle->Black, PlotLabel->"Gaze Direction"];
spd=ListLinePlot[LowpassFilter[SpeedProfile[file],0.5],Frame->True,FrameLabel->{{"Speed (cm/s)",None},{"Time (Frames)",None}},FrameTicks->{{All,None},{All,None}}, FrameStyle->Black, PlotLabel->"Speed"];
av=ListLinePlot[LowpassFilter[AngularVelocity[file],0.5],Frame->True,FrameLabel->{{"Angular velocity",None},{"Time (Frames)",None}},FrameTicks->{{All,None},{All,None}}, FrameStyle->Black, PlotLabel->"Angular velocity"];
pv=ListLinePlot[LowpassFilter[PersistenceVelocity[file],0.5],Frame->True,FrameLabel->{{"Persistent velocity",None},{"Time (Frames)",None}},FrameTicks->{{All,None},{All,None}}, FrameStyle->Black, PlotLabel->"Persistence velocity"];
Return[{dp,cp,ba,ga,scp,spd,av,pv}]
]


End[];


EndPackage[];
