unit mergevertices;
//this function merges nearby vertices as a single vertex
// due to rouding errors, meshes can exhibit gaps and tears creating non-manifold meshes
// this code merges vertices that are within a given radius (the tolerance)
// if the tolerance is zero, only identical vertices are merged
// if the tolerance is 0.0001 than vertices within 0.001 are merged

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses
  Classes, sysutils,  meshify_simplify_quadric;

//procedure UnifyVertices(var faces: TFaces;  var vertices: TVertices; Radius: single);
procedure UnifyVertices(var faces: TFaces;  var vertices: TVertices; Radius: single);


implementation

type
TSortType = single; //can be integer, single, double, etc
TSort = record
   index:  integer;
   value: TSortType;
end;
TSortArray = array of TSort;
TInts = array of integer;

//http://stackoverflow.com/questions/24335585/quicksort-drama
procedure QuickSort(left, right: integer; var s: TSortArray);
// left:      Index des 1. Elements, right: Index des letzten Elements
var
  l, r, lswap: integer;
  pivot: TSortType;
begin
  if (right > left) then begin
    l := left;
    r := right;
    pivot := s[s[(right + left) div 2].index].value;
    while (l < r) do begin
      while s[s[l].index].value < pivot do
        l := l + 1;
      while s[s[r].index].value > pivot do
        r := r - 1;
      if (l <= r) then begin
        lswap := s[r].index;
        s[r].index := s[l].index;
        s[l].index := lswap;
        l := l + 1;
        r := r - 1;
      end;
    end;
    if (left < r) then
      QuickSort(left, r, s);
    if (right > l) then
      QuickSort(l, right, s);
  end;
end;

procedure SortArrayIndices(var s: TSortArray); //sorts indices, not values!
var
 i : integer;
begin
     if length(s) < 1 then exit;
     for i := 0 to (length(s)-1) do  //set indices
         s[i].index := i;
     quicksort(low(s), high(s), s);
end;

procedure vectorAdd (var A: TPoint3f; B: TPoint3f);  inline;
//sum two vectors
begin
     A.X := A.X + B.X;
     A.Y := A.Y + B.Y;
     A.Z := A.Z + B.Z;
end; // vectorAdd()

function vectorScale(A: TPoint3f; Scale: single): TPoint3f;// overload;
begin
     result.X := A.X * Scale;
     result.Y := A.Y * Scale;
     result.Z := A.Z * Scale;
end;

procedure ClusterVertex( var faces: TFaces; var vertices: TVertices; Radius: single);
var
 s: TSortArray;
 j,i, nv,nc,nvPost: integer;
 z, dz, dx: TSortType;
 pt,sum: TPoint3f;
 face: TPoint3i;
 newVert: TVertices;
 oldFaces: TFaces;
 radiusSqr: single;
 cluster, remap: TInts;
begin
   nv := length(vertices);
   if (nv < 3) or (Radius < 0) then exit;
   setlength(s,nv);
   setlength(remap,nv);
   setlength(cluster,nv);
   for i := 0 to (nv -1) do begin
       s[i].value := vertices[i].Z;
       cluster[i] := i;
       remap[i] := -1;
   end;
   SortArrayIndices(s);
   nvPost := 0;
   setLength(newVert, nv);
   if Radius <= 0 then begin
      for i := 0 to (nv - 1) do begin
          if cluster[i] = i then begin //not part of previous cluster
             pt := vertices[s[i].index];
             j := i + 1;
             while (j < nv) and (vertices[s[j].index].Z = pt.Z) do begin  //i.Z==j.Z
                   if (vertices[s[j].index].X = pt.X) and (vertices[s[j].index].Y = pt.Y) then begin//i.X==j.X, i.Y==j.Y
                      cluster[j] := nvPost;
                      remap[s[j].index] := nvPost;
                   end;
                   j := j + 1;
             end;
             newVert[nvPost] := pt;
             cluster[i] := nvPost;
             remap[s[i].index] := nvPost;
             nvPost := nvPost + 1; //no neighbors
          end; //not yet clustered
      end; //for each vertex
   end else begin //Radius > 0
     radiusSqr := sqr(Radius); //avoids calculating square-root for each comparison
     for i := 0 to (nv - 1) do begin
         if cluster[i] = i then begin //not part of previous cluster
            z := s[s[i].index].value;
            pt := vertices[s[i].index];
            sum := pt;
            dz := 0;
            j := i + 1;
            nc := 1;
            while (dz <= Radius) and (j < nv)  do begin
                  dz := abs(s[s[j].index].value - z);
                  //dx := vectorDistance(pt, vertices[s[j].index]);
                  dx := sqr(pt.X-vertices[s[j].index].X)+ sqr(pt.Y-vertices[s[j].index].Y) + sqr(pt.Z-vertices[s[j].index].Z);
                  if dx <= radiusSqr then begin
                     vectorAdd(sum, vertices[s[j].index]);
                     cluster[j] := nvPost;
                     remap[s[j].index] := nvPost;
                     nc := nc + 1;
                  end;
                  j := j + 1;
            end;
            newVert[nvPost] := vectorScale(sum, 1/nc);
            cluster[i] := nvPost;
            remap[s[i].index] := nvPost;
            nvPost := nvPost + 1; //no neighbors
         end; //not yet clustered
     end; //for each vertex
   end;
   if nvPost = nv then exit; //no clusters - no change!
   vertices := Copy(newVert, Low(newVert), nvPost);
   //remap faces to new vertices
   oldFaces := Copy(faces, Low(faces), Length(faces));
   setlength(faces,0);
   for i := 0 to (length(oldFaces) - 1) do begin
        face.X := remap[oldFaces[i].X];
        face.Y := remap[oldFaces[i].Y];
        face.Z := remap[oldFaces[i].Z];
        if (face.X <> face.Y) and (face.X <> face.Z) and (face.Y <> face.Z) then begin  //exclude degenerate faces
             setlength(Faces,length(Faces)+1);
             Faces[High(Faces)].X :=  face.X;
             Faces[High(Faces)].Y :=  face.Y;
             Faces[High(Faces)].Z :=  face.Z;
        end;

   end;
end;

procedure UnifyVertices(var faces: TFaces;  var vertices: TVertices; Radius: single);
//STL format saves raw vertices, this uses a lot of RAM and makes estimating vertex normals impossible...
// http://www.mathworks.com/matlabcentral/fileexchange/29986-patch-slim--patchslim-m-
var vStart, vEnd: integer;
begin
	if (Radius < 0) then exit;
	vStart := length(vertices);
    ClusterVertex(faces, vertices, Radius);
    vEnd :=  length(vertices);
    if vStart = vEnd then exit;
    if (Radius = 0) then
    	writeln(format(' removed identical vertices, reducing number from %d to %d', [vStart, vEnd ]))
    else
    	writeln(format(' removed identical(ish) vertices, reducing number from %d to %d', [vStart, vEnd ]));

end;

end.

