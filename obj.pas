unit obj;
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
interface
//load and save meshes in WaveFront OBJ format

uses
    Classes, sysutils,  meshify_simplify_quadric;

procedure LoadObj(const FileName: string; var faces: TFaces; var vertices: TVertices);
procedure SaveObj(const FileName: string; var faces: TFaces; var vertices: TVertices);

implementation

function FSize (lFName: String): longint;
var F : File Of byte;
begin
  result := 0;
  if not fileexists(lFName) then exit;
  Assign (F, lFName);
  Reset (F);
  result := FileSize(F);
  Close (F);
end;

procedure LoadObj(const FileName: string; var faces: TFaces; var vertices: TVertices);
//WaveFront Obj file used by Blender
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
const
  kBlockSize = 8192;
var
   f: TextFile;
   fsz : int64;
   s : string;
   strlst : TStringList;
   i,j, num_v, num_f, new_f: integer;
begin
     fsz := FSize (FileName);
     if fsz < 32 then exit;
     //init values
     num_v := 0;
     num_f := 0;
     strlst:=TStringList.Create;
     setlength(vertices, (fsz div 70)+kBlockSize); //guess number of faces based on filesize to reduce reallocation frequencey
     setlength(faces, (fsz div 35)+kBlockSize); //guess number of vertices based on filesize to reduce reallocation frequencey
     //load faces and vertices
     AssignFile(f, FileName);
     Reset(f);
     {$IFDEF FPC}DefaultFormatSettings.DecimalSeparator := '.';{$ELSE}DecimalSeparator := '.';{$ENDIF}
     while not EOF(f) do begin
        readln(f,s);
        if length(s) < 7 then continue;
        if (s[1] <> 'v') and (s[1] <> 'f') then continue; //only read 'f'ace and 'v'ertex lines
        if (s[2] = 'p') or (s[2] = 'n') or (s[2] = 't') then continue; //ignore vp/vn/vt data: avoid delimiting text yields 20% faster loads
        strlst.DelimitedText := s;
        if (strlst.count > 3) and ( (strlst[0]) = 'f') then begin
           //warning: need to handle "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3"
           //warning: face could be triangle, quad, or more vertices!
           new_f := strlst.count - 3;
           if ((num_f+new_f) >= length(faces)) then
              setlength(faces, length(faces)+new_f+kBlockSize);
           for i := 1 to (strlst.count-1) do
               if (pos('/', strlst[i]) > 1) then // "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3" -> f v1 v2 v3
                  strlst[i] := Copy(strlst[i], 1, pos('/', strlst[i])-1);
           for j := 1 to (new_f) do begin
               faces[num_f].X := strtointDef(strlst[1], 0) - 1; //-1 since "A valid vertex index starts from 1"
               faces[num_f].Y := strtointDef(strlst[j+1], 0) - 1; //-1 since "A valid vertex index starts from 1"
               faces[num_f].Z := strtointDef(strlst[j+2], 0) - 1; //-1 since "A valid vertex index starts from 1"
               inc(num_f);
           end;
        end;
        if (strlst.count > 3) and ( (strlst[0]) = 'v') then begin
           if ((num_v+1) >= length(vertices)) then
              setlength(vertices, length(vertices)+kBlockSize);
           vertices[num_v].X := strtofloatDef(strlst[1], 0);
           vertices[num_v].Y := strtofloatDef(strlst[2], 0);
           vertices[num_v].Z := strtofloatDef(strlst[3], 0);
           inc(num_v);
        end;
     end;
     CloseFile(f);
     strlst.free;
     setlength(faces, num_f);
     setlength(vertices, num_v);
end; // LoadObj()

procedure SaveObj(const FileName: string; var faces: TFaces; var vertices: TVertices);
//create WaveFront object file
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
var
   f : TextFile;
   FileNameObj: string;
   i : integer;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     writeln('You need to open a mesh before you can save it');
     exit;
  end;
  FileNameObj := changeFileExt(FileName, '.obj');
  AssignFile(f, FileNameObj);
  ReWrite(f);
  WriteLn(f, '# WaveFront Object format image created with Surf Ice');
  for i := 0 to (length(vertices)-1) do
      WriteLn(f, 'v ' + floattostr(vertices[i].X)+' '+floattostr(vertices[i].Y)+' '+ floattostr(vertices[i].Z));
  for i := 0 to (length(faces)-1) do
      WriteLn(f, 'f ' + inttostr(faces[i].X+1)+' '+inttostr(faces[i].Y+1)+' '+ inttostr(faces[i].Z+1)); //+1 since "A valid vertex index starts from 1 "
  CloseFile(f);
end;


end.

