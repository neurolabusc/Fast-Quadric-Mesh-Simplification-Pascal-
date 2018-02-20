unit mz3;
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
interface
//load and save meshes in Surfice MZ3 format

uses
    zstream, Classes, sysutils,  meshify_simplify_quadric;

procedure LoadMz3(const FileName: string; var faces: TFaces; var vertices: TVertices);
procedure SaveMz3(const FileName: string; var faces: TFaces; var vertices: TVertices);

implementation

type
   TRGBA = packed record //Next: analyze Format Header structure
    R,G,B,A : byte;
  end;
  TFloats = array of single;
  TVertexRGBA = array of TRGBA;

function FSize (lFName: String): longint;
var F : File Of byte;
begin
  result := 0;
  if not fileexists(lFName) then exit;
  Assign (F, lFName);
  Reset (F);
  result := FileSize(F);
  Close (F);
end; // FSize()

function LoadMz3Core(const FileName: string; var Faces: TFaces; var Vertices: TVertices; var vertexRGBA: TVertexRGBA; var intensity: TFloats): boolean;
const
 kMagic =  23117; //"MZ"
 kChunkSize = 16384;
label 666;
var
  i: integer;
  bytes : array of byte;
  Magic, Attr: uint16;
  nFace, nVert, nSkip: uint32;
  isFace, isVert, isRGBA, isScalar: boolean;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
begin
     result := false;
     setlength(Faces,0);
     setlength(Vertices,0);
     setlength(vertexRGBA,0);
     setlength(intensity,0);
     if not fileexists(Filename) then exit;
     mStream := TMemoryStream.Create;
     zStream := TGZFileStream.create(FileName, gzopenread);
     setlength(bytes, kChunkSize);
     repeat
            i := zStream.read(bytes[0],kChunkSize);
            mStream.Write(bytes[0],i) ;
     until i < kChunkSize;
     zStream.Free;
     if mStream.Size < 72 then exit; //6 item header, 3 vertices (3 values, XYZ), 1 face (3 indices) each 4 bytes
     mStream.Position := 0;
     mStream.Read(Magic,2);
     mStream.Read(Attr,2);
     mStream.Read(nFace,4);
     mStream.Read(nVert,4);
     mStream.Read(nSkip,4);
     if (magic <> kMagic) then goto 666;
     isFace := (Attr and 1) > 0;
     isVert := (Attr and 2) > 0;
     isRGBA := (Attr and 4) > 0;
     isScalar := (Attr and 8) > 0;
     if (Attr > 15) then begin
        writeln('Unsupported future format '+ inttostr(Attr));
        goto 666;
     end;
     if (nFace = 0) and (isFace) then goto 666;
     if (nVert = 0) and ((isVert) or (isRGBA) or (isScalar) ) then goto 666;
     if nSkip > 0 then
        mStream.Seek(nSkip, soFromCurrent);
     result := true;
     if isFace then begin
        setlength(Faces,  nFace);
        mStream.Read(Faces[0], nFace * 3 * sizeof(int32));
     end;
     if isVert then begin
        setlength(Vertices,  nVert);
        mStream.Read(Vertices[0], nVert * 3 * sizeof(single));
     end;
     if isRGBA then begin
        setlength(vertexRGBA, nVert);
        mStream.Read(vertexRGBA[0], nVert * 4 * sizeof(byte));
     end;
     if isScalar then begin
        setlength(intensity, nVert);
        mStream.Read(intensity[0], nVert * sizeof(single));
     end;
     result := true;
   666 :
     mStream.Free;
end; // LoadMz3Core()

procedure LoadMz3(const FileName: string; var faces: TFaces; var vertices: TVertices);
var
	vertexRGBA: TVertexRGBA;
	intensity: TFloats;
begin
	LoadMz3Core(FileName, Faces, Vertices, vertexRGBA, intensity);
	if length(vertexRGBA) > 0 then begin
		writeln('Warning: ignoring vertex colors');
		setlength(vertexRGBA, 0);
	end;
	if length(intensity) > 0 then begin
		writeln('Warning: ignoring vertex intensities');
		setlength(intensity, 0);
	end;
end; // LoadMz3()

procedure SaveMz3Core(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA; intensity: TFloats);
const
 kMagic =  23117; //"MZ"
var
  isFace, isVert, isRGBA, isScalar: boolean;
  Magic, Attr: uint16;
  nFace, nVert, nSkip: uint32;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  FileNameMz3: string;
begin
  nFace := length(Faces);
  nVert := length(Vertices);
  FileNameMz3 := changeFileExt(FileName, '.mz3');
  isFace := nFace > 0;
  isVert := nVert > 0;
  isRGBA := false;
  if length(vertexRGBA) > 0 then begin
     isRGBA := true;
     nVert := length(vertexRGBA);
  end;
  isScalar := false;
  if length(intensity) > 0 then begin
     isScalar := true;
     nVert := length(intensity);
  end;
  if (nFace = 0) and (nVert = 0) then exit;
  magic := kMagic;
  Attr := 0;
  if isFace then Attr := Attr + 1;
  if isVert then Attr := Attr + 2;
  if isRGBA then Attr := Attr + 4;
  if isScalar then Attr := Attr + 8;
  nSkip := 0; //do not pad header with any extra data
  mStream := TMemoryStream.Create;
  mStream.Write(Magic,2);
  mStream.Write(Attr,2);
  mStream.Write(nFace,4);
  mStream.Write(nVert,4);
  mStream.Write(nSkip,4);
  if isFace then
     mStream.Write(Faces[0], nFace * sizeof(TPoint3i));
  if isVert  then
     mStream.Write(Vertices[0], nVert * 3 * sizeof(single));
  if isRGBA then
     mStream.Write(vertexRGBA[0], nVert * 4 * sizeof(byte));
  if isScalar then
     mStream.Write(intensity[0], nVert * sizeof(single));
  mStream.Position := 0;
  zStream := TGZFileStream.Create(FileNameMz3, gzopenwrite);
  zStream.CopyFrom(mStream, mStream.Size);
  zStream.Free;
  mStream.Free;
end; // SaveMz3Core()

procedure SaveMz3(const FileName: string; var faces: TFaces; var vertices: TVertices);
var
	vertexRGBA: TVertexRGBA;
	intensity: TFloats;
begin
	setlength(vertexRGBA, 0);
	setlength(intensity,0);
	SaveMz3Core(FileName, faces, vertices, vertexRGBA, intensity);
end; //SaveMz3()

end.
