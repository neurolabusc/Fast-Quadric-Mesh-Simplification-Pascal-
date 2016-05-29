unit gui_main;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses
  {$IFNDEF FPC}Windows,{$ELSE}DateUtils,{$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, obj, meshify_simplify_quadric;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFDEF FPC}{$R *.lfm}{$ELSE}{$R *.dfm}{$ENDIF}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  ratio: single;
  agress: integer;
  outname, inname : string;
  targetTri, startTri: integer;
  faces: TFaces;
  vertices: TVertices;
  {$IFDEF FPC} {$IFDEF DARWIN} msec: qWord;{$ELSE} msec: Int64; tic :TDateTime; {$ENDIF} {$ELSE} msec: dWord; {$ENDIF}
begin
  if not OpenDialog1.Execute then exit;
  inname := OpenDialog1.FileName;
  agress := 5;
  ratio := 0.25;
  outname := ChangeFileExt(OpenDialog1.FileName, '.slim.obj');
  LoadObj(inname, faces, vertices);
  startTri := length(faces);
  targetTri := round(length(faces) * ratio);
  if (targetTri < 0) or (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to load a mesh (File/Open) before you can simplify a mesh');
     exit;
  end;
  {$IFDEF FPC} {$IFDEF DARWIN} msec := GetTickCount64(); {$ELSE}tic := Now();{$ENDIF} {$ELSE} msec := GetTickCount();{$ENDIF}
  simplify_mesh(faces, vertices, targetTri, agress);
  {$IFDEF FPC} {$IFDEF DARWIN} msec := GetTickCount64()-msec; {$ELSE}msec := MilliSecondsBetween(Now(),tic);{$ENDIF} {$ELSE} msec := GetTickCount() - msec; {$ENDIF}
  caption := (format(' faces %d -> %d (%.3f, %.2fsec)', [startTri, length(Faces), length(Faces)/startTri, msec*0.001  ]));
  SaveObj(outname, faces, vertices);
end;

end.

