object Form1: TForm1
  Left = 198
  Top = 199
  BorderStyle = bsDialog
  Caption = 'Simplify'
  ClientHeight = 83
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 24
    Width = 465
    Height = 25
    Caption = 'Select OBJ file to simplify'
    TabOrder = 0
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.obj'
    Filter = 'OBJ mesh|*.obj'
    Left = 120
    Top = 8
  end
end
