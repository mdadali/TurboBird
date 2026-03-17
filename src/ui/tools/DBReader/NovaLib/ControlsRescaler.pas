{ Рескалер контролов - автоматизатор масштабирования и размещения контролов
  Меняет положение и размер контролов в соответствии с привязками сторон

(C) Sergey Bodrov (serbod@gmail.com) 2017

 Пример:
 [ed11       ] [ed12]
 [ed21] [ed22] [    ]

// ed11 имеет масштаб 2/3, привязан левой стороной к форме,
// правой стороной к размеру формы
SetControlPos(ControlPos, ed11, 2/3);
SetSideAnchor(ControlPos.AnchorLeft, Self, asSame);
SetSideAnchor(ControlPos.AnchorRight, Self, asSize);
AddControlPosition(ControlPos);

// ed12 привязан левой стороной к ed11, правой к границе формы
SetControlPos(ControlPos, ed12);
SetSideAnchor(ControlPos.AnchorLeft, ed11, asOpposite);
SetSideAnchor(ControlPos.AnchorRight, Self, asSame);
AddControlPosition(ControlPos);

// ed21 привязан верхней стороной к нижней стороне ed11
// левой стороной к форме
// правой стороной к ed12, его противоположной стороне (левой)
// масштаб задан 1/2
SetControlPos(ControlPos, ed21, 1/2);
SetSideAnchor(ControlPos.AnchorTop, ed11, asOpposite);
SetSideAnchor(ControlPos.AnchorLeft, Self, asSame);
SetSideAnchor(ControlPos.AnchorRight, ed12, asOpposite);
AddControlPosition(ControlPos);

// ed22 привязан левой стороной к ed21, правой стороной привязан к ed12
SetControlPos(ControlPos, ed22);
SetSideAnchor(ControlPos.AnchorLeft, ed21, asOpposite);
SetSideAnchor(ControlPos.AnchorRight, ed12, asOpposite);
AddControlPosition(ControlPos);

 }
unit ControlsRescaler;

interface

uses Classes, Controls, Types;

type
  TAnchorSide = (asNone, asSame, asOpposite, asSize);

  { Описание привязки к соседнему контролу, по одной из сторон }
  TControlSideAnchor = record
    { Соседний контрол }
    Sibling: TControl;
    { Строна соседнего контрола, к которой привязка
      asNone - нет привязки
      asSame - такая же сторона
      asOpposite - противоположная сторона
      asSize - привязка к размеру между сторонами }
    SiblingSide: TAnchorSide;
    { Промежуток, пикселей }
    Margin: Integer;
  end;

  { описание позиции и привязок контрола }
  TControlPosition = record
    { Контрол }
    Control: TControl;
    { Базовое положение контрола на форме }
    BasePos: TPoint;
    { Базовый размер контрола, пикселей }
    BaseSize: TPoint;
    { Промежуток вокруг контрола, пикселей }
    MarginAround: Integer;
    { описание привязки верхней границы }
    AnchorTop: TControlSideAnchor;
    { описание привязки нижней границы }
    AnchorLeft: TControlSideAnchor;
    { Привязка к правому соседу }
    AnchorRight: TControlSideAnchor;
    { Привязка к нижнему соседу }
    AnchorBottom: TControlSideAnchor;
    // horizontal scale: 1-full scale, 0.5 - half scale
    HScale: Real;
  end;

  TControlsRescaler = class(TComponent)
  private
    procedure AlignControlPos(const AConPos: TControlPosition);
  public
    { Массив описаний позиций и привязок }
    ControlPosArray: array of TControlPosition;
    procedure AfterConstruction(); override;
    { Установка значений структуры AControlPos из контрола AControl
      AHScale - коэффициент масштабирования по горизонтали, по умолчанию 1
      AMargin - промежуток вокруг контрола, по умолчанию 2 }
    procedure SetControlPos(var AControlPos: TControlPosition; AControl: TControl; AHScale: Real = 1; AMargin: Integer = 2);
    { Добавляет заданное описание привязки в рескалер }
    procedure AddControlPosition(const Value: TControlPosition);
    { Установка привязки к соседнему контролу, по одной из сторон
      ASideAnchor - описание привязки к одной из сторон
      ASibling - Соседний контрол, по умолчанию не задан
      ASiblingSide - Строна соседнего контрола, к которой привязка
        asNone - нет привязки
        asSame - такая же сторона
        asOpposite - противоположная сторона
        asSize - привязка к размеру между сторонами
      AMargin - Промежуток, пикселей. По умолчанию 0 }
    procedure SetSideAnchor(var ASideAnchor: TControlSideAnchor; ASibling: TControl = nil;
                      ASiblingSide: TAnchorSide = asNone; AMargin: Integer = 0);
    { Пересчет позиций и размеров контролов в соответствии с их привязками }
    procedure RescaleControls(ABaseFormSize: TPoint);
  end;

implementation

procedure TControlsRescaler.AddControlPosition(const Value: TControlPosition);
var
  n: Integer;
begin
  n := Length(ControlPosArray);
  SetLength(ControlPosArray, n+1);
  ControlPosArray[n] := Value;
end;

procedure TControlsRescaler.SetSideAnchor(var ASideAnchor: TControlSideAnchor; ASibling: TControl = nil;
                    ASiblingSide: TAnchorSide = asNone; AMargin: Integer = 0);
begin
  ASideAnchor.Sibling := ASibling;
  ASideAnchor.SiblingSide := ASiblingSide;
  ASideAnchor.Margin := AMargin;
end;

procedure TControlsRescaler.SetControlPos(var AControlPos: TControlPosition; AControl: TControl; AHScale: Real = 1; AMargin: Integer = 2);
begin
  AControlPos.Control := AControl;
  AControlPos.BasePos.X := AControl.Left;
  AControlPos.BasePos.Y := AControl.Top;
  AControlPos.BaseSize.X := AControl.Width;
  AControlPos.BaseSize.Y := AControl.Height;
  AControlPos.MarginAround := AMargin;
  AControlPos.HScale := AHScale;
  SetSideAnchor(AControlPos.AnchorTop);
  SetSideAnchor(AControlPos.AnchorLeft);
  SetSideAnchor(AControlPos.AnchorRight);
  SetSideAnchor(AControlPos.AnchorBottom);
end;

procedure TControlsRescaler.AlignControlPos(const AConPos: TControlPosition);
var
  Con: TControl;
  ConPos, ConSize: TPoint;
  Sibling: TControl;
  Anchor: TControlSideAnchor;
  n, posL: Integer;
begin
  if not Assigned(AConPos.Control) then Exit;
  Con := AConPos.Control;
  ConPos.X := Con.Left;
  ConPos.Y := Con.Top;
  ConSize.X := Con.Width;
  ConSize.Y := Con.Height;

  Anchor := AConPos.AnchorTop;
  Sibling := Anchor.Sibling;
  if Assigned(Sibling) then
  begin
    // Align to Top
    case Anchor.SiblingSide of
      asSame: ConPos.Y := Sibling.Top + AConPos.MarginAround + Anchor.Margin;
      asOpposite: ConPos.Y := Sibling.Top + Sibling.Height + AConPos.MarginAround + Anchor.Margin;
    end;
  end;

  Anchor := AConPos.AnchorLeft;
  Sibling := Anchor.Sibling;
  if Assigned(Sibling) then
  begin
    // Align to Left
    case Anchor.SiblingSide of
      asSame: ConPos.X := Sibling.Left + AConPos.MarginAround + Anchor.Margin;
      asOpposite: ConPos.X := Sibling.Left + Sibling.Width + AConPos.MarginAround + Anchor.Margin;
      //asProportional:   // only width affected
    end;
  end;

  Anchor := AConPos.AnchorRight;
  Sibling := Anchor.Sibling;
  if Assigned(Sibling) then
  begin
    // Align to Right
    n := AConPos.BaseSize.X;
    posL := ConPos.X;
    if Assigned(AConPos.AnchorLeft.Sibling) then
    begin
      if AConPos.AnchorLeft.SiblingSide = asSize then
      begin
        n := Trunc(Sibling.Width * AConPos.HScale) - AConPos.MarginAround - AConPos.AnchorLeft.Margin;
        posL := Sibling.Width - n;
      end;

      // stretch right side to sibling
      case Anchor.SiblingSide of
        asSame: n := Sibling.Left + Sibling.Width - AConPos.MarginAround - Anchor.Margin - posL;
        asOpposite: n := Sibling.Left - AConPos.MarginAround - Anchor.Margin - posL;
      end;
      if Con.Align = alNone then
        ConPos.X := posL;
      if AConPos.AnchorLeft.SiblingSide = asSize then
        ConSize.X := n
      else
        ConSize.X := Trunc(n * AConPos.HScale);
    end
    else
    begin
      // align to right side, change width and position
      ConSize.X := Trunc(n * AConPos.HScale);
      if Con.Align = alNone then
      begin
        n := ConPos.X;
        case Anchor.SiblingSide of
          asSame: n := Sibling.Left + Sibling.Width - AConPos.MarginAround - Anchor.Margin - ConSize.X;
          asOpposite: n := Sibling.Left - AConPos.MarginAround - Anchor.Margin - ConSize.X;
        end;
        ConPos.X := n;
      end;
    end;
  end;

  Anchor := AConPos.AnchorBottom;
  Sibling := Anchor.Sibling;
  if Assigned(Sibling) then
  begin
    // Align to Bottom
    case Anchor.SiblingSide of
      asSame: ConSize.Y := Sibling.Top + Sibling.Height - AConPos.MarginAround - Anchor.Margin - ConPos.Y;
      asOpposite: ConSize.Y := Sibling.Top - AConPos.MarginAround - Anchor.Margin - ConPos.Y;
    end;
  end;

  Con.SetBounds(ConPos.X, ConPos.Y, ConSize.X, ConSize.Y);
end;

procedure TControlsRescaler.RescaleControls(ABaseFormSize: TPoint);
var
  i: Integer;
begin
  for i := 0 to Length(ControlPosArray)-1 do
  begin
    AlignControlPos(ControlPosArray[i]);
  end;
end;
procedure TControlsRescaler.AfterConstruction;
begin
  inherited;
  SetLength(ControlPosArray, 0);
end;

end.
