unit SnapVirtualDataset;
{
Snap Object Dataset

Want you conciliate pure object oriented programming and delphi data aware controls?
Through Snap Object Dataset components you will be able to join your business logic
with the true rad powerfull delphi data aware controls; in this way your GUI will
continue to completely being detached from the persistence layer.

These components are completely free with source and demos

cosimo.demichele@libero.it
}
interface

uses
  Classes,
  DB,
  SnapBaseDataset;

type
  TSnapGetDataCountEvent = procedure(Sender: TSnapCustomDataset; var ACount: Integer) of object;
  TSnapGetDataValueEvent = procedure(Sender: TSnapCustomDataset; AField: TField; AIndex: Integer; var AValue: Variant) of object;
  TSnapSetDataValueEvent = procedure(Sender: TSnapCustomDataset; AField: TField; AIndex: Integer; AValue: Variant) of object;
  TSnapGetDataBlobValueEvent = procedure(Sender: TSnapCustomDataset; AField: TField; AIndex: Integer; AStream: TStream) of object;
  TSnapPostDataEvent = procedure(Sender: TSnapCustomDataset; AIndex: Integer) of object;

  TSnapCustomVirtualDataset = class(TSnapCustonIndexedDataset)
  private
    fOnGetDataValue: TSnapGetDataValueEvent;
    fOnSetDataValue: TSnapSetDataValueEvent;
    fOnGetDataCount: TSnapGetDataCountEvent;
    fOnSetDataBlobValue: TSnapGetDataBlobValueEvent;
    fOnGetDataBlobValue: TSnapGetDataBlobValueEvent;
    fSnapAddDataEvent: TSnapPostDataEvent;
    fSnapCancelDataEvent: TSnapPostDataEvent;
    fSnapPostDataEvent: TSnapPostDataEvent;
    fSnapDeleteDataEvent: TSnapPostDataEvent;
  protected
    function GetRecordCount: Integer; override;
    function GetCanModify: Boolean; override;
    procedure DoInsertRecord; override;
    procedure DoCancelRecord; override;
    procedure DoDeleteRecord; override;
    procedure DoPostRecord; override;

    procedure DoDataCount(var Count: integer); virtual;
    function GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; Value: Variant); override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
  public
    { events }
    property OnGetDataCount: TSnapGetDataCountEvent read fOnGetDataCount write fOnGetDataCount;
    property OnGetDataValue: TSnapGetDataValueEvent read fOnGetDataValue write fOnGetDataValue;
    property OnSetDataValue: TSnapSetDataValueEvent read fOnSetDataValue write fOnSetDataValue;
    property OnGetDataBlobValue: TSnapGetDataBlobValueEvent read fOnGetDataBlobValue write fOnGetDataBlobValue;
    property OnSetDataBlobValue: TSnapGetDataBlobValueEvent read fOnSetDataBlobValue write fOnSetDataBlobValue;
    property OnAddData: TSnapPostDataEvent read fSnapAddDataEvent write fSnapAddDataEvent;
    property OnCancelData: TSnapPostDataEvent read fSnapCancelDataEvent write fSnapCancelDataEvent;
    property OnPostData: TSnapPostDataEvent read fSnapPostDataEvent write fSnapPostDataEvent;
    property OnDeleteData: TSnapPostDataEvent read fSnapDeleteDataEvent write fSnapDeleteDataEvent;
  end;

  TSnapVirtualDataset = class(TSnapCustomVirtualDataset)
  published
    property Active;
    property ActiveIndex;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;

    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnNewRecord;
    property OnCalcFields;


    property MasterSource;
    property OnGetDataValue;
    property OnSetDataValue;
    property OnGetDataCount;
    property OnGetDataBlobValue;
    property OnSetDataBlobValue;
    property OnAddData;
    property OnCancelData;
    property OnPostData;
    property OnDeleteData;
  end;




implementation

{ TSnapCustomVirtualDataset }

function TSnapCustomVirtualDataset.GetRecordCount: Integer;
var
  n: integer;
begin
  n := 0;
  DoDataCount(n);
  Result := n;
end;

procedure TSnapCustomVirtualDataset.DoDataCount(var Count: integer);
begin
  if Assigned(fOnGetDataCount) then
    fOnGetDataCount(Self, Count);
end;

procedure TSnapCustomVirtualDataset.GetBlobField(Field: TField; Stream: TStream);
begin
  if Assigned(fOnGetDataBlobValue) then
    fOnGetDataBlobValue(Self, Field, FCurrent, Stream);
end;

function TSnapCustomVirtualDataset.GetFieldValue(Field: TField): Variant;
begin
  if Assigned(fOnGetDataValue) then
    fOnGetDataValue(Self, Field, FCurrent, Result);
end;

procedure TSnapCustomVirtualDataset.SetBlobField(Field: TField; Stream: TStream);
begin
  if Assigned(fOnSetDataBlobValue) then
    fOnSetDataBlobValue(Self, Field, FCurrent, Stream);
end;

procedure TSnapCustomVirtualDataset.SetFieldValue(Field: TField; Value: Variant);
begin
  if Assigned(fOnSetDataValue) then
    fOnSetDataValue(Self, Field, FCurrent, Value);
end;


function TSnapCustomVirtualDataset.GetCanModify: Boolean;
begin
  Result := True;
end;

procedure TSnapCustomVirtualDataset.DoInsertRecord;
begin
  if Assigned(fSnapAddDataEvent) then
    fSnapAddDataEvent(Self, FCurrent);
end;

procedure TSnapCustomVirtualDataset.DoCancelRecord;
begin
  if Assigned(fSnapCancelDataEvent) then
    fSnapCancelDataEvent(Self, FCurrent);
end;

procedure TSnapCustomVirtualDataset.DoDeleteRecord;
begin
  if Assigned(fSnapDeleteDataEvent) then
    fSnapDeleteDataEvent(Self, FCurrent);
end;

procedure TSnapCustomVirtualDataset.DoPostRecord;
begin
  inherited;
  if IsOpen then
  begin
    if Assigned(fSnapPostDataEvent) then
      fSnapPostDataEvent(Self, FCurrent);
  end;
end;

end.
