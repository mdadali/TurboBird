unit SpeechModule;

interface

uses
  Windows, Classes, ExtCtrls, Speech, ActiveX, ComObj, SysUtils;

type
  TSpeechUnit = class(TObject)
  private
    MessageQueue: TStringList;
    SpeechTimer: TTimer;
    // Центральный интерфейс, через который производятся все действия с речью}
    fITTSCentral: ITTSCentral;
    // Интерфейс для связи с аудиоустройством
    fIAMM: IAudioMultimediaDevice;
    // Интерфейс для перебора движков
    aTTSEnum: ITTSEnum;
    // Указатель на параметры движка
    fpModeInfo: PTTSModeInfo;
    // Список движков
    VoicesList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SelectVoiceEngine(EngineIndex: Integer);
    function GetVoicesList: TStringList;
    procedure SpeechText(const AText: string);
    procedure AddSpeechText(const AText: string);
  published
    procedure SpeechTextFromQueue(Sender: TObject);
  end;

{ Возвращает текстовое представление времени, например: "пять часов семь минут" }
function GetTimeString(ATime: TDateTime): string;

implementation

function GetTimeString(ATime: TDateTime): string;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(ATime, Hour, Min, Sec, MSec);
  case Hour of
    0: Result := 'ноль часов ';
    1: Result := 'один час ';
    2: Result := 'два часа ';
    3: Result := 'три часа ';
    4: Result := 'четыре часа ';
    5: Result := 'пять часов ';
    6: Result := 'шесть часов ';
    7: Result := 'семь часов ';
    8: Result := 'восемь часов ';
    9: Result := 'девять часов ';
    10: Result := 'десять часов ';
    11: Result := 'одиннадцать часов ';
    12: Result := 'двенадцать часов ';
    13: Result := 'тринадцать часов ';
    14: Result := 'четырнадцать часов ';
    15: Result := 'пятнадцать часов ';
    16: Result := 'шестнадцать часов ';
    17: Result := 'семнадцать часов ';
    18: Result := 'восемнадцать часов ';
    19: Result := 'девятнадцать часов ';
    20: Result := 'двадцать часов ';
    21: Result := 'двадцать один час ';
    22: Result := 'двадцать два часа ';
    23: Result := 'двадцать три часа ';
  end;

  case Min of
    0: Result := Result + 'ноль минут';
    1: Result := Result + 'одна минута';
    2: Result := Result + 'две минуты';
    3: Result := Result + 'три минуты';
    4: Result := Result + 'четыре минуты';
    5: Result := Result + 'пять минут';
    6: Result := Result + 'шесть минут';
    7: Result := Result + 'семь минут';
    8: Result := Result + 'восемь минут';
    9: Result := Result + 'девять минут';
    10: Result := Result + 'десять минут';
    11: Result := Result + 'одиннадцать минут';
    12: Result := Result + 'двенадцать минут';
    13: Result := Result + 'тринадцать минут';
    14: Result := Result + 'четырнадцать минут';
    15: Result := Result + 'пятнадцать минут';
    16: Result := Result + 'шестнадцать минут';
    17: Result := Result + 'семнадцать минут';
    18: Result := Result + 'восемнадцать минут';
    19: Result := Result + 'девятнадцать минут';
    20: Result := Result + 'двадцать минут';
    21: Result := Result + 'двадцать одна минута';
    22: Result := Result + 'двадцать две минуты';
    23: Result := Result + 'двадцать три минуты';
    24: Result := Result + 'двадцать четыре минуты';
    25: Result := Result + 'двадцать пять минут';
    26: Result := Result + 'двадцать шесть минут';
    27: Result := Result + 'двадцать семь минут';
    28: Result := Result + 'двадцать восемь минут';
    29: Result := Result + 'двадцать девять минут';
    30: Result := Result + 'тридцать минут';
    31: Result := Result + 'тридцать одна минута';
    32: Result := Result + 'тридцать две минуты';
    33: Result := Result + 'тридцать три минуты';
    34: Result := Result + 'тридцать четыре минуты';
    35: Result := Result + 'тридцать пять минут';
    36: Result := Result + 'тридцать шесть минут';
    37: Result := Result + 'тридцать семь минут';
    38: Result := Result + 'тридцать восемь минут';
    39: Result := Result + 'тридцать девять минут';
    40: Result := Result + 'сорок минут';
    41: Result := Result + 'сорок одна минута';
    42: Result := Result + 'сорок две минуты';
    43: Result := Result + 'сорок три минуты';
    44: Result := Result + 'сорок четыре минуты';
    45: Result := Result + 'сорок пять минут';
    46: Result := Result + 'сорок шесть минут';
    47: Result := Result + 'сорок семь минут';
    48: Result := Result + 'сорок восемь минут';
    49: Result := Result + 'сорок девять минут';
    50: Result := Result + 'пятьдесят минут';
    51: Result := Result + 'пятьдесят одна минута';
    52: Result := Result + 'пятьдесят две минуты';
    53: Result := Result + 'пятьдесят три минуты';
    54: Result := Result + 'пятьдесят четыре минуты';
    55: Result := Result + 'пятьдесят пять минут';
    56: Result := Result + 'пятьдесят шесть минут';
    57: Result := Result + 'пятьдесят семь минут';
    58: Result := Result + 'пятьдесят восемь минут';
    59: Result := Result + 'пятьдесят девять минут';
  end;
end;

constructor TSpeechUnit.Create();
var 
  NumFound : DWord;
  ModeInfo : TTSModeInfo;
  res: HRESULT;
begin
  inherited;

  SpeechTimer := TTimer.Create(nil);
  SpeechTimer.Interval := 500;
  SpeechTimer.OnTimer := SpeechTextFromQueue;
  MessageQueue := TStringList.Create;
  VoicesList := TStringList.Create;

  // Инициализация аудиоустройства
  res := CoCreateInstance(CLSID_MMAudioDest, nil, CLSCTX_ALL,  IID_IAudioMultiMediaDevice, fIAMM);
  if res = S_OK then
  begin
    // Создание перечисляемого объекта для перебора всех движков в системе с помощью интерфейса ITTSEnum
    res := CoCreateInstance(CLSID_TTSEnumerator, nil, CLSCTX_ALL, IID_ITTSEnum, aTTSEnum);
    if res = S_OK then
    begin
      // Сбрасываем на первый
      res := aTTSEnum.Reset();
      if res = S_OK then
      begin
        // Получаем первый движок
        res := aTTSEnum.Next(1, ModeInfo, @NumFound);
        while (res = S_OK) and (NumFound > 0) do
        begin
          VoicesList.Add(String(ModeInfo.szModeName));
          // Получаем остальные
          res := aTTSEnum.Next(1, ModeInfo, @NumFound);
        end;
      end;
    end;
  end;


  if VoicesList.Count > 0 then
    SelectVoiceEngine(0);
end;

destructor TSpeechUnit.Destroy;
begin
  if Assigned(fpModeInfo) then
  begin
    Dispose(fpModeInfo);
  end;
  FreeAndNil(VoicesList);
  FreeAndNil(MessageQueue);
  FreeAndNil(SpeechTimer);
  inherited;
end;

procedure TSpeechUnit.SpeechTextFromQueue(Sender: TObject);
begin
  if MessageQueue.Count > 0 then
  begin
    SpeechText(MessageQueue.Strings[0]);
    MessageQueue.Delete(0);
  end;
end;

function TSpeechUnit.GetVoicesList: TStringList;
begin
  Result := VoicesList;
end;

procedure TSpeechUnit.SelectVoiceEngine(EngineIndex: Integer);
var
  NumFound: DWord;
  ModeInfo : TTSModeInfo;
  res: HRESULT;
begin
  if EngineIndex < 0 then
    Exit;

  res := CoCreateInstance(CLSID_MMAudioDest, nil, CLSCTX_ALL,  IID_IAudioMultiMediaDevice, fIAMM);
  if res = S_OK then
  begin
    res := CoCreateInstance(CLSID_TTSEnumerator, nil, CLSCTX_ALL, IID_ITTSEnum, aTTSEnum);
    if res = S_OK then
    begin
      res := aTTSEnum.Reset;
      if res = S_OK then
      begin
        res := aTTSEnum.skip(EngineIndex);
        if res = S_OK then
        begin
          res := aTTSEnum.Next(1, ModeInfo, @NumFound);
          if Assigned(fpModeInfo) then
          begin
            // если fpModeInfo не равен nil
            Dispose(fpModeInfo);
            fpModeInfo := nil;
          end;
          if (res = S_OK) then
          begin
            New(fpModeInfo);
            fpModeInfo^ := ModeInfo;
            // загружаем движок по его GUID
            aTTSEnum.Select(fpModeInfo^.gModeID, fITTSCentral, IUnknown(fIAMM));
          end;
        end;
      end;
    end;
  end;
end;

procedure TSpeechUnit.SpeechText(const AText: String);
var
  SData: TSData;
begin
  if Assigned(fITTSCentral) then
  begin
    SData.dwSize := Length(AText) + 1;
    SData.pData := PChar(AText);
    try
      fITTSCentral.TextData(CHARSET_TEXT, 0, SData, nil, IID_ITTSBufNotifySink);
    except
    end;
  end;
end;

procedure TSpeechUnit.AddSpeechText(const AText: String);
begin
  MessageQueue.Add(AText);
end;


end.
