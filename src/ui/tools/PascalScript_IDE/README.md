# PascalScript Runtime FormDesigner & Debugger

![PascalScript IDE](source/images/PascalScript_IDE.gif)

PascalScriptDesigner is a cross-platform IDE for PascalScript that combines two worlds:

- **Drag-&-Drop Form Designer** (based on https://github.com/havlicekp/form-designer)  
- **PascalScript Console with Debugger** (based on RemObjects PascalScript IDE-Demo)  


## Key Features

- **Drag & Drop Form Designer** – place components using the mouse.  
- **Automatic Code Linking** – double-click on a designer element jumps directly to the code location.  
- **Debugger Support** – breakpoints, Step Into / Step Over, variable watch.  
- **Cross-platform** – Linux & Windows

## Extended Features through Modifications

- **PascalScript Debugger**: Checks whether code exists at a specific file/line  
- **JvDesignSurface**: Double-click event (`OnControlDblClick`) for designer components  
- ⚙ **Modified Components**  

All modifications are located in the directory `source/components`.

## PascalScript (Debugger)

### New function in `PascalScript.pas`:

```pascal
function TPSCustomDebugExec.HasCode(Filename: string; LineNo: integer): boolean;
var
  i, j: integer;
  fi: PFunctionInfo;
  pt: TIfList;
  r: PPositionData;
begin
  result := false;
  for i := 0 to FDebugDataForProcs.Count - 1 do
  begin
    fi := FDebugDataForProcs[i];
    pt := fi^.FPositionTable;
    for j := 0 to pt.Count - 1 do
    begin
      r := pt[j];
      result := SameText(r^.FileName, Filename) and (r^.Row = LineNo);
      if result then exit;
    end;
  end;
end;
```

Source: StackOverflow: *Making an IDE using PascalScript and SynEdit*

### Modifications in `JvDesignSurface.pas`:

```pascal
type
  TJvDesignControlEvent = procedure(Sender: TObject; AControl: TControl) of object;

  TJvDesignSurface = class(TComponent)
  private
    FOnControlDblClick: TJvDesignControlEvent;
  published
    property OnControlDblClick: TJvDesignControlEvent read FOnControlDblClick write FOnControlDblClick;
  end;
```

- Double-clicking a component triggers `OnControlDblClick`  
- Changes in `TJvDesignCustomMessenger.IsDesignMessage` enable correct handling of design messages  

## ⚠ Known Limitations / Bugs

- Events are triggered only on 32-bit versions (Linux & Windows), since the PascalScript port in Lazarus currently supports only 32-bit  
- **Alpha Status**: The project is experimental and may be unstable

