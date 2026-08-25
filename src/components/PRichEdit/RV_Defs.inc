       { Determine Delphi/C++Builder version }

{$IFNDEF VER80}                         {       not Delphi 1.0           }
  {$IFNDEF VER90}                       {       not Delphi 2.0           }
    {$DEFINE RICHVIEWCBDEF1}            (* C++Builder 1.0+              *)
    {$IFNDEF VER93}                     {       not C++Builder 1.0       }
      {$DEFINE RICHVIEWCBDEF3}          (* C++Builder 3.0+              *)
      {$IFNDEF VER110}                  {       not C++Builder 3.0       }
        {$DEFINE RICHVIEWDEF3}          (* Delphi 3.0+, C++Builder 4.0+ *)
        {$IFNDEF VER100}                {       not Delphi 3.0           }
          {$DEFINE RICHVIEWDEF4}        (* Delphi 4.0+                  *)
          {$IFNDEF VER120}              {       not Delphi 4.0           }
            {$IFNDEF VER125}            {       not C++Builder 4.0       }
              {$DEFINE RICHVIEWDEF5}    (* Delphi 5.0+                  *)
              {$IFNDEF VER130}          {       not Delphi 5.0           }
                 {$IFNDEF VER135}       {       not C++Builder 5.0       }
                   {$DEFINE RICHVIEWDEF6}(* Delphi 6.0+                 *)
                 {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
