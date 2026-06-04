PascalScript Forms – Storage Convention

TurboBird stores custom forms in the following directory:

text
[TurboBird]/data/PSStudio/Forms/<Server>/<DBAlias>/<FormName>/
Example:

text
/opt/TurboBird/data/PSStudio/Forms/MyServer/EmployeeDB/CustomerEditor/CustomerEditor.cfrm
/opt/TurboBird/data/PSStudio/Forms/MyServer/EmployeeDB/CustomerEditor/CustomerEditor.rops
Important Rules:

Each form resides in its own subdirectory, whose name matches the form name.

The directory contains exactly two files:

<FormName>.cfrm – the form layout

<FormName>.rops – the Pascal script

The folder name and both file names (without extension) must be exactly identical – including case sensitivity.

The file extensions must always be written in lowercase (.cfrm, .rops).

Server and database aliases are automatically sanitized by TurboBird (special characters are replaced with _).

Only directories that meet these conditions are recognized by TurboBird as valid forms and displayed in the tree under Forms.
