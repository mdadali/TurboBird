# DBReader
Database files reader. Directly read data from database files. No need for SQL server, DLLs, ODBC and other API.

Supported databases:
* Interbase 6, 7 (.GDB, .FDB)
* Firebird 2.1 up to 3.0 (.GDB, .FDB)
* Midas.dll/DataSnap/ClientDataSet (.CDS)
* Paradox 3.0 up to 7.x (.DB)
* dBASE/FoxPRO (.DBF)
* Mapsoft data export (.GSR)
* MS SQL Server (.MDF, .BAK)
* MS Access/MS Jet 4 (.MDB, .ACCDB)
* MS Outlook (.PST)
* MS Exchange/MS ESE (.EDB)
* MySQL InnoDB (.IBD)
* SQLite (.DB3, .SQLITE)
* Sybase SQL Anywhere (.DB)
* ElevateDB/DBISAM (.DAT)
* Clarion/TopSpeed (.TPS, .DAT)
* RaimaDB/db_VISTA/Velocis/RDM (.DBD)

Sample projects:
* Database browser - browse tables and inspect values

Dependencies:
* NovaLib (logger, RFUtils, MemStreams) - https://github.com/serbod/NovaLib
* DelphiZLib 1.2.8 (optional)