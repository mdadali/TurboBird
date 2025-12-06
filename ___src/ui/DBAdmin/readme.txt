The purpose of this example is to both demonstrate Database Management using IBX and to provide a usable tool for Database Administration. 

Unlike the Services API example, the program is focused on a specific database. While it also uses the Services API for server level functions they are always used in the context of the current database. The program is intended to demonstrate:

* Access to Database Parameters and their management
* Database Backup and Restore
* Database File Management including Secondary Files and Shadow File sets
* Inspection and Management of Database Attachments
* Access to Database Statistics
* Database schema listing
* Access to Server Properties and the Server Log
* User Management
* Monitoring of User Mappings
* Monitoring of User/Role Access Rights including the identification of “stale” user rights.
* Database Validation and Repair
* Limbo Transaction Resolution.

The example is discussed in detail in section 3 of the User Guide "Firebird Server Management using IBX" in the ibx/docs directory.
