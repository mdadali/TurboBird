/* This test illustrates the different ways it is possible to define a stored procedure */

/*Case 1. No statement terminator - also tests ignoring reserved words in comments*/

Create or alter Procedure MyProc1
As
  Declare THECOUNTRY VarChar(32);
Begin
 --Begin
 THECOUNTRY = '';
  Update COUNTRY SET COUNTRY = 'None' Where COUNTRY = :THECOUNTRY;
 /* End */
End


/*Case 2. ';' as statement terminator*/

Create or alter Procedure MyProc2
As
Begin
  Update COUNTRY SET COUNTRY = 'None' Where COUNTRY = '';
End;

/*Case 3. '^' as statement terminator*/

set term ^;
Create or alter Procedure MyProc3
As
Begin
  Update COUNTRY SET COUNTRY = 'None' Where COUNTRY = '';
End^
set term ;^



