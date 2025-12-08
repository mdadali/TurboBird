Script Engine Playground
========================

This example application illustrates use of the TIBXScript SQL script engine. It works with
the example employee database and comes with various test scripts to illustrate how it works.
These are all located in the "tests" directory.

Compile and run the application after first ensuring that the example employee database is
available on the local server. If it is on a remote server, then you will have to adjust
the IBDatabase1.DatabaseName property accordingly.

You can just type SQL queries into the left hand text box and click on "Execute" to
run them. The results appear in the right hand text box. Select queries are supported
by opening a new dynamically created window with a grid containing the query results. This
window is non-modal and multiple query results can be shown simultaneously. The grid
is a TIBDynamicGrid and clicking on the column header will resort the grid using the
selected column.

The test scripts are loaded in the left hand text box by clicking on the "Load Script"
button. The scripts are:

1. CreateCountriesTable.sql

This adds a new table "COUNTRIES" to the employee database and then populates
it with country data including the country name and ISO2 and 3 character shortnames.

2. CreateCountriesTablewithError.sql

This does the same as the above, except that the first insert statement contains
a syntax error. It may be used to experiment with the "Stop on First Error" checkbox,
and shows how the script engine can recover and continue from (some) syntax errors.

3. DeptListView.sql

This script adds a complex View to the database and tests the script engine in complex
scenarios, such as recursive queries.

4. createproc.sql

This script adds three simple stored procedures. It demonstrates the different ways
that procedure bodies can be declared (ISQL compatible, standard terminator and no
terminator). Use of comments is also demonstrated.

5. ParameterisedQueries.sql

This script demonstrates the use of IBX style query parameters for BLOB columns.
In this case a new column "Image" is added to the COUNTRY Table and an image in
 png format (the flag of St George) is added to the entry for England. The value
 of the Image column is given by a parameter ":MyImage". This is resolved by the a
 pplication which asks for the file containing the image to be placed in the field.
 You should locate and return the "flag_en.png" file.

Note that the interactive resolution of the parameter is an example. The parameter
resolution is carried out by an event handler that could, for example,
have looked for a file which might conventionally have been called "MyImage.bin"
to correspond to the query parameter.

6. Reverseall.sql

Reverses out the above.

7. SelectQuery.sql

Illustrates handling of select queries.



