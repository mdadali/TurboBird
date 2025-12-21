/* Demonstrate Parameterised queries in scripts */

Alter Table COUNTRY
  add Image Blob;

Update COUNTRY Set Image = :MyImage Where COUNTRY = 'England';
Commit;

