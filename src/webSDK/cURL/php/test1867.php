<?

  /* 
    Script to test rfc1867 multipart/form-data posts. 
    Tested with PHP/4.0.5 (Win32) and PHP 4.1.0 (Linux)
  */

if  ( $HTTP_POST_VARS )  /* PHP version 4.0.5 */
{ 
  echo "\nHello from $SERVER_SOFTWARE !!!\n\nI got your note, it said:";
  echo $HTTP_POST_VARS[readthis];
  echo "\n\n";
  echo "I also received the file you sent:\n";
  $A = $HTTP_POST_FILES[filethis];
  echo " It was named: $A[name]\n";
  echo " The size was: $A[size] bytes\n";
  echo " The type was: $A[type]\n";
  echo " I save it as: $A[tmp_name]\n\n";
  echo "Thank you, ".trim($HTTP_POST_VARS[whodunit])." :-)\n\n\n";
} 

else 

if ( $_POST )   /* PHP version 4.1.0 */
{
  echo "\nHello from $_SERVER[SERVER_SOFTWARE] !!!\n\nI got your note, it said:";
  echo $_POST[readthis];
  echo "\n\n";
  echo "I also received the file you sent:\n";
  $A = $_FILES[filethis];
  echo " It was named: $A[name]\n";
  echo " The size was: $A[size] bytes\n";
  echo " The type was: $A[type]\n";
  echo " I save it as: $A[tmp_name]\n\n";
  echo "Thank you, ".trim($_POST[whodunit])." :-)\n\n\n";
} 

else   /* PHP version ??? */ 

{
  echo "Something went wrong, either\n";
  echo "I did not get the expected data\n";
  echo "or I did not process it correctly!\n";
}

?>
