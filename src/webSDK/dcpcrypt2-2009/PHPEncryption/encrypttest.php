<html>
  <head>
    <title>PHP Encryption test</title>
  </head>
  <body>
    <?php
      $key = 'My key';
      $iv = 'abcdefghijklmnop';
      $data = 'My data';
      $encryptedecb = mcrypt_ecb(MCRYPT_RIJNDAEL_128,$key,$data,MCRYPT_ENCRYPT,$iv);
      $encryptedcbc = mcrypt_cbc(MCRYPT_RIJNDAEL_128,$key,$data,MCRYPT_ENCRYPT,$iv);
      echo '<p>Key: '.$key;
      echo '<p>IV: '.$iv;
      echo '<hr>';
      echo '<p>Data: '.$data;
      echo '<p>ECB encrypted: '.base64_encode($encryptedecb);
      echo '<p>CBC encrypted: '.base64_encode($encryptedcbc);
      $data = 'A longer piece of data';
      $encryptedecb = mcrypt_ecb(MCRYPT_RIJNDAEL_128,$key,$data,MCRYPT_ENCRYPT,$iv);
      $encryptedcbc = mcrypt_cbc(MCRYPT_RIJNDAEL_128,$key,$data,MCRYPT_ENCRYPT,$iv);
      echo '<hr>';
      echo '<p>Data: '.$data;
      echo '<p>ECB encrypted: '.base64_encode($encryptedecb);
      echo '<p>CBC encrypted: '.base64_encode($encryptedcbc);
    ?>
  </body>
</html>
