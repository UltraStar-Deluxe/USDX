program DoTags;
var
  four_letters: string;
  tag: int32;
begin
  writeln ('Program for the calculation of 4 letter tags, little and big endian.');
  writeln ('Enter 4 letters:');
  readln (four_letters);
  while length(four_letters) <> 4 do
  begin
    writeln ('This were ', length(four_letters), ' letter(s). Enter exactly 4 letters:');
    readln (four_letters);
  end;
  tag := ord(four_letters[4]) or (ord(four_letters[3]) shl 8) or (ord(four_letters[2]) shl 16) or (ord(four_letters[1]) shl 24);
  writeln ('BE-TAG: $', hexStr(tag, 8));
  tag := ord(four_letters[1]) or (ord(four_letters[2]) shl 8) or (ord(four_letters[3]) shl 16) or (ord(four_letters[4]) shl 24);
  writeln ('LE-TAG: $', hexStr(tag, 8));
  if four_letters[1] = '0' then
  begin
    writeln ('Maybe you want a digital zero in the first position. Here you go:');
    tag := ord(four_letters[4]) or (ord(four_letters[3]) shl 8) or (ord(four_letters[2]) shl 16);
    writeln ('BE-TAG: $', hexStr(tag, 8));
    tag := (ord(four_letters[2]) shl 8) or (ord(four_letters[3]) shl 16) or (ord(four_letters[4]) shl 24);
    writeln ('LE-TAG: $', hexStr(tag, 8));
  end;
end.
