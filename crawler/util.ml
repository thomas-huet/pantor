let digit n =
  if n < 10 then Char.chr (n + 48)
  else Char.chr (n + 55)

let hex s =
  let h = Bytes.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    h.[2 * i] <- digit (Char.code s.[i] / 16);
    h.[2 * i + 1] <- digit (Char.code s.[i] mod 16);
  done;
  Bytes.to_string h
