let (=>) left right = print_char (if left = right then '.' else 'F')


let () = 
  Docopt.hello => "world";
