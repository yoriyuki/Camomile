let sanitize s =
  if String.contains s '/' then 
    failwith (Printf.sprintf "String %s contains illegal cahracter '/'." s)
  else if String.contains s '\\' then
    failwith (Printf.sprintf "String %s contains illegal cahracter '\'." s)
  else ()
