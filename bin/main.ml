type builtin = Ls | Help
type command = Exit | Echo of string | Builtin of builtin

let parse_input input =
  match input with
  | "exit" -> Exit
  | "ls" -> Builtin Ls
  | "help" -> Builtin Help
  | _ -> Echo input

let execute_command = function
  | Exit ->
    print_endline "Exiting...";
    false
  | Echo s ->
    print_endline s;
    true
  | Builtin b ->
    (match b with
     | Ls -> print_endline "Listing files... (not implemented)"
     | Help -> print_endline "Available commands: exit, ls, help");
    true

let rec repl () =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file ->
    print_endline "End of input, exiting...";
    false
  | input ->
    let command = parse_input input in
    let continue = execute_command command in
    if continue then repl () else false

let () =
  print_endline "Welcome to the OCaml REPL. Type 'help' for available commands.";
  ignore (repl ())
