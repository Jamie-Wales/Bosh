type builtin = Ls | Cd of string | Help | Pwd
type command = Exit | Echo of string | Builtin of builtin
type path = Path of string list

let home_path () = Path (Unix.getenv "HOME" |> String.split_on_char '/')
let string_of_path (Path components) = String.concat "/" components

let normalize_path components =
  let rec normalize acc = function
    | [] -> List.rev acc
    | "." :: rest -> normalize acc rest
    | ".." :: rest -> normalize (if acc = [] then [] else List.tl acc) rest
    | x :: rest -> normalize (x :: acc) rest
  in
  normalize [] components

let change_dir (Path current) new_dir =
  let new_components =
    if String.length new_dir > 0 && new_dir.[0] = '/' then
      String.split_on_char '/'
        (String.sub new_dir 1 (String.length new_dir - 1))
    else current @ String.split_on_char '/' new_dir
  in
  Path (normalize_path new_components)

let print_working_dir path = print_endline (string_of_path path)

let parse_input input =
  match input with
  | "exit" -> Exit
  | "ls" -> Builtin Ls
  | "help" -> Builtin Help
  | "pwd" -> Builtin Pwd
  | s when String.length s >= 3 && String.sub s 0 3 = "cd " ->
    Builtin (Cd (String.sub s 3 (String.length s - 3)))
  | _ -> Echo input

let execute_command current_path = function
  | Exit ->
    print_endline "Exiting...";
    (current_path, false)
  | Echo s ->
    print_endline s;
    (current_path, true)
  | Builtin b -> (
      match b with
      | Ls ->
        print_endline "Listing files... (not implemented)";
        (current_path, true)
      | Help ->
        print_endline
          "Available commands: exit, ls, help, pwd, cd <directory>";
        (current_path, true)
      | Cd dir ->
        let new_path = change_dir current_path dir in
        (new_path, true)
      | Pwd ->
        print_working_dir current_path;
        (current_path, true))

let rec repl current_path =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file ->
    print_endline "End of input, exiting...";
    false
  | input ->
    let command = parse_input input in
    let new_path, continue = execute_command current_path command in
    if continue then repl new_path else false

let () = ignore (repl @@ home_path ())
