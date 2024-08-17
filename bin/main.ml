type builtin = Ls | Cd of string | Help | Pwd
type command = Exit | Echo of string | Builtin of builtin

let home = ref (Unix.getenv "HOME" |> String.split_on_char '/')

let addDir lst_ref str =
  if str = "" then ()
  else if str.[0] = '/' then
    lst_ref :=
      String.split_on_char '/' (String.sub str 1 (String.length str - 1))
  else
    (* Relative path: handle . and .. *)
    let parts = String.split_on_char '/' str in
    List.iter
      (fun part ->
         match part with
         | "." -> ()
         | ".." ->
           if !lst_ref <> [] then
             lst_ref := List.rev (List.tl (List.rev !lst_ref))
         | _ -> lst_ref := !lst_ref @ [ part ])
      parts

let print_working_dir path = print_endline (String.concat "/" path)

let parse_input input =
  match input with
  | "exit" -> Exit
  | "ls" -> Builtin Ls
  | "help" -> Builtin Help
  | "pwd" -> Builtin Pwd
  | s when String.length s >= 3 && String.sub s 0 3 = "cd " ->
    Builtin (Cd (String.sub s 3 (String.length s - 3)))
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
     | Help ->
       print_endline
         "Available commands: exit, ls, help, pwd, cd <directory>"
     | Cd dir -> addDir home dir
     | Pwd -> print_working_dir !home);
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

let _ = repl ()
