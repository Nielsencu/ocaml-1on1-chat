open Lwt
let host = ref "127.0.0.1"
let port = ref 8000
let mode = ref "server"
let msg_header = '\000'
let trip_header = '\001'
let set_mode s = 
  mode := s
let get_host_addr () = 
  let host =
  try
    Unix.inet_addr_of_string !host
  with _ ->
    let record = 
      try 
        Unix.gethostbyname !host 
      with _ ->
        failwith @@ Printf.sprintf "Unable to resolve hostname %s:%i" !host !port
      in
    record.Unix.h_addr_list.(0)
    in 
  host

let (>>*=) m f =
m >>= function
| Error _ as e -> Lwt.return e
| Ok x -> f x

let create_socket () = 
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt.return sock

let create_server_socket host_addr = 
  create_socket () >>= fun sock ->
  Lwt.catch (fun () ->
    Lwt_unix.bind sock @@ ADDR_INET(host_addr, !port) >>= fun _ ->
    Lwt_unix.listen sock 10;
    Lwt_io.printf "Server running on %s:%i\n" (Unix.string_of_inet_addr host_addr) !port >>= fun _ ->
    Lwt.return sock
  )(function
    |
    exn ->
    Lwt_io.printf "Unable to create server with error below\n" >>= fun _ ->
    Lwt.fail exn
  )

let create_client_socket host_addr = 
  create_socket () >>= fun sock ->
  Lwt.catch(fun () ->
    Lwt_unix.connect sock @@ ADDR_INET(host_addr, !port) >>= fun _ ->
    Lwt.return sock
  )(function
  | exn ->
    Lwt_io.printf "Unable to connect to server with error below\n" >>= fun _ ->
    Lwt.fail exn
  )

type header_type = Message | Trip
let get_header header_type = 
  match header_type with 
  | Message ->
    Bytes.make 1 msg_header
  | Trip ->
    Bytes.make 1 trip_header

let send_header sock header_type =
  let header_bytes = get_header header_type in
  Lwt_unix.send sock header_bytes 0 1 [] >>= fun _ -> 
    Lwt.return ()

let recv_header sock = 
  let header_buffer = Bytes.create 1 in
  Lwt_unix.recv sock header_buffer 0 1 [] >>= fun _ ->
    let header_byte = Bytes.get header_buffer 0 in 
    match header_byte with 
    | _ when header_byte = trip_header ->
      Lwt.return (Ok Trip)
    | _ when header_byte = msg_header ->
      Lwt.return (Ok Message)
    | _ -> Lwt.return (Error "Bad message")

let send_string sock str =
  let length = String.length str in
  (* Since we are using unsigned 16 bits integer, to prevent overflow we discard message with size > 2**16-1 *)
  if length >= 65535 then
    Lwt_io.printf "Message causing integer overflow" >>= fun _ ->
    Lwt.return ()
  else
    let length_bytes = Bytes.create 2 in
    let () = Bytes.set_uint16_be length_bytes 0 length in
    Lwt_unix.send sock length_bytes 0 2 [] >>= fun _ -> 
      let buffer = Bytes.of_string str in
      Lwt_unix.send sock buffer 0 length [] >>= fun _ ->
        Lwt.return()

let recv_string sock =
  let length_buffer = Bytes.create 2 in
  Lwt_unix.recv sock length_buffer 0 2 [] >>= fun bytes_received ->
    (match bytes_received with 
    | 2 -> 
      let msg_length = Bytes.get_uint16_be length_buffer 0 in
      let msg_buffer = Bytes.create msg_length in
      Lwt_unix.recv sock msg_buffer 0 msg_length [] >>= fun bytes_received ->
        (match bytes_received with 
        | 0 -> Lwt.return (Error "Bad message")
        | _ -> Lwt.return (Ok (Bytes.to_string msg_buffer))
        )
    | 0 -> Lwt.return (Error "Disconnected")
    | _ -> Lwt.return (Error "Bad message")
    )

let recv_loop (sock, sender) = 
  let rec loop () = 
    recv_header sock >>*= function 
      | Trip ->
        recv_string sock >>*= fun time ->
        Lwt_io.printf "Round-trip time: %.5f\n" (Unix.gettimeofday() -. (float_of_string time)) >>=
        fun _ -> Lwt.return() >>= loop
      | Message ->
        recv_string sock >>*= fun msg ->
        Lwt_io.printf "%s: %s\n" sender msg >>= fun _ -> 
        recv_string sock >>*= fun time -> 
        send_header sock Trip >>= fun _ ->
        send_string sock time >>= fun _ -> Lwt.return() >>= loop
  in loop()
let send_loop sock = 
  let rec loop () = 
    Lwt_io.read_line_opt Lwt_io.stdin >>= fun text ->
    match text with 
    | None -> 
      Lwt.return() >>= loop
    | Some text ->
      send_header sock Message >>= fun _ -> 
      send_string sock text >>= fun _ ->
      send_string sock (string_of_float (Unix.gettimeofday())) >>= loop
    in loop()

let run_server host_addr = 
  let rec serve () = 
  create_server_socket host_addr >>= fun server_sock ->
  Lwt_io.printf "Waiting for client to connect...\n" >>= fun _ ->
  Lwt_unix.accept server_sock >>= fun (client_sock,_) -> 
    Lwt_io.printf "Client connected! Happy Chatting :)\n" >>= fun _ ->
    let () = Lwt.async (fun () -> 
      recv_loop (client_sock, "Client") >>= fun _ ->
      Lwt.return ()) 
    in send_loop client_sock >>= serve
  in serve()

let run_client host_addr = 
  create_client_socket host_addr >>= fun sock ->
  Lwt_io.printf "Client listening on %s:%i! Happy Chatting :)\n" (Unix.string_of_inet_addr host_addr) !port >>= fun _ ->
  let () = Lwt.async (fun () -> 
    recv_loop (sock, "Server") >>= fun _ ->
    Lwt.return ()) 
  in send_loop sock

let main () = 
  let host_addr = get_host_addr () in
  match !mode with
  | "server" ->
    run_server (host_addr)
  | "client" -> 
    run_client (host_addr)
  | _ -> Lwt.fail Exit

let usage_msg = "start [-host <host>] [-port <port>] [-mode <client|server>]"

let speclist = [
  ("-host", Arg.Set_string host, "Hostname to connect/listen: default 127.0.0.1");
  ("-port", Arg.Set_int port, "Port number to connect/listen: default 8000");
  ("-mode", Arg.Symbol (["client"; "server"], set_mode), "Start as client/server: default server");
]

let () = 
  Arg.parse speclist (fun _ -> ()) usage_msg;
  Lwt_main.run @@ main()