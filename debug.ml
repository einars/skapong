open Common


let debug_messages = ref []

let debug_logger s =
  if List.length !debug_messages > 20 then
    debug_messages := (List.tl !debug_messages);

  debug_messages := List.append !debug_messages [s]

let get_debug_messages () =
  !debug_messages

let install_debug_logger () =
  install_logger debug_logger
