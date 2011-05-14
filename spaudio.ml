open Sdl
open Sdl_mixer

open Common

let audio_initialized = ref false
let audio_initialized_successfully = ref false

let initialize_audio () =
  try
    audio_initialized := true;
    Mix.open_audio 44100 Audio.S16 Audio.STEREO 512;
    audio_initialized_successfully := true;
  with _ -> ()

class wav_file file_name = 
  object (self)

    val mutable we_are_initialized = false
    val mutable we_are_initialized_successfully = false
    val mutable chunk = None

    method initialize () =
      we_are_initialized <- true;
      log "initialize %s" file_name;
      if not !audio_initialized then initialize_audio ();
      if !audio_initialized_successfully then begin
        chunk <- Some (Mix.load_wav file_name);
        we_are_initialized_successfully <- true;
      end


    method play () =
      log "would play %s" file_name;
      if not we_are_initialized then self#initialize ();
      if we_are_initialized_successfully then begin
        log "play %s" file_name;
        match chunk with
        | None -> ()
        | Some c -> ignore(Mix.play_channel Mix.any_channel c Mix.play_once)
      end

  end

(* vim: set tw=0 : *)

