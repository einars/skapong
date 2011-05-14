(* A simple BFF font support
 *
 * http://www.codehead.co.uk/cbfg/*
 *
 **)

open Common
open Bigarray
open Glcaml

exception Bff_is_crap of string

let ba_peek32 binary offs = 0
  + 0x00000001 * binary.{0 + offs}
  + 0x00000100 * binary.{1 + offs}
  + 0x00010000 * binary.{2 + offs}
  + 0x01000000 * binary.{3 + offs}


class bff_font file_name =
  object (self)

    val mutable we_are_initialized = false
    val mutable we_are_initialized_successfully = false

    val mutable our_size_x = 0
    val mutable our_size_y = 0
    val mutable our_cell_x = 0
    val mutable our_cell_y = 0
    val mutable our_bpp  = 0
    val mutable our_base = 0

    val our_texture = Array.make 1 0

    val our_char_widths = Array.make 256 0

    method die msg = raise (Bff_is_crap (file_name ^ ": " ^ msg))

    method show_information () =
      log "  %s" file_name;
      log "--------------";
      if not we_are_initialized then
        log "  not initialized yet"
      else begin
        log "  size: %dx%d" our_size_x our_size_y;
        log "  cell: %dx%d" our_cell_x our_cell_y;
        log "  bpp:  %d" our_bpp;
        log "  base: %d" our_base;
      end


    method texture () =
      if not we_are_initialized then
      let c = array_of_file file_name in
      we_are_initialized <- true;

      let load_char_widths () =
        for i = 0 to 255 do
          our_char_widths.(i) <- c.{i + 20}
        done in


      log "Loading %s, signature: %02x%02x" file_name c.{0} c.{1};

      if c.{0} != 0xbf || c.{1} != 0xf2 then self#die "BFF signature is all wrong";

      our_size_x <- ba_peek32 c 2;
      our_size_y <- ba_peek32 c 6;
      our_cell_x <- ba_peek32 c 10;
      our_cell_y <- ba_peek32 c 14;
      our_bpp <- c.{18};
      our_base <- c.{19};

      self#show_information ();

      load_char_widths ();

      if our_bpp != 8 then self#die "Only 8bpp BFF files are supported";

      (* prepare GL texture *)

      glGenTextures 1 our_texture;
      glBindTexture gl_texture_2d our_texture.(0);

      (* no need for filtering *)
      glTexParameteri gl_texture_2d gl_texture_min_filter gl_nearest;
      glTexParameteri gl_texture_2d gl_texture_mag_filter gl_nearest;
      (* stop characters from bleeding over edges *)
      glTexParameteri gl_texture_2d gl_texture_wrap_s gl_clamp_to_edge;
      glTexParameteri gl_texture_2d gl_texture_wrap_t gl_clamp_to_edge;

      let bytes = Array1.sub c 276 (our_size_x * our_size_y * (our_bpp / 8)) in
      (* glTexImage2D gl_texture_2d 0 gl_luminance our_size_x our_size_y 0 gl_luminance gl_unsigned_byte bytes; *)
      glTexImage2D gl_texture_2d 0 gl_alpha our_size_x our_size_y 0 gl_alpha gl_unsigned_byte bytes;

      we_are_initialized_successfully <- true;
      ()

    method use () =
      glEnable gl_texture_2d;
      (* glBlendFunc gl_src_alpha gl_src_alpha; *)
      glBindTexture gl_texture_2d our_texture.(0)

    method pprint (x:int) (y:int) (text:string) =

      self#use ();
      self#print x y text;


    method print x y text =

      if not we_are_initialized then self#texture();
      if we_are_initialized_successfully then begin
        let len = String.length text in
        glBegin gl_quads;

        let cur_x = ref x in

        let row_pitch = our_size_x / our_cell_x
        and col_factor = (float our_cell_x) /. (float our_size_x)
        and row_factor = (float our_cell_y) /. (float our_size_y) in

        for i = 0 to len - 1 do
          let c = Char.code text.[i] in

          let row = (c - our_base) / row_pitch
          and col = (c - our_base) mod row_pitch in

          let u = (float col) *. col_factor
          and v = (float row) *. row_factor in

          let u1 = u +. col_factor
          and v1 = v +. row_factor in

          glTexCoord2f u  v1; glVertex2i !cur_x                y;
          glTexCoord2f u1 v1; glVertex2i (!cur_x + our_cell_x) y;
          glTexCoord2f u1 v ; glVertex2i (!cur_x + our_cell_x) (y + our_cell_y);
          glTexCoord2f u  v ; glVertex2i !cur_x                (y + our_cell_y);

          cur_x := !cur_x + our_char_widths.(c)

        done;

        glEnd ();
      end


    method print_lines x y lines =
      let rec print_lines x y = function
      | line :: rest ->
        begin
          self#print x y line;
          print_lines x (y - our_size_x / our_cell_x) rest;
        end
      | _ -> ()
      in print_lines x y lines;
  end



(* vim: set tw=0 : *)

