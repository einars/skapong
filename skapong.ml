open Printf

open Sdl
open Video
open Window
open Timer
open Event
open SDLGL
open Glcaml

open Common


(* all size measurements are in millimeters *)
(* meters/centimeters -> millimeters *)
let paddle_padding = dimension_cm 15

(* paddle movement distance in 1s *)
let paddle_speed = dimension_cm 400

type gamestate =
  { mutable ball:    dimension * dimension (* current position *)
  ; mutable vector:  int * int

  ; mutable p1_pos:  dimension (* vertical center of the first paddle *)
  ; mutable p2_pos:  dimension (* vertical center of the second paddle *)

  ; mutable state:   string (* play / stop *)

  ; mutable p1_move: string (* none/up/down *)
  ; mutable p2_move: string (* none/up/down *)

  ; mutable p1_is_computer: bool
  ; mutable p2_is_computer: bool

  }


let gamestate_clean =
  { ball    = 0, 0
  ; vector  = 1, 1

  ; p1_pos  = dimension_cm 350
  ; p2_pos  = 0
  ; state   = "stop"
  ; p1_move = "none"
  ; p2_move = "none"
  ; p1_is_computer = false
  ; p2_is_computer = true

  }


type the =
  { screen:        dimensions
  ; mutable debug:      bool
  ; mutable fullscreen: bool
  ; field:         dimensions
  ; paddle:        dimensions
  ; states:        (string, string) Hashtbl.t
  ; mutable game:  gamestate
  ; mutable game_prev: gamestate
  ; mutable hz:    int (* game updates/s *)
  }


let the =
  { screen = { width  = 800
             ; height = 600
             }

  ; field = { width  = dimension_m 10
            ; height = dimension_m 7
            }

  ; paddle = { width  = dimension_cm 10
             ; height = dimension_cm 90
             }

  ; game       = gamestate_clean
  ; game_prev  = gamestate_clean
  ; states     = Hashtbl.create 4
  ; hz         = 40
  ; fullscreen = false
  ; debug      = false
  }

let log m  = kprintf (fun m -> try printf "%s\n%!" m with _ -> () ) m
let ilog m = kprintf (fun m -> if the.debug then log "%s" m) m (* ignore log *)


let reasonable_starting_angle () =
  let angle = Random.int 90 - 45 in
  let a = if Random.int 2 = 1 then 180 + angle else angle in
  if a < 0 then a + 360 else a

let restart_game () =
  if the.game.state = "stop" then (
    the.game.ball  <- half the.field.width, half the.field.height;
    if the.debug
    then the.game.vector <- make_vector 0  (dimension_m 4)
    else the.game.vector <- make_vector (reasonable_starting_angle ()) (dimension_m 3);

    the.game.state <- "play";
  )


let gl_resize () =
  glMatrixMode gl_projection;
  glLoadIdentity ();
  glOrtho 0.0 (float the.field.width) 0.0 (float the.field.height) 0.0 1.0;
  glMatrixMode gl_modelview;
  glLoadIdentity ();
  glTranslatef 0.375 0.375 0.0;
  ()



let texture = Array.make 1 0

let initialize_video () = (* {{{ *)


  let load_textures () =
    let s = load_bmp "balls/background.bmp" in
    glGenTextures 1 texture;
    glBindTexture gl_texture_2d texture.(0);

    glTexParameteri gl_texture_2d gl_texture_mag_filter gl_linear;
    glTexParameteri gl_texture_2d gl_texture_min_filter gl_linear;

    (* 2d texture, level of detail 0 (normal), 3 components (red, green, blue), x size from image, y size from image,
      border 0 (normal), rgb color data, unsigned byte data, and finally the data itself. *)
    log "Loaded %d x %d" (surface_width s) (surface_height s);
    glTexImage2D gl_texture_2d 0 3 (surface_width s) (surface_height s) 0 gl_rgb gl_unsigned_byte (surface_pixels s)


  and init_opengl () =

    glEnable gl_blend;
    glDisable gl_cull_face;
    glDisable gl_depth_test;

    glAlphaFunc gl_greater 0.0;
    glEnable gl_alpha_test;

    gl_resize ();


  and init_video_mode () =
    let flags = (if the.fullscreen then [OPENGL; FULLSCREEN; HWPALETTE] else [OPENGL; HWPALETTE]) in
    set_attribute DOUBLEBUFFER 1;
    set_attribute MULTISAMPLEBUFFERS 1;
    set_attribute MULTISAMPLESAMPLES 2;

    ignore( set_video_mode the.screen.width the.screen.height 0 flags );
    (* Video.show_cursor the.fullscreen; *)

  in

  init_video_mode ();
  init_opengl ();
  load_textures ();
  () (* }}} *)


let toggle_fullscreen () =
  the.fullscreen <- not the.fullscreen;
  initialize_video ()


let toggle_debug () =
  the.debug <- not the.debug;
  log "debug %s" (if the.debug then "on" else "off")


let draw_paddle x y is_computer =
  let hh = half the.paddle.height
  and ww = half the.paddle.width
  in
  if is_computer then
    glColor3f 0.58 0.10 0.02
  else
    glColor3f 0.45 0.81 0.38;
  glBegin gl_quads;
  glVertex2i (x - ww) (y - hh);
  glVertex2i (x + ww) (y - hh);
  glVertex2i (x + ww) (y + hh);
  glVertex2i (x - ww) (y + hh);
  glEnd ()


let draw_ball x y =
  let ball_size = dimension_cm 5 in
  glColor3f 0.3 0.3 0.5;
  glBegin gl_quads;
  glVertex2i (x - ball_size) (y - ball_size);
  glVertex2i (x + ball_size) (y - ball_size);
  glVertex2i (x + ball_size) (y + ball_size);
  glVertex2i (x - ball_size) (y + ball_size);
  glEnd ()




let state statename =
  try
    Hashtbl.find the.states statename
  with Not_found -> ""


let set_state statename value =
  Hashtbl.replace the.states statename value




type intersection = None | Point of point

let intersection_of seg1 seg2 =
  let (ax1, ay1),(ax2, ay2) = seg1
  and (ax3, ay3),(ax4, ay4) = seg2 in

  let x1 = float ax1
  and x2 = float ax2
  and x3 = float ax3
  and x4 = float ax4
  and y1 = float ay1
  and y2 = float ay2
  and y3 = float ay3
  and y4 = float ay4 in

  let den = (y4 -. y3) *. (x2 -. x1) -. (x4 -. x3) *. (y2 -. y1) in
  if abs_float den < 0.001 then None
  else
    let ua = ((x4 -. x3) *. (y1 -. y3) -. (y4 -. y3) *. (x1 -. x3) ) /. den
    and ub = ((x2 -. x1) *. (y1 -. y3) -. (y2 -. y1) *. (x1 -. x3) ) /. den in
    if ua >= 0.0 && ua <= 1.0 && ub >= 0.0 && ub <= 1.0 then begin
      Point ( x1 +. ua *. (x2 -. x1) $ int_of_float
            , y1 +. ua *. (y2 -. y1) $ int_of_float)
    end else None

let get_adjusted_reflection_angle wall pt =
  (* wall guaranteed to be vertical, it is a paddle *)
  (* returns 0..50 (0 = go directly back, 50 = max reflection *)
  (* TK: just drop a random here *)
  let _, by = pt
  and (_, wy1), (_, wy2) = wall in
  let coeff = 100 * (wy2 - by) / (wy2 - wy1) in
  log "coef %d" coeff;
  if coeff > 50 then 50 - (100 - coeff) else 50 - coeff

let adj_angle new_angle vector =
    let length = 105 * (length_of vector) / 100
    and cur_angle = angle_of vector in

    log "cur_angle of %s: %d -> %d" (string_of_vec vector) cur_angle new_angle;

    make_vector
        (if cur_angle > 0 && cur_angle <= 90 then 180 - new_angle
        else if cur_angle > 90 && cur_angle <= 180 then new_angle
        else if cur_angle > 180 && cur_angle <= 270 then 360 - new_angle
        else 180 + new_angle)
        length



(*
 full_movement: movement vector in 1s
 partial_movement: remaining lerped movement vector
*)
let rec reflecting_thrust ball_pos partial_movement full_movement surfaces =
(* actually we should sort surfaces by proximity *)
    let trajectory = ball_pos, ball_pos |+ partial_movement in
    match surfaces with
      | (name, pt1, pt2) :: rest -> (
        let wall = pt1, pt2 in
        match intersection_of trajectory wall with
        | None -> reflecting_thrust ball_pos partial_movement full_movement rest (* try next wall *)
        | Point i ->
            let wa, wb = wall in
            log "ball %s reflecting %s [%s:%s] (intersect @ %s) while full_m=%s, partial_m=%s"
              (string_of_pt ball_pos)
              name
              (string_of_pt wa)
              (string_of_pt wb)
              (string_of_pt i)
              (string_of_vec full_movement)
              (string_of_vec partial_movement);

            if is_vertical wall then begin
              log "bam!";
              let new_angle = get_adjusted_reflection_angle wall i in
              i |+ (ball_pos |+ partial_movement |- i $ adj_angle new_angle), (adj_angle new_angle full_movement)
            end
            else i |+ (ball_pos |+ partial_movement |- i $ reflect_y), (reflect_y full_movement)
        )
      | _ -> ball_pos |+ partial_movement, full_movement

let dbg_reflecting_thrust a b c d =
  let new_pos, new_dir = reflecting_thrust a b c d in
  log "ball %s -> %s, dir %s -> %s"
    (string_of_pt a)
    (string_of_pt new_pos)
    (string_of_vec c)
    (string_of_vec new_dir);
  new_pos, new_dir

let nonnull x = if x = 0 then 1 else x



let lerp_gamestate percentage p1 p2 =
  let ns = { p1 with state = p1.state } (* new state *)

  in

  ns.p1_pos <- lerp_int percentage p1.p1_pos p2.p1_pos;
  ns.p2_pos <- lerp_int percentage p1.p2_pos p2.p2_pos;
  ns.ball   <- lerp_pt percentage p1.ball p2.ball;

  ns





let calc_next_state os advance_ms =

  if os.state <> "play" || advance_ms = 0
  then os
  else begin
    let ns = { os with state = os.state } (* new state *)

    (* dx, dy are in unit/s *)

    and percent_adv = advance_ms / 10 in (* 1s = 100% *)

    let direction = os.vector |% percent_adv in

    let paddle_inc = paddle_speed * percent_adv / 100 in

    let bx, by = os.ball and dx, dy = direction in

    (* a simple AI logic *)
    if ns.p1_is_computer then (
      let hit_y =
        by - dy * bx / (nonnull dx) in

      let move_up = os.p1_pos + third the.paddle.height < hit_y
      and move_dn = os.p1_pos - third the.paddle.height > hit_y in
      os.p1_move <- "none";
      if move_up && hit_y < 2 * the.field.height then os.p1_move <- "up";
      if move_dn && hit_y > - 2 * the.field.height then os.p1_move <- "down";
    );


    if ns.p2_is_computer then (
      let hit_y =
        by + dy * (the.field.width - bx) / (nonnull dx) in

      let move_up = os.p2_pos + third the.paddle.height < hit_y
      and move_dn = os.p2_pos - third the.paddle.height > hit_y in
      os.p2_move <- "none";
      if move_up && hit_y < 2 * the.field.height then os.p2_move <- "up";
      if move_dn && hit_y > - 2 * the.field.height then os.p2_move <- "down";
    );



    if os.p1_move = "up"        then ns.p1_pos <- os.p1_pos + paddle_inc
    else if os.p1_move = "down" then ns.p1_pos <- os.p1_pos - paddle_inc;

    if os.p2_move = "up"        then ns.p2_pos <- os.p2_pos + paddle_inc
    else if os.p2_move = "down" then ns.p2_pos <- os.p2_pos - paddle_inc;

    let paddle_clip_lo = half the.paddle.height
    and paddle_clip_hi = the.field.height - half the.paddle.height
    in
    ns.p1_pos <- clamp ns.p1_pos paddle_clip_lo paddle_clip_hi;
    ns.p2_pos <- clamp ns.p2_pos paddle_clip_lo paddle_clip_hi;


    let reflective_surfaces = [
      "left-paddle", (paddle_padding + the.paddle.width, os.p1_pos - half the.paddle.height), (paddle_padding + the.paddle.width, os.p1_pos + half the.paddle.height);
      "right-paddle", (the.field.width - the.paddle.width - paddle_padding, os.p2_pos - half the.paddle.height), (the.field.width - the.paddle.width - paddle_padding, os.p2_pos + half the.paddle.height);
      "down", (0, 0), (the.field.width, 0);
      "up", (0, the.field.height), (the.field.width, the.field.height);
    ] in

    let new_ball, new_direction = reflecting_thrust os.ball direction os.vector reflective_surfaces in

    ns.vector <- new_direction;
    ns.ball <- new_ball;

    let nx, _ = new_ball in
    if nx >= the.field.width || nx <= 0
    then begin
      (* ball out of bounds, break game *)
      ns.ball <- half the.field.width, half the.field.height;
      ns.state <- "stop";
    end;

    ns

  end



let do_tick advance_ms =

  the.game_prev <- the.game;

  if the.game.state = "play" then (
    let p1_up = state "1U" = "T"
    and p1_dn = state "1D" = "T"
    and p2_up = state "2U" = "T"
    and p2_dn = state "2D" = "T"
    and p1_last = state "1LAST"
    and p2_last = state "2LAST"

    in

    the.game.p1_move <- "none";
    the.game.p2_move <- "none";

    if p1_up || p1_dn
    then if (p1_up && not p1_dn) || (p1_up && p1_dn && p1_last = "U")
      then the.game.p1_move <- "up"
      else the.game.p1_move <- "down";

    if p2_up || p2_dn
    then if (p2_up && not p2_dn) || (p2_up && p2_dn && p2_last = "U")
      then the.game.p2_move <- "up"
      else the.game.p2_move <- "down";

  );

  the.game <- calc_next_state the.game advance_ms;

  ()





let render_state state =
  glClearColor 0.1 0.1 0.1 0.0;

  glClear gl_color_buffer_bit;

  glLoadIdentity ();

  (* draw background *)
  glEnable gl_texture_2d;

  glColor3f 1.0 1.0 1.0;

  glBegin gl_quads;

  (* try to do per-pixel mapping *)
  let adjx = (float the.screen.width) /. 256.0
  and adjy = (float the.screen.height) /. 256.0 in

  let offx = (x_of state.ball $ float) /. (float the.field.width) /. 2.71
  and offy = (y_of state.ball $ float) /. (float the.field.height) /. 2.71 in

  glTexCoord2f offx offy;
  glVertex2i 0 0;

  glTexCoord2f (offx +. adjx) offy;
  glVertex2i the.field.width 0;

  glTexCoord2f (adjx +. offx) (adjy +. offy);
  glVertex2i the.field.width the.field.height;

  glTexCoord2f offx (adjy +. offy);
  glVertex2i 0 the.field.height;

  glEnd ();
  glDisable gl_texture_2d;

  draw_paddle paddle_padding state.p1_pos state.p1_is_computer;
  draw_paddle (the.field.width - paddle_padding) state.p2_pos state.p2_is_computer;

  let bx, by = state.ball
  in
  draw_ball bx by;

  swap_buffers ();

  glFinish ();

  ()



let render_lerp percentage =
  lerp_gamestate percentage the.game_prev the.game $ render_state


let rec paddle_state statename newstate =

  if newstate then
    if statename = "1U" then set_state "1LAST" "U"
    else if statename = "1D" then set_state "1LAST" "D"
    else if statename = "2U" then set_state "2LAST" "U"
    else if statename = "2D" then set_state "2LAST" "D";
    set_state statename (if newstate then "T" else "F")


let set_computer_p1 () =
  the.game.p1_is_computer <- not the.game.p1_is_computer;
  the.game.state <- "play"


let set_computer_p2 () =
  the.game.p2_is_computer <- not the.game.p2_is_computer;
  the.game.state <- "play"


exception No_more_events

let rec process_events () =
  (
    match poll_event () with
    | Key k -> (
            match k.sym with
            | K_ESCAPE ->
                Sdl.quit ();
                exit 0;
            | K_RETURN ->
                if (k.keystate = PRESSED && List.exists (fun x -> x = KMOD_LALT) k.modifiers) then toggle_fullscreen ();
            | K_1 ->
                if k.keystate = PRESSED then set_computer_p1 ();
            | K_2 ->
                if k.keystate = PRESSED then set_computer_p2 ();
            | K_SPACE ->
                if k.keystate = PRESSED then restart_game ();
            | K_D ->
                if k.keystate = PRESSED then toggle_debug ();
            | K_A ->
                paddle_state "1U" ( k.keystate = PRESSED );
                the.game.p1_is_computer <- false;
            | K_Z ->
                paddle_state "1D" ( k.keystate = PRESSED );
                the.game.p1_is_computer <- false;
            | K_K | K_UP ->
                paddle_state "2U" ( k.keystate = PRESSED );
                the.game.p2_is_computer <- false;
            | K_M | K_DOWN ->
                paddle_state "2D" ( k.keystate = PRESSED );
                the.game.p2_is_computer <- false;
            | _ -> ()
    )
    | Button b ->
        restart_game ();
    | Quit ->
        Sdl.quit ();
        exit 0;
    | Resize r ->
        ilog "resize to %dx%d" r.w r.h;
        the.screen.width <- r.w;
        the.screen.height <- r.h;
        gl_resize ();
    | NoEvent -> raise No_more_events
    | _ -> ()
  );
  process_events ()


let period = 1000 / the.hz

let rec main_loop last_time accumulator =

  try process_events ();
  with No_more_events -> ();

  let rec eat_physics accumulator =
    if accumulator >= period then begin
      ilog "tick";
      do_tick period;
      eat_physics (accumulator - period)
    end else accumulator

  in

  let time = get_ticks () in
  let frame_time = time - last_time in
  let frame_time = if frame_time > 25 then 25 else frame_time in

  let accumulator = (accumulator + frame_time) $ eat_physics in

  accumulator * 100 / period $ ilog "oompf %d";
  accumulator * 100 / period $ render_lerp;
  delay 5;
  main_loop time accumulator


let main () =
  (* test_thrust (); exit 9; *)
  Sdl.init [Sdl.VIDEO];
  Random.self_init ();
  set_caption "Skapong, the boring pong" "skapong";
  initialize_video ();
  restart_game ();
  main_loop (get_ticks ()) 0


let _ = main ()

(* vim: set tw=0 fdm=marker : *)

