open Printf

open Sdl
open Video
open Window
open Timer
open Event
open SDLGL
open Glcaml

let (|>) a b = b a
let ($) a b = b a

(* all size measurements are in millimeters *)
(* meters/centimeters -> millimeters *)
type dimension = int

let dimension_m m = m * 1000

let dimension_cm cm = cm * 10

let tau = 6.2831852

let paddle_padding = 150

let radians_of angle = float angle *. tau /. 360.0

let half x = x / 2
let middle_of a b = ( a + b ) / 2

type point = int * int

type intersection = None | Point of point

type dimensions =
  { mutable width:  dimension
  ; mutable height: dimension
  }

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

  ; p1_pos  = 0
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
  ; start_time:    float
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
  ; start_time = Unix.gettimeofday ()
  ; states     = Hashtbl.create 4
  ; hz         = 15
  ; fullscreen = false
  ; debug      = false
  }

let log m  = kprintf (fun m -> try printf "%s\n%!" m with _ -> () ) m
let ilog m = kprintf (fun m -> if the.debug then log "%s" m) m (* ignore log *)

let x_make_vector angle length =
  (float length) *. cos ( radians_of angle ) $ int_of_float,
  (float length) *. sin ( radians_of angle ) $ int_of_float

let make_vector a l =
  let x1, x2 = x_make_vector a l in
  log "Vector (a = %d, l = %d) -> (%d, %d)" a l x1 x2;
  x1, x2

let x_of pt =
  let x, _ = pt in x

let y_of pt =
  let _, y = pt in y

let (|+) pt vec =
  let x, y = pt
  and dx, dy = vec in
  x + dx, y + dy

let (|-) pt vec =
  let x, y = pt
  and dx, dy = vec in
  x - dx, y - dy

let (|%) vec percentage =
  let x, y = vec in
  x * percentage / 100, y * percentage / 100

let reflect_x vec =
  let x, y = vec in -x, y

let reflect_y vec =
  let x, y = vec in x, -y

let length_of vec =
  let x, y = vec in
  int_of_float ( sqrt (x * x + y * y $ float) )

let angle_of vec =
  let x, y = vec in
  if x == 0 then
    if y = 0 then 0 else if y >= 0 then 90 else 270
  else
    if y = 0 then 180 else
    ((float y) /. (float x) $ atan) *. 360.0 /. tau $ int_of_float

let string_of_pt p =
  let x, y = p in sprintf "(%d,%d)" x y

let string_of_vec v =
  let x, y = v in sprintf "[%d,%d]" x y


let restart_game () =
  if the.game.state = "stop" then (
    the.game.ball  <- half the.field.width, half the.field.height;
    the.game.vector <- make_vector (Random.int 360) 300;
    the.game.state <- "play";
  )


let gl_resize () =
  glOrtho 0.0 (float the.field.width) 0.0 (float the.field.height) 0.0 1.0;
  glMatrixMode gl_modelview


let texture = Array.make 1 0

let initialize_video () = (* {{{ *)


  let load_textures () =
    let s = load_bmp "balls/background.bmp" in
    glGenTextures 1 texture;
    glBindTexture gl_texture_2d texture.(0);

    glTexParameteri gl_texture_2d gl_texture_mag_filter gl_linear;
    glTexParameteri gl_texture_2d gl_texture_min_filter gl_linear;

    (*glTexParameteri gl_texture_2d gl_texture_wrap_s gl_repeat;
    glTexParameteri gl_texture_2d gl_texture_wrap_t gl_repeat;*)

    (* 2d texture, level of detail 0 (normal), 3 components (red, green, blue), x size from image, y size from image,
      border 0 (normal), rgb color data, unsigned byte data, and finally the data itself. *)
    log "Loaded %d x %d" (surface_width s) (surface_height s);
    glTexImage2D gl_texture_2d 0 3 (surface_width s) (surface_height s) 0 gl_rgb gl_unsigned_byte (surface_pixels s)


  and init_opengl () =

    glEnable gl_blend;
    glDisable gl_cull_face;
    glDisable gl_depth_test;

    glMatrixMode gl_projection;
    glLoadIdentity ();
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
  glColor3f 1.0 (if is_computer then 0.0 else 1.0) 0.0;
  glBegin gl_quads;
  glVertex2i (x - ww) (y - hh);
  glVertex2i (x + ww) (y - hh);
  glVertex2i (x + ww) (y + hh);
  glVertex2i (x - ww) (y + hh);
  glEnd ()


let draw_ball x y =
  let fx = float x
  and fy = float y
  and ball_size = 50.0 in
  glColor3f 0.9 0.9 0.7;
  glBegin gl_quads;
  glVertex2f (fx -. ball_size) (fy -. ball_size);
  glVertex2f (fx +. ball_size) (fy -. ball_size);
  glVertex2f (fx +. ball_size) (fy +. ball_size);
  glVertex2f (fx -. ball_size) (fy +. ball_size);
  glEnd ()



let clamp v lo hi =
  if v < lo then lo
  else if v > hi then hi
  else v


let is_between n lo hi =
  (n > lo && n <= hi) || (n <= lo && n > hi)


let state statename =
  try
    Hashtbl.find the.states statename
  with Not_found -> ""


let set_state statename value =
  Hashtbl.replace the.states statename value


let choose_angle percentage =
  (* convert (-1.0 .. 1.0) to (10..170) range *)
  let adj_base = 20.0 in
  (Random.int 5) - 2 +
  clamp (adj_base +. 0.5 *. (180.0 -. adj_base) *. (percentage +. 1.0) |> int_of_float) (int_of_float adj_base) (int_of_float (180.0 -. adj_base))


let vector_of seg =
  let (x1, y1),(x2, y2) = seg in
  (x2 - x1), (y2 - y1)

let intersection_of seg1 seg2 =
  let (x1, y1),(x2, y2) = seg1
  and (x3, y3),(x4, y4) = seg2 in

  let den = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1) in
  if den == 0 then None
  else
    let ua = float ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3) ) /. float den
    and ub = float ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3) ) /. float den in
    if ua > 0.0 && ua < 1.0 && ub > 0.0 && ub < 1.1 then
      Point ( x1 + int_of_float ( ua *. (x2 - x1 $ float) )
            , y1 + int_of_float ( ua *. (y2 - y1 $ float) ) )
    else None

let is_horizontal seg =
  let (x1, _), (x2, _) = seg in x1 = x2


let reflecting_thrust pt orig_direction surfaces =

  let rec rethrust pt direction orig s =
    let trajectory = pt, pt |+ direction in
    match s with
      | wall :: rest -> (
        match intersection_of trajectory wall with
        | None -> rethrust pt direction orig rest (* try next wall *)
        | Point i -> if is_horizontal wall then rethrust i (pt |+ direction |- i $ reflect_x) (reflect_x orig) surfaces
                                           else rethrust i (pt |+ direction |- i $ reflect_y) (reflect_y orig) surfaces
        (*
        | Point i -> if is_horizontal wall then rethrust i (pt |+ direction |- i |> reflect_x) surfaces
                                           else rethrust i (pt |+ direction |- i |> reflect_y) surfaces
                                           *)
        )
      | _ -> pt |+ direction, orig (* finished *)
  in

  rethrust pt orig_direction orig_direction surfaces
  (* let pt, last_direction = rethrust pt orig_direction surfaces in *)
  (* let new_direction = make_vector (angle_of last_direction) (length_of orig_direction) in *)
  (* pt, new_direction *)


let test_thrust () =
  let seg = (3, 3), (10, 3)
  in
  let np, nd = reflecting_thrust (3,0) (5,5) [seg] in
  log "%s %s" (string_of_pt np) (string_of_vec nd);

  ()



let nonnull x = if x == 0 then 1 else x

let calc_next_state os percent_frame =

  if os.state <> "play" || percent_frame = 0
  then os
  else begin
    let ns = { os with state = os.state } (* new state *)
    and inc = ((dimension_m 6) / the.hz) * percent_frame / 100

    and direction = os.vector |% percent_frame in

    let bx, by = os.ball and dx, dy = direction in

    (* a simple AI logic *)
    if ns.p1_is_computer then (
      let hit_y =
        by + dy * bx / (nonnull dx) in

      let move_up = os.p1_pos + half the.paddle.height < hit_y
      and move_dn = os.p1_pos - half the.paddle.height > hit_y in
      os.p1_move <- "none";
      if move_up && hit_y < 2 * the.field.height then os.p1_move <- "up";
      if move_dn && hit_y > - 2 * the.field.height then os.p1_move <- "down";
    );


    if ns.p2_is_computer then (
      let hit_y =
        by + dy * (the.field.width - bx) / (nonnull dx) in

      let move_up = os.p2_pos + half the.paddle.height < hit_y
      and move_dn = os.p2_pos - half the.paddle.height > hit_y in
      os.p2_move <- "none";
      if move_up && hit_y < 2 * the.field.height then os.p2_move <- "up";
      if move_dn && hit_y > - 2 * the.field.height then os.p2_move <- "down";
    );



    if os.p1_move = "up"        then ns.p1_pos <- os.p1_pos + inc
    else if os.p1_move = "down" then ns.p1_pos <- os.p1_pos - inc;

    if os.p2_move = "up"        then ns.p2_pos <- os.p2_pos + inc
    else if os.p2_move = "down" then ns.p2_pos <- os.p2_pos - inc;

    let paddle_clip_lo = half the.paddle.height
    and paddle_clip_hi = the.field.height - half the.paddle.height
    in
    ns.p1_pos <- clamp ns.p1_pos paddle_clip_lo paddle_clip_hi;
    ns.p2_pos <- clamp ns.p2_pos paddle_clip_lo paddle_clip_hi;


    let reflective_surfaces = [
      (0, 0), (the.field.width, 0);
      (0, the.field.height), (the.field.width, the.field.height);
      (paddle_padding + the.paddle.width, os.p1_pos - half the.paddle.height), (paddle_padding + the.paddle.width, os.p1_pos + half the.paddle.height);
      (the.field.width - the.paddle.width - paddle_padding, os.p2_pos - half the.paddle.height), (the.field.width - the.paddle.width - paddle_padding, os.p2_pos + half the.paddle.height);
    ] in

    let new_ball, new_direction = reflecting_thrust os.ball direction reflective_surfaces in

    ns.vector <- new_direction;

      (*
      ns.ltr <- true;
      ns.angle <- choose_angle (2.0 *. float (ns.p1_pos - !ny) /. (float the.paddle.height));
      ns.speed <- ns.speed + (dimension_m 5);
      *)

      (*
        ns.ltr <- false;
        ns.angle <- choose_angle (2.0 *. float (ns.p2_pos - !ny) /. (float the.paddle.height));
        ns.speed <- ns.speed + (dimension_m 5);
        *)

    let nx, _ = new_ball in
    if nx > the.field.width || nx <= 0
    then begin
      (* ball out of bounds, break game *)
      ns.ball <- half the.field.width, half the.field.height;
      ns.state <- "stop";
    end else
      ns.ball <- new_ball;

    ns

  end



let do_tick () =

  ilog "tick";

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

  the.game_prev <- the.game;
  the.game <- calc_next_state the.game 100;

  ()





let render_state state =
  let identity x = x in

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
  glVertex2i the.field.width (identity the.field.height);

  glTexCoord2f offx (adjy +. offy);
  glVertex2i 0 (identity the.field.height);

  glEnd ();
  glDisable gl_texture_2d;

  glEnable gl_texture_2d;

  draw_paddle paddle_padding state.p1_pos state.p1_is_computer;
  draw_paddle (the.field.width - paddle_padding) state.p2_pos state.p2_is_computer;
  glDisable gl_texture_2d;

  let bx, by = state.ball
  in
  draw_ball bx by;

  swap_buffers ();

  glFinish ();

  ()



let render_lerp percent_frame =
  (* calculate lerped ball using engine, very good *)
  ilog "render %d" percent_frame;
  calc_next_state the.game_prev percent_frame |> render_state


let rec random_nonnull lo hi =
  let x = lo + Random.int (hi - lo) in
  if x = 0 then random_nonnull lo hi else x


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


let rec main_loop_v1 last_frame =

  let period = 1.0 /. (float the.hz) in

  let now = Unix.gettimeofday() in
  let tick_diff = now -. last_frame in
  let new_frame = tick_diff > period in
  let new_last_frame =
    if new_frame then begin
      try
        process_events ();
      with No_more_events -> ();
      do_tick ();
      now
    end else last_frame
  in
    (int_of_float (100.0 *. tick_diff /. period) mod 100) |> render_lerp;
  main_loop_v1 new_last_frame


let rec main_loop_v2 expected_frame =

  let period = 1.0 /. (float the.hz) in

  let now = Unix.gettimeofday() in

  if now > expected_frame then begin
    try
      process_events ();
    with No_more_events -> ();
    do_tick ();
    let tick_diff = now -. expected_frame in
    (* log "1diff=%.04f, period=%.04f" tick_diff period; *)
    (int_of_float (100.0 *. tick_diff /. period) mod 100) |> render_lerp;
    let next_frame =
      if expected_frame +. period < now
      then now
      else expected_frame +. period in
    main_loop_v2 next_frame;
  end else begin
    let tick_diff = (period -. (expected_frame -. now)) in
    (* log "2diff=%.04f, period=%.04f" tick_diff period; *)
    (int_of_float (100.0 *. tick_diff /. period) mod 100) |> render_lerp;
    main_loop_v2 expected_frame
  end


let rec main_loop_fuckall () =


  the.hz <- 80;

  let now = Timer.get_ticks() in
  render_lerp 0;
  try process_events () with No_more_events -> ();
  do_tick ();

  let delay = (1000 / the.hz) - (Timer.get_ticks() - now) in
  if delay > 0 then Timer.delay delay;

  main_loop_fuckall ()


let main_loop () =
  Unix.gettimeofday () |> main_loop_v2
  (* main_loop_fuckall () *)

let main () =
  (* test_thrust (); exit 9; *)
  Sdl.init [Sdl.VIDEO];
  Random.self_init ();
  set_caption "Skapong, the boring pong" "skapong";
  initialize_video ();
  restart_game ();
  main_loop ()


let _ = main ()

(* vim: set tw=0 fdm=marker : *)

