open Printf

open Sdl
open Video
open Window
open Timer
open Event
open SDLGL
open Glcaml

let (|>) a b = b a

(* all size measurements are in millimeters *)
(* meters/centimeters -> millimeters *)
type dimension = int

let dimension_m m = m * 1000

let dimension_cm cm = cm * 10

let tau = 6.2831852

let rad_of_angle a = float a *. tau /. 360.0

let log m  = kprintf (fun m -> printf "%s\n%!" m) m
let ilog m = kprintf (fun m -> ()) m (* ignore log *)

let half x = x / 2
let middle_of a b = ( a + b ) / 2


type dimensions =
  { mutable width:  dimension
  ; mutable height: dimension
  }

type gamestate =
  { mutable ball:    dimension * dimension (* current position *)

  ; mutable ltr:     bool (* true if ball moving from left to right *)
  ; mutable angle:   int  (* angle, 0..180 *)
  ; mutable speed:   int  (* speed, mm/s *)


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
  ; ltr     = false
  ; angle   = 0 (* 0..180 *)
  ; speed   = 0 (* mm/s *)

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
  ; field:         dimensions
  ; paddle:        dimensions
  ; start_time:    float
  ; states:        (string, string) Hashtbl.t
  ; mutable game:  gamestate
  ; mutable game_prev: gamestate
  ; hz:            int (* game updates/s *)
  }


let the =
  { screen = { width  = 800
             ; height = 600
             }

  ; field = { width  = dimension_m 100
            ; height = dimension_m 75
            }

  ; paddle = { width  = dimension_cm 100
             ; height = dimension_cm 900
             }

  ; game       = gamestate_clean
  ; game_prev  = gamestate_clean
  ; start_time = Unix.gettimeofday ()
  ; states     = Hashtbl.create 4
  ; hz         = 20
  }


let restart_game () =
  if the.game.state = "stop" then (
    the.game.ball  <- half the.field.width, half the.field.height;
    the.game.ltr   <- Random.bool ();
    the.game.angle <- 40 + Random.int 100;
    the.game.speed <- dimension_m 40;
    the.game.state <- "play";
  )


let gl_resize () =
  glOrtho 0.0 (float the.field.width) 0.0 (float the.field.height) 0.0 1.0;
  glMatrixMode gl_modelview


let init_gl () =
  glMatrixMode gl_projection;
  glLoadIdentity ();
  glDisable gl_depth_test;

  restart_game ();
  gl_resize ()



let draw_paddle x y is_computer =
  let xx = float x
  and yy = float y
  and hh = half the.paddle.height |> float
  and ww = half the.paddle.width |> float
  in
  glColor3f 1.0 (if is_computer then 0.0 else 1.0) 0.0;
  glBegin gl_quads;
  glVertex2f (xx -. ww) (yy -. hh);
  glVertex2f (xx +. ww) (yy -. hh);
  glVertex2f (xx +. ww) (yy +. hh);
  glVertex2f (xx -. ww) (yy +. hh);
  glEnd ()


let draw_ball x y =
  let fx = float x
  and fy = float y
  and ball_size = 400.0 in
  glColor3f 1.0 1.0 0.7;
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


let calc_next_state os percent_frame =

  if os.state <> "play" || percent_frame = 0
  then os
  else begin
    let ns = { os with state = os.state } (* new state *)
    and bx, by = os.ball
    and inc = ((dimension_m 60) / the.hz) * percent_frame / 100

    and dx = ref 0 and dy = ref 0

    and speed = (os.speed / the.hz) * percent_frame / 100

    in

    (* calculate the ball advancement *)

    if os.angle >= 90
    then begin
      dx := int_of_float ((float speed) *. ((180 - os.angle) |> rad_of_angle |> sin));
      dy := -int_of_float ((float speed) *. ((180 - os.angle) |> rad_of_angle |> cos));
    end else begin
      dx := int_of_float (float speed *. (os.angle |> rad_of_angle |> sin));
      dy := int_of_float (float speed *. (os.angle |> rad_of_angle |> cos));
    end;

    if not os.ltr then dx := - !dx;

    (* a simple AI logic *)
    if ns.p1_is_computer then (
      let hit_y =
        by + (if os.ltr then !dy else - !dy) * bx / !dx in

      let move_up = os.p1_pos + half the.paddle.height < hit_y
      and move_dn = os.p1_pos - half the.paddle.height > hit_y in
      os.p1_move <- "none";
      if move_up then os.p1_move <- "up";
      if move_dn then os.p1_move <- "down";
    );


    if ns.p2_is_computer then (
      let hit_y =
        by + (if os.ltr then !dy else - !dy) * (the.field.width - bx) / !dx in
      let move_up = os.p2_pos + half the.paddle.height < hit_y
      and move_dn = os.p2_pos - half the.paddle.height > hit_y in
      os.p2_move <- "none";
      if move_up then os.p2_move <- "up";
      if move_dn then os.p2_move <- "down";
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

    let nx = ref (bx + !dx) and ny = ref (by + !dy) in

    (* check reflections *)

    if !ny <= 0 || !ny >= the.field.height then (
      ny := by;
      dy := - !dy;
      ns.angle <- 180 - os.angle;
    );

    (* reflect left paddle? *)

    if (not os.ltr) && bx >= the.paddle.width && !nx < the.paddle.width
    then begin
      (* paddle vertically (y) takes pos - paddle.height / 2 ... pos + paddle.height / 2 *)
      let lower = os.p1_pos - half the.paddle.height
      and upper = os.p1_pos + half the.paddle.height
      in
      if is_between !ny lower upper || is_between by lower upper
      then begin
        nx := bx; (* adjust the coordinate not to cross the paddle *)
        dx := - !dx;
        ns.ltr <- true;
        ns.angle <- choose_angle (2.0 *. float (ns.p1_pos - !ny) /. (float the.paddle.height));
        ns.speed <- ns.speed + (dimension_m 5);
      end;
    end;

    (* reflect right paddle? *)

    if os.ltr && bx <= the.field.width - the.paddle.width && !nx > the.field.width - the.paddle.width
    then begin
      let lower = os.p2_pos - half the.paddle.height
      and upper = os.p2_pos + half the.paddle.height
      in
      if is_between !ny lower upper || is_between by lower upper
      then begin
        nx := bx; (* adjust the coordinate not to cross the paddle *)
        dx := - !dx;
        ns.ltr <- false;
        ns.angle <- choose_angle (2.0 *. float (ns.p2_pos - !ny) /. (float the.paddle.height));
        ns.speed <- ns.speed + (dimension_m 5);
      end;
    end;

    if !nx > the.field.width || !nx <= 0
    then begin
      (* ball out of bounds, break game *)
      nx := half the.field.width;
      ny := half the.field.height;
      ns.state <- "stop";
    end;

    ns.ball <- !nx, !ny;

    ns

  end



let do_tick () =

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
  glClearColor 0.1 0.1 0.1 0.0;
  glClear gl_color_buffer_bit;

  glLoadIdentity ();

  let padding = 100 + the.paddle.width / 2
  in

  draw_paddle padding state.p1_pos state.p1_is_computer;
  draw_paddle (the.field.width - padding) state.p2_pos state.p2_is_computer;

  let bx, by = state.ball
  in
  draw_ball bx by;

  swap_buffers ();

  ()


let render () =
  render_state the.game_prev



let render_lerp =

  let lerp1 perc a b =
    a + (b - a) * perc / 100

  in

  let lerp2 perc a b =
    let a1, a2 = a
    and b1, b2 = b in
    (lerp1 perc a1 b1), (lerp1 perc a2 b2)

  in

  let render_lerp_1 percent_frame =
    (** method 1, pure lerp between previous and next position:
      * much suckage at the angles as they get smoothed out *)
    (* multiplier: state advance 0...100 *)
    let lerp_state = {
      the.game_prev with
          ball = lerp2 percent_frame the.game_prev.ball the.game.ball;
          p1_pos = lerp1 percent_frame the.game_prev.p1_pos the.game.p1_pos;
          p2_pos = lerp1 percent_frame the.game_prev.p2_pos the.game.p2_pos
    } in
    render_state lerp_state

  in

  let render_lerp_2 percent_frame =
    (* method 2: calculate lerped ball using engine, very good *)
    calc_next_state the.game_prev percent_frame |> render_state

  in

  render_lerp_2


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



let process_keyboard () =
    match poll_event () with
    | Key k -> (
            match k.sym with
            | K_ESCAPE ->
                exit 0;
            | K_1 ->
                if k.keystate = PRESSED then set_computer_p1 ();
            | K_2 ->
                if k.keystate = PRESSED then set_computer_p2 ();
            | K_SPACE ->
                if k.keystate = PRESSED then restart_game ();
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
    | Resize r ->
        ilog "resize to %dx%d" r.w r.h;
        the.screen.width <- r.w;
        the.screen.height <- r.h;
        gl_resize ();
    | _ -> ()


let rec main_loop last_frame =

  let period = 1.0 /. (float the.hz) in

  let now = Unix.gettimeofday() in
  let tick_diff = now -. last_frame in
  (* log "tick/diff=%.3f" tick_diff; *)
  let new_frame = tick_diff > period in
  let new_last_frame =
    if new_frame then begin
      process_keyboard ();
      do_tick ();
      now
    end else last_frame
  in
  (*if new_frame then begin
    render ();
  end else begin*)
    (int_of_float (100.0 *. tick_diff /. period) mod 100) |> render_lerp;
    (* Timer.delay 5; *)
  (*end;*)
  main_loop new_last_frame


let main () =
  Sdl.init [Sdl.VIDEO];
  Random.self_init ();
  ignore( set_video_mode the.screen.width the.screen.height 32 [OPENGL] );
  set_caption "Skapong, the boring pong" "skapong";
  init_gl ();
  swap_buffers ();
  main_loop 0.0


let _ = main ()

(* vim: set tw=0 : *)

