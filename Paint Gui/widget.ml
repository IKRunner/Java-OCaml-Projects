(** A library of widgets for building GUIs. *)

(********************)
(** The widget type *)
(********************)

(** A widget is an object that provides three services:
    - it can repaint itself (given an appropriate graphics context)
    - it can handle events
    - it knows its dimensions
*)
type widget = {
  repaint: Gctx.gctx -> unit;
  handle: Gctx.gctx -> Gctx.event -> unit;
  size: unit -> Gctx.dimension
}

(************************)
(**   Layout Widgets    *)
(************************)

(** A simple widget that just occupies space *)
let space (p: Gctx.dimension) : widget =
  { repaint = (fun _ -> ());
    handle = (fun _ _ -> ());
    size = (fun _ -> p);
  }

(** A widget that adds a one-pixel border to an existing widget *)
let border (w: widget) : widget =
  { repaint = (fun (g: Gctx.gctx) ->
        let (width, height) = w.size () in
        let x = width + 3 in    (* not "+ 4" because we start at 0! *)
        let y = height + 3 in
        Gctx.draw_line g (0,0) (x,0);
        Gctx.draw_line g (0,0) (0, y);
        Gctx.draw_line g (x,0) (x, y);
        Gctx.draw_line g (0, y) (x, y);
        let g = Gctx.translate g (2,2) in
        w.repaint g);

    handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
        w.handle (Gctx.translate g (2,2)) e);

    size = (fun () ->
        let (width, height) = w.size () in
        width + 4, height + 4);
  }

(* A helper function that determines whether a given event is within a
   region of a widget whose upper-left hand corner is (0,0) with width
   w and height h.  *)
let event_within (g: Gctx.gctx) (e: Gctx.event)
    ((w, h): Gctx.dimension) : bool =
  let (mouse_x, mouse_y) = Gctx.event_pos e g in
  mouse_x >= 0 && mouse_x < w && mouse_y >= 0 && mouse_y < h

(** The hpair widget lays out two widgets horizontally, aligned at
   their top edges. *)
let hpair (w1:widget) (w2:widget) : widget = {
   repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (fst (w1.size ()),0) in
      w2.repaint g);
   handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g = (Gctx.translate g (fst (w1.size ()), 0)) in
         if event_within g e (w2.size ()) then w2.handle g e else ());
   size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (x1 + x2, max y1 y2))
   }

(** The vpair widget lays out two widgets vertically, aligned at their
   left edges.

   TODO: You will need to implement vpair in Task 1 *)
let vpair (w1: widget) (w2: widget) : widget = {
  repaint = (fun (g: Gctx.gctx) -> w1.repaint g;
    let newG = Gctx.translate g (0, snd(w1.size())) in
      w2.repaint newG);
  
  handle = (fun (g: Gctx.gctx) (e: Gctx.event)-> 
    if event_within g e (w1.size());
    then w1.handle g e
    else let newG = (Gctx.translate g (0, snd (w1.size()))) in
      if event_within newG e (w2.size()) then w2.handle newG e else ());
      
  size = (fun () -> let (x1, y1) = w1.size() in
    let (x2, y2) = w2.size() in (max x1 x2, y1 + y2))
  }
   

(* TIP: the OCaml List module provides a function fold_right
   (List.fold_right) that behaves like the "fold" function we've seen
   on previous homeworks except that it takes its arguments in a
   different order.

   Also, if you look at the List interface, you will see that there is
   a fold_left function. You may want to think about what this does,
   and how it's different from the fold you're used to.  *)

(* TODO: You will need to implement list_layout in Task 1 *)
let list_layout (pair: widget -> widget -> widget)
         (ws: widget list) : widget =
         
  (*Question*)
  List.fold_right (fun x acc -> pair x acc)ws (space(0, 0))
  
let hlist (ws: widget list) : widget = list_layout hpair ws
let vlist (ws: widget list) : widget = list_layout vpair ws


(*****************************)
(**       Label Widgets      *)
(*****************************)

(* Throughout the paint program, we will find the need to associate some value
   with a widget, and also to provide a way to update that value. The mechanism
   for this is "value_controller" objects, which is generic to accomodate
   values of different types. (Don't worry about add_change_listener for now.)

   Because both the widget and the controller share the same, mutable value,
   the constructor must create both together. For label widgets, the value
   we're dealing with is of type string. *)

(** A record of functions that allows us to read and write the string
    associated with a label. *)
type label_controller = { get_label : unit -> string;
                          set_label : string -> unit }

(** Construct a label widget and its controller. *)
let label (s: string) : widget * label_controller =
  let r = { contents = s } in
  { repaint = (fun (g: Gctx.gctx) ->
        Gctx.draw_string g (0,0) r.contents);
    handle = (fun _ _ -> ());
    size = (fun () -> Gctx.text_size r.contents)
  },
  {
    get_label = (fun () -> r.contents);
    set_label = (fun (s: string) -> r.contents <- s);
  }


(*****************************************)
(**    Event Listeners and Notifiers     *)
(*****************************************)

(** An event listener processes events as they "flow" through the widget
    hierarchy. *)

type event_listener = Gctx.gctx -> Gctx.event -> unit

(* Below we define two special forms of event_listeners. *)

(** Performs an action upon receiving a mouse click. *)
let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    if Gctx.event_type e = Gctx.MouseDown then action ()

(** Performs an action upon receiving a key press. *)
let key_listener (action: char -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    begin match Gctx.event_type e with
      | Gctx.KeyPress key -> action key
      | _ -> ()
    end

(** A notifier_controller is associated with a notifier widget.
    It allows the program to add event listeners to the notifier. *)
type notifier_controller = {
  add_event_listener: event_listener -> unit
}

(** A notifier widget is a widget "wrapper" that doesn't take up any
    extra screen space -- it extends an existing widget with the
    ability to react to events. It maintains a list of of "listeners"
    that eavesdrop on the events propagated through the notifier
    widget.

    When an event comes in to the notifier, it is passed to each
    event_listener in turn, and then passed to the child widget. *)
let notifier (w: widget) : widget * notifier_controller =
  let listeners = { contents = [] } in
  { repaint = w.repaint;
    handle =
      (fun (g: Gctx.gctx) (e: Gctx.event) ->
         List.iter (fun h -> h g e) listeners.contents;
         w.handle g e);
    size = w.size
  },
  { add_event_listener =
      fun (newl: event_listener) ->
        listeners.contents <- newl :: listeners.contents
  }


(*****************************************)
(**               Button                 *)
(*****************************************)

(** A button has a string, which can be controlled by the
    corresponding value_controller, and an event listener, which can be
    controlled by the notifier_controller to add listeners (e.g. a
    mouseclick_listener) that will perform an action when the button is
    pressed. *)
let button (s: string)
         : widget * label_controller * notifier_controller =
  let (w, lc) = label s in
  let (w', nc) = notifier w in
  (w', lc, nc)


(*****************************************)
(**               Canvas                 *)
(*****************************************)

(** A Canvas is a bordered widget with a notifier_controller. New
   event listeners can be added using the notifier_controller. The
   interior of the canvas will be redrawn by calling a user-specified
   function, provided as a parameter of the canvas widget
   constructor. *)
let canvas (dim: Gctx.dimension) (paint : Gctx.gctx -> unit)
         : widget * notifier_controller =
  let w =
    { repaint = paint;
      handle = (fun _ _ -> ());
      size = (fun _ -> dim) }
  in
  notifier (border w)


(*****************************************)
(**              Checkbox                *)
(*****************************************)
(* TODO: Task 5 requires you to develop a checkbox widget *)

(** A checkbox is a controller for a value associated with a widget.
    This controller can read and write the value. It also allows change
    listeners to be registered by the application. The listeners are
    run whenever this value is set. *)
type 'a value_controller = {
  add_change_listener : ('a -> unit) -> unit;
  get_value           : unit -> 'a;
  change_value        : 'a -> unit
}

(** TODO: The first part of task 5 requires you to implement the following
    generic function. This function takes a value of type 'a and returns a
    value controller for it. Carefully consider what state needs to be
    associated with any value controller. *)
let make_controller (v: 'a) : 'a value_controller =
  (*Internal state*)
  let initVal = {contents = v} in      (*Initial value*)
  let changeList = {contents = []} in  (*Initial list of change listeners*)
 
  { 
    (*Add new change listener to list of change listeners*)
    add_change_listener = (fun f -> changeList.contents <- 
                           changeList.contents @ [f]);
                           
    (*Return value stored by value_controller.*)
    get_value = (fun () -> initVal.contents);
    
    (*Update value_controller to provided value;
      *Call list of change_listeners in value_controller
      *with newly set value as argument to each.*)       
    change_value = (fun x -> initVal.contents <- x; List.iter (fun n -> n 
                    initVal.contents) changeList.contents)
  }
  


(** TODO: Don't forget to use the helper function you defined above
   when implementing the checkbox function!

   If your checkbox implementation does not work, do _not_ comment it
   out, because your code will not compile upon submission. Instead,
   you can replace the function body with

      failwith "Checkbox: unimplemented"

    before submitting your code. *)
let checkbox (init: bool) (s: string) : widget * bool value_controller =
  
  (*Internal state*)
  (let state = make_controller init in  
  
  {
    (*Create checkbox, fill if checkbox state changes to ON*)
    repaint = (fun (g) -> Gctx.draw_rect g (0, 20) (20, 20); 
                          Gctx.draw_string g (21, 0) s; 
                          if state.get_value() = true 
                          then  Gctx.fill_rect g (0, 19) (19, 19) else ());
    
    (*If click detected in bounds of checkbox then change state*)
    handle = (fun (g) (e) -> begin match Gctx.event_type e with 
                               | Gctx.MouseDown -> if state.get_value()
                                                   then state.change_value false
                                                   else state.change_value true
                               | _ -> () (*Anything else DO NOTHING*)
                             end);
    
    (*Return size of checkbox including string*)
    size = (fun () -> let (w, h) = Gctx.text_size s in (20 + w, max 20 h))  
  }
  
    (*Return bool value controller*)
    , state)
  


(*****************************************)
(**          Additional widgets          *)
(*****************************************)

(* TODO: In Task 6 you may choose to add a radio_button widget, a
   color slider, or (after discussing your idea with course staff via
   a private Piazza post) some other widget of your choice. *)

(**************************)
(*Display current slider color*)
let slider_color (color: Gctx.color) (dim: int * int) (init: int) 
  : widget * int value_controller = 
  
  (*Internal state*)
  let state = make_controller init in
 
  ({
    (*Repaint rectangle according  to  location of click*)
    repaint = (fun (g) ->  (*Draw rectangle to fill*)
                           Gctx.draw_rect g (0, 10) dim; 
                          (*Scale down rgb value to width of rectangle*)
                           let fillWidth = int_of_float ((float_of_int 
                           (fst(dim))) *. ((float_of_int (state.get_value())) 
                           /. 255.0)) in
                           
                           (*Set height of fill*)
                           let fillHeight = snd(dim) - 2 in
                           
                           (*Repaint slider with current value of color*)
                           Gctx.fill_rect (Gctx.with_color g color) (1, 9) 
                           (fillWidth, fillHeight));  
    
    (*On appropriate mouse event, scale up coordiate to rgb value*)
    handle = (fun (g) (e) -> let mousPos = Gctx.event_pos e g in  
    
                             begin match Gctx.event_type e with
                                (*Fill on click*)
                                | Gctx.MouseDown -> let newFill = int_of_float
                                  (255.0 *. ((float_of_int (fst(mousPos))) /. 
                                  (float_of_int (fst(dim))))) in 
                                  state.change_value newFill
                                (*Fill on Drag*)  
                                | Gctx.MouseDrag -> let newFill = int_of_float
                                  (255.0 *. ((float_of_int (fst(mousPos))) /. 
                                  (float_of_int (fst(dim))))) in 
                                  state.change_value newFill
                                | _ -> () (*Anything else DO NOTHING*)
                              end);
    
   (*Return dimension*)
   size = (fun () -> dim)
  }
  
  (*Return value controller*)
  , state)
(**************************)


(**************************)
(*Display current slider count*)
let slider_count (count: int) (color: string) : widget * int value_controller = 
 
 (*Internal state*)
 let state = make_controller count in
 
 (*Dimention of text argument*)
 let (w, h) = Gctx.text_size color in 
  
({
  (*Draw current color and count*)
  repaint = (fun g -> Gctx.draw_string g (5, 0) color; 
                       Gctx.draw_line g (0, h + 1) (w + 10, h + 1);
                       Gctx.draw_string g (0, h + 2) 
                       (string_of_int (state.get_value())));
 
  (*DO NOTHING*)
  handle = (fun (_) (_) -> ());
  
  (*Returns widget size*)
  size = (fun () -> (30, h + 1 + snd(Gctx.text_size
                    (string_of_int (state.get_value())))))
}

(*Return value controller*)
, state)

(**************************)

(**************************)
  
(*TODO Thickness slider*)  
let slider_thick (width: int) (maxThick: int )(init: int) : 
  widget * int value_controller =

  (*Internal State*)
  let state = make_controller init in
  
  
  ({
    repaint = (fun g ->  (*Draw line*)
                         Gctx.draw_line g (0, 10) (width, 10);  
                         
                          let slidPos = int_of_float ((float_of_int width) *. 
                          ((float_of_int (state.get_value())) /. 
                          (float_of_int maxThick)) ) in
                          
                         (*Draw slider box according to current state value*)
                         Gctx.draw_rect g (slidPos, 20) (3, 20);
                         Gctx.fill_rect g (slidPos, 20 ) (2, 19));
                         
    (*On appropriate mouse event, scale up coordiate to thickness value*)                          
    handle = (fun (g) (e) -> let mousPos = Gctx.event_pos e g in
               
                              begin match Gctx.event_type e with
                                | Gctx.MouseDown -> let newPos = int_of_float((
                                  float_of_int maxThick) *. ((float_of_int (fst
                                  (mousPos))) /. (float_of_int width)))in
                                  state.change_value newPos
                                | Gctx.MouseDrag -> let newPos = int_of_float((
                                  float_of_int maxThick) *. ((float_of_int (fst
                                  (mousPos))) /. (float_of_int width)))in
                                  state.change_value newPos
                                | _ -> ()
                              end);
                              
    (*Return dimension*)                          
    size = (fun () -> (width, 20))
  
  }
  
  (*Return value controller*)
  , state)

(**************************)
