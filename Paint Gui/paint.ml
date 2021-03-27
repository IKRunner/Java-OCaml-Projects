(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(*Line thickness*)
type thickness = int

(*New shape thickness globals*)
let newThick = 10
let deFault = 1

(*Slider dimension*)
let sliderDim = (100 , 10)
let thickSlidWidth = 100

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 2, 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of {color: color; thickness: thickness; p1: point; p2: point}
  | Points of {color: color; points: point list}
  | Ellipse of {color: color; thickness: thickness; p1: point; p2: point}
  | Rectangle of {color: color; thickness: thickness; p1: point; p2: point}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location of
        the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 2, 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point
  | RectStartMode
  | RectEndMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 3, 5, and *)
  (* possibly 6 *) 
  
  (*Current linke thickness*)
  mutable thickness : thickness;
  
  (*Shape to be previewed*)
  mutable preview: shape option; (*Added field of state*)
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  thickness = deFault;      (*Added*)
  preview = None;           (*Added*)
  (* TODO: You will need to add new state for Tasks 3, 5, and maybe 6 *)
  
}


(** This function creates a graphics context with the appropriate
    pen color.
*)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (t: thickness) : gctx =
  let g = with_color g c in
  let g = with_thickness g t in
  g


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview.  *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points ps -> draw_points (with_params g ps.color deFault) ps.points 
                    (*Added*)
      (*Calculated using bounding box method*)
      | Ellipse e -> let (x1, y1) = e.p1 in let(x2, y2) = e.p2 in
                     let origin = ((x1 + x2)/2,(y1 + y2)/2) in 
                     let rx = abs (x2 - x1)/2 in
                     let ry = abs (y2 - y1)/2 in
                     draw_ellipse (with_params g e.color e.thickness) 
                                   origin rx ry (*Added*)
     (*Added*)
     | Rectangle r -> let (x1, y1) = r.p1 in let (x2, y2) = r.p2 in 

                      if x2 > x1  && y2 < y1
                      then draw_rect (with_params g r.color r.thickness) 
                           (x1,y1) ( abs(x2-x1),  abs(y2-y1))
          
                      else if x2 > x1 && y2 > y1 
                      then draw_rect (with_params g r.color r.thickness) 
                           (x1, y2) (abs (x1-x2), abs (y1-y2))
          
                      else if x2 < x1 && y2 < y1
                      then draw_rect (with_params g r.color r.thickness) 
                           (x2, y1)(abs (x1-x2), abs (y1-y2))
         
                      else draw_rect (with_params g r.color r.thickness) 
                          (x2, y2) (abs (x1-x2), abs (y1-y2))
         
   end in
  Deque.iterate draw_shape paint.shapes;

  (*Draw shape preview ADDED*)
  begin match paint.preview with
  | None -> ()
  | Some n -> draw_shape n
  end

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur 
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      begin match paint.mode with 
        (*Line mode*)
        | LineStartMode ->
           (* The paint_canvas was waiting for the first click of a line,   
              so change it to LineEndMode, recording the starting point of  
              the line. *)
           paint.mode <- LineEndMode p
        | LineEndMode p1 -> () (*DO NOTHING*)
         (*Point mode, create point preview*)
        | PointMode -> paint.preview <- Some (Points {color = paint.color; 
                                         points = [p]});
         (*Ellipse mode*)
        | EllipseStartMode -> paint.mode <- EllipseEndMode p
        | EllipseEndMode p1 -> () (*DO NOTHING*)
        
        (*Rectangle mode*)
        | RectStartMode -> paint.mode <- RectEndMode p
        | RectEndMode pr -> () (*DO NOTHING*)
       end
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged    
         with the button down. Initially there is nothing to do, but you'll  
         need to update this part for Task 2, 3, 4 and maybe 6. *)
       begin match paint.mode with
        (*Line mode*)
        | LineEndMode p1 -> paint.preview <- Some (Line {color = paint.color; 
                                             thickness = paint.thickness;
                                             p1 = p1; p2 = p}); 
         (*Point mode, continually add point to preview*)
        | PointMode -> let points =  begin match paint.preview with
                                      | Some (Points ps) -> ps.points
                                      | _ -> []
                                     end in 
                         paint.preview <- Some (Points {color = paint.color; 
                                          points = [p] @ points})
        (*Ellipse mode*)
        | EllipseEndMode px -> paint.preview <- Some (Ellipse {color = 
                                                paint.color; thickness = 
                                                paint.thickness; p1 = p; 
                                                p2 = px})
        (*Rectangle mode*)
        | RectEndMode pr -> paint.preview <- Some (Rectangle {color = 
                                             paint.color; thickness = 
                                             paint.thickness; p1 = p;
                                             p2 = pr}) 
        | _ -> () (*DO NOTHING*)
       end     
       
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2, *)
      (* 3, 4, and possibly 6 need to do something different here.           *)
      begin match paint.mode with
        (*Line mode, create line and add to deque of shapes*)
        | LineEndMode p1->  Deque.insert_tail (Line {color = paint.color; 
                            thickness = paint.thickness; p1 = p1; p2 = p}) 
                            paint.shapes; paint.preview <- None; 
                            paint.mode <- LineStartMode
        (*Point mode, create point list and add to deqeue of shapes*)
        | PointMode -> begin match paint.preview with 
                         | None -> ()
                         (*Extract list of points from preview
                           * clear preview and insert into deque of shapes*)
                         | Some x  -> begin match x with
                                        | Points p -> paint.preview <- None; 
                                          Deque.insert_tail (Points {color = 
                                          paint.color; points = p.points}) 
                                          paint.shapes
                                        | _ -> () (*DO NOTHING*)
                                      end
                        end                     
        (*Ellipse mode, create ellipse and add to deque of shapes*)
        | EllipseEndMode px -> Deque.insert_tail (Ellipse {color = paint.color; 
                               thickness = paint.thickness; p1 = p; p2 = px}) 
                               paint.shapes; paint.preview <- None; 
                               paint.mode <- EllipseStartMode
        (*Rectangle mode, create rectangle and add to deque of shapes*)
        | RectEndMode pr -> Deque.insert_tail (Rectangle {color = paint.color;
                            thickness = paint.thickness; p1 = p; p2 = pr})
                            paint.shapes; paint.preview <- None;
                            paint.mode <- RectStartMode
        | _ -> () (*DO NOTHING*)
      end
    
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over  
       the canvas without pushing any buttons) and the KeyPress event (where 
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the
   paint program -- the buttons, color selectors, etc., and
   lays them out in the top - level window. *)
(* TODO: Tasks 1, 2, 4, 5, and 6 involving adding new buttons or
   changing the layout of the Paint GUI. Initially the layout is very
   ugly because we use only the hpair widget demonstrated in
   Lecture. Task 1 is to make improvements to make the layout more
   appealing. You may choose to arrange the buttons and other GUI
   elements of the paint program however you like (so long as it is
   easily apparent how to use the interface ).  The sample screen shot
   of our solution provides one possible design.  Also, feel free to
   improve the visual components of the GUI, for example, our solution
   puts borders around the buttons and uses a custom "color button"
   that changes its appearance based on whether or not the color is
   currently selected.  *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(*Create Point button*)
let (w_point, lc_point, nc_point) = button "Point"

(*Create Line button*)
let (w_line, lc_line, nc_line) = button "Line"

(*Create Elllipse button*)
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"

(*Create Rectangle button*)
let (w_rectangle, lc_rectangle, nc_rectangle) = button "Rectangle"

(*Create thick line widget and value controller*)
let (thickWidget, thickControl) = (checkbox false "Thick line") 

(*Create slider widgets with specified color and value controller*)
let (redSlideWidget, redSlideControl) = slider_color red sliderDim 0
let (blueSlideWidget, blueSlideControl) = slider_color blue sliderDim 0
let (greenSlideWidget, greenSlideControl) = slider_color green sliderDim 0
let (thickSlideWidget, thickSlideControl) = slider_thick thickSlidWidth 50 23

(*Create slider count widgets*)
let (redSlideDisplay, redDispControl) = slider_count 
                                          (redSlideControl.get_value()) "r"
 
let (greenSlideDisplay, greenDispControl) = slider_count 
                                         (greenSlideControl.get_value()) "g"
 
let (blueSlideDisplay, blueDispControl) = slider_count 
                                         (blueSlideControl.get_value()) "b"

let (thickSlideDisplay, thickDispControl) = slider_count 
                                         (thickSlideControl.get_value()) "Th"

 
(*Create Line, Point, Ellipse, Rectangle, Thickline, Undo, slider borders*)
let lineBorder = border w_line
let thickBorderWidget = border thickWidget
let pointBorder = border w_point
let ellipseBorder = border w_ellipse
let undoBorder = border w_undo
let rectangleBorder = border w_rectangle


(** This function runs when the Undo button is clicked.
   It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 2, 3 and 4. *)
let undo () : unit =
  paint.preview <- None;
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)
    
    
(*Undo event listener*)
;; nc_undo.add_event_listener (mouseclick_listener undo)

(*Point button event listener ADDED*)
;; nc_point.add_event_listener (mouseclick_listener (fun () -> paint.mode <- 
                                                              PointMode))
                                                              
(*Line button event listener ADDED*)     
;; nc_line.add_event_listener (mouseclick_listener (fun () -> paint.mode <- 
                                                              LineStartMode))
                                                              
(*Ellipse button event listener ADDED*)     
;; nc_ellipse.add_event_listener (mouseclick_listener (fun () -> paint.mode <- 
                                                              EllipseStartMode))                                                             
(*Rectangle button event listener ADDED*)     
;; nc_rectangle.add_event_listener (mouseclick_listener (fun () -> paint.mode <- 
                                                              RectStartMode))

(****Thickness change listeners****)

(*Listener for thickness slider.
 *When checkbox is checked, turn on slider thickness controller. 
 *Update thickness display*)
;; thickSlideControl.add_change_listener (fun value -> 
                        (if thickControl.get_value() 
                        then paint.thickness <- thickSlideControl.get_value());
                        thickDispControl.change_value value);

(*Listener for change of state of checkbox
 *When checkbox checked, update thickness to current slider value
 *Change thickness to default when checkbox unchecked*)
;; thickControl.add_change_listener (fun value -> 
                          if value 
                          then paint.thickness <- thickSlideControl.get_value()
                          else paint.thickness <- deFault);

(**********************************)


(*Add change listeners to slider control widgets
 *Slider control display responds to respective rgb slider ADDED*)                                                    
;; redSlideControl.add_change_listener(fun (value) -> paint.color <- 
                                { r = value; g = greenSlideControl.get_value(); 
                                b = blueSlideControl.get_value() };
                                redDispControl.change_value value)
                                                       
;; greenSlideControl.add_change_listener(fun (value) ->  paint.color <- 
                                { r = redSlideControl.get_value(); g = value; 
                                b = blueSlideControl.get_value() };
                                greenDispControl.change_value value)
                                                       
;; blueSlideControl.add_change_listener(fun (value) ->  paint.color <- 
                                 { r = redSlideControl.get_value(); 
                                 g = greenSlideControl.get_value(); b = value };
                                 blueDispControl.change_value value)


(** A spacer widget *)
let spacer : widget = space (10,10)
 
(*Create slider vlist Widget ADDED*)
let slid_widg : widget =
  vlist[redSlideWidget; greenSlideWidget; blueSlideWidget]

(*Create slider counter hlist Widget*)
let count_widg : widget = 
  hlist[redSlideDisplay; spacer; greenSlideDisplay; spacer; blueSlideDisplay]

  
  
(** The mode toolbar, initially containing just the Undo button. *)
(*  TODO: you will need to add more buttons to the toolbar in 
    Tasks 3, 5, and possibly 6. *)
let mode_toolbar : widget = 
  Widget.hlist [spacer; spacer; lineBorder; spacer; pointBorder; spacer; 
                ellipseBorder; spacer; rectangleBorder; spacer; 
                thickBorderWidget; spacer; undoBorder;  spacer; 
                thickSlideWidget]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color 
   and some buttons for changing it. Both the indicator and the buttons 
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given 
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, width-1) (width-1, width-1) in   
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected 
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created 
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)  
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c; (*Automatically update rgb slider and display*)
                        redSlideControl.change_value c.r;
                        blueSlideControl.change_value c.b;
                        greenSlideControl.change_value c.g));
  (*Return color button widget*)
  w

(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
   let color_toolbar : widget =

(**Old Version**)
(*     hpair (hpair color_indicator spacer)
   (hpair (hpair (color_button black) spacer)
   (hpair (hpair (color_button white) spacer)
   (hpair (hpair (color_button red) spacer)
   (hpair (hpair (color_button green) spacer)
   (hpair (hpair (color_button blue) spacer)
   (hpair (hpair (color_button yellow) spacer)
   (hpair (hpair (color_button cyan) spacer)
   (color_button magenta)))))))) *)
 (**************)  
   
     Widget.hlist [spacer; spacer; color_indicator; spacer; slid_widg; spacer; 
                   color_button black; spacer; color_button white; spacer; 
                   color_button red; spacer; color_button green; spacer;
                   color_button blue; spacer; color_button yellow; spacer; 
                   color_button cyan; spacer; color_button magenta; spacer; 
                   count_widg; spacer; thickSlideDisplay]

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
let paint_widget =
(*    hpair mode_toolbar (hpair spacer (hpair color_toolbar paint_canvas)) *)
  Widget.vlist [paint_canvas; spacer; mode_toolbar; spacer; color_toolbar;]


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
