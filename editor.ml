
let (>>=) = Lwt.bind

let px s = Js.string (string_of_int s ^ "px")
let em s = Js.string (string_of_int s ^ "em")
let percent s = Js.string (string_of_int s ^ "%")
let key_of_code = function
  | 3 -> `Enter
  | 8 -> `Backspace
  | 9 -> `Tab
  | 13 -> `Enter
  | 16 -> `Shift
  | 17 -> `Ctrl
  | 18 -> `Alt
  | 19 -> `Pause
  | 20 -> `CapsLock
  | 27 -> `Esc
  | 32 -> `Space
  | 33 -> `PageUp
  | 34 -> `PageDown
  | 35 -> `End
  | 36 -> `Home
  | 37 -> `Left
  | 38 -> `Up
  | 39 -> `Right
  | 40 -> `Down
  | 44 -> `PrintScrn
  | 45 -> `Insert
  | 46 -> `Delete
  | 127 -> `Delete
  | 63232 -> `Up
  | 63233 -> `Down
  | 63234 -> `Left
  | 63235 -> `Right
  | 63272 -> `Delete
  | 63273 -> `Home
  | 63275 -> `End
  | 63276 -> `PageUp
  | 63277 -> `PageDown
  | 63302 -> `Insert
  | n -> `Other n

(* Render the editor *)
module View = struct

  class type input = object
  inherit Dom_html.element
  method focus : unit Js.meth
  method blur : unit Js.meth
  end
  type 'a t = {
    view : 'a Zed_view.t;
    dom_cursor : Dom_html.element Js.t;
    mutable blink_cursor : unit Lwt.t;
    container : Dom_html.element Js.t;
    container_txt : Dom_html.element Js.t;
    mutable input : input Js.t;
    mutable col_size : int;
  }

  let create c parent =
    let view = Zed_view.create c 10 83 in
    let container = Dom_html.(createDiv document) in
    let container_txt = Dom_html.(createDiv document) in
    Dom.appendChild container container_txt;
    container##style##position <- Js.string "relative";
    Dom.appendChild parent container;
    (* container_txt##style##height  <- px (11  * 16); *)
    (* container_txt##style##overflow <- Js.string "auto"; *)
    let dom_cursor = Dom_html.(createSpan document) in
    dom_cursor##classList##add(Js.string "cursor");
    dom_cursor##style##position <- Js.string "absolute";
    dom_cursor##innerHTML <- Js.string "\xC2\xA0";
    {
      view;
      container;
      container_txt;
      dom_cursor;
      blink_cursor = Lwt.return_unit;
      input = (Dom_html.(createInput document) :> input Js.t);
      col_size = 0;
    }

  let size_of_line () = 18
  let size_of_col () = 8


  let display' t =
    (* Format.eprintf "display %s@." t.name; *)
    let start,lines = Zed_view.get_lines t.view in
    (* let all_lines = Zed_edit.lines (Zed_view.edit t.view) in *)
    (* let count = Zed_lines.count all_lines in *)
    let content = t.container_txt in
    let cursor = Zed_view.cursor t.view in
    let cur = Zed_cursor.get_line cursor in
    content##innerHTML <- Js.string "";
    (* let s_div = Dom_html.(createDiv document) in *)
    (* s_div##style##height <- px (start * 18); *)
    (* let e_div = Dom_html.(createDiv document) in *)
    (* let emiss = count - (List.length lines) - start in *)
    (* e_div##style##height <- px (emiss * 18); *)
    (* Dom.appendChild content s_div; *)
    let start = ref start in
    let numsize =
      let rec loop r acc =
        if acc < 10 then r + 1 else loop (succ r) (acc/10)
      in loop 1 (!start + List.length lines) in

    t.col_size <- numsize;
    List.iter (fun r ->
        let div = Dom_html.(createDiv document) in
        div##classList##add(Js.string "line");
        div##style##width <- percent 100;
        if !start = cur
        then div##classList##add(Js.string "current");
        let span = Dom_html.(createSpan document) in
        span##innerHTML <- Js.string (string_of_int (1 + !start));
        span##style##width <- px (size_of_col () * t.col_size);
        Dom.appendChild div span;
        (match r with
         | Some r ->
           let d = Dom_html.(createPre document) in
           d##innerHTML <- Js.string (Zed_rope.to_string r);
           Dom.appendChild div d;
         | None ->
           div##classList##add(Js.string "empty");
           let d = Dom_html.(createPre document) in
           Dom.appendChild div d);
        incr start;
        Dom.appendChild content div
      ) lines;
      (* Dom.appendChild content e_div *)
    ()
  let display t =
    Dom_html._requestAnimationFrame (Js.wrap_callback (fun _ -> display' t))

  let rec show_cursor view () =
    view.dom_cursor##style##visibility <- Js.string "visible";
    Lwt_js.sleep 0.530 >>= hide_cursor view
  and hide_cursor view () =
    view.dom_cursor##style##visibility <- Js.string "hidden";
    Lwt_js.sleep 0.530 >>= show_cursor view

  let round x = int_of_float (x +. 0.5)

  let pos_of_xy v x y =
    let lines = Zed_edit.lines (Zed_view.edit v.view) in
    let line = int_of_float (y /. (float_of_int (size_of_line ())) ) + Zed_view.line_start v.view in
    let col =  round (x /. (float_of_int (size_of_col ()))) - v.col_size in
    try
      let pos = Zed_lines.line_start lines line in
      let en = Zed_lines.line_stop lines line in
      let delta = en - pos in
      let col = if col > delta then delta else col in
      pos + col
    with
      Zed_lines.Out_of_bounds -> Zed_lines.length lines


  let focus ?e v =
    begin
      match e with
      | None -> ()
      | Some e ->
        let t = Dom_html.eventTarget e in
        let rect = v.container_txt##getBoundingClientRect() in
        let x = float_of_int e##clientX -. rect##left in
        let y = float_of_int e##clientY -. rect##top in
        let pos = pos_of_xy v x y in
        Format.eprintf "x:%f ; y:%f@." x y;
        Format.eprintf "goto pos %d@." pos;
        (try Zed_edit.goto (Zed_view.context v.view) pos with _ -> ());
        Js.Unsafe.global##debugEvent <- e;
        Js.Unsafe.global##debugTarget <- x;
        print_endline (Js.to_string (Obj.magic  (t##textContent)))
    end;
    Lwt.cancel v.blink_cursor;
    v.input##focus();
    v.blink_cursor <- show_cursor v ()

  let blur v =
    Lwt.cancel v.blink_cursor;
    v.input##blur();
    v.dom_cursor##style##visibility <- Js.string "hidden"

  let cursor_position v cb =
    let _ = React.S.map (fun (line, col) ->
        cb (line * size_of_line ()) (col * (size_of_col ())  + (v.col_size * (size_of_col ())))
      ) (Zed_view.cursor_position v.view)
    in
    ()

  let set_input t dom =
    let div = Dom_html.(createDiv document) in
    Dom.appendChild div t.dom_cursor;
    cursor_position t (fun top left ->
        div##style##top <- px top;
        div##style##left <- px left;

        t.dom_cursor##style##top <- px top;
        t.dom_cursor##style##left <- px left
      );
    dom##onblur <- Dom_html.handler (fun _ -> blur t; Js._false);
    div##style##position <- Js.string "absolute";
    div##style##height <- px 0;
    div##style##width <- px 3;
    div##style##overflow <- Js.string "hidden";
    Dom.appendChild div dom;
    Dom.appendChild t.container div;
    Dom.appendChild t.container t.dom_cursor;
    t.input <- (dom :> input Js.t)

  let init view =
    Lwt.ignore_result (
      Lwt_js_events.blurs
        Dom_html.window
        (fun _ _ ->
           blur view;
           Lwt.return_unit));
    blur view;
    view.container##onmousedown <- Dom_html.handler (fun e -> focus ~e view; Js._false);
    let _ = React.E.map (fun _ -> display view) (Zed_view.changes view.view)
    in display view;
    ()
end

(* Basic input, doesn't handle selection *)
module Input = struct

  type 'a t = {
    input : Dom_html.textAreaElement Js.t;
    mutable prev : Js.js_string Js.t;
    context : 'a Zed_edit.context;
    view : 'a View.t;
  }

  let create context view =
    let i = Dom_html.(createTextarea document) in
    let i' = Js.Unsafe.coerce i in
    i'##autocorrect <- Js.string "off";
    i'##autocapitalize <- Js.string "off";
    i'##spellcheck <- Js.string "false";
    i##style##position <- Js.string "absolute";
    i##style##padding <- px 0;
    i##style##width <- px 1000;
    i##style##height <- em 1;
    i##style##outline <- Js.string "none";
    i'##tabindex <- Js.string "0";
    {input=i;context;prev=Js.string "";view}

  let reset i =
    i.input##value <- Js.string "";
    i.prev <- Js.string ""

  let empty i =
    i.input##value == Js.string "" &&
    i.prev == Js.string ""

  let listen_loop ~first input =
    let i = input.input in
    let context = input.context in
    let rec loop' first () =
      let s = i##value in
      let prev = input.prev in
      if s <> prev
      then begin
        let same = ref 0 in
        (try
           for i = 0 to min s##length prev##length - 1 do
             if s##charCodeAt(i) == prev##charCodeAt(i)
             then incr same
             else raise Not_found
           done
         with Not_found -> ());
        let remove_n =
          if !same < prev##length
          then prev##length - !same
          else 0 in
        let rest = s##substring_toEnd(!same) in
        input.prev <- s;
        begin try Zed_edit.move context (- remove_n) with _ ->
          let cursor = Zed_edit.cursor context in
          let pos = Zed_cursor.get_position cursor in
          Format.eprintf "pos:%d rem:%d@." pos remove_n;() end;
        Zed_edit.replace context remove_n (Zed_rope.of_string (Js.to_string rest));

        if first then reset input;
        Lwt_js.sleep 0.05 >>= loop' false
      end
      else
        begin
          Lwt_js.sleep 0.10 >>= loop' false
        end in
    loop' first ()


  exception Other_key
  let init input =
    let i = input.input in
    let context = input.context in
    i##onkeydown <- Dom_html.handler (fun e ->
        try begin
          match key_of_code e##keyCode with
          | `Backspace -> reset input; Zed_edit.delete_prev_char context
          | `Delete -> reset input; Zed_edit.delete_next_char context
          | `Enter -> reset input; Zed_edit.insert context (Zed_rope.of_string "\n")
          | `Left -> reset input; Zed_edit.prev_char context
          | `Right -> reset input; Zed_edit.next_char context
          | `Up -> reset input; Zed_edit.prev_line context
          | `Down -> reset input; Zed_edit.next_line context
          | _ -> raise Other_key
        end;
          Js._false
        with Other_key -> Js._true
      );
    View.set_input input.view i;
    Lwt.ignore_result (listen_loop ~first:true input);
end

let rope_iter_js f r =
  let s = Zed_rope.to_string r in
  let j = Js.string s in
  let j' = f j in
  let s' = Js.to_string j' in
  Zed_rope.of_string s'


type 'a data =
  {
    owner : int;
    data : 'a;
  } deriving (Json)

type action =
  | Update of (int * int * string)  deriving (Json)
  (* | Insert of (int * string) *)
  (* | Remove of (int * int) *)
  (* | Move of int *)
type msg = action data deriving (Json)

(* let str_size x = (Js.string x)##length *)
(* let merge a b = match a,b with *)
(*   | Insert (i,str),Insert (j,str2) when i + str_size str = j -> Some (Insert (i, str ^ str2 )) *)
(*   (\* | Remove (p,n),Remove(p',n') when p - n = p' -> Some (Remove (p, n + n' - 1)) *\) *)
(*   | _ -> None *)
let cache owner e =
  (* let e',send' = React.E.create () in *)
  (* let pred = ref None in *)
  (* let _ = React.E.map (fun x -> *)
  (*     match !pred with *)
  (*     | None -> pred:=Some x *)
  (*     | Some x' -> match merge x' x with *)
  (*       | None -> send' x'; pred := Some x *)
  (*       | Some m -> pred := Some m *)
  (*   ) e in *)
  let e'' = React.E.map (fun data -> {data;owner}) e in
  e''


let _ = Dom_html.window##onload <- Dom_html.handler (fun _ ->
    let editor : unit Zed_edit.t = Zed_edit.create
        ~lowercase:(rope_iter_js (fun s -> s##toLowerCase()))
        ~uppercase:(rope_iter_js (fun s -> s##toUpperCase())) () in
    let cursor = Zed_edit.new_cursor editor in
    let context = Zed_edit.context editor cursor in
    let content = Dom_html.getElementById "content" in
    let text = Js.Opt.case (content##textContent) (fun () -> "" ) Js.to_string in
    content##innerHTML <- Js.string "";
    let view = View.create context content in
    let input = Input.create context view in

    let copy_editor = Zed_edit.create
        ~lowercase:(rope_iter_js (fun s -> s##toLowerCase()))
        ~uppercase:(rope_iter_js (fun s -> s##toUpperCase())) () in
    let copy_cursor = Zed_edit.new_cursor copy_editor in
    let copy_context = Zed_edit.context copy_editor copy_cursor in
    let copy_content = Dom_html.getElementById "copy_content" in
    let copy_view = View.create copy_context copy_content in
    let copy_input = Input.create copy_context copy_view in

    let raw,send_raw = React.E.create () in

    (* apply a patch on context *)
    let patch ctx (a,b,c) =
      Lwt.async (fun _ ->
          Lwt_js.sleep 0.1 >>= fun () ->
          Zed_edit.patch ctx a b (Zed_rope.of_string c);
          Lwt.return_unit) in

    (* sync ctx *)
    let sync ctx owner =
      let ed = Zed_edit.edit ctx in
      (* let cursor = Zed_edit.cursor ctx in *)
      (* let cpos = Zed_cursor.position cursor in *)
      (* let _ = React.S.map (fun i -> *)
      (*     let data = Move i in *)
      (*     let msg = {owner;data} in *)
      (*     let str = Json_msg.to_string msg in *)
      (*     send_raw str *)
      (*   ) cpos in *)

      (* send changes, but previously apply patches *)
      let to_send = React.E.fmap (fun (pos,a,r,this_comes_from_a_patch) ->
          if not this_comes_from_a_patch then
            let text = Zed_edit.text ed in
            let added = Zed_rope.sub text pos a in
            let str = Zed_rope.to_string added in
            let data = match r,str with
              | 0,"" -> assert false
              (* | 0,_  -> Insert (pos,str) *)
              (* | len,"" -> Remove (pos,len) *)
              | _    -> Update (pos,r,str) in
            Some data
          else None
        ) (Zed_edit.changes ed) in
      let _ = React.E.map (fun msg ->
          let str = Json_msg.to_string msg in
          send_raw str
        ) (cache owner to_send) in

      (* listen for msg and patch context *)
      let _ = React.E.map (fun s ->
          let m = Json_msg.from_string s in
          if m.owner <> owner
          then match m.data with
            | Update updt -> patch ctx updt
            (* | Insert (pos,string) -> patch ctx (pos,0,string) *)
            (* | Remove (pos,len)    -> patch ctx (pos,len,"") *)
            (* | Move _ -> () *)
        ) raw in
      () in
    sync context 1;
    sync copy_context 2;
    Zed_edit.insert context (Zed_rope.of_string text);
    let _ = React.E.map (print_endline) raw in
    View.init copy_view;
    View.init view;

    Input.init input;
    Input.init copy_input;
    Js._true)
