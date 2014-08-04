type code =
  | Char of int
  | Enter
  | Escape
  | Tab
  | Up
  | Down
  | Left
  | Right
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Next_page
  | Prev_page
  | Home
  | End
  | Insert
  | Delete
  | Backspace
  | PageUp
  | PageDown
  | PrintScrn
  | Esc
type t = {
  control : bool;
  meta : bool;
  shift : bool;
  code : code;
}

let compare = compare

let of_code = function
  | 3 -> Enter
  | 8 -> Backspace
  | 9 -> Tab
  | 13 -> Enter
  (* | 16 -> Shift *)
  (* | 17 -> Ctrl *)
  (* | 18 -> Alt *)
  (* | 19 -> Pause *)
  (* | 20 -> CapsLock *)
  | 27 -> Esc
  | 33 -> PageUp
  | 34 -> PageDown
  | 35 -> End
  | 36 -> Home
  | 37 -> Left
  | 38 -> Up
  | 39 -> Right
  | 40 -> Down
  | 44 -> PrintScrn
  | 45 -> Insert
  | 46 -> Delete
  | 127 -> Delete
  | 63232 -> Up
  | 63233 -> Down
  | 63234 -> Left
  | 63235 -> Right
  | 63272 -> Delete
  | 63273 -> Home
  | 63275 -> End
  | 63276 -> PageUp
  | 63277 -> PageDown
  | 63302 -> Insert
  | n -> Char n
