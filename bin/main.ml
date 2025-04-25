open Js_of_ocaml_toplevel

let () = print_endline "Hello, World!"
let stdouts = ref Jstr.empty (* for capturing toplevel outputs. *)
let stdouts_reset () = stdouts := Jstr.empty

let stdouts_append ~js_string:d =
  stdouts := Jstr.append !stdouts (Obj.magic d : Jstr.t)

let initialize () =
  Jsoo_runtime.Sys.set_channel_output' stdout stdouts_append;
  Jsoo_runtime.Sys.set_channel_output' stderr stdouts_append;
  JsooTop.initialize ()

let execute source =
  Brr.Console.(log [ "executing"; source ]);
  stdouts_reset ();
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  let () = JsooTop.execute true fmt (source ^ ";;") in
  Buffer.contents buf

let do_ code exec_previous =
  let precode ?(preclass = "") ~class_ () =
    let code = Brr.El.code ~at:[ Brr.At.class' (Jstr.v class_) ] [] in
    let pre = Brr.El.pre ~at:[ Brr.At.class' (Jstr.v preclass) ] [ code ] in
    (pre, code)
  in
  let preoutput, output =
    precode ~preclass:"outputpre" ~class_:"output nohighlight" ()
  in
  let pretypes, types =
    precode ~preclass:"typespre" ~class_:"types language-ocaml" ()
  in
  let execute_button =
    Brr.El.input
      ~at:[ Brr.At.type' (Jstr.v "button"); Brr.At.value (Jstr.v "Execute") ]
      ()
  in
  let back_button =
    Brr.El.input
      ~at:
        [
          Brr.At.type' (Jstr.v "button");
          Brr.At.value (Jstr.v "Back to original content");
        ]
      ()
  in
  let () =
    Brr.El.insert_siblings `After code
      [
        Brr.El.div
          ~at:[ Brr.At.class' (Jstr.v "button-container") ]
          [ execute_button; back_button ];
        pretypes;
        preoutput;
      ]
  in
  let initial_content = Jv.get (Brr.El.to_jv code) "innerText" in
  let _unlistener =
    Brr.Ev.listen Brr.Ev.click
      (fun _ev ->
    Brr.El.set_class
      (Jstr.v "start-animation")
      (false)
      code;
    let _ = Brr.G.set_timeout ~ms:0 (fun _ ->     Brr.El.set_class
      (Jstr.v "start-animation")
      (true)
      code)
    in
        let code = Brr.El.to_jv code in
        let textarea = Jv.get code "textareaElement" in
        Jv.set textarea "value" initial_content;
        ignore @@ Jv.call code "scheduleHighlight" [||])
      (Brr.El.as_target back_button)
  in
  let exec () =
    exec_previous ();
    let content = Jv.get (Brr.El.to_jv code) "innerText" |> Jv.to_string in
    let values = execute content in
    Jv.set (Brr.El.to_jv types) "textContent"
      (Jv.of_string (String.trim values));
    Brr.El.set_class
      (Jstr.v "no-types-defined")
      (String.length values = 0)
      pretypes;
    Brr.El.set_class
      (Jstr.v "start-animation")
      (false)
      pretypes;
    let _ = Brr.G.set_timeout ~ms:0 (fun _ ->     Brr.El.set_class
      (Jstr.v "start-animation")
      (true)
      pretypes)
    in
    Brr.El.set_class
      (Jstr.v "no-types-defined")
      (String.length (Jstr.to_string !stdouts) = 0)
      preoutput;
    Brr.El.set_class
      (Jstr.v "start-animation")
      (false)
      preoutput;
    let _ = Brr.G.set_timeout ~ms:0 (fun _ ->     Brr.El.set_class
      (Jstr.v "start-animation")
      (true)
      preoutput)
    in
    Brr.El.set_at (Jstr.v "data-highlighted") None types;
    ignore
    @@ Jv.call (Jv.get Jv.global "hljs") "highlightElement"
         [| Brr.El.to_jv types |];
    Jv.set (Brr.El.to_jv output) "innerText"
      (!stdouts |> Jstr.to_string |> String.trim |> Jv.of_string)
  in
  let _unlistener =
    Brr.Ev.listen Brr.Ev.click
      (fun _ev ->
        initialize ();
        exec ())
      (Brr.El.as_target execute_button)
  in
  exec

let exec_all =
  Brr.El.fold_find_by_selector do_ (Jstr.v "code-input") (fun () -> ())

let () =
  initialize ();
  exec_all ()
