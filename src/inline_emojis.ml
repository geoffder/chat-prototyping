open Core
open Bonsai_revery
open Bonsai_revery.Components

module type EMOJI_CONFIG = sig
  val emojis      : Attr.KindSpec.Image.source Map.M(String).t
  val box_style   : Style.t list
  val emoji_style : Style.t list
  val size        : int
  val big_scale   : float
end

let make_emoji_config
    ?(box_style = Style.[ flex_direction `Row; justify_content `FlexStart ])
    ?(emoji_style = [])
    ?(size = 30)
    ?(big_scale = 1.)
    emojis
  =
  ( module struct
    let emojis = emojis
    let box_style = box_style
    let emoji_style = emoji_style
    let size = size
    let big_scale = big_scale
  end : EMOJI_CONFIG )

module EmojiText (Config : EMOJI_CONFIG) = struct
  open Config

  let rgx = Str.regexp ":[a-z0-9_]+:"

  let only_emojis parts =
    List.for_all parts ~f:begin function
      | Str.Delim _ | Str.Text " " -> true
      | _ -> false
    end

  let small_style = Style.(width size :: height size :: emoji_style)

  let big_style =
    let scaled = Int.of_float (Float.of_int size *. big_scale) in
    Style.(width scaled :: height scaled :: emoji_style)

  let emoji_style big = if big then big_style else small_style

  let make text_attrs input_text =
    let parts = Str.full_split rgx input_text in
    let big = only_emojis parts in
    let code_to_component code =
      String.strip ~drop:(Char.equal ':') code
      |> Map.find emojis
      |> function
      | Some source ->
        image Attr.[ style (emoji_style big)
                   ; kind KindSpec.(ImageNode (Image.make ~source ()))
                   ]
      | None -> text text_attrs code in
    let f = function
      | Str.Text s  -> text text_attrs s
      | Str.Delim s -> code_to_component s in
    parts |> List.map ~f |> box Attr.[ style box_style ]
end
