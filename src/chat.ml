open Core
open Bonsai_revery
open Bonsai_revery.Components
open Bonsai.Infix
open Inline_emojis

module Comment = struct
  type t =
    { author : string
    ; content : string
    ; timestamp : string
    }
  [@@deriving sexp, equal, fields]
end

module Input = struct
  type t = string * (string -> Event.t) * Element.t
end

module Model = struct
  type t =
    { user : string
    ; comments : Comment.t Map.M(Int).t
    }
  [@@deriving sexp, equal]

  let default = { user = "Steve"; comments = Map.empty (module Int) }
end

module Action = struct
  type t =
    | Post of Comment.t
    | Remove of int
  [@@deriving sexp_of]
end

module Result = Element

module Theme = struct
  let font_size = 25.
  let rem factor = font_size *. factor
  let remi factor = rem factor |> Float.to_int
  let app_background = Color.hex "#f4edfe"
  let text_color = Color.hex "#513B70"
  let dimmed_text_color = Color.hex "#DAC5F7"
  let title_text_color = Color.hex "#EADDFC"
  let panel_background = Color.hex "#F9F5FF"
  let panel_border_color = Color.hex "#EADDFC"
  let panel_border = Style.border ~width:1 ~color:panel_border_color
  let button_color = Color.hex "#9573C4"
  let hovered_button_color = Color.hex "#C9AEF0"
  let danger_color = Color.hex "#f7c5c6"
  let font_info = Attr.KindSpec.(TextNode (Text.make ~size:font_size ()))
  let bonsai_path = "bonsai.png"
end

module Styles = struct
  let app_container =
    Style.
      [ position `Absolute
      ; top 0
      ; bottom 0
      ; left 0
      ; right 0
      ; align_items `Stretch
      ; justify_content `FlexStart
      ; flex_direction `Column
      ; background_color Theme.app_background
      ; padding_vertical 2
      ; padding_horizontal 6
      ; overflow `Hidden
      ]

  let bonsai =
    Style.
      [ align_self `FlexStart
      ; margin_top (Theme.remi 1.)
      ; margin_bottom (Theme.remi 1.)
      ; margin_left 50
      ; margin_right 100
      ; width 150
      ; height 150
      ]

  let title =
    Style.
      [ color Theme.title_text_color
      ; align_self `Center
      ; margin_top (Theme.remi 1.)
      ; text_wrap NoWrap
      ; flex_grow 1
      ]

  let title_font =
    Attr.KindSpec.update_text ~f:(fun a -> { a with size = Theme.rem 4. }) Theme.font_info
end

module Components = struct
  module Author = struct
    module Styles = struct
      let box = Style.[ margin 6; flex_direction `Row; align_self `FlexStart ]
      let author_text = Style.[ color Theme.text_color ]

      let author_font =
        Attr.KindSpec.update_text
          ~f:(fun a -> { a with size = Theme.rem 1.25 })
          Theme.font_info

      let timestamp_text =
        Style.[ align_self `Center; color Theme.text_color; margin_left 20 ]

      let timestamp_font =
        Attr.KindSpec.update_text
          ~f:(fun a -> { a with size = Theme.rem 0.5 })
          Theme.font_info

      let remove_button ~hovered =
        Style.
          [ color
              ( match hovered with
              | true  -> Theme.danger_color
              | false -> Colors.black )
          ; transform [ TranslateY 2. ]
          ; margin_left 10
          ]

      let remove_button_font =
        Attr.KindSpec.update_text
          ~f:(fun a ->
            { a with family = Revery.Font.Family.fromFile "FontAwesome5FreeSolid.otf" })
          Theme.font_info
    end

    let remove_button key inject =
      button
        (fun ~hovered ->
          [ Attr.style (Styles.remove_button ~hovered)
          ; Attr.on_click (Event.Many [ inject (Action.Remove key) ])
          ; Attr.kind Styles.remove_button_font
          ])
        {||}

    let view ~key ~inject ~author ~timestamp =
      box
        Attr.[ style Styles.box ]
        [ text Attr.[ style Styles.author_text; kind Styles.author_font ] author
        ; text Attr.[ style Styles.timestamp_text; kind Styles.timestamp_font ] timestamp
        ; remove_button key inject
        ]
  end

  module Content = struct
    module Styles = struct
      let text =
        Style.[ color Theme.text_color; text_wrap WrapIgnoreWhitespace; flex_wrap `Wrap ]

      let font = Theme.font_info
    end

    let emojis =
      [ "bonsai", "bonsai.png"
      ; "pogchamp", "pogchamp.png"
      ; "admonish", "admonish.png"
      ; "dunno_left", "dunno_left.png"
      ]
      |> Map.of_alist_exn (module String)
      |> Map.map ~f:(fun p -> Attr.KindSpec.Image.File p)

    (* NOTE: This, with the text style accomplishes a decent feel of text wrapping
     * with emojis WITHOUT newlines, but when they are introduced, the text box of course
     * just expands, with the next emoji simply coming in at the next spot in the growing
     * row. What is the best way to achieve the hybrid? Must there be inner boxes with Row
     * flex and wrap, within a column flex box? (with strings broken up by newlines to
     * be placed in separate boxes accordingly?)
     *
     * From looking at mardown.re of Revery, this seems to be the case. The container is
     * columnar, and the markdown is parsed into blocks e.g. paragraphs, code, lists, etc,
     * then they are mapped into elements to be the children of the columnar block.
     * Therefore, linebreaks become an empty ( " " ) text element (taking a row). So far,
     * this is simple to translate wrt text, but wanting emojis inline with the text means
     * that I will need to have row flexed inline emoji boxes as a "paragraph" level element
     * existing within the outer column flex. Newlines will break the input string into
     * separate emoji boxes. *)
    let box_style =
      Style.
        [ flex_direction `Row
        ; flex_wrap `Wrap
        ; justify_content `FlexStart
        ; align_items `FlexStart
        ]

    module EmojiBox = EmojiText ((val make_emoji_config ~big_scale:2. ~box_style emojis))

    let view ~content =
      box
        Attr.[ style Style.[ margin_left 6 ] ]
        [ EmojiBox.make Attr.[ style Styles.text; kind Styles.font ] content ]
  end

  module Comment = struct
    module Styles = struct
      let box =
        Style.
          [ flex_direction `Column
          ; margin 2
          ; padding_vertical 4
          ; align_items `FlexStart
          ; background_color Theme.panel_background
          ; Theme.panel_border
          ]
    end

    let component =
      Bonsai.pure
        ~f:(fun ((key : int), (comment : Comment.t), (inject : Action.t -> Event.t)) ->
          let { Comment.author; content; timestamp } = comment in
          box
            Attr.[ style Styles.box ]
            [ Author.view ~key ~inject ~author ~timestamp; Content.view ~content ])
  end

  module AddComment = struct
    module Styles = struct
      let container =
        Style.
          [ flex_direction `Row
          ; background_color Theme.panel_background
          ; Theme.panel_border
          ; margin 2
          ; align_items `Center
          ; overflow `Hidden
          ; flex_grow 1
          ]

      let input = Style.[ border ~width:0 ~color:Colors.transparent_white; width 4000 ]
    end

    let view children = box Attr.[ style Styles.container ] (box [] [] :: children)
  end
end

let comment_list =
  let%map.Bonsai comments =
    Tuple2.map_fst ~f:(fun (model : Model.t) -> model.comments)
    @>> Bonsai.Map.associ_input_with_extra (module Int) Components.Comment.component
  in
  (* TODO: calculate max height based on height of container? *)
  box
    Attr.
      [ style
          Style.
            [ flex_direction `ColumnReverse
            ; flex_grow 100
            ; max_height 765
            ; overflow `Hidden
            ]
      ]
    (Map.data comments |> List.rev)

let text_input =
  Bonsai.pure ~f:(fun ((model : Model.t), inject) ->
      Text_input.props
        ~placeholder:""
        ~autofocus:true
        ~on_key_down:(fun event value set_value ->
          match event.key with
          | Return when not (String.is_empty value) ->
            let timestamp =
              Time.now () |> Time.to_string_trimmed ~zone:(Lazy.force Timezone.local)
            in
            let comment = { Comment.author = model.user; content = value; timestamp } in
            Event.Many [ inject (Action.Post comment); set_value "" ]
          | _ -> Event.no_op)
        Attr.[ kind Theme.font_info ])
  >>> Text_input.component

let add_comment =
  let%map.Bonsai model, inject = Bonsai.pure ~f:Fn.id
  and _value, _set_value, text_input = text_input in
  Components.AddComment.view [ text_input ]

let state_component =
  Bonsai.state_machine
    (module Model)
    (module Action)
    [%here]
    ~default_model:Model.default
    ~apply_action:
      begin
        fun ~inject:_ ~schedule_event:_ () model -> function
        | Post comment ->
          let key =
            match Map.max_elt model.comments with
            | Some (key, _) -> key + 1
            | None          -> 0
          in
          let comments = Map.add_exn model.comments ~key ~data:comment in
          { model with comments }
        | Remove key   -> { model with comments = Map.remove model.comments key }
      end

let app : (unit, Element.t) Bonsai_revery.Bonsai.t =
  state_component
  >>> let%map.Bonsai comment_list = comment_list
      and add_comment = add_comment in
      let title = text Attr.[ style Styles.title; kind Styles.title_font ] "Chatter" in
      let bonsai =
        image
          Attr.
            [ style Styles.bonsai
            ; kind
                KindSpec.(
                  ImageNode (Image.make ~source:(Image.File Theme.bonsai_path) ()))
            ]
      in
      let header =
        box
          Attr.[ style Style.[ justify_content `FlexStart; flex_direction `Row ] ]
          [ bonsai; title ]
      in
      box Attr.[ style Styles.app_container ] [ header; comment_list; add_comment ]
