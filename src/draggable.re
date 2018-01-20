type position = {
  x: int,
  y: int
};

type mouse_button_status =
  | Up
  | Down;

type mouse = {
  button: mouse_button_status,
  pin_offset: position
};

let subtract_positions = (a, b) => {x: a.x - b.x, y: a.y - b.y};

let add_positions = (a, b) => {x: a.x + b.x, y: a.y + b.y};

let string_of_position = some_position =>
  "{x: "
  ++ string_of_int(some_position.x)
  ++ ", y: "
  ++ string_of_int(some_position.y)
  ++ "}";

let get_el = opt_el =>
  switch opt_el {
  | Some(el) => el
  | None => Webapi.Dom.Document.createElement("div", Webapi.Dom.document)
  };

type state = {
  translate: position,
  mouse,
  el_ref: ref(option(Dom.element))
};

type action =
  | Mouse_Down(position)
  | Mouse_Up;

let component = ReasonReact.reducerComponent("Draggable");

let make = _children => {
  let base_styles =
    ReactDOMRe.Style.make(
      ~backgroundColor="red",
      ~display="inline-block",
      ~padding="50px",
      ~position="absolute",
      ~cursor="move",
      ()
    );
  let set_ref = (the_ref, {ReasonReact.state}) =>
    state.el_ref := Js.Nullable.to_opt(the_ref);
  let drag_handler = (event, {ReasonReact.state}) =>
    switch state.mouse.button {
    | Up => ()
    | Down =>
      let new_mouse_position = {
        x: ReactEventRe.Mouse.screenX(event),
        y: ReactEventRe.Mouse.screenY(event)
      };
      let translate =
        subtract_positions(new_mouse_position, state.mouse.pin_offset);
      let x = string_of_int(translate.x) ++ "px";
      let y = string_of_int(translate.y) ++ "px";
      let transform = "translate(" ++ x ++ "," ++ y ++ ")";
      let el =
        state.el_ref^ |> get_el |> Webapi.Dom.Element.unsafeAsHtmlElement;
      let style = Webapi.Dom.HtmlElement.style(el);
      Webapi.Dom.CssStyleDeclaration.setProperty(
        "transform",
        transform,
        "",
        style
      );
      ReactEventRe.Mouse.stopPropagation(event);
      ReactEventRe.Mouse.preventDefault(event);
    };
  {
    ...component,
    initialState: () => {
      translate: {
        x: 0,
        y: 0
      },
      mouse: {
        button: Up,
        pin_offset: {
          x: 0,
          y: 0
        }
      },
      el_ref: ref(None)
    },
    reducer: (action, state) =>
      switch action {
      | Mouse_Down(inital_position) =>
        ReasonReact.SilentUpdate({
          ...state,
          mouse: {
            pin_offset: subtract_positions(inital_position, state.translate),
            button: Down
          }
        })
      | Mouse_Up =>
        let el = state.el_ref^ |> get_el;
        let rect = Webapi.Dom.Element.getBoundingClientRect(el);
        let x = Webapi.Dom.DomRect.x(rect);
        let y = Webapi.Dom.DomRect.y(rect);
        ReasonReact.SilentUpdate({
          ...state,
          translate: {
            x,
            y
          },
          mouse: {
            ...state.mouse,
            button: Up
          }
        });
      },
    render: ({state, send, handle}) => {
      print_endline("render");
      let x = string_of_int(state.translate.x) ++ "px";
      let y = string_of_int(state.translate.y) ++ "px";
      let transform = "translate(" ++ x ++ "," ++ y ++ ")";
      let greeting = "Howdy :)";
      <div
        style=(
          ReactDOMRe.Style.combine(
            ReactDOMRe.Style.make(~transform, ()),
            base_styles
          )
        )
        ref=(handle(set_ref))
        onMouseDown=(
          event =>
            send(
              {
                ReactEventRe.Mouse.stopPropagation(event);
                ReactEventRe.Mouse.preventDefault(event);
                Mouse_Down({
                  x: ReactEventRe.Mouse.screenX(event),
                  y: ReactEventRe.Mouse.screenY(event)
                });
              }
            )
        )
        onMouseUp=(
          event =>
            send(
              {
                ReactEventRe.Mouse.stopPropagation(event);
                ReactEventRe.Mouse.preventDefault(event);
                Mouse_Up;
              }
            )
        )
        onMouseMove=(
          /* TODO: See if possible to pass needed state values so handle is not required */
          handle(
            drag_handler
          )
        )>
        (ReasonReact.stringToElement(greeting))
      </div>;
    }
  };
};