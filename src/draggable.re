open Nice;

type position = {
  x: int,
  y: int
};

type action =
  | Mouse_Down(position)
  | Drag(position)
  | Mouse_Up;

type mouse_button_status =
  | Up
  | Down;

type mouse = {
  button: mouse_button_status,
  pin_offset: position,
  prev_position: position
};

type state = {
  position,
  mouse
};

let base_styles =
  ReactDOMRe.Style.make(
    ~backgroundColor="red",
    ~display="inline-block",
    ~padding="50px",
    ~position="absolute",
    ~cursor="move",
    ()
  );

let styles =
  css([
    BackgroundColor(Red), 
    Display(InlineBlock), 
    Padding(Px(50)), 
    Position(Absolute)
  ]);

let component = ReasonReact.reducerComponent("Draggable");

let subtract_positions = (a, b) => {x: a.x - b.x, y: a.y - b.y};

let add_positions = (a, b) => {x: a.x + b.x, y: a.y + b.y};

let string_of_position = some_position =>
  "{x: "
  ++ string_of_int(some_position.x)
  ++ ", y: "
  ++ string_of_int(some_position.y)
  ++ "}";

let get_pin_offset = (global_mouse, card_pos) =>
  subtract_positions(global_mouse, card_pos);

let make = _children => {
  ...component,
  initialState: () => {
    position: {
      x: 0,
      y: 0
    },
    mouse: {
      button: Up,
      pin_offset: {
        x: 0,
        y: 0
      },
      prev_position: {
        x: 0,
        y: 0
      }
    }
  },
  reducer: (action, state) =>
    switch action {
    | Mouse_Down(inital_position) =>
      ReasonReact.Update({
        ...state,
        mouse: {
          prev_position: inital_position,
          pin_offset: subtract_positions(inital_position, state.position),
          button: Down
        }
      })
    | Mouse_Up =>
      ReasonReact.Update({
        ...state,
        mouse: {
          ...state.mouse,
          button: Up
        }
      })
    | Drag(new_mouse_position) =>
      switch state.mouse.button {
      | Up => ReasonReact.NoUpdate
      | Down =>
        let position =
          subtract_positions(new_mouse_position, state.mouse.pin_offset);
        let prev_position = new_mouse_position;
        ReasonReact.Update({
          position,
          mouse: {
            ...state.mouse,
            prev_position
          }
        });
      }
    },
  render: ({state, send}) => {
    let left = string_of_int(state.position.x) ++ "px";
    let top = string_of_int(state.position.y) ++ "px";
    let greeting = "Howdy :)";
    <div
      style=(
        ReactDOMRe.Style.combine(
          ReactDOMRe.Style.make(~left, ~top, ()),
          base_styles
        )
      )
      onMouseDown=(
        event =>
          send(
            Mouse_Down({
              x: ReactEventRe.Mouse.screenX(event),
              y: ReactEventRe.Mouse.screenY(event)
            })
          )
      )
      onMouseUp=(_event => send(Mouse_Up))
      onMouseMove=(
        event =>
          send(
            Drag({
              x: ReactEventRe.Mouse.screenX(event),
              y: ReactEventRe.Mouse.screenY(event)
            })
          )
      )
      onMouseLeave=(
        event => {
          print_endline("leave");
          send(
            Drag({
              x: ReactEventRe.Mouse.screenX(event),
              y: ReactEventRe.Mouse.screenY(event)
            })
          );
        }
      )
      onMouseEnter=(
        event => {
          print_endline("enter");
          send(
            Drag({
              x: ReactEventRe.Mouse.screenX(event),
              y: ReactEventRe.Mouse.screenY(event)
            })
          );
        }
      )>
      (ReasonReact.stringToElement(greeting))
    </div>;
  }
};