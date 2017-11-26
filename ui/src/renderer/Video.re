let component = ReasonReact.statelessComponent("Hello");

let make = (~src, _children) => {
    ...component,
    render: (_self) =>
        <video controls=Js.true_ width="100%">
            <source src=src />
        </video>
};
