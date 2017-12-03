type period = (float, float);

let periodContains = (time: float, (startTime, endTime): period) =>
    startTime <= time && time < endTime;

type subtitle = {
    period: period,
    foreign: string,
    native: option(string),
};

type video = {
    url: string,
    subtitles: array(subtitle),
};

module Decode = {
    let period = (json) =>
        Json.Decode.(json |> pair(Json.Decode.float, Json.Decode.float));

    let subtitle = (json) => Json.Decode.{
        period: json |> field("period", period),
        foreign: json |> field("foreign", string),
        native: json |> optional(field("native", string)),
    };

    let video = (json) => Json.Decode.{
        url: json |> field("url", string),
        subtitles: json |> field("subtitles", array(subtitle)),
    };
}
