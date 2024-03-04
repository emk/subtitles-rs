open Jest;
open Models;

describe("#Decode.video",
    ExpectJs.(() => {
        test("decodes video from JSON", () => {
            let json: Js.Json.t = [%bs.raw {|
                {
                    "url": "http://example.com/v.mp3",
                    "subtitles": [
                        {
                            "period": [1.0, 5.0],
                            "foreign": "Agua",
                            "native": "Water"
                        }
                    ]
                }
            |}];
            let expected = {
                url: "http://example.com/v.mp3",
                subtitles: [|
                    {
                        period: (1.0, 5.0),
                        foreign: "Agua",
                        native: Some("Water")
                    }
                |]
            };
            expect(Decode.video(json)) |> toEqual(expected)
        });
    })
);
