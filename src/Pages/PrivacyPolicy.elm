module Pages.PrivacyPolicy exposing (view)

import Html exposing (div, h2, h3, p, ul, li, span, a, text)
import Html.Attributes exposing (href, style)


view =
    let
        bold =
            style [ ( "font-weight", "bold" ) ]

        wrapStyle =
            style [ ( "width", "80%" ), ( "margin", "auto" ) ]
    in
        [ div [ wrapStyle ]
            [ h2 []
                [ text "Privacy Policy" ]
            , p []
                [ text "This website uses Google Analytics to collect usage data and page view information. As such is the case, "
                , a [ href "https://www.google.com/analytics/terms/us.html" ]
                    [ text "Google's own terms apply" ]
                , text ".  "
                ]
            , h3 []
                [ text "What is collected?" ]
            , p []
                [ text "Standard Google Analytics page view data, which may include but not be limited to... "
                , ul []
                    [ li []
                        [ text "from which website you came;" ]
                    , li []
                        [ text "how long did you stay;" ]
                    , li []
                        [ text "which browser, OS, etc, you are using." ]
                    ]
                , text "We also register the total number of saved posts in your account (in a non identifiable way)."
                ]
            , h3 []
                [ text "What is ", span [ bold ] [ text "not" ], text " collected?" ]
            , p []
                [ text "This website does not collect any identifying information. Also, other than the total number of saved posts, no other information about your account/saved posts is collected."
                ]
            , h3 []
                [ text "Why is  this data collected?" ]
            , p []
                [ text "The collected data serves the purpose of understanding how the app is being used. "
                , text "On top of the Google Analytics data, the total number of saved posts is gathered to assess whether further optimizations are needed in the app."
                ]
            , h3 []
                [ text "Opting out" ]
            , p []
                [ text "You can always opt out of google analytics using "
                , a [ href "https://tools.google.com/dlpage/gaoptout" ]
                    [ text "Google's browser add-on" ]
                , text ". Turning Google Analytics off also prevents the total number of saved posts from being collected."
                ]
            ]
        ]
