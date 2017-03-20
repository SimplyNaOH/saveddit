# Saveddit
###### View and filter your reddit saved posts

## UPDATE 2017/03/20

**There's a major refactor coming up in the
[refactor branch](https://github.com/SimplyNaOH/saveddit/tree/refactor). If you
are considering contributing to this project, you should start there!**

## Introduction

This is an elm web-app that, once you give it access to your reddit account,
let's you see your saved posts and filter them by subreddit. This is still a
work in progress, **you can try it out
[here](https://SimplyNaOH.github.io/saveddit/)**.

Unfortunately, I don't have the time to finish and polish this. However, I think
this is a useful tool for people like me that hoard saved posts. That is why
**I'm asking the community for help**.

## Contributing

At the moment the app is in a very rudimentary stage and there is lots of work
to be done. The tasks will be arranged in GitHub issues. Please feel free to
contact me through a PM in reddit (/u/SimplyNaOH) if you have any doubts about
the code. For discussion though, it would be better to go to a relevant issue or
open a new one.

Things to bear in mind:
* The code is in a very crude state, so I am open to pull requests that just
refactor or improve sections of code.

* Currently I'm using an ad hoc component,
[`elm-searchable-menu`](https://github.com/SimplyNaOH/elm-searchable-menu), that
I've published. However, the component's implementation is also very rudimentary
and I wouldn't mind moving away from it. On top, its integration into the app
was too done ad hoc and thus the code could use a refactor. For reference, the
change towards this component was done in
[115f6dd](https://github.com/SimplyNaOH/saveddit/commit/115f6dd72e4c813408e399100d5b91f8f9a78667).

* There will be also issues regarding future features of the app, but they are
not a priority.


## Building and running a local copy

This app uses the reddit API which requires a `client_id` assigned to an app,
and a `redirect_uri` that points to where its hosted. So, to run a (local) copy
hosted somewhere else you need to create your own reddit app and use it's `client_id`:

1. Go to [the apps tab in your preferences](https://www.reddit.com/prefs/apps/)
  in reddit.
2. Click on create an app.
3. Choose *installed app*.
4. Use an appropriate redirect uri. For local testing you can use
  `http://127.0.0.1:8000/index.html` (change the port number to match your
    server, see step 7.).
5. Modify `build.sh` to match your `client_id` (provided by reddit once you
  create the app) and `redirect_uri`.
6. Run `build.sh`
7. Serve your app. For instance you can run `python -m http.server 8000` with
  Python 3.x (for other options,
    [see here](https://gist.github.com/willurd/5720255)).

# LICENSE

Copyright (C) 2017 SimplyNaOH

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
