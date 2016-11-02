[![Stories in Ready](https://badge.waffle.io/inaka/cowboy-trails.png?label=ready&title=Ready)](https://waffle.io/inaka/cowboy-trails)

<img src="https://lh5.googleusercontent.com/-Y1n1Vh4FjLE/TXDZiQ_zSVI/AAAAAAAAAJY/h47az_0MxO0/s1600/Western+backdrop+04.png" height="200" width="100%" />

# cowboy-trails
Cowboy routes on steroids!

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

## Why Cowboy Trails?
**Cowboy-Trails** enables you to:

* Add information to `cowboy` routes, which can be used later to interact with
  the server in a higher abstraction level.

* Define the server routes directly within the module that implements them.

## How to Use it?
The first use case for `cowboy_trails` is to compile `cowboy` routes.

Normally with `cowboy` you compile routes in the following way:

```erlang
Routes = [{'_',
           [ {"/resource1", resource1_handler, []}
           , {"/resource2/[:id]", resource2_handler, []}
           ]
          }
         ],
cowboy_router:compile(Routes),
```

Trails is also fully compatible with `cowboy` routes, so you can pass the same
routes in order to be processed by trails:

```erlang
trails:compile(Routes),
```

So far it seems like there is not any difference, right? But the most common case
with `cowboy` is that you usually work with a single host, even though you're
required to keep defining the host parameter within the routes (`[{'_', [...]}]`).

Well, with trails you have another useful function to compile single host routes:

```erlang
%% You only define the routes/paths
Routes = [ {"/resource1", resource1_handler, []}
         , {"/resource2/[:id]", resource2_handler, []}
         ],
trails:single_host_compile(Routes),
```

Now, let's suppose that you want to add additional information (metadata) to
cowboy routes related with the semantics of each HTTP method.

```erlang
Metadata = #{put => #{description => "PUT method"},
             post => #{ description => "POST method"},
             get => #{ description => "GET method"}},
Trail = trails:trail("/",
                     cowboy_static,
                     {private_file, "index2.html"},
                     Metadata,
                     []),
%% You can later retrieve the metadata:
Metadata = trails:metadata(Trail),
```

This can be used later to generate documentation related to each endpoint.

Normally, when you work with `cowboy` you have to define all routes in one place:

```erlang
Routes =
  [{'_',
    [ {"/", cowboy_static, {file, "www/index.html"}}
    , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
    , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
    , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
    , {"/api/status", spts_status_handler,  []}
    , {"/api/games", spts_games_handler, []}
    , {"/api/games/:game_id", spts_single_game_handler, []}
    , {"/api/games/:game_id/serpents", spts_serpents_handler, []}
    , { "/api/games/:game_id/serpents/:token"
      , spts_single_serpent_handler, []
      }
    , {"/api/games/:game_id/news", lasse_handler, [spts_news_handler]}
    ]
   }
  ],
Dispatch = cowboy_router:compile(Routes),
```

But now with `trails` you're able to define the routes on each resource handler.
The handler must implement the callback `trails/0` and return the specific
routes for that handler. For a better understanding, you can check out the
examples in the `test` folder ([trails_test_handler](./test/trails_test_handler.erl)).

Once you have implemented the `trails/0` callback on your handlers, you can do
something like this:

```erlang
Handlers =
  [ spts_status_handler
  , spts_games_handler
  , spts_single_game_handler
  , spts_serpents_handler
  , spts_single_serpent_handler
  , spts_news_handler
  ],
Trails =
  [ {"/", cowboy_static, {file, "www/index.html"}}
  , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
  , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
  , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
  | trails:trails(Handlers)
  ],
trails:single_host_compile(Trails),
```

This way each handler keeps their own routes, as it should be, and you can
merge them easily.

## Example

For more information about `cowboy_trails`, how to use it and the different
functions that it exposes, please check this [Example](./example).

## Testing

This project's test suites include [meta testing](http://inaka.net/blog/2015/11/13/erlang-meta-test-revisited/).
Therefore, in order to run the tests, it requires a proper plt.
Otherwise, when you try `rebar3 ct`, you'll get an error similar to:

```erlang
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ktn_meta_SUITE:dialyzer failed on line 60
Reason: {test_case_failed,No plts at ../../*.plt - you need to at least have one}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
```

To generate the required plt, just run `rebar3 dialyzer` once and then you can
run `rebar3 ct` as many times as you like.