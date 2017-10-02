-module(tx_xform).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = tx_xform_prv:init(State),
    {ok, State1}.
