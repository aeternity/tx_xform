-module(tx_xform_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, generate).
-define(NAMESPACE, tx_xform).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ?NAMESPACE},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 tx_xform"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Generating aetx mapping module ...~n", []),
    ProjApps = rebar_state:project_apps(State),
    Opts = rebar_state:get(State, tx_xform_opts, []),
    {Outfile, InApp} =
        out_file(
          proplists:get_value(out_file, Opts, "aecore/src/txs/aetx_handlers.erl"),
          ProjApps),
    rebar_api:debug("Outfile = ~p~n", [Outfile]),
    ok = filelib:ensure_dir(Outfile),
    rebar_api:debug("Outfile directory ensured", []),
    Dirs = [filename:join(rebar_app_info:out_dir(App), "ebin") || App <- ProjApps],
    rebar_api:debug("Dirs = ~p", [Dirs]),
    generate(Dirs, Outfile),
    rebar_api:debug("Code generated", []),
    add_paths(State),
    rebar_erlc_compiler:compile(InApp),
    {ok, State}.

out_file(F, Apps) ->
    [App|Rest] = filename:split(F),
    AppBin = list_to_binary(App),
    case [A || A <- Apps,
               rebar_app_info:name(A) == AppBin] of
        [AppInfo] ->
            {filename:join([rebar_app_info:out_dir(AppInfo) | Rest]), AppInfo};
        [] ->
            {F, none}
    end.

add_paths(State) ->
    DepsPaths = rebar_state:code_paths(State, all_deps),
    code:add_pathsz(DepsPaths),
    rebar_api:debug("DepsPaths added", []).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

find(Dirs) ->
    lists:foldr(fun find_beams/2, [], Dirs).

generate(Dirs, OutF) ->
    Mod = list_to_atom(filename:basename(OutF, ".erl")),
    case find(Dirs) of
        [_|_] = Found ->
            Forms = [{attribute, 1, module, Mod},
                {attribute, 3, export, [{handler, 1}, {handler_by_type, 1}]}]
                ++ lists:flatten([Rdef || {_, _, {_, Rdef}, _} <- Found])
                ++ [{function, 50, handler, 1,
                    clauses(Found, 50)}]
                ++ [{function, 100, handler_by_type, 1,
                    type_clauses(Found, 100)}],
            case compile:forms(Forms, [report_errors, report_warnings]) of
                error ->
                    {error, Found};
                Ok ->
                    pp(Forms, OutF),
                    Ok
            end;
        [] ->
            rebar_api:info("No aetx behaviors found", [])
    end.

pp(Forms, F) ->
    {ok, Fd} = file:open(F, [write]),
    try
        lists:foreach(
          fun(Form) ->
                  io:put_chars(Fd, erl_pp:form(Form))
          end, Forms)
    after
        file:close(Fd)
    end.

clauses(Found, L0) ->
    {Cs, _} = lists:mapfoldl(
                fun({Mod, _, {RecName, _}, _}, L) ->
                        {{clause, L, [{record, L, RecName, []}],
                          [],
                          [{atom, L+1, Mod}]},
                         L+2}
                end, L0, Found),
    Cs.

type_clauses(Found, L0) ->
    {Cs, _} = lists:mapfoldl(
                fun({Mod, _, _, Str}, L) ->
                  case Str of
                    {error, Err} ->
                        rebar_api:error("Error in ~p: ~p", [Mod, Err]),
                        {{error, Err}, L + 1};
                    _ ->
                        {{clause, L, write_binary(Str, L),
                          [],
                          [{atom, L+1, Mod}]},
                          L+2}
                  end
                end, L0, Found),
    Cs.


find_beams(D, Acc) ->
    filelib:fold_files(
      D, ".*\\.beam$", false,
      fun(F, A) ->
              case beam_lib:chunks(F, [attributes, abstract_code]) of
                  {ok, {Mod, [{attributes, As}, Abst]}} ->
                      case lists:member({behavior, [aetx]}, As)
                          orelse lists:member({behaviour, [aetx]}, As) of
                          true  -> [{Mod, F, rec_type(Abst),
                                     transaction_type(Abst)} | A];
                          false -> A
                      end;
                  _ ->
                      A
              end
      end, Acc).

rec_type({abstract_code, {raw_abstract_v1, As}}) ->
    Rec =
        case [T || {attribute,_,spec,{{new,1},T}} <- As] of
            [[{type,_,'fun',
               [_In, {type,_,union,
                   [{type,_,tuple,[{atom,_,ok},RecType]},_ErrorClause]}]}]] ->
                case RecType of
                    {user_type,_,UType,[]} ->
                        recname_from_user_type(UType, As);
                    {type,_,record,[{atom,_,RecName}]} ->
                        RecName;
                    Other ->
                        {error, [?LINE, Other]}
                end;
            [[{type,_,'fun',
               [_In, {type,_,tuple,[{atom,_,ok},RecType]}]}]] ->
                case RecType of
                    {user_type,_,UType,[]} ->
                        recname_from_user_type(UType, As);
                    {type,_,record,[{atom,_,RecName}]} ->
                        RecName;
                    Other ->
                        {error, [?LINE, Other]}
                end;
            OtherT ->
                {error, [?LINE, OtherT]}
        end,
    rec_def(Rec, As).

recname_from_user_type(UType, As) ->
    case [Val || {attribute,_,type,{Ty,Val,_}} <- As,
               Ty =:= UType] of
        [] ->
            {error, [?LINE, {unknown_type, UType}]};
        [{type,_,record,[{atom,_,R}]}] ->
            R;
        Other ->
            {error, [?LINE, Other]}
    end.

rec_def({error, _} = Err, _) ->
    Err;
rec_def(RecName, As) when is_atom(RecName) ->
    case [R || {attribute,_,record,{Rn,_}} = R <- As,
               Rn =:= RecName] of
        [] ->
            {error, [?LINE, {cannot_find_record_def, RecName}]};
        [Rdef] ->
            {RecName, prepend_user_types(Rdef, As)}
    end.


prepend_user_types(Def, As) ->
    prepend_user_types(tuple_to_list(Def), [Def], As).

prepend_user_types([{user_type,_,Ty,_}|T], Acc, As) ->
    Def = user_type(Ty, As),
    prepend_user_types(
      T, prepend_user_types(tuple_to_list(Def), [Def|Acc], As), As);
prepend_user_types([Tup|T], Acc, As) when is_tuple(Tup) ->
    prepend_user_types(
      T, prepend_user_types(tuple_to_list(Tup), Acc, As), As);
prepend_user_types([H|T], Acc, As) when is_list(H) ->
    prepend_user_types(T, prepend_user_types(H, Acc, As), As);
prepend_user_types([_|T], Acc, As) ->
    prepend_user_types(T, Acc, As);
prepend_user_types([], Acc, _) ->
    Acc.

user_type(Type, As) ->
    case [T || {attribute,_,type,{Ty,_,_}} = T <- As,
               Ty =:= Type] of
        [] ->
            {error, {unknown_user_type, Type}};
        [UserType] ->
            UserType
    end.

transaction_type({abstract_code, {raw_abstract_v1, As}}) ->
    case [T || {function,_,type,0,T} <- As] of 
        [[{clause, _, _, _, R}]] ->
            read_binary(R);
        OtherT ->
            {error, [?LINE, OtherT]}
    end.

read_binary([{bin,_, [{bin_element,_, {string,_,Str},
                                                default,default}]}]) ->
    Str;
read_binary(_) ->
  {error, [?LINE, type_not_binary]}.

write_binary(Str, L) ->
    [{bin,L, [{bin_element,L, {string,L,Str}, default,default}]}].

