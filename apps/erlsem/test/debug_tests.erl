-module(debug_tests).


t1_test() ->
  Symtab = #{
    t9 =>
        {ty_scheme,[],
            {union,
                [
                  {fun_full,[{named,0,{ty_ref,'.',t9,0},[]}],{predef,any}},
                  {fun_full,[{predef,none}],{predef,any}}
                ]}}
  },

  
  global_state:with_new_state(fun() ->
    ty_parser:set_symtab(Symtab),
    %System = maps:keys(Symtab),
     
    Ty = {named, noloc, {ty_ref, '.', t9, 0}, []},
    Node = ty_parser:parse(Ty),

    io:format(user, "Check emptiness of ~p~n", [Node]),

    % emptiness
    ty_node:is_empty(Node),
    ty_node:is_empty(Node)

    % lists:foreach(fun(Name) -> 
    %   io:format(user, "Parsing ~p~n", [Name]),

    %   % parse
    %   Ty = {named, noloc, {ty_ref, '.', Name, 0}, []},
    %   Node = ty_parser:parse(Ty),

    %   io:format(user, "Check emptiness of ~p~n", [Node]),

    %   % emptiness
    %   ty_node:is_empty(Node),
    %   % cache
    %   ty_node:is_empty(Node)

    % end, System),
  end),
  ok.