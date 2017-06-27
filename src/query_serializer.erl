-module(query_serializer).
-export([serialize/2]).

serialize(Data, Rules) ->

  List = serialize(<<"">>, Data, Rules),

  concat(lists:join(<<"&">>, lists:flatten(List))).


serialize(Name, Data, #{<<"type">> := <<"string">>}) ->

  concat([Name, <<"=">>, Data]);


serialize(Name, Data, Rules) when map_size(Rules) == 0 ->

  concat([Name, <<"=">>, Data]);


serialize(Name, Data, #{<<"members">> := Members, <<"type">> := <<"structure">>}) ->

  Fun = fun({MemberKey, MemberRule}) ->

    MemberValue = maps:get(MemberKey, Data, nil),

    case MemberValue of

      nil ->
        [];

      _Else ->

        MemberName = maps:get(<<"locationName">>, MemberRule, MemberKey),

        NewName = case Name of

          <<"">> -> MemberName;

          X -> concat([Name, <<".">>, MemberName])
        end,

        serialize(NewName, MemberValue, MemberRule)
    end
  end,

  lists:map(Fun, maps:to_list(Members));

serialize(Name, List, #{<<"type">> := <<"list">>, <<"member">> := MemberRule} = Rules) ->

  Fun = fun({Index, Value}) ->

    Suffix = concat([<<".">>, erlang:integer_to_binary(Index)]),

    ModifiedName = case Rules of

       #{<<"flattened">> := true, <<"member">> := #{<<"locationName">> := LocationName}} ->

         SplittedName = string:split(Name, <<".">>),

         WithoutLast = lists:droplast(SplittedName),

         WithLocation = lists:append(WithoutLast, [LocationName]),

         NameWithLocation = concat(lists:join(<<".">>, WithLocation)),

         concat([NameWithLocation, Suffix]);

       #{<<"flattened">> := true} ->

         concat([Name, Suffix]);

       _Else ->

         concat([Name, <<".">>, maps:get(<<"locationName">>, MemberRule, <<"member">>), Suffix])
    end,

    serialize(ModifiedName, Value, MemberRule)
  end,

  ListWithIndex = lists:zip(lists:seq(1, length(List)), List),

  G = lists:map(Fun, ListWithIndex),

  G;

serialize(Name, Map, #{<<"type">> := <<"map">>} = Rules) ->

  Fun = fun({Index, {MapKey, MapValue}}) ->

    Prefix = case maps:get(<<"flattened">>, Rules, false) of true -> <<".">>; _Else -> <<".entry.">> end,

    Position = concat([Prefix, erlang:integer_to_binary(Index),  <<".">>]),

    KeyName = concat([Position, maps:get(<<"locationName">>, maps:get(<<"key">>, Rules, #{}), <<"key">>)]),

    ValueName = concat([Position, maps:get(<<"locationName">>, maps:get(<<"value">>, Rules, #{}), <<"value">>)]),

    [

      serialize(concat([Name, KeyName]), MapKey, maps:get(<<"key">>, Rules)),

      serialize(concat([Name, ValueName]), MapValue, maps:get(<<"value">>, Rules))
    ]
  end,

  List = maps:to_list(Map),

  ListWithIndex = lists:zip(lists:seq(1, length(List)), List),

  lists:map(Fun, ListWithIndex),


concat(List) ->
  unicode:characters_to_binary(List, utf8).


