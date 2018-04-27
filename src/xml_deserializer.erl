-module(xml_deserializer).
-export([deserialize/3]).
-export([deserialize_error/1]).

-include_lib("xmerl/include/xmerl.hrl").
-define(SCAN_OPTIONS, [{space,normalize}, {acc_fun, fun normalize_spaces/3}]).


parse_scalar(nil, _) ->
  nil;

parse_scalar(Text, <<"boolean">>) ->
  case Text of "true" -> true; "false" -> false end;

parse_scalar(Text, <<"string">>) ->
  case Text of Text when is_binary(Text) -> Text; Text -> list_to_binary(Text) end;

parse_scalar(Text, <<"float">>) ->
  {Float, _} = string:to_float(Text),
  Float;

parse_scalar(Text, <<"integer">>) ->
  {Integer, _} = string:to_integer(Text),
  Integer
.

parse(ByName, {RuleName, #{<<"type">> := Type} = Rule})

  when Type == <<"string">>
  orelse Type == <<"boolean">>
  orelse Type == <<"float">>
  orelse Type == <<"integer">> ->

  KeyName = get_location_name(Rule, RuleName),

  DecodedText = case maps:get(KeyName, ByName, "") of

    [#xmlElement{attributes = Attributes, content = [#xmlText{value = Text}]}] ->
      case is_base64(Attributes) of true -> binary_to_list(base64:decode(Text)); false -> Text end;

    _Else ->
      nil
  end,

  {parse_scalar(KeyName, <<"string">>), parse_scalar(DecodedText, Type)};



parse(ByName, {RuleName, #{<<"type">> := <<"map">>, <<"key">> := KeyRule, <<"value">> := ValueRule} = Rule})  ->


  [#xmlElement{content = Content}] = maps:get(RuleName, ByName),

  ParsedValue = case Content of

    Content when 0 == length(Content) -> #{};

    Content ->

      KeyName = get_location_name(KeyRule, <<"key">>),

      ValueName = get_location_name(ValueRule, <<"value">>),

      case maps:get(<<"flattened">>, Rule, false) of

        false -> lists:foldl(fun (#xmlElement{name=entry, content = [
            #xmlElement{content = [#xmlText{}]} = Key,
            #xmlElement{content = [#xmlText{}]} = Value
          ]}, Acc) ->

            {_, K} = parse(#{KeyName => [Key]}, {<<"key">>, KeyRule}),
            {_, V} = parse(#{ValueName => [Value]}, {<<"value">>, ValueRule}),

            Acc#{K => V}

          end, #{}, Content);

        true -> lists:foldl(fun ([

            #xmlElement{content = [#xmlText{}]} = Key,
            #xmlElement{content = [#xmlText{}]} = Value
          ], Acc) ->

            {_, K} = parse(#{KeyName => [Key]}, {<<"key">>, KeyRule}),
            {_, V} = parse(#{ValueName => [Value]}, {<<"value">>, ValueRule}),

            Acc#{K => V}
          end, #{}, part(Content))

      end
  end,

  {RuleName, ParsedValue};


parse(ByName, {RuleName, #{<<"type">> := <<"list">>, <<"member">> := MemberRule} = Rule}) ->

  Flattened = maps:get(<<"flattened">>, Rule, false),

  MemberName = get_location_name(MemberRule, case Flattened of false -> <<"member">>; true -> RuleName end),

  Children = case Flattened of

    false ->
      [#xmlElement{content = Content}] = maps:get(RuleName, ByName),

      Content;

    true ->
      maps:get(MemberName, ByName)

  end,

  Values = lists:foldl(fun (#xmlElement{} = Element, Acc) ->

    ChildByName = by_name([Element]),

    {_, V} = parse(ChildByName, {MemberName, MemberRule}),

    Acc ++ [V]

  end, [], Children),

  {RuleName, Values};


parse(ByName, {Name, #{<<"type">> := <<"structure">>, <<"members">> := Members}}) ->

  [#xmlElement{content = Children}] = maps:get(Name, ByName),

  ChildrenByName = by_name(Children),

  Values = fold_map_members(ChildrenByName, Members),

  {Name, Values}.


deserialize(Xml, ResultWrapper,  #{<<"type">> := <<"structure">>, <<"members">> := Members}) ->

  {Root, _} = xmerl_scan:string(Xml, ?SCAN_OPTIONS),

  [#xmlElement{content = Content}] = xmerl_xpath:string(ResultWrapper, Root),

  ByName = by_name(Content),

  fold_map_members(ByName, Members).


deserialize_error(Xml) ->

  {#xmlElement{} = Root, _} = xmerl_scan:string(Xml, ?SCAN_OPTIONS),

  {
    list_to_atom(extract_text_from_path("//*/Code/text()", Root)),
    list_to_binary(extract_text_from_path("//*/Message/text()", Root)),
    list_to_binary(extract_text_from_path("//*/RequestId/text()", Root))
  }.

fold_map_members(ElementsByName, Members) ->

  maps:fold(fun (RuleName, RuleValue, Acc) ->

    {K, V} = parse(ElementsByName, {RuleName, RuleValue}),

    maps:put(K, V, Acc)

  end, #{}, Members).


extract_text_from_path(Path, Element) ->

  [#xmlText{value = Value}] = xmerl_xpath:string(Path, Element),

  Value.


get_location_name(Rule, Default) ->
  maps:get(<<"locationName">>, Rule, Default).

normalize_spaces(#xmlText{value = " ", pos = P}, Acc, S) ->
  {Acc, P, S};  % new return format

normalize_spaces(X, Acc, S) ->
  {[X|Acc], S}.


by_name(Content) ->

  lists:foldl(fun (Child, Acc) ->

    case Child of
      #xmlElement{name = Name} = Element ->

        Value = maps:get(atom_to_binary(Name, utf8), Acc, []),

        maps:put(atom_to_binary(Name, utf8), Value ++ [Element], Acc);

      _Else -> Acc
    end

  end, #{}, Content).


is_base64(Attributes) ->

  Predicate = fun(#xmlAttribute{name = Name, value = Value}) ->
    Name == encoding andalso Value == "base64"
  end,

  lists:any(Predicate, Attributes).


part(List) ->

  part(List, []).

part([], Acc) ->

  lists:reverse(Acc);

part([H], Acc) ->

  lists:reverse([[H]|Acc]);

part([H1,H2|T], Acc) ->

  part(T, [[H1,H2]|Acc]).