defmodule AWS.CodeGen.Shape do

  def expand(nil, _), do: nil

  def expand(shape = %{}, all) do

    Enum.into(shape, %{}, fn({key, value}) ->

      case value do
         %{"shape" => shape} ->
          {key, expand(all[shape], all)}
        map when is_map(map) ->
          {key, expand(map, all)}
        other ->
          {key, other}
      end
    end)

  end

  def expand(value, all) do
    expand(all[value], all)
  end

end