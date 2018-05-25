defmodule AWS.CodeGen.QueryHelpers do
  alias AWS.CodeGen.QueryService.{Action, Service}

  def param_list(%Action{schema: %{"input" => %{"shape" => %{"members" => members}}}}) do
    case members do
      %{"StackName" => _} = opts when map_size(opts) > 1 -> ~s(stack_name, options \\\\ [])
      %{"StackName" => _} -> ~s(stack_name)
      _ -> ~s(options \\\\ [])
    end
  end

  def param_list(%Action{schema: %{"input" => %{"shape" => _}}}), do: ""

  def options_input(%Action{schema: %{"input" => %{"shape" => %{"members" => members}}}}) do
    case members do
      %{"StackName" => _} = opts when map_size(opts) > 1 ->
        ~s([{"StackName", stack_name} | options])

      %{"StackName" => _} ->
        ~s([{"StackName", stack_name}])

      _ ->
        "options"
    end
  end

  def options_input(%Action{schema: %{"input" => %{"shape" => _}}}), do: ""

  def module_name(%Service{module_name: name}) do
    name |> String.replace_leading("AWS.", "ExAws.")
  end
end
