defmodule AWS.CodeGen.ShapeTest do
  use ExUnit.Case

  @shapes %{
    "string" => %{"type" => "string"},
    "parent" => %{"type" => "structure", "members" => %{"child_property" => %{ "shape" => "child"}}},
    "child"  => %{"type" => "child_type", "x" => ["y", "z"]}
   }

  test 'expand shapes' do

    assert %{'type' => 'string'} = AWS.CodeGen.Shape.expand("string", @shapes)
  end

  test 'expand nested shapes' do

    assert %{
      'type' => 'structure',
      'members' => %{
        'child_property' => %{
          'type' => 'child_type', 'x' => ['x', 'z']}
      }
    } = AWS.CodeGen.Shape.expand("parent", @shapes)

  end

end
      