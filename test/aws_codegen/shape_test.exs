defmodule AWS.CodeGen.ShapeTest do
  use ExUnit.Case

  @shapes %{
    "string" => %{"type" => "string"},
    "parent" => %{"type" => "structure", "members" => %{"child_property" => %{"shape" => "child"}}},
    "child"  => %{"type" => "child_type", "x" => ["y", "z"]},
    "error"  => %{"type" => "structure", "members" => %{"message" => %{"shape" => "string"}}}
   }

  test "expand shapes" do

    assert %{"type" => "string"} = AWS.CodeGen.Shape.expand("string", @shapes)
  end

  test "expand nested shapes" do

    assert %{"members" => %{
      "child_property" => %{
        "type" => "child_type",
        "x" => ["y", "z"]
      }
    }, "type" => "structure"} = AWS.CodeGen.Shape.expand("parent", @shapes)

  end

  test "expand errors" do

    assert [{
      :error,
      %{"members" => %{"message" => %{"type" => "string"}}}
    }] = AWS.CodeGen.Shape.expand_errors([%{
      "shape" => "error"
    }], @shapes)

  end

end
      