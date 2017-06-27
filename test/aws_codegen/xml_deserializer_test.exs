defmodule AWS.CodeGen.XmlDeserializerTest do
  use ExUnit.Case

  def deserialize(xml, rules) do

   :xml_deserializer.deserialize('<xml><Result>#{xml}</Result><SomethingElse></SomethingElse></xml>', 'Result', rules)

  end

  describe "structures" do

    test "string elements to properties" do

      assert %{
        "QueueUrl" => "http://sqs.us-east-2.amazonaws.com/770098461991/queue2"
      } = deserialize("<QueueUrl>http://sqs.us-east-2.amazonaws.com/770098461991/queue2</QueueUrl>", %{
        "type" => "structure",
        "members" => %{
          "QueueUrl" => %{"type" => "string"}
        }
      })
    end

    test "nested structures" do

      assert %{
        "Nested" => %{"QueueUrl" => "http://sqs.us-east-2.amazonaws.com/770098461991/queue2"}
      } = deserialize("
          <Nested>
            <QueueUrl>http://sqs.us-east-2.amazonaws.com/770098461991/queue2</QueueUrl>
          </Nested>", %{
        "type" => "structure",
        "members" => %{
          "Nested" => %{
            "type" => "structure",
            "members" => %{
              "QueueUrl" => %{"type" => "string"}
            }
          }
        }
      })
    end


  end


  describe "lists" do

    test "converts xml lists of strings into arrays of strings" do

      assert %{
        "Items" => ["a", "b", "c"]
      } = deserialize("
          <Items><member>a</member><member>b</member><member>c</member></Items>
        ", %{
          "type" => "structure",
          "members" => %{
            "Items" => %{
              "type" => "list",
              "member" => %{"type" => "string"}
            }
        }
      })

    end


    test "observes list member names when present" do

      assert %{
        "Items" => ["a", "b", "c"]
      } = deserialize("
          <Items><Item>a</Item><Item>b</Item><Item>c</Item></Items>
        ", %{
        "type" => "structure",
        "members" => %{
          "Items" => %{
            "type" => "list",
            "member" => %{"type" => "string", "locationName" => "Item"}
          }
        }
      })

    end


    test "parse lists of structures" do

      assert %{
        "Items" => [%{"Name" => "a"}, %{"Name" => "b"}, %{"Name" => "c"}]
      } = deserialize("
          <Items><member><Name>a</Name></member><member><Name>b</Name></member><member><Name>c</Name></member></Items>
        ", %{
        "type" => "structure",
        "members" => %{
          "Items" => %{
            "type" => "list",
            "member" => %{ "type" => "structure", "members" => %{"Name" => %{"type" => "string"}}}
          }
        }
      })

    end

    test "parse lists of structures with locationName" do

      assert %{
        "Items" => [%{"Name" => "a"}, %{"Name" => "b"}]
      } = deserialize("
          <Items><OtherName><Name>a</Name></OtherName><OtherName><Name>b</Name></OtherName></Items>
        ", %{
        "type" => "structure",
        "members" => %{
          "Items" => %{
            "type" => "list",
            "member" => %{ "type" => "structure", "locationName" => "OtherName", "members" => %{"Name" => %{"type" => "string"}}}
          }
        }
      })

    end

  end


  describe "flattened lists" do

    test "collect sibling elements of the same name" do

      assert %{
        "Count" => "3",
        "Topic" => ["A", "B", "C"]
      } = deserialize("
          <Count>3</Count>
          <Item>A</Item>
          <Item>B</Item>
          <Item>C</Item>
        ", %{
        "type" => "structure",
        "members" => %{
          "Count" => %{ "type" => "string" },
          "Topic" => %{
            "type" => "list",
            "flattened" => true,
            "member" => %{
              "type" => "string",
              "locationName" => "Item"
            }
          }
        }
      })
    end

    test "uses locationName when defined" do

      assert %{
        "OtherName" => ["A", "B", "C"]
      } = deserialize("
          <Topic>A</Topic>
          <Topic>B</Topic>
          <Topic>C</Topic>
        ", %{
        "type" => "structure",
        "members" => %{
          "OtherName" => %{
            "type" => "list",
            "flattened" => true,
            "member" => %{
              "type" => "string",
              "locationName" => "Topic"
            }
          }
        }
      })
    end

    test "can contains complex structures" do

       assert %{
         "Topic" => [%{
            "OtherStructure" => %{"Arn" => "arn:..."}
         }]
       } = deserialize("
          <Topic>
            <OtherStructure><Arn>arn:...</Arn></OtherStructure>
          </Topic>
         ", %{
         "type" => "structure",
         "members" => %{
           "Topic" => %{
             "type" => "list",
             "flattened" => true,
             "member" => %{
               "type" => "structure",
               "members" => %{
                 "OtherStructure" => %{
                   "type" => "structure", "members" => %{"Arn" => %{"type" => "string"}}
                 }
               }
             }
           }
         }
       })

    end

  end


  describe "non-flattened maps" do

    test "returns empty map when element is empty" do

      assert %{
        "Parameters" => %{}
      } = deserialize("<Parameters />", %{
        "type" => "structure",
        "members" => %{
          "Parameters" => %{
            "type" => "map",
            "key"   => %{"type" => "string"},
            "value" => %{"type" => "string"}
          }
        }
      })

    end

    test "expects entry, key, and value elements by default" do

        assert %{
          "Parameters" => %{
            "foo" => "bar",
            "bar" => "foo"
          }
        } = deserialize("
            <Parameters>
              <entry>
                <key>foo</key>
                <value>bar</value>
              </entry>
              <entry>
                <key>bar</key>
                <value>foo</value>
              </entry>
            </Parameters>
          ", %{
          "type" => "structure",
          "members" => %{
            "Parameters" => %{
              "type" => "map",
              "key"   => %{"type" => "string"},
              "value" => %{"type" => "string"}
            }
          }
        })

    end

    test "uses alternate names for key and value elements" do

      assert %{
        "Parameters" => %{
          "foo" => "bar"
        }
      } = deserialize("
          <Parameters><entry><otherkey>foo</otherkey><othervalue>bar</othervalue></entry></Parameters>
        ", %{
        "type" => "structure",
        "members" => %{
          "Parameters" => %{
            "type" => "map",
            "key"   => %{"locationName" => "otherkey", "type" => "string"},
            "value" => %{"locationName" => "othervalue", "type" => "string"}
          }
        }
      })

    end

  end

  describe "flattened map" do

    test "expects key and value elements by default" do

      assert %{
          "Parameters" => %{
            "foo" => "bar",
            "bar" => "foo"
        }
      } = deserialize("
          <Parameters><key>foo</key><value>bar</value><key>bar</key><value>foo</value></Parameters>
        ", %{
        "type" => "structure",
        "members" => %{
          "Parameters" => %{
            "type" => "map",
            "flattened" => true,
            "key"   => %{"type" => "string"},
            "value" => %{"type" => "string"}
          }
        }
      })

    end

    test "uses alternate names for key and value elements" do

      assert %{
          "Parameters" => %{
            "foo" => "bar",
            "bar" => "foo"
        }
      } = deserialize("
          <Parameters><otherkey>foo</otherkey><othervalue>bar</othervalue><otherkey>bar</otherkey><othervalue>foo</othervalue></Parameters>
        ", %{
        "type" => "structure",
        "members" => %{
          "Parameters" => %{
            "type" => "map",
            "flattened" => true,
            "key"   => %{"locationName" => "otherkey", "type" => "string"},
            "value" => %{"locationName" => "othervalue", "type" => "string"}
          }
        }
      })

    end

  end


  describe "scalars" do

    test "converts the string into boolean value" do

      assert %{
          "Success" => true,
          "Retried" => false,
          "Empty" => nil
      } = deserialize("
          <Success>true</Success>
          <Retried>false</Retried>
          <Empty />
        ", %{
         "type" => "structure",
         "members" => %{
           "Success" => %{ "type" => "boolean" },
           "Retried" => %{ "type" => "boolean" },
           "Empty" => %{ "type" => "boolean" }
         }
      })
    end

    test "converts float" do

      assert %{
          "Took" => 123.456
      } = deserialize("<Took>123.456</Took>", %{
         "type" => "structure",
         "members" => %{
           "Took" => %{ "type" => "float" }
         }
      })

    end


    test "strings are not trimed" do

      assert %{
          "Took" => " abc "
      } = deserialize("<Took> abc </Took>", %{
         "type" => "structure",
         "members" => %{
           "Took" => %{ "type" => "string" }
         }
      })

    end

    test "converts integer" do

      assert %{
          "Took" => 123
      } = deserialize("<Took>123</Took>", %{
         "type" => "structure",
         "members" => %{
           "Took" => %{ "type" => "integer" }
         }
      })

    end


    test "decodes 64 strings" do

      assert %{
        "Value" => "foo"
      } = deserialize("
          <Value encoding=\"base64\">
            Zm9v
          </Value>
        ", %{
         "type" => "structure",
         "members" => %{
           "Value" => %{ "type" => "string" }
         }
      })

    end

  end




end