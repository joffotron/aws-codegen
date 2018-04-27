defmodule AWS.CodeGen.Mixfile do
  use Mix.Project

  def project do
    [app: :aws_codegen,
     version: "0.0.1",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [{:earmark, "~> 1.1", only: :dev},
     {:ex_doc, "~> 0.15.0", only: :dev},
     {:poison, "~> 3.1"}]
  end
end

#%AWS.CodeGen.QueryService.Action{arity: 3,function_name: "cancel_update_stack",
#  name: "CancelUpdateStack",
#  schema: %{
#    "errors" => [
#      TokenAlreadyExistsException: %{
#        "error" => %{
#          "code" => "TokenAlreadyExistsException",
#          "httpStatusCode" => 400,
#          "senderFault" => true
#        },
#        "exception" => true,
#        "members" => %{},
#        "type" => "structure"
#      }
#    ],
#    "http" => %{
#      "method" => "POST",
#      "requestUri" => "/"
#    },
#    "input" => %{
#      "shape" => %{
#        "members" => %{
#          "ClientRequestToken" => %{
#            "max" => 128,
#            "min" => 1,
#            "pattern" => "[a-zA-Z0-9][-a-zA-Z0-9]*",
#            "type" => "string"
#          },
#          "StackName" => %{
#            "type" => "string"
#          }
#        },
#        "required" => ["StackName"],
#        "type" => "structure"
#      }
#    },
#    "name" => "CancelUpdateStack",
#    "output" => %{
#      "resultWrapper" => nil,
#      "shape" => nil
#    }
#  }
#}
