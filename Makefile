SPEC_PATH=../aws-sdk-go/models/apis
TEMPLATE_PATH=priv_ex
OUTPUT_PATH=generated/aws

all: generate

generate:
	mix run generate.exs elixir $(SPEC_PATH) $(TEMPLATE_PATH) $(OUTPUT_PATH)
