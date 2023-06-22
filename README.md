# erf - Erlang REST Framework
![erf ci](https://github.com/nomasystems/erf/actions/workflows/ci.yml/badge.svg)

A design-first Erlang REST framework.

## Specification requirements

### OAS 3.0
- Path parameters MUST be of type string. You can use the `pattern` keyword to refine your type spec.

## License

`erf` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.

### Additional Licenses

This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.

Additionally, it allows for `swagger-ui` hosting, which is licensed under the Apache 2.0 license. For more details, please refer to the associated [LICENSE](priv/swagger-ui/LICENSE) file.
