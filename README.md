# erf - Erlang REST Framework
[![erf ci](https://github.com/nomasystems/erf/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/erf/actions/workflows/ci.yml)
[![erf docs](https://github.com/nomasystems/erf/actions/workflows/docs.yml/badge.svg)](https://nomasystems.github.io/erf)

A design-first Erlang REST framework.

## Specification requirements

### OAS 3.0
- Path parameters MUST be of type string. You can use the `pattern` keyword to refine your type spec.

## Contributing

We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `erf` in the open-source community. Don't forget to check out our [contribution guidelines](CONTRIBUTING.md) to ensure smooth collaboration! :rocket:

## Support

If you need help or have any questions, please don't hesitate to open an issue or contact the maintainers directly.

## License

`erf` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.

### Additional Licenses

This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.

Additionally, it allows for `swagger-ui` hosting, which is licensed under the Apache 2.0 license. For more details, please refer to the associated [LICENSE](priv/swagger-ui/LICENSE) file.
