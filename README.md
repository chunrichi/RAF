# RAF

RESTful API Framework, a simple framework for ABAP to build RESTful APIs.

## Installation

1. Download the latest release from [GitHub](.)
2. Import the package zip file into sap system with [ABAPGIT](https://github.com/abapGit/abapGit)
3. Use the package `ZRAF` as recommended below.

## Usage

### Develop and publish interfaces

The framework provides a simple way to develop and publish RESTful APIs.

1. Create a new function like `ZFM_RAF00_IN_DEMO` in package `ZRAF00_DEMO`.
2. Add the configuration information to the SM34 `ZVRAF_CONF` View naming `demo`.
3. Call the api with the following url: `/sap/bc/zraf/demo`

You can find demo code and demo report in the package `ZRAF00_DEMO`. Use the demo report `ZDEMO_RAF_INIT` to initialize and test the framework.

### Consumer publish interfaces

The framework provides a simple way to consume RESTful APIs.

You can call the class `ZCL_RAF_OUTBOUND_DIRT` to consume open apis. 
> Or you can make a function and configuration it in SM34 `ZVRAF_CONF` View. See the demo report `ZDEMO_RAF_INIT` for details.

### Log

You can find the record information of consumed or called interface in the tcode `ZRAF_LOG`.

### Configuration

1. You can change the base url of the framework in the tcode `sicf`.
   1. find the default configuration in `sicf` by name `zraf`
   2. close the default url and add a new one
2. You can change the way to access the specific processing URI or Params by change the configuration in the report `ZRAF_CONFIG` (It's still in development)
   - URI like `/sap/bc/zraf/demo`
   - Params like `/sap/bc/zraf?apino=demo`

## Todo

Our goal is to build a API Management System within SAP.

- [X] simpler way to config the framework. 🚩
- [X] simpler way to consume open apis. 🚩
- [X] log the record of consumed or called interface. 🚩
- [ ] config report for config base info. 🚧
- [ ] find way to limiter for the api. 🧐
- [ ] find wat to throttling for customers api. 🧐
- [ ] Improve developer experience.
