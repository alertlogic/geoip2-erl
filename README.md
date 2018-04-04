# Erlang wrapper for MaxMind GeoIP2 C library

The geoip2-erl is a safe port wrapper OTP application for MaxMind's [libmaxminddb](https://github.com/maxmind/libmaxminddb).

## Basic build instruction

    rebar3 compile

## Configuration

In your `sys.config` you can specify multiple named instances of geoip2 servers and for every instance you can specify the path of its mmdb file and optionally its options. Right now the only option you can specify is `{cache, false}` turning off default map cache of IP lookups.

Example:
```erlang
{geoip2, [{
    instances, [
        {city, "./GeoIP2-City.mmdb"},
        {city_no_cache, "./GeoIP2-City.mmdb", [{cache, false}]}
    ]
}]} 
```

## Usage

To lookup for data related to a given IPv4 / IPv6 address you can specify it as a tuple / string / binary:

```erlang
{ok, ResultMap} = geoip2:lookup(city, "8.8.8.8").
```

To quickly extract just an interesting part of information from returned map you can use `get` (throws error) or `find` (returns `error | {ok, Value}`) functions :

```erlang
{ok, Result} = geoip2:find([country, iso_code], ResultMap).
```

or

```erlang
Result = geoip2:get([country, iso_code], ResultMap, <<"EN">>)
```

If you are interested only in specified part of information you can use "combo" versions of `get` or `find` functions: 

```erlang
{ok, Result} = geoip2:find(city, [country, iso_code], "8.8.8.8").
```

To dynamically add instance of geoip2 server use:

```erlang
geoip2:start(country, "./GeoIP2-Country.mmdb").
```

There are also `stop` and `restart` functions available.

## License

This library is licensed under the MIT license.