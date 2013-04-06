URI
===

[![Build Status](https://travis-ci.org/erlware/uri.png)](https://travis-ci.org/erlware/uri)

A module for generating, parsing, encoding, and decoding uris.
At the moment this module isn't very sympathetic to non-http
uri's, but that could/should change in the future.

uri is a record that represents the different parts of a uri,
as defined by rfc-2396. It has the following fields:
