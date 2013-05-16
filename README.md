# k6\_bytea [![Build Status](https://secure.travis-ci.org/kivikakk/k6_bytea.png)](http://travis-ci.org/kivikakk/k6\_bytea)

**A mutable byte array for Erlang.**

k6\_bytea provides a mutable byte array for Erlang, intended for scenarios
where using binary strings would be egregious to performance or semantics.

The full documentation is available as `edoc`; `make doc` will generate them in
the `doc` directory.

You can use the following in your `rebar.config`:

```erlang
{deps, [
    {k6_bytea, "1.1.2", {git, "https://github.com/kivikakk/k6_bytea.git", {tag, "v1.1.2"}}}
]}.
```

Slightly dubious statistics on k6\_bytea's performance are available [in a blog
post I wrote about it](http://kivikakk.ee/2013/05/13/k6_bytea.html).

## authorship

Original author: [Arlen Cuss](https://github.com/kivikakk).

## copyright and licensing

The [MIT license](http://opensource.org/licenses/MIT).

Copyright &copy; 2013 Arlen Cuss

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
