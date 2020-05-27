# SchemeBBS

Anonymous BBS written in MIT Scheme

## Demo

[https://textboard.org/prog](http://textboard.org/prog)

[https://textboard.org](http://textboard.org)


## Run it

```
./init.sh 8080
```

SchemeBBS should not directly serve clients, even if possible to do so. The HTTP implementation is far too incomplete and you should use your favorite web server as a reverse proxy.

## Patching MIT Scheme

The file runtime/http-syntax.scm follows the RFC 2616 which requires
that the value of the Location header be an absolute URI.

The standard has been replaced (see RFC 7231 section 7.1.2.) and a
relative URI is now allowed.

How to apply this patch:

```
curl -O http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/9.2/mit-scheme-9.2.tar.gz
curl -O ttps://gitlab.com/naughtybits/schemebbs/-/raw/master/mit-scheme-9.2_patches/patch-runtime_http-syntax.scm
tar xzvf mit-scheme-9.2.tar.gz
patch -p0 < patch-runtime_http-syntax.scm
cd mit-scheme-9.2/src
./configure
make
sudo make install
```

## License

Copyright 2020 Ben Bitdiddle

Permission is hereby granted, free of charge, to any person obtaining a copy of 
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
